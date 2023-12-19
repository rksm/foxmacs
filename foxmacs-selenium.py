from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement

from typing import cast
import esprima
from esprima.nodes import Node, Module, Script, BlockStatement, ExpressionStatement, VariableDeclaration, ReturnStatement, FunctionDeclaration
import escodegen

import json
import sys
import pydantic
import traceback

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


class State(object):
    driver: webdriver.Remote

    def __init__(self, geckodriver: str):
        super().__init__()
        capabilities = webdriver.DesiredCapabilities.FIREFOX.copy()
        capabilities["marionette"] = True
        service = Service(
            executable_path=geckodriver,
            service_args=["--marionette-port", "2828", "--connect-existing"],
            capabilities=capabilities)
        self.driver = webdriver.Firefox(service=service)

    def disconnect(self):
        self.driver.quit()


state: State | None = None

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


class Error(pydantic.BaseModel):
    error: str
    stacktrace: str | None = None


class Command(pydantic.BaseModel):
    id: str
    command: str
    payload: dict | None = None


class Connect(pydantic.BaseModel):
    geckodriver: str

    def run(self) -> dict:
        global state
        if state is not None:
            state.disconnect()
        state = State(self.geckodriver)
        return {"status": "ok"}


class TabInfo(pydantic.BaseModel):
    url: str
    title: str
    handle: str
    visible: bool

    def __repr__(self):
        return f"TabInfo(url={self.url}, title={self.title}, handle={self.handle})"

    @staticmethod
    def list_tabs(driver: webdriver.Remote) -> list["TabInfo"]:
        current_window = driver.current_window_handle
        result = []
        for handle in driver.window_handles:
            driver.switch_to.window(handle)
            visible = handle == current_window
            result.append(
                TabInfo(url=driver.current_url,
                        title=driver.title,
                        handle=handle,
                        visible=visible))
        driver.switch_to.window(current_window)
        return result

    @staticmethod
    def run(state: State) -> dict:
        tabs = TabInfo.list_tabs(state.driver)
        return {"tabs": [tab.model_dump() for tab in tabs]}


class TabActivate(pydantic.BaseModel):
    handle: str

    def run(self, state: State) -> dict:
        state.driver.switch_to.window(self.handle)
        return {"status": "ok"}


# tabs = TabInfo.list_tabs(driver)


class Frame(pydantic.BaseModel):
    id: str
    src: str | None = None

    @staticmethod
    def list_frames(driver: webdriver.Remote) -> list[tuple[WebElement, "Frame"]]:
        result = []
        for iframe in driver.find_elements(By.XPATH, "//iframe"):
            try:
                driver.switch_to.frame(iframe)
                result.append((iframe, Frame(id=iframe.id, src=iframe.get_attribute("src"))))
                result.extend(Frame.list_frames(driver))
            except:
                pass
            finally:
                driver.switch_to.parent_frame()
        return result

    @staticmethod
    def run(state: State) -> dict:
        frames = Frame.list_frames(state.driver)
        return {
            "frames": [{
                "id": frame.id,
                "src": frame.src
            } for _, frame in frames]
        }


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# js / source transform

def _tfm_for_return(ast: Node) -> list[Node]:
    if isinstance(ast, Module) or isinstance(ast, Script) or isinstance(ast, BlockStatement):
        return _tfm_for_return(ast.body[-1])
    elif ast.type == "ExpressionStatement":
        return_stmt = ReturnStatement(ast.expression)
        return [return_stmt]
    elif isinstance(ast, VariableDeclaration):
        decl = ast.declarations[-1]
        return_stmt = ReturnStatement(decl.id)
        return [ast, return_stmt]
    elif isinstance(ast, FunctionDeclaration):
        return_stmt = ReturnStatement(ast.id)
        return [ast, return_stmt]
    else:
        return [ast]


def _add_return_for_last_statement(ast: Module | Script):
    body = cast(list[Node], ast.body)
    last = body.pop()
    body.extend(_tfm_for_return(last))


def return_last(source: str) -> str:
    ast = esprima.parseScript(source)
    _add_return_for_last_statement(ast)
    return str(escodegen.generate(ast))

class EvalJS(pydantic.BaseModel):
    code: str
    frame: int | str | None = None

    def run(self, state: State) -> dict:
        try:
            if self.frame is not None:
                # print(Frame.list_frames(state.driver))
                frames = Frame.list_frames(state.driver)
                frame = [el for el, frame in frames if frame.id == self.frame]
                if frame and frame[0]:
                    state.driver.switch_to.frame(frame[0])
                else:
                    raise Exception(f"Frame {self.frame} not found")
            result = state.driver.execute_script(return_last(self.code))
        finally:
            state.driver.switch_to.default_content()

        return {"result": result}


def process_command(cmd: Command) -> dict:
    if cmd.command == "connect":
        return Connect(**cmd.payload if cmd.payload else {}).run()

    if state is None:
        raise Exception("Not connected")

    if cmd.command == "tab-info":
        return TabInfo.run(state)

    if cmd.command == "tab-activate" and cmd.payload:
        return TabActivate(**cmd.payload).run(state)

    if cmd.command == "list-frames":
        return Frame.run(state)

    if cmd.command == "eval-js" and cmd.payload is not None:
        return EvalJS(**cmd.payload).run(state)

    raise Exception(f"Unknown command: {cmd.command}")


def main():
    while True:
        # Read a line of input from stdin
        try:
            command = input()
        except EOFError:
            # End of file (EOF) indicates no more input will be coming,
            # for example when the input is redirected from a file
            break

        try:
            cmd = Command(**json.loads(command))

            if cmd.command == "exit":
                break

            response = process_command(cmd)
            response["id"] = cmd.id
            print(json.dumps(response))

        except Exception as err:
            stacktrace = None
            if err.__traceback__ is not None:
                stacktrace = "\n".join(traceback.format_tb(
                    err.__traceback__)).strip()
            err = Error(error=str(err), stacktrace=stacktrace)
            print(err.model_dump_json())

        finally:
            sys.stdout.flush()


if __name__ == '__main__':
    main()
