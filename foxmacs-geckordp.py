from geckordp.settings import GECKORDP
from geckordp.rdp_client import RDPClient
from geckordp.actors.root import RootActor
from geckordp.actors.events import Events
from geckordp.actors.web_console import WebConsoleActor
from geckordp.actors.descriptors.tab import TabActor
from geckordp.actors.targets.window_global import WindowGlobalActor
from geckordp.actors.watcher import WatcherActor
from geckordp.actors.network_content import NetworkContentActor
from geckordp.actors.network_event import NetworkEventActor
from geckordp.actors.network_parent import NetworkParentActor
from geckordp.actors.storage import CookieStorageActor

from geckordp.actors.inspector import InspectorActor
from geckordp.actors.walker import WalkerActor
from geckordp.actors.node import NodeActor
from geckordp.actors.node_list import NodeListActor
from geckordp.actors.string import StringActor

from typing import cast
from urllib.parse import urlparse
from concurrent.futures import Future
import time

import sys
import json
from types import TracebackType
import traceback
import pydantic
import importlib

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


class State(object):
    client: RDPClient
    root: RootActor

    def __init__(self, host: str, port: int):
        super().__init__()
        self.client = RDPClient()
        self.client.connect(host, port)
        self.root = RootActor(self.client)

    def disconnect(self):
        self.client.disconnect()


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
    host: str = "localhost"
    port: int = 6000

    def run(self) -> dict:
        global state
        if state is not None:
            state.disconnect()
        state = State(self.host, self.port)
        return {"status": "ok"}


class TabInfo(pydantic.BaseModel):

    def run(self, state: State) -> dict:
        tab = state.root.current_tab()
        return {"title": tab["title"], "url": tab["url"]}


class ListFrames(pydantic.BaseModel):

    def run(self, state: State) -> dict:
        tab_ctx = state.root.current_tab()
        tab = TabActor(state.client, tab_ctx["actor"])
        actor_ids = tab.get_target()
        web = WindowGlobalActor(state.client, actor_ids["actor"])
        return {"frames": web.list_frames()}


class EvalJS(pydantic.BaseModel):
    code: str
    frame: int | None = None

    def run(self, state: State) -> dict:
        tab_descriptor = state.root.current_tab()
        tab = TabActor(state.client, tab_descriptor["actor"])
        actor_ids = tab.get_target()

        # receive here evaluation results

        result = None

        async def on_evaluation_result(data: dict):
            # beautify python dictionary and print it
            # print(json.dumps(data, indent=2))
            nonlocal result
            result = data

        # add event listener with the specified console actor ID
        console_actor_id = actor_ids["consoleActor"]
        state.client.add_event_listener(
            console_actor_id, Events.WebConsole.EVALUATION_RESULT, on_evaluation_result)

        # initialize console and start listening
        console = WebConsoleActor(state.client, console_actor_id)
        console.start_listeners([])

        # console.evaluate_js_async("""
        #     (() => { return document.title; })();
        # """)

        console.evaluate_js_async(self.code)

        while result is None:
            time.sleep(0.1)

        return result



def process_command(cmd: Command) -> dict:
    if cmd.command == "connect":
        return Connect(**cmd.payload if cmd.payload else {}).run()

    if state is None or not state.client.connected():
        raise Exception("Not connected")

    if cmd.command == "tab-info":
        return TabInfo().run(state)

    if cmd.command == "list-frames":
        return ListFrames().run(state)

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
