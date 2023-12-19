set dotenv-load

export PATH := "./node_modules/.bin:" + env_var('PATH')

default:
    just --list

activate:
    source ~/.emacs.d/.foxmacs/venv/bin/activate
