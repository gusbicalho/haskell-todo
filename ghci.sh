#! /usr/bin/env bash
export SERVICE_CONFIG='{"version": "ghci"}'
stack ghci haskell-todo:lib # haskell-todo:haskell-todo-test
