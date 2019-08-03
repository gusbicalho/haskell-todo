#! /usr/bin/env bash
export SERVICE_CONFIG='{"version": "ghci"}'
stack ghci servant-test:lib servant-test:servant-test-test
