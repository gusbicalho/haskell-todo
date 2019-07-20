#! /usr/bin/env bash
export SERVICE_CONFIG='{"port": 8080, "version": "ghci"}'
stack ghci servant-test:lib servant-test:servant-test-test
