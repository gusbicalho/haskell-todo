#! /usr/bin/env bash
export SERVICE_CONFIG='{"port": 8080, "version": "ghci", "sqliteFile": "test.db", "jwtKeyPath": "resources/test.key"}'
stack ghci servant-test:lib servant-test:servant-test-test
