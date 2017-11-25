#!/bin/bash
set -e
stack install ghcid
~/.local/bin/ghcid --command="stack ghci" --test=WatertightL.runTests
