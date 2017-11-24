#!/bin/bash
set -e
ghcid --command="stack ghci" --test=runTests
