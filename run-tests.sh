#!/bin/bash

set -xeuo pipefail

dune build
flashcard -cmd "_build/default/bin/main.exe" -ext lox test/
