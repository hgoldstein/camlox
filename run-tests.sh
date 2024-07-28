#!/bin/bash

set -xeuo pipefail

flashcard -verbose -cmd "_build/default/bin/main.exe" -ext lox -root test/
