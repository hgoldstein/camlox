#!/usr/bin/env python3

import argparse
import logging
import os
import pathlib
import shutil
import subprocess
from typing import Generator
from dataclasses import dataclass


ROOT = pathlib.Path(__file__).resolve().parent
DIFF_CMD = ["difft", "--exit-code", "--display=inline"]

formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
ch.setFormatter(formatter)
log.addHandler(ch)


@dataclass
class OutputMismatch(object):
    pass


@dataclass
class NoOutputFile(object):
    pass


Result = NoOutputFile | OutputMismatch | None


@dataclass
class Test:
    file: pathlib.Path

    def expect(self) -> pathlib.Path:
        return self.file.with_suffix(".expect")

    def output(self) -> pathlib.Path:
        return self.file.with_suffix(".output")

    def has_diff(self) -> bool:
        return (
            subprocess.run(
                ["difft", "--exit-code", "--check-only", self.expect(), self.output()],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            ).returncode
            != 0
        )

    def print_diff(self) -> None:
        subprocess.run(["difft", "--display=side-by-side-show-both", self.expect(), self.output()])

    def name(self) -> str:
        return str(self.file.relative_to(ROOT))

    def run(self, promote: bool) -> Result:
        if not self.expect().exists() and not promote:
            return NoOutputFile()

        proc = subprocess.run(
            ["dune", "exec", "--display=quiet", "camlox", self.file],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )

        out = proc.stdout.decode()
        if proc.returncode != 0:
            out += f"[{proc.returncode}]\n"

        with open(self.output(), "w") as f:
            f.write(out)

        if promote:
            shutil.copy(self.output(), self.expect())
            return

        if not self.has_diff():
            return

        return OutputMismatch()


def tests() -> Generator[Test, None, None]:
    for d, _, files in os.walk(ROOT):
        for f in files:
            if f.endswith(".lox"):
                yield Test(file=(pathlib.Path(d) / f))


def run_tests(promote: bool) -> None:
    for test in tests():
        log.info(f"Running `{test.name()}'")
        match test.run(promote):
            case None:
                log.info(f"`{test.name()}' passed!")
            case OutputMismatch():
                log.error(f"`{test.name()}' failed, output mismatch:")
                test.print_diff()
            case NoOutputFile():
                log.error(f"`{test.name()}' failed, no output file")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--promote", action="store_true")
    args = parser.parse_args()
    run_tests(args.promote)


if __name__ == "__main__":
    main()
