#!/usr/bin/env python

import argparse
import logging
import os
import pathlib
import shutil
import subprocess
from typing import Generator
from dataclasses import dataclass


ROOT = pathlib.Path(__file__).resolve().parent

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
    output: str


@dataclass
class Promoted(object):
    pass


Result = NoOutputFile | OutputMismatch | Promoted | None

def has_difft() -> bool:
    try:
        subprocess.check_call(['difft'])
        return True
    except FileNotFoundError as e:
        return False

@dataclass
class Test:
    file: pathlib.Path

    def expect(self) -> pathlib.Path:
        return self.file.with_suffix(".expect")

    def output(self) -> pathlib.Path:
        return self.file.with_suffix(".output")

    def diff_check_cmd(self) -> list[str]:
        if has_difft():
            return ["difft", "--exit-code", "--check-only", str(self.expect()), str(self.output())]
        else:
            return ["diff", "--brief", str(self.expect()), str(self.output())]

    def has_diff(self) -> bool:
        return subprocess.run(
            self.diff_check_cmd(),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        ).returncode != 0

    def diff_print_cmd(self) -> list[str]:
        if has_difft():
            return ["difft", "--display=side-by-side-show-both", str(self.expect()), str(self.output())]
        else:
            return ["diff", "--unified", str(self.expect()), str(self.output())]

    def print_diff(self) -> None:
        subprocess.run(self.diff_print_cmd())

    def name(self) -> str:
        return str(self.file.relative_to(ROOT))

    def run(self, promote: bool) -> Result:
        proc = subprocess.run(
            ["dune", "exec", "--display=quiet", "camlox", self.file],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )

        if not self.expect().exists() and not promote:
            return NoOutputFile(output=proc.stdout.decode())

        out = proc.stdout.decode()
        if proc.returncode != 0:
            out += f"[{proc.returncode}]\n"

        with open(self.output(), "w") as f:
            f.write(out)

        if promote:
            shutil.copy(self.output(), self.expect())
            return Promoted()

        if not self.has_diff():
            return

        return OutputMismatch()


def generate_tests() -> Generator[Test, None, None]:
    for d, _, files in os.walk(ROOT):
        for f in files:
            if f.endswith(".lox"):
                yield Test(file=(pathlib.Path(d) / f))


def tests_from_file_list(files: list[str]) -> Generator[Test, None, None]:
    for f in files:
        yield Test(file=pathlib.Path(f).resolve())


def run_tests(files: list[str], promote: bool) -> bool:
    failed = False
    for test in tests_from_file_list(files) if len(files) > 0 else generate_tests():
        log.info(f"Running `{test.name()}'")
        match r := test.run(promote):
            case None:
                log.info(f"`{test.name()}' passed")
            case Promoted():
                log.info(f"`{test.name()}' promoted output to expected.")
            case OutputMismatch():
                log.error(f"`{test.name()}' failed, output mismatch:")
                test.print_diff()
                failed = True
            case NoOutputFile():
                log.error(f"`{test.name()}' failed, no output file")
                log.info(f"`{test.name()}' output:\n{r.output}")
                failed = True
    return failed


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="*")
    parser.add_argument("-p", "--promote", action="store_true")
    args = parser.parse_args()
    if run_tests(args.files, args.promote):
        exit(1)
    else:
        exit(0)


if __name__ == "__main__":
    main()
