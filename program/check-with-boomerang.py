#!/usr/local/bin/python

from __future__ import print_function
from easyprocess import EasyProcess

import os
import csv
from os.path import splitext, join
import subprocess
import sys
import time

BOOMERANG_COMMAND = "boomerang"
TEMP_BOOM_FILENAME = "generated.boom"
TEST_EXT = '.ls'
BASELINE_EXT = '.out'
BASE_FLAGS = ["-iterativedeepenstrategy"]
TIMEOUT = 60

REPETITION_COUNT = 2

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def gather_datum(prog, path, base):
    process_output = EasyProcess([prog] + BASE_FLAGS + [join(path, base + TEST_EXT)]).call(timeout=TIMEOUT)
    return (process_output.stdout,process_output.stderr,process_output.return_code)

def run_boomerang():
    process_output = EasyProcess([BOOMERANG_COMMAND,TEMP_BOOM_FILENAME]).call(timeout=TIMEOUT)
    return (process_output.stdout,process_output.stderr,process_output.return_code)

def check_files(prog, path, base):
    test_name = join(path, base + TEST_EXT)
    (datum,err,return_code) = gather_datum(prog, path, base)
    if return_code != 0:
        raise Exception(test_name + " failed with stdout:" + datum + "and stderr:" + err)
    elif datum == "":
        if err == "":
	    raise Exception("synthesis of " + test_name + " timed out")
        else:
            raise Exception("synthesis of " + test_name +
                            " errored with error:" + err)
    with open(TEMP_BOOM_FILENAME, 'w') as f:
        f.write(datum)
    (datum,err,return_code) = run_boomerang()
    if return_code != 0:
        raise Exception("boomerang failed on the generated files of " + test_name +
                        "with stdout:" + datum + "stderr:" + err)

def raise_usage(args):
    raise Exception("Usage: {0} <prog> <test|testdir>".format(args[0]))

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        data = []
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(TEMP_BOOM_FILENAME):
            raise Exception(TEMP_BOOM_FILENAME + " exists, move then rerun")
        elif os.path.exists(path) and os.path.isdir(path):
            try:
                for path, base in find_tests(path):
		    check_files(prog, path, base)
            finally:
                if os.path.exists(TEMP_BOOM_FILENAME):
                    os.remove(TEMP_BOOM_FILENAME)
        else:
            try:
                path, filename = os.path.split(path)
                base, ext = splitext(filename)
                if ext != TEST_EXT:
                    raise_usage(args)
                else:
                    check_files(prog, path, base)
            finally:
                os.remove(TEMP_BOOM_FILENAME)
    else:
        raise_usage(args)

if __name__ == '__main__':
    main(sys.argv)
