#!/usr/local/bin/python

from __future__ import print_function
from easyprocess import EasyProcess

import os
from os.path import splitext, join
import subprocess
import sys
import time

TEST_EXT = '.ls'
BASELINE_EXT = '.out'
BOOM_EXT = '.boom'
BASE_FLAGS = ['-generate_boomerang_spec']
TIMEOUT_TIME = 6000
REPETITION_COUNT = 2
BASE_BOOM_DIR = 'generated_boomerang_specs/'

def ensure_dir(f):
    if not os.path.exists(f):
        os.makedirs(f)

def find_tests_and_ensure_output_dirs(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
        ensure_dir(BASE_BOOM_DIR + path)
    return tests

def gather_boom(rootlength, prog, path, base):
    process_output = EasyProcess([prog] + BASE_FLAGS + [join(path, base + TEST_EXT)]).call(timeout=TIMEOUT_TIME)
    if (process_output.stderr != "" or process_output.return_code != 0):
        raise Exception("failure")
    return process_output.stdout

def write_boom(boom, path, base):
    with open(join(BASE_BOOM_DIR+path, base + BOOM_EXT),'w') as boomfile:
        boomfile.write(boom)

def print_usage(args):
    print("Usage: {0} <prog> <testdir>".format(args[0]))

def transform_data(path, base, run_data):
    current_data = {"Test":join(path, base + TEST_EXT).replace("_","-")[6:]}
    run_data_transpose = transpose(run_data)
    for index in range(len(run_data_transpose)/2):
	col_name = run_data_transpose[index][0]
	col_data = run_data_transpose[index+1]
        if "" in col_data:
	    current_data[col_name]="Timeout"
        else:
            col = [float(x) for x in col_data]
            current_data[col_name] = str(sum(col)/len(col))
    return current_data

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        rootlength = len(path)
        data = []
        base_boom_dir = ensure_dir('generated_boomerang_specs/')
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            for path, base in find_tests_and_ensure_output_dirs(path):
                print(join(path, base + TEST_EXT).replace("_","-")[rootlength:])
                current_boom = gather_boom(rootlength,prog, path, base)
                write_boom(current_boom, path, base)
        else:
            print_usage(args)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
