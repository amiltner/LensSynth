#!/usr/local/bin/python

from __future__ import print_function
from easyprocess import EasyProcess

import os
import csv
from os.path import splitext, join
import subprocess
import sys
import time

TEST_EXT = '.ls'
BASELINE_EXT = '.out'
BASE_FLAGS = []
TIMEOUT_TIME = 8

REPETITION_COUNT = 10

def transpose(matrix):
    return zip(*matrix)

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def gather_datum(prog, path, base, additional_flags):
    return EasyProcess([prog] + BASE_FLAGS + additional_flags + [join(path, base + TEST_EXT)]).call(timeout=TIMEOUT_TIME).stdout

def gather_data(prog, path, base):
    current_data = {"Test":join(path, base + TEST_EXT).replace("_","-")[6:]}
    run_data = []
    timeout = False
    for iteration in range(REPETITION_COUNT):
    	datum = gather_datum(prog, path, base,['-data'])
	if datum == "":
		timeout = True
		break
	else:
		run_data.append(datum.split(","))
    if timeout:
	current_data["ComputationTime"]="Timeout"
	current_data["ExamplesRequired"]="Timeout"
    else:
        run_data_transpose = transpose(run_data)
	computation_time_col = [float(x) for x in run_data_transpose[0]]
	examples_required_col = [float(x) for x in run_data_transpose[1]]
	current_data["ComputationTime"]="{:.5f}".format(sum(computation_time_col)/len(computation_time_col))
	current_data["ExamplesRequired"]="{:.1f}".format(sum(examples_required_col)/len(examples_required_col))
    expanded_run_data = []
    for iteration in range(REPETITION_COUNT):
    	datum = gather_datum(prog, path, base,['-forceexpand'])
	if datum == "":
		timeout = True
		break
	else:
		expanded_run_data.append(datum.split(","))
    if timeout:
	current_data["ForceExpandTime"]="Timeout"
    else:
        expanded_run_data_transpose = transpose(expanded_run_data)
	expanded_computation_time_col = [float(x) for x in expanded_run_data_transpose[0]]
	current_data["ForceExpandTime"]="{:.5f}".format(sum(expanded_computation_time_col)/len(expanded_computation_time_col))
    return current_data

def print_data(data):
    with open("generated_data/data.csv", "wb") as csvfile:
	datawriter = csv.DictWriter(csvfile,fieldnames=data[0].keys())
	datawriter.writeheader()
	datawriter.writerows(data)

def print_usage(args):
    print("Usage: {0} <prog> <test|testdir>".format(args[0]))

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
        data = []
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            for path, base in find_tests(path):
		print(join(path, base + TEST_EXT).replace("_","-")[6:])
		current_data = gather_data(prog, path, base)
                data.append(current_data)
	    print_data(data)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                data = gather_data(prog, path, base)
		print_data([data])
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
