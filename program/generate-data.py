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
BASE_FLAGS = ["-iterativedeepenstrategy"]
TIMEOUT_TIME = 60
GENERATE_EXAMPLES_TIMEOUT_TIME = 60

REPETITION_COUNT = 2

def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)

def transpose(matrix):
    return zip(*matrix)

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def gather_datum(prog, path, base, additional_flags, timeout):
    #print([prog] + BASE_FLAGS + additional_flags + [join(path, base + TEST_EXT)])
    process_output = EasyProcess([prog] + BASE_FLAGS + additional_flags + [join(path, base + TEST_EXT)]).call(timeout=timeout)
    return (process_output.stdout,process_output.stderr)

def gather_data(rootlength, prog, path, base):
    current_data = {"Test":join(path, base + TEST_EXT).replace("_","-")[rootlength:]}
    run_data = []
    timeout = False
    error = False
    for iteration in range(REPETITION_COUNT):
    	(datum,err) = gather_datum(prog, path, base,['-time'],TIMEOUT_TIME)
	if datum == "":
            if err == "":
		timeout = True
            else:
                error = True
	    break
	else:
	    run_data.append(datum.split(","))
    if error:
        current_data["ComputationTime"]="Error"
    elif timeout:
	current_data["ComputationTime"]="Timeout"
    else:
        run_data_transpose = transpose(run_data)
	computation_time_col = [float(x) for x in run_data_transpose[0]]
	current_data["ComputationTime"]="{:.5f}".format(sum(computation_time_col)/len(computation_time_col))
    generate_exs_run_data = []
    timeout = False
    error = False
    for iteration in range(REPETITION_COUNT):
    	(datum,err) = gather_datum(prog, path, base,['-generatedexamples'],GENERATE_EXAMPLES_TIMEOUT_TIME)
	if datum == "":
            if err == "":
		timeout = True
            else:
                error = True
	    break
	else:
	    generate_exs_run_data.append(datum.split(","))
    if error:
        current_data["ExamplesRequired"]="Error"
    elif timeout:
	current_data["ExamplesRequired"]="Timeout"
    else:
        generate_exs_run_data_transpose = transpose(generate_exs_run_data)
	example_number_col = [float(x) for x in generate_exs_run_data_transpose[0]]
	current_data["ExamplesRequired"]="{:.1f}".format(sum(example_number_col)/len(example_number_col))
    expanded_run_data = []
    timeout = False
    error = False
    for iteration in range(REPETITION_COUNT):
    	(datum,err) = gather_datum(prog, path, base,['-forceexpand','-time'],TIMEOUT_TIME)
	if datum == "":
            if err == "":
		timeout = True
            else:
                print("there was an error")
                error = True
	    break
	else:
	    expanded_run_data.append(datum.split(","))
    if error:
        current_data["ForceExpandTime"]="Error"
    elif timeout:
	current_data["ForceExpandTime"]="Timeout"
    else:
        expanded_run_data_transpose = transpose(expanded_run_data)
	expanded_computation_time_col = [float(x) for x in expanded_run_data_transpose[0]]
	current_data["ForceExpandTime"]="{:.5f}".format(sum(expanded_computation_time_col)/len(expanded_computation_time_col))
    max_ex_run_data = []
    timeout = False
    error = False
    (datum,err) = gather_datum(prog, path, base,['-max_to_specify'],TIMEOUT_TIME)
    if datum == "":
        if err == "":
	    timeout = True
        else:
            print(err)
            print("there was an error")
            error = True
    else:
	max_ex_run_data=int(datum)
    if error:
        current_data["MaxExampleCount"]="Error"
    elif timeout:
	current_data["MaxExampleCount"]="Timeout"
    else:
	current_data["MaxExampleCount"]=max_ex_run_data
    return current_data

def print_data(data):
    ensure_dir("generated_data/")
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
        rootlength = len(path)
        data = []
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            for path, base in find_tests(path):
		print(join(path, base + TEST_EXT).replace("_","-")[rootlength:])
		current_data = gather_data(rootlength,prog, path, base)
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
