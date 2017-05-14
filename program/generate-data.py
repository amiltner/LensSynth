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
TIMEOUT_TIME = 120
GENERATE_EXAMPLES_TIMEOUT_TIME = 600000

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
    process_output = EasyProcess([prog] + BASE_FLAGS + additional_flags + [join(path, base + TEST_EXT)]).call(timeout=timeout)
    return (process_output.stdout,process_output.stderr)


def gather_data(rootlength, prog, path, base):
    current_data = {"Test":join(path, base).replace("_","-")[rootlength:]}

    def gather_col(flags, run_combiner, col_name, timeout_time, repetition_count):
        run_data = []
        timeout = False
        error = False
        for iteration in range(repetition_count):
    	    (datum,err) = gather_datum(prog, path, base,flags,timeout_time)
            if datum == "":
                if err == "":
                    timeout = True
                else:
                    error = True
                    break
            else:
	            run_data.append(datum.split(","))
            if error:
                current_data[col_name]=-1
            elif timeout:
	            current_data[col_name]=-1
            else:
                run_data_transpose = transpose(run_data)
                current_data[col_name]=run_combiner(run_data_transpose)

    def ctime_combiner(run_data_transpose):
        computation_time_col = [float(x) for x in run_data_transpose[0]]
        ans = sum(computation_time_col)/len(computation_time_col)
        ans = (int)(ans * 1000)
        return ans

    def exs_reqd_combiner(run_data_transpose):
	    example_number_col = [float(x) for x in run_data_transpose[0]]
	    return "{:.1f}".format(sum(example_number_col)/len(example_number_col))

    def max_exs_reqd_combiner(run_data_transpose):
	    example_number_col = [float(x) for x in run_data_transpose[0]]
	    return int(sum(example_number_col)/len(example_number_col))

    def specsize_combiner(run_data_transpose):
	    example_number_col = [float(x) for x in run_data_transpose[0]]
	    return int(sum(example_number_col)/len(example_number_col))


    gather_col(['-time'],ctime_combiner,"ComputationTime",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-forceexpand','-time'],ctime_combiner,"ForceExpandTime",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-naive_strategy','-time'],ctime_combiner,"NaiveStrategy",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-naive_pqueue','-time'],ctime_combiner,"NaivePQueue",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-no_short_circuit','-time'],ctime_combiner,"NoShortCircuit",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-no_lens_context','-time'],ctime_combiner,"NoLensContext",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-no_short_circuit','-no_inferred_expansions','-no_lens_context','-time'],ctime_combiner,"NoInferenceNoLCNoSC",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-no_short_circuit','-no_lens_context','-time'],ctime_combiner,"NoLCNoSC",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-no_inferred_expansions','-no_lens_context','-time'],ctime_combiner,"NoInferenceNoLC",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-no_inferred_expansions','-time'],ctime_combiner,"NoInference",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-generatedexamples'],exs_reqd_combiner,"ExamplesRequired",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-max_to_specify'],max_exs_reqd_combiner,"MaxExampleCount",TIMEOUT_TIME,1)
    gather_col(['-spec_size'],max_exs_reqd_combiner,"SpecSize",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-expansions_inferred'],max_exs_reqd_combiner,"ExpansionsInferred",TIMEOUT_TIME,1)
    gather_col(['-expansions_inferred','-no_lens_context'],max_exs_reqd_combiner,"ExpansionsInferredNoLensContext",TIMEOUT_TIME,1)
    gather_col(['-expansions_forced'],max_exs_reqd_combiner,"ExpansionsForced",TIMEOUT_TIME,1)
    gather_col(['-expansions_forced','-no_lens_context'],max_exs_reqd_combiner,"ExpansionsForcedNoLensContext",TIMEOUT_TIME,1)
    gather_col(['-specs_visited'],max_exs_reqd_combiner,"SpecsVisited",TIMEOUT_TIME,1)
    gather_col(['-specs_visited','-no_inferred_expansions'],max_exs_reqd_combiner,"SpecsVisitedNoInference",TIMEOUT_TIME,1)
    gather_col(['-specs_visited','-no_lens_context'],max_exs_reqd_combiner,"SpecsVisitedNoLensContext",TIMEOUT_TIME,1)
    gather_col(['-expansions_performed'],max_exs_reqd_combiner,"ExpansionsPerformed",TIMEOUT_TIME,1)
    gather_col(['-expansions_performed','-no_lens_context'],max_exs_reqd_combiner,"ExpansionsPerformedNoLensContext",TIMEOUT_TIME,1)
    #gather_col(['-naive_pqueue','-no_lens_context','-time'],ctime_combiner,"NoLensContextNPQ",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-naive_pqueue','-no_short_circuit','-no_inferred_expansions','-no_lens_context','-time'],ctime_combiner,"NoInferenceNoLCNoSCNPQ",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-naive_pqueue','-no_short_circuit','-no_lens_context','-time'],ctime_combiner,"NoLCNoSCNPQ",TIMEOUT_TIME,REPETITION_COUNT)
    #gather_col(['-naive_pqueue','-no_inferred_expansions','-no_lens_context','-time'],ctime_combiner,"NoInferenceNoLCNPQ",TIMEOUT_TIME,REPETITION_COUNT)

    return current_data

def specsize_compare(x,y):
    return int(x["SpecSize"])-int(y["SpecSize"])

def sort_data(data):
    return sorted(data,cmp=specsize_compare)

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
	    current_data[col_name]=-1
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
            data = sort_data(data)
	    print_data(data)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                data = gather_data(prog, path, base)
                sort_data(data)
		print_data([data])
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
