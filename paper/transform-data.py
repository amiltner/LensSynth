#!/usr/local/bin/python

from __future__ import print_function
from easyprocess import EasyProcess

import os
import csv
from os.path import splitext, join
import subprocess
import sys
import time
import matplotlib
import numpy as np
import matplotlib.pyplot as plt

matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'

TEST_EXT = '.ls'
BASELINE_EXT = '.out'
BASE_FLAGS = []
TIMEOUT_TIME = 600
GENERATE_EXAMPLES_TIMEOUT_TIME = 600000

REPETITION_COUNT = 10

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
        print(col_name)
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
    gather_col(['-naive_expansion_search','-no_lens_context','-time'],ctime_combiner,"NaiveExpansionNoLC",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-use_only_forced_expansions','-no_lens_context','-time'],ctime_combiner,"OnlyForcedExpansionsNoLC",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-naive_expansion_search','-time'],ctime_combiner,"NaiveExpansion",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-use_only_forced_expansions','-time'],ctime_combiner,"OnlyForcedExpansions",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-forceexpand','-naive_expansion_search','-time'],ctime_combiner,"NoUDTypes",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-generatedexamples'],exs_reqd_combiner,"ExamplesRequired",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-max_to_specify'],max_exs_reqd_combiner,"MaxExampleCount",TIMEOUT_TIME,1)
    gather_col(['-spec_size'],max_exs_reqd_combiner,"SpecSize",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-lens_size'],max_exs_reqd_combiner,"LensSize",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-lens_size','-no_lens_context'],max_exs_reqd_combiner,"LensSizeNoLensContext",TIMEOUT_TIME,REPETITION_COUNT)
    gather_col(['-expansions_inferred'],max_exs_reqd_combiner,"ExpansionsInferred",TIMEOUT_TIME,1)
    gather_col(['-expansions_inferred','-no_lens_context'],max_exs_reqd_combiner,"ExpansionsInferredNoLensContext",TIMEOUT_TIME,1)
    gather_col(['-expansions_forced'],max_exs_reqd_combiner,"ExpansionsForced",TIMEOUT_TIME,1)
    gather_col(['-expansions_forced','-no_lens_context'],max_exs_reqd_combiner,"ExpansionsForcedNoLensContext",TIMEOUT_TIME,1)
    gather_col(['-specs_visited'],max_exs_reqd_combiner,"SpecsVisited",TIMEOUT_TIME,1)
    gather_col(['-specs_visited','-naive_expansion_search'],max_exs_reqd_combiner,"SpecsVisitedNaiveExpansion",TIMEOUT_TIME,1)
    gather_col(['-specs_visited','-use_only_forced_expansions'],max_exs_reqd_combiner,"SpecsVisitedOnlyForcedExpansions",TIMEOUT_TIME,1)
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
    print("Usage: {0} <file1>".format(args[0]))

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

def retrieve_csv(filename):
    csv_rows = []
    with open(filename, 'rb') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            csv_rows.append(row)
    return csv_rows

def project_column_from_csv(csv_obj, col_name):
    return [r[col_name] for r in csv_obj]

def write_to_filename(filename, s):
    with open(filename, "wb") as f:
        f.write(s)

def generate_examples_required_data(input_csv):
    zero_count_ind = 0
    one_to_five_count_ind = 1
    six_to_ten_count_ind = 2
    eleven_to_fifteen_count_ind = 3
    sixteen_to_twenty_count_ind = 4
    over_twenty_count_ind = 5

    zero_count_text = "0"
    one_to_five_count_text = "1-5"
    six_to_ten_count_text = "6-10"
    eleven_to_fifteen_count_text = "11-15"
    sixteen_to_twenty_count_text = "16-20"
    over_twenty_count_text = ">20"
    ind_to_text = [zero_count_text,
                   one_to_five_count_text,
                   six_to_ten_count_text,
                   eleven_to_fifteen_count_text,
                   sixteen_to_twenty_count_text,
                   over_twenty_count_text]

    experimental_values = [0,0,0,0,0,0,]
    determinizing_values = [0,0,0,0,0,0,]

    def add_to_correct_group(count_values, n):
        if n < 0.0:
            raise Exception("SOMETHING WENT WRONG")
        if n == 0.0:
            count_values[zero_count_ind] = count_values[zero_count_ind]+1
        elif n <= 5.0:
            count_values[one_to_five_count_ind] = count_values[one_to_five_count_ind]+1
        elif n <= 10.0:
            count_values[six_to_ten_count_ind] = count_values[six_to_ten_count_ind]+1
        elif n <= 15.0:
            count_values[eleven_to_fifteen_count_ind] = count_values[eleven_to_fifteen_count_ind]+1
        elif n <= 20.0:
            count_values[sixteen_to_twenty_count_ind] = count_values[sixteen_to_twenty_count_ind]+1
        else:
            count_values[over_twenty_count_ind] = count_values[over_twenty_count_ind]+1

    experimental_vals = project_column_from_csv(input_csv, "ExamplesRequired")
    for example_num in experimental_vals:
        add_to_correct_group(experimental_values, float(example_num))
    determinizing_vals = project_column_from_csv(input_csv, "MaxExampleCount")
    for example_num in determinizing_vals:
        add_to_correct_group(determinizing_values, float(example_num))
    

    ind = np.arange(6)
    width = 0.35

    fig, ax = plt.subplots()

    rects1 = ax.bar(ind, experimental_values, width, color='#ffffb3', align='center')
    rects2 = ax.bar(ind+width, determinizing_values, width, color='#998ec3', align='center')

    ax.set_ylabel('Count')
    ax.set_title("Examples Required for Benchmarks")
    ax.set_xticks(ind + width / 2)
    ax.set_xticklabels(ind_to_text)

    ax.legend((rects1[0],rects2[0]),("Experimental Average", "Determinize Permutations"))

    plt.savefig("examples.eps")

def generate_uninferred_expansions_data(input_csv):
    zero_count_ind = 0
    one_to_five_count_ind = 1
    six_to_ten_count_ind = 2
    eleven_to_fifteen_count_ind = 3
    sixteen_to_twenty_count_ind = 4
    over_twenty_count_ind = 5

    zero_count_text = "0"
    one_count_text = "1"
    two_count_text = "2"
    three_count_text = "3"
    four_count_text = "4"
    ind_to_text = [zero_count_text,
                   one_count_text,
                   two_count_text,
                   three_count_text,
                   four_count_text]

    uninferred_values = [0,0,0,0,0,]
    unforced_values = [0,0,0,0,0,]

    def add_to_correct_group(count_values, n):
        if n == 0.0:
            count_values[zero_count_ind] = count_values[zero_count_ind]+1
        elif n == 1.0:
            count_values[one_to_five_count_ind] = count_values[one_to_five_count_ind]+1
        elif n == 2.0:
            count_values[six_to_ten_count_ind] = count_values[six_to_ten_count_ind]+1
        elif n == 3.0:
            count_values[eleven_to_fifteen_count_ind] = count_values[eleven_to_fifteen_count_ind]+1
        elif n == 4.0:
            count_values[sixteen_to_twenty_count_ind] = count_values[sixteen_to_twenty_count_ind]+1
        else:
            raise Exception("SOMETHING WENT WRONG")

    total_exps = project_column_from_csv(input_csv, "ExpansionsPerformedNoLensContext")
    forced_exps = project_column_from_csv(input_csv, "ExpansionsForcedNoLensContext")
    inferred_exps = project_column_from_csv(input_csv, "ExpansionsInferredNoLensContext")
    total_and_inferred = zip(total_exps, inferred_exps)
    total_and_forced = zip(total_exps, forced_exps)
    for (total_exp,forced_exp) in total_and_forced:
        add_to_correct_group(unforced_values, float(total_exp)-float(forced_exp))
    for (total_exp,inferred_exp) in total_and_inferred:
        add_to_correct_group(uninferred_values, float(total_exp)-float(inferred_exp))

    ind = np.arange(5)
    width = 0.35

    fig, ax = plt.subplots()

    rects1 = ax.bar(ind, uninferred_values, width, color='#ffffb3', align='center')
    rects2 = ax.bar(ind+width, unforced_values, width, color='#998ec3', align='center')

    ax.set_ylabel('Count')
    ax.set_title("Examples Required for Benchmarks")
    ax.set_xticks(ind + width / 2)
    ax.set_xticklabels(ind_to_text)

    ax.legend((rects1[0],rects2[0]),("Full Inference","Forced Only"))

    plt.savefig("uninferred.eps")

def generate_time_vs_tasks_data(input_csv):
    def create_step_plot(colname, outputname):
        col_vals = [float(x) for x in project_column_from_csv(input_csv, colname) if x != "-1"]
        col_vals_and_endpoints = col_vals + [0,600000]
        x_vals = sorted([x for x in set(col_vals_and_endpoints)])
        print(x_vals)
        x_count_dict = {key: 0 for key in x_vals}
        for val in col_vals:
            x_count_dict[val] = x_count_dict[val]+1
        print(x_count_dict)
        return 0
    create_step_plot("FlashExtract","FlashExtract")

def main(args):
    if len(args) == 2:
        input_filepath = args[1]
        input_csv = retrieve_csv(input_filepath)
        ensure_dir("generated-graphs/")
        generate_examples_required_data(input_csv)
        generate_uninferred_expansions_data(input_csv)
        generate_time_vs_tasks_data(input_csv)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
