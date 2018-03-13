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
import matplotlib as mpl
mpl.use('pgf')
import numpy as np
import matplotlib.pyplot as plt

plt.rc('font', size=10)
plt.rc('legend', fontsize=10)
plt.rcParams['text.usetex'] = True
plt.rcParams['text.latex.preamble'] = '\usepackage{libertine},\usepackage[libertine]{newtxmath},\usepackage[T1]{fontenc}'

generated_graphs_base = "generated-graphs/"
transformed_data_base = "transformed-data/"

def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)


def print_usage(args):
    print("Usage: {0} <file1>".format(args[0]))

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

def generate_ast_sizes_graph_confused(input_csv):
    N = 10
    opt_quot_ast = project_column_from_csv(input_csv, "OO")

    ind = np.arange(N)  # the x locations for the groups
    width = 0.35       # the width of the bars

    fig, ax = plt.subplots()
    rects1 = ax.bar(ind - width, opt_quot_ast, width=width, color='r',align='center')

    opt_noquot_ast = project_column_from_csv(input_csv, "SO")
    rects2 = ax.bar(ind, opt_noquot_ast, width=width,align='center')

    pure_boom_ast = project_column_from_csv(input_csv, "SS")
    rects3 = ax.bar(ind + width, pure_boom_ast, width=width, color='y',align='center')

    # add some text for labels, title and axes ticks
    ax.set_ylabel('Scores')
    ax.set_title('Scores by group and gender')
    ax.set_xticks(ind + width*(1.5))
    ax.set_xticklabels(project_column_from_csv(input_csv, "Test"))

    ax.legend((rects1[0], rects2[0], rects3[0]), ('OO', 'SS', 'SO'))

    fig = plt.figure(1,tight_layout=True)
    fig.set_figheight(2)
    fig.set_figwidth(6)

    fig.savefig(generated_graphs_base + "asts.eps", bbox_inches='tight')

def generate_ast_sizes_graph(input_csv):
    N = 10
    ind = np.arange(N)  # the x locations for the groups

    width = 0.25       # the width of the bars

    fig, ax = plt.subplots()
    input_csv.sort(key=lambda x : int(x["OO"]))

    qre_optician_means = project_column_from_csv(input_csv, "OO")
    rects1 = ax.bar(ind, qre_optician_means, width, color='r')

    basic_boomerang_means = project_column_from_csv(input_csv, "SO")
    rects2 = ax.bar(ind + width, basic_boomerang_means, width, color='y')

    optician_means = project_column_from_csv(input_csv, "SS")
    rects3 = ax.bar(ind+width*2, optician_means, width, color='b')

    # add some text for labels, title and axes ticks
    ax.set_ylabel('AST Count')
    ax.set_title('AST Counts by Benchmark')
    ax.set_xticks(ind + width*1.5)
    ax.set_xticklabels(project_column_from_csv(input_csv, "Test"),rotation=45)

    ax.legend((rects1[0], rects2[0], rects3[0]), ('\\textbf{QS}', '\\textbf{BS}', '\\textbf{NS}'),bbox_to_anchor=(0.92,.922),borderaxespad=0)

    fig = plt.figure(1,tight_layout=True)
    fig.set_figheight(2)
    fig.set_figwidth(6)

    fig.savefig(generated_graphs_base + "asts.eps", bbox_inches='tight')

def generate_time_vs_tasks_graph(input_csv):
    fig, ax = plt.subplots()

    def create_step_plot(colname, outputname,style,width):
        col_vals = [float(x) for x in project_column_from_csv(input_csv, colname) if int(x) < 7500]
        print(col_vals)
        col_vals_and_endpoints = col_vals + [0,8000]
        x_vals = sorted([x for x in set(col_vals_and_endpoints)])
        x_count_dict = {key: 0 for key in x_vals}
        for val in col_vals:
            x_count_dict[val] = x_count_dict[val]+1
        x_completed_counts = []
        acc = 0
        for val in x_vals:
            acc = acc + x_count_dict[val]
            x_completed_counts.append(acc)
        x_completed_counts = [0] + x_completed_counts[:len(x_completed_counts)-1]
        if (style != '-'):
            ax.step(x_vals,x_completed_counts,label=outputname,linestyle=style,linewidth=width, dashes=(5,1))
        else:
            ax.step(x_vals,x_completed_counts,label=outputname,linestyle=style,linewidth=width)

    normal_size = 2
    full_size = 3

    ax.step([0,8000],[14,14],label="Benchmarks With\nNontrivial Quotients",linestyle=":",
            linewidth=1, dashes=(1,1))

    create_step_plot("OO","\\textbf{QS}",'-',normal_size)
    create_step_plot("SO","\\textbf{CS}",':',normal_size)
    create_step_plot("SS","\\textbf{LS}",'-',normal_size)

    ax.set_ylabel('Benchmarks\nDefinable')
    ax.set_xlabel('AST Count')
    ax.set_title("AST Count vs Benchmarks Definable")

    l = ax.legend(bbox_to_anchor=(1.56,1),borderaxespad=0)
    plt.setp(l.texts,weight='bold')

    plt.xticks(np.arange(0, 7501, 2500))
    plt.yticks(np.arange(0, 15.1, 5))

    fig = plt.figure(1,tight_layout=True)
    fig.set_figheight(2)
    fig.set_figwidth(4)

    fig.savefig(generated_graphs_base + "asts.eps", bbox_inches='tight')

def main(args):
    if len(args) == 2:
        input_filepath = args[1]
        input_csv = retrieve_csv(input_filepath)
        generate_ast_sizes_graph(input_csv)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
