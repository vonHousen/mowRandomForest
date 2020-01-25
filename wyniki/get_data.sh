#!/bin/bash

# get confusion matrices for multiple parameters
cat $1 | grep -A 7 "\"complexity\"" > complexity_matrices.txt
cat $1 | grep -A 7 "\"z ratio\"" > z_ratio_matrices.txt
cat $1 | grep -A 7 "\"subset\"" > subset_matrices.txt
cat $1 | grep -A 7 "\"tree numbers\"" > tree_numbers_matrices.txt

