#!/bin/sh
cd .. || exit 1

MINCAMLC=ocaml/mincamlc
ASML=tools/asml
OPTION=-test\ -back
tests="tests/gen-code/"
generate=".s"
# test topics
topics=("" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )

# clean the generated asml files and results files
rm tests/arm/*.asml 2> /dev/null 1> /dev/null
rm tests/arm/*.actual 2> /dev/null 1> /dev/null

# variables to count the number of passed and failed tests
passed=0
failed=0

# run one test case for each iteration in gen-code and make sure the asml code generated is correct and give the expected result
echo "---------- TESTING FRONT-END PASSES TO ASML GENERATION ----------"

for test_case in `ls "$tests"*.ml`
do
    file=$(basename $test_case)
    topic_num=$(echo $file | cut -d'-' -f1)
    file_name=$(echo $file | cut -d'-' -f2)
    expected=$(echo $file_name | cut -d'.' -f1)".expected"
    result=$(echo $file_name | cut -d'.' -f1)".actual"
    file_generated=$(echo $file_name | cut -d'.' -f1)"${generate}"
    # exit 1 


    [[ topic_num != old_topic_num ]] &&  echo -e "\t Iteration $topic_num : ${topics[topic_num]}"
        
    echo "Testing compiler on $(echo $test_case | cut -d'/' -f3 )"
    old_topic_num=$topic_num; 
done