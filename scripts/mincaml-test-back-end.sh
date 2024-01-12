#!/bin/sh
cd .. || exit 1

MINCAMLC=ocaml/mincamlc
COMPILE=""
EXEC=""
OPTION=-test\ -back
tests="tests/gen-code/"
tests_abs=$(pwd)"${tests}"
generate=".s"
# test topics
topics=("" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )
n_test=`ls "$tests"*.ml | wc -l`

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

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
    file_name="${tests_abs}"$(echo $file | cut -d'-' -f2)
    file_generated="$(echo $file_name | cut -d'.' -f1)""${generate}"
    result=$(echo $file_name | cut -d'.' -f1)".actual"
    expected=$(echo $file_name | cut -d'.' -f1)".expected"

    (( topic_num != old_topic_num )) &&  echo && echo -e "\t Iteration $topic_num : ${topics[topic_num]}"
    
    echo -n "    Test on: $file_name ..."
    echo "Nothing to print" > $result
    echo $($MINCAMLC $OPTION "$test_case") 2> $file_generated 1> $file_generated
    echo $($COMPILE $file_generated) > "${file_name}"".comp"
    echo $($EXEC "${file_name}"".comp") 2> $result 1> $result
    echo $(diff -s "$result" "$expected") 1> /dev/null
    if diff "$result" "$expected" 2> /dev/null 
        then 
            echo -e "\r${GREEN} OK${RESET}"
            passed=$((passed+1))
        else
            echo -e "${RED}KO${RESET}"
            failed=$((failed+1))
    fi
    old_topic_num=$topic_num; 
done

rm -f ${tests_abs}*'.comp' ${tests_abs}*'.actual' ${tests_abs}*${generate}

echo 
echo "Passed tests : $passed/$n_test"
echo "Failed tests : $failed/$n_test"