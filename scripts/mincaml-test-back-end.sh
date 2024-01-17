#!/bin/sh
cd .. || exit 1

MINCAMLC=ocaml/mincamlc
COMPILE_AS=arm-none-eabi-as
COMPILE_LD=arm-none-eabi-ld
COMPILEOPT=-mfpu=fpv5-d16
libmincaml=ARM/libmincaml.S
EXEC=qemu-arm
OPTION=-o
tests="tests/gen-code/"
tests_abs=$(pwd)/"${tests}"
generate=".s"
obj_generate=".o"
arm_generate=".arm"
# test topics
topics=("" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )

test_files=`ls "$tests"*.ml | grep -v front`

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

# clean the generated asml files and results files
rm tests/gen-code/*.asml 2> /dev/null 1> /dev/null
rm tests/gen-code/*.actual 2> /dev/null 1> /dev/null

# variables to count the number of passed and failed tests
passed=0
failed=0

# run one test case for each iteration in gen-code and make sure the asml code generated is correct and give the expected result
echo "---------- TESTING BACK-END WITH ARM GENERATION ----------"
for test_case in $test_files
do  
    file=$(basename $test_case)
    topic_num=$(echo $file | cut -d'-' -f1)
    file_name="${tests_abs}"$(echo $file | cut -d'-' -f2,3)
    file_generated="$(echo $file_name | cut -d'.' -f1)""${generate}"
    object_file="$(echo $file_name | cut -d'.' -f1)""${obj_generate}"
    arm_file="$(echo $file_name | cut -d'.' -f1)""${arm_generate}"
    result=$(echo $file_name | cut -d'.' -f1)".actual"
    expected=$(echo $file_name | cut -d'.' -f1)".expected"
    

    (( topic_num != old_topic_num )) &&  echo && echo -e "\t - Iteration $topic_num : ${topics[topic_num]} -"
    
    echo -n "Test on: "$file" ..."
    $MINCAMLC "$test_case" $OPTION "$file_generated"
    $COMPILE_AS $COMPILEOPT "$file_generated" "$libmincaml" $OPTION "$object_file"
    $COMPILE_LD "$object_file" $OPTION "$arm_file"
    echo $($EXEC "$arm_file") 2> "$result" 1> "$result"
    echo $(diff -s "$result" "$expected") 1> /dev/null
    if diff "$result" "$expected" 2> /dev/null 
        then 
            echo -e "${GREEN} OK${RESET}"
            passed=$((passed+1))
        else
            echo -e "${RED} KO${RESET}"
            failed=$((failed+1))
    fi
    
    old_topic_num=$topic_num; 
    num_test=$(($num_test+1))
done

rm -f ${tests_abs}*'.comp' ${tests_abs}*'.actual' ${tests_abs}*${generate} ${tests_abs}*$obj_generate ${tests_abs}*$arm_generate

echo -e "\n---------- END TESTING ----------"
echo "Passed tests : $passed/$num_test"
echo "Failed tests : $failed/$num_test"
echo -e "-----------------------------------\n"

echo -e "\n- Back-end : génération et exécution de l'ARM -" >> resultats_tests.txt
echo "Passed tests : $passed/$num_test" >> resultats_tests.txt
echo "Failed tests : $failed/$num_test" >> resultats_tests.txt