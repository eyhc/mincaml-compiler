#!/bin/bash
cd .. || exit 1

MINCAMLC=ocaml/mincamlc
CC=arm-linux-gnueabi-gcc
COMPILEOPT="-mfpu=fpv5-d16 -lm"
libmincaml=ARM/libmincaml.S
OPTION="-n_iter 0"
tests="tests/gen-code/"
tests_abs=$(pwd)/"${tests}"
EXEC=qemu-arm
test_files=`ls "$tests"*.ml | grep -v front`
float_files=`ls "$tests"*.ml | grep float`

# test topics
topics=("" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

export QEMU_LD_PREFIX=/usr/arm-linux-gnueabi/

# variables to count the number of passed and failed tests
passed=0
failed=0

# run all test cases in gen-code and make sure the assembler code generated is correct and the arm code executed gives the expected result
echo "------------ TESTING BACK-END WITH ARM GENERATION ------------"
for test_case in $test_files
do  
    # create the files' names
    file=$(basename $test_case)
    topic_num=$(echo $file | cut -d'-' -f1)
    file_name="${tests_abs}"$(echo $file | cut -d'-' -f2,3)
    file_generated="$(echo $file_name | cut -d'.' -f1)"".s"
    arm_file="$(echo $file_name | cut -d'.' -f1)"".arm"
    result=$(echo $file_name | cut -d'.' -f1)".actual"
    expected=$(echo $file_name | cut -d'.' -f1)".expected"

    (( topic_num != old_topic_num )) &&  echo && echo -e "\t - Iteration $topic_num : ${topics[topic_num]} -"
    
    echo -n "Test on: "$file" ..."
    # execute the commands to create the assembler file and the arm file
    $MINCAMLC $OPTION "$test_case" -o "$file_generated"
    $CC "$file_generated" "$libmincaml" -o "$arm_file" $COMPILEOPT
    # to control the number of digits written in the float results
    if [[ $float_files =~ $file ]]; then
        echo $($EXEC "$arm_file") 2> "$result" 1> "$result"
        res=""
        for i in `cat $result`
            do res="${res} $(echo $i |  perl -pe 's/0+$//xg')"
        done
        echo $res > "$result"
    else
        echo $($EXEC "$arm_file") 2> "$result" 1> "$result"
    fi
    # compare the actual result with the expected result
    echo $(diff -s "$result" "$expected") 1> /dev/null
    
    # determine if the test failed or passed
    if diff "$result" "$expected" 2> /dev/null 
        then 
            echo -e "${GREEN} OK${RESET}"
            passed=$((passed+1))
        else
            echo -e "${RED} KO${RESET}"
            failed=$((failed+1))
    fi
    
    old_topic_num=$topic_num; 
done

# clean the generated arm files, assembler files and results files
rm -f ${tests}/*.actual 2> /dev/null 1> /dev/null
rm -f ${tests}/*.arm 2> /dev/null 1> /dev/null
rm -f ${tests}/*.s 2> /dev/null 1> /dev/null

# show the results of the tests
echo -e "\n---------- END TESTING ----------"
echo "Passed tests : $passed / $((passed + failed))"
echo "Failed tests : $failed / $((passed + failed))"
echo -e "-----------------------------------\n"

# save the results into resultats_tests.txt
echo -e "\n- Back-end : génération et exécution de l'ARM -" >> resultats_tests.txt
echo "Passed tests : $passed / $((passed + failed))" >> resultats_tests.txt
echo "Failed tests : $failed / $((passed + failed))" >> resultats_tests.txt