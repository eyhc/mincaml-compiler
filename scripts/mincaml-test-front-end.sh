#! /bin/bash
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
EXEC=tools/asml
OPTION=-asml
tests="tests/gen-code/"
tests_abs=$(pwd)"/${tests}"
test_files=`ls "$tests"*.ml | grep -v back`

# test topics
topics=("" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

# variables to count the number of passed and failed tests
passed=0
failed=0

# run all test cases in gen-code and make sure the asml code generated and executed is correct and gives the expected result
echo "---------- TESTING FRONT-END PASSES TO ASML GENERATION ----------"
for test_case in $test_files
do
    # create the files' names
    file=$(basename $test_case)
    topic_num=$(echo $file | cut -d'-' -f1)
    file_name=$(echo $file | cut -d'-' -f2,3)
    file_generated="${tests_abs}""$(echo $file_name | cut -d'.' -f1)"".asml"
    result="${tests_abs}"$(echo $file_name | cut -d'.' -f1)".actual"
    expected="${tests_abs}"$(echo $file_name | cut -d'.' -f1)".expected"
    
    (( topic_num != old_topic_num )) &&  echo && echo -e "\t - Iteration $topic_num : ${topics[topic_num]} - "
    
    echo -n "Test on: "$file_name" ..."
    # to control that something is written inside the .actual file
    touch "$result"
    echo "Nothing to print" > "$result"

    # execute the commands to create the asml file and compare with the expected result
    echo $($MINCAMLC $OPTION "$test_case") 2> "$file_generated" 1> "$file_generated"
    echo $($EXEC "$file_generated") 2> "$result" 1> "$result"
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


# clean the generated asml files and results files
rm -f ${tests}/*.asml 2> /dev/null 1> /dev/null
rm -f ${tests}/*.actual 2> /dev/null 1> /dev/null

# show the results of the tests
echo -e "\n---------- END TESTING ----------"
echo "Passed tests : $passed / $((passed + failed))"
echo "Failed tests : $failed / $((passed + failed))"
echo -e "-----------------------------------\n"

# save the results into resultats_tests.txt
echo -e "\n- Front-end : génération et exécution de l'ASML -" >> resultats_tests.txt
echo "Passed tests : $passed / $((passed + failed))" >> resultats_tests.txt
echo "Failed tests : $failed / $((passed + failed))" >> resultats_tests.txt