#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
EXEC=tools/asml
OPTION=-asml
tests="tests/gen-code/"
tests_abs=$(pwd)"/${tests}"
generate=.asml
# test topics
topics=("" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )

test_files=`ls "$tests"*.ml | grep -v back`

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
echo "---------- TESTING FRONT-END PASSES TO ASML GENERATION ----------"
for test_case in $test_files
do
    
    file=$(basename $test_case)
    topic_num=$(echo $file | cut -d'-' -f1)
    file_name=$(echo $file | cut -d'-' -f2,3)
    file_generated="${tests_abs}""$(echo $file_name | cut -d'.' -f1)""${generate}"
    result="${tests_abs}"$(echo $file_name | cut -d'.' -f1)".actual"
    expected="${tests_abs}"$(echo $file_name | cut -d'.' -f1)".expected"
    
    (( topic_num != old_topic_num )) &&  echo && echo -e "\t - Iteration $topic_num : ${topics[topic_num]} - "
    
    echo -n "Test on: "$file_name" ..."
    touch "$result"
    echo "Nothing to print" > "$result"
    echo $($MINCAMLC $OPTION "$test_case") 2> "$file_generated" 1> "$file_generated"
    echo $($EXEC "$file_generated") 2> "$result" 1> "$result"
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

rm -f ${tests_abs}*actual ${tests_abs}*${generate}

echo -e "\n---------- END TESTING ----------"
echo "Passed tests : $passed/$num_test"
echo "Failed tests : $failed/$num_test"
echo -e "-----------------------------------\n"

echo -e "\nFront-end : génération et exécution de l'ASML" >> resultats_tests.txt
echo "Passed tests : $passed/$num_test" >> resultats_tests.txt
echo "Failed tests : $failed/$num_test" >> resultats_tests.txt