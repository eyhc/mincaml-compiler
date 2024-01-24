#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

# TODO change this to point to your mincamlc executable if it's different, or add
# it to your PATH. Use the appropriate option to run the parser as soon
# as it is implemented
MINCAMLC=ocaml/mincamlc
OPTION=-p

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

# variables to count the number of passed and failed tests
passed=0
failed=0

# run all test cases in syntax/valid and make sure they are parsed without error
echo "------------------ TESTING PARSER ------------------\n"
for test_case in tests/syntax/valid/*.ml
do
    file=$(basename $test_case)
    file_name=$(echo $file | cut -d'-' -f2)

    echo -n "Test on: "$test_case" ..."
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then 
        echo "${GREEN} OK${RESET}"
        passed=$((passed+1))
    else
        echo "${RED} KO${RESET}"
        failed=$((failed+1))
    fi
done

# run all test cases in syntax/invalid and make sure the parser returns an error
for test_case in tests/syntax/invalid/*.ml
do
    file=$(basename $test_case)
    file_name=$(echo $file | cut -d'-' -f2)

    echo -n "Test on: "$test_case" ..."
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "${RED} KO${RESET}"
        failed=$((failed+1))
    else 
        echo "${GREEN} OK${RESET}"
        passed=$((passed+1))
    fi
done

# show the results of the tests
echo "\n---------- END TESTING ----------"
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they succeed to fail)"
echo "Tests failed : $failed / $((passed + failed))"
echo "-----------------------------------\n"

# save the results into resultats_tests.txt
echo "RESUME DES TESTS" > resultats_tests.txt
echo "- Parser -" >> resultats_tests.txt
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they succeed to fail)" >> resultats_tests.txt
echo "Tests failed : $failed / $((passed + failed))" >> resultats_tests.txt