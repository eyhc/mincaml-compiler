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
# run all test cases in syntax/invalid and make sure the parser returns an error

echo -e "------------------ TESTING PARSER ------------------\n"
for test_case in tests/syntax/valid/*.ml
do
    file=$(basename $test_case)
    file_name=$(echo $file | cut -d'-' -f2)

    echo -n "Test on: "$test_case" ..."
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then 
        echo -e "${GREEN} OK${RESET}"
        passed=$((passed+1))
    else
        echo -e "${RED} KO${RESET}"
        failed=$((failed+1))
    fi
done

for test_case in tests/syntax/invalid/*.ml
do
    file=$(basename $test_case)
    file_name=$(echo $file | cut -d'-' -f2)

    echo -n "Test on: "$test_case" ..."
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo -e "${GREEN} OK${RESET}"
        failed=$((failed+1))
    else 
        echo -e "${RED} KO${RESET}"
        passed=$((passed+1))
    fi
done

echo -e "\n---------- END TESTING ----------"
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they return KO)"
echo "Tests failed : $failed / $((passed + failed))"
echo -e "-----------------------------------\n"
echo "RESUME DES TESTS" > resultats_tests.txt
echo "- Parser -" >> resultats_tests.txt
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they return KO)" >> resultats_tests.txt
echo -e "Tests failed : $failed / $((passed + failed))" >> resultats_tests.txt