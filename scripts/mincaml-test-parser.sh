#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

# TODO change this to point to your mincamlc executable if it's different, or add
# it to your PATH. Use the appropriate option to run the parser as soon
# as it is implemented
MINCAMLC=ocaml/mincamlc
OPTION=-p

# variables to count the number of passed and failed tests
passed=0
failed=0

# run all test cases in syntax/valid and make sure they are parsed without error
# run all test cases in syntax/invalid and make sure the parser returns an error

echo "---------- TESTING PARSER ----------"
for test_case in tests/syntax/valid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
        passed=$((passed+1))
    else 
        echo "KO"
        failed=$((failed+1))
    fi
done

for test_case in tests/syntax/invalid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
        failed=$((failed+1))
    else 
        echo "KO"
        passed=$((passed+1))
    fi
done

echo "\n---------- END TESTING ----------"
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they return KO)"
echo "Tests failed : $failed / $((passed + failed))"
echo "-----------------------------------\n"
echo "Parser" > resultats_tests.txt
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they return KO)" >> resultats_tests.txt
echo "Tests failed : $failed / $((passed + failed))\n" >> resultats_tests.txt