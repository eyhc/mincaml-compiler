#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

# TODO change this to point to your mincamlc executable if it's different, or add
# it to your PATH. Use the appropriate option to run the parser as soon
# as it is implemented
MINCAMLC=ocaml/mincamlc
OPTION=-p

# run all test cases in syntax/valid and make sure they are parsed without error
# run all test cases in syntax/invalid and make sure the parser returns an error

# TODO extends this script to run test in subdirectories
# 
echo "---------- TESTING PARSER ----------"
for test_case in tests/syntax/valid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

for test_case in tests/syntax/invalid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

echo "---------- END TESTING ----------\n"