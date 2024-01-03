#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
OPTION=-t

# run all test cases in typechecking/valid and make sure they are typechecked without error
# run all test cases in typechecking/invalid and make sure the compiler returns an error
 
echo "SIMPLES TYPECHECKING"
for test_case in tests/typechecking/valid/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

for test_case in tests/typechecking/invalid/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

echo "ITERATION 1 : ARITHMETIC OPERATIONS"
for test_case in tests/typechecking/valid/arithmetic_operations/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

for test_case in tests/typechecking/invalid/arithmetic_operations/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

echo "ITERATION 2 : CALL TO EXTERNAL FUNCTION"
for test_case in tests/typechecking/valid/call_to_external_functions/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

for test_case in tests/typechecking/invalid/call_to_external_functions/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

echo "ITERATION 3 : IF_THEN_ELSE"
for test_case in tests/typechecking/valid/if_then_else/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done

for test_case in tests/typechecking/invalid/if_then_else/*.ml
do
    echo "testing compiler on: $test_case"
    if $MINCAMLC $OPTION "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO"
    fi
done