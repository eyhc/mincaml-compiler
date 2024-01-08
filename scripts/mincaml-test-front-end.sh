#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
ASML=tools/asml
OPTION=-asml

# run one test case for each iteration in gen-code and make sure the asml code generated is correct

echo "TESTING FRONT-END PASSES TO ASML GENERATION"
echo "for Iteration 1 : arithmetic operations"
test_case=tests/gen-code/test_simple_addition.ml
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> tests/gen-code/test_asml_simple_addition.asml 1> tests/gen-code/test_asml_simple_addition.asml
echo $($ASML -v tests/gen-code/test_asml_simple_addition.asml) 2> tests/gen-code/test_asml_simple_addition.actual 1> tests/gen-code/test_asml_simple_addition.actual
echo $(diff -s tests/gen-code/test_asml_simple_addition.actual tests/gen-code/test_asml_simple_addition.expected)
if diff -s tests/gen-code/test_asml_simple_addition.actual tests/gen-code/test_asml_simple_addition.expected 2> /dev/null 1> /dev/null
    then 
        echo "OK"
    else
        echo "KO"
fi

echo "\nfor Iteration 2 : call to external functions"
test_case=tests/gen-code/test_call_to_print_int.ml
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> tests/gen-code/test_asml_call_to_print_int.asml 1> tests/gen-code/test_asml_call_to_print_int.asml
echo $($ASML tests/gen-code/test_asml_simple_addition.asml) 2> tests/gen-code/test_asml_simple_addition.actual 1> tests/gen-code/test_asml_simple_addition.actual
echo $(diff -s tests/gen-code/test_asml_call_to_print_int.actual tests/gen-code/test_asml_call_to_print_int.expected)
if diff -s tests/gen-code/test_asml_call_to_print_int.actual tests/gen-code/test_asml_call_to_print_int.expected 2> /dev/null 1> /dev/null
    then 
        echo "OK"
    else
        echo "KO"
fi

echo "\nfor Iteration 3 : if_then_else"
test_case=tests/gen-code/test_if_then_else.ml
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> tests/gen-code/test_asml_if_then_else.asml 1> tests/gen-code/test_asml_if_then_else.asml
echo $($ASML -v tests/gen-code/test_asml_simple_addition.asml) 2> tests/gen-code/test_asml_simple_addition.actual 1> tests/gen-code/test_asml_simple_addition.actual
echo $(diff -s tests/gen-code/test_asml_if_then_else.actual tests/gen-code/test_asml_if_then_else.expected)
if diff -s tests/gen-code/test_asml_if_then_else.actual tests/gen-code/test_asml_if_then_else.expected 2> /dev/null 1> /dev/null
    then 
        echo "OK"
    else
        echo "KO"
fi