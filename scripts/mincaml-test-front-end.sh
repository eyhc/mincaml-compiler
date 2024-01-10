#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
ASML=tools/asml
OPTION=-asml

# clean the generated asml files and results files
rm tests/gen-code/*.asml 2> /dev/null 1> /dev/null
rm tests/gen-code/*.actual 2> /dev/null 1> /dev/null

# variables to count the number of passed and failed tests
passed=0
failed=0

# run one test case for each iteration in gen-code and make sure the asml code generated is correct and give the expected result
echo "---------- TESTING FRONT-END PASSES TO ASML GENERATION ----------"
echo "for Iteration 1 : arithmetic operations"
test_case=tests/gen-code/simple_addition.ml
asml_generated=tests/gen-code/simple_addition.asml
result=tests/gen-code/simple_addition_asml.actual
expected=tests/gen-code/simple_addition_asml.expected
echo "testing compiler on: $test_case"
echo "Nothing to print" > $result
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\nfor Iteration 2 : call to external functions"
test_case=tests/gen-code/call_to_print_int.ml
asml_generated=tests/gen-code/call_to_print_int.asml
result=tests/gen-code/call_to_print_int_asml.actual
expected=tests/gen-code/call_to_print_int_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\nfor Iteration 3 : if_then_else"
test_case=tests/gen-code/if_then_else.ml
asml_generated=tests/gen-code/if_then_else.asml
result=tests/gen-code/if_then_else_asml.actual
expected=tests/gen-code/if_then_else_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\nfor Iteration 4 : functions"
test_case=tests/gen-code/definition_function.ml
asml_generated=tests/gen-code/definition_function.asml
result=tests/gen-code/definition_function_asml.actual
expected=tests/gen-code/definition_function_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\nfor Iteration 5 : arrays and tuples"
test_case=tests/gen-code/manipulate_tuple.ml
asml_generated=tests/gen-code/manipulate_tuple.asml
result=tests/gen-code/manipulate_tuple_asml.actual
expected=tests/gen-code/manipulate_tuple_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

test_case=tests/gen-code/change_value_in_array.ml
asml_generated=tests/gen-code/change_value_in_array.asml
result=tests/gen-code/change_value_in_array_asml.actual
expected=tests/gen-code/change_value_in_array_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\nfor Iteration 6 : closure"
test_case=tests/gen-code/closure.ml
asml_generated=tests/gen-code/closure.asml
result=tests/gen-code/closure_asml.actual
expected=tests/gen-code/closure_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\nfor Iteration 7 : floats"
test_case=tests/gen-code/float_tab.ml
asml_generated=tests/gen-code/float_tab.asml
result=tests/gen-code/float_tab_asml.actual
expected=tests/gen-code/float_tab_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

test_case=tests/gen-code/call_to_truncate.ml
asml_generated=tests/gen-code/call_to_truncate.asml
result=tests/gen-code/call_to_truncate_asml.actual
expected=tests/gen-code/call_to_truncate_asml.expected
echo "testing compiler on: $test_case"
echo $($MINCAMLC $OPTION "$test_case") 2> $asml_generated 1> $asml_generated
echo $($ASML "$asml_generated") 2> $result 1> $result
echo $(diff -s "$result" "$expected")
if diff -s "$result" "$expected" 2> /dev/null 1> /dev/null
    then 
        echo "OK"
        passed=$((passed+1))
    else
        echo "KO"
        failed=$((failed+1))
fi

echo "\n---------- END TESTING ----------"
echo "Tests passed : $passed / $((passed + failed))"
echo "Tests failed : $failed / $((passed + failed))"
echo "-----------------------------------\n"
echo "Generation asml" >> resultats_tests.txt
echo "Tests passed : $passed / $((passed + failed))" >> resultats_tests.txt
echo "Tests failed : $failed / $((passed + failed))" >> resultats_tests.txt