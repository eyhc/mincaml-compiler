#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
OPTION=-t
tests="tests/typechecking/"
valid_tests="tests/typechecking/valid/"
# test topics
topics=("simples typechecking" "arithmetic operations" "call to external functions"
    "if_then_else" "functions" "arrays and tuples" 
    "closure" "floats" )
old_topic_num=-1
RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'


# variables to count the number of passed and failed tests
passed=0
failed=0

# run all test cases in typechecking/valid and make sure they are typechecked without error
# run all test cases in typechecking/invalid and make sure the compiler returns an error
echo -e "---------- TESTING TYPECHECKER ----------\n"
for dir_tests in $(ls "${tests}")
do 
    echo "${dir_tests^^} TESTS"
    for dir in $(ls "${tests}${dir_tests}")
    do
        topic_num=$(echo "$dir" | cut -d'-' -f1)
        (( topic_num != old_topic_num )) &&  echo && echo -e "\t - Iteration $topic_num : ${topics[topic_num]} - "
        tests_abs=$(pwd)"/${tests}${dir_tests}/${dir}/"
        test_files=`ls "$tests_abs"*.ml`
        for test_case in $test_files
        do
            file_name=$(basename $test_case)  
            test_file=$(echo $test_case | cut -d'/' -f6,7,8,9,10) 
            valid=$(echo $test_case | cut -d'/' -f8) 
            echo -n "Test on: "$file_name" ..."
            if $MINCAMLC $OPTION "$test_file" 2> /dev/null 1> /dev/null
            then
                echo -e "${GREEN} OK${RESET}"
                if [[ $valid = "valid" ]]
                then
                    passed=$((passed+1))
                else
                    failed=$((failed+1))
                fi
            else
                echo -e "${RED} KO${RESET}"
                if [[ $valid = "valid" ]]
                then
                    failed=$((failed+1))
                else
                    passed=$((passed+1))
                fi
            fi
            old_topic_num=$topic_num; 
            num_test=$(($num_test+1))
        done
    done
    if [[ $dir_tests = "invalid" ]]
    then
        echo -e "\n..................................................."
    else
        echo
    fi
done

echo "---------- END TESTING ----------"
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they return KO)"
echo "Tests failed : $failed / $((passed + failed))"
echo -e "-----------------------------------\n"
echo -e "\n- Typechecker -" >> resultats_tests.txt
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they return KO)" >> resultats_tests.txt
echo "Tests failed : $failed / $((passed + failed))" >> resultats_tests.txt