#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
OPTION=-t
tests="tests/typechecking/"
old_topic_num=-1

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'


# variables to count the number of passed and failed tests
passed=0
failed=0

# run all test cases in typechecking/valid and make sure they are typechecked without error
# run all test cases in typechecking/invalid and make sure the compiler returns an error
echo "---------------- TESTING TYPECHECKER ----------------\n"
for dir_tests in $(ls "${tests}")
do 
    echo "$(echo "$dir_tests" | tr '[:lower:]' '[:upper:]') TESTS"
    for dir in $(ls "${tests}${dir_tests}")
    do
        topic_num=$(echo "$dir" | cut -d'-' -f1)
        topic_name=$(echo "$dir" | cut -d'-' -f2)
        echo "\n\t - Iteration $topic_num : $topic_name - "
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
                if [ "$valid" = "valid" ];
                then
                    echo "${GREEN} OK${RESET}"
                    passed=$((passed+1))
                else
                    echo "${RED} KO${RESET}"
                    failed=$((failed+1))
                fi;
            else
                if [ "$valid" = "valid" ];
                then
                    echo "${RED} KO${RESET}"
                    failed=$((failed+1))
                else
                    echo "${GREEN} OK${RESET}"
                    passed=$((passed+1))
                fi;
            fi
            old_topic_num=$topic_num; 
        done
    done
    if [ "$dir_tests" = "invalid" ];
    then
        echo "\n..................................................."
    else
        echo
    fi
done

# show the results of the tests
echo "---------- END TESTING ----------"
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they succeed to fail)"
echo "Tests failed : $failed / $((passed + failed))"
echo "-----------------------------------\n"

# save the results into resultats_tests.txt
echo "\n- Typechecker -" >> resultats_tests.txt
echo "Tests passed : $passed / $((passed + failed)) (invalid tests are passed if they succeed to fail)" >> resultats_tests.txt
echo "Tests failed : $failed / $((passed + failed))" >> resultats_tests.txt