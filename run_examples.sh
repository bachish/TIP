for test_case in examples/*.tip
do
  echo "test case: "
  echo $test_case
    ./tip -types $test_case
done
