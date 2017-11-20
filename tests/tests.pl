:- load_files([
    tests/abduction_test,
    tests/deduction_test,
    tests/intersection_test,
    tests/basic_test,
    tests/weights_test,
    tests/explain_test,
    tests/trig_test,
    tests/lexinf_test,
    tests/lexinf_learn_test,
    tests/query_test,
    tests/solve_test
], [ if(not_loaded) ]).
