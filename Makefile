all: test

SWIPL = swipl -p library=prolog
test:
	$(SWIPL) -l tests/tests.pl -g run_tests,halt

test-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt


