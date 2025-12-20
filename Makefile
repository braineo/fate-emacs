.PHONY: all
all: prepare

.PHONY: prepare
prepare:
	bash ./bin/prepare.bash

.PHONY: test
test:
	@for file in .github/test-*.el; do \
		echo "Running tests in $$file"; \
		emacs -nw --batch -L modules -l "$$file" -f ert-run-tests-batch-and-exit; \
	done
