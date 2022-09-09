# Makefile for testing KiCS2

########################################################################
# Testing: run test suites to check the installation
#

# Executable of CurryCheck:
CURRYCHECK := $(shell which curry-check)

export RUNTESTPARAMS=

# run the test suites to check the installation
.PHONY: runtest
runtest:
	@if [ ! -x "$(CURRYCHECK)" ] ; then \
	  echo "Executable 'curry-check' is not installed!" && echo "To run the tests, install it by > cypm install currycheck" ; \
	else $(MAKE) runalltests ; fi

.PHONY: runalltests
runalltests:
	cd testsuite && ./test.sh $(RUNTESTPARAMS)
	$(MAKE) -C currytools runtest

# run the test suites in verbose mode so that all output is shown:
.PHONY: runtestverbose
runtestverbose:
	$(MAKE) runtest RUNTESTPARAMS=-v
