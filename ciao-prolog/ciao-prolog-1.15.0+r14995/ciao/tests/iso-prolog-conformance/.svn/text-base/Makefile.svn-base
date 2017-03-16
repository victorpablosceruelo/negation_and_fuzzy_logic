phony:
	@echo =====================================================
	@echo \'make compile\' compiles the test suite
	@echo \'make execute\' executes the test suite
	@echo \'make all\' compiles and executes
	@echo \'make clean\' removes the '*.po' and '*.itf'
	@echo \'make realclean\' '*.po', '*.itf', and executables
	@echo =====================================================
	@echo DO NOT erase data files in this directory
	@echo They are used by the test
	@echo =====================================================


EXEC_NAME=test_iso_conformance

clean:
	/bin/rm -f *.po *.itf

realclean: clean
	/bin/rm -f $(EXEC_NAME)

compile:
	ciaoc $(EXEC_NAME)

execute:
	./$(EXEC_NAME)

all: compile execute
