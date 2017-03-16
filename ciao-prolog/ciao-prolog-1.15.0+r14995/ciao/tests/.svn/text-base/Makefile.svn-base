CIAOBUILDDIRtemp=../../build
include $(CIAOBUILDDIRtemp)/CONFIG_mkf

phony:
	@echo "Say make <something>, please"

test: distclean compile run

test-local: distclean compile-local run-local

compile: gen-ciaoc
	$(CIAOC) -v run_tests
	@echo "exec $(BINROOT)/ciao-shell \$$0 \"\$$@\"" > test_scripts/exec_pldiff

compile-local: gen-ciaoc
	$(SETLOCALCIAO) $(CIAOC) -v -x run_tests
	@echo "exec ../../bin/ciao-shell \$$0 \"\$$@\"" > test_scripts/exec_pldiff

run:
	./run_tests

run-local:
	$(SETLOCALCIAO) ./run_tests

gen-ciaoc:
	@/bin/rm -f ciaoc_name.pl
	@echo "ciaoc_name('$(CIAOC)')." > ciaoc_name.pl

distclean:
	find . -name '*.po' -exec rm {} \;
	find . -name '*.itf' -exec rm {} \;
	find . -name '*.o' -exec rm {} \;
	find . -name '*.so' -exec rm {} \;
	find . -name '*.dll' -exec rm {} \;
	find . -name '*_glue.c' -exec rm {} \;
	find . -name 'tmpciao*' -exec rm {} \;
	find . -name '*.log' -exec rm {} \;
	-rm -f load_dynlibs/load_dynlibs
	-rm -f object_test/object_test
	-rm -f test_java/plserver
	-rm -f test_java/j2pl_test.class
	-rm -f actmods_test/simple_server
	-rm -f actmods_test/simple_client_with_main 
	-rm -f actmods_test/*.addr
	-rm -f persistentdb/persistentdb
	-rm -f persistentdb/queue 
	-rm -f persistentdb/example_static
	-rm -f persistentdb/example_dynamic 
	-rm -f remote_exec/client
	-rm -f remote_exec/server
	-rm -f xml_indexed.pl
	-rm -f run_tests
	-rm -rf pers
	-rm -rf pers_queue

cleanbackups:
	find . -name '*~' -exec rm {} \;

totalclean: distclean cleanbackups

shell:
	$(SETLOCALCIAO) $(CIAOSHELL)
