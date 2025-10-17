# DIR -> directory to build
# EXE -> name of the executable
# TESTS -> number of tests, using format n-in.ss and n-out.ss for 0 to n exclusive
# FEEDBACK -> bool to indicate if feedback tests should be run

init:
	mkdir -p $(DIR) $(DIR)/Other $(DIR)/Tests
	printf "#!/bin/bash\njava -jar ./Other/$(EXE).jar\n" > $(DIR)/$(EXE) && chmod +x $(DIR)/$(EXE)
	for t in $$(seq 0 $$(($(TESTS)-1))); do \
		touch $(DIR)/Tests/$$t-in.ss $(DIR)/Tests/$$t-out.ss; \
	done

jar: 
	sbt -DEXECUTABLE=$(EXE).jar -DHW="hw$(DIR)" assembly
	cp target/scala-3.7.2/$(EXE).jar $(DIR)/Other/$(EXE).jar

test: jar
	make coverage
	make itest

itest:
	cd $(DIR) && \
	for t in $$(seq 0 $$(($(TESTS)-1))); do \
			echo "Running test $$t..."; \
			exec ./$(EXE) < Tests/$$t-in.ss | diff --ignore-trailing-space --ignore-blank-lines - Tests/$$t-out.ss; \
	done
	if [ "$(FEEDBACK)" = "true" ] ; then make feedback ; fi

stest:
	cd $(DIR) && \
	for test_file in ../src/test/ForStudents/ForStudents_$(DIR)/*-in.ss; do \
			if [ -f "$$test_file" ]; then \
					test_num=$$(basename "$$test_file" -in.ss); \
					echo "Running test $$test_num..."; \
					exec ./$(EXE) < "$$test_file" | diff --ignore-trailing-space --ignore-blank-lines - ../src/test/ForStudents/ForStudents_$(DIR)/"$$test_num-out.ss"; \
			fi; \
	done

feedback:
		cd $(DIR) && \
		for d in $$(find ../Feedback/$(DIR)/Tests -type d -mindepth 1 -maxdepth 1); do \
				echo "Running tests in $$d..."; \
				for t in $$(seq 0 $$(($(TESTS)-1))); do \
						if [ -e $$d/$$t-in.ss ]; then \
							echo "  Running test $$t..."; \
							exec ./$(EXE) < $$d/$$t-in.ss | diff --ignore-trailing-space --ignore-blank-lines - $$d/$$t-out.ss; \
						else \
							echo "    Test $$t does not exist, skipping..."; \
							continue; \
						fi; \
				done \
		done

coverage:
	sbt compile coverage test
	sbt coverageAggregate
	firefox target/scala-3.7.2/scoverage-report/index.html