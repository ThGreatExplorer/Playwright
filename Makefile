# DIR -> directory to build
# EXE -> name of the executable
# TESTS -> number of tests, using format n-in.ss and n-out.ss for 0 to n exclusive
# FEEDBACK -> bool to indicate if feedback tests should be run

build:
		mkdir -p $(DIR) $(DIR)/Other $(DIR)/Tests
		printf "#!/bin/bash\njava -jar ./Other/$(EXE).jar\n" > $(DIR)/$(EXE) && chmod +x $(DIR)/$(EXE)
		sbt test
		sbt -DEXECUTABLE=$(EXE).jar -DHW="hw$(DIR)" assembly
		cp target/scala-3.7.2/$(EXE).jar $(DIR)/Other/$(EXE).jar
		make test
		if [ "$(FEEDBACK)" = "true" ] ; then make feedback ; fi

rebuild:
		sbt -DEXECUTABLE=$(EXE).jar -DHW="hw$(DIR)" assembly
		cp target/scala-3.7.2/$(EXE).jar $(DIR)/Other/$(EXE).jar
		make test

test:
		cd $(DIR) && \
		for t in $$(seq 0 $$(($(TESTS)-1))); do \
				echo "Running test $$t..."; \
				./$(EXE) < Tests/$$t-in.ss | diff - Tests/$$t-out.ss; \
		done

feedback:
		cd Feedback/$(DIR)/Tests && \
		for d in $$(find . -type d -mindepth 1 -maxdepth 1); do \
				echo "Running tests in $$d..."; \
				for t in $$(seq 0 $$(($(TESTS)-1))); do \
						if [ -e $$d/$$t-in.ss ]; then \
							echo "  Running test $$t..."; \
							../../../$(DIR)/$(EXE) < $$d/$$t-in.ss | diff --ignore-trailing-space --ignore-blank-lines - $$d/$$t-out.ss; \
						else \
							echo "    Test $$t does not exist, skipping..."; \
							continue; \
						fi; \
				done \
		done