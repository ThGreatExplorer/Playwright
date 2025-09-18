# DIR -> directory to build
# EXE -> name of the executable
# TESTS -> number of tests, using format n-in.ss and n-out.ss for 0 to n exclusive

build:
		mkdir -p $(DIR) $(DIR)/Other $(DIR)/Tests
	  printf "#!/bin/bash\njava -jar ./Other/$(EXE).jar\n" > $(DIR)/$(EXE) && chmod +x $(DIR)/$(EXE)
		sbt test
		sbt -DEXECUTABLE=$(EXE).jar -DHW="hw$(DIR)" assembly
		cp target/scala-3.7.2/$(EXE).jar $(DIR)/Other/$(EXE).jar
		make test

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