one:
	sbt test
	sbt assembly
	cp target/scala-3.7.2/xcount.jar 1/Other/xcount.jar
	cd 1 && ./xcount < Tests/0-in.ss | diff - Tests/0-out.ss