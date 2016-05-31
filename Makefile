all: run

run:
	lein cljsbuild once

clean:
	lein clean
