.PHONY: all clean

all: fastcast.py

run: fastcast.py
	python3 fastcast-gui.py

fastcast.py: lib *.fut
	futhark-pyopencl --library fastcast.fut

lib:
	futhark-pkg sync

clean:
	rm -f fastcast.py
	rm -rf lib
