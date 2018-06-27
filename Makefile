all: fastcast.py

run: fastcast.py
	python3 fastcast-gui.py

fastcast.py: *.fut
	futhark-pyopencl --library fastcast.fut
