all: test

test: test.o
	g++ test.o -o test `pkg-config --libs gdal`

test.o: test.cpp
	g++ -c test.cpp `pkg-config --cflags gdal`


