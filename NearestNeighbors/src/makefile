CXX_STD = CXX11 #generates -std=gnu++11 option for g++ when compiling
# PKG_CPPFLAGS=-UNDEBUG -g -fopenmp #passed to g++ when compiling
# PKG_LIBS=-lgomp #passed to g++ when creating a shared library

knn: knn.o interface.o
	g++ knn.o interface.o -o knn -I /usr/share/R/include/

knn.o: knn.cpp knn.h
	g++ -c -I /usr/include/eigen3 knn.cpp

interface.o: interface.cpp
	g++ -c interface.cpp -o interface.o -I /usr/share/R/include/ /usr/share/R/include/R_ext/

clean:
	rm -f *.o knn
