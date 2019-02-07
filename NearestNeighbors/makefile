knn: knn.o
	g++ knn.o -o knn

knn.o: knn.cpp knn.h
	g++ -c -I ../../eigen knn.cpp

clean:
	rm -f *.o knn
