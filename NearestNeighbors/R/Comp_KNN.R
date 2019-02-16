# # dont understand how to write this need to revisit
# data(zip.train, package="ElemStatLearn")
# x <- zip.train[1:5, -1]
# y <- zip.train[1:5, 1]
# 
# testx <- zip.train[6, -1]
# max_neighbors = 3
# ret <- vector(mode="double", length=max_neighbors)
# testy <- zip.train[6, 1]
# 
# # if the below line is uncommented then it fails to build dont know why
# #.C("NN1toKmaxPredict", as.double(x), as.double(y), as.integer(nrow(x)), as.integer(ncol(x)), as.integer(max_neighbors), as.double(testx), as.double(ret), PACKAGE="NearestNeighbors")
# #testy
# testy
