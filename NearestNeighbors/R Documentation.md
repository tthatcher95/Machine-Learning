** How to use NN1toKmaxPredict**

train_inputs_ptr <- c(1, 2.5, 4.5)
train_label_ptr <- c(1, 2, 3)
nrow <- 3
ncol <- 1
max_neighbors <- 2
test_input_ptr <- 2
test_prediction_ptr <- c(0, 0, 0)

.C("NN1toKmaxPredict", as.double(train_inputs_ptr), as.double(train_label_ptr),
                       as.int(nrow), as.int(ncol), as.int(max_neighbors),
                       as.double(test_input_ptr), as.double(test_prediction_ptr))
