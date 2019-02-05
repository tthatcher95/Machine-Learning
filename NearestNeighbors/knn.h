int Predict1toMaxNeighbors(  double *train_inputs_ptr, double *train_label_ptr,
                             int nrow, int ncol, int max_neighbors,
                             double *test_input_ptr,
                             double *test_prediction_ptr);

int NNCV( double *train_inputs_ptr,
          double *train_label_ptr,
          int kmax, int fmax);

double validError(  double *train_inputs_ptr, double *train_label_ptr,
                    double *test_input_ptr, int kmax, double *test_label_ptr );
