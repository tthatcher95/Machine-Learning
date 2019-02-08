#include <R.h>//for error
#include <R_ext/Rdynload.h>//for registration
#include "knn.h"

void NN1toKmaxPredict(double *train_inputs_ptr, double *train_label_ptr,
                      int nrow, int ncol, int max_neighbors,
                      double *test_input_ptr,
                      double *test_prediction_ptr) {

  int status = NN1toKmaxPredict_C(train_inputs_ptr, train_label_ptr,
                                  nrow, ncol, max_neighbors,
                                  test_input_ptr,
                                  test_prediction_ptr);

  if(status == ERROR_TOO_MANY_NEIGHBORS){
      error("too many neighbors (should be at most nrow)");
  }
  if(status == ERROR_TOO_FEW_NEIGHBORS){
    error("too few neighbors (should be at least 1)");
  }
  if(status == ERROR_NO_TRAIN_DATA){
    error("no train data");
  }
  if(status == ERROR_NO_TEST_DATA){
    error("no test data");
  }
}

// Code required to register the interface function with R.
R_CMethodDef cMethods[] = {
  {"my_mean_interface", (DL_FUNC) &my_mean_interface, 3},
  {NULL, NULL, 0}
};
extern "C" {
  void R_init_NearestNeighbors(DllInfo *info) { // Make sure it is R_init_yourPackageName
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    //R_useDynamicSymbols call says the DLL is not to be searched for
    //entry points specified by character strings so .C etc calls will
    //only find registered symbols.
    R_useDynamicSymbols(info, FALSE);
  }
}
