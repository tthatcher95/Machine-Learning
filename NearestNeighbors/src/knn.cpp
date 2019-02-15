#include <Eigen/Dense>
#include <algorithm>
#include <functional>
#include <array>
#include <iostream>
#include "knn.h"

int NN1toKmaxPredict_C(
  // inputs
  double *train_inputs_ptr, double *train_label_ptr,
  int n_observations, int n_features, int max_neighbors,
  double *test_input_ptr, // n_features
  // output
  double *test_prediction_ptr) // max_neighbors
  {

    // Eigen Library that we will be using:
    // VectorXd(n)
    // VectorXi
    // MatrixXd(n, p)
    // MatrixXi

    Eigen::VectorXd distance_vec(n_observations);
    Eigen::Map< Eigen::MatrixXd > train_inputs_mat(train_inputs_ptr, n_observations, n_features);
    Eigen::Map< Eigen::VectorXd > test_input_vec(test_input_ptr, n_features);
    Eigen::VectorXi sorted_index_vec(n_observations);

    // error checking
    if ( max_neighbors > n_observations ) {
      return ERROR_TOO_MANY_NEIGHBORS;
    }
    if ( max_neighbors < 1 ) {
      return ERROR_TOO_FEW_NEIGHBORS;
    }
    if ( train_inputs_mat.size() == 0 ) {
      return ERROR_NO_TRAIN_DATA;
    }
    if ( test_input_vec.size() == 0) {
      return ERROR_NO_TEST_DATA;
    }

    int row = 0;
    int neighbors = 0;
    double total_labels = 0;

    // Step 1: Compute Distances
    for(int i=0; i<n_observations; i++) {
      distance_vec(i) = (train_inputs_mat.row(i).transpose()-test_input_vec).array().abs().sum();
      sorted_index_vec(i) = i;
    }

    // Print before sorting
    // std::cout << sorted_index_vec << std::endl;

    // Step 2: Sort Indicies based on Indicies
    std::sort(sorted_index_vec.data(), // Gets first element in sorted_index_vec
              sorted_index_vec.data() + sorted_index_vec.size(), // Gets last element in sorted_index_vec

              // Lambda Function to sort based on distances
              [&distance_vec](int left, int right) {
                return distance_vec(left) < distance_vec(right);
              }
    );
    // Print after sorting
    // std::cout << sorted_index_vec << std::endl;

    // // Step 3: Compute Neighbors (Apply Equation)
    for(int k=0; k<max_neighbors; k++) {

      row = sorted_index_vec(k);
      neighbors = k+1;
      total_labels += train_label_ptr[row];

      // Total/k
      test_prediction_ptr[k] = total_labels/neighbors;

    }
    return 0;
  }