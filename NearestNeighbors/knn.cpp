#include <Eigen/Dense>
#include <algorithm>
#include <functional>
#include <array>
#include <iostream>
#include "knn.h"

int NN1toKmaxPredict_C(
  // inputs
  double *train_inputs_ptr, double *train_label_ptr,
  int nrow, int ncol, int max_neighbors,
  double *test_input_ptr, // ncol
  // output
  double *test_prediction_ptr) // max_neighbors
  {

    // Eigen Library that we will be using:
    // VectorXd(n)
    // VectorXi
    // MatrixXd(n, p)
    // MatrixXi

    Eigen::VectorXd distance_vec(nrow);
    Eigen::Map< Eigen::MatrixXd > train_inputs_mat(train_inputs_ptr, nrow, ncol);
    Eigen::Map< Eigen::VectorXd > test_input_vec(test_input_ptr, ncol);
    Eigen::VectorXi sorted_index_vec(nrow);

    int row = 0;
    int neighbors = 0;
    double total_labels = 0;

    // Step 1: Compute Distances
    for(int i=0; i<nrow; i++) {
      distance_vec(i) = (train_inputs_mat.row(i).transpose()-test_input_vec).array().abs().sum();
      sorted_index_vec(i) = i;
    }

    // Print before sorting
    // std::cout << sorted_index_vec << std::endl;

    // Step 2: Sort Indicies based on Indicies
    std::sort(sorted_index_vec.data(), // Gets first element in sorted_index_vec
              sorted_index_vec.data() + sorted_index_vec.size()), // Gets last element in sorted_index_vec

              // Lambda Function to sort based on distances
              [&distance_vec](int left, int right) {
                return distance_vec(left) < distance_vec(right);
              };
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


int main()
{
  int nrow = 3;
  int ncol = 2;
  int max_neighbors = 3;
  int n = nrow * ncol;

  double *train_inputs_ptr = new double[n];
  for (int i = 0; i < n; i+= 1)
  {
    train_inputs_ptr[i] = double(i + 1.0);
  }
  double *train_label_ptr = new double[nrow];
  for (int i = 0; i < nrow; i+= 1)
  {
    train_label_ptr[i] = double(i + 1);
  }
  double *test_input_ptr = new double[ncol];

  for (int i = 0; i < ncol; i+= 1) {
    test_input_ptr[i] = double(i + 1.5);
  }


  double *test_prediction_ptr = new double[nrow];

  NN1toKmaxPredict_C( train_inputs_ptr, train_label_ptr,
                          nrow, ncol, max_neighbors,
                          test_input_ptr, test_prediction_ptr);

  for(int i = 0; i < max_neighbors; i++)
  {
    std::cout << "neighbor "<< i + 1<< " prediction: " << test_prediction_ptr[i] << '\n';
  }

  return 0;
}
