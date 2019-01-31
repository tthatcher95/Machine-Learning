#include <Eigen/Dense>
#include <algorithm>
#include <functional>
#include <array>
#include <iostream>

int Predict1toMaxNeighbors(double *train_inputs_ptr, double *train_label_ptr,
  int nrow, int ncol, int max_neighbors,
  double *test_input_ptr, // ncol
  double *test_prediction_ptr) // max_neighbors {

    // Eigen Library that we will be using:
    // VectorXd(n)
    // VectorXi
    // MatrixXd(n, p)
    // MatrixXi

    Eigen::VectorXd distance_vec(nrow);
    Eigen::Map< Eigen::MatrixXd > train_inputs_mat(train_inputs_ptr, nrow, ncol);
    Eigen::Map< Eigen::MatrixXd > train_inputs_vec(test_inputs_ptr, ncol);
    Eigen::VectorXd diff_vec(ncol);
    Eigen::VectorXi sorted_index_vec(nrow);
    
    int row = 0;
    int neighbors = 0;
    double total_labels = 0;

    // Step 1: Compute Distances
    for(int i=0; i<nrow; i++) {
      diff_vec = train_inputs_mat.row(i).transpose() - test_input_vec;
      distance_vec(i) = diff_vec.norm();
      sorted_index_vec(i) = i;

    }
    // Print before sorting
    // std::cout << sorted_index_vec << std::endl;

    // Step 2: Sort Indicies based on Indicies
    std::sort(sorted_index_vec.data(), // Gets first element in sorted_index_vec
              sorted_index_vec.data() + sorted_index_vec.size()) // Gets last element in sorted_index_vec

              // Lambda Function to sort based on distances
              [&distance_vec](int left, int right) {
                return distance_vec(left) < distance_vec(right);
              };
    // Print after sorting
    // std::cout << sorted_index_vec << std::endl;

    // Step 3: Compute Neighbors (Apply Equation)
    for(int k=0; k<max_neighbors; k++) {

      row = sorted_index_vec(k);
      neighbors = k+1;
      total_labels += train_label_ptr[row];

      // Total/k
      test_prediction_ptr[k] = total_labels/neighbors;

    }
    return 0;
  }
