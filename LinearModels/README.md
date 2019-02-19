Coding project 2: linear models for regression and binary classification.

For this project you will be writing an R package
that implements gradient descent algorithms for linear models.
You are allowed and encouraged to discuss your code/ideas with other students,
but it is strictly forbidden to copy any code/documentation/tests/etc from other groups,
or any web pages. Your code/project must be written from scratch by the members of your group.
Any violations will result in one or more of the following:
- a zero on this assignment,
- a failing grade in this class,
- expulsion from NAU.

* Algorithms to code
By now you know how to code functions in C++ or R so you have the choice.
- if you code the algorithm in C++, write an R function that calls your C++ code via .C
- if you code the algorithm in pure R, use matrix/vector arithmetic for efficiency (do NOT use for loops over data points or features)

** Linear models with early stopping regularization
R Function names: LMSquareLossIterations, LMLogisticLossIterations
- Inputs: X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
- Output: W.mat, matrix of weight vectors, one for each iteration, n_features x max.iterations.
  Should be able to get a matrix of predictions via X.mat %*% W.mat
- there should be one function for the square loss, one function for the logistic loss.
- both functions should optimize the mean loss (not the total loss).
- make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column,
  and keep track of the mean/sd of each column, so you can return W.mat on the original scale
  (if you use the unscaled X.mat during gradient descent, it will not converge -- numerical instability).
R Function names: LMSquareLossEarlyStoppingCV, LMLogisticLossEarlyStoppingCV
- Inputs: X.mat, y.vec, fold.vec, max.iterations.
- should use K-fold cross-validation based on the fold IDs provided in fold.vec
- for each train/validation split, use LM___LossIterations to compute a sequence of models on the train data,
  then compute the validation loss of each model.
- compute mean.validation.loss.vec, which is a vector (with max.iterations elements) of mean validation loss over all K folds.
- minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
- finally use LM__LossIterations(max.iterations=selected.steps) on the whole training data set.
- Output a list with the following named elements:
  - mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
  - selected.steps
  - weight.vec, the weight vector found by using gradient descent with selected.steps on the whole training data set.
  - predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions
    (real numbers for regression, probabilities for binary classification).

** Linear models with L2 regularization
R Function names: LMSquareLossL2, LMLogisticLossL2.
- Inputs: X.scaled.mat, y.vec, penalty (non-negative numeric scalar), opt.thresh (positive numeric scalar), initial.weight.vec.
- these functions should assume that X.scaled.mat already has mean=0 and sd=1 for each of its columns,
  and its job is to find the optimal weight vector that minimizes the following cost function:
  \sum_{i=1}^n L[w^T x_i, y_i] + penalty * ||w||, where L is either the logistic or square loss.
- On the documentation pages for these functions you should write the math/equation of the cost function that it minimizes.
- opt.thresh should be a threshold on the L1-norm (sum of absolute values) of the gradient.
  I will test your code to make sure that the L1-norm of the gradient of your solution is less than opt.thresh.
- Output: optimal weight vector for the given penalty parameter.
R Function names: LMSquareLossL2penalties, LMLogisticLossL2penalties.
- Inputs: X.mat, y.vec, penalty.vec (vector of decreasing penalty values)
- this function should begin by scaling X.mat to obtain X.scaled.mat with each column mean=0 and sd=1
- it should then loop over penalty values, calling LM__LossL2 to get the optimal weight vector for each.
- Output: W.mat (n_features x n_penalties), weight matrix on original scale,
  that can be used to get predictions via X.mat %*% W.mat
R Function names: LMSquareLossL2CV, LMLogisticLossL2CV.
- Inputs: X.mat, y.vec, fold.vec, penalty.vec
- should use K-fold cross-validation based on the fold IDs provided in fold.vec
- for each train/validation split, use LM___LossL2penalties to compute optimal L2-penalized models on the train data,
  then compute the validation loss of each model.
- compute mean.validation.loss.vec, which is a vector (with n_penalties elements) of mean validation loss over all K folds.
- minimize the mean validation loss to determine selected.penalty, the optimal penalty value.
- finally use LM__LossL2penalties(penalty.vec=selected.penalty) on the whole training data set.
- Output a list with the following named elements:
  - mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty (for plotting train/validation loss curves)
  - weight.vec, the weight vector found by using gradient descent with selected.penalty on the whole training data set.
  - predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions
    (real numbers for regression, probabilities for binary classification).

** Documentation and tests
- for each R function, write documentation with at least one example of how to use it.
- write at least two tests for each R function, in tests/testthat/test-xxx.R.
    You should at least test that
    (1) for valid inputs your function returns an output of the expected type/dimension,
    (2) for an invalid input, your function stops with an informative error message.

* Experiments/application: run your code on the following data sets.
- Binary classification: LMLogisticLossL2CV, LMLogisticLossEarlyStoppingCV
  - ElemStatLearn::spam 2-class [4601, 57] output is last column (spam).
  - ElemStatLearn::SAheart 2-class [462, 9] output is last column (chd).
  - ElemStatLearn::zip.train: 10-class [7291, 256] output is first column. (ignore classes other than 0 and 1)
- Regression: LMSquareLossL2CV, LMSquareLossEarlyStoppingCV
  - ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
  - ElemStatLearn::ozone [111 x 3] output is first column (ozone).
- For each data set, use 4-fold cross-validation to evaluate the prediction accuracy of your code.
  For each split s=1 to 4, set aside the data in fold s as a test set.
  Use ___CV to train a model on the other folds
  (which should be used in your ___CV function as internal train/validation sets/splits),
  then make a prediction on the test fold s.
- For each train/test split,
  to show that your algorithm is actually learning something
  non-trivial from the inputs/features,
  compute a baseline predictor that ignores the inputs/features.
  - Regression: the mean of the training labels/outputs.
  - Binary classification: the most frequent class/label/output in the training data.
- For each data set, compute a 4 x 3 matrix of mean test loss values:
  - each of the four rows are for a specific test set,
  - the first column is for the early stopping predictor,
  - the second column is for the L2 regularized predictor,
  - the third column is for the baseline/un-informed predictor.
- Make one or more plot(s) or table(s) that compares these test loss values.
  For each of the five data sets,
  is early stopping more accurate than L2 regularization?
  Are the linear models more accurate than the baseline?
- for each data set, run ___CV functions on the entire data set,
  and plot the mean validation loss as a function of the regularization parameter.
  plot the mean train loss in one color, and the mean validation loss in another color.
  Plot a point and/or text label to emphasize the regularization parameter
  selected by minimizing the mean validation loss function.
- Write up your results in vignettes/report.Rmd that shows the R code that you used
  for the experiments/application, along with the output.
  - Documentation: [[http://r-pkgs.had.co.nz/vignettes.html][Vignettes chapter of R packages book]].
  - Example [[https://github.com/cran/glmnet/blob/master/vignettes/glmnet_beta.Rmd][Rmd vignette source code]].
    [[https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html][vignette rendered to HTML]].
  - For this assignment the headings should be as follows:

#+BEGIN_SRC
## Data set 1: spam

### Matrix of loss values

print out and/or plot the matrix.

comment on difference in accuracy.

### Train/validation loss plots

plot the two loss functions.

What are the optimal regularization parameters?

## Data set 2: ...
#+END_SRC

*** Grading rubric: 100 points.
Your group should submit a link to your repo on GitHub.
- 20 points for completeness of report.
  - 4 points for each data set (2 points each for loss matrix and train/validation loss plot)
- 20 points if your R package passes with no WARNING/ERROR on
  https://win-builder.r-project.org/
  - minus 5 points for every WARNING/ERROR.
- 20 points for group evaluations -- this is to make sure that each group member participates more or less equally. You will get points deducted if your fellow group members give you a bad evaluation.
- 20 points for accuracy of your code
  (I will run tests to make sure your LM__LossL2 functions accurately compute the solution to the L2-regularized problems).
- 10 points for R documentation pages.
  - 4 points for informative example code.
  - 4 points for documenting types/dimensions of inputs/outputs.
  - 2 points for writing the cost function in the LM__LossL2 docs.
- 10 points for tests, as described above.
Extra credit:
  - 2 points extra credit if, in your R package,
    you write a test that makes sure your LM__LossL2 functions compute a solution
    which has L1-norm of the gradient less than the specified opt.thresh.
  - 2-6 points extra credit if, in your Rmd report,
    you also use LMSquareLossL2CV functions on the binary classification data sets,
    and comment on the difference in accuracy between logistic/square losses. (2 points per data set)
  - 2-10 points extra credit if, in your Rmd report,
    you also compare against NNLearnCV, and comment on
    whether or not linear models or nearest neighbors is more accurate
    (2 points per data set).
  - 2 points extra credit if, in your Rmd report,
    you use LaTeX code/MathJax to type the equations
    for the cost/loss functions for each learning algorithm.
  - 2 points if, in your GitHub repo, you setup Travis-CI to check your R package,
    and have a green badge that indicates a build that passes checks.
    See [[https://juliasilge.com/blog/beginners-guide-to-travis/][blog]]
    and [[https://docs.travis-ci.com/user/languages/r/][docs]].
  - if you submit your work early you will get feedback from me and extra credit:
    - First week: 10 points if you have written all R functions described above by Fri Feb 22. (1 point per function)
    - Second week: 10 more points if you have started your report by Fri Mar 1 --
      please send me your rendered html report by email.
      You will get 2 points of extra credit for the analysis of each data set
      (1 point for plots of train/validation loss versus regularization parameter,
      1 point for 4-fold CV loss matrix table/plot).
    - Third week: do tests/docs, finish report, make sure package passes R CMD check with no WARNING/ERROR on win-builder.
