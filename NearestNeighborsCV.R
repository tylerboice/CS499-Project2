NearestNeighborsCV <- function(X_mat, y_vec, X_new, num_folds, max_neighbors)
{
  validation_fold_vec <- sample(rep(1:num_folds, l=nrow(X_mat)))
  error_mat <- numeric(num_folds, max_neighbors)
  for(num_neighbors in 1:max_neighbors)
  {
    ComputePredictions <- knn(X_mat, X_new, cl=y_vec, num_neighbors)
    rbind(error_mat, KFoldCV(X_mat, y_vec, computePredictions, fold_vec))
    mean_error_vec <- mean(error_mat, num_neighbors)
  }
  outputList <- list(X_new, mean_error_vec)
}
