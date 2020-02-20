KFoldCV <- function(X_mat, y_vec, ComputePredictions, fold_vec)
{
	# currently K is a placeholder for testing
	# K is set to the num_folds as declared in NearestNeighborCV
	K = which.max(fold_vec)
	errorVec <- numeric(K)
	for (k in 1:K)
	{
		# X/Y_new should be all observations related to fold_vec[fold_vec=k]
		X_new <- X_mat[fold_vec=k]
		Y_new <- y_vec[fold_vec=k]
		# X/Y_train should be all observations not in X/Y_new
		X_train <- X_mat[!X_new]
		Y_train <- y_vec[!Y_new]
		pred_new <- ComputePredictions(X_train, Y_train, X_new)
		errorVec <- ZeroOneLoss(pred_new, Y_new)
	}
	return <- errorVec
}
