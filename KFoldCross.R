KFoldCV <- function(X_mat, y_vec, ComputePredictions, fold_vec)
{
	# currently K is a placeholder for testing
	# K is set to the num_folds as declared in NearestNeighborCV
	K = which.max(fold_vec)
	errorVec <- numeric(K)
	for (k in 1:K)
	{
		# X/Y_new should be all observations related to fold_vec[fold_vec=k]
		items <- which(fold_vec == k)
		notItems <- which(fold_vec != k)
		# items is a vector containing indices of places where fold_vec = k
		X_new <- X_mat[items]
		Y_new <- y_vec[items]
		# X/Y_train should be all observations not in X/Y_new
		X_train <- X_mat[notItems]
		Y_train <- y_vec[notItems]
		pred_new <- ComputePredictions(X_train, Y_train, X_new)
		errorVec <- ZeroOneLoss(pred_new, Y_new)
	}
	return <- errorVec
}
