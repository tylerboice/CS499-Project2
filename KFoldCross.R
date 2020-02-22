# Compute Predictions is the wrapper for the standard knn function
ComputePredictions <- function(X_train, y_train, X_new) class::knn(X_train,y_train, X_new)

# K Fold Cross-Validation Algorithm
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
		pred_new <- ComputePredictions(X_train, X_new,y_train)
		errorVec <- ZeroOneLoss(pred_new, Y_new)
	}
	return <- errorVec
}

# Nearest Neighbors Cross Validation
NearestNeighborsCV <- function(X_mat, y_vec, X_new, num_folds, max_neighbors)
{
  validation_fold_vec <- sample(rep(1:num_folds, l=nrow(X_mat)))
  error_mat <- as.numeric(num_folds, max_neighbors)
  for(num_neighbors in 1:max_neighbors)
  {
    ComputePredictions <- class::knn(X_mat, X_new, y_vec, max_neighbors)
    traceback()
    rbind(error_mat, KFoldCV(X_mat, y_vec, computePredictions, fold_vec))
    mean_error_vec <- mean(error_mat, num_neighbors)
  }
  outputList <- list(X_new, mean_error_vec)
}

# install package if it is not already
if(!require("data.table")){
  install.packages("data.table")
}

# attach all functions provided by these packages
library(data.table)
library(ggplot2)

# download spam data set to local directory, if it is not present
if(!file.exists("spam.data")){
  download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

# Read spam data set and conver to X matrix and y vector we need for gradient descent
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE])
y.vec <- spam.dt[[ncol(spam.dt)]]
X.sc <- scale(X.raw) # scaled X/feature/input matrix

# compute and visualize validation error as a function of number of neighbors
result <- NearestNeighborsCV(X.train, y.vec, ComputePredictions, 5, 10)
ggplot()+
  geom_line(aes(
    neighbors, mean_error_vec, group=validation.fold),
    data=validation.error)

# compute and visualize test error results
err.dt.list <- list()
# assign folds.
for(test.fold in 1:5){
  # split into train/test sets.
  for(algorithm in c("baseline", "1-NN", "NNCV")){
    # run algorithm and store test error.
    err.dt.list[[paste(test.fold, algorithm)]] <- data.table(=
    	test.fold, algorithm, error.percent)
  }
}
err.dt <- do.call(rbind, err.dt.list)

ggplot()+
  geom_point(aes(
    error.percent, algorithm),
    data=err.dt)

# Compute Predictions is the wrapper for the standard knn function
ComputePredictions <- function(X_train, y_train,X_new) class::knn(X_train,X_new,y_train)

# K Fold Cross-Validation Algorithm
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
		X_new <- X_mat[items,]
		Y_new <- y_vec[items,]
		# X/Y_train should be all observations not in X/Y_new
		X_train <- X_mat[notItems,]
		Y_train <- y_vec[notItems,]
		pred_new <- ComputePredictions(X_train, X_new,y_train)
		errorVec <- ZeroOneLoss(pred_new, Y_new)
	}
	return <- errorVec
}


# Testing
set.seed(109)
n.folds <- 10
fold.vec <- sample(rep(1:n.folds, l=nrow(X.sc)))

err.dt.list <- list()
for(validation.fold in 1:n.folds){
  is.validation <- fold.vec == validation.fold
  is.train <- !is.validation
  X.train <- X.sc[is.train, ]
  y.train <- y.vec[is.train]
  for(neighbors in 1:20){
    pred <- class::knn(X.train, X.sc, y.train, k=neighbors)
    pred.y <- as.integer(paste(pred))
    pred.dt <- data.table(
      set=ifelse(is.train, "train", "validation"),
      pred.y,
      label.y=y.vec,
      is.error=pred.y != y.vec)
    mean.err <- pred.dt[, .(
      percent.error=100*mean(is.error)
    ), by=set]
    err.dt.list[[paste(validation.fold, neighbors)]] <- data.table(
      validation.fold, neighbors, mean.err)
  }
}
err.dt <- do.call(rbind, err.dt.list)

ggplot()+
  geom_point(aes(
    mean_error_vec, algorithm),
    data=err.dt)
