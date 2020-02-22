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

# # COMMENT BLOCK
# # compute and visualize validation error as a function of number of neighbors
# result <- NearestNeighborsCV(X.sc, y.vec)
#  ggplot()+
#    geom_line(aes(
#      neighbors, error.percent, group=validation.fold),
#      data=validation.error)
# 
# ## compute and visualize test error results:
# err.dt.list <- list()
# ## assign folds.
# for(test.fold in 1:5){
#   ## split into train/test sets.
#   for(algorithm in c("baseline", "1-NN", "NNCV")){
#     ## run algorithm and store test error.
#     err.dt.list[[paste(test.fold, algorithm)]] <- data.table(
#       test.fold, algorithm, error.percent)
#   }
# }
# err.dt <- do.call(rbind, err.dt.list)
# 
# ggplot()+
#   geom_point(aes(
#     error.percent, algorithm),
#     data=err.dt)

# # TEST PLOTTING CODE
ggplot()+
  geom_line(aes(
    neighbors, percent.error, color=set, group=paste(set, validation.fold)),
    data=err.dt)

mean.dt <- err.dt[, .(
  mean.percent=mean(percent.error),
  sd=sd(percent.error)
), by=.(set, neighbors)]
min.dt <- mean.dt[set=="validation"][which.min(mean.percent)]
gg <- ggplot()+
  geom_ribbon(aes(
    neighbors, ymin=mean.percent-sd, ymax=mean.percent+sd, fill=set),
    alpha=0.5,
    data=mean.dt)+
  geom_line(aes(
    neighbors, mean.percent, color=set),
    data=mean.dt)+
  geom_point(aes(
    neighbors, mean.percent, color=set),
    data=min.dt)+
  coord_cartesian(xlim=c(0, 25))
directlabels::direct.label(gg, "last.polygons")
# # END TEST PLOTTING CODE

ComputePredictions <- function(X_train, y_train,X_new) class::knn(X_train,X_new,y_train)

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
