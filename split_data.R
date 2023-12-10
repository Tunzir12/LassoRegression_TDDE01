# Function for splitting data into subsets with random sampling
# Splits data into 'k' non-overlapping subsets, with the size distributions 
# given by vector 'distributions'.
# 'distributions' need not sum to one, it needs only give the relative
# size of each resulting subset
# e.g. 'distributions' = c(2,1,1) will divide the input data into 
# one 50% set and two 25% sets.
# If 'distributions' is not given, each subset will have equal number of 
# data points. If the data points can not be split evenly, and there remains
# one data point unassigned, it will be randomly assigned to one of the k sets

# data_frame: input data frame
# k: number of output sets
# seed: seed for random number generator (optional)
# distributions: vector describing the size distributions of each 
#                of the k output sets
split_data <- function(data_frame, k, seed, distributions = rep(1,k)) {
  
  if(length(distributions) != k) {
    stop("Error in split_data(): Length of distributions vector does not match value k")
  }
  
  if(!missing(seed)) {
    set.seed(seed)
  } 
  
  # list to hold the k resulting data frames
  split_sets = list()
  
  # N: total data size
  # n: data size of the rest (changes every loop)
  norm_distributions = distributions/sum(distributions)
  N <- dim(data_frame)[1]
  rest <- data_frame
  for (i in 1:k) {
    n <- dim(rest)[1]
    indices <- sample(1:n, floor(N*norm_distributions[i]))
    split_sets[[i]] <- rest[indices,]
    rest <- rest[-indices, ]  # remove current subset from rest
  }
  
  # if there's a remaining data point in rest (uneven split), 
  # put the remaining data point in one of the sets, randomly choosing which
  if(dim(rest)[1] != 0){
    i = sample(1:k, 1)
    n = dim(split_sets[[i]])[1]
    split_sets[[i]][n+1,] <- rest[1,]
  }

  # return a list containing the desired data frames 
  return(split_sets)
}
  