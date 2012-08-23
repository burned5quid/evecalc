construct.base.price.matrix <- function(price.weights = mineral.price.weights) {
    base.matrix      <- diag(rep(1, 7));
    base.matrix[, 1] <- -price.weights;
    
    return(base.matrix[2:7, ]);
}
