## This function creates a special matrix that can cache its inverse
##cache of inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # this is a default if cacheSolve has not yet been used
    #set the value of the matrix
    set <- function(y) {
        #cache the matrix
        x <<- y # caches the input 
        m <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x
    #set the value of the inverse
    setinverse <- function(solve) m <<- solve
    #get the value of the inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special matrix returned by "makeCacheMatrix"
#If the inverse is calculated, then this function will retrieve the inverse from the cache


cacheSolve <- function(x=matrix(), ...) {
    m <- x$getinverse() #if an inverse has already been calculated this gets it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #otherwise
    data <- x$get() # run the get function to get the value of the input matrix
    m <- solve(data, ...) # compute the value of the inverse of the input matrix
    x$setinverse(m) # run the set inverse function on the inverse to cache the inverse
    m # return the inverse
}

###Test if works:
###mat <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
##mat2 <- makeCacheMatrix(mat)
##cacheSolve(mat2)
##mat
##solve(mat) %*% mat == diag(nrow = nrow(mat), ncol = ncol(mat))
##     [,1] [,2]
[1,] TRUE TRUE
[2,] TRUE TRUE