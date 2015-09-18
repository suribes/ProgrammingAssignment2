# Assignment: Caching the Inverse of a Matrix

# Compute and store the inverse of a matrix created with the 'solve' function. 
# 1. Check to see if the inverse has already been calculated.
# 2. If is not calculated, calculate the inverse of the matrix using the 'solve' function in R.
# 3. Then, set the value of the inverse in the cache via the 'setInverse' function.
# 4. If calculated, get the inverse from the cache using 'getInverse'.
# 5. Return the inversed matrix.

# Functions definitions

# 'makeCacheMatrix' creates a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse 

# Inputs: y (matrix), inverse (inverted matrix) 
# Ouputs: mx (matrix), matrixInverse (inverted cached matrix)

makeCacheMatrix <- function(mx = matrix()) {
    # Initialize variables
    matrixInverse <- NULL
    
    # List available functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    # Functions Implementations
    
    # Initialize private variables
    set <- function(y) {
        # Assign new matrix values
        mx <<- y
        
        # Reset inverse matrix
        matrixInverse <<- NULL
    }
    
    # Retreive matrix
    get <- function() mx
    
    # Assign inversed matrix
    setInverse <- function(inverse) matrixInverse <<- inverse
    
    # Retrieve inversed matrix
    getInverse <- function() matrixInverse
}


## Write a short comment describing this function

# 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'. If the inverse has
# already been calculated (and the matrix has not changed), then
# 'cacheSolve' should retrieve the inverse from the cache.

# Inputs: mx (matrix), inverse (inversed matrix) 
# Ouputs: mxInverse (matrix), matrixInverse (inversed cached matrix)

cacheSolve <- function(mx, ...) {
    ## Return a matrix that is the inverse of 'mx'
    
    # Get cached inverted matrix
    mxInverse <- mx$getInverse()
    
    if(is.null(mxInverse)) {
        # Calculate inverse of stored matrix
        mxInverse <- solve(mx$get(), ...)
        
        # Set inversed matrix
        mx$setInverse(mxInverse)        
    }
    else {
        message("getting cached data")
    }
    
    # Return inversed matrix
    mxInverse
}
