# In this R code, there are two functions for evaluating the inverse 
# of a matrix and caching it.  The first function, makeMatrix, creates
# an R object of class list and of type makeMatrix.  When called, it 
# stores its argument x, a matrix (assumed to be non-singular), and
# creates functions to set and get the matrix and its inverse.  When 
# setting the inverse, it caches the inverse so that it will not have
# to be needlessly recomputed.  This is accomplished by using the 
# '<<-' assignment operator, which assigns the value into the parent
# environment, rather than the immediate function environmment.

makeMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

# The second function returns the inverse of the list object of type 
# makeMatrix above. If the inverse has already been computed, and 
# thus cached, the cached inverse is returned.  Otherwise, it is
# calculated and returned.

cachesolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) 
    x$setinverse(i)
    i
}
