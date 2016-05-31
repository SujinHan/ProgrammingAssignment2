## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix is a function that returns the list of functions 
# It works to store a matrix and cached value of the inverse of the matrix.
# And it contains the following functions:
# **set    set the value of a matrix
# **get    get the value of a matrix
# **setInverse      set the cached value (inverse of the matrix)
# **getInverse      get the cahced value (inverse of the matrix)
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    # Holds the cached value or NULL if nothing is cached
    # Initially nothing is cached so set it to NULL
    # Store a matrix
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x
    # Return the stored matrix
    setInverse<-function(inverse)inv<<-inverse
    getInverse<-function()inv
    # Return a list. Each named element of the list is a function
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # get the cached value
    inv<-x$getInverse()
    # if a cached value exists, then return it
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # otherwise, get the matrix, calculate the inverse and strore it in the cache
    mat<-x$get()
    inv<-solve(mat,...)
    x$setInverse(inv)
    # return the inverse
    inv
}

