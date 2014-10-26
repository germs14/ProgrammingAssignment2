#########################################################################################################
## Pair of functions that creates a special "matrix" object that can cache its inverse.                ##
## Function 1: makeCacheMatrix creates a special "matrix",  by using the following steps               ##
##      set the value of the matrix                                                                    ##
##      get the value of the matrix                                                                    ##
##      cache the inverse of the matrix                                                                ##
## Function 2: cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix     ##
## above. If the inverse has already been calculated (and the matrix has not changed), then the        ##
## cachesolve will retrieve the inverse from the cache.                                                ##
#########################################################################################################
#########################################################################################################
####################################### STEPS FOR TESTING ############################################### 
###### source("cachematrix.R")                                                                     ######
###### my_matrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))                             ######
###### my_matrix$get()                                                                             ######
###### cacheSolve(my_matrix)                                                                       ######
###### my_matrix$getmatrix()                                                                       ######
###### cacheSolve(my_matrix)                                                                       ######
###### my_matrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))                                         ######
###### cacheSolve(my_matrix)                                                                       ######
###### my_matrix$get()                                                                             ######
#########################################################################################################

## makeCacheMatrix: This function creates a special "matrix" object that can cache its invers
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## The value on m will be set to NULL if cacheSolve has not yet been used)
        #set the value of the matrix
        set <- function(y) {
                x <<- y    ## To cache the input matrix so that cacheSolve can check if the matrix has been changed
                m <<- NULL ## To set m, the matrix inverse if used cacheSolve, to NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        # To create a list for the four functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

# The following function calculates the inverse of the special "matrix" returned by makeCacheMatrix above. 
# The cahceSolve function first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
# of the inverse in the cache via the setmean function.

cacheSolve <- function(x=matrix(), ...) {
        # To compare matrix to prior one
        m <- x$getmatrix() # To get the inverse that has been calculated 
        if(!is.null(m)) {  # check to see if cacheSolve has been run before
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m # return the inverse
}
