##cachematrix.R by NiklasS81 Programming Assignment 2: Lexical Scoping

##The makeCacheMatrix function has a matrix input. 
#It sets the value of the matrix, gets the matrix value, sets the inverted matrix value 
#and gets the inverted matrix value.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        ## set matrix value
        set_matrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        
        get_matrix <- function() x                                       #get matrix value
        set_inverse <- function(inverse) inv_matrix <<- inverse          #set inverted matrix value
        get_inverse <- function() inv_matrix                             #get value of inverted matrix
        list(set_matrix = set_matrix, get_matrix = get_matrix, 
             set_inverse = set_inverse, get_inverse = get_inverse)
}

##The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {                      #if inv_matrix is not NULL
                message("Getting cached data")          #Write message "Getting cached data"
                return(inv_matrix)                      #return the inverted matrix
        }
        mdata <- x$get_matrix()                         #get matrix data
        inv_matrix <- solve(mdata, ...)                 #invert matrix with the solve function
        x$set_inverse(inv_matrix)                       #set inverted matrix
        inv_matrix                                      #return the inverted matrix
}
