## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix(): create a special matrix object that 
##can catch its inverse

makeCacheMatrix <- function(x = matrix()) {
ivr <- NULL                 ##set inverse as NULL
set <- function(y){
        x <<- y
        ivr <<- NULL
}
get <- function()x                                ##function to obtain matrix x
set_inverse <- function(inverse) ivr <<- inverse  ##set matrix x as inverse
get_inverse <- function() ivr                     ##get matrix x as inverse
list(set = set, 
     get = get,
     set_inverse = set_inverse,
     get_invese = get_inverse)
}

## Write a short comment describing this function

##cacheSolve(): computes the inverse of the matrix returned 
##by the function makeCacheMatrix(). If it has been already calculated 
##(the matrix remained unchanged), "cacheSolve" should retrieve the inverse 
##from the cache

cacheSolve <- function(x, ...) {
        ivr <- x$get_inverse()   ##get value from previous function
        if(!is.null(ivr)){       ##check whether inverse value is NULL
                message("getting cached data")
                return(ivr)      ##return inverse value
        }
        data <- x$get()
        ivr <- solve(data,...)   ##calculate inverse value
        x$set_inverse(ivr)
        ivr                      ##return a matrix that is the inverse of 'x'
}

##Example: 
q <- makeCacheMatrix(matrix(1:4,2,4)
q$get()
q$get_inverse()
cacheSolve(q)