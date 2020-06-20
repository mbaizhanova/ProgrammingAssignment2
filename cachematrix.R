## Put comments here that give an overall description of what your
## functions do

## In this file there are the following two functions. 
## The first one, makeCacheMatrix(), creates an object that stores the matrix 
## and its inverse.

## The second function, cacheSolve(), takes as an input the result that is
## returned by the makeCacheMatrix() to access the inverse of the matrix from
## the cache that is stored in the first function environment.

## Write a short comment describing this function

## This function creates a matrix that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve
        getinverse <- function() im
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

## This function calculates the inverse of the matrix created by the first 
## function, makeCacheMatrix(). If the inverse of the matrix has already been 
## found and the matrix did not change, then the function gets the inverse from
## the cache and skips the computation. If not, it calculates the inverse and 
## sets the value of the inverse in the cache by the setinverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)){
                message("Getting cashed data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}

## Test of the functions

## m <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2) ## creating a matrix with 
## 2 rows and 2 columns
## m
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## newm <- makeCacheMatrix(m) ## creating a new matrix using makeCacheMatrix()
## cacheSolve(newm) ## running cacheSolve() function and getting the inverse 
## of the matrix
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## cacheSolve(newm) ## running cacheSolve() again and getting the result from 
## the cache
## Getting cashed data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
 