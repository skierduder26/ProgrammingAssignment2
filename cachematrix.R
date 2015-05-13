## R Programming Coursera
## Programming Assignment 2: Cache the inverse of a matrix

## Author: Skierduder26
## Date: May-13-2015

## makeMatrix 
## Takes an invertible matrix and converts it to a
## cached list such that the matrix and it's inverse
## can be retrieved without the computational expense
## of computing the inverse many times

makeMatrix <- function(x = matrix()) {
  ## Convert a matrix 'x' to a list object that stores the
  ## matrix and it's inverse
  
  # initialize inverse to empty
  x_inv <- NULL
  set <- function(y) { # initialize matrix 'x'
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x # retrieve matrix 'x'
  setinverse <- function(inverse) x_inv <<- inverse # cache inverse
  getinverse <- function() x_inv # retrieve inverse
  
  # create a list to cache matrix and inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Takes the "list" matrix produced by makeMatrix
## and computes the inverse if it has not already
## been computed, or retrieves the inverse if it
## has already been stored

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  x_inv <- x$getinverse() #try to retrieve the inverse
  
  if(!is.null(x_inv)) {
    # if inverse exists, return inverse
    message("Retrieving cached matrix inverse")
    return(x_inv)
  }
  
  # otherwise retrieve matrix, compute and cache inverse
  matrix <- x$get()
  x_inv <- solve(matrix, ...)
  x$setinverse(x_inv)
  x_inv
}
