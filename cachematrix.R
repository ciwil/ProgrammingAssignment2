# This second programming assignment will require you to write
# an R function is able to cache potentially time-consuming 
# computations. For example, taking the mean of a numeric vector 
# is typically a fast operation. However, for a very long vector, 
# it may take too long to compute the mean, especially if it has to be 
# computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, 
# it may make sense to cache the value of the mean so that when we need it again, 
# it can be looked up in the cache rather than recomputed.

# In this Programming Assignment will take advantage of the scoping rules
# of the R language and how they can be manipulated to preserve state 
# inside of an R object.

# This function receives a invertible matrix and returns a vector 
# which is a 4 elements list matrix, 
# your inverse initially NULL, 
# and 4 functions to set or get any of the two matrices

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  # Function set:
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  # Function get:
  get <-function(y) x
  # Function setinv:
  setinverse <- function(inverse) inv <<-inverse
  # Function getinv:
  getinverse <- function() inv
  # The list holding the four functions:
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

# This function receives a makeCacheMatrix
# if the inverse is not cached, calculates it and stores this result on makeCacheMatrix
# and then returns the inverse.
# If cached prints a message and returns the inverse matrix.

cacheSolve <- function(x, ...) {
  # Return an inverted matrix from cache memory
  # (empty if a new original matrix has been created):
  inv<-x$getinverse()
  #If the inverse matrix is NOT empty then:
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  # If the inverse matrix IS empty then:
  # Calls the original matrix from cache memory:
  invmatrix<-x$get()
  # Creates an inverse matrix:
  inv<-solve(invmatrix)
  x$setinverse(inv)
  inv
}