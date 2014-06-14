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
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <-function(y) x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
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
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  invmatrix<-x$get()
  inv<-solve(invmatrix)
  x$setinverse(inv)
  inv
}