## Put comments here that give an overall description of what your
## functions do

##Returns a list of functions the cache the inverse of a matrix in the closure  
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL  #Initial value of the inverse, before it is stored
  
  #This function assigns a matrix into the closure, in preparation for computing the inverse
  set <- function(y) {
    
    #The <<- operator gives r access to variables outside the scope of the function
    x <<-y  

    #If we have a new data, we need to delete any previous inverses which have been calculated
    inverse <<- NULL 
  }
  
  get <- function() x
  
  #set a value for the inverse into the closure 
  setinverse <- function(inv) inverse <<- inv
  
  #get the cached version of inverse from the closure
  getinverse <- function() inverse
  
  #return a list of the functions we'e just specified
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


##Takes a cachematrix object and returns an inverse.
#Computes inverse if not already computed, otherwise returns result from cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    #If cached value exists, do not perform invese calculate
    if(!is.null(inverse)){
      message("getting inverse from cache")
      return(inverse)
    }
    
    #get the raw matrix from the cachematrix object
    data <- x$get()
    
    #compute the inverse
    inv <- solve(data,...)
    
    #assign it to the cache
    x$setinverse(inv)
    
    #return the inverse 
    inv
  
}

#test it works
c_inv <- makeCacheMatrix(matrix(runif(10000),100,100))

cacheSolve(c_inv)
cacheSolve(c_inv)
