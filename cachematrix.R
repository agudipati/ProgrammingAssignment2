##The following functions calculate the inverse of a given matrix and stores in the cache(in another environment) for
##later retrieval

#makeCacheMatrix function sets and gets the value of matrix and inverse of the matrix from the cache and returns a list of
#the functions
makeCacheMatrix <- function(x=matrix())
{
  i<<-NULL
  
  #sets or initializes the matrix in the cache
  set <- function(y)
  {
    x<<-y
    i<<-NULL
  }
  
  #retrieves the matrix from the cache
  get <- function() x
  
  #stores the inverse of the given matrix
  setinverse<<-function(inverse)
  {
    i <<- inverse
    
  }
  
  #Retrieves the inverse of the matrix from the cache
  getinverse <- function() i
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


#makecacheSolve function checks if the inverse has already been calculated and stored in cache. If so, it gets the inverse from the cache
#and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the
#cache via the setinverse function

cacheSolve <- function(x, ...)
{
  i <- x$getinverse()
  
  if(!is.null(i))
  {
    message("Retrieving the inverse of the matrix from cache")
    return(i)
  }
  
  #calculates the inverse, stores in cache and finally returns it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
