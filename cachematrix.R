## from a matrix, a list with 4 functions is created
## this is accessed in the later function to calculate or retrieve its inverse

#this function creates a list of 4 named functions set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL #placeholder for matrix inverse
  
  set <- function(y) { #creates function set
    x <<- y #assigns value of y to x in parent environment
    i <<- NULL #resets i in parent environment
  }
  get <- function() x #creates function get, which displays  x
  
  setinverse <- function(inverse) i <<- inverse #creates function setinverse
  #wich assigns argument inverse to i in parent environment
  
  getinverse <- function() i#creates function getinverse
  #wich retrieves i
  
  list(set = set, get = get, #creates a list
       setinverse = setinverse, #functions can be called with $
       getinverse = getinverse)
}

#this function takes as input a matrix created by the function makeCacheMatrix
#it has as output the inverse of x 
#if the inverse has been calculated before, it will cache it; 
#if not it will calculate it

cacheSolve <- function(x, ...) {
  i <- x$getinverse() #using function getinverse from makeCacheMatrix
  if(!is.null(i)) { #checking if i is empty
    message("getting cached data")
    return(i) #if not, it gives the cached value
  }
  data <- x$get() #using function get from makeCacheMatrix
  i <- solve(data, ...) #calculating the inverse matrix
  x$setinverse(i) #assigning the inverse matrix to setinverse
  i 
}

