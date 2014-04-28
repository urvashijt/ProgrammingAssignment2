## This function returns the list of four functions. It also takes the argument of 
## the form of a matrix and creates a cache matrix "m" which would be used to store the 
## value of a computed inverse of the matrix. The cached value will be retrived to avoide unnecesary calculations in future

makeCacheMatrix <- function(x = matrix()) {      ## It takes an argument x of type matrix
  m <- NULL                                      ## Assigns null value to variable m
  set <- function (y) {                          ## defines function "set" that takes argument "y"
    x <<- y                                      ## Assigns the value received in argument to the variable "x" in cache
    m<<- NULL                                    ## Sets the value of variable "m" in cahce as NUll
    
  }
  
  get <- function () x                          ## defines an anonymous function "get" which returns the value of matrix "x"
  setInverse <- function(z) m <<- z     ## defines function "setInverse" which sets the value of variable "m" in cache with the argument received 
  getInverse <- function () m           ## defines function "getInverse" which returns the value of variable "m"
  list (set = set, get= get,          
        setInverse= setInverse, getInverse= getInverse) ## returns the list of four functions defined earlier

}


## cacheSolve function would first retrive the value of variable "m" from cache. If it is not found to be NuLL, meaning the inverse of matrix
## has been calculated before, will return the data from cache otherwise will compute the inverse of the matrix and return that data 
## At the same time it will also set the value of variable "m" in cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
  m <- x$getInverse ()                  ## Calls the function "getInverse" and assigns the value to variable "m"
  if (!is.null(m)){                     ## If the value of "m" is not Null, means "m" has been calculated
                                        ## before so no need to compute it again
    message("getting cached data")      ## and send the message the the value is being retrived from cache
    return (m)                          ## returns the cached value
  }
  
                                        ## if the vlaue of "m" is Null, means we need to compute the inverse of the matrix
  data <- x$get()                       ## new variable "data" gets the value of original Matirx by using "get" function
  m <- solve(data,...)                  ## variable "m" gets the value of inverse matrix by computing inverse using "solve" function
  x$setInverse(m)                       ## sets the value of "m" in cache using setInverse function, so no need to compute in future if needed
  m                                     ## returns the value of m
}
