## makeCacheMatrix creates a list containing a function to
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix
# 5. returns the list of created functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {# Create the matrix in the working environment
    x <<- y #Set the value
    m <<- NULL #Clear the cache
  }
  get <- function() x #Get the value of the matrix
  setInverse <- function(inverse) m <<- inverse #Set the value of inverse of the matrix in cache
  getInverse <- function() m #Get the value of inverse of the matrix from cache
  
  #Return the created functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##cacheSolve returns a matrix that is the inverse of 'x'
# 1.check if the inverted matrix does already exist in cache
# 2.if yes returns the inverse from cache
# 3.if no get value of the matrix
# 4.computes the inverse
# 5.sets the inverse in cache
# 6.returns the inverse from cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #Fetchs the cached value for the inverse
  if(!is.null(m)) { #Checks if the cache is not empty=the inverse is already computed
    message("getting cached data")
    return(m) #Returns the inverse from the cache
  }
  
  data <- x$get()#Get value of matrix
  m <- solve(data, ...)#Computes the inverse
  x$setInverse(m)#Set the value of inverse in cache
  m #Return the inverse of matrix
}