
## @x: a square invertible matrix
makeCacheMatrix<- function (x = matrix()) {

  ## return: a list containing functions to
  
  
  
  inv<- NULL
  ##              1. set the matrix
    set<- function(y){
   # use `<<-` to assign a value 
  # different from the current environment. 
    x<<- y
    inv<<- NULL
    }
  ##              2. get the matrix
  get<- function() x
  
  ##              3. set the inverse
  setInverse<- function(inverse) inv <<- inverse 
  ##              4. get the inverse
  getInverse <- function() inv
  
  ##         this list is used as the input to cacheSolve()
  list(set=set,get=get,setInverse =setInverse,getInverse=getInverse)
       
}


 



cacheSolve <- function(x, ...) {
## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
  inv <- x$getInverse()
  # if the inverse has already been calculated
  if(!is.null(inv)) {
   # get it from the cache and skips the computation.
    message("getting cached data")
    return(inv)
  }
 # otherwise, calculates the inverse 
   mat.data <- x$get()
   inv<- solve(mat.data,...)
# sets the value of the inverse in the cache via the setinv function.
   x$setInverse(inv)
   return(inv)
}

