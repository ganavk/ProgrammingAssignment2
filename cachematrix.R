# This program demonstrates how to use 'Cache' feature in R.The function makeCacheMatrix
# creates the inverse of a square matrix and stores in a matrix in Global environment
# The function cacheSolve checks if the inverse already exists and if it exists gets the
# inverse of the function from the matrix in the global environment
 


# makeCacheMatrix returns a list of function. This also sets the inverst of the matrix first time
#set the value of the matrix
#get the value of the matrix
#set the value of the matrix
#get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
 # initially set the value to NULL
           mcache <- NULL 
           # store a matrix 
           setMatrix <- function(newValue) { 
                   x <<- newValue 
                   # since the matrix is assigned a new value, flush the cache 
                   mcache <<- NULL 
           } 
   
           # returns the stored matrix 
           getMatrix <- function() { 
                   x 
         } 
  
         # cache the given argument  
         cacheInverse <- function(solve) { 
                 mcache <<- solve 
           } 
   
           # get the cached value 
           getInverse <- function() { 
                   mcache 
           } 
          
           # return a list. Each named element of the list is a function 
           list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
   } 
 

## This function checks if inverse matrix already present in cache and if presents
## gets it from cache. If not inverses the matrix.

cacheSolve <- function(newmat, ...) {
        ## Return a matrix that is the inverse of 'x'
           minverse <- newmat$getInverse() 
           # if a cached value exists return it 
           if(!is.null(minverse)) { 
                   message("getting cached inverse of the matrix") 
                   return(minverse) 
           } 
           # otherwise get the matrix, caclulate the inverse and store it in the cache 
           data <- newmat$getMatrix() 
           minverse <- solve(data) 
           newmat$cacheInverse(minverse) 
            
         # return the inverse 
         minverse 
  

}
