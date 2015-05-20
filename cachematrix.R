##
##  =========================================================
## 2 functions: 
##
##  makeCacheMatrix: an object with 4 methods
##  cache solve: get the inverse of the object of type makeCacheMatrix passed to it


##  =========================================================

##  makeCacheMatrix: essentially an object with 4 methods
##       Initialize with M=makeCacheMatrix()
##       $set - stores the persistent copy of the matrix, clears the inverse
##              ex: M$set(matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3))
##       $get - gets the persistent copy of the matrix
##              ex: M$get()
##       $setinverse - sets the persistent copy of the inverse
##              ex: M$setinverse(matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3))
##       $getinverse - gets the persistent copy of the inverse
##              ex: M$getinverse()

makeCacheMatrix <- function(m = matrix()) {         ## accepts a matrix x
  I <- NULL                                         ## initializes the inverse variable
  m <- NULL                                         #initializes the matrixvariable
  set <- function(y) {                              ## declares the "set" method
    m <<- y                                            ## save the thing that 'set' is passed in to the persistent variable x
    I <<- NULL                                         ## clear out the old persistent inverse
  }
  get <- function() m                               ##declares the 'get' method to return the persistent matrix
  setinverse <- function(inverse) I <<- inverse     ##declares the 'setinverse' method to store the thing passed into the persistent inverse variable
  getinverse <- function() I                        ##declares the 'getinverse' method to return the persistent inverse
  list(set = set, get = get,                        ## returns the pointers to the methods
       setinverse = setinverse,
       getinverse = getinverse)

}

##  =========================================================

## Passed in the object of type makeCacheMatrix
## if we have the inverse calculated, just return the inverse.
## if we don't have the inverse yet, calculate it and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## do we have an existing inverse, if so return it
        I=x$getinverse()
        if(!is.null(I)) {
          print("Cached value found")
          
           return(I)
        }
        print("Cached value NOT found")
        
        ## no inverse calculated yet
        ##retrieve the matrix
        A=x$get()
        
        ## assume the matrix is square - this is a potential source for error becasue we haven't verified it is
        ##use solve() to invert
        Ainv=solve(A)
        
        ##store result
        x$setinverse(Ainv)
        
        return(Ainv)
        
        
}


