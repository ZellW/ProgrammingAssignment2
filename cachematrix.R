# The mean of a numeric vector is typically a fast operation. However, for a very long vector, 
# it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). 
# If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when 
# we need it again, it can be looked up in the cache rather than recomputed. 
# Tese functions  take advantage of the scoping rules of the R language and how they can be manipulated 
# to preserve state inside of an R object.
#
# The following two functions are used to cache the inverse of  mymatrix.

makeCacheMatrix <- function(x = mymatrix()) {
     #intialize and store null cached value called mymatrix
     mymatrix <- NULL
     set = function(y) {
          # use <<- to assign y to x so it is available to a an envirnment outside the function
          # (Lexical Scoping)
          x <<- y
          # my mattrix is also set as a symbol (or variable) available outside the function
          mymatrix <<- NULL
     }
     #get the value of the matrix
     get = function() x
     # Invert mymatrix
     setmymatrix <- function(inverse) mymatrix <<- mymatrixinverse
     getmymatrix <- function() mymatrix
     # Return the value of mymatrix
     list(set=set, get=get, setmymatrix=setmymatrix, getmymatrix=getmymatrix)
     # I thank John Sebastian for his help on Get - Set. This was functionality that wwas new to me.
     # John also provided assistance on mtrix math.  I have not done matrix math for 30 years.  Thanks John!
}

## cacheSolve function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x' 
     mymatrix <- x$getinv() 
     # Test to see if the cache - mymatrix - is empty.  If YES get it from cache.       
     if(!is.null(mymatrix)) { 
          message("getting cached data") 
          return(mymatrix) 
     } 
     # If cache is null, calcualte the inverse 
     matrixdata <- x$get() 
     mymatrix <- solve(matrixdata) 
     x$setinv(mymatrix) 
     mymatrix 
}
