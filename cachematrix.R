

## makeCacheMatrix: This function creates a special "matrix" object
#that can cache its inverse.

# makeCacheMatrix will take a matrix as a argument and if
#ino matrix is provided when the function is called ,
# a matrix will be generated.
#The function will then gets the value of the matrix,
#sets the value of its inverse , gets the value of its inverse. 
#and, returns a list a functions that can be called )

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y #set the value of matrix
        i <<- NULL #reset the new matrix's inverse
    }
    get <- function() x 
    # this function takes no argument and returns a matrix
    
    setinverse <- function(inverse) i <<- inverse  
    #this function take the inverse of the matrix 
    #and give its value to i.
    
    getinverse <- function() i
    # this function takes no arguement and returns i

    list (set=set, get=get, 
          setinverse=setinverse,
          getinverse=getinverse)
    # returns a list of functions
}


##This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then 
#the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() # retrieve the inverse from the cache
    if(!is.null(i)){
        message("getting cached data")
        return(i) 
        # check if inverse of the matrix exists 
        #and if it dose, returns its value.
    }
    # if inverse dose not exits, it will be calculated and returned.
    data <- x$get() 
    i <- solve(data,...)
    x$setinverse(i)
    i #returns i 
}

# I know that we aren't supposed to run the code. 
#However, if you want to run it, feel free to do so.
#Here are some commands you can use:
# > m1 <- matrix(c(4,3,3,2),2,2) 
# > mcm <- makeCacheMatrix(m1)
# > cacheSolve(mcm)
# > cacheSolve(mcm)
