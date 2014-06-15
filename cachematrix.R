# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# These following two functions help to cache the inverse of a matrix.



# makeCacheMatrix is a function returing a list which can give the target
# 4 functions to fulfill caching, inversing and so on

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL               #at first time, NULL is given to i
        
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }                       #when a new matrix is about to apply, and if 
                                #don't want to make a new cachematrix, maybe
                                #for saving storage, use this function to load
                                #a new matrix to the existed cachematrix
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse 
                                #when setinverse were called, cached data
                                #overwrites the value of i
        
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()     #get the cached data
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }                       #check if we have any cached data
        
        data <- x$get()         #if this is the first time, load the matrix
        
        i <- solve(data)        #and calculate the inverse matrix
        
        x$setinverse(i)         #and then store the result for next time
        
        i
}


# a<-matrix(c(3,4,4,4,3,2,5,6,6,3,2,6,5,2,5,5),4,4)     #for testing