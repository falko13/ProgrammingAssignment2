## Functions presented below provide caching  
## solution for potentially time-consuming computations


# This function creates a "matrix" object 
# that object includes 4 methods that can be used to:
# get current matrix of an object, set new one, set certain "solve" value, get back its value 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y         #write new matrix to the cashe(external environment)
                s <<- NULL      #because matix is changed, old "solve" value need to be erased
        }
        get <- function() x     #function to read matrix from cache(external environment) of its object
        setsolve <- function(solve) s <<- solve         #write new "solve" value to the cashe
        getsolve <- function() s                        #retrive "solve" value to the cashe
        list(set = set, get = get,   #return list of all methods
             setsolve = setsolve,
             getsolve = getsolve)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has  
# already been calculated (and the matrix has not changed), 
# then cacheSolve uses funxtion above to retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()       #check if x-object already 
        if(!is.null(s)) {       #has inverse matrix in its cache
                message("getting cached data") 
                return(s)       #return inversed matrix from cache  with appropriate message

        }
        data <- x$get()         #get matrix from x-object
        s <- solve(data, ...)   #calculate inverse of this matrix
        x$setsolve(s)           #write inverse of this matrix to the cache
        s                       #return inversed matrix
}





