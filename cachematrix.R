## The following function — makeCasheMatrix and cacheSolve all for the 
## creation and caching of the inverse of a matrix.  The functions work
## to solve computation time for complex calculations that may be used 
## frequently. Both functions used the structure proved in the example exercise
## in the R Programming course on Coursera - https://class.coursera.org/rprog-010/human_grading/view/courses/973491/assessments/3/submissions 


## makeCacheNatrix creates function that sets the value of a matrix, 
## gets the value of the matrix, set the value of the inverse of the matrix and
## gets the value of the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
	   m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function gets the value of the inverse of a matrix, checks to
## is see if the inverse of the matrix already exists and returns the existing 
## value. Otherwise it computes the inverse and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
