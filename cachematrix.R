## Here are two functions that work together to a) take a matrix as input,
## b) calculate the inverse of that matrix, c) cache that inverse, and
## d) produce the inverse when asked, so that the cached value will be
## produced if it has already been calculated, and, if not, the inverse
## will be calculated and produced on the spot.

## makeCacheMatrix will take a matrix as input. It will track whether an
## inverse has been calculated for this matrix. It contains several 
## smaller functions that enable "cacheSolve", the other part of this 
## assignment, to interact with it and cache and calculate (if necessary)
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    Inverse <- NULL         ## sets Inverse (value returned) as NULL.
    set <- function(y){     ## "set" allows input matrix to be changed
        x<<-y               ## without calling makeCacheMatrix.
        Inverse <<- NULL
    }
    
    get <- function() x     ## "get" brings the original matrix to
                            ## "cacheSolve".
    setinverse <- function(NewInverse) Inverse <<-NewInverse
                            ## "setinverse" stores the inverse of the matrix
                            ## calculated in "cacheSolve" in the variable
                            ## "Inverse"
    getinverse <- function() Inverse
                            ## "getinverse" brings the values stored in
                            ## the varible "Inverse" to "cacheSolve" when
                            ## it calls this function
    list(set=set, get = get,      ## not sure what this does!!
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve will return the inverse of a matrix previously run through
## makeCacheMatrix. It uses small functions stored in makeCacheMatrix. 
## If the inverse has not been calculated, it calculates and stores
## it in a variable in makeCacheMatrix. If the inverse has previously been
## calculated, it returns the cached value without recalculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inverse <- x$getinverse()   ## gets the value in the variable
                                ## "Inverse" stored in makeCacheMatrix.                            
    if(!is.null(Inverse)) {     ## If this inverse has already been
        message("getting cached data")  ## solved, it returns the
        return(Inverse)         ## cached value and a message.
                                ## In this case cacheSolve stops here.
        
    }
    data <- x$get()             ## If not already solved, this gets
                                ## the matrix to solve ....
    Inverse <- solve(data, ...) ## This line assigns the solution to the
                                ## variable "Inverse".
    x$setinverse(Inverse)       ## This sends this value back to
                                ## makeCacheMatrix.
    Inverse                     ## The value of "Inverse is returned.
}
