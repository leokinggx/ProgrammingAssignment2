## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## return four methods, $set (setting the matrix); 
    ## $get (getting the matrix); and similarly for the 
    ## inverse of the original matrix using $setinv & $getinv
    
    if (diff(dim(x)) != 0 ) print("error: input is not a square matrix!")
    inv     <- NULL
    set     <- function(y)  {x<<-y ; inv <<- NULL}
    get     <- function( )  x
    set_inv <- function(y)  inv <<- y 
    get_inv <- function( )  inv
    
    return  ( list( set = set, get = get,setinv = set_inv, getinv = get_inv) )
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x',
    ## directly getting if the inverse has been calculated 
    ## or perform the calculation
    
    inv     <- x$getinv()
    if      (is.null(inv))        inv <- solve(x$get())
    else    print("getting cached invers.")    
    return  (inv)
}
