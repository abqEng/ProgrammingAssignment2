## This second programming assignment requires the development of an R function 
## that is able to cache potentially time-consuming computations.
## For this Programming Assignment the function will take advantage of 
## the scoping rules of the R language and how they can be manipulated to 
## preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.
## --NOTE-- For this assignment, it is assumed that the matrix supplied is invertible.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("using cached matrix")
        return(m)
    }
    m <- x$get()
    mi <- solve(m)
    x$setinv(mi)
    mi 
}

## ------------------------TESTS FOR ABOVE FUNCTIONS------------------------- ##

## 1)create square matrix with known inverse
x <- array( c(2, -3, 5, -7), dim=c(2,2) )

## 2)create special matrix that holds it's inverse
t1 <- makeCacheMatrix(x)

## 3)solve for the inverse
cacheSolve(t1)

## 4)confirm inverse by matrix multiplication
t1$get() %*% t1$getinv()

## 5)solve again to see if cache is used
cacheSolve(t1)

## 6)change matrix to test if inverse is reset
x <- t1$getinv()
t1$set(x)
t1$getinv()

## 7)repeat steps 3-5
cacheSolve(t1)
t1$get() %*% t1$getinv()
cacheSolve(t1)


