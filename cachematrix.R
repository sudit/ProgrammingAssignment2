## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "special" matrix that will be used in cacheSolve
## to determine if an invers already exists or it needs to be computed

makeCacheMatrix <-function (z= matrix()) {
    invmat<- NULL
    inv <-function (w) {
        z <<- w
        invmat <<- NULL
    }
    getmat <- function () z
    setinv <- function (solve) invmat <<- solve
    getinv <- function () invmat
    list (inv=inv, getmat=getmat, setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## Computes Matrix Inverse if needed or recalls one if it already exists
cacheSolve <- function (z,...) {
    invmat<-z$getinv ()
    if(!is.null(invmat)) {
        message ("getting cached inverse matrix")
        return (invmat)
    }
    datainv <- z$getmat()
    invmat <- solve(datainv,...)
    z$setinv(invmat)
    invmat
}
