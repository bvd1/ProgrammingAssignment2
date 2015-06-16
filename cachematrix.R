## Programming Assignment 2
## Create a special matrix object that is cached, retrieve it and calculate the inverse.
# Use example from https://github.com/rdpeng/ProgrammingAssignment2

## This function creates a special "matrix" object, which is really a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    # set the value of the matrix
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    # get the value of the matrix
    get<-function() x
    
    # set the value of the inverse (use solve function for inverse)
    setmatrix<-function(solve) m<<- solve
    
    # get the value of the mean
    getmatrix<-function() m
    
    # show output
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x=matrix(), ...) {
    
    # check if there is cache data, if so use it 
    m<-x$getmatrix()
        if(!is.null(m)){
        return(m)
    }
    
    #otherwise calculate the inverse of the matrix (use solve function for inverse)
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}


# Check:
# create a square, nonsingular matrix:
x <- matrix(c(1,2,3,4,5,6,6,6,11),nrow = 3,ncol = 3)

# save its inverse to cache and use this cache with cacheSolve
cacheSolve(makeCacheMatrix(x))



