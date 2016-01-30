## This program consists of a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function (y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setInverse<-function(inverse) m<<-inverse
	getInverse<-function() m
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of the 'matrix' created by the 
## makeCacheMatrix above. If the inverse has already been calculated(and 
## the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getInverse()
	if(!is.null(m)){
		message("This is cached data")
		return(m)
	}
	matr<-x$get()
	m<-solve(matr,...)
	x$setInverse(m)
	m
}
