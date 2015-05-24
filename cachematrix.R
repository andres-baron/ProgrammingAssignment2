## The two functions below will retrive from cache the inverse of
## matrix if it has already been calculates or else it will
## calculate the inverse and put it in cache

## Function makeCacheMatrix should store cache of matrix

makeCacheMatrix <- function(x = matrix()) {
        # x is an invertible matrix. It should be a square matrix. 
        inv<-NULL
        # set the matrix
        set<-function(y){
                ## use "<<-" to assign a value to an object from another environment
                x<<-y
                inv<<-NULL
        }
        # get the matrix
        get<-function() x
        # set the inverse
        setmatrix<-function(solve) inv<<- solve
        # get the inverse
        getmatrix<-function() inv
        # creates a list
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Function cacheSolve would call cache or else solve for the inverse of the matrix

cacheSolve <- function(x=matrix(), ...) {
        # x is the output of makeCacheMatrix() funtion
        # this function returns the inverse of the matrix from makeCacheMatrix()
        inv<-x$getmatrix()
        # if the inverse has already been calculated
        if(!is.null(inv)){
                # get it from cache. first give message "getting cached data"
                message("getting cached data")
                # then return inverse
                return(inv)
        }
        # if it has not been calculated, then calculate inverse
        matrix<-x$get()
        inv<-solve(matrix, ...)
        # stores the value of the inverse in the cache by the sermatrix function
        x$setmatrix(inv)
        # returns the value of inv variable
       
	 return(inv)
}
