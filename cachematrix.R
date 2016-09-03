## Put comments here that give an overall description 
## There are two functions for the assignment as required. 
## The first function sets up the special matrix and associated functions
## The second function computes the inverse of an invertible matrix

## Assumptions -
##              : X  is a square invertible matrix
##              : No Error checking for the input is needed
##              : The first function should be called before the second
##              : Input x to the second function is a matrix of type makeCacheMatrix
##
## Input Format Examples -
##              : X is a vector c(1,15,8,7,16,2,19,3,5) 
##              : X is a matrix(c(1,15,8,7,16,2,19,3,5),nrow=3,ncol=3)
##


# ----------------------- FIRST FUNCTION ------------------------------
## The first function uses similar  functions like the cachemean example.
## It takes x as input 
##      x can be a vector whose length is a perfect square
##      or x can be a square matrix
## The function defines 4 addtional functions
##      setmatdata - Resets the value of the matrix with argument y
##      getmatdata - prints the current matrix
##      setmatinv  - sets the matrix inverse to argument y
##      getmatinv  - prints the value of cached inverse

makeCacheMatrix <- function(x = matrix()) {

        # If input is a vector, convert to matrix
        dimmat <- sqrt(length(x))
        dim(x)<- c(dimmat, dimmat)
        
        # set the variable mi(matrix inverse)to NULL
        mi <- NULL
        
        # function setmatdata - Reset the value of previous matrix with y
        setmatdata <- function(y = matrix()) {
                x <<- y
                #calculate the dimension of the matrix
                dimmat <- sqrt(length(x))
                dim(x)<- c(dimmat, dimmat)
                # set the variable mi(matrix inverse)to NULL
                mi <<- NULL
        }
        
        # function getmatdata - prints the current matrix
        getmatdata <- function()
                {
                x
        }
 
        # function setmatinv - Reset the previous matrix inverse with matinv
        setmatinv <- function(matinv)
                {
                        mi <<- matinv
        }
        
        # function getmatinv - prints the current matrix inverse
        getmatinv <- function()
                {
                        mi
        }
        
        # List if functions for the special matrix
        list(setmatdata = setmatdata,
             getmatdata = getmatdata,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
        
} # end of first function makeCacheMatrix


# ----------------------- SECOND FUNCTION ------------------------------
## This function checks if the matrix inverse is in cache
## If cache is NULL the function calculates the inverse of a matrix x 
## The input x is of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## Check if the inverse of 'x' already exists and retun cached value
        my_inv <- x$getmatinv()
        if(!is.null(my_inv)) {
                message(" Getting cached data for matrix Inverse")
                return(my_inv)
        }
        
        ## Calculate inverse of the matrix if cache is NULL
        mat_data <- x$getmatdata()
        my_inv <- solve(mat_data, ...)
        
        # Set the value of inverse to cache / update cache
        x$setmatinv(my_inv)
        
        # Return the inverse of matrix x
        my_inv
        
}  # end of second function cacheSolve


# -----------------------  END of ASSIGNMENT----------------------------