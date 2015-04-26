# *** AUTHOR IVAN S. *** # 

# When working with large data sets it is prudent to do an operation once and store its result
# so that later we do not have to re-do the operation because with large data sets this can be
# time and resource - consuming. These two functions perform a 'solve' operation to a matrix to
# get its inverse. By storing this result, we do not need to re-run 'solve' for the same matrix. We
# need to use '<<-' operator that stores result in a an environment that does not reset between
# function calls, otherwise result will be looked up in current environment which will always be NULL

# makeCacheMatrix is a function that contains functions for creating a numeric matrix
# storing its inverse, and retrieving both the matrix and its inverse. All these "mini"
# functions can be called separately because they are stored in a list and it is easy
# to access individual members of a any list by using a '$' symbol

makeCacheMatrix <- function(x = matrix()) {             # <-- Create a function that takes a matrix as an argument
        
        inverseMatrix <- NULL                           # <-- Your ultimate goal is to store the inverse of your
                                                        #     matrix in this variable but everytime you call makeCacheMatrix
                                                        #     function you need to reset this variable to NULL to clear off
                                                        #     any previous results stored in it
        
        set <- function(y) {                            # <-- create a function inside function that takes a matrix as an
                                                        #     argument
                
                x <<- y                                 # <-- change original matrix passed in line 6 to whatever argument is
                                                        #     passed to function 'set' as (y) in line 11. This is all that is
                                                        #     needed if you are done with inverse of previous matrix and
                                                        #     want to get an inverse of this new matrix in (y)
                
                inverseMatrix <<- NULL                  # <-- reset your previous result to NULL before proccessing current
                                                        #     matrix and storing its inverse in this variable
        }
        get <- function() x                             # <-- another function inside function, probably the simplest of all
                                                        #     because it doesn't take any arguments and simply returns whatever
                                                        #     matrix is currently stored in variable x   
        
        setInverseMatrix <- function(arg) inverseMatrix <<- arg     # <- another function inside function that assigns 
                                                                    #    inverseMatrix to whatever you pass to it in (arg),
                                                                    #    notice that this is not where you actually perform
                                                                    #    the inverse calculation. WARNING - you do not want
                                                                    #    to call this function manually - this function is meant
                                                                    #    to be called from inside cacheSolve() function
        
        getInverseMatrix <- function() inverseMatrix    # <-- another simple function inside function that returns inverse of
                                                        #     your matrix. You can call this and assign its result to a
                                                        #     variable to preserve the inverse of current matrix. This way you
                                                        #     can preserve inverses of more than one matrix
        
        list(set = set, get = get,                      # <-- take all 4 functions created above and put them in a list, that
                setInverseMatrix = setInverseMatrix,    #     way each one of them can be easily accessed individually
                getInverseMatrix = getInverseMatrix)    #     through '$' symbol
}


# cacheSolve performs the actual inverse calculation only if needed. This can can be faster and use less computing power.
# If calculation needs to be performed, it produces its result and stores it for later use so that calculation is not
# needlessly duplicated

cacheSolve <- function(x, ...) {                        # <-- create a function to retrieve or calculate inverse of matrix
                                                        #     passed as argument x
        
        inverseMatrix <- x$getInverseMatrix()           # <-- call one of the functions stored in a list from makeCacheMatrix
                                                        #     by using '$' to access it as a member of said list
        
        if(!is.null(inverseMatrix)) {                   # <-- check if inverseMatrix retrieved in previous step is something               
                                                        #     other than NULL, meaning you've already performed an inverse
                
                message("getting cached data")          # <-- if it is, print a message...
                
                return(inverseMatrix)                   # <-- ...and produce inverseMatrix as result. This way you do not need
        }                                               #     to expend resources and time again to calculate again
        
        data <- x$get()                                 # <-- if inverseMatrix is still set to NULL, it will need to be calculated.
                                                        #     First, you need to call a function created under makeCacheMatrix
                                                        #     to copy over your matrix so you can perform an inverse operation
                                                        #     to it
        
        inverseMatrix <- solve(data, ...)               # <-- this is the actual transormation from matrix to inverse of matrix
                                                             
        x$setInverseMatrix(inverseMatrix)               # <-  take result from above and store it by calling a storing function
                                                        #     created under makeCacheMatrix
        
        inverseMatrix                                   # <-- return inverse of matrix
        
}
