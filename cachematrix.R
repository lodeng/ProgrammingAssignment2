makeVector <- function(x = numeric()) {
        # define variable in this function
        m <- NULL
        
        # set y to in variable x in makeVector 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get x which is the date we need 
        get <- function() x
        
        # give the value of mean to m
        setmean <- function(mean) m <<- mean
        
        # get m which is the result 
        getmean <- function() m
        
        # makeVector get a list, and there are for elements.
        # each element is the function we defined before
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


# the x must be a makeVector
cachemean <- function(x, ...) {

        # x$getmean is the getmean function in makeVector
        # it means we check whether the value of mean we counted before
        m <- x$getmean()
        if(!is.null(m)) {
        
        ## if m is not null, we return the result
                message("getting cached data")
                return(m)
        }
        
        # get get function in x, so we can get the data we need 
        data <- x$get()
        
        # get the value of mean 
        m <- mean(data, ...)
        
        # get setmean function in x, and save the result into x
        # next time we use cachemean, x saved the results
        x$setmean(m)
        
        # return
        m
}
