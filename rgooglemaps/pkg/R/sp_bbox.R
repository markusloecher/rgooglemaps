sp_bbox = function (obj) 
{
    is_points <- function(obj) {
        is <- FALSE
        if (is.array(obj)) 
            if (length(dim(obj)) == 2) 
                if (dim(obj)[2] >= 2) 
                  is <- TRUE
        is
    }
    if (!is_points(obj)) 
        stop("object not a >= 2-column array")
    xr <- range(obj[, 1], na.rm = TRUE)
    yr <- range(obj[, 2], na.rm = TRUE)
    res <- rbind(x = xr, y = yr)
    colnames(res) <- c("min", "max")
    res
}
