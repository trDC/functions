# Function to map particular values of a vector to other values - good for quick operations where you don't really want to create a separate mapping table and do a merge.
# Can be piped nicely. Does not alter the underlying vector
#' @param vector A vector, some particular values of which we want to map to other values. Function returns a new object without editing the underlying vector.
#' @param map_from A vector of values in vector that we want to map to other values
#' @param map_to The vector of corresponding values that we want to map the values in map_from to. Must be the same length as map_from
map_value <- function(vector, map_from, map_to) {
  stopifnot(length(map_from) == length(map_to))
  stopifnot(class(map_from) == class(map_to)) # No dodgy stuff, please
  
  x <- copy(vector)
  for(i in 1:length(map_from)) {
    if(map_from[i] %>% is.na) {
      x[is.na(vector)] <- map_to[i]
    } else {
      x[vector == map_from[i]] <- map_to[i]
    }
  }
  return(x)
}