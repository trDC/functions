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

# Replace all NA values in every NUMERIC or INTEGER column only of a data table.
replace_all_NAs <- function(x,replace_val = 0) {
  colorder <- names(x) # to preserve the column order
  tmp1 <- x[, !(sapply(x,class) %>% as.character() %in% c("numeric","integer")), with = F] %>% as.data.table # keep only the columns that are NOT numeric
  tmp2 <- x[,   sapply(x,class) %>% as.character() %in% c("numeric","integer") , with = F] %>% 
    sapply(function(y){y = ifelse(is.na(y), replace_val, y)}) %>% as.data.table
  if(identical(tmp1,data.table())){
    tmp3 <- tmp2
  } else {
    tmp3 <- cbind(tmp1,tmp2) %>% setcolorder(colorder) 
  }
  
  return(tmp3)
}

# Replace all NaN values in every NUMERIC or INTEGER column only of a data table.
replace_all_NaNs <- function(x,replace_val = 0) {
  colorder <- names(x) # to preserve the column order
  tmp1 <- x[, !(sapply(x,class) %>% as.character() %in% c("numeric","integer")), with = F] %>% as.data.table # keep only the columns that are NOT numeric
  tmp2 <- x[,   sapply(x,class) %>% as.character() %in% c("numeric","integer") , with = F] %>% 
    sapply(function(y){y = ifelse(is.nan(y), replace_val, y)}) %>% as.data.table
  if(identical(tmp1,data.table())){
    tmp3 <- tmp2
  } else {
    tmp3 <- cbind(tmp1,tmp2) %>% setcolorder(colorder)
  }
  
  return(tmp3)
}

# Transforms the numeric portion of rows in a data table to a distribution (i.e. each row sums to 1)
row_distribution <- function(x){
  colorder <- names(x) # to preserve the column order
  tmp1 <- x[, !(sapply(x,class) %>% as.character %in% c("numeric","integer","double")), with = F] %>% as.data.table # keep only the columns that are NOT numeric.
  tmp2 <- x[,   sapply(x,class) %>% as.character %in% c("numeric","integer","double"), with = F] %>% # keep only the columns that are numeric
    apply(MARGIN = 1, FUN = function(y){y/sum(y, na.rm = T)}) %>% 
    t %>% 
    as.data.table %>% 
    replace_all_NAs
  
  y <- cbind(tmp1,tmp2) %>% setcolorder(colorder)
  return(y)
}