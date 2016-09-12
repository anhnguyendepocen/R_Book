#' 
#' Appendix 1: Additional Material
#' 
#' 
#' ## Regular Expressions
#' 
#' 
#' ### Exercises
#' 
#' #### 1: String to numeric
#' 
#' Consider a vector of values of this form:
#' 
#' strings <- c("ASk/20005-01-45/90", "Alldatk/25-17-4567/990")
#' 
#' The goal is to extract the numerical values in the centre
#' between the hyphens, i.e. '01' in '-01-', and turn these 
#' characters into a numerical vector. Can you do it with a 
#' single substitution string so that you would have something
#' like the following?
#' 
#' numvec <- as.numeric(gsub(A, B, strings))
#' 
#' What values of 'A' and 'B' would do the trick?
#' 
#' <!--- Some solutions:
#' 
#' strings <- c("ASk/20005-01-45/90", "Alldatk/25-17-4567/990")
#' x <- as.numeric(sub("^[^-]*-([0-9]+)-.*$","\\1",strings))
#' x
#' x <- as.numeric(gsub("^[^-]*-|-.*$","",strings))
#' x
#' -->
#' 
