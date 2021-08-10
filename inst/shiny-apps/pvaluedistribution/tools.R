#------------------------------------------------------------------------------------------------------
# function to test if value is an integer so that df can be checked for non whole number values
#NOT USED. DIDNT WORK WITH SHINY prob apps for df limitation
# used until change basic plotting function to set scales for ranges of df instead of exact integers
# from http://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
#check.integer <- function(N){
#  !grepl("[^[:digit:]]", format(40, scientific = FALSE, digits = 20))
#}

# INSTEAD, USED THIS FROM THE as.integer help page
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

