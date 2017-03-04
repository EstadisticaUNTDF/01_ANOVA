ks.test_auto <- function(x) {
  #Determine parameters of distribution
  m <- mean(x)
  s <- sd(x)
  #Test
  ans <- ks.test(x, pnorm, m, s)
  c(ans[[1]], ans[[2]])
}

qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  c(slope = unname(slope), int = unname(int))
}


repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}

median.test.default <- function(x, na.rm = FALSE){
  great_median <- median(unlist(x), na.rm = na.rm)
  x_bool <- as.data.frame(sapply(x, function(x) x > great_median))
  cont_table <- table(melt(x_bool, measure.vars = 1:ncol(x_bool)))
  chisq.test(cont_table)
}  