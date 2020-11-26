padz <- function(x, n=max(nchar(x))){ gsub(" ", "0", formatC(x, width=n))}
