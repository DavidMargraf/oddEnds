# Functions to find the first, and/or last observations 
#  based on a particular key variable such as ID number.
first <- function(x, key){
  a <- deparse(substitute(x))
  b <- deparse(substitute(key))
  nam <- make.names(paste0("first.", b))
  nam2 <- paste0(a,"$", b)
  c <- eval(parse(text = nam2))
  d <- as.numeric(!duplicated(c))
  assign(nam, d, envir = .GlobalEnv)
}

last <- function(x, key){
  a <- deparse(substitute(x))
  b <- deparse(substitute(key))
  nam <- make.names(paste0("last.", b))
  nam2 <- paste0(a,"$", b)
  c <- eval(parse(text = nam2))
  d <- as.numeric(!duplicated(c, fromLast = T))
  assign(nam, d, envir = .GlobalEnv)
}

firstLast <- function(x, key){
  # first observation
  a <- deparse(substitute(x))
  b <- deparse(substitute(key))
  nam <- make.names(paste0("first.", b))
  nam2 <- paste0(a,"$", b)
  c <- eval(parse(text = nam2))
  d <- as.numeric(!duplicated(c))
  assign(nam, d, envir = .GlobalEnv)
  
  # last observation
  e <- deparse(substitute(x))
  f <- deparse(substitute(key))
  nam3 <- make.names(paste0("last.", f))
  nam4 <- paste0(a,"$", f)
  g <- eval(parse(text = nam4))
  h <- as.numeric(!duplicated(g, fromLast = T))
  assign(nam3, h, envir = .GlobalEnv)
}

# Example code
df <-data.frame(id=rep(seq(10), each=10), thing = rep(c("A","B")))
df

first(df, id)
last(df, id)
firstLast(df, id)
