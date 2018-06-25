# Functions to flag the first, and/or last observations 
#  based on a particular key variable such as ID number.
first <- function(x, key, verbose = TRUE){
  
  a <- deparse(substitute(x))
  
  if (missing(key)) {
    b = names(x[1])
    # print(key)
    # print(deparse(substitute(key)))
    # print(paste0(a, "$", key))
    nam <- paste0("first.", b)
    nam2 <- paste0(a, "$", b)
    # print(nam)
    # print(nam2)
    
    message("Using the first column in that data set since a key variable was not specified. \n")

  } else if (!missing(key)){
  
    b <- deparse(substitute(key))
    nam <- paste0("first.", b)
    nam2 <- paste0(a,"$", b)
  }
  
  c <- eval(parse(text = nam2))
  d <- as.numeric(!duplicated(c))
  assign(nam, d, envir = .GlobalEnv)
  
  
  if (verbose) {
    message(paste0("The first observations were flagged for the column labeled",
                   " `", b ,"` ",
                   "in data set,",
                   " `", a ,"` ",
                   "and stored in the vector",
                   " `", nam ,"`. "))
  }
} # end first()

last <- function(x, key){
  a <- deparse(substitute(x))
  b <- deparse(substitute(key))
  nam <- paste0("last.", b)
  nam2 <- paste0(a,"$", b)
  c <- eval(parse(text = nam2))
  d <- as.numeric(!duplicated(c, fromLast = T))
  assign(nam, d, envir = .GlobalEnv)
}

firstLast <- function(x, key){
  # first observation
  a <- deparse(substitute(x))
  b <- deparse(substitute(key))
  nam <- paste0("first.", b)
  nam2 <- paste0(a,"$", b)
  c <- eval(parse(text = nam2))
  d <- as.numeric(!duplicated(c))
  assign(nam, d, envir = .GlobalEnv)
  
  # last observation
  e <- deparse(substitute(x))
  f <- deparse(substitute(key))
  nam3 <- paste0("last.", f)
  nam4 <- paste0(a,"$", f)
  g <- eval(parse(text = nam4))
  h <- as.numeric(!duplicated(g, fromLast = T))
  assign(nam3, h, envir = .GlobalEnv)
}

# Example code
df <-data.frame(id=rep(seq(10), each=10), thing = rep(c("A","B")))
df

library(dplyr)
df <- data.frame(id=rep(seq(10), each=10)) %>% 
      mutate(thing = sample(c("A","B"), length(id), replace = T)) 

df <- df %>% mutate(first.id) %>% filter(first.id==1)
df

first(df, id)
last(df, id)
firstLast(df, id)
