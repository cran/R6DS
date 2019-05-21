## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F-------------------------------------------------------
library(R6)
library(R6DS)

## ----assignment----------------------------------------------------------
x <- 0
y <- x

## ----passing-------------------------------------------------------------
# define a function which changes the value of the passed variable,
# and then the new one
func <- function(val){ val <- -1; return(val) }

# remember that x = 0
z <- func(x)
x
z

## ----func, eval=F--------------------------------------------------------
#  # define a function which changes the value of the passed variable,
#  # and then the new one
#  func <- funcion(val){ print(val) }

## ----passing2, eval=F----------------------------------------------------
#  func <- function(val){ val <- -1 }
#  func(x)

## ----assignment2, eval=F-------------------------------------------------
#  y <- x

## ----passing3, eval=F----------------------------------------------------
#  func <- function(val){ val <- -1 }
#  func(x)

## ----rclass--------------------------------------------------------------
library(R6)

RClass <- R6Class("RClass", portable = FALSE, class = FALSE)

RClass$set("private", ".val", 0)

RClass$set("public", "initialize", function(val=0){ .val <<- val })

RClass$set("active", "Val", function(){ return(.val) })

RClass$set("public", "Set", function(newval){ .val <<- newval })

RClass$set("public", "finalize", function(){ print(paste("obj",.val,"deleted!")) })

## ----tmp1----------------------------------------------------------------
tmp1 = RClass$new()
rm(tmp1)

gc()

## ----aa_function---------------------------------------------------------
ftmp <- function(){ tmp <- RClass$new() }
ftmp()

gc()

## ----a_function----------------------------------------------------------
ftmp <- function(tmp){ tmp$Set(1) }

## ----tmp1_1--------------------------------------------------------------
tmp1 = RClass$new()
ftmp(tmp1)

gc()
tmp1$Val

## ----bst-----------------------------------------------------------------
# we define the "<" and "="
lessthan <- function(x, y) return(x < y)
equal <- function(x, y) return(x == y)
# then we initialize the tree
bst <- RBST$new(lessthan=lessthan, equal=equal)

# the nodes in a vector
nodes <- c(8, 3, 10, 1, 6, 14, 4, 7, 13)
# we add the nodes or elements
bst$insert(collapse = as.list(nodes))

## ----traverse------------------------------------------------------------
# create an empty container to hold the elements
# we choose the data structure queue
container <- RQueue$new()
container$size

# then we define the callback function which takes two arguments
# node: the node or element in the tree
# queue: additional argument hopefully pass-by-reference
callback <- function(item, queue) queue$enqueue(item)
# note that item is the value of the node but not the node!

# traverse-in-order
bst$traverse(mode="in", callback=callback, container)
# it should be a sorted list of the elements in the tree
# 1  3  4  6  7  8 10 13 14
unlist(container$toList)

# empty the container
container <- RQueue$new()
container$size

# traverse-pre-order
bst$traverse(mode="pre", callback=callback, container)
# it should be
# 8  3  1  6  4  7 10 14 13
unlist(container$toList)

# empty the container
container <- RQueue$new()
container$size

# traverse-post-order
bst$traverse(mode="post", callback=callback, container)
# it should be 
# 1  4  7  6  3 13 14 10  8
unlist(container$toList)

