# plateLayout for plates and arrays

# a pair of test files for use by Ben
#plateLayout.96 <- "/Users/Shared/data/scgc/FACS/layout/plate_layout_96_2.txt"
#plateLayout.384 <- "/Users/Shared/data/scgc/FACS/layout/plate_layout_384_2.txt"

#' Converts a suitable data.frame, matrix or list to class plateLayout
#'
#' A plateLayout is a character vector class.  The value of the vector is
#' the character description of the well type (i.e. a label) and the names
#' of each element are the names of the well placement.  We use a A01 format
#' description instead of A:1.  Wells are stored in A01, A02, A03 order along the 
#' length of the vector.  
#'
#' @family LAYOUT
#' @export
#' @param x a data.frame, list, vector or matrix of plate layout descriptions
#' @return a plateLayout class vector
plateLayout <- function(x){
   if (missing(x)) stop("data frame or matrix must be provided")
   if (is.plateLayout(x)) return(x)
   
   if (is.data.frame(x) || is.matrix(x)) {
      d = dim(x)
      nm <- getWellnames(d[1]*d[2])
      x <- as.vector(unlist(t(x)))
      names(x) <- nm
   } else if (is.matrix(x)){
      d <- length(x)
      nm <- getWellnames(d[1]*d[2])
      x <- as.vector(unlist(t(x)))
      names(x) <- nm
   } else {
      # list or vector lands us here
      if (is.list(x)) x <- unlist(x)
   }
   class(x) <- c(class(x), "plateLayout")
   x
}

#' Test is the object is of class plateLayout
#'
#' @family LAYOUT
#' @export
#' @param x the object to test
#' @return logical, TRUE if the object is of class plateLayout
is.plateLayout <- function(x) {
   inherits(x, "plateLayout")
}

#' Read a tab delimited plateLayout file
#'
#' Layout files define for well plates the "type" for each well.  Generally, these
#' files are created in a spreadsheet and saved in tab-delimited form for subsequent
#' use.
#'
#' @family LAYOUT
#' @export
#' @param file character, the fully qualified filename
#' @return a plateLayout class object
read.plateLayout <- function(file = NULL){
   if (is.null(file)) stop("read.plateLayout: file is required")
   x <- read.table(file, sep = "\t", header = TRUE, row.names = 1, stringsAsFactors = FALSE)
   colnames(x) <- sprintf("%2.2i", 1:ncol(x))
   plateLayout(x)
}

#' Write a tab-delimited plateLayout file
#'
#' @family LAYOUT
#' @export
#' @param x a plateLayout class object
#' @param file the fully qualified output filename
write.plateLayout <- function(x, file = "plateLayout.txt"){
   if (!is.plateLayout(x)) stop("Input must be of class plateLayout")
   m <- as.matrix(x)
   write.table(m, file = file, quote = FALSE, sep = "\t", row.names = TRUE)
}

#' Get a welltype for given well(s)
#'
#' @family LAYOUT
#' @export
#' @aliases getWellType
#' @param x a plateLayout object
#' @param well character vector of one or more well names
#' @param check logical, check well name for A01 format?
#' @return returns a named vector, one element for each input well name
getWellType <- function(x, well, check = TRUE) UseMethod("getWellType", x)
getWellType.plateLayout <- function(x, well, check = TRUE){
   if (check) { well <- check.wellnames(well)}
   ix = names(x) %in% well
   y <- unlist(x[ix]) 
   return(y)
}

#'  Returns the dimensions of a nwell plateLayout
#' 
#'  @family LAYOUT 
#'  @export
#'  @method dim plateLayout
#'  @param x either a plateLayout or an integer
#'  @return a two element vector of [nrows, ncols]
dim.plateLayout <- function(x){
   if (inherits(x, "plateLayout")) {
      nwell <- length(x)
   } else {
      nwell <- x
   }
   switch(as.character(nwell),
      "12" = c(3,4),
      "96" = c(8,12),
      "384" = c(16,24),
      NULL)
}


#' Returns the number of wells in a layout
#'
#' @family LAYOUT 
#' @export
#' @aliases countWells
#' @param x a plateLayout
#' @return numeric, number of wells
countWells <- function(x) UseMethod("countWells", x)
countWells.plateLayout <- function(x) {
   d <- dim(x)
   d[1] * d[2]
}


#' Returns the column names of the plateLayout object as if it were a matrix
#'
#' @family LAYOUT 
#' @export
#' @param x plateLayout class object
#' @return a character vector of the column names
colnames.plateLayout <- function(x){
   dims <- dim(x)
   sprintf("%2.2i", 1:dims[2])
}


#' Returns the row names of the plateLayout object as if it were a matrix
#'
#' @family LAYOUT 
#' @export
#' @param x plateLayout class object
#' @return a character vector of the row names
rownames.plateLayout <- function(x){
   dims <- dim(x)
   LETTERS[1:dims[1]]
}

#' Returns the dimnames of the plateLayout object as if it were a matrix
#'
#' @family LAYOUT 
#' @export
#' @method dimnames plateLayout
#' @param x plateLayout class object
#' @return a two element list of dimnames
dimnames.plateLayout <- function(x){
   dims <- dim(x)
   list(LETTERS[1:dims[1]] , sprintf("%2.2i", 1:dims[2]) )  
}

#' Returns a vector of names A01, A02,A03,...
#'
#' @family LAYOUT 
#' @export
#' @param x a plateLayout object or an integer (384, 96,  12 etc)
#' @param to.matrix logical, if TRUE then the returned value is an appropriately sized
#'  matrix rather than a vector
#' @return a vector (or matrix) of the wellnames
getWellnames <- function(x, to.matrix = FALSE) UseMethod("getWellnames",x)
getWellnames.numeric <- function(x, to.matrix = FALSE){
   dims <- dim.plateLayout(x)
   if (is.null(dims)) {return(NULL)}
   a <- LETTERS[1:dims[1]]
   b <- sprintf("%2.2i", 1:dims[2])
   nm <- unlist(lapply(a, paste, b, sep = ""))
   
   if (to.matrix){
      nm <- matrix(data = nm, ncol = dims[2], nrow = dims[1], byrow = TRUE)
   }
   return(nm)
}
getWellnames.plateLayout<- function(x, to.matrix = FALSE){
   nm <- names(x)
   if (to.matrix){
      dims <- dim(x)
      nm <- matrix(data = nm, ncol = dims[2], nrow = dims[1], byrow = TRUE)
   }
   return(nm)
}


#' Returns a matrix form of the plate layout
#'
#' @family LAYOUT
#' @export
#' @method as.matrix plateLayout
#' @param x a plateLayout class object
#' @param ... further arguments (ignored)
#' @return a matrix of the well types with dimnames
as.matrix.plateLayout <- function(x, ...){
   dims <- dim(x)
   matrix(data = x, ncol = dims[2], nrow = dims[1], byrow = TRUE,
      dimnames = dimnames(x), ...)
}

#' Creates a plateLayout with the names attached and type represented by 
#' the a random selection of \code{ntype}.
#'
#' @family LAYOUT 
#' @export
#' @param nwell integer, the number of wells on the plate
#' @param ntype integer, the number of unique dummy types to sample from LETTERS
#' @param to.matrix logical, if TRUE return a matrix
create.plateLayout<- function(nwell = 384, ntype = 3, to.matrix = FALSE){
   x <- sample(LETTERS[seq_len(ntype)], nwell, replace = TRUE)
   names(x) <- getWellnames(nwell, to.matrix = FALSE)
   class(x) <- c(class(x), "plateLayout")
   if (to.matrix){
      dims <- dim(x)
      x <- matrix(data = x, ncol = dims[2], nrow = dims[1], byrow = TRUE)
   }

   return(x)
}

#' Translates A01, A02 names to Tecan position assignments
#' 
#' Tecan assigns position number for each in the first column, then by each in 
#' the second column so that A01 -> 1, B01 -> 2, C01 -> 3 and so on
#'
#' @family LAYOUT
#' @export
#' @param x a plateLayout class object
#' @return a numeric vector of Tecan position
to_tecan <- function(x) UseMethod("to_tecan", x) 
to_tecan.plateLayout <- function(x){
   n <- countWells(x)
   dims <- dim(x)
   y <- as.vector(t(matrix(1:n, ncol = dims[2], nrow = dims[1], byrow = FALSE)))
   names(y) <- names(x)
   y
}

#' Checks a well name to make sure it follows the A01 format
#'
#' @export
#' @param well character vector of well names
#' @param reverse logical, if TRUE convert "A01"... to  "A:1" "B:2" "C:3"
#' @return a character vector of well names
check.wellname <- function(well, reverse = FALSE){
   
   if (reverse) {
      well <- check.wellname(well)
      L <- substring(well, 1,1)
      N <- as.numeric(substring(well, 2, nchar(well)))
      well <- sprintf("%s:%s", L, N)
   } else {
      well <- gsub(":", "", well, fixed = TRUE)
      ix <- nchar(well) < 3
      x <- strsplit(well[ix], "")
      well[ix] <- paste(sapply(x,'[', 1), sprintf("%2.2i", as.numeric(sapply(x,'[', 2))), sep = "") 
   }
   return(well)
}

#' A wrapper around \code{\link{check.wellname}}
#'
#' @export
#' @param well character vector of well names
#' @param ... further arguments for \code{\link{check.wellname}}
#' @return a character vector of well names
check.wellnames <- function(well, ...){
   check.wellname(well,...)}

