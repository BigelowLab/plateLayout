#Container_Layout RefClass

# the definition
Plate_LayoutRefClass <- setRefClass("Plate_LayoutRefClass", fields = list(data = 'character'))


#' A pretty summary of the object
#'
#' @family PLATE_LAYOUT
#' @name Plate_Layout_show
NULL
Plate_LayoutRefClass$methods(
    show = function(){
        cat("Reference object of class",classLabel(class(.self)), "\n")
        if (length(.self$data) > 0){
            pdim <- .self$dim()
            tbl <- table(.self$data)
            cat("   nwell:", .self$count(), "\n")
            cat(sprintf("   dim: [ %d, %d ]", pdim[1], pdim[2]), "\n")
            for (tb in names(tbl)) cat(sprintf("      %s: %0.0f", tb, tbl[tb]), "\n") 
        } else {
            cat("   no data loaded\n")
        }
    })
#' Get the dimensions of the plate, currently on 12, 96 and 384 well plates are known
#' 
#' @name Plate_Layout_dim
#' @family PLATE_LAYOUT
#' @param n numeric number of wells (optional).  If not provided then the length of the 
#'    current definition is used
#' @return a two element vector of [nrows, ncols] or NULL
NULL
Plate_LayoutRefClass$methods(
   dim = function(n){
      if (missing(n)) n <- length(.self$data)
      switch(as.character(n),
         "12" = c(3,4),
         "96" = c(8,12),
         "384" = c(16,24),
         NULL)
   })


#' Return the number of wells
#' 
#' @name Plate_Layout_count
#' @family PLATE_LAYOUT
#' @return the number of wells
NULL
Plate_LayoutRefClass$methods(
   count = function(){
      length(.self$data)
   })

#' Retrieve the column names
#' 
#' @name Plate_Layout_colnames
#' @family PLATE_LAYOUT
#' @param the format used for the column names
#' @return the number of wells
NULL
Plate_LayoutRefClass$methods(
   colnames = function(format = "%2.2i"){
      d <- .self$dim()
      if (is.null(d)) return(d)
      sprintf("%2.2i", 1:d[2])
   })

#' Retrieve the row names
#' 
#' @name Plate_Layout_rownames
#' @family PLATE_LAYOUT
#' @return the rownames of wells
NULL
Plate_LayoutRefClass$methods(
   rownames = function(){
      d <- .self$dim()
      if (is.null(d)) return(d)
      LETTERS[1:d[1]]
   })
 
#' Returns the dimnames of the plate as if it were a matrix
#' 
#' @name Plate_Layout_dimnames
#' @family PLATE_LAYOUT
#' @return the dim names of wells
NULL
Plate_LayoutRefClass$methods(
   dimnames = function(){
      list(.self$rownames(), .self$colnames())
   })
     

#' Get the names of the wells
#'
#' @name Plate_Layout_get_names
#' @family PLATE_LAYOUT
#' @param n numeric number of wells (optional).  If not provided then the length of the 
#'    current definition is used
#' @param asMatrix logical, if TRUE return the names in a matrix
#' @return either a matrix or vector of well names or NULL
NULL
Plate_LayoutRefClass$methods(
   get_names = function(n, asMatrix = FALSE){
      if (missing(n)) n <- .self$count()
      d <- .self$dim(n)
      if (is.null(d)) return(NULL)
      a <- LETTERS[1:d[1]]
      b <- sprintf("%2.2i", 1:d[2])
      nm <- unlist(lapply(a, paste, b, sep = ""))
      if (asMatrix) nm <- matrix(nm, ncol = d[2], nrow = d[1], byrow = TRUE,
         dimnames = .self$dimnames()) 
      nm
   })

#' Force one or more well names into the A01 or A:1 format
#'
#' @name Plate_Layout_check_names
#' @family PLATE_LAYOUT
#' @param well character vector of one or more well names
#' @param reverse logical, if true then produce A:1 else produce A01
#' @return named character vector
NULL
Plate_LayoutRefClass$methods(
   check_names = function(well, reverse = FALSE){
      if (reverse){
         well <- .self$check_names(well)
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
   })


#' Get one or more well types
#' 
#' @name Plate_Layout_get_types
#' @family PLATE_LAYOUT
#' @param well character vector of one or more well names, by default all are provided
#' @param check logical, test if well name is in A01 format?
#' @param asMatrix logical, if TRUE return the names in a matrix
#' @return named character vector
NULL
Plate_LayoutRefClass$methods(
   get_types = function(well, check = TRUE, asMatrix = FALSE){
      if (missing(well)) well <- .self$get_names()
      if (check) well <- .self$check_names(well)
      ix <- names(.self$data) %in% well
      #return(unlist(.self$data[ix])) 
      typ <- unlist(.self$data[ix])
      if (asMatrix) {
         d <- .self$dim()
         typ <- matrix(typ, ncol = d[2], nrow = d[1], byrow = TRUE,
          dimnames = .self$dimnames() )
      }
      typ
   })
#' Read layout information from a character vector
#' 
#' @name Plate_Layout_read_text
#' @family PLATE_LAYOUT
#' @param txt the input charcater vector, list, data.frame or matrix.  Wellnames for 
#'    the latter two are guessed at.  If a vector or list then the names provided are used.
#' @return the length of the data vector invisibly
NULL
Plate_LayoutRefClass$methods(
   read_text = function(txt){
      if (is.plateLayout(txt)){
         .self$data <- unlist(txt)
      } else if (is.data.frame(txt) || is.matrix(txt)) {
         d <- base::dim(txt)
         .self$data <- as.vector(unlist(t(txt)))
         names(.self$data) <- .self$get_names(d[1]*d[2])
      } else {
         .self$data <- unlist(txt)
      }
      invisible(.self$count())
   })
   
#' Read layout information from a file
#' 
#' @name Plate_Layout_read_file
#' @family PLATE_LAYOUT
#' @param filename the input filename
#' @return the length of the data vector invisibly
NULL
Plate_LayoutRefClass$methods(
   read_file = function(filename){
      if (missing(filename)) stop("filename must be provided")
      if (!file.exists(filename)) stop(paste("filename not found:", filename))
      x <- read.table(filename, sep = "\t", header = TRUE, row.names = 1, stringsAsFactors = FALSE)
      .self$read_text(x)
   })

#' Write layout information to a file
#' 
#' @name Plate_Layout_write_file
#' @family PLATE_LAYOUT
#' @param filename the output filename
#' @return logical
NULL
Plate_LayoutRefClass$methods(
   write_file = function(filename){
      d <- .self$dim()
      m <- matrix(.self$data, byrow = TRUE, 
         ncol = d[2], nrow = d[1], dimnames = .self$dimnames())
      write.table(m, file = filename, quote = FALSE, sep = "\t", 
         row.names = TRUE, col.names = NA)
      sapply(filename, file.exists)
   })
   
#' Translates A01, A02 names to Tecan position assignments
#' 
#' Tecan assigns position number for each in the first column, then by each in 
#' the second column so that A01 -> 1, B01 -> 2, C01 -> 3 and so on
#'
#' @name Plate_Layout_to_tecan
#' @family PLATE_LAYOUT
#' @return a character vector of Tecan position invisibly
NULL
Plate_LayoutRefClass$methods(
   to_tecan = function(){
      n <- .self$count()
      if (n == 0) return(NULL)
      d <- .self$dim()
      y <- as.vector(t(matrix(as.character(1:n), ncol = d[2], nrow = d[1], byrow = FALSE)))
      names(y) <- names(.self$data)
      invisible(y)
   })

####### Methods above
####### Functions below

#' Create an instance of a Plate_Layout from text, plateLayout object or file
#'
#' @family PLATE_LAYOUT
#' @export
#' @param x either a character vector, plateLayout class, OR a filename
#' @param asText logical indicating the input is a character vector to parse
#' @return an instance of Plate_Layout reference class
Plate_Layout <- function(x, asText = FALSE){
   X <- Plate_LayoutRefClass$new()
   if (!missing(x)){
      if (asText) {
         X$read_text(x)
      } else if (is.plateLayout(x)){
         X$read_text(x)
      } else {
         if (!file.exists(x[1])) stop(paste("file not found:" , x[1]))
         X$read_file(x[1])
      }
   }
   invisible(X)
}

#' Create and instance of Plate_Layout with fake data
#'
#' @family PLATE_LAYOUT
#' @export
#' @param nwell integer, the number of wells on the plate
#' @param ntype integer, the number of unique dummy types to sample from LETTERS
#' @return an instance of Plate_Layout reference class
Create_Plate_Layout <- function(nwell = 384, ntype = 3){
   X <- Plate_Layout()
   if (nwell != 1){
     x <- sample(LETTERS[seq_len(ntype)], nwell, replace = TRUE)
     names(x) <- X$get_names(nwell)
   } else {
     x <- c('1' = "1:1")
   }
   X$read_text(x)
   invisible(X)
}

#' Create a lookup table for translating tecan (1,2,3,...) to A01 (A01, B01, C01,...)
#'
#' @export
#' @param nwell numeric the number of wells in the container
#' @param invert logical if TRUE flip the names and values to yield
#'    a character vector c(1 = A01, 2 = B01, etc)
#' @return named character vector c(A01 = 1, B01 = 2, etc)
tecan_A01 <- function(nwell = 384, invert = FALSE){
    if (nwell > 1){
        x <- Create_Plate_Layout(nwell)$to_tecan()
    } else {
        x <- c("1:1" = "1")
    }
    if (invert){
        x <- structure(names(x), .Names = as.character(unname(x)))
    }
    return(x)
}
