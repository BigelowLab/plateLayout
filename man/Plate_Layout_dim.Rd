% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plate_Layout.R
\name{Plate_Layout_dim}
\alias{Plate_Layout_dim}
\title{Get the dimensions of the plate, currently on 12, 96 and 384 well plates are known}
\arguments{
\item{n}{numeric number of wells (optional).  If not provided then the length of the 
current definition is used}
}
\value{
a two element vector of [nrows, ncols] or NULL
}
\description{
Get the dimensions of the plate, currently on 12, 96 and 384 well plates are known
}
\seealso{
Other PLATE_LAYOUT: \code{\link{Create_Plate_Layout}},
  \code{\link{Plate_Layout_check_names}},
  \code{\link{Plate_Layout_colnames}},
  \code{\link{Plate_Layout_count}},
  \code{\link{Plate_Layout_dimnames}},
  \code{\link{Plate_Layout_get_names}},
  \code{\link{Plate_Layout_get_types}},
  \code{\link{Plate_Layout_read_file}},
  \code{\link{Plate_Layout_read_text}},
  \code{\link{Plate_Layout_rownames}},
  \code{\link{Plate_Layout_show}},
  \code{\link{Plate_Layout_to_tecan}},
  \code{\link{Plate_Layout_write_file}},
  \code{\link{Plate_Layout}}
}
