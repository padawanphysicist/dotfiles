options(java.parameters = "-Xmx8000m")

q <- function (save="no", ...) {
  quit(save=save, ...)
}

.Last <- function() {
  if(interactive()) {
    try(savehistory(Sys.getenv("R_HISTFILE")))
  }
}

## print.tbl_df <- function(x, ...) glimpse(x, ...)

message("\n*** Successfully loaded .Rprofile ***\n")
