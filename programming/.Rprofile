options("width"=80)
options("repos" = c(CRAN = "http://cran.r-project.org/"))
options("crayon.enabled"=FALSE)
options(java.parameters = "-Xmx8000m")

q <- function (save="no", ...) {
  quit(save=save, ...)
}

.Last <- function() {
  if(interactive()) {
    try(savehistory(Sys.getenv("R_HISTFILE")))
  }
}

message("\n*** Successfully loaded .Rprofile ***\n")
