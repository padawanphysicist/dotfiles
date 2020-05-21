options("width"=80)
# options(prompt="R> ", digits=4, show.signif.stars=FALSE)
options("repos" = c(CRAN = "http://cran.r-project.org/"))

q <- function (save="no", ...) {
  quit(save=save, ...)
}

#if(Sys.getenv("TERM") == "xterm-256color")
#  library("colorout")

.Last <- function() {
  if(interactive()) {
    try(savehistory(Sys.getenv("R_HISTFILE")))
  }
}

message("\n*** Successfully loaded .Rprofile ***\n")
