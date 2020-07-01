Source_FVR_py <- function(notebook = TRUE) {
  ##### Source our Python Code for Later Use ####
  # All the code
  if (notebook == TRUE) {
    filelist <- list.files("../python/")
    for (f in filelist) {
      source_python(paste0("../python/", f))
    }
  } else if (notebook == FALSE) {
    filelist <- list.files("python/")
    for (f in filelist) {
      source_python(paste0("python/", f))
    }
  }

}
