# This function will take a vector with names of libraries and check if 
# they are installed. If not, it will install them and load them
load_libraries <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}
