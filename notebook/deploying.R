library(devtools);
library(roxygen2); # Read in the roxygen2 R package
load_all(".");
roxygenise();

devtools::document()
devtools::build()

