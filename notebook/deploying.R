library(devtools);
library(roxygen2); # Read in the roxygen2 R package
load_all(".");
roxygenise();

#devtools::document()
devtools::build()

#bridge23_kab= rio::import("data/bridge23_kab.Rds")
#bridge23_prov= rio::import("data/bridge23_prov.Rds")
usethis::use_data(bridge23_kab, bridge23_prov, overwrite = T)

