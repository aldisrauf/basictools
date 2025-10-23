library(devtools);
library(roxygen2); # Read in the roxygen2 R package

load_all(".");
roxygenise();

#devtools::document()
devtools::build()


library(rio)
bridge23_kab= rio::import("data/bridge23_kab.Rda")
bridge23_prov= rio::import("data/bridge23_prov.Rda")
kbli_details= rio::import("data/keterangan kbli.Rds")
export(kbli_details, "data/kbli_details.rda")
usethis::use_data(bridge23_kab, bridge23_prov, kbli_details, overwrite = T)

