devtools::install_github(remotes::install_github("bss-osca/tfa/tfa-package", upgrade = FALSE))

url <- "https://raw.githubusercontent.com/bss-osca/tfa/master/slides/03-transform/03-transform_examples.Rmd"
download.file(url, 
              "03-transform_ex.Rmd",   # stores file in R working directory
              mode="w")                # "w" means "write," and is used for text files