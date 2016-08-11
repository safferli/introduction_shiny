# require(devtools)
# library(httr)
# set_config(use_proxy(url="10.26.0.16", port=3128))
# install_github("ramnathv/slidify")
# install_github("ramnathv/slidifyLibraries")

library(slidify)

wd <- path.expand("~/Documents/github/introduction_shiny")
setwd(wd)

author("shiny-introduction", use_git = FALSE)
slidify("index.Rmd")
