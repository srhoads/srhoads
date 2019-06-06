
pkg <- function (package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) 
      do.call(library, list(package))
    else {
      install.packages(package, 
                       repos = c("https://cloud.r-project.org", 
                                 "http://owi.usgs.gov/R/"), dependencies = NA, 
                       type = getOption("pkgType"))
      do.call(library, list(package))
    }
  }
}

pkg_tidy <- function (package1="tidyverse", ...) {
  packages <- unique(c(c(package1, ...), "magrittr"))
  for (package in packages) {
    if (package %in% rownames(installed.packages())) do.call(library, list(package))
    else {
      install.packages(package, repos = c("https://cloud.r-project.org", 
                                          "http://owi.usgs.gov/R/"), dependencies = NA, 
                       type = getOption("pkgType"))
      do.call(library, list(package))
    }
  }
}

install.packages_wrapper <- function(package,  dependencies = NA, githubrepo=NULL,
                                     repos = c("https://cloud.r-project.org", "http://owi.usgs.gov/R/"), 
                                     type = getOption("pkgType")){
  tryCatch(install.packages(package, repos=repos, dependencies=dependencies, type=type),
           error=function(e){
             if ('devtools' %in% rownames(installed.packages())) do.call(library, list(package)) else install.packages("devtools")
             if(is.null(githubrepo)|!exists("githubrepo")) githubrepo <- "srhoads"
             tryCatch(devtools::install_github(paste0(githubrepo, "/", package)), error=function(e) paste0(package, " --can't find"))
           })
}  


pkg2 <- function (package1=NULL, ..., pipes=T) {
  if(is.null(package1)) package1 <- "tidyverse"
  packages <- unique(c(package1, ...))
  if(pipes) packages <- unique(c(packages, "magrittr"))
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {do.call(library, list(package)); print(paste0(package, " loaded"))}
    else {
      tryCatch(install.packages_wrapper(package), error=function(e) paste0(package, " idk"))
      tryCatch(do.call(library, list(package)), error=function(e) paste0(package, " idk"))
    }
  }
}