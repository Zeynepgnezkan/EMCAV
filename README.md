# EMCAV
R Package

Installation:
To install the package, please run the following code in R:

if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
install_github('Zeynepgnezkan/EMCAV')
If the above doesn't work, try this:

if('remotes' %in% rownames(installed.packages())==FALSE){
    install.packages('remotes')
    library(remotes)
}else{
    library(remotes)
}
install_url(url="https://github.com/Zeynepgnezkan/EMCAV/archive/master.zip", INSTALL_opt= "--no-multiarch")
