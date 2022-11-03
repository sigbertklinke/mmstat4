if(interactive()) {
  # install from an official CRAN server
  install.packages("foreign")
  # install from a specific CRAN server
  install.packages("foreign", repos="https://ftp.gwdg.de/pub/misc/cran/")
  # install from GitHub
  install.packages("devtools")
  library("devtools")
  install_github("simecek/additivityTests")
}
