library("mmstat4")
# run example program
run("stat/sum.R")
# list of all example programs
prg()
prg(pattern=".py")  # show only python programs
# file name of example program
prg("stat/sum.R")
# editexample program
file.edit(prg("stat/sum.R"))  # RStudio editor
