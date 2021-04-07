dirs  <- list.dirs(full.names = FALSE, recursive = FALSE)
files <- setdiff(list.files(), dirs)
for (file in files) {
  cat(file, ': ')
  i <- 0
  for (dir in dirs) {
    fname <- paste0(dir, '/', file)
    if (file.exists(fname)) {
      i <- i+1
      unlink(fname)
      cmd <- sprintf("cd %s; ln -s ../%s; cd ..", dir, file)
      system(cmd)
      print(cmd)
    }
  }
  cat(i, ' soft links created', "\n")
}