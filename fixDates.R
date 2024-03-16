files <- list.files("~/blosxom/data", pattern="^[cn][[:digit:]]",         
                    recursive=TRUE, full=TRUE)
dates <- sub("^.*/[nc]", "", sub(".txt$", "", files))
for (i in seq_along(files)) {
  if (difftime(Sys.time(), file.info(files[i])$mtime, units="days") < 10) { 
    print(files[i])
    system(paste("touch -d", dates[i], files[i])) 
  }
}
