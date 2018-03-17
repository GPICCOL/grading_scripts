library(rmarkdown)
library(rjson)

gradefile <- "_grades"
dir <- "e2_3/e"
practice <- "e2e"

renderReport <- function(fname, lname, fn, tms){
  render(input = paste(dir, "report.Rmd", sep = "/"), output_file = paste0(fn, ".pdf"), 
         params = list(fn = fname, ln = lname, filename = fn, timestamp = tms))
}

renderJSON <- function(dt){
  
}



computeScores <- function(fn){
  d <- read.csv(paste0(dir, "/", fn,".csv"), stringsAsFactors = FALSE, strip.white = TRUE)
  colnames(d) <- c("Task", "PA", "PE")
  totpa <- d[which(d$Task == "Total"), c("PA")]
  totpe <- d[which(d$Task == "Total"), c("PE")]
  g = totpe/totpa
  return(g)
}

filenames <- tolower(list.files(dir, pattern="*.csv", full.names=FALSE))
id <- sapply(strsplit(filenames, split = "_", fixed = TRUE), "[", 1)
id <- id[nchar(id)>0]

#Create full roster
nm <- unique(read.csv("../analysis/id.csv", stringsAsFactors = FALSE)[ , 2:5])

#Create list of files to grade
n <- nm[which(nm$pawsid %in% tolower(id)), ]
n$fl_name <- sapply(n$pawsid, function(x) filenames[grep(x, filenames)])
n$fl_name <- sapply(strsplit(n$fl_name, split = ".", fixed = TRUE), "[", 1)

n$tmstp <- sapply(strsplit(n$fl_name, split = "_", fixed = TRUE), "[", 3)
n$tmstp <- as.POSIXct(as.numeric(n$tmstp)/1000, origin="1970-01-01", tz = "America/Chicago")

#Compute Grades
grades <- sapply(unique(n$fl_name), computeScores)
gdf <- data.frame(pawsid = sapply(strsplit(names(grades), split = "_", fixed = TRUE), "[", 1), 
                  grades = grades)
gdf <- merge(nm, gdf, by = "pawsid", all.x = TRUE)
gdf <- gdf[order(gdf$l_name) , ]
write.csv(gdf, file = paste0(dir, "/", gradefile,".csv"), row.names = FALSE)

#Create Individual Reports
for(i in 1:dim(n)[1]){
  renderReport(n$f_name[i], n$l_name[i], n$fl_name[i], n$tmstp[i])
  }

#Single Report Generation
#renderReport(n$f_name[1], n$l_name[1], n$fl_name[1], n$tmstp[1])
#renderReport(n$f_name[5], n$l_name[5], n$fl_name[5])
#renderReport(n$f_name[1], n$l_name[1], paste0(n$pawsid[1],"_",practice))

