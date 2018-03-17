library(rmarkdown)
library(jsonlite)

gradefile <- "_grades"
dir <- "Mar_16"

filenames <- tolower(list.files(dir, pattern="*.csv", full.names=FALSE))
#filenames <- filenames[-which(filenames == paste0(gradefile, ".csv"))]
id <- sapply(strsplit(filenames, split = "_", fixed = TRUE), "[", 1)
id <- id[nchar(id)>0]

#Create full roster
nm <- unique(read.csv("../analysis/id.csv", stringsAsFactors = FALSE)[ , 2:5])

#Create list of files to grade
n <- nm[sapply(id, function(x) which(nm$pawsid == x)), ]
#n$fl_name <- sapply(n$pawsid, function(x) filenames[grep(x, filenames)])
n$fln <- filenames
n$fl_name <- sapply(strsplit(n$fln, split = ".", fixed = TRUE), "[", 1)

n$tmstp <- sapply(strsplit(n$fl_name, split = "_", fixed = TRUE), "[", 3)
n$tmstp <- as.POSIXct(as.numeric(n$tmstp)/1000, origin="1970-01-01", tz = "America/Chicago")


#Create JSON
df <- n
df[8:11] <- NA
colnames(df) <- c("fName", "lName", "pawsid", "email", "origfile", "filename", "timestamp",
                  "submissiontime","excercise","task","totalscore")
exc <- sapply(strsplit(df$origfile, split = "_", fixed = TRUE), "[", 2)
df$task <- sapply(strsplit(exc, split = "w{1}[0-9]", fixed = FALSE), "[", 2)
df$excercise <- regmatches(exc,regexpr("w{1}[0-9]",exc))
df$submissiontime <- sapply(strsplit(df$filename, split = "_", fixed = TRUE), "[", 3)

scoreJSON <- function(file_name, flag){
  fl <- read.csv(paste0(dir, "/", file_name), stringsAsFactors = FALSE, strip.white = TRUE)
  colnames(fl) <- c("question", "points", "earned")
  score <- fl[which(fl$question == "Total"), 3]/fl[which(fl$question == "Total"), 2]*100
  if (flag == "score") {
    return(score)
  }
  if (flag == "results") {
    stp <- which(fl$question == "Total") - 1
    fl <- fl[1:stp, ]
    fl[which(fl$points == 0), 2:3] <- "Not Evaluated"
    return(fl)
  }
}

df$totalscore <- sapply(df$origfile, scoreJSON, flag = "score")
write_json(unname(apply(df, 1, function(x) c(as.list(x), list(results = scoreJSON(x[5], flag = "results"))))), 
           paste0(dir, "/", dir, "_results.js"), pretty = TRUE, auto_unbox = TRUE)

#Save grades
write.csv(df, file = paste0(dir, "/", gradefile,".csv"), row.names = FALSE)

