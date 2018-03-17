library(mailR)

pwd <- strsplit(read.table("../credentials.txt", stringsAsFactors = FALSE)[2, ], split = ":")[[1]][2]
dir <- "e1/a"
t <- "Report: Microsoft Excel Practice Exercise"
b <- "Please, find attached the report for the practice exercise you submitted."

#Sender Function
sendReport <- function(pid, title, body, report, p){
  address <- paste0(pid, "@lsu.edu")
  send.mail(from = "isds1102s4@lsu.edu", to = address, subject = title, body = body,
            encoding = "utf-8", smtp = list(
              host.name = "outlook.office365.com",
              port = 587,
              user.name = "isds1102s4@lsu.edu",
              passwd = p,
              tls = TRUE), 
            authenticate = TRUE, attach.files = report)
}

#Create full roster
nm <- unique(read.csv("../analysis/id.csv", stringsAsFactors = FALSE)[ , 2:5])

#filenames <- list.files(dir, pattern="*.csv", full.names=FALSE)
filenames <- list.files(dir, pattern="*.pdf", full.names=FALSE)
id <- sapply(strsplit(filenames, split = "_", fixed = TRUE), "[", 1)

n <- nm[which(nm$pawsid %in% tolower(id)), ]
n$fl_name <- sapply(n$pawsid, function(x) filenames[grep(x, filenames)])
n$fl_name <- sapply(strsplit(n$fl_name, split = ".", fixed = TRUE), "[", 1)

n$attach <- paste0(dir, "/", n$fl_name,".pdf")

#Make body
b <- paste("Dear", n$f_name, n$l_name, "\n\n", b, "\n\nSincerely, \n\nThe ISDS1102 team\n\n", sep = " ")


#Send Individual Reports
#for(i in 19:25){  
for(i in 1:dim(n)[1]){
  sendReport(n$pawsid[i], t, b[i], n$attach[i], pwd)
  sendReport("isds1102s4", t, b[i], n$attach[i], pwd)
}


#Send Single Reports
#sendReport(n$pawsid[5], t, b[5], n$attach[5], pwd)
#sendReport("isds1102s4", t, b[5], n$attach[5], pwd)
