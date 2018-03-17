library(rmarkdown)

scoresfile <- "_scores"
dir <- "resource_use/"

sessions <- "sessionsupto1023"
pages <- "pvsupto1023"
class <- "classAttendance"
si <- ""
openlab <- ""

fill_dates <- function(x, dt){
  d <- seq.Date(min(dt$Date, na.rm = TRUE), max(dt$Date, na.rm = TRUE), by= "day")
  z <- data.frame(ID = rep(x, length(d)), Date = d)
  z <- merge(z, dt[which(dt$ID == x), ], all.x = TRUE, by.x = "Date", by.y = "Date")
  return(z)
}

renderReport <- function(fname, lname, fn, r, s, d, m){
  render(input = paste(dir, "progress_report.Rmd", sep = ""), output_file = paste0(fn, ".pdf"), 
         params = list(fn = fname, ln = lname, range = r, sessions = s, days = d, means = m))
}

# session_usage <-  function(x) {
#   tot <- sapply(split(x$sessions, x$pawsid), function(z) sum(z>0))
#   return(tot)
# }

#Create full roster
nm <- read.csv("../analysis/id.csv", stringsAsFactors = FALSE)[ , c(2:5, 11, 14)]

#Create datasets
##Attendance
a <- read.csv(paste0(dir, class, ".csv"), stringsAsFactors = FALSE)
d <- as.Date(gsub("X", "", colnames(a[4:dim(a)[2]])), format = "%m.%d.%y")


##Sessions
s <- read.csv(paste0(dir, sessions, ".csv"), stringsAsFactors = FALSE)[ , 2:5]
s$Date <- as.Date(as.character(s$Date), format = "%Y%m%d")
s <- as.data.frame(do.call(rbind, lapply(as.character(na.omit(unique(s$ID))), fill_dates, dt = s)))
s$Sessions[is.na(s$Sessions)] <- 0
s <- s[ , c(2,1,4,5)]
names(s) <- c("ID", "date", "name", "sessions")
s <- merge(s, nm[ , c("ID", "pawsid")], all.x = FALSE, by.x = "ID", by.y = "ID")
s <- s[with(s, order(ID, date)), ]

##Page Views
p <- read.csv(paste0(dir, pages, ".csv"), stringsAsFactors = FALSE)[ , 2:6]
p$Date <- as.Date(as.character(p$Date), format = "%Y%m%d")
p <- as.data.frame(do.call(rbind, lapply(as.character(na.omit(unique(s$ID))), fill_dates, dt = p)))
p <- p[ , c(2,1,5,6)]
names(p) <- c("ID", "date", "page", "views")
p <- merge(p, nm[ , c("ID", "pawsid")], all.x = FALSE, by.x = "ID", by.y = "ID")

##Attendance


##Supplemental Instructor Review


##Open Lab





#Compute Results
##Sessions
total <- s
fourweeks <- s[s$date >= "2017-09-22" & s$date <=  "2017-10-22", ]
twoweeks <- s[s$date >= "2017-10-08" & s$date <= "2017-10-22", ]
l <- list(tot = total, four = fourweeks, two = twoweeks)
sessions <- do.call(cbind, lapply(l, function(x) sapply(split(x$sessions, x$pawsid), function(z) sum(z))))

days <- do.call(cbind, lapply(l, function(x) sapply(split(x$sessions, x$pawsid), function(z) sum(z>0))))

ranges <- lapply(l, function(x) range(x$date))

means <- list(sessions = format(round(colSums(sessions)/dim(sessions)[1], 2), nsmall = 2), 
              days = format(round(colSums(days)/dim(sessions)[1], 2), nsmall = 2))






#Single Report Generation
paws <- "adani54"
renderReport(nm$f_name[nm$pawsid == paws], nm$l_name[nm$pawsid == paws], nm$pawsid[nm$pawsid == paws], 
             ranges, sessions[paws, ], days[paws, ], means)



with(s[s$ID == "599e05ed7d7a095e1d57f5d5", ], sum(sessions>0))


with(s[s$ID == "599e05ed7d7a095e1d57f5d5", ], plot(date, sessions, type = "l"))
with(p[p$ID == "599ef1baed88a26328bebe65", ], plot(date, views, type = "l"))






#REPORT BUILDING


#Create list of files to grade
n <- nm[which(nm$pawsid %in% tolower(id)), ]

#Create Individual Reports
for(i in 1:dim(n)[1]){
  renderReport(n$f_name[i], n$l_name[i], n$pawsid[i])
}

#Single Report Generation
#renderReport(n$f_name[5], n$l_name[5], n$pawsid[5])





