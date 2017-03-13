qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "https://data.bls.gov:443/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# Current url:
# https://data.bls.gov:443/cew/data/api/2015/a/industry/1028.csv