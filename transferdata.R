
filenames <- list.files("./data/Data files/")

nodat <- sub(".dat", "", filenames)

csvs_value <- grep(".csv", nodat, value = TRUE);csvs_value

for(v in csvs_value){
  nodat <- nodat[nodat!=v]
}

nodat <- nodat[nodat != "tosse.r"]



for(filename in nodat) {
  datfile <- paste("./data/Data files/", filename, ".dat",sep = "")
  csvfile <- paste("./data/", filename, ".csv",sep = "")
  print( datfile )
  print( csvfile )
  
  data <- read.delim(datfile, header = TRUE)
  View(data)
  write.csv(data, csvfile, row.names = FALSE)
}

substr("Album Sales 1.dat")

data <- read.delim("./data/Data files/Album Sales 1.dat", header = TRUE)
View(data)
write.csv(data, "./data/Album Sales 1.csv", row.names = FALSE)


