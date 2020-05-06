




setwd('~/work/statistics-R/')


heights <- read.csv("./data/heights.csv", header = T)

cor(heights$father, heights$son)

