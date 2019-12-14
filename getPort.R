library(readr)
portNum <- sample(5000:10000, 1)
write_file(as.character(portNum), "port.txt")