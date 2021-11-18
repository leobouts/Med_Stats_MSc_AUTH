library(zen4R)
#Create a Zenodo manager (always the starting point)
zen <- ZenodoManager$new()
#Interrogate and get a Zenodo record with its DOI or internal ID
my.zen <- zen$getRecordByDOI("10.5281/zenodo.4266758")
my.zen <- zen$getRecordById("4266758")

files <- my.zen$listFiles()
x <- read.csv(files[1,4])
