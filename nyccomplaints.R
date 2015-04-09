setwd("C:/Users/TimBo/Downloads/R docs and scripts/ComplaintClust")
NYC311 = read.csv('2014_NYC.csv', header=T)
#https://data.cityofnewyork.us/Social-Services/2014-NYC/c9is-sbit

library(plyr)
library(dplyr)
NYC311$Complaint.Type = tolower(NYC311$Complaint.Type) #repeated complaints, different case
NYC311$Complaint.Type = gsub('s$', '', NYC311$Complaint.Type) #repeated complaints, some plural
NYC311$Incident.Zip = gsub('-[[:digit:]]{4}$', '', NYC311$Incident.Zip) #some 6 and some 10 digit zips
NYC311$Complaint.Type = gsub('paint - plaster', 'paint/plaster', NYC311$Complaint.Type) #repeated complaints, different punct
NYC311$Complaint.Type = gsub('general construction', 'construction', NYC311$Complaint.Type) #merging differently coded complaints
NYC311$Complaint.Type = gsub('nonconst', 'construction', NYC311$Complaint.Type)
NYC311$Complaint.Type = gsub('street sign - [[:alpha:]]', 'street sign', NYC311$Complaint.Type) #merge all street sign complaints
NYC311$Complaint.Type = gsub('fire alarm - .+','fire alarm', NYC311$Complaint.Type) #merge all fire alarm complaints

idx = grepl('[[:digit:]]{5}', NYC311$Incident.Zip) #remove complaints with no zipcode
NYC311clean = NYC311[idx,]
NYC311byZip = ddply(NYC311clean, .(Incident.Zip, Complaint.Type), count) #counts by zip and complaint 


library(tidyr) #prepare data for pca
raw = spread(NYC311byZip, Complaint.Type, n)
raw[is.na(raw)] = 0
counts = which(colSums(raw[,-1]) < 10)
zipcodes = raw[,1]
raw = raw[,-1]
raw = raw[,-counts]
processed = scale(raw, center=T, scale=T)

library(psych)
pca = principal(processed, nfactor=6, covar=F)
pca$loadings

NYCPCs = pca$scores
set.seed(400)
cluster=kmeans(processed, 4)

library(scatterplot3d)
library(rgl)
scatterplot3d(NYCPCs[,2], NYCPCs[,1], NYCPCs[,3], color=cluster$cluster)
plot3d(NYCPCs[,2], NYCPCs[,1], NYCPCs[,3], col=cluster$cluster)

#zip code shapefiles available here:
#https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
library(maptools)
library(RColorBrewer)
setwd("C:/Users/TimBo/Downloads/R docs and scripts/ComplaintClust/NYC_zipcodes")
NYC = readShapePoly('ZIP_CODE_040114.shp')

zipcolors = data.frame(zip = NYC$ZIPCODE, color = 0)
for(i in 1:nrow(zipcolors)){
  if(zipcolors[i,1] %in% zipcodes){
    zipcolors[i,2] = cluster$cluster[which(zipcodes == zipcolors[i,1])]
  }
}
colors = brewer.pal(4, 'Dark2')
plot(NYC, col=colors[zipcolors$color])
title(paste ("NYC Zip Codes Clustered by Complaints"))

cluster$centers