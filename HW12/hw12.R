rm(list=ls())
library(jpeg)
library(RCurl)

fileName <- "DM2016.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)

rgb = matrix(NA,1018*1554,5)
index=1

for (i in c(2:1019))
{
  y=1
  row = strsplit(linn[i]," ")
  row = unlist(row)
  
  for(pcol in seq(2,4663,3))
  {
    rgb[index,1] = i-1
    rgb[index,2] = y
    rgb[index,3] =  as.numeric(row[pcol])/255
    rgb[index,4] = as.numeric(row[pcol+1])/255
    rgb[index,5] =  as.numeric(row[pcol+2])/255
    y = y + 1
    index = index + 1
  }
  
}
close(conn)

rgb_data = data.frame(x = as.vector(rgb[,1]), y = as.vector(rgb[,2]), r.value=as.vector(rgb[,3]), g.value=as.vector(rgb[,4]),b.value=as.vector(rgb[,5]))
#head(rgb_data)

kColors <- 25
kMeans <- kmeans(rgb_data[, c(3,4,5)], 
                 centers = kColors)

clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x , data=rgb_data, main="k-mean cluster analysis",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 3colours")


########################################################################################################
fileName <- "DM2016.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)

img = matrix(NA,1018,4662)
index=1

for (i in c(2:1019))
{
  row = strsplit(linn[i]," ")
  row = unlist(row)
  img[i-1,] = as.numeric(row[2:length(row)])
}

close(conn)

pca = prcomp(img[,1:ncol(img)],scale. = FALSE,retx = TRUE,center = FALSE)

plot((cumsum(pca$sdev^2)/sum(pca$sdev^2))*100,type="b",pch=4,col="blue",ylab="cumulative variance captured",xlab="principal components",main="Comulative Variance (%) Captured \n by Principal Components")

library(scatterplot3d)
with(mtcars, {
  scatterplot3d(pca$x[,1],   
                pca$x[,2],  
                pca$x[,3],  
                color = "blue",
                xlab = "pca 1",
                ylab = "pca 2",
                zlab = "pca 3",
                main="3-D Scatterplot of PCA 1, PCA 2, PCA 3")
})


barplot(pca,type="b",col="blue",xlab="Principal Component Number",ylab="Eigenvalues",pch=4)


#####################################################################################################
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(gplots)

data = read.csv("extract_medium.csv",sep = ";")

data$Marriage = factor(data$Marriage,level= c(1,2,3,4,5), labels=c("Married","Widowed","Divorced","Seperated","Never married (includes under 15 years)"))
education_labels = c("Not in universe (Under 2 years)","No schooling completed","Nursery school to 4th grade","5th grade or 6th grade","7th grade or 8th grade","9th grade","10th grade","11th grade","12th grade,no diploma")
education_labels = c(education_labels,c("High school graduate","Some college, but less than 1 year","One or more years of college, no degree","Associate degree","Bachelor's degree"))
education_labels = c(education_labels,c("Master's degree","Professional degree","Doctorate degree"))

data$Education = factor(data$Education,levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),labels = education_labels)
data$Sex= factor(data$Sex,level=c(1,2), labels = c("Male","Female"))

group= group_by(data, Sex, Education)
sum_earnings = summarise(group, 
                        avg_earnings = mean(Earnings))
summ_t = dcast(sum_earnings,  Education ~ Sex , value.var = "avg_earnings")
row.names(summ_t) <- summ_t$Education
summ_t <- summ_t[,2:3]

dt_matrix <- data.matrix(summ_t)

 heatmap(dt_matrix, Rowv=NA, Colv=NA,  col = brewer.pal(9, "Blues"), scale="column", margins=c(5,10),cexRow = .75,cexCol = .90)
 
#####################################################################################################
 
 # by marraige and education
 
 colnames(data)
 
 group= group_by(data, Marriage, Education)
 sum_earnings = summarise(group, 
                          avg_earnings = mean(Earnings))
 summ_t = dcast(sum_earnings,  Education ~ Marriage , value.var = "avg_earnings")
 row.names(summ_t) <- summ_t$Education
 summ_t <- summ_t[,2:6]
 data[1:10,]
 
 dt_matrix <- data.matrix(summ_t)
 dt_matrix[is.na(dt_matrix)] = 0
 
 heatmap(dt_matrix, Rowv=NA, Colv=NA,  col = brewer.pal(9, "Blues"), scale="column", margins=c(5,10),cexRow = .75,cexCol = .90)
 
 # gender and marriage
 
 group= group_by(data, Sex, Marriage)
 sum_earnings = summarise(group, 
                          avg_earnings = mean(Earnings))
 summ_t = dcast(sum_earnings,  Marriage ~ Sex , value.var = "avg_earnings")
 row.names(summ_t) <- summ_t$Marriage
 summ_t <- summ_t[,2:3]
 
 dt_matrix <- data.matrix(summ_t)
 dt_matrix[is.na(dt_matrix)] = 0
 
 heatmap(dt_matrix, Rowv=NA, Colv=NA,  col = brewer.pal(9, "Blues"), scale="column", margins=c(5,10),cexRow = .75,cexCol = .90)
 
 #################################################################################################################################
 library(TSP)
 
 fileName <- "DM2016.txt"
 conn <- file(fileName,open="r")
 linn <-readLines(conn)
 
 img = matrix(NA,1018,4662)
 index=1
 for (i in c(2:1019))
 {
   row = strsplit(linn[i]," ")
   row = unlist(row)
   img[i-1,] = as.numeric(row[2:length(row)])
 }
 close(conn)
 
 d_matrix = dist(img)
 tsp = TSP(d_matrix) 
 x = solve_TSP(tsp,method="nearest_insertion")
 img_order = x
order_img = matrix(NA,1018,4662)
for(i in c(1:1018))
{
  order_img[i,]= img[img_order[i],]
}

img1 = array(NA,c(1018,1554,3))
for (i in c(1:1018))
{
  row = order_img[i,]
  y=1
  for(pcol in seq(1,4662,3))
  {
    img1[i,y,1] = as.numeric(row[pcol])/255
    img1[i,y,2] = as.numeric(row[pcol+1])/255
    img1[i,y,3] = as.numeric(row[pcol+2])/255
    y = y + 1
  }
}

require('jpeg')
res = dim(img1)[1:2] # get the resolution
#if (!add) # initialize an empty plot area if add==FALSE
  plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
rasterImage(img1,1,1,res[1],res[2])


##############################

rgb = matrix(NA,1018*1554,5)
index=1

for (i in c(1:1018))
{
  y=1
  row = order_img[i,]
  
  for(pcol in seq(1,4662,3))
  {
    rgb[index,1] = i 
    rgb[index,2] = y
    rgb[index,3] =  as.numeric(row[pcol])/255
    rgb[index,4] = as.numeric(row[pcol+1])/255
    rgb[index,5] =  as.numeric(row[pcol+2])/255
    index = index + 1
    y = y + 1
  }
}

head(rgb)

rgb_data = data.frame(x = as.vector(rgb[,1]), y = as.vector(rgb[,2]), r.value=as.vector(rgb[,3]), g.value=as.vector(rgb[,4]),b.value=as.vector(rgb[,5]))
#head(rgb_data)

require('jpeg')
res = dim(img1)[1:2] # get the resolution
if (!add) # initialize an empty plot area if add==FALSE
  plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
rasterImage(img1,1,1,res[1],res[2])
