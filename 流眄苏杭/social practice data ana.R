setwd("..\\Desktop\\data")
data1 <- openxlsx::read.xlsx("1.xlsx")

timeDataPro <- function(data1){
  library(dplyr)
  time <- strsplit(data1[,2]," ")
  date <- as.data.frame(c())
  for(i in 1:length(time)){
    date <- rbind(date,time[[i]][1])
  }
  index <- grep(pattern = "27",x = date[,1]) %>% 
    append(.,grep("28",date[,1]))
  
  IsWeekend <- as.data.frame(rep(0,22))
  IsWeekend[index,] <- 1
  
  temp <- as.data.frame(c())
  for(i in 1:length(time)){
    temp <- rbind(temp,time[[i]][2])
  }
  time <- temp
  rm(temp)
  temp <- strsplit(time[,1],":")
  time <- temp
  rm(temp)
  
  hour <- as.data.frame(c())
  minute <- as.data.frame(c())
  second <- as.data.frame(c())
  for(i in 1:length(time)){
    hour <- rbind(hour,time[[i]][1])
    minute <- rbind(minute,time[[i]][2])
    second <- rbind(second,time[[i]][3])
  }
  timeData <- cbind(date,IsWeekend) %>% cbind(.,hour) %>% 
    cbind(.,minute) %>% cbind(.,second)
  colnames(timeData) <- c("date","IsWeekend","hour","minute","second")
  return(timeData)
}
timeData <- timeDataPro(data1)


location <- strsplit(data1[,6], "(", fixed = TRUE)

for(i in 1:length(location)){
  location[[i]] <- location[[i]][2]
}

location <- sub(x = location[],pattern = ")",replacement = "",fixed = T )
location <- model.matrix( ~ factor(location) - 1 )
colnames(location) <- gsub("factor(location)","",colnames(location),fixed = T)

data1[,3] <- gsub("秒","",data1[,3],fixed = T)
data1 <- data1[,-c(1,2,6)]
data1 <- cbind(timeData,data1) %>% cbind(location,.)

write.csv(data1,file = "data1.csv",row.names = FALSE, fileEncoding = "gbk")

rm(list = ls())
gc()

rm(list=ls())
gc()

train <- read.csv(file = "data1.csv",fileEncoding = "gbk")
train[grep("N/A",train[,26]),26] <- NA
# Principal Component Analysis
# ++++++++++++++++++++++++++
library(dplyr)
library(DMwR2)
train <- knnImputation(train,scale = F,meth = "weighAvg")
train <- train[,-26]
train <- train[,-52]
train$日期 <- as.Date(train$日期) %>% as.numeric()
train$来源 <- as.factor(train$来源) %>% as.numeric()
write.csv(train,file = "train1.csv",row.names = F,fileEncoding = "gbk")
library(FactoMineR)
library(factoextra)
res.pca <- prcomp(train,  scale = F)#????PCA???????õ?iris??PCA????

# Extract eigenvalues/variances
get_eig(res.pca)#??ȡ????ֵ

write.csv(res.pca$rotation,file = "pca.csv",fileEncoding = "gbk")
png(file="scatter1.png",width =2560 ,height=1600, units = "px", res=200)
# Default plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 95))#?????ɷ???״ͼ
dev.off()

# Scree plot - Eigenvalues
png(file="scatter2.png",width =2560 ,height=1600, units = "px", res=200)
fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE
         ,ggtheme = theme_classic())#???ٷֱ?ת??Ϊ?÷?
dev.off()
png(file="scatter3.png",width =2560 ,height=1600, units = "px", res=200)
# Use only bar  or line plot: geom = "bar" or geom = "line"
fviz_eig(res.pca, geom="line")#??????ʯͼ
dev.off()

png(file="PCA1.png",width =2560 ,height=1600, units = "px", res=200)
fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
dev.off()

cor1 <- cor(train)


library(pheatmap)
png(file="heatmap1.png",width =2560 ,height=1600, units = "px", res=200)
pheatmap(cor1 ,color = colorRampPalette(c("navy", "white", "firebrick3"))(50),fontsize = 6)
dev.off()
# diag(cor1) = NA
# index <- as.data.frame(c())
# for(i in 1:ncol(cor1)){
#   index <- append(index,which(cor1[,i] > 0.9))
# }
# which(cor1[,51] > 0.9)
form1 <- lm(train$Q6.您对.高质量发展青年互联网文化.是其成为文化强国有机组成部分.的认同程度.~.,
            data = train[,1:20])
summary(form1)
