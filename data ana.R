setwd("..\\Desktop")
data1 <- read.csv(file = "data.csv")

hour <- substr(data1$提交答卷时间,10,11)
minute <- substr(data1$提交答卷时间,13,14)
second <- substr(data1$提交答卷时间,16,17)

data1 <- cbind(hour,data1)
data1 <- cbind(minute,data1)
data1 <- cbind(second,data1)
data1 <- data1[,-5]

location <- strsplit(data1[,8], "(", fixed = TRUE)

for(i in 1:68){
  location[[i]] <- location[[i]][2]
}

location <- sub(x = location[],pattern = ")",replacement = "",fixed = T )

data1 <- data1[,-8]

data1 <- cbind(location,data1)

char2numeric <- function(input,ncol){
  if(is.character(input[,ncol])){
    input[,ncol] <- as.numeric(input[,ncol])
  }
  return(input)
}

data1 <- char2numeric(data1,4)

write.csv(data1,file = "data1.csv",col.names = FALSE)

rm(list = ls())
gc()

setwd("..\\Desktop")
data1 <- read.csv(file = "data1.csv")
data1 <- data1[,-1]

loc1 <- data.frame(大连 = c(rep(0,96)),盘锦 = c(rep(0,96)),
                     沈阳 = c(rep(0,96)),葫芦岛 = c(rep(0,96)),
                     哈尔滨 = c(rep(0,96)),商丘 = c(rep(0,96)),
                     未知 = c(rep(0,96)))

loc1[which(data1[,1] == "辽宁-沈阳"),3] <- 1 
loc1[which(data1[,1] == "辽宁-大连"),1] <- 1
loc1[which(data1[,1] == "辽宁-未知"),7] <- 1
loc1[which(data1[,1] == "辽宁-盘锦"),2] <- 1
loc1[which(data1[,1] == "辽宁-葫芦岛"),4] <- 1
loc1[which(data1[,1] == "黑龙江-哈尔滨"),5] <- 1
loc1[which(data1[,1] == "河南-商丘"),6] <- 1

data1 <- cbind(loc1,data1)
data1 <- data1[,-8]

data1[,12] <- sub(x = data1[,12],pattern = "秒",fixed = T,replacement = "")

source1 <- data.frame(微信 = c(rep(0,96)),链接 = c(rep(0,96)),
                     手机提交 = c(rep(0,96)),纸质问卷 = c(rep(0,96)))

source1[which(data1[,13] == "微信"),1] <- 1
source1[which(data1[,13] == "链接"),2] <- 1
source1[which(data1[,13] == "手机提交"),3] <- 1
source1[which(data1[,13] == "纸质问卷"),4] <- 1

data1 <- cbind(source1,data1)
data1 <- data1[,c(-15,-17,-18)]


target <- data1[,c(27,28,29)]
data1 <- data1[,c(-27,-28,-29)]
data1 <- cbind(target,data1)

write.csv(data1,file = "dataF.csv",row.names = FALSE)

rm(list=ls())
gc()

setwd("..\\Desktop")
train <- read.csv(file = "dataF.csv")

# Principal Component Analysis
# ++++++++++++++++++++++++++

library(DMwR2)
train <- knnImputation(train,scale = F,meth = "weighAvg")


library(FactoMineR)
library(factoextra)
res.pca <- prcomp(train,  scale = F)#进行PCA分析并得到iris的PCA结果

# Extract eigenvalues/variances
get_eig(res.pca)#提取特征值

# Default plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 95))#绘主成分柱状图

# Scree plot - Eigenvalues
fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE
         ,ggtheme = theme_classic())#将百分比转换为得分

# Use only bar  or line plot: geom = "bar" or geom = "line"
fviz_eig(res.pca, geom="line")#绘制碎石图

fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))

cor1 <- cor(train)


library(pheatmap)
pheatmap(cor1 ,color = colorRampPalette(c("navy", "white", "firebrick3"))(50),fontsize = 6)

form1 <- lm(train[,1]~train[,2]+train[,3])
summary(form1)
