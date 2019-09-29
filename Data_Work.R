load("pmesi.RData")

sum(is.na(pmesi.final))*100/(dim(pmesi.final)[1]*dim(pmesi.final)[2])
sum(is.na(pmesi.final$`Highest Level of Conflict Intensity`))

table(complete.cases(pmesi.final))

count.missing <- rep(NA, dim(pmesi.final)[2])

for (i in 1:dim(pmesi.final)[2]){
  count.missing[i] <- sum(is.na(pmesi.final[,i]))
}
plot(count.missing)
table(count.missing > 170)

# remove variables with more than 170 missing entries
data = pmesi.final[,count.missing<170]
table(complete.cases(data))

# NAs in HLCI assume are zero
sum(is.na(data$`Highest Level of Conflict Intensity`))
data$`Highest Level of Conflict Intensity`[is.na(data$`Highest Level of Conflict Intensity`)] <- 0


count.missing2 <- rep(NA, dim(data)[2])

for (i in 1:dim(data)[2]){
  count.missing2[i] <- sum(is.na(data[,i]))
}
plot(count.missing2)

# remove incomplete cases

complete <- complete.cases(data)
data <- data[complete,]

#write.csv(data, file="data_complete.csv")

# Naive Bayes does not work because it needs categorical information

# PCA
#install.packages("factoextra")
library(factoextra)

PCAdata <- data
prin_comp <- prcomp(data[-c(2:3)], scale.=T)
biplot(prin_comp, scale=0)

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_varex <- pr_var/sum(pr_var)

plot(pr_varex, xlab = "Principal Component", ylab = "Proportion of VAriance Explained", type = "b")
plot(cumsum(pr_varex), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", type="b")

#fviz_eig(prin_comp)
#fviz_pca_ind(prin_comp, col.ind = "cos2", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"), repel=TRUE)

factanal(PCAdata,2,rotation='varimax')


#k means clustering
for (i in 4:880){
data.norm[,(i-3)] <- (data[,i]-mean(data[,i]))/sd(data[,i])
}
dist_data <- dist(data[-c(1:3)])
hc_data <- hclust(dist_data, method="complete")
clusters_k4 <- cutree(hc_data, k=2)

library(dendextend)
dend_data <- as.dendrogram(hc_data)
dend_colored <- color_branches(dend_data, h=6e16)
plot(dend_colored)

clusters <- kmeans(data[-c(1:3),],3)
