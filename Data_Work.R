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

write.csv(data, file="data_complete.csv")
