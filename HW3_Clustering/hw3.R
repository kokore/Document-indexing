teens <- read.csv("snsdata.csv")

head(teens,3)

dim(teens)

str(teens)

summary(teens$age)

teens = na.omit(teens)

dim(teens)

interests <- teens[5:40]

interest_z <-as.data.frame(lapply(interests, scale)) 

str(interest_z)

range<-2:20 
tries <- 100 
avg.totw.ss <- integer(length(range))

for(v in range){
  v.totw.ss <- integer(tries)
  for(i in 1:tries){
    k.temp <- kmeans(interest_z,centers = v)
    v.totw.ss[i] <- k.temp$tot.withinss
  }
  avg.totw.ss[v-1] <- mean(v.totw.ss)
}
plot(range,avg.totw.ss,type="b", main="Total Within SS by Various K", 
     ylab = "Average Total Within Sum of Squares", xlab = "Value of K") 

teen_clusters <- kmeans(interest_z, 11)

teen_clusters$size 

teen_clusters$centers 

teen_clusters$cluster

summary(teen_clusters)

print(teen_clusters)
