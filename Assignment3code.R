

#Varibles

> setwd("/Users/willmcgrath/Desktop/R/Assignment 3")
> epilab3 <- read.csv("~/Desktop/R/Assignment 3/epilab3.csv")
>   View(epilab3)

> subset_WE <- subset(epilab3, region == "Global West")
> subset_EA <- subset(epilab3, region == "Asia-Pacific")

> hist(subset_WE$APO)
> lines(density(subset_WE$APO,na.rm=TRUE,bw=1))
> lines(density(subset_WE$APO,na.rm=TRUE,bw="SJ"))

> hist(subset_EA$APO)
> lines(density(subset_EA$APO,na.rm=TRUE,bw=1))
> lines(density(subset_EA$APO,na.rm=TRUE,bw="SJ"))

> qqnorm(subset_EA$APO, main = "East Asia (APO) vs. Normal Distribution")
> qqline(subset_EA$APO) 

> qqnorm(subset_WE$APO, main = "Global West (APO) vs. Normal Distribution")
> qqline(subset_WE$APO) 

#Linear Model  

> lin.mod.epinew <- lm(EPI ~ BDH + PAE + PHL + WWT + AIR, data = epilab3)
> summary(lin.mod.epinew)
> ggplot(epilab3, aes(x = AIR, y = EPI)) +
  +     geom_point() +          
  +     stat_smooth(method = "lm")          

Subset Model

>  lin.mod.sub <- lm(EPI ~ BDH + PAE + PHL + WWT + AIR, data = subset_EA)
>  
> summary(lin.mod.sub)
> ggplot(subset_EA, aes(x = AIR, y = EPI))+geom_point()+stat_smooth(method = "lm") 

The main model captures a wider range of variability and the subset model provides region insights and reduced varibilty that cant be generalized to the main model. The main model has more data points making it a better fit when explaining the relationship between AIR and EPI.

#Classification


> v<-c("BDH","PAE","PHL","WWT","AIR","region")
> epilabdata_clean <- na.omit(epilab3[v])
> v<-c("BDH","PAE","PHL","WWT","AIR")

> subsetgroup1<-subset(epilabdata_clean, region %in% c("Asia-Pacific","Global West","Latin America & Caribbean"))
> n<-nrow(subsetgroup1)
> train_indexes <- sample(n, n * 0.7)
> epilab_train <- subsetgroup1[train_indexes, ]
> epilab_test <- subsetgroup1[-train_indexes, ]
> sqrt(52)
[1] 7.21110

> k <- 7
> KNNpred <- knn(train = epilab_train[v], 
                 +                test = epilab_test[v], 
                 +                cl = epilab_train$region, 
                 +                k = k)


contingency.table <- table(Actual=KNNpred, Predicted = epilab_test$region, dnn=list('predicted','actual'))
> 
  > print(contingency.table)
actual
predicted                   Asia-Pacific Global West Latin America & Caribbean
Asia-Pacific                         4           0                         1
Global West                          2           7                         1
Latin America & Caribbean            2           0                         6
> 
  > contingency.matrix = as.matrix(contingency.table)
> 
  > sum(diag(contingency.matrix))/length(epilab_test$region)
[1] 0.7391304

#Subset2

subsetgroup1<-subset(epilabdata_clean, region %in% c("Eastern Europe","Former Soviet States","Sub-Saharan Africa"))
> 
  > View(subsetgroup1)
> n<-nrow(subsetgroup1)
> train_indexes <- sample(n, n * 0.7)
> epilab_train <- subsetgroup1[train_indexes, ]
> epilab_test <- subsetgroup1[-train_indexes, ]
> sqrt(52)
[1] 7.211103
> k <- 7
> KNNpred <- knn(train = epilab_train[v], 
                 +                test = epilab_test[v],   
                 +                cl = epilab_train$region,
                 +                k = k)

> contingency.table <- table(Actual=KNNpred, Predicted = epilab_test$region, dnn=list('predicted','actual'))
> print(contingency.table)
actual
predicted              Eastern Europe Former Soviet States Sub-Saharan Africa
Eastern Europe                    4                    0                  1
Former Soviet States              1                    1                  0
Sub-Saharan Africa                1                    2                 13
> contingency.matrix = as.matrix(contingency.table)
> sum(diag(contingency.matrix))/length(epilab_test$region)
[1] 0.7826087

I think both models are very similar and hard to tell which is better they both shine when classifying different groups, Global West and Sub Saharan Africa which all have similar charteristics. The second model does however have less misses and is more accrute but mostly due to its almost complete accruacy with Sub Saharan Africa.

#Kmeans

> subset_group1 <- subset(epilabdata_clean, region %in% c("Southern Asia", "Eastern Europe", "Latin America & Caribbean"))
> 
>subset_group2 <- subset(epilabdata_clean, region %in% c("Sub-Saharan Africa", "Greater Middle East", "Asia-Pacific")) 
> ggplot(subset_group1, aes(x = WWT, y = AIR, colour = region)) +
  +     geom_point()
> set.seed(123)
> 
  > kmeans_group1 <- kmeans(subset_group1[v], centers = 3, nstart = 25)
> kmeans_group2 <- kmeans(subset_group2[v], centers = 3, nstart = 25)

> group1_wcss <- kmeans_group1$tot.withinss
> group2_wcss <- kmeans_group2$tot.withinss
> group1_wcss
[1] 86366.73
> group2_wcss
[1] 110463.8

Group one is more closely clustered compared to group 2

#Group 1
> wcss <- c()
> ks<-c(1,2,3,4)
> for (k in ks) {
  +        kmeans_result <- kmeans(subset_group1[, c("BDH", "PAE", "PHL", "WWT", "AIR")], centers = k, nstart = 25)
  +      wcss <- c(wcss, kmeans_result$tot.withinss)
  +      }
> plot(ks, wcss, type = "b", xlab = "Number of Clusters (k)", ylab = "WCSS",
       +         main = "WCSS for Different k Values", pch = 19)

#Group 2

> wcss <- c()
> ks<-c(1,2,3,4)
> for (k in ks) {
  +        kmeans_result <- kmeans(subset_group2[, c("BDH", "PAE", "PHL", "WWT", "AIR")], centers = k, nstart = 25)
  +      wcss <- c(wcss, kmeans_result$tot.withinss)
  +      }
> plot(ks, wcss, type = "b", xlab = "Number of Clusters (k)", ylab = "WCSS",
       +         main = "WCSS for Different k Values", pch = 19)

Group one has better performance lower Wcss for k 1:4 however group 1s elbow point seems to be at k=3 but k=2 also seems to start to level out and group 2 we can see definite elbow at k=3.
