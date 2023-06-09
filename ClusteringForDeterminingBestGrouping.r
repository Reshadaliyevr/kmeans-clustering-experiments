#Ramin Hasanli , Rashad Aliyev

# Load necessary libraries
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(flexclust)
library(ClusterR)

# Download the whole dataset
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_all_corr.csv','cardioto_all_corr.csv')
ctg_all <- read.csv("cardioto_all_corr.csv",row.names = 1)

# Download the dataset without classes and NSP
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_noClass_corr.csv','cardioto_noClass_corr.csv')
ctg_noClass <- read.csv("cardioto_noClass_corr.csv",row.names = 1)

set.seed(7777)

#accuracy calculation function from lab script
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

#kmeans
ctg_noClass.kmeans=kmeans(ctg_noClass,10)

# Function for baseline k-means clustering
runBaselineKMeans <- function(data) {
  # Implement k-means with default parameters
  baselineKMeans <- kmeans(data, centers = 10)  # Adjust centers if needed
  
  # Return the baseline k-means result
  return(baselineKMeans)
}

#Using Kmeans++ algorithm
runImprovedKMeans <- function(data) {
  set.seed(7777)
  clusterResult <- KMeans_rcpp(data, clusters = 10, initializer = "kmeans++")
  return(clusterResult)
}

#getting information about clustering
print(ctg_noClass.kmeans)
print(ctg_noClass.kmeans$iter)
print(ctg_noClass.kmeans$centers)

# Quality of the clustering            
res3 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
res3
accuracyCalc(res3,1)
#Accuracy is equal to 41.38%

# Distance matrix
distMatrix <- dist(ctg_noClass)
# Cluster statistics
clusterStats <- cluster.stats(distMatrix, ctg_noClass.kmeans$cluster)

# Between-cluster sum of squares is the total sum of squares minus the within-cluster sum of squares
totalss <- sum(distMatrix^2)/length(ctg_noClass)
betweenss <- totalss - ctg_noClass.kmeans$tot.withinss

# Print between-cluster sum of squares
cat("Between-cluster sum of squares:", betweenss, "\n")

#Data scaling
ctg_noClass_scaled <- scale(ctg_noClass)
str(ctg_noClass_scaled)
summary(ctg_noClass_scaled)

ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,10)
str(ctgScale.kmeans)

#Quality of clustering after Data Scaling
res4 = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
res4
accuracyCalc(res4,1)
#Accuracy is equal to 47.44%

# Finding optimal number of groups
wss <- vector(mode = "integer" ,length = 15)

#  1 to 15 clusters
for (i in 1:15) {
  kmeans.group <- kmeans(ctg_noClass_scaled, centers = i, nstart=25, iter.max = 25)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
  mainScriptResult_scaled <- kmeans(ctg_noClass_scaled, centers = i, nstart = 25, iter.max = 15) 
}
confusion_matrix_scaled <- table(ctg_all$CLASS, mainScriptResult_scaled$cluster)
accuracy_scaled <- accuracyCalc(confusion_matrix_scaled, 1)

# total within-cluster sum of squares per number of groups
plot(1:15, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")
###EXPERIMENTS

#We have 10 different classes from 1 to 10.
#Our goal is to get best accuracy possible with doing some experiments.

#TEST1
#Changing number of clusters to 3
ctg_noClass.kmeans=kmeans(ctg_noClass,3)
test1 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test1
cat("Test1 Accuracy:",accuracyCalc(test1,1),"\n")
#Accuracy is equal to 31.51%

#TEST1_scaled
#Changing number of clusters to 3
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,3)
test1_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test1_scaled
cat("Test1 (scaled) Accuracy:",accuracyCalc(test1_scaled,1),"\n")
#Accuracy is equal to 34.75%

#TEST2
#Changing number of clusters to 4
ctg_noClass.kmeans=kmeans(ctg_noClass,4)
test2 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test2
cat("Test2 Accuracy:",accuracyCalc(test2,1),"\n")
#Accuracy is equal to 31.70%

#TEST2_scaled
#Changing number of clusters to 4
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,4)
test2_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test2_scaled
cat("Test2 (scaled) Accuracy:",accuracyCalc(test2_scaled,1),"\n")
#Accuracy is equal to 34.28%

#TEST3
#Changing number of clusters to 5
ctg_noClass.kmeans=kmeans(ctg_noClass,5)
test3 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test3
cat("Test3 Accuracy:",accuracyCalc(test3,1),"\n")
#Accuracy is equal to 38.04%

#TEST3_scaled
#Changing number of clusters to 5
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,5)
test3_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test3_scaled
cat("Test3 (scaled) Accuracy:",accuracyCalc(test3_scaled,1),"\n")
#Accuracy is equal to 44.71

#TEST4
#Changing number of clusters to 6
ctg_noClass.kmeans=kmeans(ctg_noClass,6)
test4 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test4
cat("Test4 Accuracy:",accuracyCalc(test4,1),"\n")
#Accuracy is equal to 39.83%

#TEST4_scaled
#Changing number of clusters to 6
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,6)
test4_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test4_scaled
cat("Test4 (scaled) Accuracy:",accuracyCalc(test4_scaled,1),"\n")
#Accuracy is equal to 45.60%

#TEST5
#Changing number of clusters to 7
ctg_noClass.kmeans=kmeans(ctg_noClass,7)
test5 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test5
cat("Test5 Accuracy:",accuracyCalc(test5,1),"\n")
#Accuracy is equal to 40.15%

#TEST5_scaled
#Changing number of clusters to 7
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,7)
test5_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test5_scaled
cat("Test5 (scaled) Accuracy:",accuracyCalc(test5_scaled,1),"\n")
#Accuracy is equal to 44.19%

#TEST6
#Changing number of clusters to 8
ctg_noClass.kmeans=kmeans(ctg_noClass,8)
test6 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test6
cat("Test6 Accuracy:",accuracyCalc(test6,1),"\n")
#Accuracy is equal to 41.80%

#TEST6_scaled
#Changing number of clusters to 8
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,8)
test6_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test6_scaled
cat("Test6 (scaled) Accuracy:",accuracyCalc(test6_scaled,1),"\n")
#Accuracy is equal to 47.76%

#TEST7
#Changing number of clusters to 9
ctg_noClass.kmeans=kmeans(ctg_noClass,9)
test7 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test7
cat("Test7 Accuracy:",accuracyCalc(test7,1),"\n")
#Accuracy is equal to 43.02%

#TEST7_scaled
#Changing number of clusters to 9
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9)
test7_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test7_scaled
cat("Test7 (scaled) Accuracy:",accuracyCalc(test7_scaled,1),"\n")
# Accuracy is equal to 47.86%

#TEST8
#Changing number of clusters to 11
ctg_noClass.kmeans=kmeans(ctg_noClass,11)
test8 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test8
cat("Test8 Accuracy:",accuracyCalc(test8,1),"\n")
#Accuracy is equal to 43.02%

#TEST8_scaled
#Changing number of clusters to 11
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,11)
test8_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test8_scaled
cat("Test8 (scaled) Accuracy:",accuracyCalc(test8_scaled,1),"\n")
#Accuracy is equal to 47.44%

#TEST9
#Changing number of clusters to 12
ctg_noClass.kmeans=kmeans(ctg_noClass,12)
test9 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test9
cat("Test9 Accuracy:",accuracyCalc(test9,1),"\n")
#Accuracy is equal to 43.87%
#We can see that TEST9(Acccuracy=43.87%) is better than others. Now we can add max iter to kmeans

#test9_scaled
#Changing number of clusters to 12
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,12)
test9_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test9_scaled
cat("Test9 (scaled) Accuracy:",accuracyCalc(test9_scaled,1),"\n")
#Accuracy is equal to 51.62%  
#We can see that in scaled data TEST9(Acccuracy=51.62%) is better than others. Now we can add max iter to kmeans

#TEST10
ctg_noClass.kmeans=kmeans(ctg_noClass,9, iter.max = 10)
test10 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test10
cat("Test10 Accuracy:",accuracyCalc(test10,1),"\n")
#Accuracy is equal to 41.83%

#Test10_scaled
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9, iter.max = 10)
test10_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test10_scaled
cat("Test10 (scaled) Accuracy:",accuracyCalc(test10_scaled,1),"\n")

#TEST11
ctg_noClass.kmeans=kmeans(ctg_noClass,9, iter.max = 15)
test11 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test11
cat("Test11 Accuracy:",accuracyCalc(test11,1),"\n")
#Accuracy is equal to 42.83%

#TEST11_scaled
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9, iter.max = 15)
test11_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test11_scaled
cat("Test11 (scaled) Accuracy:",accuracyCalc(test11_scaled,1),"\n")

#TEST12
ctg_noClass.kmeans=kmeans(ctg_noClass,9, iter.max = 20)
test12 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test12
cat( "Test12 Accuracy:",accuracyCalc(test12,1),"\n")

#TEST12_scaled
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9, iter.max = 20)
test12_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test12_scaled
cat( "Test12 (scaled) Accuracy:",accuracyCalc(test12_scaled,1),"\n")

#TEST13
ctg_noClass.kmeans=kmeans(ctg_noClass,9, iter.max = 30)
test13 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test13
cat("Test13 Accuracy:",accuracyCalc(test13,1),"\n")
#Accuracy is equal to 42.50%. That means iter.max=15 is better.

#test13_scaled
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9, iter.max = 30)
test13_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test13_scaled
cat("Test13 (scaled) Accuracy:",accuracyCalc(test13_scaled,1),"\n")

#TEST14
#Adding nstart parameter 
ctg_noClass.kmeans=kmeans(ctg_noClass,9,iter.max = 15, nstart = 5)
test14 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test14
cat("Test14 Accuracy:",accuracyCalc(test14,1),"\n")
#Accuracy is equal to 43.02% and it is higher than other accuracies.

#TEST14_scaled
#Adding nstart parameter
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9,iter.max = 15, nstart = 5)
test14_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test14_scaled
cat("Test14 (scaled) Accuracy:",accuracyCalc(test14_scaled,1),"\n")

#TEST15
#Adding nstart parameter 
ctg_noClass.kmeans=kmeans(ctg_noClass,9,iter.max = 15, nstart = 15)
test15 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test15
cat("Test15 Accuracy:",accuracyCalc(test15,1),"\n")
#Accuracy is equal to 42.60%.

#TEST15_scaled
#Adding nstart parameter
ctg_noClass_scaled.kmeans=kmeans(ctg_noClass_scaled,9,iter.max = 15, nstart = 15)
test15_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test15_scaled
cat("Test15 (scaled) Accuracy:",accuracyCalc(test15_scaled,1),"\n")

#TEST16
#Adding nstart parameter 
ctg_noClass.kmeans = kmeans(ctg_noClass,9, iter.max = 25, nstart = 25)
test16 = table(ctg_all$CLASS,ctg_noClass.kmeans$cluster)
test16
cat("Test16 Accuracy:",accuracyCalc(test16,1),"\n")
#Accuracy is equal to 41.09%. 

#test16_scaled
#Adding nstart parameter
ctg_noClass_scaled.kmeans = kmeans(ctg_noClass_scaled,9, iter.max = 25, nstart = 25)
test16_scaled = table(ctg_all$CLASS,ctg_noClass_scaled.kmeans$cluster)
test16_scaled
cat("Test16 (scaled) Accuracy:",accuracyCalc(test16_scaled,1),"\n")

#Test17
#Using Improved Kmeans with scaled data
cat("Improved Kmeans with scaled data:\n")
ctg_noClass_scaled.ImprovedKMeans = runImprovedKMeans(ctg_noClass_scaled)
test17 = table(ctg_all$CLASS,ctg_noClass_scaled.ImprovedKMeans$cluster)
test17
cat("Test17 Improved Kmeans with scaled data Accuracy:",accuracyCalc(test17,1),"\n")

#Test18
#Using Baseline with scaled data
cat("Baseline with scaled data:\n")
ctg_noClass_scaled.BaselineKMeans = runBaselineKMeans(ctg_noClass_scaled)
test18 = table(ctg_all$CLASS,ctg_noClass_scaled.BaselineKMeans$cluster)
test18
cat("Test18 Baseline with scaled data Accuracy:",accuracyCalc(test18,1),"\n")

#Run Baseline Kmeans with unscaled data
baselineResult <- runBaselineKMeans(ctg_noClass)
#Run Improved Kmeans with unscaled data
improvedScriptResult <- runImprovedKMeans(ctg_noClass)
#Run Main Script with scaled data  
mainScriptResult_scaled <- ctg_noClass_scaled.kmeans
#Run Main Script with unscaled data
mainScriptResult <- ctg_noClass.kmeans

# Calculate accuracy for main script output
accuracyMainScript <- accuracyCalc(table(ctg_all$CLASS, mainScriptResult$cluster), 1)
# Calculate accuracy for the baseline output
accuracyBaseline <- accuracyCalc(table(ctg_all$CLASS, baselineResult$cluster), 1)
# Calculate accuracy for improved script output
accuracyImprovedScript <- accuracyCalc(table(ctg_all$CLASS, improvedScriptResult$cluster), 1)
#Calculate accuracy for main script output with scaled data
accuracyMainScript_scaled <- accuracyCalc(table(ctg_all$CLASS, mainScriptResult_scaled$cluster), 1)

# Compare the accuracy results
if (accuracyMainScript > accuracyBaseline && accuracyMainScript > accuracyImprovedScript) {
  print("Main script achieved a better accuracy than the baseline and improved script")
} else if (accuracyMainScript < accuracyBaseline && accuracyImprovedScript < accuracyBaseline) {
  print("Main script and improved script achieved a worse accuracy than the baseline.")
} else if (accuracyImprovedScript > accuracyMainScript && accuracyImprovedScript > accuracyBaseline) {
  print("Improved script achieved a better accuracy than the baseline and main script.")
}

# Print within-cluster sum of squares
cat("Within-cluster sum of squares:", ctg_noClass.kmeans$tot.withinss, "\n")
# Print important numbers from main script's output
cat("Number of iterations in Main Script :", mainScriptResult$iter, "\n")
# Print important numbers from improved script's output
cat("Number of iterations in Improved Script:", improvedScriptResult$iter.max, "\n")  
# Print important numbers from the baseline result
cat("Number of iterations in Baseline:", baselineResult$iter, "\n")
# Print important numbers from main script's output with scaled data
cat("Number of iterations in Main Script with scaled data:", mainScriptResult_scaled$iter, "\n")

# Compare the accuracy results
cat("\nComparison of Accuracy:\n")
cat("Accuracy of Main Script:", accuracyMainScript, "\n")
cat("Accuracy of Baseline:", accuracyBaseline, "\n")
cat("Accuracy of Improved Kmeans with unscaled data:", accuracyImprovedScript, "\n")
cat("Accuracy of Main script with scaled data:", accuracy_scaled, "\n")
cat("Accuracy of Improved Kmeans with scaled data:", accuracyCalc(table(ctg_all$CLASS, ctg_noClass_scaled.ImprovedKMeans$cluster), 1), "\n")
cat("Accuracy of Baseline with scaled data:", accuracyCalc(table(ctg_all$CLASS, ctg_noClass_scaled.BaselineKMeans$cluster),1),"\n")



#In conclusion, we have done 34 tests to find better clustering accuracy for ctg_noClass and ctg_noClass_scaled data.
#The analysis suggests that data scaling and increasing the number of clusters improved the accuracy of the k-means clustering algorithm on the dataset. 
#We achieved higher accuracy than baseline and scaled data.
#The best accuracy of 51.62% was achieved with 12 clusters and data scaling. 
#However, the overall improvement in accuracy was modest, indicating that alternative clustering algorithms and techniques might be more suitable for this dataset.
