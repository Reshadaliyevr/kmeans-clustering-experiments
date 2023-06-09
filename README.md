# K-Means Clustering Optimization for Cardiotocography Dataset

## Task Description

The goal of this task is to determine the best grouping for the Cardiotocography dataset using the k-means clustering algorithm. The task requirements include:

- Analyzing the dataset, excluding the "Class" and "NSP" fields.
- Finding the best grouping with a maximum of 15 clusters, considering the reference grouping with 10 groups based on the "Class" attribute.
- Performing a minimum of 10 tests to achieve better results than the default k-means algorithm with 10 groups and without data preprocessing.

## Dataset Description

The Cardiotocography dataset consists of 2126 fetal cardiotocograms (CTGs) with various measurements of fetal heart rate (FHR) and uterine contraction (UC) features. It is a multivariate dataset with 23 real-valued attributes.

## Script Explanation

The provided script is written in R and performs the following steps:

1. **Data Loading and Preparation**: Downloads and reads the Cardiotocography dataset, excluding the unnecessary fields.

2. **K-Means Clustering**: Applies the k-means algorithm with default parameters and calculates the accuracy.

3. **Data Scaling**: Performs data scaling using the `scale()` function and applies k-means clustering to the scaled data.

4. **Finding Optimal Number of Clusters**: Tests different numbers of clusters and plots the within-cluster sum of squares (WSS) to determine the optimal number.

5. **Experimentation**: Conducts a series of tests by varying the number of clusters and reports the accuracy results.

6. **Conclusion**: Summarizes the findings and suggests considering alternative clustering algorithms for further improvement.

## Getting Started

To run the script and perform k-means clustering optimization on the Cardiotocography dataset, follow these steps:

1. Download the dataset file from the provided URL.
2. Ensure that R and the required libraries are installed.
3. Run the script in an R environment or script editor.
4. Analyze the results and refer to the generated outputs for clustering accuracy and optimization attempts.

## Contributions and Future Enhancements

Contributions and enhancements to this project could include:

- Refactoring the code for better readability and maintainability.
- Adding alternative clustering algorithms for comparison.
- Implementing additional evaluation metrics.
- Creating visualizations to better understand the dataset and clustering results.
- Providing alternative optimization techniques or preprocessing methods.

Collaboration and further development of this project will contribute to the understanding and application of clustering algorithms in data analysis.

Happy clustering!

