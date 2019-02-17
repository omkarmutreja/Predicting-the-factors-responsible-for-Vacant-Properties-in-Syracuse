# Predicting-the-factors-responsible-for-Vacant-Properties-in-Syracuse
This repository explains about several reasons behind vacant properties in Syracuse. It involves data from 3 various sources and explains the procedure of merging them and then analyzing on the final version.

## Data Collection
Data: Collected data mainly from three sources:

Syracuse Crime Data (2017) - [Type of Data: Block Level Data]
Syracuse Vacant Property Data (2017) - [Type of Data: Parcel Level Data]
US Census Data (2010) - [Type of Data: Block Group Level Data] Since the 3 datasets are at different levels, they need to be merged to perform analysis and build prediction models.

## Data Pre-processing
Merging the Data: To perform analysis, first merged the crime data with the vacancy property data. Then, found latitude and longitude for the addresses of the merged dataset. To obtain the final dataset, used KNN algorithm to assign latitude, longitude points of the US Census data to the nearest latitude, longitude points of the merged dataset.

Performed final analysis on this merged dataset by selecting the important and relevant columns. (Refer to FinalAnalysis.R file)

## Models
1) Linear/Logistic Regression
2) Naive Bayes
3) KNN
4) SVM
5) Random Forest
