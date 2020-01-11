# US--Airlines-Dataset-Analysis
Background of Project:
The main aim of this project was to find the pattern and trends in US Airlines Services, for which the required datasets were provided. The analysis answers various points of interests like busiest route, most delayed airlines and relationship between distance between airports and flying time etc. Various libraries were used to perform the analysis, suitable visualizations are added to analysis for a visual support.

Dataset OR Inputs:
For this analysis, we have given three interrelated datasets of US Airlines which contains information about the Air Routes, Airlines which provides the flying services and Airports information. Apart from these sets there was one pdf which played a vital role in understanding few column values like Cancellation Reason. Cleaning of this data was necessary as there were some missing values for columns like longitude, latitude, Arrival delays etc. I feel the data was highly clean with few drawbacks like Flight date was split into three columns which is not good for analysis, but we are not performing Time series analysis so leaving them unchanged.

Challenges and Learnings:
Large number of rows, many a times R studio crashed on my system mainly because of the huge dataset. I then make sure to choose only relevant rows for the analysis that is removing outliers, cancelled, diverted flights etc.
US Map visualization, it was the biggest challenge for me and took me almost 5 hours to get what I wanted out of the map, i.e. Number flights per county plotted on a colour scale. I had to use SQL queries to get the desired data, same could be done using DPLYR but since I had a strong knowledge of SQL I decided to go for SQL queries.
Regression model to predict arrival delay, it was one of the most exciting analysis which I performed. Model is not great since I am in a learning phase, but it still points out the power of Data Analytics on real world scenarios. 
