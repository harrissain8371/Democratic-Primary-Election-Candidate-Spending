# Democratic-Primary-Election-Candidate-Spending

Objective 

To present the analysis of publicly available data set containing information about the money spend by each of the 24 candidates in the primary election campaign . The cleaned data set contains 159625 records spanning from August 2015 to February 2016.

Outcome

The data is analyzed using R programming language, and found to confirm with Central Limit Theorem. Also, the analysis shows comparison of spending pattern of Clinton and Sanders campaigns.

Dataset Background

Presidential Candidate Spending data is available at Federal Election Commission. 
There are 159925 records in the original dataset. However, the data after cleaning shows 159625 records for 24 candidates. 
Some of the graphs derived out of this dataset is available at the Tableau website.
This fairly good sized dataset is selected due to the large number of records that is required to show conformity with the Central Limit Theorem.

Data Import and Cleaning in R

Presidential Candidate Spending dataset is a CSV file. Used readcsv function to import the dataset. read.csv(file='2016_presidential_candidate_expenditures.csv', header=TRUE)
The headers of the data frame were then changed to add meaningful column headers, using colnames
The original dataset did not have information about candidate’s political affiliation. This was corrected first using subset to split the data frame and adding political affiliation, and then using rbind to combine the split data frames back to the original size.



