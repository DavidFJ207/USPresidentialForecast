# Forecasting the 2024 US Presidential Election by Analyzing Demographic Patterns and Predicting Swing States

## Abstract

This paper forecasts the 2024 U.S. presidential election by analyzing demographic, historical voting, and recent polling data. Using a combination of regression analysis, historical trend evaluation, and a Bayesian framework, our model assesses how demographic factors—such as race, education, and gender—shape voter preferences and party support. Our findings reveal a majority of key states and the national polling average leaning toward Republicans, suggesting Donald Trump is positioned to win the 2024 Election. These insights enhance our understanding of voter preferences across states and emphasize the significance of targeted campaign strategies.

## File Structure

The repo is structured as:

-   `data/00-simulated_data` contains simulated data of the 'Presidential General Election Polls', aimed for predicting results of the 2024 USA Presidential Election.
-   `data/01-raw_data` contains the raw data of the Emerson pollster of 'Presidential General Election Polls'.
-   `data/02-analysis_data` contains the cleaned dataset that was constructed, and the data that is prepared for being analyzed, based on the raw data of the Emerson pollster of 'Presidential General Election Polls'.
-   `models` contains fitted models, specifically the two Bayesian models aimed to predict the 2024 USA Presidential Elections. 
-   `other` contains details about LLM(Large-Language Model) chat interactions, as well as the sketches of the data presentation.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the rendered PDF of the paper. 
-   `scripts` contains the R scripts used to simulate the data, test the simulated data, download the data, clean the data, test the cleaned data, and finally model the data.
-   `README.md` is the current file, which is a file that describes the key information of the project, including the abstract, a description of the files included, and statement on LLM usage.


## Statement on LLM usage

Parts of the R-code and analysis were completed with the aid of the LLM(Large-Language Model) ChatGPT.