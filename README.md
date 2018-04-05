# Project description

The data file contains details of various nations and their flags. In this file the fields are separated 
by spaces. With this data
1. Try to predict the religion of a country from the characteristics of the flag with at least two method. You must comare the methods and select the one you think as better

2. Use some clustering method to find clusters of flags with respect their characteristics (only)
Explain the clusters found

For both you need to explain the methods you have used, select the variables you think as useful, check how good is your prediction/clustering. For your report keep in mind that your report will be consider by someone with small knowledge of the methods so you need to explain them in a rigorous but simple way.

Attribute Information:
1. name Name of the country concerned
2. landmass 1=N.America, 2=S.America, 3=Europe, 4=Africa, 4=Asia, 6=Oceania
3. zone Geographic quadrant, based on Greenwich and the Equator
1=NE, 2=SE, 3=SW, 4=NW
4. area in thousands of square km
5. population in round millions
6. language 1=English, 2=Spanish, 3=French, 4=German, 5=Slavic, 6=Other 
Indo-European, 7=Chinese, 8=Arabic, 
9=Japanese/Turkish/Finnish/Magyar, 10=Others
7. religion 0=Catholic, 1=Other Christian, 2=Muslim, 3=Buddhist, 4=Hindu,
5=Ethnic, 6=Marxist, 7=Others
8. bars Number of vertical bars in the flag
9. stripes Number of horizontal stripes in the flag
10. colours Number of different colours in the flag
11. red 0 if red absent, 1 if red present in the flag
12. green same for green
13. blue same for blue
14. gold same for gold (also yellow)
15. white same for white
16. black same for black
17. orange same for orange (also brown)
18. mainhue predominant colour in the flag (tie-breaks decided by taking
the topmost hue, if that fails then the most central hue,
and if that fails the leftmost hue)
19. circles Number of circles in the flag
20. crosses Number of (upright) crosses
21. saltires Number of diagonal crosses
22. quarters Number of quartered sections
23. sunstars Number of sun or star symbols
24. crescent 1 if a crescent moon symbol present, else 0
25. triangle 1 if any triangles present, 0 otherwise
26. icon 1 if an inanimate image present (e.g., a boat), otherwise 0
27. animate 1 if an animate image (e.g., an eagle, a tree, a human hand)
present, 0 otherwise
28. text 1 if any letters or writing on the flag (e.g., a motto or
slogan), 0 otherwise
29. topleft colour in the top-left corner (moving right to decide 
tie-breaks)
30. botright Colour in the bottom-left corner (moving left to decide 
tie-breaks)

# Flag-dataset-classification-R

Introduction
The data set contains details of various nations and their flags. In the first part of this project, we will try
predicting the religion of a country from its flag’s characteristics. In order to efficiently train the models
we will need to transform the data set into proper form and we will also need to reduce the number of
attributes which will be used as input in the models. It is crucial that we accurately specify, which
characteristics of a flag are correlated to a country’s religion and therefore we might be able to predict it.
In the second part of the project clustering methods will be used to find clusters of flags with respect to
their characteristics. We will then discover the common characteristics of the countries within each
cluster. We hope that there will be a connection between the clusters and the religions of the within
countries.

Dataset
The dataset consists of 30 variables, 10 of them are numeric and the rest of them are either Boolean or
nominal. Figure 1 describes the names and the values of each variable.

![alt text](https://github.com/evagian/Flag-dataset-classification-R/blob/master/images/flag%20dataset.png)

Flag characteristics correlated to religion

![alt text](https://github.com/evagian/Flag-dataset-classification-R/blob/master/images/correlation-religion.png)

Evaluate classification model accuracy 

![alt text](https://github.com/evagian/Flag-dataset-classification-R/blob/master/images/classification-results2.png)

