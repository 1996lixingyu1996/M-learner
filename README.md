# M-learner:A Flexible And Powerful Framework To Study Heterogeneous Treatment Effect In Mediation Model

## Requirements
To install requirements
```setup
install.packages("randomForestSRC")
install.packages("xgboost")
install.packages("rpart")
install.packages("tidyverse")
install.packages("mediation")
install.packages("latex2exp")
install.packages("rio")
install.packages("tsne")
instal.packages("akima")
```
## Simulations
The simulation files are divided into two parts: scenarios without a mediator and scenarios with a mediator, containing 4 and 7 cases respectively, each implemented with two types of learners—Random Forest and XGBoost. In addition, the files include code for the baseline k-means method and for calculating the mediation proportion.


## Real Data Analysis

We analyzed the heterogeneous indirect treatment effect using the Jobs II data, which can be downloaded from R package mediation. "We provide the processed data file jobs_v2.csv in the Realdataanalysis folder. For details, see jobs.Rmd.

And We will provide python version in the future.
