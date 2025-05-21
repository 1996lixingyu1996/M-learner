# M-learner:A Flexible And Powerful Framework To Study Heterogeneous Treatment Effect In Mediation Model

## Simulation Part

### No mediators seeting

In the no mediators setting, we consider four distinct scenarios: Simple (1), Complex (2), Global (3), and Null (4). For each scenario, we run the corresponding script Experiment[1-4]_no_mediator_rf/xgb.R, where the numeral (1–4) denotes the specific scenario. Each script generates 100 selected profiles across 100 replications. To visualize the mediation results, we utilize the visual_no_mediator_rf/xgb.R script.

### With a mediator
In the  mediator setting, we consider seven distinct scenarios: Simple-All (1), Simple-Part (2), Complex-All (3), Complex-All (4), Simple-Null1 (5),Simple-Null2 (6),  Simple-Global (7). For each scenario, we run the corresponding script Experiment[1-7]_mediator_rf/xgb.R, where the numeral (1–7) denotes the specific scenario. Each script generates 100 selected profiles across 100 replications. To visualize the mediation results, we utilize the visual_mediator_rf/xgb.R script. In addition, mediation proportion can be calculated with calculation_mediation.R script.




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
