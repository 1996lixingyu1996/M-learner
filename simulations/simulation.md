# M-learner:A Flexible And Powerful Framework To Study Heterogeneous Treatment Effect In Mediation Model

## Simulation Part

### No mediators

In the no mediators setting, we consider four distinct scenarios: Simple (1), Complex (2), Global (3), and Null (4). For each scenario, we run the corresponding script Experiment[1-4]_no_mediator_rf/xgb.R, where the numeral (1–4) denotes the specific scenario. Each script generates 100 selected profiles across 100 replications. To visualize the mediation results, we utilize the visual_no_mediator_rf/xgb.R script.

### With a mediator
In the  mediator setting, we consider seven distinct scenarios: Simple-All (1), Simple-Part (2), Complex-All (3), Complex-All (4), Simple-Null1 (5),Simple-Null2 (6),  Simple-Global (7). For each scenario, we run the corresponding script Experiment[1-7]_mediator_rf/xgb.R, where the numeral (1–7) denotes the specific scenario. Each script generates 100 selected profiles across 100 replications. To visualize the mediation results, we utilize the visual_mediator_rf/xgb.R script. In addition, mediation proportion can be calculated with calculation_mediation.R script.

### K-means
For the K-means method, the kmeans.R and kmeans_mediation.R script are provided, all scenarios can use the same scripts.

### Calibration
For calibration, the calibration_example.R script is provided, all scenarios use the same script, the difference is to change the data path.
