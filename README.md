# [Video Quality Prediction under Time-varying Loads](https://ieeexplore.ieee.org/abstract/document/8591004)

This project compares the baseline approach of this [study](https://ieeexplore.ieee.org/abstract/document/7367349), who proposed a hypothesis that by gathering a large number of kernel variables, it would be possible to learn and forecast the behavior of the client system. Their hypothesis did not require detailed knowledge of the system components and interactions. This proposition is intriguing as it suggests that the network manager could potentially deploy the predictor without substantial supervision or setup, essentially "blindly". This technique was referred to as the Unadjusted (UA) Machine Learning (ML) technique.

Using the Load-Adjusted (LA) ML technique first in this [work](https://ieeexplore.ieee.org/abstract/document/7465715), the project contributes adaptive learning methods characterized by lower computational complexity and improved prediction accuracy. This is achieved through:

1. **Integration of Regularization via Elastic Net ML:** The Elastic Net is employed to automate the selection of regression parameters and functions, streamlining the learning process.

2. **Utilization of Load-Adjusted Learning:** Building upon the concept introduced in this [study](https://ieeexplore.ieee.org/abstract/document/7163768), such as incorporating TCP socket count data to enhance the performance of the prediction algorithm.

3. **Evaluation of Subset Selection Versus Load-Adjusted Learning:** The study assesses whether subset selection, the method utilized in the baseline, or load-adjusted learning techniques improve the condition number of various learning algorithms across the dataset traces provided by the authors of the [baseline study](https://ieeexplore.ieee.org/abstract/document/7367349).

By incorporating these methodologies, the proposed adaptive learning framework aims to provide more accurate predictions with reduced computational overhead compared to existing approaches.

Machine Learning models used in the project are:
- Linear Regression
- Ridge Regression
- Least Absolute Shrinkage and Selection Operator (LASSO)
- Elastic Net
