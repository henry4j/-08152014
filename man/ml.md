#### What Is Statistical Learning?

* in essence, statistical learning refers to a set of approaches for estimating f and tools for evaluating estimates.
* why Estimate f? predict response Y when not easy to obtain; understand how Y is affected, when X changes (inference).
* depending on prediction, inference, or both of the goals, methods are chosen w/ tradeoffs of accuracy and interpretability.
  * Lasso, Least Squares LR, GAM (generalized additive models), Bagging, Boosting, and SVM (support vector machine).
* how estimate f?
  * **f(X) = β<sub>0</sub> + β<sub>1</sub>X<sub>1</sub> + β<sub>2</sub>X<sub>2</sub> + ... + β<sub>p</sub>X<sub>p</sub> -- (2.3)** # we assume a p-dim. function, a linear function.
  * **Y ≈ β<sub>0</sub> + β<sub>1</sub>X<sub>1</sub> + β<sub>2</sub>X<sub>2</sub> + ... + β<sub>p</sub>X<sub>p</sub>** # we need to estimate params; fit or train a model; commonly, **least squares LR** fit.
  * non-parametric methods: a **thin-plate spline** w/ a level of smoothness produces a remarkably accurate estimate of **f**.
* in unsupervised learning problems, we lack a response variable that can supervise our analysis, e.g. cluster analysis.
* in regression problems, least squares linear regression is used w/ quantitative responses, 
* in classification problems, logistic regression is typically used w/ qualitative responses; to predict class probabilities.
* in assessing model, MSE (mean squared error in training and test phases)

\ | data set consists | `advertising` | `income` of 30 individuals
--- | --- | --- | ---
predictors/features | input variables X; independent variables | TV, radio, & newspaper budgets | years of education and seniority
response | output variable Y; dependent variable | sales of a product in 200 markets | income

* **Y = f(X) + e -- (2.1)**, where **f** is some fixed, but unknown fuction; **e** is a random error term w/ mean zero.
* **Y' = f'(X) -- (2.2)**, where **f'** is our estimate for **f**; as a black box **f'** yields accurate predictions **Y'** for **Y**.
* **E(Y - Y')² = [f(X) - f'(X)] + Var(e) -- (2.3)** # accuracy of **Y'** as a prediction depends reducible & irreducible errors.

#

\- | Predicted - | Predicted + | %
--- | --- | --- | ---
\- Cases | TN: 9,760 | FP: 140 | -
\+ Cases | FN: 40 | TP: 60 | recall: 60%
% | - | precision: 30% | accuracy: 98.2%

##### Why Estimate f?

* Prediction: Y<sup>^</sup> = f<sup>^</sup>(X) where they represent our estimate of f and resulting prediction for Y.
  * to minimize `reducible error`, while `irreducible error` proposes an upper bound on the prediction accuracy for Y.
* Inference: how each predictor is associated with the response? (in linear, non-linear, more or less interpretable models).
