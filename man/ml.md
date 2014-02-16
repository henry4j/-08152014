#### What Is Statistical Learning?

\- | data set consists of ...
--- | ---
predictors/features | input variables X; independent variables
response | output variable Y; dependent variable

* `ad` data set of `sales` of a product in 200 markets with ad budgets for 3 media: TV, radio, and newspaper.
* `Y = f(X) + e` -- 2.1, where `f` is some fixed, but unknown fuction; `e` is a random error term w/ mean zero 
* `Y' = f'(X)` -- 2.2
* `income` data set of 30 individuals suggests one might predict `income` using `years of education` and `seniority`.
* In essence, statistical learning refers to a set of approaches for estimating f and tools for evaluating estimates.


\- | Predicted - | Predicted + | %
--- | --- | --- | ---
\- Cases | TN: 9,760 | FP: 140 | -
\+ Cases | FN: 40 | TP: 60 | recall: 60%
% | - | precision: 30% | accuracy: 98.2%

##### Why Estimate f?

* Prediction: Y<sup>^</sup> = f<sup>^</sup>(X) where they represent our estimate of f and resulting prediction for Y.
  * to minimize `reducible error`, while `irreducible error` proposes an upper bound on the prediction accuracy for Y.
* Inference: how each predictor is associated with the response? (in linear, non-linear, more or less interpretable models).
