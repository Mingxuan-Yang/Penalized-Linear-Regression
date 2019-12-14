## Glossary

`response`: Variables of interest in an experiment (those that are measured or observed) are called response or dependent variables.

`predictor`: Variables in the experiment that affect the response and can be set or measured by the experimenter are called predictor, explanatory, or independent variables.

`interaction`: An interaction is a term in a statistical model in which the effect of two, or more variables is not simply additive.

`Ridge regression`: Ridge regression is particularly useful to mitigate the problem of multicollinearity in linear regression, which commonly occurs in models with large numbers of parameters. In general, the method provides improved efficiency in parameter estimation problems in exchange for a tolerable amount of bias.

With predictors (X) and response (Y) centered and scaled, this method can be conceptualized by posing a constraint to the least squares problem:

<div align="center">
min<sub>&beta;</sub>(Y-X&beta;)<sup>T</sup>(Y-X&beta;)
</div>  
<div align="center">
&sum;&beta;<sub>j</sub><sup>2</sup>&le;t
</div>

Thus the ridge regression coefficient estimates are the values that minimize

<div align="center">
(Y-X&beta;)<sup>T</sup>(Y-X&beta;)+&lambda;&sum;&beta;<sub>j</sub><sup>2</sup>
</div>

where &lambda;&ge;0 is a tuning parameter, to be determined separately.

`Lasso regression`: Lasso regression is a regression analysis method that performs both variable selection and regularization in order to enhance the prediction accuracy and interpretability of the statistical model it produces.

Similar to `Ridge regression`, with predictors (X) and response (Y) centered and scaled, this method can be conceptualized by posing a constraint to the least squares problem:

<div align="center">
min<sub>&beta;</sub>(Y-X&beta;)<sup>T</sup>(Y-X&beta;)
</div>  
<div align="center">
&sum;|&beta;<sub>j</sub>|&le;t
</div>

Thus the lasso regression coefficient estimates are the values that minimize

<div align="center">
(Y-X&beta;)<sup>T</sup>(Y-X&beta;)+&lambda;&sum;|&beta;<sub>j</sub>|
</div>

where &lambda;&ge;0 is a tuning parameter, to be determined separately.

`lambda`: As shown in the definition of `Ridge regression` and `Lasso regression`, &lambda;&ge;0 is a tuning parameter, to be determined separately.

`prop`: Under our model setting, prop is &beta;<sub>j</sub><sup>2</sup>&frasl;&beta;<sub>OLS</sub><sup>2</sup>; for ridge regression, and is |&beta;|&frasl;&sum;|&beta;| for lasso regression.




