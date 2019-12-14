## Welcome to PLR!

This application can be used to fit penalized linear regression models, such as Lasso and Ridge, to a dataset uploaded by the user, and more importantly to visualize the process of model fitting.

Specifically, this app serves the following functions:

1. `EDA` (Exploratory Data Analysis), which displays the numeric summary and statistical plots for individual variables from the dataset, together with scatter plots of both type bivariate and trivariate relationships amont different variables.

2. `Regression`, which fits a penalized linear regression model to the uploaded dataset, with a range of parameters to adjust by users, such as model type (Ridge vs. Lasso), shrinkage parameter (well-known as lambda) range and interaction level. Moreover, this part visualizes the process of fitting the model, by displaying how coefficients are shrunk as shrinkage parameter changes and how the final coefficients are settled for a specific choice of lambda.

3. A series of other fuctions, such as `glossary` to offer guidance to users.

Note that the user need to upload a valid data frame object in CSV format in order to navigate into the main page of our shiny app. This data frame can be changed through `dataset management` part in `More`. Users are recommended to put response as the first column of the uploaded data frame.
