FaaS - API modeling <!-- and model update-->
================

<!-- badges: start -->
[![minimal R 
version](https://img.shields.io/badge/R=%3D-4.4.0-blue.svg)](https://cran.r-project.org/) 
[![minimal R 
version](https://img.shields.io/badge/R=%3D-4.4.1-blue.svg)](https://cran.r-project.org/) 
[![minimal R 
version](https://img.shields.io/badge/R=%3D-4.4.2-blue.svg)](https://cran.r-project.org/) 
[![minimal R 
version](https://img.shields.io/badge/R=%3D-4.4.3-blue.svg)](https://cran.r-project.org/) 
[![License: MPL
2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://www.mozilla.org/en-US/MPL/2.0/)
<!-- badges: end -->

# faas4i

**Repository for running scale modeling and model update on FaaS**

Scale modeling performs an exhaustive search for best models in time series data, providing information about the fit of the best models, their cross-validation accuracy measures and many other outputs that are usually of interest. 

<!-- The model update module, on the other hand, allows models obtained using the scale modeling to be updated and reestimated based on a new dataset.

Both products, the scale modeling and the model update on FaaS, are briefly described below. The full description of their parameters can be found on the [Wiki](https://github.com/4intelligence/faas4i/wiki) of the repository. -->

A brief description of the scale modeling is presented below. The full description of their parameters can be found on the [Wiki](https://github.com/4intelligence/faas4i/wiki) of the repository.

## Installation

Before you start this installation, make sure you have the package **remotes** installed in your machine:

``` r
install.packages("remotes")
```

We currently require that you have the version **5.2.1** or greater of the package **curl** installed in your machine to use **faas4i**. 

Then you can install **faas4i**:
``` r
remotes::install_github("4intelligence/faas4i", force = TRUE, upgrade = FALSE)
```

Don't forget to load the library and you are all set to start using the package!

``` r
library("faas4i")
```

## Authentication

Each user will need to setup the authentication using the function **login**. The function **login** will display a URI where 4CastHub's user email and password will be required, as well as the two-factor-authentication code.

``` r
faas4i::login()
## Once the url is printed, copy and paste it to your browser and follow with authentication
```
By default, the login function will wait 90 seconds for authentication. If you wish to adjust the wait time, it is possible to change the parameter using a numeric value for **sleep_time**.

# Basic Usage

## I) Scale Modeling

The scale modeling requests are sent via ‘run\_models’ function. There are some arguments to feed this function. We are going to walk through all of them in this example and then will call the API.

#### 1\) Data List \[‘data\_list’\]

- A list of datasets to perform modeling, where the list elements must be named  after the Y variable name. It is not possible to have more than one Y (dependent) variable with same name in a **data_list**.
- The user should also define the **date_variable** and its format (**date_format**).

Let us see two examples of data lists, one with 1 Y and the other with
multiple Y’s <br>

##### Example 1 data\_list \[single Y\]:

``` r
# Load the dataset with our data
# Since this dataset is stored in the package, we can load it directly
dataset_1 <- faas4i::dataset_1

# But you will need to do something similar to the line below when loading 
# your own dataset locally
# dataset_1 <- readxl::read_excel("./inputs/dataset_1.xlsx")

# Put it inside a list (therefore, a 'data list')
# and name the list element with the name of the target variable
data_list <-  list(dataset_1)
names(data_list) <- c("fs_pim")

# Also, specify the date variable and its format 
date_variable <- "DATE_VARIABLE"
date_format <- '%Y-%m-%d'
```

<br>

##### Example 2 data\_list \[multiple Ys\]:

``` r
# Load a data frame with our data
# Again, we can load the data directly from the package for this example
dataset_1 <- faas4i::dataset_1
dataset_2 <- faas4i::dataset_2
dataset_3 <- faas4i::dataset_3

# But you will need to read it when you are loading your own data locally
# dataset_1 <- readxl::read_excel("./inputs/dataset_1.xlsx")
# dataset_2 <- readxl::read_excel("./inputs/dataset_2.xlsx")
# dataset_3 <- readxl::read_excel("./inputs/dataset_3.xlsx")

# Put it inside a list (therefore, a 'data list')
# and name every list element with the name of the target variable
data_list <-  list(dataset_1, dataset_2, dataset_3)
names(data_list) <- c("fs_pim", "fs_pmc", "fs_pib")

# Also, specify the date variable and its format 
# (must have the same name in all datasets)
date_variable <- "DATE_VARIABLE"
date_format <- '%Y-%m-%d'
```

<br>

#### 2\) **Model Specifications \[‘model\_spec’\]**

The model specifications should be provided in a list format as:

``` r
## Default settings
model_spec <- list(n_steps = <input>,
                   n_windows = <input>,
                   log = TRUE,
                   seas.d = TRUE,
                   n_best = 20,
                   accuracy_crit = "MAPE",
                   exclusions = list(),
                   golden_variables = c(),
                   fill_forecast = FALSE,
                   cv_summary = 'mean',
                   selection_methods = list(
                     lasso = TRUE,
                     rf = TRUE,
                     corr = TRUE,
                     apply.collinear = TRUE),
                   lags = list(),
                   allowdrift = TRUE,
                   allowoutliers = TRUE)
```

The critical and required input we expect from users is the CV setting (n\_steps and
n\_windows). All remaining non-provided arguments will assume their default values, as defined above. You can find a full description of these arguments on the [Wiki](https://github.com/4intelligence/faas4i/wiki) of the repository.

<br>

#### 3\) Project Name \[‘project\_name’\]

Define a project name. A string with character and/or numeric inputs that should be at most 50 characters long. Special
characters will be removed.

``` r
project_name <- "example_project"
```

#### 4\) User Model \[‘user\_model’\]

The definition of a model (or more than one) that user wants to see among the ARIMA models available in the plataform. The user can set the variables it wants in the model, the ARIMA order and the variables constraints.  
By default, the `user_model` parameter is an empty list, to define a user model it is necessary to create a list in which the list names are the response variable names and the values are lists of specifications, as described below.  
Each user model may contain the following parameters:  
- **vars**: A vector with the names of the explanatory variables the user wants in the customized model;  
- **order** (Optional): A vector with the ARIMA order (p, d, q) of the customized model. Such vector should always be of length 3, but the user can define as 'NA' the ARIMA terms that should be estimated freely, for example (NA, 1, NA) indicates that the ARIMA should be differenced, but `p` and `q` are free to be optimized. Users have the flexibility to specify all `p`, `d` and `q`, only `d` (in this case, `p` and `q` should be set to NA) or only `p` and `q` (in this case, `d` should be set to NA);
- **constraints** (Optional): A named list with the variables and constraints that the user wish to impose in the coefficients of this model. It is possible to set a specific value or a range of values, for 1 or more variables in **vars**;
  - At least one variable set on `vars` must be free of constraints;
  - It is also possible to add constraints to the intercept, which should be defined as the other variables, matching the name **intercept**;
  - If a constraint such as greater than 0 is needed, it can be defined as c(0, Inf), similarly, for constraints that are less than 0, the format is c(-Inf, 0).

```R
# defining an user_model for one Y
user_model <- list(
  "fs_pim" = list(
    list(
      "vars" = c("fs_ici", "fs_pmc", "fs_pop_des"),
      "order" = c(NA, 0, NA),
      "constraints" = list(
        "intercept"= c(3), 
        "fs_ici"= c(0, Inf),
        "fs_pmc"= c(-1, 1))
    ), # user model 1 for dataset_1
  )
)

# defining an user_model for multiple Y
user_model <- list(
  "fs_pim" = list(
    list(
      "vars" = c("fs_ici", "fs_pmc", "fs_pop_des"),
      "order" = c(NA, 0, NA),
      "constraints" = list(
        "intercept"= c(3), 
        "fs_ici"= c(0, Inf),
        "fs_pmc"= c(-1, 1))
    ), # user model 1 for dataset_1
    list(
      "vars" = c("fs_ici", "fs_pmc"),
      "order" = c(1, 1, 1),
      "constraints" = list(
        "fs_ici"= c(0.5))
    )
  ), # user model 2 for dataset_1
  "fs_pmc" = list(
    list(
      "vars" = c("fs_ici", "fs_pim", "fs_pop_ea"),
      "order" = c(NA, NA, NA),
      "constraints" = list(
        "intercept"= c(0),
        "fs_ici"= c(0, Inf),
        "fs_pim"= c(-1, 1))
    ) # user model 1 for dataset_3
  )
)
```

#### 5\) Send job request

Wants to make sure everything is alright? Though not necessary, you can validate your request beforehand by using the following function:

``` r
faas4i::validate_models(data_list = data_list, date_variable = date_variable, 
                        date_format = date_format, model_spec = model_spec,
                        project_name = project_name, user_model = user_model) 
```
It will return a message indicating if your specifications are correctly defined and point out to the arguments that need adjustment (if any). 

Or you can simply send your **FaaS API** request. We'll take care of running the *validate_models* and let you know if something needs your attention before we can proceed. If everything is correct, we'll automatically send the request, and you will see a message with the status of your request in your console.

``` r
faas4i::run_models(data_list = data_list, date_variable = date_variable, 
                   date_format = date_format, model_spec = model_spec,
                   project_name = project_name, user_model = user_model) 
```

## II) Other Functionalities

#### 1\) List projects 

You can check directly in R all projects you have in the platform using the following function:

```r

my_projects <- list_projects()

```

This function returns a list with information of the projects: project id, project name, status, creation date, last time it was updated, etc.

#### 2\) Download forecast pack 

You can also download the project output (forecast pack) in RDS format directly in R using the function:

```r

download_zip(project_id = "project_id",
             path = "path",
             filename = "file_name")

```
To download the forecast pack, you will need the **project_id** which is an information available in the output of the **list_projects** function. You need to give the **path** of the directory you want to save the forecast pack and **filename**.  
