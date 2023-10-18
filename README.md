FaaS - API modeling <!-- and model update-->
================

<!-- badges: start -->
[![minimal R 
version](https://img.shields.io/badge/R%3E%3D-4.0.3-blue.svg)](https://cran.r-project.org/) [![License: MPL
2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://www.mozilla.org/en-US/MPL/2.0/)
<!-- badges: end -->

# faas4i

**Repository for running scale modeling and model update on FaaS**

Scale modeling performs an exhaustive search for best models in time series data, providing information about the fit of the best models, their cross-validation accuracy measures and many other outputs that are usually of interest. 

<!-- The model update module, on the other hand, allows models obtained using the scale modeling to be updated and reestimated based on a new dataset.

Both products, the scale modeling and the model update on FaaS, are briefly described below. The full description of their parameters can be found on the [Wiki](https://github.com/4intelligence/faas4i/wiki) of the repository. -->

A brief description of the scale modeling is presented below. The full description of their parameters can be found on the [Wiki](https://github.com/4intelligence/faas4i/wiki) of the repository.

## Installation

Before you start this installation, make sure you have the package **remotes** installed in your machine

``` r
install.packages("remotes")

remotes::install_github("4intelligence/faas4i", force = TRUE)
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

- A list of datasets to perform modeling, where the list elements must be named  after the Y variable name. 
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
                   user_model = list())
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

#### 4\) Send job request

Wants to make sure everything is alright? Though not necessary, you can validate your request beforehand by using the following function:

``` r
faas4i::validate_models(data_list = data_list, date_variable = date_variable, 
                        date_format = date_format, model_spec = model_spec,
                        project_name = project_name) 
```
It will return a message indicating if your specifications are correctly defined and point out to the arguments that need adjustment (if any). 

Or you can simply send your **FaaS API** request. We'll take care of running the *validate_models* and let you know if something needs your attention before we can proceed. If everything is correct, we'll automatically send the request, and you will see a message with the status of your request in your console.

``` r
faas4i::run_models(data_list = data_list, date_variable = date_variable, 
                   date_format = date_format, model_spec = model_spec,
                   project_name = project_name) 
```

## II) Model Update

Once you have done your initial modeling and are ready to update your **forecast_pack** with your **new_data**, you can use the **run\_update** function to send your request. As of now we will only update the first 25 models of your **forecast_pack**. 

Below we briefly describe the arguments that you will need to feed the **run\_update** function. 

#### 1\) Pack List \[‘pack\_list’\]

The **pack_list** is a list with information about all packs to be updated. For each pack we need its original **forecast_pack**, obtained in the FaaS modeling, and a **new_data**. 

##### Example 1 pack\_list \[single Y\]:

``` r
# Load a data frame with our data
dataset_1 <- readxl::read_excel("your_path/inputs/dataset_1.xlsx")

# Load forecast_pack with models
forecast_pack_1 <- readRDS("your_path/forecast_1_fs_pim.rds")

# Put it inside a list and name the list with the elements' name
pack1 <- list(forecast_pack = forecast_pack_1,
              new_data = dataset_1)
              
# Put the list inside the pack_list
pack_list <-  list(pack1)

# Also, specify the date variable and its format 
date_variable <- "DATE_VARIABLE"
date_format <- '%Y-%m-%d'
```

<br>

##### Example 2 pack\_list \[multiple Ys\]:

[CAUTION: heavy sized pack_list may lead to problems in the request, preferably send single Y as in the previous example - We are working on this limitation]

``` r
# Load data frames with our data
dataset_1 <- readxl::read_excel("your_path/dataset_1.xlsx")
dataset_2 <- readxl::read_excel("your_path/dataset_2.xlsx")
dataset_3 <- readxl::read_excel("your_path/dataset_3.xlsx")

# Load forecast_pack with models
forecast_pack_1 <- readRDS("your_path/forecast_1_fs_pim.rds")
forecast_pack_2 <- readRDS("your_path/forecast_2_fs_pmc.rds")
forecast_pack_3 <- readRDS("your_path/forecast_3_fs_pib.rds")

# Put each forecast pack and new dataset inside a list and name the 
# list with the elements' name
pack1 <- list(forecast_pack = forecast_pack_1,
              new_data = dataset_1)
pack2 <- list(forecast_pack = forecast_pack_2,
              new_data = dataset_2)
pack3 <- list(forecast_pack = forecast_pack_3,
              new_data = dataset_3)
              
# Put the list inside the pack_list
pack_list <-  list(pack1,
                   pack2,
                   pack3)

# Also, specify the date variable and its format 
# (must have the same name in all datasets)
date_variable <- "DATE_VARIABLE"
date_format <- '%Y-%m-%d'
```

<br>

#### 2\) Cross-validation update \[cv_update\]

The parameter **cv_update** can be set to TRUE or FALSE. 

#### 3\) Model Specifications \[‘model\_spec’\]

Regardless of whether you are updating one or multiple packs, the **model_spec** follows the same logic. A list of desired modeling specification by the user, for example: <br>
``` r
model_spec <- list(n_steps = 12,
                   n_windows = 12,
                   cv_summary = "mean",
                   fill_forecast = FALSE)
```
All parameters in the **model_spec** are optional. However, the cross-validation parameters (**n_steps**, **n_windows** and **cv_summary**) can only be changed when **cv_update** is TRUE. If these parameters are not specified, we keep the original parameters.

#### 4\) Initial modeling date \[‘base\_dates’\]

The parameter **base_dates** can be set to TRUE or FALSE.

#### 5\) Outlier Update \[‘outlier\_update’\]

The parameter **outlier_update** can be set to TRUE or FALSE. Can only be set to TRUE when **cv_update** is TRUE.

#### 6\) Model Breakdown \[‘breakdown’\]

The parameter **breakdown** can be set to TRUE, FALSE or vector with 'row_id' of models to run breakdown (max of 3 models).

#### 7\) Project Name \[‘project\_name’\]

Define a project name. A string with character and/or numeric inputs that should be at most 50 characters long. Special
characters will be removed.

#### 8\) Send job request

Once all the files and parameters have been chosen, you can send the FaaS update request. We'll make a few validation checks and let you know if something needs your attention before we can proceed. If everything is correctly specified, we'll automatically send the request, and you will see a message with the status of your request in your console.

The default request is:
``` r
faas4i::run_update(pack_list = pack_list, date_variable = date_variable,
                   date_format = date_format, project_name = project_name, 
                   cv_update = FALSE, base_dates = TRUE, model_spec = list()) 
```


## III) Other Functionalities

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
