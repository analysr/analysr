
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analysr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/analysr/analysr/workflows/R-CMD-check/badge.svg)](https://github.com/analysr/analysr/actions)
[![Lint](https://github.com/analysr/analysr/workflows/lint/badge.svg)](https://github.com/analysr/analysr/actions)
[![Build
Status](https://travis-ci.com/analysr/analysr.svg?branch=main)](https://travis-ci.com/analysr/analysr)
<!-- badges: end -->

## Overview

A query language for time-dependent data analysis.

  - [Package Documentation](https://analysr.github.io/analysr/)
  - [Learning R](https://analysr.github.io/learning/)

## Installation

``` r
# Install the development version from GitHub:
install.packages("devtools")
devtools::install_github("analysr/analysr")
```

## Usage

### 1\. General concepts

We use `magrittr` which is a forward-pipe operator for R. This provides
a mechanism for chaining commands with a new forward-pipe operator:
**%\>%**. This operator passes a value, or the result of an expression,
into the next function call or expression. Note that this is not a pipe.
For example :  
\- **x %\>% f** is equivalent to **f(x)**  
\- **x %\>% f(y)** is equivalent to **f(x, y)**  
\- **x %\>% f %\>% g %\>% h** is equivalent to **h(g(f(x)))**

### 2\. General operation of our query language

When we code our functions for our keywords, they are of type :
function(model, params)

For example, the following query:

``` r
add_description(after(at_most(observed(model, temperature >= 40), 15*days), surgery), `fever post surgery`)
```

is broken down as follows:

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)  
  %>% at_most(45*days) 
  %>% after(`Subcutaneous immunotherapy`)
  %>% add_description(`Hypertension after immunotherapy`)
)
```

So *model* must contain the model 5 table, and then we give instructions
to pass to the next function (so here **at\_most** doesn’t make sense
alone).

### 3\. Keywords

#### Observed

A query always starts with observed, as follows: - **observed**(*model*,
conditions) This function can be complemented by various keywords that
do not work alone, see the secondary keywords below.

#### Secondary keywords

These keywords complement other keywords for specific information.

##### before

The **before** function is used to return the set of events or
measurements that verify a certain condition observed before an event /
measurement.

Example :

``` r
(
  analysr_env
  %>% observed(`Microalbumin Creatinine Ratio` > 300)  
  %>% before(`Renal dialysis (procedure)`)
)
```

This query returns all patients with a microalbumin-creatinine ratio
greater than 300 mg/g before dialysis.

##### after

The **after** function is used to return the set of events or
measurements that verify a certain condition observed after an event /
measurement.

Example :

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)  
  %>% after(`Subcutaneous immunotherapy`)
)
```

This query returns all patients with a diastolic blood pressure above
mmHg after subcutaneous immunotherapy.

##### at\_most

The **at\_most** function returns the set of events or measurements that
verify a certain condition observed at most (at\_most) a certain amount
of time after (after) or before (before) an event / measurement.

Example :

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)  
  %>% at_most(45*days)
  %>% after(`Subcutaneous immunotherapy`)
)
```

This query returns all measures with a diastolic blood pressure greater
than mmHg at least 45 days after subcutaneous immunotherapy.

##### at\_least

The **at\_least** function returns the set of events or measurements
that verify a certain condition observed at least (at\_least) a certain
amount of time after (after) or before (before) an event / measurement.

Example :

``` r
(
  analysr_env
  %>% observed(`Hepatitis C antibody test`)
  %>% at_least(10*days)
  %>% after(`Standard pregnancy test`)
)
```

This query returns all measures where patients were tested positive for
hepatitis C antibodies at least 10 days after a standard pregnancy test.

##### add\_description

The **add\_description** function adds a description to the description
table.

Example :

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)  
  %>% at_most(45*days) 
  %>% after(`Subcutaneous immunotherapy`)
  %>% add_description(`Hypertension after immunotherapy`)
)
```

This query adds `Hypertension after immunotherapy` to the table
description for people with blood pressure \>125 mm Hg up to 45 days
after subcutaneous immunotherapy.

##### from / to

The **from/to** functions are used to return all the events or
measurements included in a period between two defined dates.

Example :

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)  
  %>% from(2013-02-06T19:00:00Z) 
  %>% to(2014-10-08T20:00:00Z)
)
```

This query returns people who had a blood pressure above 125 mmHg
between 6 February 2013 at 7pm and 8 October 2014 at 8pm.

##### inside

The **inside** function is used to return the set of events or
measurements that verify a certain condition observed during a certain
period.

Example :

``` r
(
  analysr_env
  %>% observed(`Respiratory rate`  >= 12)
  %>% inside(`Respiratory therapy`)
)
```

This query returns all patients with a respiratory rate greater than
12/min during respiration therapy.

##### outside

The **outside** function is used to return the set of events or
measurements that verify a certain condition observed outside a certain
period.

Example :

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure`  >= 75)
  %>% outside(`Diabetes self management plan`)
)
```

This query returns all measures with a diastolic blood pressure greater
than 75 mmHg outside their diabetes self-management plan period.

### 4\. Import functions

##### import\_events\_csv

The **import\_events\_csv** function is used to import the events table
from a CSV file.

Example :

``` r
import_events_csv(
    csv_path = "csv_events_path",
    stat_unit = "PATIENT",
    date = "DATE",
    tag = "DESCRIPTION")
```

##### import\_measures\_csv

The import\_measures\_csv function is used to import the measures table
from a CSV file.

Example :

``` r
import_measures_csv(
    csv_path = "csv_measures_path",
    stat_unit = "PATIENT",
    date = "DATE",
    tag = "DESCRIPTION",
    value = "VALUE")
```

##### import\_periods\_csv

The **import\_periods\_csv** function is used to import the periods
table from a CSV file.

Example :

``` r
import_periods_csv(
    csv_path = "csv_periods_path",
    stat_unit = "PATIENT",
    begin = "START",
    end = "STOP",
    tag = "DESCRIPTION")
```

##### import\_stat\_units\_csv

The **import\_stat\_units\_csv** function is used to import the periods
table from a CSV file.

Example :

``` r
import_stat_units_csv(
    csv_path = "./benchmark/csv_100/patients_100.csv",
    stat_unit = "UserId",
    optional_data = c("BIRTHDATE","DEATHDATE","FIRST","LAST","RACE","ETHNICITY","GENDER","STATE","HEALTHCARE_EXPENSES","HEALTHCARE_COVERAGE")
)
```
