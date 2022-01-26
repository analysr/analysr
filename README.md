
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

-   [Package Documentation](https://analysr.github.io/analysr/)
-   [Learning R](https://analysr.github.io/learning/)

## Installation

``` r
# Install the development version from GitHub:
install.packages("devtools")
devtools::install_github("analysr/analysr")
```

## Usage

### 1. General concepts

We use `magrittr` which is a forward-pipe operator for R. This provides
a mechanism for chaining commands with a new forward-pipe operator:
**%>%**. This operator passes a value, or the result of an expression,
into the next function call or expression. Note that this is not a pipe.
For example : - **x %>% f** is equivalent to **f(x)** - **x %>% f(y)**
is equivalent to **f(x, y)** - **x %>% f %>% g %>% h** is equivalent to
**h(g(f(x)))**

### 2. General operation of our query language

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
to pass to the next function (so here **at_most** doesn’t make sense
alone).

Steps to use our library correctly: \* Import data thanks
`import_[your table]_csv` (where table can be *periods*, *measures*,
*events* or *stat_units*) or import_FHIR \* Write request with keywords
\* `create_feature` or `add_description`

#### When to use `create_feature` or `add_description` ?

`add_description` is use to add a description to the description table
for a future use.

`create_feature` will add a column in stat_units table. The stats_units
table will contain all the features you want to extract (this is the
features extraction part).

Basically, **stats_units table contains the final result**.

#### How does the language work ?

The request starts by the `observed` tag. This will create a `selection`
table. This one will be used by other tag afterwards.

The user can add time selection keywords (**cf. 4. keywords**) to select
a range time or refine his request.

The range period can be selected for each data, before or after a date
to highlights a data. (**cf. 4. keywords, before or after**)

On the contrary the `from [...] to` select an absolute time range
between two weel-defined dates.

`during` is used for selecting an element from the the selection table
which take place during a period of time.

#### What do you mean by `model` ?

A `model` in this documentation is an Analysr Env.

An Analysr Env (named `analysr_env`) is by default created in the
package environment.

This is why we use `analysr_env` at the beginning of a request, like
this one:

``` r
(
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)
  %>% at_most(45*days)
  %>% after(`Subcutaneous immunotherapy`)
)
```

As most of the keywords return an updated model, you can also do
something like that:

``` r
model <- (
  analysr_env
  %>% observed(`Diastolic Blood Pressure` > 125)
  %>% after(`Subcutaneous immunotherapy`)
)

(
  model
  %>% observed(`Microalbumin Creatinine Ratio` > 300)
  %>% before(`Renal dialysis (procedure)`)
)
```

The `restrict` function is useful to get a restricted version of your
model to some condition.

If you want to create a new one use `setup_new_env` function.

### 3. Functions to edit data

#### “Inducing” functions

##### induce_period

This fucntion allows you to create period from measures table depending
on a condition (on measures or events), and add them to periods table.

``` r
induce_event(model = analysr_env, condition, tag_to_create)
```

##### induce_event

This function allows you to create event from measures table depending
on a condition, and add them to events table.

``` r
induce_event(model = analysr_env, condition, tag_to_create)
```

##### induce_measure

This function allows you to create a measure from using a certain
function.

``` r
induce_measure(model = analysr_env, tag_to_create, calcul, tag_ref)
```

For exemple if you want to create a Body Mass Index (named BMI here)
entry for each Weight entry you have to use this:

``` r
induce_measure(analysr_env, "BMI", Weight / (Size * Size))
```

This function operate only on measure table.

#### fix_granularity

This function allow you to sets a given granularity to a set of data by
aggregating or imputing them.

``` r
fix_granularity(
  tag_wanted,
  period_start,
  period_end,
  temporal_granularity,
  stat_unit_wanted = NULL,
  aggregation_method = mean_aggregate,
  impute_method = linear_impute,
  information_lost_after = 5 * temporal_granularity
)
```

### 4. Keywords

#### A) **observed**

A query always starts with observed, as follows: - **observed**(*model*,
conditions) This function can be complemented by various keywords that
do not work alone, see the secondary keywords below.

#### B) **before**

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

#### C) **after**

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

#### D) **at_most**

The **at_most** function returns the set of events or measurements that
verify a certain condition observed at most (at_most) a certain amount
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

#### E) **at_least**

The **at_least** function returns the set of events or measurements that
verify a certain condition observed at least (at_least) a certain amount
of time after (after) or before (before) an event / measurement.

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

#### F) **from / to**

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

#### G) **inside** or **during**

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

#### H) **outside**

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

#### I) **restrict** or **create_cohort**

The **restrict** function allow you to restrict your model to a specific
environment. Example :

``` r
model <- (
  analysr_env
  %>% restrict(`Diastolic Blood Pressure`  >= 75)
)
```

or

``` r
model <- restrict(analysr_env, `Diastolic Blood Pressure`  >= 75)
```

to restrain your model only to the stat units who have a measure named
*“Diastolic Blood Pressure”* which is over *75* (unit unknown there).

#### J) **add_description**

The **add_description** function adds a description to the description
table. The stat units described by the tag passed in argument:

``` r
add_description(input, label)
```

The `input` can be a vector of stat units or an AnalysR env.

Example 1:

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

Exemple 2:

``` r
add_description(c(1, 6, 6), "Fever")
```

Example 2:

``` r
add_description(analysr_env, "Fever")
```

#### L) **create_feature**

This function create a new coll named with the description tag in the
stat_units table. **The function will extract feature form the measure
table.** You need to specify which tag you want, in which period you
want to execute the aggregation method and which col name you want in
the stat_units table.

``` r
create_feature(
  model,
  tag_to_create,
  wanted_tag,
  start,
  end,
  aggregation_method = mean_aggregate
)
```

Example:

     model <- create_feature(analysr_env,
                              "Temp_average_2006",
                              "Temperature",
                              lubridate::parse_date_time("2006/01/01 01:00:00", "ymd-HMS"),
                              lubridate::parse_date_time("2006/12/31 23:59:00", "ymd-HMS"))

#### M) **extract_feature**

The **extract_feature** function is quite the same goal as
**create_feature**. This function create a new coll named with the
description tag in the stat_units table. **The function will extract
feature form the description table.**

``` r
extract_feature(model, tag)
```

#### N) **described_by**

The **described_by** function allows you to filter the selection table
(or if you want this allows you to refine the request).

``` r
described_by(model, condition)
```

Exemple:

``` r
(
    analysr_env
    %>% observed(Temperature > 38)
    %>% described_by(Gender == "Male")
)
```

#### O) **export_raw** (*no test*)

The **export_raw** function is an experimental feature. There is no unit
test. This allows you to export data grouped by stat_units.

### 5. Import functions

#### A) **import_events_csv**

The **import_events_csv** function is used to import the events table
from a CSV file.

Example :

``` r
import_events_csv(
    csv_path = "csv_events_path",
    stat_unit = "PATIENT",
    date = "DATE",
    tag = "DESCRIPTION")
```

#### B) **import_measures_csv**

The import_measures_csv function is used to import the measures table
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

#### C) **import_periods_csv**

The **import_periods_csv** function is used to import the periods table
from a CSV file.

Example :

``` r
import_periods_csv(
    csv_path = "csv_periods_path",
    stat_unit = "PATIENT",
    begin = "START",
    end = "STOP",
    tag = "DESCRIPTION")
```

#### D) **import_stat_units_csv**

The **import_stat_units_csv** function is used to import the periods
table from a CSV file.

Example :

``` r
import_stat_units_csv(
    csv_path = "./benchmark/csv_100/patients_100.csv",
    stat_unit = "UserId",
    optional_data = c("BIRTHDATE","DEATHDATE","FIRST","LAST","RACE","ETHNICITY","GENDER","STATE","HEALTHCARE_EXPENSES","HEALTHCARE_COVERAGE")
)
```

#### E) **import_FHIR**

The **import_FHIR** function is used to import data from a stat_units
(here *de facto* a patient) from a FHIR file.

Example :

``` r
import_stat_units_csv(
    folder_path = "./fhir_files/"
)
```

This will import all the FHIR files which are in the `./fhir_files/`
folder.
