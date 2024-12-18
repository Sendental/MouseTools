
# Mouse Clinical Trial Tools

## Overview

`MouseTools` is an R package designed to streamline data cleaning,
management, and analysis in clinical trials or other longitudinal
studies. Originally developed to assist with a mouse vaccine trial
dataset, this package provides general-purpose tools that can be applied
to any dataset with similar challenges, such as handling inconsistent
date formats, detecting numeric outliers, and summarizing datasets with
multiple units of measurement.

### Why Should I Use This Package?

1.  **Simplify Data Wrangling**: With functions like `date_standardizer`
    and `summary_table_with_conversion`, this package automates common
    yet tedious data cleaning tasks, ensuring consistency and
    reliability in your analyses.
2.  **Detect and Manage Outliers**: The `outlier_detection` function
    helps you identify and flag outliers using robust statistical
    methods, making your datasets cleaner and your results more
    trustworthy.
3.  **Generate Visual Insights**: The `plot_trend` function enables
    quick and informative visualizations to help you understand trends
    in your data.
4.  **Versatile Applications**: While designed for clinical trial
    datasets, these functions are generalizable to various domains,
    including finance, public health, and social sciences.

------------------------------------------------------------------------

## How to Access the Package

You can install `MouseTools` from GitHub:

``` r
# Install the package from GitHub
devtools::install_github("Sendental/MouseTools")
```

    ## Downloading GitHub repo Sendental/MouseTools@HEAD

    ## ── R CMD build ──────────────────────────────────────────────────────────────────
    ##      checking for file ‘/tmp/RtmpUM9pQG/remotes144199d793a/Sendental-MouseTools-8e856bb/DESCRIPTION’ ...  ✔  checking for file ‘/tmp/RtmpUM9pQG/remotes144199d793a/Sendental-MouseTools-8e856bb/DESCRIPTION’ (468ms)
    ##   ─  preparing ‘MouseTools’:
    ##      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts (484ms)
    ##   ─  checking for empty or unneeded directories
    ## ─  building ‘MouseTools_0.0.0.9000.tar.gz’
    ##      Warning: invalid uid value replaced by that for user 'nobody'
    ##    
    ## 

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.4'
    ## (as 'lib' is unspecified)

Once installed, load the package:

``` r
library(MouseTools)
```

## How to Use the Package

Below are two examples demonstrating how to use the package:

**Example 1: Standardizing Dates**

Do you have inconsistent date formats? date_standardizer ensures
uniformity:

``` r
df <- data.frame(Date = c("01/02/2020", "2020/02/03", "31/12/2020", "InvalidDate"))
cleaned_df <- date_standardizer(df, Date)
print(cleaned_df)
```

    ##         Date
    ## 1 2020-02-01
    ## 2 2020-02-03
    ## 3 2020-12-31
    ## 4       <NA>

**Example 2: Detecting Outliers**

Identify numeric outliers with outlier_detection:

``` r
df <- data.frame(Values = c(1, 2, 3, 100))
outliers <- outlier_detection(df, Values)
print(outliers)
```

    ##   Values
    ## 1    100

## Documentation and Vignettes

For detailed usage and extended examples, refer to the package vignette:

``` r
vignette("MouseTools")
```

## License

This package is distributed under the GPL license.
