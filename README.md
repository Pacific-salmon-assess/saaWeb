# Salmon Assessment Analytics Web

## Installation

You can install the latest version of `saaWeb` from [GitLab](https://github.com/Pacific-salmon-assess/saaWeb) with:

``` r
install.packages("remotes") 
remotes::install_git("https://github.com/Pacific-salmon-assess/saaWeb") 
```

## Salmon Age Batch

``` r
age_batch_df <- getAgeBatchList()
age_batch_detail_df <- getAgeBatchDetails(c(114159, 114161))
```


## Salmon Age Results

``` r
scale_age_df <- getAgeBatchScaleResults(c(107135,105721))
```
