# Salmon Assessment Analytics Web

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pacific-salmon-assess/saaWeb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pacific-salmon-assess/saaWeb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


## Installation

You can install the latest version of `saaWeb` from [GitLab](https://github.com/Pacific-salmon-assess/saaWeb) with:

``` r
install.packages("remotes") 
remotes::install_git("https://github.com/Pacific-salmon-assess/saaWeb") 
```

## CWT Data Extractor

``` r
example_query_file <- system.file("query_doc", "tag_release.qry", package = "saaWeb")
x <- runCwtExtractorQuery(example_query_file)
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
