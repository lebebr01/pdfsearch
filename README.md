# pdfsearch

[![R build status](https://github.com/lebebr01/pdfsearch/workflows/R-CMD-check/badge.svg)](https://github.com/lebebr01/pdfsearch/actions?workflow=R-CMD-check)
[![Build status](https://ci.appveyor.com/api/projects/status/kjptcw7m8tlajmix?svg=true)](https://ci.appveyor.com/project/lebebr01/pdfsearch)
[![codecov](https://codecov.io/github/lebebr01/pdfsearch/graph/badge.svg?token=q71fGp24Dr)](https://codecov.io/github/lebebr01/pdfsearch)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/pdfsearch)](https://cran.r-project.org/package=pdfsearch)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.00668/status.svg)](https://doi.org/10.21105/joss.00668)

This package defines a few useful functions for keyword searching using the [pdftools](https://github.com/ropensci/pdftools)  package developed by [rOpenSci](https://ropensci.org/).

The package can be installed from CRAN directly:

``` r
install.packages("pdfsearch")
```

To install the development version you use devtools:

``` r
install.packages("devtools")
devtools::install_github('lebebr01/pdfsearch')
```

## Basic Usage
There are currently two functions in this package of use to users. The first `keyword_search` takes a single pdf and searches for keywords from the pdf. The second `keyword_directory` does the same search over a directory of pdfs.

## Example with `keyword_search`
The package comes with two pdf files from [arXiv](https://arxiv.org/) to use as test cases. Below is an example of using the `keyword_search` function.

``` r
library(pdfsearch)
file <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')

result <- keyword_search(file, 
            keyword = c('measurement', 'error'),
            path = TRUE)

head(result$line_text, n = 2)
```

```
## [[1]]
## [1] "Reiter, Maria DeYoreo∗ arXiv:1610.00147v1 [stat.ME] 1 Oct 2016 Abstract Often in surveys, key items are subject to measurement errors. "
## 
## [[2]]
## [1] "In some settings, however, analysts have access to a data source on different individuals with high quality measurements of the error-prone survey items. "
```

The location of the keyword match, including page number and line number, the actual line of text, and a tokenized version of the text (raw text split by individual words) are returned by default.

In addition, by default the hyphenated words at the end of the text are combined with the continued word at the start of the next line. If this behavior is not of interest, set the `remove_hyphen` argument to `FALSE`.

### Surrounding lines of text 
It may be useful to extract not just the line of text that the keyword is found in, but also surrounding text to have additional context when looking at the keyword results. This can be added by using the argument `surround_lines` as follows:

``` r
file <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')

result <- keyword_search(file, 
            keyword = c('measurement', 'error'),
            path = TRUE, surround_lines = 1)
head(result)
head(result$line_text, n = 2)
```

## Example with `keyword_directory`
The `keyword_directory` function allows users to search for keywords in multiple PDF files in one function call. The same functionality from the `keyword_search` function can be invoked, specifically `remove_hyphen` and `surround_lines`. Below is an example of searching a single directory. 


``` r
directory <- system.file('pdf', package = 'pdfsearch')

# do search over two files
directory_result <- keyword_directory(directory, 
       keyword = c('repeated measures', 'measurement error'),
       surround_lines = 1)

head(directory_result, n = 2)
```

A few other useful arguments are possible when searching for keywords within multiple PDF files in a directory. One is the `recursive` (default is `FALSE`), where if set to `TRUE` will search within subdirectories as well, the default function behavior will not venture into subdirectories. Finally, if the directory has many PDF files, testing the function first on a handful of PDF files may be desired. The number of PDF files can be limited with the argument `max_search` where a positive integer can be specified indicating the number of PDF files to search. For example, is `max_search = 2`, only the first two PDF files will be searched within the directory.

### Shiny App
The package also has a simple Shiny app that can be called using the following command

``` r
run_shiny()
```

## Usage in Research
The pdfsearch package may be most useful to those conducting research syntheses or meta-analyses. The package can allow users to search for keywords related to a research question; therefore, instead of searching the entire text of a document, specific portions of the text can be identified to be searched. This could increase the reproducibility and reduce the time needed to collect the data for the research synthesis or meta-analysis.

As an example, the package is currently being used to explore the evolution of statistical software and quantitative methods used in published social science research (https://ww2.amstat.org/meetings/jsm/2018/onlineprogram/AbstractDetails.cfm?abstractid=330777). This process involves getting PDF files from published research articles and using pdfsearch to search for specific software and quantitative methods keywords within the research articles. The results of the keyword matches will be explored using research synthesis methods. A pre-print of the paper and slides from the presentation will be posted to the GitHub repo as part of the package later this summer.

