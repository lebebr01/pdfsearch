# pdfsearch

[![Build Status](https://travis-ci.org/lebebr01/pdfsearch.svg?branch=master)](https://travis-ci.org/lebebr01/pdfsearch)
[![codecov.io](https://codecov.io/github/lebebr01/pdfsearch/coverage.svg?branch=master)](https://codecov.io/github/lebebr01/pdfsearch?branch=master)

This package defines a few useful functions for keyword searching using the [pdftools](https://github.com/ropensci/pdftools)  package developed by [rOpenSci](https://ropensci.org/).

To install use devtools:

```r
devtools::install_github('lebebr01/pdfsearch')
```

## Basic Usage
There are currently two functions in this package. The first `keyword_search` takes a single pdf and searches for keywords from the pdf. The second `directory_search` does the same search over a directory of pdfs.

## Examples
The package comes with two pdf files from [arXiv](https://arxiv.org/) to use as test cases. Below is an example of using the `keyword_search` function.

```r
library(pdfsearch)
file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')

result <- keyword_search(file, 
            keyword = c('repeated measures', 'mixed effects'),
            path = TRUE)
head(result)
```

```
## # A tibble: 6 × 4
##             keyword page_num line_num line_text
##               <chr>    <int>    <int>    <list>
## 1 repeated measures        1       24 <chr [1]>
## 2 repeated measures        2       57 <chr [1]>
## 3 repeated measures        2      108 <chr [1]>
## 4 repeated measures        2      110 <chr [1]>
## 5 repeated measures        2      125 <chr [1]>
## 6 repeated measures        6      444 <chr [1]>
```

```r
head(result$line_text, n = 2)
```

```
## [[1]]
## [1] "cally the repeated measures design, including the crossover           get false confidence about lack of negative effects. Statistical"
## 
## [[2]]
## [1] "fast iterations and testing many ideas can reap the most         erations to repeated measures design, with variants to the"
```

The location of the keyword match, including page number and line number, and the actual line of text are returned by default.

### Surrounding lines of text 
It may be useful to extract not just the line of text that the keyword is found in, but also surrounding text to have additional context when looking at the keyword results. This can be added by using the argument `surround_lines` as follows:

```r
file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')

result <- keyword_search(file, 
            keyword = c('repeated measures', 'mixed effects'),
            path = TRUE, surround_lines = 1)
head(result)
```

```
## # A tibble: 6 × 4
##             keyword page_num line_num line_text
##               <chr>    <int>    <int>    <list>
## 1 repeated measures        1       24 <chr [3]>
## 2 repeated measures        2       57 <chr [3]>
## 3 repeated measures        2      108 <chr [3]>
## 4 repeated measures        2      110 <chr [3]>
## 5 repeated measures        2      125 <chr [3]>
## 6 repeated measures        6      444 <chr [3]>
```

```r
head(result$line_text, n = 2)
```

```
## [[1]]
## [1] "introduce more sophisticated experimental designs, specifi-           only would we miss potentially beneficial effects, we may also"  
## [2] "cally the repeated measures design, including the crossover           get false confidence about lack of negative effects. Statistical"
## [3] "design and related variants, to increase KPI sensitivity with         power increases with larger effect size, and smaller variances." 
## 
## [[2]]
## [1] "a limitation to any online experimentation platform, where       within-subject variation. We also discuss practical consid-"   
## [2] "fast iterations and testing many ideas can reap the most         erations to repeated measures design, with variants to the"    
## [3] "rewards.                                                         crossover design to study the carry over effect, including the"
```

### Shiny App
The package also has a simple Shiny app that can be called using the following command

```r
run_shiny()
```


