---
title: 'pdfsearch: Search Tools for PDF Files'
tags:
  - Keyword Search
  - PDF
  - reproducible research
  - R
authors:
 - name: Brandon LeBeau
   orcid: 0000-0002-1265-8761
   affiliation: 1
affiliations:
 - name: University of Iowa
   index: 1
date: 30 March 2018
bibliography: paper.bib
---

# Summary

PDF files are common formats for reports, journal articles, briefs, and many other documents. PDFs are lightweight, portable, and easily viewed across operating systems. Even though PDF files are ubiquitous, extracting and finding text within a PDF can be time consuming and not easily reproducible. The pdftools R package [@pdftools], which uses the poppler C++ library to extract text from PDF documents, aids in the ability to import text data from PDF files to manipulate in R. The pdfsearch package [@pdfsearch] is an R package [@r-base] that extends the text extraction of pdftools to allow for keyword searching within a single PDF or a directory of PDF files.

The pdfsearch package can aid users in manipulation of text data from PDF files in R and may also improve the reproducibility of the extraction and manipulation tasks. Users can search for keywords within PDF files where the location of the match and the raw text from the match are returned. This aspect of searching for keywords may be most useful for those conducting research syntheses or meta-analyses [@cooper2017] that are more reproducible and less time consuming than current practice. Current research synthesis or meta-analysis practice involves the reading of each document to search for the presence of certain terms, phrases, or statistical effect size information to answer specific research questions. The improved workflow with the pdfsearch package would allow those conducting research syntheses, the ability to narrow down relavent portions of text based on the keyword matches returned by the package instead of looking at the entire text of the document. In addition, regular expressions could be written to search and extract statistical information needed to compute effect sizes automatically. 

As an example, the package is currently being used to explore the evolution of statistical software and quantitative methods used in published social science research [@lebeau2018]. This process involves getting PDF files from published research articles and using pdfsearch to search for specific software and quantitative methods keywords within the research articles. The results of the keyword matches will be explored using research synthesis methods [@cooper2017].

The package vignette includes more information on this package. Included in the vignette are keyword searches within PDF documents and an exploration of the output from the package. The vignette also discusses limitations of the package. Below is example output of the package searching for the phrase "repeated measures" from @2015arXiv. Link to the package release can be found here: <https://doi.org/10.5281/zenodo.1210279>

![Example output searching for "repeated measures" phrase in a single PDF document.](joss.png)

# References
