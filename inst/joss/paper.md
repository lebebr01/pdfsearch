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

PDF files are common formats for reports, journal articles, briefs, and many other documents. PDFs are lightweight, portable, and easily viewed across operating systems. Even though PDF files are ubiquitous extracting and finding text within a PDF can be time consuming and not easily reproducible. The pdftools R package [@pdftools], which uses the poppler C++ library, to extract text from PDF documents aids in the ability to import text data from PDF files to manipulate in R. The pdfsearch package [@pdfsearch] is an R package [@r-base] that extends the text extraction of pdftools to allow for keyword searching within a single PDF or a directory of PDF files.

The pdfsearch package can aid users in manipulation of text data from PDF files in R and may also improve the reproducibility of the extraction and manipulation tasks. Users can search for keywords within PDF file(s) where the location of the match and the raw text from match are returned. This aspect of searching for keywords may be most useful for those conducting research syntheses that are more reproducible and less time consuming than is currently done. The package vignette includes more information on this package, including example keyword searches and discusses limitations of the package. Below is example output of one of these examples. Link to release can be found here: <https://doi.org/10.5281/zenodo.1210279>

![Example Output](joss.png)

# References
