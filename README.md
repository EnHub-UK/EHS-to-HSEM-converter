<p align="center">
  <img src="assets/logo.jpg" width="100%">
</p>

# EHS ➦ HSEM | converter

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

[![DOI](https://zenodo.org/badge/308007860.svg)](https://zenodo.org/badge/latestdoi/308007860)

This `R` project converts original **English Housing Survey** (EHS) data sources into **Housing Stock Energy Model** (HSEM) tables, following the same methodology applied in the CHM:

  > Hughes, M., Armitage, P., Palmer, J., & Stone, A. (2012). Converting English Housing Survey Data for Use in Energy Models.

The main reason for having a dedicated *EHS project* is <u>to comply with licensing rules</u>. Therefore, upon accessing the sources for the first time, users must request access to the relevant datasets from [UK-Data-Service](https://www.ukdataservice.ac.uk).

<p align="center">
  <img src="assets/UKDS Logos_Col_Grey_300dpi.png" width="30%">
</p>

Once granted, the datasets need to be stored (or redirected) as shown at the bottom of this page. (Only the *documentation files* are hard copied to `/data` because these files are converted into plain text, so these can be accessed using *regular expressions* within the scripts).

Another reason for having a dedicated *repo* is that the *EHS* information is employed along different projects, and so the datasets can be stored and shared in a central repository.

<span style="background-color: #ffb82e"> Please note that...</span> **a)** the implementation (ie. stage B) has not been optimised for `R` , and so its performance may be relatively poor in some cases; and **b)** this repo is not supported anymore, as it will be replaced by a more comprehensive converter destined to process survey data from the four UK countries.

### Data Sources

|         |                                                                                                                                                                                                                                                     |
|---------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|   2008  |   Study Number **6612 - English Housing Survey, 2008: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2008: Housing Stock Data [computer file]. Colchester, Essex: UK Data Archive [distributor], November 2010. SN: 6612 , doi.org/10.5255/UKDA-SN-6612-1           |
|   2009  |   Study Number **6804 - English Housing Survey, 2009: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2009: Housing Stock Data. [data collection]. 3rd Edition. UK Data Service. SN: 6804, doi.org/10.5255/UKDA-SN-6804-3                                       |
|   2010  |   Study Number **7039 - English Housing Survey, 2010: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2010: Housing Stock Data [computer file]. 4th Edition. Colchester, Essex: UK Data Archive [distributor], March 2013. SN: 7039, doi.org/10.5255/UKDA-SN-7039-4  |
|   2011  |   Study Number **7386 - English Housing Survey, 2011: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2011: Housing Stock Data [computer file]. Colchester, Essex: UK Data Archive [distributor], August 2013. SN: 7386, doi.org/10.5255/UKDA-SN-7386-1              |
|   2012  |   Study Number **7511 - English Housing Survey, 2012: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2012: Housing Stock Data [computer file]. Colchester, Essex: UK Data Archive [distributor], July 2014. SN: 7511, doi.org/10.5255/UKDA-SN-7511-1                |
|   2013  |   Study Number **7802 - English Housing Survey, 2013: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2013: Housing Stock Data. [data collection]. 2nd Edition. UK Data Service. SN: 7802, doi.org/10.5255/UKDA-SN-7802-2                                       |
|   2014  |   Study Number **8010 - English Housing Survey, 2014: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2014: Housing Stock Data. [data collection]. 4th Edition. UK Data Service. SN: 8010, doi.org/10.5255/UKDA-SN-8010-4                                       |
|   2015  |   Study Number **8186 - English Housing Survey, 2015: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2015: Housing Stock Data. [data collection]. UK Data Service. SN: 8186, doi.org/10.5255/UKDA-SN-8186-1                                                    |
|   2016  |   Study Number **8350 - English Housing Survey, 2016: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2018). English Housing Survey, 2016: Housing Stock Data. [data collection]. UK Data Service. SN: 8350, doi.org/10.5255/UKDA-SN-8350-1                                              |
|   2017  |   Study Number **8494 - English Housing Survey, 2017: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2019). English Housing Survey, 2017: Housing Stock Data. [data collection]. UK Data Service. SN: 8494, doi.org/10.5255/UKDA-SN-8494-1                                              |
|   2018  |   Study Number **8670 - English Housing Survey, 2018: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2020). English Housing Survey, 2018: Housing Stock Data. [data collection]. UK Data Service. SN: 8670, doi.org/10.5255/UKDA-SN-8670-1   
|   2019  |   Study Number **8670 - English Housing Survey, 2018: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2022). English Housing Survey, 2019: Housing Stock Data. [data collection]. UK Data Service. SN: 8923, DOI: 10.5255/UKDA-SN-8923-1
|   2020  |   Study Number **8670 - English Housing Survey, 2018: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2023). English Housing Survey, 2020: Housing Stock Data. [data collection]. UK Data Service. SN: 9058, doi: 10.5255/UKDA-SN-9058-1


----

### *myScript* structure

- `scripts/main.R` serves as selector of workflows and modules

- `scripts/modules` libraries, functions and project variables.

- `scripts/workflows/A__initialAnalysis.R` loads multiple EHS (raw) sources, converts them into *tibbles* and performs descriptive and survey-enriched analysis.

- `scripts/workflows/B__conversion-HSEMs__allocation.R` converts the chosen raw dataset into a format suitable for Housing Stock Energy Models.

- `scripts/workflows/C__households` extracts detailed information regarding household members. This is also used for generating profiles.

- `scripts/workflows/D__predictive.R` combines all the data sources into single tables, considering only those with common variables; useful for predictive analysis (eg. random forests, j48, glm, ...)




### *data* Structure

The EHS datasets need to be requested in `.stata` format, and copied to `../data/` (as indicated below).

```sh
.
├── assets
├── data
│   ├── UKDA-6612-stata11 -> <symlinks>/UKDA-6612-stata11
│   ├── UKDA-6804-stata11 -> <symlinks>/UKDA-6804-stata11
│   ├── UKDA-7039-stata11 -> <symlinks>/UKDA-7039-stata11
│   ├── UKDA-7386-stata11 -> <symlinks>/UKDA-7386-stata11
│   ├── UKDA-7511-stata11 -> <symlinks>/UKDA-7511-stata11
│   ├── UKDA-7802-stata11 -> <symlinks>/UKDA-7802-stata11
│   ├── UKDA-8010-stata11 -> <symlinks>/UKDA-8010-stata11
│   ├── UKDA-8186-stata11 -> <symlinks>/UKDA-8186-stata11
│   ├── UKDA-8350-stata11 -> <symlinks>/UKDA-8350-stata11
│   ├── UKDA-8494-stata -> <symlinks>/UKDA-8494-stata
│   ├── UKDA-8670-stata -> <symlinks>/UKDA-8670-stata
│   ├── UKDA-8923-stata -> <symlinks>/UKDA-8923-stata
│   ├── UKDA-9058-stata -> <symlinks>/UKDA-9058-stata
│   ├── auxiliary_tables
│   └── data_dictionaries
├── export
└── scripts
    ├── modules
    └── workflows
```
---

 **NOTES**
 1. **data** folder employs `symlinks` to a centralised database where the original repositories are previously retrieved from [UK-Data-Archive](https://www.ukdataservice.ac.uk/get-data.aspx).
