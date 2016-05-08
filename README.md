<p align="center">
  <img src="public/logo.jpg" width="100%">
</p>

# EHS ➦ HSEM | converter

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

This `R` project converts original **English Housing Survey** (EHS) data sources into **Housing Stock Energy Model** (HSEM) tables.

The main reason for having a dedicated *EHS project* is <u>to comply with licensing rules</u>. Therefore, users should request access to the relevant datasets from [UK-Data-Service](https://www.ukdataservice.ac.uk).

<p align="center">
  <img src="public/UKDS Logos_Col_Grey_300dpi.png" width="30%">
</p>

Once granted, these can be copied (or redirected) as shown at the bottom of this page. (Only the *documentation files* are copied to `/myData` because these are converted into plain text.)

Another reason is that the EHS information is employed along different projects, and so the datasets can be stored in a central repository.


### Data Sources

|         |                                                                                                                                                                                                                                                     |
|---------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|   2008  |   Study Number **6612 - English Housing Survey, 2008: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2008: Housing Stock Data [computer file]. Colchester, Essex: UK Data Archive [distributor], November 2010. SN: 6612 , http://dx.doi.org/10.5255/UKDA-SN-6612-1           |
|   2009  |   Study Number **6804 - English Housing Survey, 2009: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2009: Housing Stock Data. [data collection]. 3rd Edition. UK Data Service. SN: 6804, http://doi.org/10.5255/UKDA-SN-6804-3                                       |
|   2010  |   Study Number **7039 - English Housing Survey, 2010: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2010: Housing Stock Data [computer file]. 4th Edition. Colchester, Essex: UK Data Archive [distributor], March 2013. SN: 7039, http://dx.doi.org/10.5255/UKDA-SN-7039-4  |
|   2011  |   Study Number **7386 - English Housing Survey, 2011: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2011: Housing Stock Data [computer file]. Colchester, Essex: UK Data Archive [distributor], August 2013. SN: 7386, http://dx.doi.org/10.5255/UKDA-SN-7386-1              |
|   2012  |   Study Number **7511 - English Housing Survey, 2012: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government, English Housing Survey, 2012: Housing Stock Data [computer file]. Colchester, Essex: UK Data Archive [distributor], July 2014. SN: 7511, http://dx.doi.org/10.5255/UKDA-SN-7511-1                |
|   2013  |   Study Number **7802 - English Housing Survey, 2013: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2013: Housing Stock Data. [data collection]. 2nd Edition. UK Data Service. SN: 7802, http://doi.org/10.5255/UKDA-SN-7802-2                                       |
|   2014  |   Study Number **8010 - English Housing Survey, 2014: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2014: Housing Stock Data. [data collection]. 4th Edition. UK Data Service. SN: 8010, http://doi.org/10.5255/UKDA-SN-8010-4                                       |
|   2015  |   Study Number **8186 - English Housing Survey, 2015: Housing Stock Data**                                                                                                                                                                          |
|         |   Department for Communities and Local Government. (2017). English Housing Survey, 2015: Housing Stock Data. [data collection]. UK Data Service. SN: 8186, http://doi.org/10.5255/UKDA-SN-8186-1                                                    |
|   2016  |   Study Number **8350 - English Housing Survey, 2016: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2018). English Housing Survey, 2016: Housing Stock Data. [data collection]. UK Data Service. SN: 8350, http://doi.org/10.5255/UKDA-SN-8350-1                                              |
|   2017  |   Study Number **8494 - English Housing Survey, 2017: Housing Stock Data**                                                                                                                                                                          |
|         |   Ministry of Housing, Communities and Local Government. (2019). English Housing Survey, 2017: Housing Stock Data. [data collection]. UK Data Service. SN: 8494, http://doi.org/10.5255/UKDA-SN-8494-1                                              |


----
### *myScript* structure

- `myScript/A__initialAnalysis.R` loads multiple EHS (raw) sources, converts them into *tibbles* and performs descriptive and survey analysis.

- `myScript/B__conversion-HSEMs__export.R` converts the chosen raw dataset into a format employed by HSEMs [see methodology].

    > Hughes, M., Armitage, P., Palmer, J., & Stone, A. (2012). Converting English Housing Survey Data for Use in Energy Models.

- `myScript/C__households` extracts detailed information regarding household members to be then used in the generation of profiles.

- `myScript/D__predictive.R` combines all the data sources into a single tables, with only common variables, to be used in predictive analysis (eg. random forests, j48, glm, ...)


### *myData* Structure

The EHS datasets are requested in `.stata` format, and need to be copied to `../myData/` (see below).

```sh
.
├── AuxiliaryData
│   ├── CHM-data
│   └── allissue
├── UKDA-6612-stata8
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   │   ├── derived
│   │   │   │   └── detailed
│   │   │   ├── fuel_poverty
│   │   │   ├── interview
│   │   │   ├── market_value
│   │   │   └── physical
│   │   ├── excel
│   │   ├── pdf
│   │   └── stataissue
│   └── stata8
│       ├── derived
│       │   └── detailed
│       ├── fuel_poverty
│       ├── interview
│       ├── market_value
│       └── physical
├── UKDA-6804-stata11
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   │   ├── derived
│   │   │   │   └── detailed
│   │   │   ├── fuel_poverty
│   │   │   ├── interview
│   │   │   ├── market_value
│   │   │   └── physical
│   │   ├── excel
│   │   └── pdf
│   └── stata11
│       ├── derived
│       │   └── detailed
│       ├── fuel_poverty
│       ├── interview
│       ├── market_value
│       └── physical
├── UKDA-7039-stata9
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   │   ├── derived
│   │   │   │   └── detailed
│   │   │   ├── fuel_poverty
│   │   │   ├── interview
│   │   │   └── physical
│   │   ├── excel
│   │   ├── pdf
│   │   └── stataissue
│   └── stata9
│       ├── derived
│       │   └── detailed
│       ├── fuel_poverty
│       ├── interview
│       └── physical
├── UKDA-7386-stata9
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   │   ├── derived
│   │   │   │   └── detailed
│   │   │   ├── fuel_poverty
│   │   │   ├── interview
│   │   │   └── physical
│   │   ├── excel
│   │   ├── pdf
│   │   └── stataissue
│   └── stata9
│       ├── derived
│       │   └── detailed
│       ├── fuel_poverty
│       ├── interview
│       └── physical
├── UKDA-7511-stata11
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   │   ├── derived
│   │   │   │   └── detailed
│   │   │   ├── interview
│   │   │   └── physical
│   │   ├── excel
│   │   └── pdf
│   └── stata11
│       ├── derived
│       │   └── detailed
│       ├── interview
│       └── physical
├── UKDA-7802-stata11
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   │   ├── derived
│   │   │   │   └── detailed
│   │   │   ├── interview
│   │   │   └── physical
│   │   ├── excel
│   │   └── pdf
│   └── stata11
│       ├── derived
│       │   └── detailed
│       ├── interview
│       └── physical
├── UKDA-8010-stata11
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   ├── excel
│   │   └── pdf
│   └── stata11
├── UKDA-8186-stata11
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   ├── excel
│   │   └── pdf
│   └── stata11
├── UKDA-8350-stata11
│   ├── mrdoc
│   │   ├── UKDA
│   │   ├── allissue
│   │   ├── excel
│   │   └── pdf
│   └── stata11
└── UKDA-8494-stata
    ├── mrdoc
    │   ├── UKDA
    │   ├── excel
    │   └── pdf
    └── stata
        └── stata11

```
---

 **NOTES**
 1. **myData** folder employs `symlinks` to a centralised database where the original repositories are previously retrieved from [UK-Data-Archive](https://www.ukdataservice.ac.uk/get-data.aspx).
