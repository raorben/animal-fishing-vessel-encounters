# animal-fishing-vessel-encounters
R scripts to extract animal-fishing vessel encounters using the Global Fishing Watch datasets and animal tracking data. 


[![DOI](https://zenodo.org/badge/221094317.svg)](https://zenodo.org/badge/latestdoi/221094317)
![GitHub tag (latest
SemVer)](https://img.shields.io/github/tag/raorben/animal-fishing-vessel-encounters.svg?color=blue&label=Version)

Cite as: Orben, R.A. and L.G. Torres 2021. animal-fishing-vessel-encounters. Version 1.0 GitHub
repository. <https://github.com/raorben/animal-fishing-vessel-encounters>
<DOI:10.5281/zenodo.221094317>



The script series "lowres" is intended to overlay animal tracking data on gridded Global Fishing Watch data to produce potential overlap events (daily scale, with user defined radius). The script series "highres" does the same with the higher resolution dataset. These are the most transferable to other projects. 

The scripts are from:

Orben RA, LG Torres, J Adams, M Hester, SA Shaffer, MG Conners, L Young K Ozaki, F Sato, T Deguchi, RM Suryan, D Koordesma. 2021. Across borders: External factors and prior behavior influence North Pacific albatross associations with fishing vessels. Journal of Applied Ecology. 

These scripts represent the framework for the dataproccessing and analysis methods applied in the paper. Please don't expect them to be fully operational without some fiddling. Rather they are here to provide specific documentation of the methods used and to be a resource for future analysis. 

The scripts are arranged in "series" and then numbered sequentially. 

AlbatrossGPS: scripts to compile, interpolate, and process animal tracking data. 

EncounterInter: scripts to match AIS data with animal tracking data

EnvironmentalData_Extraction: script to extract environmental data along tracks

BRT_models: scripts to run boosted regression tree models. Note - models were run with the full suite of variables, results saved, and models re-run. The succesive model runs were not documented within the scripts 

