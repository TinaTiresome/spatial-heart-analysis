# spatial-heart-analysis
This is the repository for the data analysis of my Bachelor's project in the field of Human Geography and Urban Planning. Specifically, under the umbrella theme of Demography and Healthy Ageing.
## structure
spatial-heart-analysis/                                                                        
  ├── data/                                                                                                                      
  │   ├── data_import/                                                                                                  
  │   │    ├──  imported_data.RData                                                                                                
  │   │    └──  shapefile_data.RData                                                                                
  │   └── processed_data/                                                                          
  │    ..      ├──  data.RData                                                              
  │    ..      └──  data_unbalanced.RData                                                            
  ├── outputs/                                                                                                              
  │   ├── maps/                                                                                                                                                                                      
  │   ├── graphs/                                                                                                                                                                                         
  │   └── results/                                                           
  ├── scripts/                                                                                                                                                                                                       
  │   ├── 1 data import.R                                                                                                                                                                                      
  │   ├── 2 data transformations.R                                   
  │   ├── 3 pooled OLS.R                                                  
  │   ├── 4 other data analysis.R                                              
  │   ├── 5 data visualisation and mapping.R                                     
  │   └── 6 tidy base model analysis with functions.R                                                                                                                                                                         
  └── README.md                                       

## Data Sources
### CBS - Proximity to Amenities by Municipality
CBS StatLine [2013-2018] Proximity to amenities; distance location, regional figures. 
Dataset: 80305ENG https://opendata.cbs.nl/statline/#/CBS/en/dataset/80305ENG/table?ts

### CBS - Persons with dispensed medicines by Municipality
CBS StatLine [2013-2018] Personen met verstrekte geneesmiddelen; regio (gemeente). 
Dataset: 83251NED https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83251NED/table

### CBS - Regional Key Figures by Municipality
CBS StatLine [2013-2018] Regionale kerncijfers Nederland.
Dataset: 70072NED https://opendata.cbs.nl/statline/#/CBS/nl/dataset/70072ned/table

### CBS - Measures Financial Relations Act Dataset
CBS StatLine [2013-2018] Maatstaven Financiële-verhoudingswet (Fvw), regio, 2007 - 2023.
Dataset: 60039FVW https://opendata.cbs.nl/statline/#/CBS/nl/dataset/60039fvw/table

### CBS and PDOK - Administrative Boundaries as Shapefiles from PDOK webservice API
CBS and PDOK [2014-2018] Municipal Boundaries. Dataset: CBS Gebiedsindelingen.       
https://www.pdok.nl/introductie/-/article/cbs-gebiedsindelingen 
                                                                                                                                                                                                                                               
