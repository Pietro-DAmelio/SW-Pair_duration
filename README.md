# SW-Pair_duration

#Data and code for the article:

Influence of pair duration on reproduction under different nest predation pressures in a life-long monogamous cooperative passerine

by:
D’Amelio B. Pietro, Covas Rita, Ferreira C. André, Fortuna Rita, Silva R. Liliana, Theron Franck, Rybak Fanny, Doutrelant Claire


sessionInfo()

R version 4.1.2 (2021-11-01) Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale: [1] LC_COLLATE=English_United States.1252 LC_CTYPE=English_United States.1252
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C
[5] LC_TIME=English_United States.1252


GENERAL INFORMATION

1. Title of Dataset: Data from: Influence of pair duration on reproduction under different nest predation pressures in a life-long monogamous cooperative passerine

2. Author Information
	A. Principal Investigator Contact Information
		Name: Pietro D'Amelio	
		Institution: Centre d’Ecologie Fonctionnelle et Evolutive, CNRS, Univ Montpellier, EPHE, IRD
		Address: 1919 Rte de Mende, Montpellier, France
		Email: pie.damelio@gmail.com

	B. Associate or Co-investigator Contact Information
		Name: Rita Covas
		Institution: CIBIO-InBIO - Research Centre in Biodiversity and Genetic Resources
		Address: Campus Agrário de Vairão, 4485–661 Vairão, Portugal
		Email: rita.covas@cibio.up.pt

3. Date of data collection (single date, range, approximate date):   from seasons 2011/2012 to 2018/2019

4. Geographic location of data collection: Benfontein Nature Reserve, Northern Cape Province, South Africa (28°520 S, 24°500 E)

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: ODbL

2. Recommended citation for this dataset: D'Amelio, Pietro B et al. (2023), Influence of pair duration on reproduction under different nest predation pressures in a life-long monogamous cooperative passerine, Dataset (link to be added after acceptance and uploading to final location)


DATA & FILE OVERVIEW

1. File List: 
longitudinal_dataset.csv
cross-sectional_dataset.csv

2. Relationship between files, if important: all IDs present in the cross-sectional dataset are present in the longitudinal, the structure differs (see methods)

3. Are there multiple versions of the dataset? No

METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: See Fortuna et al. (2021), D'Amelio et al. (2022), methods of this paper and supplementary material (all freely available)

2. Methods for processing the data: 
<describe how the submitted data were generated from the raw or collected data>
The data presented here are similar to the raw collected data, they are only simplified and summarized

DATA-SPECIFIC INFORMATION FOR: longitudinal_dataset.csv
  
1. Number of variables: 72

2. Number of cases/rows: 13968 rows

3. Variable List: 
Note that a specific selection of the variables for each analysis is made at the beginning of each script (see scripts "data handling" section), so the length and structure of each analysis differs accordingly.
One row per egg. The format is 'Column name - variable name as in the manuscript (if present) - description of the variable'.

BreederMom - Mother ID - ID of the female that laid each egg (modified from the raw dataset)
BreederDad - Father ID - ID of the male that fathered the clutch (modified from the raw dataset)
ColSize - Colony size - Number of birds captured in that colony in previous winter
Nest - Nest ID - ID of the Nest from which that egg belongs to
colony - Colony ID - ID of the colony where the female was breeding
season - Season - breeding season during which the clutch was laid
Laying_date- When the first egg was layed
Egg_num-  ID of the clutch within the clutch
Ring- ID of the chick (modified from the raw dataset)

4. Missing data codes: NA

5. Specialized formats or other abbreviations used:
  
