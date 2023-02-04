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

3. Date of data collection (single date, range, approximate date):   from seasons 2008/2009 to 2018/2019

4. Geographic location of data collection: Benfontein Nature Reserve, Northern Cape Province, South Africa (28°520 S, 24°500 E)

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: ODbL

2. Recommended citation for this dataset: D'Amelio, Pietro B et al. (2023), Influence of pair duration on reproduction under different nest predation pressures in a life-long monogamous cooperative passerine, Dataset (link to be added after acceptance and uploading to final location)


DATA & FILE OVERVIEW

1. File List: 
Longitudinal_dataset.csv
Cross-sectional_dataset.csv

2. Relationship between files, if important: all IDs present in the cross-sectional dataset are present in the longitudinal, the structure differs (see methods)

3. Are there multiple versions of the dataset? No

METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: See Fortuna et al. (2021), D'Amelio et al. (2022), methods of this paper and supplementary material (all freely available)

2. Methods for processing the data: 
<describe how the submitted data were generated from the raw or collected data>
The data presented here are similar to the raw collected data, they are only simplified and summarized

DATA-SPECIFIC INFORMATION FOR: Longitudinal_dataset.csv
  
1. Number of variables: 67

2. Number of cases/rows: 15660 rows (including headers)

3. Variable List: 
	
Note that a specific selection of the variables for each analysis is made at the beginning of each script (see scripts "data handling" section), so the length and structure of each analysis differs accordingly.
	
One row per egg. The format is 'Column name - variable name as in the manuscript (if present) - description of the variable'.

	
Season - Season - breeding season during which the clutch was laid
	
Colony - Colony ID - ID of the colony where the egg was layed
	
Nest - Nest ID - ID of the nest where the egg was layed
	
Laying_date - When the first egg of the clutch was layed
	
Egg_num -  ID of the egg within the clutch

unique_ID - merging the info of the columns "Colony", "Nest", "Laying_date" and, "Egg_num" we can create a unique identifier for each row
	
Fate - What happened to each egg/chick
	
Ring- ID of the chick (modified from the raw dataset)
	
ColNestLaying - unique identifier for each cluch
	
BreederMom - Mother ID - ID of the female that laid each egg (modified from the raw dataset)
	
BreederDad - Father ID - ID of the male that fathered the clutch (modified from the raw dataset) - see suplementary material for the assignment of extra pair chicks
	
predation - Treatment ("natural", "protected") - whether at the moment of laying and during rearing the colony was protected from nest predation from snakes or not
	
double_protection_state - whether the colony was protected for the full season or only for part of it
	
season_of_protection - For how many consecutive seasons was the colony protected
	
Mom_MinAge - Female Age - Minimum age of the female in days at the moment of laying
	
Dad_MinAge - Male Age - Minimum age of the male in days at the moment of laying
	
Mom_Min_Seasons - Female Age - Minimum age of the female in number of seasons at the moment of laying

Dad_Min_Seasons - Male Age - Minimum age of the female in number of seasons at the moment of laying
	
Pair - Pair ID - Column merging "BreederMom" and "BreederDad" when both are present
	
total_pair_duration_nogaps - total pair duration - final pair duration (in season) of the given pair
	
breeding_exp_pair_nogaps - Pair duration - Duration (in season) of the pair at the moment of laying 
	
breeding_exp_pair_days - Pair duration - Duration (in days) of the pair at the moment of laying
	
breeding_exp_pair_days_season - Pair duration (used for breeding offset) - Duration (in days) of the pair at the moment of the last laying of the season
	
breeding_exp_pair_days_season_earliest - Pair duration (used for breeding onset) - Duration (in days) of the pair at the moment of the earliest laying of the season
	
pair_breeding_attempt_ordinal - the ordinal number of the breeding attempt of the pair (across seasons)
	
julian_date_first - breeding (laying) onset - number of days since season's first laying of the population 
	
julian_date_last - breeding (laying) offset -  difference in the number of days between the pair’s last laying date and the end of the season (the last laying of the population)
	
tarsus_mom - tarsus + intertarsal joint - measure in mm of the tarsus + intertarsal joint of the female (mother)

tarsus_dad - tarsus + intertarsal joint - measure in mm of the tarsus + intertarsal joint of the male (father)

N_of_life_partners_Mom - Number of partners - Total number of partners that the female had in her reproductive life
	
N_of_life_partners_Dad - Number of partners - Total number of partners that the male had in his reproductive life
	
tot_fitness - number of fledglings (chicks) - number of fledglings produced by that specific pair in that given season
	
N_EPP_chicks - number of extra pair paternity fledglings produced by that specific pair in that given season 
	
N_eggs_season - Number of eggs - number of eggs produced by that specific pair in that given season

breeding_exp_Mom_nogaps - Female breeding experience - Breeding experience (in season) of the female at the moment of laying 

breeding_exp_Mom_days - Female breeding experience - Breeding experience (in days) of the female at the moment of laying
	
breeding_exp_Mom_days_season - Female breeding experience (used for breeding offset) - Duration (in days) of the female breeding experience at the moment of the last laying of the season
	
breeding_exp_Mom_days_season_earliest	- Female breeding experience (used for breeding onset) - Duration (in days) of the female breeding experience at the moment of the first laying of the season
	
Mom_breeding_attempt_ordinal  - the ordinal number of the breeding attempt of the female (across seasons)
	
Mom_breeding_attempt_ordinal_season - the ordinal number of the breeding attempt of the female (within each seasons)
	
breeding_exp_Dad_nogaps - male breeding experience - Breeding experience (in season) of the male at the moment of laying 

breeding_exp_Dad_days - male breeding experience - Breeding experience (in days) of the male at the moment of laying
	
breeding_exp_Dad_days_season - male breeding experience (used for breeding offset) - Duration (in days) of the male breeding experience at the moment of the last laying of the season
	
breeding_exp_Dad_days_season_earliest	- male breeding experience (used for breeding onset) - Duration (in days) of the male breeding experience at the moment of the first laying of the season
	
Dad_breeding_attempt_ordinal - the ordinal number of the breeding attempt of the male (across seasons) 
	
breeding_exp_Mom_tot - Total breeding experience (in seasons) the female
	
breeding_exp_Dad_tot - Total breeding experience (in seasons) the male
	
Min_Age_Mom_season_days - Female age (used for breeding offset) - Minimum age of the female in days at the moment of the last laying of the season
	
Min_Age_Mom_season_days_earliest - Female age (used for breeding onset) - Minimum age of the female in days at the moment of the first laying of the season 
	
Min_Age_Dad_season_days - male age (used for breeding offset) - Minimum age of the male in days at the moment of the last laying of the season
	
Min_Age_Dad_season_days_earliest - male age (used for breeding onset) - Minimum age of the male in days at the moment of the first laying of the season 

Final_clutch_size - Cluch size - number of eggs layed in the nest
	
N_Clutches_Pair - Number of clutches - number of clutches produced by that specific pair in that given season
	
N_Clutches_Mom - number of clutches produced by that specific female in that given season 
	
N_Clutches_Dad - number of clutches produced by that specific male in that given season
	
egg_mass - Egg mass - The mass of the egg, in grams
	
N_hatched - Brood size - Number of chicks that hatched in the clutch (measure of intranest competition)
	
succ - binary score for successful fledging from laying 
	
hatch_succ - binary score for successful hatching from laying (hatchability)
	
succ_from_hatch - fledging success/nestling survival - binary score for successful fledging from hatching
	
fledging_mass - fledging mass - mass of the chick, in grams, at day 17
	
wing_chick - length of the wing in mm
	
ringer - who ringed and measured the chick
	
Fate_clutch - fate of the clutch to be used in the relaying analysis (if any chick fledged --> "successful", if the last alive of the clutch died as chicks --> "failed_chicks", if all failed as eggs --> "failed_eggs")
	
Relaying_int - relaying interval - Number of days between 2 breeding attempts
	
ColSize - Colony size - Number of birds captured in that colony at the end of the winter
	
Group_Size - Group size - number of birds contributing to raising that specific clutch (see methods and supporting information) for how it's calculated
	


	
	
	
4. Missing data codes: NA
	
	

5. Specialized formats or other abbreviations used:









DATA-SPECIFIC INFORMATION FOR: Cross-sectional_dataset.csv
  
1. Number of variables: 61

2. Number of cases/rows: 749 rows (including headers)

3. Variable List: 
	
Note that a specific selection of the variables for each analysis is made at the beginning of each script (see scripts "data handling" section), so the length and structure of each analysis differs accordingly.

Variables description is the same as Longitudinal_dataset, but instead of males and females there is Focal and Mate

Focal--> the individual of a given breeding experience

Mate--> the mate of the focal for that specific breeding attempt.

Differing from Longitudinal_dataset is the column "ring"

ring - ID of the focal bird

In addition to the Longitudinal_dataset is the column "sex"

sex - sex of the focal bird ("1" is males, "2" is females)

4. Missing data codes: NA
	
	

5. Specialized formats or other abbreviations used:
  
