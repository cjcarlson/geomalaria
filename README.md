# Malaria and geoengineering forecasts
Codebase accompanies: Carlson _et al._ (2022) "Solar geoengineering could redistribute malaria risk in developing countries." _Nature Communications_, accepted in principle.

# Raw files

There are several components for the pipeline (below) in the Raw folder; larger climate files are stored elsewhere[...add...] 

```
Raw
|
├─## These are falciparum / vivax prevalence files that go into Figures S9-S11
├─2020_GBD2019_Global_PfPR_2000.tif
├─2020_GBD2019_Global_PfPR_2019.tif
├─2020_GBD2019_Global_PvPR_2000.tif
├─2020_GBD2019_Global_PvPR_2019.tif
|
├─## These are the raw Bayesian thermal response curves used to generate the R0 maps 
├─map_Angambiae_Pfalc_08_12_20.csv
├─map_Angambiae_Pvivax_08_12_20.csv
├─map_Anstephensi_Pvivax_08_12_20.csv
```

# Directory of Code

There are three folders' worth of scripts to execute the entire pipeline in the paper. Two multi-step pipelines turn climate data into derived products (the R0 analysis and the population at risk, or PAR, analysis). All the code to produce visualizations in the paper is stored in a third paper.

```
Visualization
|
├─## THESE CAN BE RUN WITH RAW DATA
├─Raw Temperature Change G3.R -- generates Figure S5 and S6
├─Raw Temperature Change GLENS.R -- generates Figure S7 and S8
|
├─## REQUIRES OUTPUTS FROM "GENERATE R0" SCRIPTS
├─R0 Visualization - Figures 1 and 2.R -- generates Figures 1 and 2
├─Prevalence Visualization - supplemental figures.R -- generates Figures S9-S11
|
├─## REQUIRES OUTPUTS FROM "GENERATE PAR" SCRIPTS
└PAR Visualization - Figure 3 and others.R -- generates Figure 3 and Figure S1-S4
```

These scripts generate maps of R0(T) for 2020 and 2070 and generate the relevant figures:

```
Generate R0
|
├─## RUN THESE AS A BATCH FIRST
├─Current R0 GLENS LatAm.R
├─Current R0 GLENS Asia.R
├─Current R0 GLENS Africa.R
├─Current R0 G3 LatAm.R
├─Current R0 G3 Asia.R
├─Current R0 G3 Africa.R
|
├─## THEN THIS
├─Current R0 Consolidate.R
|
├─## RUN THESE AS A BATCH FIRST
├─Current R0 GLENS LatAm.R
├─Current R0 GLENS Asia.R
├─Current R0 GLENS Africa.R
├─Current R0 G3 LatAm.R
├─Current R0 G3 Asia.R
├─Current R0 G3 Africa.R
|
├─## THEN THIS
└─Future R0 Consolidate.R
```

These scripts generate population at risk tables. The pipeline was originally designed with GLENS files that began after 2020, and additional .nc layers were added after the fact to add just the year 2020; separate scripts generate these different timepoints. Two different scripts combine these tables into workable file formats, and then they're merged after the fact.

```
Generate PAR
|
├─## RUN THESE AS A BATCH 
├─PAR G3 LatAm.R 
├─PAR G3 Asia.R 
├─PAR G3 Africa.R 
|
├─## RUN THESE AS A BATCH 
├─PAR GLENS LatAm 2020.R 
├─PAR GLENS Asia 2020.R 
├─PAR GLENS Africa 2020.R 
|
├─## THEN THIS
├─PAR Rename and Consolidate 2020.R
|
├─## RUN THESE AS A BATCH
├─PAR GLENS LatAm 2030 to 2070.R 
├─PAR GLENS Asia 2030 to 2070.R 
├─PAR GLENS Africa 2030 to 2070.R 
|
├─## THEN THIS
└─PAR Rename and Consolidate 2030 to 2070.R
```
