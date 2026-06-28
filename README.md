# Project: Parrotfish demonstrate little behavioral shift during marine heatwaves 
# Authors: Golanski et al. 2026 

## General Description

This project investigates the effects of Marine Heatwave (MHW) events on the behavior of parrotfish in the Eilat region of the Red Sea. The analysis is based on satellite-derived Sea Surface Temperature (SST) data (OISST) and long-term acoustic telemetry data from tagged fish.

The project's goal is to quantify changes in activity patterns, depth use, and movement of fish before, during, and after MHWs. It also aims to determine if the magnitude of the behavioral response is dependent on the characteristics of the MHW itself (e.g., its duration, intensity, and rate of onset).

## Directory Structure

```
.
├── code/                   # All R analysis scripts
├── data/
│   ├── Eilat climetology/  # Sea Surface Temperature (SST) data
│   ├── MHWs_def/           # Definitions of Marine Heatwave events
│   └── parrotfish data/    # Acoustic data and metadata for parrotfish
├── results/
│   ├── Detection Tests/    # Results of tests on fish detection during MHWs
│   ├── GAM/                # Saved GAMM model objects (R objects)
│   ├── Mean Models/        # Models based on periodic mean values
│   ├── MHW Characteristics lm/ # Linear models testing relationships with MHW characteristics
│   └── plots/              # Graphs and visual outputs of the analysis
└── Red_Sea_MHWs_Parrotfish.Rproj # RStudio project file
```

## Analysis Workflow

The analysis process is divided into several stages, represented by numbered scripts in the `code/` directory:

1.  **`01_getClimate.R`**: Downloads OISST data for the Eilat region from 1982-2022.
2.  **`02_define_MHWs.R`**: Identifies and defines MHW events from the SST data. The script produces MHW definitions using two methods: a fixed baseline and a detrended baseline.
3.  **`03_short_MHWs_fish_fitting.R`**: Performs an initial match between short MHW events (<31 days) and the available fish data. It identifies which fish were present before, during, and after each event. This stage also involves a manual, visual filtering step to remove fish with partial or problematic data. The filtered files are saved in `data/MHWs_def/MHWs_filtered/`.
4.  **`04_MHWs summary.R`**: A primary statistical analysis based on **periodic averages**. This script calculates mean behavioral metrics (activity, depth, displacement) for each fish in each stage (Before, During, After) and fits GLMMs to test for effects.
5.  **`05_stage_comparisons.R`**: Calculates the **change** in behavior (e.g., the difference in mean depth between "During" and "Before"). It then runs simple linear models to test if these changes are related to MHW characteristics (like intensity, duration, etc.), including a Principal Component Analysis (PCA) of the MHW metrics.
6.  **`06_GAMMs.R`**: The most complex and central analysis. This script uses the raw data (not averages) and fits `bam` (Big Additive Models) to model behavioral patterns across the diel cycle (`TimeOrdinal`) and how they change between MHW stages. This stage includes rigorous data filtering, AIC-based model selection, and autocorrelation checks.
7.  **`07_plots.R`**: Generates all figures for scientific publication, using the objects saved from the previous scripts.

## How to Run

1.  Open the `Red_Sea_MHWs_Parrotfish.Rproj` project file in RStudio.
2.  Run the scripts in the `code/` directory in numerical order (01 through 08).
3.  Note: Some scripts (like `06_GAMMs.R`) include interactive stops (`readline`) to inspect intermediate results. You will need to press Enter in the console to proceed.
4.  Note 2: In some scripts you need to choose if you conduct the analyses on the fixed or detrended baselines; read the documantation carefully!   

**Important Note:** A crucial part of the analysis (in script `03`) involves manual filtering. To reproduce the results exactly, you should use the pre-filtered files located in the `data/MHWs_def/MHWs_filtered/` directory.
