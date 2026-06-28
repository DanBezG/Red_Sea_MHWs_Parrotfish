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

## Key Variables and Columns

Throughout the analysis, several key identifiers and calculated metrics are generated. Understanding them is crucial for interpreting the data and results.

### In MHW Definition Files (`data/MHWs_def/MHWs_filtered/`)

In the `Short_MHW_Events` sheet within the MHW definition Excel files:

*   **`start_1week`, `end_1week`, `start_1.5`, `end_1.5`, `start_3`, `end_3`**:
    *   These columns define the "before" and "after" analysis windows surrounding each MHW.
    *   The time windows are not a simple fixed duration. The logic in script `03` truncates these periods to ensure they **do not overlap** with adjacent MHW events. This is a critical methodological detail for ensuring the independence of the "before" and "after" periods.
*   **`Fish_IDs`**:
    *   A comma-separated list of fish ID numbers that were deemed suitable for analysis for that specific MHW event, having passed the initial filtering criteria (i.e., presence during the MHW and in at least one of the before/after periods).

### In Processed Results Files (e.g., in `results/Mean Models/` or `results/MHW Characteristics lm/`)

*   **`Serial`**:
    *   A sequential numeric ID (1, 2, 3...) assigned to each MHW event within a specific analysis run.
    *   
*   **`Serial_fish_id`**:
    *   A concatenated identifier created by joining `Serial` and `fish_id` (e.g., "3_1255800").
    *   This is the most critical unique identifier at the observation level. It represents a **single specific fish in the context of a single specific MHW event**. Since the same fish can experience multiple MHWs, `fish_id` alone is not sufficient to uniquely identify a behavioral response.
*   **`mean_disp_max`**:
    *   A calculated metric for the daily spatial range of a fish. As defined in script `04`, it is the difference between the 5% most distant `distance_shore` observations and the 95% closest observations during daylight hours.
    *   It is a derived metric, not a direct observation, designed to represent the typical daily movement range.

### In the GAMM Analysis (`results/GAM/`)

*   **`TimeOrdinal`**:
    *   A continuous numeric variable (ranging from 1 to 5) representing the time of day
    *   This is not simply the hour of the day. As generated in script `06`, `TimeOrdinal` is **normalized to the length of the solar day**. For example, the period between sunrise and solar noon is always mapped to the numeric range [2, 3], regardless of whether it's a long summer day or a short winter day.
    *   This allows the GAMM models to correctly fit the **shape** of the diel (24-hour) behavioral cycle and compare it across different seasons and MHW stages, independent of the changing day length.

## Usage and Citation

The data and code provided in this repository are open-access. If you use these materials for your own research or meta-analyses, please cite the original associated publication:

Golanski, D., et al. (2026). Parrotfish demonstrate little behavioral shift during marine heatwaves. Biology Letters

For questions or issues regarding the data or code, please contact the corresponding author at: dangolanski712@gmail.com
