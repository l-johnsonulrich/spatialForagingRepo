# Spatial Foraging Repository

This repository contains the R scripts and data associated with the manuscript:

**"Spatial position relative to group members affects weight gain in meerkats (Suricata suricatta)"**

*Authors:* Rasekuwane Mosia, Vlad Demartsev, Aliza le Roux, Marta B Manser, Ariana Strandburg-Peshkin, and Lily Johnson-Ulrich.

The study investigates the relationship between spatial position within foraging meerkat groups and foraging success, using a combination of high-resolution GPS tracking and weight measurements to analyze spatial dynamics and their effects on individual weight gain.

## Repository Overview

### Manuscript Link
The full manuscript is available upon request. The data and R scripts in this repository directly support the results and analyses presented in the manuscript.

### Scripts

The numbered R scripts in this repository are organized to follow the logical flow of data processing and analysis:

0. **`0_Computer_spatial_metrics.R`**: Prepares and cleans raw data for analysis. This includes importing GPS level1 data and calculating default spatial metrics over a 10 meter discretization every second. 
1. **`1_SpatialPosition.R`**: Calculates spatial positions relative to the group (front, back, side, and center) using group centroid data and heading vectors and extracts addition spatial positions (binary front vs back and ranked position).
2. **`2_ID_codes_filtering_script.R`**: Creates a list of meerkat ID codes from the long-term data.
3. **`3_R_Weight_differences`**: Uses long-term weight data to calculate weight gain between morning and lunch weights for all meerkat groups used during the study periods. 
4. **`4_Combine_Weight_Position.R`**: This script combines weight data from 03_Output with position data from 01_Output resulting in the main dataframe used for analysis with 195 rows.
5. **`5_Stats.R`**: Performs repeatability analyses to test consistency in individual spatial positions. Models the relationship between spatial position and weight gain using generalized linear mixed models (GLMMs). Generates visualizations, including violin plots and slope graphs, to illustrate the key findings of the study.

### Data

Data files used in the analyses include:
- **GPS data**: High-resolution tracking of meerkat movements within groups. Use of these data is restricted and requires explicit permission from the authors.
- **Weight data**: Individual weight measurements before and after foraging periods. Use of these data is restricted and requires explicit permission from the authors.
- **Metadata**: Includes group composition, individual traits (age, sex, rank), and other contextual information. Use of these data is restricted and requires explicit permission from the authors.

### Output
Key outputs from the scripts include:
- 04_Output/positionAndWeight20241206.csv is the data file used for analysis in the manuscript. This file may be used only for the purpose of reviewing or validating the results of the study. 
- Model results for the effects of spatial position on weight gain.
- Figures illustrating group dynamics and spatial positioning.

## Installation and Setup

1. **Clone the Repository**
   ```bash
   git clone https://github.com/l-johnsonulrich/spatialForagingRepo.git
   cd spatialForagingRepo
   ```

2. **Install Dependencies**
   Ensure you have R installed with the following packages:
   - `glmmTMB`
   - `emmeans`
   - `rptR`
   - `modelbased`
   - `ggplot2`
   - `dplyr`
   - `tidyr`

   Install missing packages using:
   ```R
   install.packages(c("glmmTMB", "emmeans", "rptR", "modelbased", "ggplot2", "dplyr", "tidyr"))
   ```

3. **Run the Scripts**
   Execute the scripts in the following order to replicate the analyses:
   ```bash
   Rscript 00-Computing_spatial_metrics
   Rscript 01_SpatialPosition_Update_23.09.04
   Rscript 02_ID_codes_filtering_script
   Rscript 03_R_Weight_differences_(Group_Weights).R
   Rscript 04_Combine_Weight_Position
   Rscript 05_Stats.R
   ```

## Results

The analyses demonstrate:
- Significant individual differences in spatial positioning within foraging groups.
- Effects of spatial position on weight gain, particularly for younger and dominant female meerkats.
- Visualizations of spatial distributions and weight gain trends.

## Citation

If you use this repository or its contents in your work, please cite the manuscript:

> Mosia, R., Demartsev, V., le Roux, A., Manser, M. B., Strandburg-Peshkin, A., & Johnson-Ulrich, L. (2024). Spatial position relative to group members affects weight gain in meerkats (Suricata suricatta).

## License

This repository uses a dual-license model:

1. **Code**: Licensed under the MIT License. See the `LICENSE_CODE` file for details.
2. **Data**: The data in this repository is protected and may only be used with explicit permission from the authors. See the `LICENSE_DATA` file for details.

## Contact

For questions or further information, please contact:

Lily Johnson-Ulrich  
University of Zurich  
Department of Evolutionary Biology and Environmental Studies  
Email: l.johnsonulrich@gmail.com

