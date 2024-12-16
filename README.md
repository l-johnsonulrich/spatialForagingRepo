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

1. **`1_data_preprocessing.R`**: Prepares and cleans raw data for analysis. This includes importing GPS data, cleaning weight records, and standardizing variables.
2. **`2_spatial_position_calculations.R`**: Calculates spatial positions relative to the group (front, back, side, and center) using group centroid data and heading vectors.
3. **`3_repeatability_analysis.R`**: Performs repeatability analyses to test consistency in individual spatial positions.
4. **`4_weight_gain_analysis.R`**: Models the relationship between spatial position and weight gain using generalized linear mixed models (GLMMs).
5. **`5_visualizations.R`**: Generates visualizations, including violin plots and slope graphs, to illustrate the key findings of the study.

### Data

Data files used in the analyses include:
- **GPS data**: High-resolution tracking of meerkat movements within groups. Use of these data is restricted and requires explicit permission from the authors.
- **Weight data**: Individual weight measurements before and after foraging periods. Permission is required to use these data.
- **Metadata**: Includes group composition, individual traits (age, sex, rank), and other contextual information. Permission is required to use these data.

### Output
Key outputs from the scripts include:
- Summaries of spatial position metrics (front, back, side, center).
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
   Rscript 1_data_preprocessing.R
   Rscript 2_spatial_position_calculations.R
   Rscript 3_repeatability_analysis.R
   Rscript 4_weight_gain_analysis.R
   Rscript 5_visualizations.R
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

