## Data Pre-processing for SOCRATES

**Latest data retrieval from ZENODO on October 28, 2024.**

### Manual Changes:

- **Zimbabwe**: Removed the second `studyDay` (from the two-day survey) from the `sday` file before loading the full survey data to avoid merging issues with participant data. Without this adjustment, the `sday` file merges with the contact data, leading to issues for participants who did not register contacts. Added survey year "2013" (variable `syear`).

- **Zambia and South Africa**: Added survey year 2011 (variable `syear`). Some surveys were erroneously registered with the date 13/12/1091. Assuming the weekday (Saturday) is correct, the correct date should likely be the 12th, with 13 intended as month 3 (March). Zambia's February 2011 data consistently spans at least two consecutive days, with no data for the 11th or 13th. Since SOCRATES focuses on the day of the week and does not perform longitudinal analysis, dates falling within the overall min-max range pose no issue.

- **France**: Updated dataset based on Béraud et al. (2015) with a clean imputation of supplementary professional contacts (SPC) using the methods of Hens et al. (2009). Included the new dataset, documented the methods in an Rmd (and HTML) file, and linked it within the SOCRATES tool.

- **Belgium 2010-2011**: Updated the dataset with a clean imputation of SPC using Hens et al. (2009) methods. This update is not yet aligned with the ZENODO data.

- **Belgium 2010 Household Study**: Added the variable `country` to the dataset.

- **UK 2018**: Adjusted the `country` from 'UK' to 'United Kingdom'.

- **"CoMix 2.0"**: Filtered files to include only those with a `.json` extension or containing "comix_2" to prevent issues with country-specific duplications.

- **Major Update of CoMix Datasets**: The latest ZENODO version resolved an issue with `part_id` in the contact data, which previously listed only the `part_id` of the last wave a participant joined. While average rates may not change significantly, wave-specific values show considerable variation.

- **Correction of Poland and Portugal CoMix Data**: Fixed a previous issue in SOCRATES where data for Poland and Portugal was switched, referencing descriptions from Verelst et al. (2021) for validation. The current labels in the ZENODO data are correct.

- **Belgian CoMix Data**: Merged data from `belgium2020_comix`, `comix_multi`, and `comix_update` to compile all 48 survey waves. Resolved age-range inconsistencies (see commit on the check function). Standardized panel notation: instances of "Panel B" and "Panel BC" on the same dates were merged to "Panel BC" for consistency across waves. Renamed `part_age_min` and `part_age_max` to `part_age_est_min` and `part_age_est_max`, respectively.

- **UK CoMix Data**: Included UK CoMix data after updating the `load_survey` function to address `row.names` in column `X`. Added the `country` variable to the UK CoMix survey and updated the year notation to use the full year format, changing 20, 21 and 22 to 2020, 2021 and 2022 respectively.

### Automated Data Selection/Standardization
- If `location` is not a standard category (i.e., "home", "work", "school", "transport", "leisure", "otherplace"), it is set to "otherplace".
- If a participant's age range is specified, the "exact" age is removed.

