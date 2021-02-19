---
output:
  html_document: default
---
## General

SOCRATES is developped as part of a social contact data sharing initiative and assessment of mitigation strategies for COVID-19. The methods and some case studies are provided in:


[Willem L, Van Hoang T, Funk S, Coletti P, Beutels P, Hens N. SOCRATES: an online tool leveraging a social contact data sharing initiative to assess mitigation strategies for COVID-19. BMC Res Notes 13, 293 (2020).](https://doi.org/10.1186/s13104-020-05136-9)


## Major platform updates

* [v1.51 - 2020-12-22] Changed methods to calculate weighted, symmetric and per capita matrices. (1) The reference population data is not truncated anymore at the upper age limit of the participants. As such, the last age group contains ALL ages up to 105 years of age (=the default of the wpp package). This truncation was included to align the participant and referece population data, but was not consistent with the notation of the open-end age group. (2) The combined weights are standardized based on post-strafication weights and truncation of weights is done after standardisation (and followed by another standardisation). More documentation on the post-stratification weights is provided [here](https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_weights.pdf).

* [v1.45 - 2020-11-10] Updated the UN world population prospects version from the 2015 to the 2019 revision^(1)^.

* [v1.44 - 2020-10-14] Participants with reported contacts without age information are no longer removed by default, but these contacts are "ignored". If you want to remove participants with at least one missing contact age for the selected contact features, select "Missing contact age: remove participant".

* [v1.37 - 2020-10-05] Contacts whose ages are not given exactly will have by default their age sampled at random from the given range. If you want to use the midpoint, deselect "Age range: sample at random"

* [v1.30 - 2020-06-30] Updated the supplementary professional contacts (SPC) of the French dataset (Béraud 2015). [More info](https://github.com/lwillem/socrates_rshiny/blob/master/doc/doc_spc_france.md)

* [v1.16 - 2020-03-30] The decision whether to exclude participants with missing contact ages is now based on selection within the contact_matrix function from socialmixr, which first selects the contact types of interest before checking missing contact ages. Before, these participants were removed by SOCRATES irrespectively of the contact details. From now on, if for example a location type with missing ages is not selected, the participant is not removed.



<br></br>
<br></br>

<font size="1">
(1) With each revision of the World Population Prospects, the Population Division of UN DESA carries out a “re-estimation” of recent or historical demographic trends for many countries and areas of the world. These demographic estimates are based on the most recently available data sources, such as censuses, demographic surveys, registries of vital events, population registers and various other sources (e.g., refugee statistics). With each new data collection, the time series of fertility, mortality and migration, as well as population trends by age and sex, can be extended and, if necessary, corrected retrospectively. 
(https://population.un.org/wpp/Publications/Files/WPP2019_Methodology.pdf)
</font>
