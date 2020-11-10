---
output:
  html_document: default
---
# Platform updates

* [2020-10-05] Contacts whose ages are not given exactly will have by default their age sampled at random from the given range. If you want to use the midpoint, deselect "Age range: sample at random"

* [2020-10-14] Participants with reported contacts without age information are no longer removed by default, but these contacts are "ignored". If you want to remove participants with at least one missing contact age for the selected contact features, select "Missing contact age: remove participant".

* [2020-11-10] The decision whether to exclude participants with missing contact ages is now based on selection within the contact_matrix function from socialmixr, which first selects the contact types of interest before checking missing contact ages. Before, these participants were removed by SOCRATES irrespectively of the contact details. From now on, if for example a location type with missing ages is not selected, the participant is not removed.

* [2020-11-10] Updated the UN world population prospects version from the 2015 to the 2019 revision^(1)^.



<br></br>
<br></br>

<font size="1">
(1) With each revision of the World Population Prospects, the Population Division of UN DESA carries out a “re-estimation” of recent or historical demographic trends for many countries and areas of the world. These demographic estimates are based on the most recently available data sources, such as censuses, demographic surveys, registries of vital events, population registers and various other sources (e.g., refugee statistics). With each new data collection, the time series of fertility, mortality and migration, as well as population trends by age and sex, can be extended and, if necessary, corrected retrospectively. 
(https://population.un.org/wpp/Publications/Files/WPP2019_Methodology.pdf)
</font>