## Supplementary Professional Contacts for Béraud et al (2015)

Original dataset: [Béraud et al. (2015) The French Connection: The First Large Population-Based Contact Survey in France Relevant for the Spread of Infectious Diseases. PLoS One 10(7).](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0133203)

Methods: [Hens, N., Goeyvaerts, N., Aerts, M. et al. Mining social mixing patterns for infectious disease models based on a two-day population survey in Belgium. BMC Infect Dis 9, 5 (2009).](https://doi.org/10.1186/1471-2334-9-5)

### Contact survey
A threshold value was set at 20 per day for contacts made at work, in order to reduce reporting bias for individuals with a high number professional contacts such as for example a bus driver. If participants had more than 20 professional contacts, they were asked not to report them individually but to indicate the number of these supplementary professional contacts (SPC) and their age distribution (0-2y, 3-10y, 11-17y, 18-64y, 65-99y). 

### Censoring
We applied censoring to the SPC, considering they followed a negative binomial distribution, in order to retain 95% of the SPC. This 95% boundary results in censoring at a maximum of 134 SPC per participant and per day

### Imputation
SPC were defined when participant i reported having n<sub>i</sub><sup>w</sup> (>20) contacts at work made in a specific set of age-categories I<sub>i</sub><sup>a</sup>. We used the gender, duration of contact and whether the contact involved skin-to-skin touching of the reported contacts at work when 10 <  n<sub>i</sub><sup>w</sup> < 20 as a basis for imputation. This set of contacts was resampled with probabilities according to the reported age distribution, taking into account the French population age structure in 2012 (INSEE) and implemented into the data set when the day of study was a weekday.

### Extraction from Methods section from Hens et al (2009)
In order to impute the other contact characteristics, plausible assumptions were made based on the available information from other POLYMOD countries. Mossong et al (2008) did not find a significant association between age and whether contacts involved skin-to-skin touching for EW, IT, LU and PL. Therefore we imputed, this variable in the Belgian dataset using the same distribution as when 10 <  n<sub>i</sub><sup>w</sup> ≤ 20. The imputation of gender of contacts was performed based on the same reasoning. For the imputation of contact characteristics like duration and usual frequency, we considered it unlikely that a single professional contact in a large set of such contacts would last longer than 4 hours and reoccur daily. Therefore, we imputed these two variables jointly by sampling from the bivariate distribution of duration and frequency for work contacts of participants for whom 10 < n<sub>i</sub><sup>w</sup> ≤ 20. This method could also be more widely applied to all characteristics in an attempt to avoid disrupting dependencies, but could just the same also enforce dependencies. Whereas the imputation for age and gender was well founded (negligible change in distribution with n<sub>i</sub><sup>w</sup>), other imputations seemed more speculative. Indeed, the higher the recorded n~i~^w^ was, the shorter the contact duration were, but the distribution of contact frequency remained quite stable. Furthermore, the choice of distribution for the duration of contacts would be subjective since (a) it was clearly unknown for what was missing and (b) the distributions varied substantially between countries.



