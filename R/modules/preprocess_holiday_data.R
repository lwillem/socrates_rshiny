#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => PREPROCESS AND SAVE COUNTRY-SPECIFIC HOLIDAY DATA
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________


# clear workspace
rm(list=ls())

## SET HOLIDAY DATES ####

#1a. Belgium: March-May 2006 (POLYMOD)
#1b. Belgium: 2010-2011 (Flemish survey)
# Hens et al: https://www.ncbi.nlm.nih.gov/pubmed/19943919
# Willem et al (...)
data.frame(iso3 = 'BEL',
           date = c(seq(as.Date("2006-02-27"), as.Date("2006-03-03"), by="days"), # Winter holidays
                    seq(as.Date("2006-03-03"), as.Date("2006-03-17"), by="days"), # Spring time/Easter holidays
                    as.Date("2006-05-01"),   # Public holidays
                    as.Date("2006-05-05"),   # Public holidays
                    as.Date("2006-05-25"),   # Public holidays
                    #2010-2011
                    seq(as.Date("2010-10-30"), as.Date("2010-11-07"), by="days"), # Fall break
                    as.Date("2010-11-11"),   # Public holiday
                    seq(as.Date("2010-12-25"), as.Date("2011-01-09"), by="days")) # Winter break
) -> holiday_BEL

#TODO: add 2020 holidays

#2. Germany: January-February, May-July 2006
#https://www.feiertagskalender.ch/ferien.php?geo=3062&jahr=2006&klasse=0&hl=en
data.frame(iso3='DEU',
           date = c(seq(as.Date("2005-12-21"), as.Date("2006-01-07"), by="days"),  #Christmas holidays
                    seq(as.Date("2006-01-30"), as.Date("2006-02-03"), by="days"),  #Winter holidays 
                    seq(as.Date("2006-04-10"), as.Date("2006-04-21"), by="days"),  #Easter holidays
                    seq(as.Date("2006-05-25"), as.Date("2006-05-26"), by="days"),  #Ascension Days
                    as.Date("2006-05-26"),                                         #Bank holiday  
                    as.Date("2006-07-05"),                                         #Bank holiday  
                    seq(as.Date("2006-07-06"), as.Date("2006-08-19"), by="days")) #Summer holiday 
) -> holiday_DEU

#3.Finland: March-June 2006
data.frame(iso3='FIN',
           date = c(seq(as.Date("2006-02-20"), as.Date("2006-03-11"), by="days"), #Winter holidays
                    seq(as.Date("2006-04-13"), as.Date("2006-04-17"), by="days"), #spring/easter holidays
                    seq(as.Date("2006-06-03"), as.Date("2006-08-15"), by="days"), #Summer holidays
                    as.Date("2006-05-01"),  # Public holidays
                    as.Date("2006-05-05"),  # Public holidays
                    as.Date("2006-05-25"))  # Public holidays
) -> holiday_FIN

#4.UK: April-May 2006: England
data.frame(iso3='GBR',
           date = c(seq(as.Date("2006-04-03"), as.Date("2006-04-21"), by="days"), #Spring time/easter holidays
                    as.Date("2006-05-01")) #public holiday
) -> holiday_GBR

#5.Italy: May-June 2006
data.frame(iso3='ITA',
           date = c(as.Date("2006-05-01"),as.Date("2006-06-02")) #public holiday
) -> holiday_ITA

#6.Luxembourg: May 2005,January-March, May 2006
data.frame(iso3='LUX',
           date = c(seq(as.Date("2005-05-14"), as.Date("2005-05-21"), by="days"), #???
                    as.Date("2005-05-01"), # public holiday 2005
                    as.Date("2005-05-05"), # public holiday 2005
                    seq(as.Date("2005-12-24"), as.Date("2006-01-08"), by="days"), #Christmast holidays
                    seq(as.Date("2006-02-25"), as.Date("2006-03-05"), by="days"), #Winter holidays
                    as.Date("2006-05-01"), #public holiday 2006
                    as.Date("2006-05-05"), #public holiday 2006
                    as.Date("2006-05-25")) #public holiday 2006
) -> holiday_LUX

#7.The Netherlands: February-September2006:South
data.frame(iso3='NDL',
           date = c(seq(as.Date("2006-02-25"), as.Date("2006-03-05"), by="days"), #Winter holidays 
                    seq(as.Date("2006-04-29"), as.Date("2006-05-07"), by="days"), #3rd term
                    seq(as.Date("2006-07-01"), as.Date("2006-09-03"), by="days"), #Autumn holidays 2005
                    as.Date("2006-04-17"), #public holiday 2006
                    as.Date("2006-04-30"), #public holiday 2006
                    as.Date("2006-05-05"), #public holiday 2006
                    as.Date("2006-05-25"), #public holiday 2006
                    as.Date("2006-06-05")) #public holiday 2006
) -> holiday_NDL
  
#8.Poland: March-April 2006: South
data.frame(iso3='POL',
           date = c(seq(as.Date("2006-04-13"), as.Date("2006-04-18"), by="days")) #spring time/Easter holidays 
) -> holiday_POL



###Hong kong: Jan 2015 - March 2016
#https://www.sis.edu.hk/school-terms-holidays-201516-calendar/
data.frame(iso3='POL',
           date = c(as.Date("2015-01-01"), #new year 2015
                    seq(as.Date("2015-02-16"), as.Date("2015-02-20"), by="days"), # lunar new year 2016 
                    seq(as.Date("2015-03-30"), as.Date("2015-04-03"), by="days"), #Breaks
                    seq(as.Date("2015-06-27"), as.Date("2015-08-15"), by="days"), #Summer holiday
                    as.Date("2016-01-01"), #new year 2016
                    seq(as.Date("2016-02-08"), as.Date("2016-02-12"), by="days"), #lunar new year 2016
                    seq(as.Date("2016-03-29"), as.Date("2016-04-07"), by="days")) # Breaks 
) -> holiday_HKG


###Peru: June 2011 - 07 December 2011 :https://www.amersol.edu.pe/uploaded/documents/2019-2020/Calendar/FDR_Academic_Year_Calendar_19-20.pdf
#https://www.amersol.edu.pe/uploaded/documents/2018-19/Calendar/Calendar_2018-2019.pdf

data.frame(iso3='PER',
           date = c(as.Date("2011-06-29"),  # St. Peter & St. Paul
                    seq(as.Date("2011-07-01"), as.Date("2011-07-29"), by="days"), #  term break
                    as.Date("2011-08-30"),  # No School - Santa Rosa de Lima
                    seq(as.Date("2011-10-03"), as.Date("2011-10-07"), by="days"), #  october break
                    as.Date("2011-11-01"),  # all Saints's day
                    seq(as.Date("2011-11-24"), as.Date("2011-11-25"), by="days")) #  thanks giving day (the fourth Thursday of November)
) -> holiday_PER

###Zimbawee: March 2013 - September 2013:  https://publicholidays.africa/zimbabwe/school-holidays/2018-dates/ (Consulted Tapiwa)
data.frame(iso3='PER',
           date = c(seq(as.Date("2013-04-01"), as.Date("2013-05-01"), by="days"), # term 1 holiday
                    seq(as.Date("2013-08-01"), as.Date("2013-09-01"), by="days")) # term 2 holiday 
) -> holiday_ZWE


## China: December 2017 - May 2018: Shanghai school (http://www.lyceeshanghai.com/parental-information/school-calendar/?lang=en)
#https://publicholidays.cn/school-holidays/shanghai/
data.frame(iso3='CHN',
           date = c(seq(as.Date("2018-01-24"), as.Date("2018-02-16"), by="days"), # Winter holiday/chinese new year
                    seq(as.Date("2018-04-05"), as.Date("2018-04-06"), by="days"), # Tomb sweeping day/Qingming Festival
                    as.Date("2018-05-01")) #Labour day
) -> holiday_CHN



## MERGE ####
holiday_data_opt <- ls()
holiday_all <- NULL
for(holiday_country in holiday_data_opt){
  holiday_all <- rbind(holiday_all,
                       get(holiday_country))
}

## SAVE ####
save(holiday_all,
        file='data/holiday_all.RData')
