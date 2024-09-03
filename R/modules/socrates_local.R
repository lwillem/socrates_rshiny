#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# Run a remote SOCRATES UI
#
# Copyright 2024, SIMID
#___________________________________________________________________________

# load package(s)
library('shiny')
library('socialmixr')
library('countrycode')
library('data.table')
library('markdown')
library('wpp2015')
library('wpp2019')
library('tidyverse') 
library('ggthemes')  
library('ggpubr')  
library('jsonlite')
library('curl')
library('XML')
library('rsconnect')

# option 1: run public UI via GitHub ----
runGitHub('socrates_rshiny','lwillem')

# option 2: run public UI via URL (of GitHub repo) ----
runUrl('https://github.com/lwillem/socrates_rshiny/archive/master.tar.gz')

# option 3: run latest version
runGitHub('socrates_rshiny','lwillem',ref = 'dev')

