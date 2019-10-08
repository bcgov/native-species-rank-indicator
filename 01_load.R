# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


remotes::install_github("bcgov/ranktrends")

library(dplyr)
library(ranktrends) # currently stored on 00_functions_temp_only.R



# read in data set from data catalogue

ref.0  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/bc-vertebrates-conservation-status-rank-history-1992-2012/resource/842bcf0f-acd2-4587-a6df-843ba33ec271https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

