Read me files: 


Files generated for preprocessing of the data. 

1) clean_xls.R

Main script to preprocess vertebrate data to be input in 01_read script.
This documents all the individual updates and annomoloies in the dataset 
as vetted by CDC Leah and Lea. 
The output of the script is "verts_retroranks.csv"

This formats the data which will be eventually posted on bcdata and is only 
required to be run once. 


2)  consolidate_inverts.R 

This script consolidates and preprocessed the invertbrate data in prepartion 
for 01_read script (analogous to the clean_xls scritp for the vertebrates
described above) 
The output of the script is "inverts_retroranks.csv"

This formats the data which will be eventually posted on bcdata and is only 
required to be run once.  


3) format_base_tables.R

this script was used to format table for an external review and retro rank of
invertebrate raw data. this is not required for running the indicator or 
preprocessing data for the indicator. 

The output of the script is 3 x csv files for Molluscs, Odonata and Lepidoptera groups. 


 