############################################################
# NewBioE.r
# Program to Implement the BioEngergetics Model
# Matt Nahorniak
# South Fork Research, Inc
# 2012

#########################################################################
# To Run Program:
#
# R-files "NewBioE.R" and "NewBioE_Functions.R", and excel files 
# "Lookup_Tables.xlsx" and "BioEInput.xlsx" must be in working directory.
# 
# Use excel to select species and enter required inputs in "BioEInput.xlsx"
# In this file.  Use [ctrl-a] or manually select all rows, then use [ctrl-r]
# or right-click and select "run line or selection".  Entire program will
# run at once.  Output will be written to file "BioEOutput.xlsx".
##########################################################################
##########################################################################




# clean up anything hanging around (lots of junk in memory if program
# crashed or was stopped early
#	rm(list=ls(all=TRUE))
 

# Tell R to keep all plots made so user can scroll through them
windows(record = TRUE)

# load functions stored in R.script "NewBioE_Function.R"
	source("BioE_Functions.R")

# Read Input Data from "BioEInput.xlsx"
	Input=ReadInputFile()

# Read the constants from the Lookup table
	Constants=ReadConstants(Input$Species)

# Call the main function that calls all the functions for each
# formula and solves based on solution method.
	Results = BioE(Input, Constants)

# Done.  Make some plots and write results to file.
	write.output(Input, Results)
	plot.results(Input, Results)




