# NOa

# Program to calculate niche overlaps over multiple niche
# axes per individual.

# Program reads in individual data,  calculates electivity scores for any resource usage variables,
# calculates niche overlaps between pairs of species, and runs null model tests of (i) differential use of niche 
# space by multiple species; and (ii) even distribution of species across niche space.

# Cut and paste the commands into R. They may be pasted into R in groups of rows - all comments 
# are preceded by a hash, so will not be acted on in R.

# Sections of the program which require input from the user are bracketed by

# ??????????????????????????????????????????????????

# The user should have the following files in the same directory:# one program file NOa.R,# one data file MEE3_070_sm_ExampleA.txt# one availability file MEE3_070_sm_ExampleAhabitatAvail.txt# and one file of R functions, MEE3_070_sm_NicheFunctions.txt

# This illustration examines mean niche overlap between three species
# on six niche axes (1) Habitat association 
# (resource selection); (2) ticks (count data); (3) colour 
# categorical data; (4) sex (binary data); (5) tailbody (proportion 
# data); and (6) weight (measurement data)

# In this example, different species are compared for niche overlaps within a single site.# The basic data file ExampleA.csv has its first two columns labelled id (for individual)# and species. Subsequent columns are the variables of different types which were measured.

# Since one of the variables, habitat, is of the “resource selection” type, we must also# supply a matching availability file ExampleAhabitatAvail.csv. There must be# one availability file for each resource-selection type of variable.

# The variables may be habitat where found, food eaten, morphological columns, etc.# They may refer to resource usage, or be categories, counts, or continuous data. If continuous,# they may be measurements or ratios of measurement. The possible types of# variables are:# ”cat” = categorical, but not resource selection# ”bin” = binary# ”cts” = continuous, use raw data (no transformation)# ”mesa” = measurement, continuous positive, take logs# ”pent” = percentage data, bounds at 0 and 100, use logits# ”propane” = proportion data, bounds at 0 and 1, use logits# ”count” = count# ”rsel” = resource selection, categorical

# The variable types need to be specified while running the program.# The commands in the program file NOa.txt are to be copied and pasted into# R, using the directory which contains the program and data files. At some stages in# the program file, the user needs to type in appropriate details, e.g. specifying the types of# variables. 

# The analysis is divided into four distinct sections, 
# each identified by
#############################################################
# 1. Analysis A: Niche Overlap incorporating multiple niche axes
# 2. Tests determining if two species occupy different niche space,
#    seperately for each each axis, and for niche overlap averaged
#    across axes.
# 3. Tests determining if the species are evenly distributed across,
#    or clumped within niche space. This is done seperately for each
#    each axis, and for niche overlap averaged across axes.
# 4. The compilation of the above results into a single R object
#    for ease of comparison/printing/saving

# Sections of the program which require input from the user
# are bracketed by

# ??????????????????????????????????????????????????


# When supplying your own data, make the first two columns id = individual and# species = species or some other taxonomic group. The data should have the column# names across the top, and thereafter one row per individual.# For any resource type of variable, there must be an associated file giving the availabilities.# The first row should be the names of the choices, and the second row is either# the percentages or the proportions of the different choices. In Example A, there is one# resource selection variable, called habitat, with three choices, grass, forest and# rock. Within the file, the choices are sorted alphabetically, and a check is done to be# sure the choices named in the availability file match those in the appropriate column# of the main data file.# There are comments throughout the program to explain what is being calculated at# each stage.    