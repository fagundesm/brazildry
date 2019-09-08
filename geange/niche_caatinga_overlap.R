# MEE3_070_sm_NOa

# A general method for combining different data types into
# a unified multivariate analysis of niche overlap.
# This program calculates niche overlaps over multiple niche axes
# when each axis of niche space is measured using THE SAME
# individuals.

# Program reads in individual data,  calculates 
# electivity scores for any resource usage variables,
# calculate niche overlaps between pairs of species, and
# runs null model tests of (i) differential use of niche 
# space by multiple species; and (ii) even distribution 
# of species across niche space.

# This illustration examines mean niche overlap between three species
# on six niche axes (1) Habitat association 
# (resource selection); (2) ticks (count data); (3) colour 
# categorical data; (4) sex (binary data); (5) tailbody (proportion 
# data); and (6) weight (measurement data)

# Data is read into the program from external datafiles:
# 1. "MEE3_070_sm_ExampleA.txt" measurements for each individual across
# the six niche axes
# 2. "MEE3_070_sm_ExampleAhabitatAvail.txt" resource availability data

# Likewise, functions are read into the program from
# the file "MEE3_070_sm_NicheFunctions.txt"

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

# Cut and paste the following commands into R.
# They may be pasted into R in groups of rows - all comments 
# are preceded by a hash, so will not be acted on in R.

# Sections of the program which require input from the user
# are bracketed by

# ??????????????????????????????????????????????????
  


#   setwd --> Usar Session - Choose


# -----------------------------------------------------------
#             Functions Input
# -----------------------------------------------------------


# Read in the niche overlap functions:

source("geange/MEE3_070_sm_NOa/MEE3_070_sm_NicheFunctions.txt")


#############################################################
# -----------------------------------------------------------
#          Analysis A: Niche Overlap
# -----------------------------------------------------------
#############################################################

# Analysis is for calculating niche overlaps over
# multiple niche axes per individual. Pairs of species
# have their niche overlaps calculated and tested for
# significance. Individual-based variables are used, and 
# electivity may be included as a categorical type of data.

# The input data set is a .txt file.
# It needs to have its first two columns
# labelled "id" and "species". Subsequent columns are
# individual-based variables, one per column.

# Input the individual data file:


library(dplyr)
#Fazendo uma matriz de correlaçao entre todas as espécies com réplicas, independente do plot
# ??????????????????????????????????????????????????
A.df1 <- read.csv("geange/dadosmai.csv",T)
A.df1$plot <- as.numeric(A.df1$plot)
A.df1 <- arrange(A.df1, group_by(plot))
A.df1 <- filter(A.df1, plot == "11")
A.df <- A.df1[,-c(1,2)]

# ?????????????????????????????????????????????????? 
# Ensure the first two column names are "id" and "species".
colnames(A.df)[1] <- "id"
colnames(A.df)[2] <- "species"

# Ensure that the first 2 cols are factors.
A.df$id      <- as.factor(A.df$id)
A.df$species <- as.factor(A.df$species)

# Store some vectors of names:
spnames   <- sort(unique(as.character(A.df$species)))
no.spp    <- length(spnames)

varnames <- colnames(A.df)[-(1:2)]    
no.vars  <- length(varnames) 

# Make a vector of variable types to match the variable names (varnames):
# "cat"   = categorical, but not resource selection
# "bin"   = binary
# "cts"   = continuous, use raw data (no transformation)
# "meas"  = measurement, continuous positive, take logs
# "pcent" = percentage data, bounds at 0 and 100, use logits
# "propn" = proportion data, bounds at 0 and 1, use logits
# "count" = count
# "rsel"  = resource selection, categorical


# ??????????????????????????????????????????????????
# The following command goes with the ExampleA.txt data set:
# The following command goes with the ExampleA.txt data set:
vartypes <- c("cts","cts","cts","cts","cts","cts","cts")
# ??????????????????????????????????????????????????


# Check they are correctly labelled:
cbind(varnames,vartypes)

# Any variables of resource selection type should have an
# associated availability vector for the site.

#Check to see if any measurements have a value at or below zero
for (vv in 1:no.vars) 
  if (vartypes[vv] == "meas")
  {
    y <- A.df[,colnames(A.df)==varnames[vv]]
    if (min(y)<=0) 
    {
      print(paste("Measurement variable",varnames[vv],"has value(s) at or below zero",sep=""))
      print("Is this what you intended, or are they missing values?")
    }
  }


# Set up a list of objects which are NULL if this is not
# a resource selection variable, and with the availability
# vector if it is resource selection.
avail.list <- vector("list",no.vars)
names(avail.list) <- varnames

# Set the right matrices into the list.
# Run through the following routine for each resource selection
# type of variable.


# ??????????????????????????????????????????????????
# Routine start:
# In the ExampleA.txt data, just the first variable "habitat" is of
# type "rsel" =  resource selection.

# Read in the availability vector for this resource from a file.
# These are percentages of the various choices.
#avail.vect <- read.table("MEE3_070_sm_NOa/MEE3_070_sm_ExampleAhabitatAvail.txt",T)[1,]

# Sort alphabetically:
#avail.vect <- avail.vect[sort.list(names(avail.vect))]

# Check that the resource types used are all in the availability
# vector, need names to match.
#used <- levels(A.df$history)
#used %in% names(avail.vect)

# If not all TRUE, go back to data files, rename to make them match,
# start analysis again.

# Ensure the availabilities are percentages:
#avail.vect <- avail.vect/sum(avail.vect)*100
# If they were already percentages, this makes no change.

#avail.list[[5]] <- avail.vect  # Stored in first component, as
# "habitat" was the first variable.

# Routine end.

# Read in more availability data if required for other variables.
# Do the routine above, between the dashed lines, for each "rsel"
# variable.
# Put them in the correct component of the list.
# ??????????????????????????????????????????????????



# Next look at avail.list, check it seems right - availability
# vectors should match "rsel" type variables, NULL elsewhere.
# Different options within each resource should be in alphabetical
# order.

avail.list


# Set up R objects to store results

# The object alpha.list has one component per variable.
# The components are NULL for ordinary variables.
# For resource selection variables, the component is
# the matrix of Manly's alpha values for that variable. 
# The matrix has:
# Rows = species,
# Cols = choices for that resource (e.g. the resource
# "habitat" may have choices "grass", "rock", "forest"),

alpha.list <- vector("list",no.vars)
names(alpha.list) <- varnames

for (vv in 1:no.vars) if (vartypes[vv]=="rsel")
{
  choices <- unique(A.df[,vv+2])
  no.ch   <- length(choices)
  alpha.list[[vv]] <- matrix(NA,no.spp,no.ch)
  dimnames(alpha.list[[vv]]) <- list(spnames,choices)
}

# Set up an array of niche overlaps.
# The object no.array is an array of niche overlaps.
# It is a 3-D array, with rows and columns being species 
# (a square symmetric matrix for pairwise niche overlaps), 
# and the layers are the dimensions for the multivariate 
# niche overlap measure (one dimension per variable).
# Rows and columns are species, layers are variables.

no.array  <- array(1,c(no.spp,no.spp,no.vars))
dimnames(no.array) <- list(spnames,spnames,varnames)

# Run through each variable in turn, identify its type,
# calculate the appropriate NO matrix and store it in
# the right layer of the no.array.
for (vv in 1:no.vars)
{
  y <- A.df[,colnames(A.df)==varnames[vv]]
  if (vartypes[vv] == "bin")
    no.array[,,vv] <- no.bin.fn(A.df$species,y)
  if (vartypes[vv] == "cat")
    no.array[,,vv] <- no.cat.fn(A.df$species,y)
  if (vartypes[vv] == "count")
    no.array[,,vv] <- no.count.fn(A.df$species,y)
  if (vartypes[vv] == "cts")
    no.array[,,vv] <- no.cts.fn(A.df$species,y)
  if (vartypes[vv] == "meas")
    no.array[,,vv] <- no.cts.fn(A.df$species,log.fn(y))
  if (vartypes[vv] == "pcent")
    no.array[,,vv] <- no.cts.fn(A.df$species,
                                logit.pcent.fn(y))
  if (vartypes[vv] == "propn")
    no.array[,,vv] <- no.cts.fn(A.df$species,
                                logit.propn.fn(y))
  if (vartypes[vv] == "rsel")
  {
    
    # Do Manly's alpha calculations, store.
    no.choices <- length(avail.list[[vv]])
    choicenames <- names(avail.list[[vv]])
    avail.vect <- avail.list[[vv]]
    alpha.mat <- alpha.fn(A.df$species,y,avail.vect)
    alpha.list[[vv]] <- alpha.mat         
    
    # Do niche overlaps, as proportions in categories:
    no.array[,,vv] <- no.rsel.cat.fn(alpha.mat)
  }
}

# Also calculate overall NO measures, averaged over the
# dimensions.
no.overall.mat <- apply(no.array,c(1,2),mean)
no.overall.mat.sd <- apply(no.array,c(1,2),sd)
apply(no.overall.mat, 1, min)

#fazer a soma das matrizes
print(a3 <- sum(as.matrix(no.overall.mat)/2))

mean(a3/length(no.overall.mat))


