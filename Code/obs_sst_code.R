######
## Environmental data access and visualization code
######

## Here is where you would actually source the functions in "sst_functions.R" Importantly, all of these functions will require connection to the shared drive.

## Testing things out
code.path<- "./Code/" # You may need to update this
source(paste(code.path, "obs_sst_functions.R", sep = ""))

# Extracting OISST dataset -- full times series for the NES LME
oisst.dat<- env_data_extract(data.set = "OISST", dates = NULL, box = c(-77, -60, 35, 46), out.dir = "~/Dropbox/Andrew/Work/GMRI/Projects/AllData/", mask = NULL)

# What did we get?
print(oisst.dat) # Daily OISST data, 13707 days to be exact.
plot(oisst.dat[[1]])

# Visualizing OISST data
env_data_timeseries(oisst.dat, baseline = c("1982-01-01", "2011-01-01"), regions = c("NELME", "GoM", "SNE-MAB"), out.dir = "~/Dropbox/Andrew/Work/GMRI/Projects/AllData/")

