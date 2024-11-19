# You may encounter issues if you attempt to run a Shiny app from a directory that also contains an R package structure. 
# In the FIM folder, the R/ subdirectory is used to store R scripts that define functions and objects for the FIM. 
# When you load a Shiny app from such a directory, R may attempt to source those files in a way that conflicts 
# with how R packages are typically loaded. 
# A work around is to store a script called "_disable_autoload.R" in the directory to prevent automatic sourcing. 