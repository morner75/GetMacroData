#!/bin/bash

Rscript R/01_periodic_update.R 
Rscript R/02_MacroFilesProc.R 
Rscript R/03_GetMacroData.R
Rscript R/04_MacroDataOranising.R