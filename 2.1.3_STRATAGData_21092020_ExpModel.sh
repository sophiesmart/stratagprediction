#!/bin/bash --login
########## SBATCH directives ##########
#                                     #
#  Complete these as necessary for    #
#  Slurm based systems e.g. Hawk      #
#                                     #
#######################################
#
#SBATCH --job-name=stratamice
#SBATCH --partition=c_compute_neuro1
#SBATCH --ntasks=40
#SBATCH --mem-per-cpu=4000
#SBATCH --ntasks-per-node=40
#SBATCH --time=8:00:00
#SBATCH --account=scw1529
#SBATCH --output /nfshome/store02/users/c.mpmss1/STRATAG/logs/stratamice_out.%J
#SBATCH --error /nfshome/store02/users/c.mpmss1/STRATAG/logs/stratamice_err.%J
#SBATCH --mail-type=END
#SBATCH --mail-user=SMARTS1@cardiff.ac.uk
#

# 2.1.3_STRATAGData_21092020_ExpModel

### Run in environment
# module load R/4.0.0
### Export library R directory
# export R_LIBS_mpmss1=/nfshome/store02/users/c.mpmss1/software/R/library
### Make R library directory
# mkdir -p $R_LIBS_mpmss1
### Make logs directory
# mkdir -p /nfshome/store02/users/c.mpmss1/STRATAG/logs
# Rscript /nfshome/store02/users/c.mpmss1/STRATAG/2.1.1_STRATAGData_21092020_ExpModel.R


module load R/4.0.0
# Export library R directory
export R_LIBS_mpmss1=/nfshome/store02/users/c.mpmss1/software/R/library

Rscript /nfshome/store02/users/c.mpmss1/STRATAG/2.1.2_STRATAGData_21092020_ExpModel.R

#dos2unix /nfshome/store02/users/c.mpmss1/STRATAG/2.1.3_STRATAGData_21092020_ExpModel.sh
#chmod +x /nfshome/store02/users/c.mpmss1/STRATAG/2.1.3_STRATAGData_21092020_ExpModel.sh
#on Hawk\
#sbatch 2.1.3_STRATAGData_21092020_ExpModel.sh