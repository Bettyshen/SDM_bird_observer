#!/bin/bash
#SBATCH --job-name=Train-Rjob          # Job name
#SBATCH --output=res.txt           # Output file
#SBATCH --error=error.txt          # Error file
#SBATCH --time=150:00:00            # Time limit hh:mm:ss
#SBATCH --partition=share          # Partition
#SBATCH --ntasks=16                 # Run on a single CPU
#SBATCH --mem=50G                   # Job memory request
#SBATCH --mail-type=BEGIN,END,FAIL               # Mail events (BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=onid@oregonstate.edu       # Your email address
#SBATCH --constraint=el8

# Load the R module (if required)
module load R/4.3.3
module load gdal/3.0.4
module load geos/3.7.2
module load proj/8.2.1
module load sqlite

Rscript /nfs/stak/users/stanleyf/hpc-share/Ecology/Scripts/Fiona_SDM_Step1_SpatioTempWeights_Cluster.R  # Path to R script
