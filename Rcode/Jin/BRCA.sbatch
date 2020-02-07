#!/bin/bash
#SBATCH --mem-per-cpu=10G
#SBATCH --job-name=BRCA
#SBATCH --output=BRCA.o%j
#SBATCH --time=03-11:0:0
# SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --mail-user jinsq@ksu.edu
#SBATCH -J BRCA
module load R
R --no-save -q < BRCA_analysis.R
