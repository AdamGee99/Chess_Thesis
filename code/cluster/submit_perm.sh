#! /bin/bash
#
#SBATCH --mem-per-cpu 8000
#SBATCH -c 8
#SBATCH -t 30:00
#SBATCH -a 6-100
#SBATCH --mail-user=arg15@sfu.ca
##SBATCH --mail-type=ALL

module load gcc/12.3 r/4.4.0

echo "Launching R"
date

Rscript bot_perm.R

echo "Completed"
date

# end of script