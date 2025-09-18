# setup.R
# ------------------------------------------------------------------------------
# This script sets up the R environment for this project.
# Run each section line by line, and check that no errors appear before continuing.
# ------------------------------------------------------------------------------

# --- 1. Install precommit and run initial formatting checks -------------------

install.packages("precommit")

# → Open a terminal and run the following command:
#   chmod +x run_precommit.sh

# This will install Git hooks and run checks (e.g., styler, lintr)
# It may take a few seconds the first time
system("./run_precommit.sh")

# If style checks fail, run the script a second time.
# Some changes may be automatically applied by 'styler'.
# If it fails again, read the error messages and resolve them before continuing.

# --- 2. Install renv and snapshot the environment ----------------------------

install.packages("renv")
renv::snapshot()

# This saves the current package versions to renv.lock.
# Select option 2 to install all relevant packages.
# It ensures that collaborators use the same environment.
