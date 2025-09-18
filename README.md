# RWE Project Template Repository V1.0

A production-ready, **mandatory** template that all Real-World Evidence projects must start from.  
It enforces a common project structure, modular code, and strict governance around credentials, data handling, and reproducibility.
All default functions are well-documented; if you are struggling, try `?function` to see the docs, e.g. `?build_grepl_expression`.

---

## Versioning
| Version | Changes made | Date | Author |
|---------|--------------|------|--------|
| V1.0 | Baseline Template Repository | 09/09/2025 | Alycia Perkins |

---

## 🚀 Motivation

- **Consistency:** Every analyst can open any project and instantly understand the layout.
- **Reproducibility:** A functioning `run_all.R` file means results can be re-run months later and match.
- **Audit-readiness:** Automatic pre-commit checks protect against leaking identifiable data (IPD).
- **Velocity:** Common helper and extraction functions reduce boiler-plate coding, allowing RWE members to focus on generation of analysis results.

---

## ✅ Project Kick-Off Checklist

Follow these steps when starting a project.  Skipping any box will break the pipeline later!

| Step | What you do | Why it matters |
|------|-------------|----------------|
| 1. | Click **`Use this template → Create a new repository`** on GitHub | Copies the repo structure |
| 2. | Name the repo in the format `client-disease-studytype` and add a one-line description | Easier search & audits |
| 3. | Clone the repo inside your AWS RStudio using *File → New Project... → Version Control → Git* | Keeps all code inside the secure environment |
| 4. | `data/input/project_constants.R` → **replace every placeholder** (`project_name`, `standardised_schema`, ICD-10 regex, etc.). If there are additional analysts beyond the lead and QC analyst, add them here. | Tailors the pipeline |
| 5. | **Do not edit** `data/input/operational_constants.R` unless a change is required *across all projects*. | Global consistency |
| 6. | Open `setup.R` and follow the instructions to setup pre-commit and renv. | Activates sensitive-data & style checks, locks package versions used team-wide, avoids setup errors. |
| 7. | Commit the above edits → **push to GitHub** | Establish baseline |
| 8. | GitHub → Settings → Collaborators & teams → **add the RWE team (Admin)** | Ensures support & reviews |
| 9. | **Run** `source("scripts/run_all.R")` once to verify the whole pipeline executes. Check the project folder has been created in `RWE_Projects`. | Confirms setup success |
| 11. | 🎉 Project set-up complete! |

---

## 🗂 Repository Layout & File Purpose

```
data/
├── raw/                       # This folder should always be empty on GitHub. Used in Secure Data Environments to store filtered datasets from 01_data_collection. 
├── processed/                 # This folder should always be empty on GitHub. Used in Secure Data Environments to store processed datasets from 02_data_cleaning and 03_cohort_generation. 
├── interim_results/           # This folder should always be empty on GitHub. Used in Secure Data Environments to store interim results that have not been QC'd from 04_data_analysis. 
├── input/                     # 
│   ├── operational_constants.R# Organisation-wide constants (e.g., colour palette, list of raw schemas). You will rarely edit this. 
│   └── project_constants.R    # Study-specific constants (e.g., database name, ICD-10 regex, file-naming convention). Edit this every project.
scripts/
├── 00_initialise/
│   ├── init.R                 # Loads packages, credentials, constants
│   └── helper_functions.R     # Shared utility fns (DB conn, obfuscation, etc.)
├── 01_data_collection/
│   ├── cdm_data_collection_fcns.R  # CDM extraction helpers
│   ├── raw_data_collection_fcns.R  # Raw schema extraction helpers
│   └── run_data_collection.R       # Builds study population & pulls data
├── 02_data_cleaning/
│   ├── data_cleaning_fcns.R   # Table-level cleaning and column derivation (e.g. simple mappings)
│   ├── data_derivation_fcns.R # Derived tables (e.g., BMI)
│   └── run_data_cleaning.R    # Creates `analysis_data`
├── 03_cohort_generation/
│   ├── cohort_generation_fcns.R # Eligibility rules
│   └── run_cohort_generation.R  # Builds `analysis_cohorts`
├── 04_data_analysis/
│   ├── data_analysis_fcns.R   # Re-usable analysis helpers
│   └── run_data_analysis.R    # Produces figures / tables
├── run_all.R                  # Orchestrates 00→04 end-to-end
```

Each file contains boilerplate text and code explaining the use of the file. Read these carefully to understand how to make changes to the code to fit your analysis purpose. Note that this structure is a guideline; where necessary, changes may be made to suit the project structure. 

Below is a **check-list–style overview** of what the project analysts must change or confirm in every `*_fcns.R` **function library** and every `run_*.R` **orchestration script** when converting  the template into a live study.  
Treat each bullet as a gating item during project initiation or code review.

---

### 0. `data/inputs/`

| File | Mandatory edits | Why |
|------|-----------------|-----------|
| **`operational_constants.R`** | **None necessary** unless the entire RWE team agrees a global value needs changing. | Ensures every project uses the same global constants/colour scheme. |
| **`project_constants.R`** | • Set `project_name`, `primary_dbname`, `primary_diagnosis_icd10_regex`, `minimum_study_entry_date`, `standardised_schema`, and any other global variables required for analysis. <br>• Adjust the file-path generators to suit your project's needs. | Central source of truth for filenames, schemas and study definitions. Reproducibility depends on these being correct. |


### 1. `00_initialise/`

| File | Mandatory edits | Why |
|------|-----------------|-----------|
| **`helper_functions.R`** | *Usually none*, but: <br>• Add *generic* helpers you expect to reuse across the entire project (e.g., date utilities, logging wrappers). <br>• If you add DB helpers, make them parameter-driven. **Never** hard-code schema/table names in functions. | Keeps the shared toolbox project-agnostic. |
| **`init.R`** | *Usually none* | Sets up the project environment and sources constants.

---
### 2. `01_data_collection/`

| File | Mandatory edits | Why |
|------|-----------------|----------------|
| **`cdm_data_collection_fcns.R`** | *Usually none*, but if necessary, add unit-style examples (“`@examples`”) for every new helper. | Maintains modularity; keeps run scripts clean; documents usage. |
| **`raw_data_collection_fcns.R`** | Make sure the default `patient_id_names` vector covers every real ID column you might encounter in the raw tables. | Prevents later ID-collision bugs and keeps prefix logic centralised. |
| **`run_data_collection.R`** | 1. **Study population**: Ensure the ICD-10 regex, date filter, and schema referenced in the call to `build_diagnosis_cohort()` are aligned with your protocol. These should be changed in the `project_constants.R` file. <br>2. **`cdm_table_info` tibble**: list every CDM table you need, with a clear `filter_expression` string in R code which will be injected into the table query (empty `""` = no filter). <br>3. **`raw_table_info` tibble**: Similarly to `cdm_table_info`, enumerate raw tables and patient ID prefixes. <br>4. **Logging** – keep, extend, or refine the `message()` calls so the run log clearly shows each step when the full pipeline is run. | Delivers the deterministic input dataset for all downstream steps. Any omission or wrong filter makes the whole pipeline irreproducible. Outputs used for downstream analyses are `cdm_data`, `raw_data`, and `study_population`. |

---

### 3. `02_data_cleaning/`

| File | Mandatory edits | Why |
|------|-----------------|-----|
| **`data_cleaning_fcns.R`** | • Replace every example cleaner with study-specific logic or add new cleaners for additional tables (e.g., convert units, drop implausible values). <br>• `clean_*()` functions must *only* take a single table as input and return the cleaned version—no global side-effects. Cleaning can involve derivation of new columns within the table that do not change the size of the table, e.g. simple mappings. <br>• Document assumptions (e.g., *“height always in cm”*). | Cleaners run in batch by `run_data_cleaning.R`. |
| **`data_derivation_fcns.R`** | • Add derived measures required for analysis (e.g., BMI, treatment lines, time-to-event endpoints). <br>• Every derivation must: (1) accept cleaned input; (2) output a tibble keyed by `patient_id` and *at least* one analysis variable. | Keeps CDM and derived data clearly separated, aiding QC. |
| **`run_data_cleaning.R`** | 1. Source your new cleaner/deriver files. <br>2. In the `analysis_data <- list()` section, call the correct cleaner for each table you collected. <br>3. In the `analysis_data$derived_data` section, call every derivation function you created. | Guarantees that `analysis_data` is fully ready for cohort-building & epidemiological analysis. |

---

### 4. `03_cohort_generation/`

| File | Mandatory edits | Why |
|------|-----------------|-----|
| **`cohort_generation_fcns.R`** | • Delete the example ICD-10 cohorts and implement the exact eligibility logic from the study protocol. <br>• If a cohort requires multiple eligibility dates (e.g., treatment start *and* diagnosis), return those columns so downstream analysis can use them. | Cohorts define denominators; mistakes here invalidate results. |
| **`run_cohort_generation.R`** | • Call every cohort generator you added and store them in `analysis_cohorts`. <br>• If only one cohort exists, still save it as a named list (`primary_cohort = …`). <br>• Update log messages and the `saveRDS()` variable names if needed. | Provides a machine-readable catalogue of cohorts for QC and modelling. |

---

### 5. `04_data_analysis/`

| File | Mandatory edits | Why |
|------|-----------------|-----|
| **`data_analysis_fcns.R`** | • Populate with reusable analysis helpers: e.g., summary-table builders, plotting wrappers, statistical analyses, and figures and tables. <br>• Each helper should accept `analysis_data` and a `cohort` tibble, increasing potential for re-use across projects. | Keeps `run_data_analysis.R` concise and promotes code reuse. |
| **`run_data_analysis.R`** | 1. Source `data_analysis_fcns.R`. This section is much more free-form, as epidemiological analyses are widely varied by project. <br>2. For each cohort in `analysis_cohorts`, call the appropriate analysis functions and write outputs to `data/processed/…`. <br>3. Follow QC protocol to ensure high quality outputs [TBD]. <br>4. Ensure every plot/table creation step is scripted - **no manual clicks**. | Makes your results fully reproducible and audit-friendly. |

---

### 6. `run_all.R` *(pipeline driver)*

| Required review | Why |
|-----------------|-----|
| • Confirm the logical flags (`run_data_collection`) point to the right locations. <br>• Add `tryCatch()` blocks or assertions if you want the script to fail fast when inputs are missing. | `run_all.R` is the single command auditors will execute. Its success or failure is the yard-stick for reproducibility. |

---

### 7. House-keeping Tips for Every Change

1. **Version Control Everything** – Commit small, atomic changes per file.  
2. **Document Assumptions** – Inline comments + `roxygen2` headers.  
3. **Run Pre-commit** – `precommit::run_precommit()` before pushing large refactors. This should automatically run when committing to Git.
4. **Lock Packages** – `renv::snapshot()` immediately after installing a new CRAN/BioC/GitHub package.  
5. **Regenerate Outputs** – After any change to `*_fcns.R`, re-run `run_all.R` end-to-end and ensure the correct pieces of the pipeline are updated appropriately. If there are changes to data collection functions, you may need to manually change the flag to run data collection. 

Follow this checklist and each edited script will slot cleanly into the larger pipeline, guaranteeing a transparent, reproducible, and auditable analysis.


---

## ✨ Getting Started with GitHub

1. **Branch early, branch often** – Use the Git pane in RStudio (`New Branch`) for each iteration of the analysis.  
2. **Protect `main`** – Only merge via Pull Requests after peer review.  
3. **Pull frequently** – Keeps your local work in sync, especially if multiple ICs are working on the same project.

A quick beginner-friendly guide: <https://www.freecodecamp.org/news/guide-to-git-github-for-beginners-and-experienced-devs/>

---

## 🔐 Accessing Data

1. Open RStudio in AWS.  
2. Ensure each of the following are run in the console with your specific credentials to put them in R memory:

   ```r
   POSTGRESQL_HOST     <- "..."
   POSTGRESQL_PORT     <- 5432
   POSTGRESQL_USER     <- "..."
   POSTGRESQL_PASSWORD <- "..."
   ```

3. `create_db_connection()` (from `helper_functions.R`) is called automatically inside `run_data_collection.R`.

> **Tip:** Credentials live only in your AWS home directory - never on the Git repo, never on your laptop.

---

## 💾 Saving Data

| Folder | Contents | Version-controlled? |
|--------|----------|---------------------|
| `data/input/` | Mapping files, lookup tables, **`project_constants.R`** | ✔ Yes |
| `data/raw/` | IPD & intermediate `.rds` outputs (e.g. `*_cdm_data.rds`) | ❌ No (git-ignored) |
| `data/processed/` | Anonymised summaries | ❌ No by default (can be toggled) |
| `data/interim_results/` | QC outputs, not yet ready for QC | ❌ No by default (can be toggled) |

---

## 💾 Universal Project Folder

The Universal Project Folder is where final results go for QC purposes. This is set up by the project lead during project initialisation. It should exist under `RWE_Projects`. If it does not, check that `init.R` has run successfully. 

Any final results that are to be QC'd should be saved here using the `save_results` function in the `helper_functions.R` file. This ensures there is an auditable trail of results which can be tied back to code at a particular point in time for each analyst working on the project.

See the Quality Control section for further details on how to carry out QC on these results. 

---

## 🧰 Pre-commit in Detail

- Automatically runs **before each commit**.  
- Blocks pushes containing:
  - Obvious patient IDs  
  - Debugging code
  - Unrunnable code
  - Large files which may contain IPD
- Auto-formats code with `styler` following the tidyverse style guide.

If it fails, read the console message, fix the file(s), stage, and re-commit.

---

## 📦 `renv` for Package Management

```r
# Take a snapshot after adding a new package
renv::snapshot()

# Sync to the versions in renv.lock
renv::restore()
```

---

## ✍️ How to Write Code in This Template

1. **Use functions written in the `*_fcns.R` files to ensure a reproducible pipeline is developed**  
    •  Pure functions; no code is run in these files.
2. **No magic numbers**  
    •  Any strings, numbers, or other free-text fields across files should be stored as a variable in `project_constants.R`.
3. **Only run code in `run_*.R`**  
    •  These scripts orchestrate and call your functions in the correct order.  
4. **One function = one purpose**, where possible – makes debugging and QCing easier.  
5. **Name clearly:** verbs first (`clean_`, `derive_`, `generate_`).  
6. **Tidyverse style:** no `::` or `.data` required (the linter fixes minor slips).

Use `Ctrl+Alt+X` in RStudio to refactor highlighted code into a function automatically.

---

## 🔄 Running the Full Pipeline (`run_all.R`)

`run_all.R` **must** be able to execute without manual edits after you finish Step 11 of the kickoff checklist. Any version of the pipeline that is pushed to main should be able to run from start-to-end with meaningful outputs.

```r
source("scripts/run_all.R")
```

It will:

1. **Initialise** the environment 
2. **Collect** data (or read previously saved RDS).
3. **Clean & derive**
4. **Generate cohorts** 
5. **Analyse & output** results to `data/processed/`

If any step fails, fix the underlying function/script; do **not** edit `run_all.R` to “skip” stages. This is okay when debugging, but all code on the main branch should be automated.

---

## 🩺 Quality Control 

Coming soon.

---

## 🧪 Troubleshooting

1.  Git tab not appearing in RStudio?
    -   Go to the "Terminal" tab and type `git status`
    -   An error message will appear starting with `fatal: detected dubious ownership in repository`, followed by a suggested command to run which starts with `git config --global --add safe.directory /some-directory-name`. Run this command.
    -   Restart your R session by clicking the red power button in the top right corner of RStudio. The git tab should now appear!
2.  Getting the error `fatal: Could not read from remote repository.` when trying to pull/push to git? Try adding a new [GitHub token](https://github.com/settings/tokens) and using it as the 'password' in the pop-up box. 'username' doesn't matter.
3.  Getting the error `Author identity unknown` when you first connect Github with RStudio? Run the commands suggested by the message to add yourself as an author when pushing commits from RStudio, e.g. `git config --global user.email your.github.email@etc.com`
4.  `renv` stopping `precommit` from running when committing to Git? Delete the project in R and restart, following the `setup.R` instructions closely.
5.  Running into any other issues? Lean on the team, and if you solve something, open a GitHub PR to add it to this troubleshooting guide! Chances are if you struggle someone else will too.

---

## 📚 References & Further Reading

- Tidyverse style guide – <https://style.tidyverse.org/>
- HealthyR file-structure – <https://argoshare.is.ed.ac.uk/healthyr_book/file-structure-and-workflow.html>
- Git visualiser – <https://onlywei.github.io/explain-git-with-d3/#rebase>
- Functional programming motivation – <http://adv-r.had.co.nz/Functional-programming.html#fp-motivation>As we are not computer scientists, this Motivation section gives a great overview of what we should expect our functional programs to look like on a smaller scale.
- `renv` docs – <https://rstudio.github.io/renv/>
- `here` package – <https://here.r-lib.org/>
- `styler` – <https://styler.r-lib.org/>

---

*Happy analysing – and remember, **run_all.R is your single source of truth**!* 🚀
