#' Load and Standardize a Table's Patient Identifier Column with Prefix
#'
#' This function retrieves a table from the database (using a specified schema and table
#' name) via **dbplyr**. It searches for a patient identifier column among a set of possible
#' names, renames the found column to `"patient_id"`, and then prepends a user-specified prefix
#' to each value in the patient_id column. Retains only patients in the provided `"cohort"` object.
#'
#' @param conn A valid DBI connection object.
#' @param schema A character string specifying the schema in which the table resides.
#' @param table A character string specifying the table name.
#' @param cohort A data frame or lazy query that contains a \code{patient_id} column.
#'    Can be NULL if no cohort is provided, in which case the entire table is loaded without filtering.
#' @param collect Logical. If \code{TRUE}, the function collects the table into memory.
#' @param prefix A character string to prepend to each patient_id value.
#' @param patient_id_names A character vector of possible patient identifier column names.
#'   Defaults to \code{c("patient_id", "patient_key", "subject_id", "subject", "subject_key")}.
#'
#' @return A tibble (or lazy query if \code{collect} is \code{FALSE}) with the patient identifier
#'   column renamed to `"patient_id"` and modified by the provided prefix.
#'
#' @examples
#' \dontrun{
#' standardized_table <- load_raw_table(
#'   conn, "schema1", "table1",
#'   collect = TRUE, prefix = "raw_"
#' )
#' }
load_raw_table <- function(conn, schema, table, cohort, collect = TRUE, prefix = "",
                           patient_id_names = c(
                             "patient_id", "patient_key",
                             "subject_id", "subject", "subject_key"
                           )) {
  # Retrieve the table using dbplyr
  tbl_obj <- tbl(conn, in_schema(schema, table))

  # List available column names in the table
  cols <- tbl_vars(tbl_obj)

  # Identify which of the possible patient ID names exist in this table
  matching_names <- intersect(patient_id_names, cols)

  if (length(matching_names) == 0) {
    stop(sprintf("No valid patient identifier column found in table '%s' in schema '%s'.", table, schema))
  }

  # Prefer "patient_id" if available; otherwise, use the first match.
  selected_name <- if ("patient_id" %in% matching_names) "patient_id" else matching_names[1]

  # Rename the found column to 'patient_id'
  tbl_obj <- tbl_obj %>% rename(patient_id = !!sym(selected_name))

  # Prepend the specified prefix to the patient_id column
  tbl_obj <- tbl_obj %>% mutate(patient_id = paste0(prefix, patient_id))

  # Retain only cohort members (if applicable).
  if (is.null(cohort)) {
    message("No ICD-10 cohort provided. Loading the entire table.")
  } else {
    tbl_obj <- tbl_obj %>% inner_join(cohort, by = "patient_id", copy = TRUE)
  }

  # Optionally collect the data into memory
  if (collect) {
    tbl_obj <- tbl_obj %>% collect()
  }

  return(tbl_obj)
}

#' Load and Standardize Multiple Tables Using a Tribble Input
#'
#' This function retrieves multiple tables from the database based on a provided tibble (or
#' data frame) containing columns for the schema name, the table name, and a user-specified
#' prefix to add to the patient identifier. For each row in the tibble, it uses
#' \code{load_raw_table()} to load and standardize the table.
#' Retains only patients in the provided `"cohort"` object.
#'
#' @param conn A valid DBI connection object.
#' @param table_info A tibble (or data frame) with columns:
#'        - `schema`: The schema name for each table.
#'        - `table`: The table name.
#'        - `prefix`: The prefix to add to the patient identifier.
#' @param cohort A data frame or lazy query that contains a \code{patient_id} column.
#' @param collect Logical. If \code{TRUE}, each table is collected into memory.
#' @param patient_id_names A character vector of possible patient identifier column names.
#'        Defaults to \code{c("patient_id", "patient_key", "subject_id", "subject", "subject_key")}.
#'
#' @return A named list of tables with the patient identifier standardized to `"patient_id"`
#'         and prefixed with the specified string.
#'
#' @examples
#' \dontrun{
#'
#' example_cohort <- build_diagnosis_cohort(
#'   conn = conn,
#'   icd10_pattern = "^C",
#'   schema = "my_schema",
#'   min_date = "2020-01-01"
#' )
#'
#' table_info <- tibble::tribble(
#'   ~schema,              ~table,                ~prefix,
#'   "raw_automated_ouh",  "pathology_synoptic",  "ouh_",
#'   "raw_automated_cuh",  "pathology_synoptic",  "cambridge_",
#' )
#' standardized_tables <- load_multiple_raw_tables(
#'   conn, table_info,
#'   collect = TRUE, cohort = example_cohort
#' )
#' }
load_multiple_raw_tables <- function(conn, table_info, cohort, collect = TRUE,
                                     patient_id_names = c(
                                       "patient_id", "patient_key",
                                       "subject_id", "subject", "subject_key"
                                     )) {
  # Validate that table_info contains the required columns
  required_cols <- c("schema", "table", "prefix")
  if (!all(required_cols %in% names(table_info))) {
    stop("table_info must have columns: 'schema', 'table', and 'prefix'.")
  }

  result_list <- list()

  # Loop through each row of the table_info tibble
  for (i in seq_len(nrow(table_info))) {
    schema_name <- table_info$schema[i]
    table_name <- table_info$table[i]
    prefix <- table_info$prefix[i]

    standardized_tbl <- load_raw_table(
      conn = conn,
      schema = schema_name,
      table = table_name,
      collect = collect,
      cohort = cohort,
      prefix = prefix,
      patient_id_names = patient_id_names
    )

    # Store the standardized table in the result list, naming it by the table name
    result_list[[table_name]] <- standardized_tbl
  }

  return(result_list)
}
