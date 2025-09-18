#' Build Diagnosis Cohort by ICD10 Diagnosis Codes
#'
#' This function builds a patient cohort by filtering the diagnoses table based on an ICD10 regular expression.
#' It optionally restricts the cohort to diagnoses on or after a specified date.
#'
#' @param conn A valid DBI connection object.
#' @param icd10_pattern A regular expression pattern (a character string) to filter ICD10 diagnosis codes.
#'   Defaults to the global variable \code{primary_diagnosis_icd10_regex} if available.
#' @param schema A character string specifying the database schema containing the diagnoses table.
#'   Defaults to the global variable \code{standardised_schema} if available.
#' @param min_date Optional. A date (or character string convertible to a date). Only include diagnoses on or after this date.
#'   Defaults to \code{NA}, meaning no date filtering is applied.
#' @param collect Logical. If \code{TRUE}, the function returns a collected (in-memory) data frame;
#'   if \code{FALSE}, it returns a lazy query. Defaults to \code{TRUE}.
#'
#' @return A data frame or lazy query with distinct \code{patient_id}s that meet the criteria.
#'
#' @examples
#' \dontrun{
#' # Using default icd10_pattern and schema from global variables:
#' cohort <- build_diagnosis_cohort(
#'   conn = conn,
#'   min_date = "2020-01-01"
#' )
#'
#' # Specifying a custom ICD10 pattern:
#' cohort <- build_diagnosis_cohort(
#'   conn = conn,
#'   icd10_pattern = "^I[0-9]{2}", # Example: ICD10 codes starting with "I" followed by two digits
#'   schema = "my_schema",
#'   min_date = "2020-01-01"
#' )
#' }
build_diagnosis_cohort <- function(conn,
                                   icd10_pattern = primary_diagnosis_icd10_regex,
                                   schema = standardised_schema,
                                   min_date = NA,
                                   collect = TRUE) {
  # Validate connection
  if (!DBI::dbIsValid(conn)) {
    stop("Invalid database connection provided.")
  }

  # Validate that icd10_pattern is a non-empty character string.
  if (!is.character(icd10_pattern) || length(icd10_pattern) != 1 || nchar(icd10_pattern) == 0) {
    stop("icd10_pattern must be a non-empty character string.")
  }

  # Validate schema argument.
  if (!is.character(schema) || length(schema) != 1 || nchar(schema) == 0) {
    stop("schema must be a non-empty character string.")
  }

  # Access the diagnoses table in the specified schema.
  diagnoses_tbl <- tbl(conn, in_schema(schema, "diagnoses"))

  # Filter rows where the diagnosis code matches the ICD10 pattern.
  cohort_query <- diagnoses_tbl %>%
    filter(grepl(icd10_pattern, diagnosis_code))

  # Optionally filter by a minimum diagnosis date.
  if (!is.na(min_date)) {
    # Try to convert min_date to a Date if needed.
    min_date_parsed <- try(as.Date(min_date), silent = TRUE)
    if (inherits(min_date_parsed, "try-error") || is.na(min_date_parsed)) {
      stop("min_date must be a valid date or a string convertible to a date.")
    }
    cohort_query <- cohort_query %>%
      filter(diagnosis_date >= min_date_parsed)
  }

  # Select distinct patient IDs.
  cohort_query <- cohort_query %>%
    select(patient_id) %>%
    distinct()

  # Return either a lazy query or a collected data frame.
  if (collect) {
    return(cohort_query %>% collect())
  } else {
    return(cohort_query)
  }
}

#' Load a CDM Table for a Specified Patient Cohort
#'
#' This function loads a table from a Common Data Model (CDM) within the specified schema.
#' It checks that the table exists and filters the table to include only records for patients in the provided cohort.
#'
#' @param conn A valid DBI connection object.
#' @param schema A character string specifying the database schema containing the CDM table.
#' @param table_name The name of the table to load.
#' @param cohort A data frame or lazy query that contains a \code{patient_id} column.
#'    Can be NULL, in which case the entire table is loaded without filtering.
#' @param collect Logical. If \code{TRUE}, returns a collected (in-memory) data frame;
#'   if \code{FALSE}, returns a lazy query. Defaults to \code{TRUE}.
#'
#' @return A data frame or lazy query containing records from the CDM table for patients in the cohort.
#'
#' @examples
#' \dontrun{
#' procedures_data <- load_cdm_table(
#'   conn = conn,
#'   schema = "my_schema",
#'   table_name = "procedures",
#'   cohort = cohort
#' )
#' }
load_cdm_table <- function(conn,
                           schema,
                           table_name,
                           cohort,
                           collect = TRUE) {
  # Validate connection.
  if (!DBI::dbIsValid(conn)) {
    stop("Invalid database connection provided.")
  }
  
  # Validate schema and table_name.
  if (!is.character(schema) || length(schema) != 1 || nchar(schema) == 0) {
    stop("schema must be a non-empty character string.")
  }
  if (!is.character(table_name) || length(table_name) != 1 || nchar(table_name) == 0) {
    stop("table_name must be a non-empty character string.")
  }
  
  # Validate that cohort has a patient_id column.
  if (!is.null(cohort)) {
    if (!("patient_id" %in% colnames(cohort))) {
      stop("The provided cohort does not contain a 'patient_id' column.")
    }
  }
  
  # Check if the table exists in the specified schema.
  if (!DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = table_name))) {
    stop(paste0("Table '", table_name, "' not found in schema '", schema, "'."))
  }
  
  message("Loading table: ", table_name, " from schema: ", schema, "...")
  
  if (is.null(cohort)) {
    message("No ICD-10 cohort provided. Loading the entire table.")
    cdm_query <- tbl(conn, in_schema(schema, table_name))
  } else {
    # Load the table and restrict to the patient cohort.
    cdm_query <- tbl(conn, in_schema(schema, table_name)) %>%
      inner_join(cohort, by = "patient_id", copy = TRUE)
  }
  
  # Return either a lazy query or a collected data frame.
  if (collect) {
    return(cdm_query %>% collect())
  } else {
    return(cdm_query)
  }
}

#' Load Multiple CDM Tables for a Specified Patient Cohort
#'
#' This function loads multiple CDM tables (specified as a character vector) from the given schema,
#' filtering each to include only records for patients in the provided cohort.
#'
#' @param conn A valid DBI connection object.
#' @param schema A character string specifying the database schema containing the CDM tables.
#'   Defaults to the global variable \code{standardised_schema} if available.
#' @param table_names A character vector of table names to load.
#' @param cohort A data frame or lazy query that contains a \code{patient_id} column.
#'    Can be NULL, in which case the entire table is loaded without filtering.
#' @return A named list of data frames or lazy queries corresponding to each specified table.
#'
#' @examples
#' \dontrun{
#' tables_to_load <- c("procedures", "observations", "medications")
#' cdm_tables <- load_multiple_cdm_tables(
#'   conn = conn,
#'   schema = "my_schema",
#'   table_names = tables_to_load,
#'   cohort = cohort
#' )
#' }
load_multiple_cdm_tables <- function(conn,
                                     schema = standardised_schema,
                                     table_names,
                                     cohort,
                                     collect = TRUE) {
  # Validate connection.
  if (!DBI::dbIsValid(conn)) {
    stop("Invalid database connection provided.")
  }
  
  # Validate schema.
  if (!is.character(schema) || length(schema) != 1 || nchar(schema) == 0) {
    stop("schema must be a non-empty character string.")
  }
  
  # Validate table_names.
  if (!is.character(table_names) || length(table_names) == 0) {
    stop("table_names must be a non-empty character vector.")
  }
  
  # Validate that cohort has a patient_id column.
  if (!is.null(cohort)) {
    if (!("patient_id" %in% colnames(cohort))) {
      stop("The provided cohort does not contain a 'patient_id' column.")
    }
  }
  
  # Load each table using the load_cdm_table function.
  # Run through each of these on the database before collecting to ensure correct syntax in filters.
  table_queries <- lapply(
    X = table_names,
    FUN = function(tbl_name) {
      load_cdm_table(conn = conn, schema = schema, table_name = tbl_name, cohort = cohort, collect = FALSE)
    }
  )
  
  if (collect) {
    # Collect the results into a list of data frames.
    loaded_tables <- lapply(
      X = table_queries,
      FUN = function(tbl) {
        tbl %>% collect()
      }
    )
  } else {
    # Keep the results as lazy queries.
    loaded_tables <- table_queries
  }
  
  # Assign table names to the resulting list.
  names(loaded_tables) <- table_names
  
  return(loaded_tables)
}

#' Load a Filtered CDM Table from a Single Schema Using a Filter Expression
#'
#' This function retrieves a CDM table from the database given a specific schema and table name,
#' joins it with a provided patient cohort (which must include a `patient_id` column), and applies a
#' filter expression to restrict the records. The filter expression should be provided as a complete R
#' expression in string form, referencing columns in the table (e.g.,
#' `"grepl('aspirin', drug_name, ignore.case = TRUE)"` or `"observation_name == 'height' & observation_value < 160"`).
#'
#' If an empty string is passed as filter_expression, no filtering is applied and the complete table (joined with
#' the cohort) is returned.
#'
#' @param conn A valid DBI connection object.
#' @param schema A character string specifying the schema name where the table resides.
#' @param table A character string specifying the table name.
#' @param filter_expression A string representing a complete R expression to filter the table.
#'        If empty, no filtering will be performed.
#' @param cohort A data frame or lazy query that contains a `patient_id` column.
#'    Can be NULL, in which case the entire table is loaded without filtering.
#' @param collect Logical. If \code{TRUE}, the resulting filtered table is collected into memory.
#'
#' @return A filtered table (data frame or lazy query) containing only the records that match the filter
#'         expression (or all records if filter_expression is empty).
#'
#' @examples
#' \dontrun{
#' # Returns only records where drug_name contains "aspirin"
#' filtered_table <- load_filtered_cdm_table_single(
#'   conn = conn,
#'   schema = "my_schema",
#'   table = "medications",
#'   filter_expression = "grepl('aspirin', drug_name, ignore.case = TRUE)",
#'   cohort = cohort,
#'   collect = TRUE
#' )
#'
#' # Returns the complete table (joined with the cohort) because the filter_expression is empty.
#' complete_table <- load_filtered_cdm_table_single(
#'   conn = conn,
#'   schema = "my_schema",
#'   table = "medications",
#'   filter_expression = "",
#'   cohort = cohort,
#'   collect = TRUE
#' )
#' }
load_filtered_cdm_table <- function(conn, schema, table, filter_expression, cohort, collect = TRUE) {
  # Validate the database connection.
  if (!DBI::dbIsValid(conn)) {
    stop("Invalid database connection provided.")
  }
  
  # Validate required parameters.
  if (!is.character(schema) || nchar(schema) == 0) {
    stop("Parameter 'schema' must be a non-empty character string.")
  }
  if (!is.character(table) || nchar(table) == 0) {
    stop("Parameter 'table' must be a non-empty character string.")
  }
  
  if (!is.null(cohort)) {
    if (!("patient_id" %in% colnames(cohort))) {
      stop("The provided cohort does not contain a 'patient_id' column.")
    }
  }
  
  # Check if the specified table exists in the given schema.
  if (!DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = table))) {
    stop(sprintf("Table '%s' not found in schema '%s'.", table, schema))
  }
  
  # Retrieve the table and join it with the cohort (if applicable).
  if (is.null(cohort)) {
    message("No ICD-10 cohort provided. Loading the entire table.")
    tbl_obj <- tbl(conn, in_schema(schema, table))
  } else {
    # Load the table and restrict to the patient cohort.
    tbl_obj <- tbl(conn, in_schema(schema, table)) %>%
      inner_join(cohort, by = "patient_id", copy = TRUE)
  }
  
  # If filter_expression is non-empty, parse and apply it.
  if (nchar(trimws(filter_expression)) > 0) {
    filter_expr <- parse_expr(filter_expression)
    tbl_obj <- tbl_obj %>% filter(!!filter_expr)
  }
  
  # Optionally collect the data into memory.
  if (collect) {
    tbl_obj <- tbl_obj %>% collect()
  }
  
  return(tbl_obj)
}

#' Load Multiple Filtered CDM Tables Using a Simplified Tribble Specification
#'
#' This function retrieves multiple CDM tables from the database based on a provided tibble
#' that specifies, for each table, the schema, table name, and a filter expression. The filter
#' expression is provided as a complete R expression (in string form) that is evaluated in the
#' context of the table (for example, it can reference any column in the table). If the filter
#' expression is empty (or contains only whitespace), no filtering is performed and the complete
#' table (after joining with the cohort) is returned.
#'
#' Each table is joined with a patient cohort (which must contain a `patient_id` column) and filtered
#' accordingly.
#'
#' @param conn A valid DBI connection object.
#' @param table_info A tibble (or data frame) with the following required columns:
#'   - `schema`: The name of the schema where the table resides.
#'   - `table`: The name of the table.
#'   - `filter_expression`: A complete R expression (as a string) to filter the table.
#'       If empty, no filtering is performed.
#' @param cohort A data frame or lazy query that contains a `patient_id` column.
#'    Can be NULL, in which case the entire table is loaded without filtering.
#' @param collect Logical. If \code{TRUE}, each filtered table is collected into memory.
#'
#' @return A named list of filtered tables (either lazy queries or in-memory data frames).
#'
#' @examples
#' \dontrun{
#' table_info <- tibble::tribble(
#'   ~schema,    ~table,         ~filter_expression,
#'   "schema1",  "medications",  "grepl('aspirin', drug_name, ignore.case = TRUE)",
#'   "schema2",  "observations", "observation_value < 5",
#'   "schema3",  "demographics", "" # Return the complete table after joining with cohort.
#' )
#'
#' filtered_tables <- load_multiple_filtered_cdm_tables(
#'   conn = conn,
#'   table_info = table_info,
#'   cohort = cohort,
#'   collect = TRUE
#' )
#' }
load_multiple_filtered_cdm_tables <- function(conn, table_info, cohort, collect = TRUE) {
  # Validate connection.
  if (!DBI::dbIsValid(conn)) {
    stop("Invalid database connection provided.")
  }
  
  # Ensure table_info contains the required columns.
  required_cols <- c("schema", "table", "filter_expression")
  missing_cols <- setdiff(required_cols, names(table_info))
  if (length(missing_cols) > 0) {
    stop("table_info is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check that there are no duplicate table entries.
  dup_tables <- table_info$table[duplicated(table_info$table)]
  if (length(dup_tables) > 0) {
    stop(
      "Each table should be queried once. Duplicate entries found for table(s): ",
      paste(unique(dup_tables), collapse = ", "),
      ". If multiple conditions are needed, combine them in the filter_expression column using the '&' operator."
    )
  }
  
  # Validate that cohort has a patient_id column.
  if (!is.null(cohort)) {
    if (!("patient_id" %in% colnames(cohort))) {
      stop("The provided cohort does not contain a 'patient_id' column.")
    }
  }
  
  # Define an inner helper function to process a single row.
  process_row <- function(row) {
    schema <- as.character(row[["schema"]])
    table_name <- as.character(row[["table"]])
    filter_expr_str <- as.character(row[["filter_expression"]])
    
    if (nchar(schema) == 0) stop("Schema is empty in one row.")
    if (nchar(table_name) == 0) stop("Table is empty in one row.")
    
    # Check if the table exists.
    if (!DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = table_name))) {
      stop(sprintf("Table '%s' not found in schema '%s'.", table_name, schema))
    }
    
    # Retrieve the table and join it with the cohort (if applicable).
    if (is.null(cohort)) {
      message("No ICD-10 cohort provided. Loading the entire table.")
      tbl_obj <- tbl(conn, in_schema(schema, table_name))
    } else {
      # Load the table and restrict to the patient cohort.
      tbl_obj <- tbl(conn, in_schema(schema, table_name)) %>%
        inner_join(cohort, by = "patient_id", copy = TRUE)
    }
    
    # If the filter_expression is non-empty (after trimming whitespace), parse and apply it.
    if (nchar(trimws(filter_expr_str)) > 0) {
      filter_expr <- rlang::parse_expr(filter_expr_str)
      tbl_obj <- tbl_obj %>% filter(!!filter_expr)
    }
    
    return(tbl_obj)
  }
  
  # Process each row of table_info.
  rows_list <- split(table_info, seq(nrow(table_info)))
  result_list <- lapply(rows_list, process_row)
  
  # If collect is TRUE, collect the results into data frames.
  if (collect) {
    result_list <- lapply(result_list, function(tbl) {
      tbl %>% collect()
    })
  }
  
  # Name the list elements using the table names.
  names(result_list) <- table_info$table
  
  return(result_list)
}
