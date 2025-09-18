# Any helper functions used throughout the project should be placed here.
# Consider adding them to the ProfessionalServices package if relevant.

#' Create a PostgreSQL Database Connection
#'
#' This function establishes a connection to a PostgreSQL database using the provided
#' connection parameters. It prints a message indicating which database it is connecting to.
#'
#' @param dbname A character string specifying the name of the database to connect to. The default
#'   is `primary_dbname`.
#' @param host A character string specifying the host address of the PostgreSQL server. The default
#'   is `POSTGRESQL_HOST`.
#' @param port An integer specifying the port number on which the PostgreSQL server is listening.
#'   The default is `POSTGRESQL_PORT`.
#' @param password A character string for the database password. The default is `POSTGRESQL_PASSWORD`.
#' @param user A character string for the username used for authentication. The default is
#'   `POSTGRESQL_USER`.
#'
#' @return A DBI connection object representing the connection to the PostgreSQL database.
#'
#' @details This function utilizes the `RPostgres` package to create a connection to a PostgreSQL
#'   database. Ensure that all required connection parameters are correctly specified and that the
#'   database is accessible.
#'
#' @examples
#' \dontrun{
#' # Create a database connection
#' conn <- create_db_connection(
#'   dbname = "my_database",
#'   host = "localhost",
#'   port = 5432,
#'   password = "secret_password",
#'   user = "db_user"
#' )
#' }
#'
#' @export
create_db_connection <- function(
    dbname = primary_dbname,
    host = POSTGRESQL_HOST,
    port = POSTGRESQL_PORT,
    password = POSTGRESQL_PASSWORD,
    user = POSTGRESQL_USER) {
  print(paste0("Connecting to DB: ", dbname))
  return(
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      port = port,
      password = password,
      user = user
    )
  )
}

#' Disconnect from a Database
#'
#' This function terminates an active database connection.
#'
#' @param connection A DBI database connection object that is currently active.
#'
#' @return A logical value indicating whether the disconnection was successful.
#'
#' @details This function uses `DBI::dbDisconnect` to safely close a database connection.
#'   It is important to disconnect from the database once operations are completed to free up
#'   resources.
#'
#' @examples
#' \dontrun{
#' # Assume 'conn' is an active database connection
#' success <- end_db_connection(conn)
#' }
#'
#' @export
end_db_connection <- function(connection) {
  DBI::dbDisconnect(connection)
}

#' Obfuscate Small Count Values in a Dataframe Column
#'
#' This function converts a vector of numeric count values into character values and
#' obfuscates values that are less than or equal to 5 by replacing them with "≤5".
#'
#' @param col A numeric vector representing count values. The function applies a transformation
#'   to each element: values between 1 and 5 (inclusive) are replaced with the string "≤5",
#'   while all other values are converted to their character representation.
#'
#' @return A character vector where small count values (between 1 and 5) have been replaced with "≤5",
#'   and all other values have been converted to character strings.
#'
#' @details The function uses `dplyr::case_when` to conditionally transform the input vector. The
#'   special syntax `{{col}}` is used for tidy evaluation, making the function compatible with
#'   dplyr pipelines. This is particularly useful when summarizing or anonymizing data where small counts
#'   might pose a risk of re-identification.
#'
#' @examples
#' \dontrun{
#' # Example vector of count values
#' counts <- c(2, 10, 5, 7, 1, 20)
#'
#' # Apply the obfuscation to hide small numbers
#' result <- hideSmallNumbers(counts)
#'
#' # Result will be: c("≤5", "10", "≤5", "7", "≤5", "20")
#' print(result)
#' }
#'
#' @export
hideSmallNumbers <- function(col) {
  return(case_when(
    {{ col }} >= 1 & {{ col }} <= 5 ~ "≤5",
    .default = as.character({{ col }})
  ))
}



# Run all .R files within the folder that contains the file current_filepath, with the option to not include current_filepath
source_entire_folder <- function(current_filepath, source_current = FALSE, show_warnings = TRUE, verbose = TRUE) {
  filedir <- dirname(current_filepath)
  files <- list.files(filedir, full.names = TRUE, recursive = TRUE, pattern = "*\\.R$")

  if (!source_current) {
    files <- files[files != current_filepath]
  }

  if (!length(files) && show_warnings) {
    warning("No R files in ", filedir)
  }

  for (f in files) {
    if (verbose) {
      cat("sourcing: ", f, "\n")
    }
    ## TODO:  add caught whether error or not and return that
    try(source(f, local = FALSE, echo = FALSE), silent = !verbose)
  }
  return(invisible(NULL))
}


#' Retrieve Schemas from the Database
#'
#' This function queries the database to extract all distinct table schemas from
#' the `information_schema.tables` table and returns them in an arranged tibble.
#'
#' @param conn A DBI database connection object. The default is `connection`. Ensure
#'   that this connection is active and valid.
#'
#' @return A tibble (or data frame) with one column, `table_schema`, containing the names
#'   of the schemas present in the database.
#'
#' @details The function executes the SQL query to retrieve all rows from the
#'   `information_schema.tables` view, extracts unique schema names, and then arranges
#'   them in alphabetical order.
#'
#' @examples
#' \dontrun{
#' # Establish a connection (example using RSQLite)
#' connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Get all schemas in the database
#' schemas <- get_schema_in_db(connection)
#'
#' # Print the schemas
#' print(schemas)
#' }
#'
#' @export
get_schema_in_db <- function(conn = connection) {
  DBI::dbGetQuery(conn, "SELECT * FROM information_schema.tables") %>%
    distinct(table_schema) %>%
    arrange(table_schema)
}

#' Retrieve Tables from a Specific Schema
#'
#' This function fetches all distinct table names within a specified schema from
#' the database's `information_schema.tables` view.
#'
#' @param conn A DBI database connection object. The default is `connection`. Make sure
#'   the connection is properly established.
#' @param schema A character string indicating the name of the schema from which to
#'   retrieve the tables.
#'
#' @return A tibble (or data frame) with one column, `table_name`, listing the table names
#'   present in the specified schema.
#'
#' @details The function runs a query to extract all rows from the `information_schema.tables`
#'   view, filters the results to include only those rows where the `table_schema` matches the
#'   provided schema, removes duplicate table names, and arranges the final list in alphabetical order.
#'
#' @examples
#' \dontrun{
#' # Establish a connection (example using RSQLite)
#' connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Retrieve tables from the 'public' schema
#' tables <- get_tables_in_schema(connection, schema = "public")
#'
#' # Display the table names
#' print(tables)
#' }
#'
#' @export
get_tables_in_schema <- function(conn = connection, schema) {
  DBI::dbGetQuery(conn, "SELECT * FROM information_schema.tables") %>%
    filter(table_schema == schema) %>%
    distinct(table_name) %>%
    arrange(table_name)
}


#' Build a `grepl()` Filter Expression as a Character String
#'
#' Quickly construct a character string that can be passed to the *filter-expression*
#' column of the **CDM/Raw data-collection `*_fcns.R` helpers**.
#'
#' @param regex   A **character vector** of one or more regular-expression patterns
#'                (e.g. `c("^C90", "^C91")`).
#'                Patterns are concatenated with a logical **OR** (`|`) so the
#'                resulting expression matches **any** supplied pattern.
#' @param column  A **single quoted string** giving the *column name*
#'                on which to apply the regex. Defaults to `"column"`.
#'
#' @return        A single **character string** in the form
#'                `"grepl('pattern1|pattern2', column_name)"`.
#'                This is designed to drop straight into the
#'                `filter_expression` field of the `tibble::tribble()` used in
#'                `load_multiple_filtered_cdm_tables()` or
#'                `load_multiple_raw_tables()`.
#'
#' @details
#' Building the expression **as a string** rather than inline R code keeps
#' `run_data_collection.R` declarative and compatible with lazy `dbplyr`
#' back-end filtering.  The returned string can be evaluated safely inside the
#' helper functions where the table’s columns are in scope.
#'
#' The function performs basic validation:
#' * `regex` must be a non-empty character vector.
#' * `column` must be a non-empty, length-one character string.
#'
#' @examples
#' # Construct a filter that keeps rows where diagnosis_code starts with C90 or C91
#' build_grepl_expression(
#'   regex  = c("^C90", "^C91"),
#'   column = "diagnosis_code"
#' )
#' #> "grepl('^C90|^C91', diagnosis_code)"
#'
#' # Use the result inside a tribble passed to load_multiple_filtered_cdm_tables()
#' cdm_table_info <- tibble::tribble(
#'   ~schema, ~table,       ~filter_expression,
#'   "stage", "diagnoses",  build_grepl_expression(c("^C90", "^C91"), "diagnosis_code")
#' )
#'
#' @export
build_grepl_expression <- function(regex_list, column, ignore.case = TRUE) {
  ## ---- input validation -----------------------------------------------------
  stopifnot(
    "`regex_list` must be a non-empty character vector" =
      is.character(regex_list) && length(regex_list) > 0 && all(nzchar(regex_list)),
    "`column` must be a single non-empty character string" =
      is.character(column) && length(column) == 1L && nzchar(column)
  )

  ## ---- build expression -----------------------------------------------------
  pattern <- paste0(regex_list, collapse = "|") # OR-concatenate patterns
  expr <- paste0("grepl('", pattern, "', ", column, ", ignore.case = ", ignore.case, ")")

  return(expr)
}

#### PROJECT DATA STORAGE FUNCTIONS ####

# ── 1. SETUP ────────────────────────────────────────────────────────────────────
init_project <- function(proj_root, analysts) {
  # create per‐analyst folders, plus logs
  dir.create(proj_root, showWarnings = FALSE)
  dir.create(path(proj_root, "results"), showWarnings = FALSE)
  for (analyst in analysts) {
    dir.create(path(proj_root, "results", analyst), showWarnings = FALSE)
  }
  dir.create(path(proj_root, "results", "qc_output"), showWarnings = FALSE)
  dir.create(path(proj_root, "logs"), showWarnings = FALSE)

  # initialize empty audit log if needed
  audit_log <- path(proj_root, "logs", "audit_log.csv")
  if (!file_exists(audit_log)) {
    write.csv(
      tibble(
        timestamp = character(),
        analyst   = character(),
        file      = character(),
        action    = character()
      ),
      audit_log
    )
  }
}

# ── 2. SAVE RESULTS ────────────────────────────────────────────────────────────
save_results <- function(data, name, analyst, proj_root = project_root) {
  ts <- format(now(), "%Y%m%d%H%M%S")
  sha <- system("git rev-parse HEAD", intern = TRUE)
  dir <- path(proj_root, "results", analyst)
  file <- path(dir, glue("{name}_{ts}_{sha}.rds"))

  write_rds(data, file)

  # append to audit log
  log_entry <- tibble(
    timestamp = now(),
    analyst   = analyst,
    file      = file,
    action    = "save"
  )
  write_csv(log_entry,
    path(proj_root, "logs", "audit_log.csv"),
    append = TRUE
  )
}


## Jo's QC functions go here
