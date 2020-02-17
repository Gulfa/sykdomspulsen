is_mssql <- function(conn){
  return(conn@info$dbms.name=="Microsoft SQL Server")
}

#' use_db
#' @param conn a
#' @param db a
#' @export
use_db <- function(conn, db) {
  tryCatch({
    a <- DBI::dbExecute(conn, glue::glue({
      "USE {db};"
    }))
  }, error = function(e){
    a <- DBI::dbExecute(conn, glue::glue({
      "CREATE DATABASE {db};"
    }))
    a <- DBI::dbExecute(conn, glue::glue({
      "USE {db};"
    }))
  })
}

#' get_field_types
#' @param conn a
#' @param dt a
get_field_types <- function(conn, dt) {
  field_types <- vapply(dt, DBI::dbDataType,
    dbObj = conn,
    FUN.VALUE = character(1)
  )
  return(field_types)
}

random_uuid <- function() {
  x <- uuid::UUIDgenerate(F)
  x <- gsub("-", "", x)
  x <- paste0("a", x)
  x
}

random_file <- function(folder, extension = ".csv") {
  fs::path(folder, paste0(random_uuid(), extension))
}

write_data_infile <- function(
  dt,
  file = "/xtmp/x123.csv",
  colnames=T,
  eol="\n",
  quote = "auto",
  na = "\\N"
  ) {
  fwrite(dt,
    file = file,
    logical01 = T,
    na = na,
    col.names=colnames,
    eol=eol,
    quote = quote,
    scipen=12
  )
}

load_data_infile <- function(
  conn,
  db_config,
  table,
  dt,
  file
) UseMethod("load_data_infile")

load_data_infile.default <- function(conn = NULL, db_config = NULL, table, dt = NULL, file = "/xtmp/x123.csv") {
  t0 <- Sys.time()

  if (is.null(conn) & is.null(db_config)) {
    stop("conn and db_config both have error")
  } else if (is.null(conn) & !is.null(db_config)) {
    conn <- get_db_connection(db_config = db_config)
    use_db(conn, db_config$db)
    on.exit(DBI::dbDisconnect(conn))
  }

  correct_order <- DBI::dbListFields(conn, table)
  if(length(correct_order)>0) dt <- dt[,correct_order,with=F]
  write_data_infile(dt = dt, file = file)
  on.exit(fs::file_delete(file), add = T)

  sep <- ","
  eol <- "\n"
  quote <- '"'
  skip <- 0
  header <- T
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  sql <- paste0(
    "LOAD DATA INFILE ", DBI::dbQuoteString(conn, path), "\n",
    "INTO TABLE ", DBI::dbQuoteIdentifier(conn, table), "\n",
    "CHARACTER SET utf8", "\n",
    "FIELDS TERMINATED BY ", DBI::dbQuoteString(conn, sep), "\n",
    "OPTIONALLY ENCLOSED BY ", DBI::dbQuoteString(conn, quote), "\n",
    "LINES TERMINATED BY ", DBI::dbQuoteString(conn, eol), "\n",
    "IGNORE ", skip + as.integer(header), " LINES \n",
    "(", paste0(correct_order, collapse = ","), ")"
  )
  DBI::dbExecute(conn, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if(config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  invisible()
}

`load_data_infile.Microsoft SQL Server` <- function(
  conn = NULL,
  db_config = NULL,
  table,
  dt,
  file = tempfile()
  ) {
  a <- Sys.time()

  if (is.null(conn) & is.null(db_config)) {
    stop("conn and db_config both have error")
  } else if (is.null(conn) & !is.null(db_config)) {
    conn <- get_db_connection(db_config = db_config)
    use_db(conn, db_config$db)
    on.exit(DBI::dbDisconnect(conn))
  }

  correct_order <- DBI::dbListFields(conn, table)
  if(length(correct_order)>0) dt <- dt[,correct_order,with=F]
  write_data_infile(
    dt = dt,
    file = file,
    colnames=F,
    eol = "\n",
    quote = F,
    na="")
  on.exit(fs::file_delete(file), add = T)

  format_file <- tempfile()
  on.exit(fs::file_delete(format_file), add = T)

  args <- c(
    table,
    "format" ,
    "nul",
    "-q",
    "-c",
    "-t,",
    "-f",
    format_file,
    "-S",
    db_config$server,
    "-d",
    db_config$db,
    "-U",
    db_config$user,
    "-P",
    db_config$password
  )
  processx::run(
    "bcp",
    args,
    echo_cmd = F
  )

  args <- c(
    table,
    "in" ,
    file,
    "-S",
    db_config$server,
    "-d",
    db_config$db,
    "-U",
    db_config$user,
    "-P",
    db_config$password,
    "-f",
    format_file
  )
  system2(
    "bcp",
    args=args,
    stdout=NULL
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  if(config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  invisible()
}

######### upsert_load_data_infile

upsert_load_data_infile <- function(
  conn = NULL,
  db_config,
  table,
  dt,
  file,
  fields,
  keys,
  drop_indexes
){
  if (is.null(conn) & is.null(db_config)) {
    stop("conn and db_config both have error")
  } else if (is.null(conn) & !is.null(db_config)) {
    conn <- get_db_connection(db_config = db_config)
    use_db(conn, db_config$db)
    on.exit(DBI::dbDisconnect(conn))
  }

  upsert_load_data_infile_internal(
    conn = conn,
    db_config = db_config,
    table = table,
    dt = dt,
    file = file,
    fields = fields,
    keys = keys,
    drop_indexes = drop_indexes
  )

}

upsert_load_data_infile_internal <- function(
  conn,
  db_config,
  table,
  dt,
  file,
  fields,
  keys,
  drop_indexes
  ) UseMethod("upsert_load_data_infile_internal")

upsert_load_data_infile_internal.default <- function(
  conn = NULL,
  db_config = NULL,
  table,
  dt,
  file = "/xtmp/x123.csv",
  fields,
  keys = NULL,
  drop_indexes = NULL
) {
  temp_name <- random_uuid()
  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(conn, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(conn, sql)

  # TO SPEED UP EFFICIENCY DROP ALL INDEXES HERE
  if (!is.null(drop_indexes)) {
    for (i in drop_indexes) {
      try(
        DBI::dbExecute(
          conn,
          glue::glue("ALTER TABLE `{temp_name}` DROP INDEX `{i}`")
        ),
        TRUE
      )
    }
  }

  load_data_infile(
    conn = conn,
    db_config = db_config,
    table = temp_name,
    dt = dt,
    file = file
    )

  t0 <- Sys.time()

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql <- glue::glue("
    INSERT INTO {table} SELECT {vals_fields} FROM {temp_name}
    ON DUPLICATE KEY UPDATE {vals};
    ")
  DBI::dbExecute(conn, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if(config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  invisible()
}

`upsert_load_data_infile_internal.Microsoft SQL Server` <- function(
  conn = NULL,
  db_config = NULL,
  table,
  dt,
  file = tempfile(),
  fields,
  keys,
  drop_indexes = NULL
) {
  # conn <- schema$output$conn
  # db_config <- config$db_config
  # table <- schema$output$db_table
  # dt <- data_clean
  # file <- tempfile()
  # fields <- schema$output$db_fields
  # keys <- schema$output$keys
  # drop_indexes <- NULL

  temp_name <- paste0("tmp",random_uuid())

  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(conn, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE 1 = 0;")
  DBI::dbExecute(conn, sql)

  load_data_infile(
    conn = conn,
    db_config = db_config,
    table = temp_name,
    dt = dt,
    file = file
    )

  a <- Sys.time()
  add_index(
    conn = conn,
    table = temp_name,
    keys = keys
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue("{t} = {s}", t=paste0("t.",keys), s=paste0("s.",keys))
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  sql_update_set <- glue::glue("{t} = {s}", t=paste0("t.",fields), s=paste0("s.",fields))
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse=", ")
  sql_insert_s_fields <- paste0(paste0("s.",fields), collapse=", ")

  sql <- glue::glue("
  MERGE {table} t
  USING {temp_name} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED BY TARGET
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  ")

  DBI::dbExecute(conn, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  if(config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  invisible()
}

######### create_table
create_table <- function(conn, table, fields, keys) UseMethod("create_table")

create_table.default <- function(conn, table, fields, keys=NULL) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "TEXT CHARACTER SET utf8 COLLATE utf8_unicode_ci"

  sql <- DBI::sqlCreateTable(conn, table, fields_new,
                             row.names = F, temporary = F
  )
  DBI::dbExecute(conn, sql)
}

`create_table.Microsoft SQL Server` <- function(conn, table, fields, keys=NULL) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "NVARCHAR (50)"
  fields_new[fields == "DOUBLE"] <- "FLOAT"
  fields_new[fields == "BOOLEAN"] <- "BIT"

  if(!is.null(keys)) fields_new[names(fields_new) %in% keys] <- paste0(fields_new[names(fields_new) %in% keys], " NOT NULL")
  sql <- DBI::sqlCreateTable(conn, table, fields_new,
                             row.names = F, temporary = F
  )
  DBI::dbExecute(conn, sql)
}

######### add_constraint
add_constraint <- function(conn, table, keys) UseMethod("add_constraint")

add_constraint.default <- function(conn, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}")
  sql <- glue::glue("
          ALTER table {table}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});")
  #print(sql)
  a <- DBI::dbExecute(conn, sql)
  # DBI::dbExecute(conn, "SHOW INDEX FROM x");
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if(config$verbose) message(glue::glue("Added constraint {constraint} in {dif} seconds to {table}"))
}

######### drop_constraint
drop_constraint <- function(conn, table) UseMethod("drop_constraint")

drop_constraint.default <- function(conn, table) {
  constraint <- glue::glue("PK_{table}")
  sql <- glue::glue("
          ALTER table {table}
          DROP CONSTRAINT {constraint};")
  #print(sql)
  try(a <- DBI::dbExecute(conn, sql),TRUE)
}

######### add_index
add_index <- function(conn, table, keys) UseMethod("add_index")

add_index.default <- function(conn, table, keys, index = random_uuid()) {
  keys <- glue::glue_collapse(keys, sep = ", ")
  index <- glue::glue("IND_{index}")

  sql <- glue::glue("
    ALTER TABLE `{table}` ADD INDEX `{index}` ({keys})
    ;")
  #print(sql)
  try(a <- DBI::dbExecute(conn, sql),TRUE)
}

`add_index.Microsoft SQL Server` <- function(conn, table, keys, index = random_uuid()) {
  keys <- glue::glue_collapse(keys, sep = ", ")
  index <- glue::glue("IND_{index}")

  sql <- glue::glue("
    CREATE UNIQUE CLUSTERED INDEX {index} ON {table} ({keys})
    ;")
  #print(sql)
  try(a <- DBI::dbExecute(conn, sql),TRUE)
}


drop_all_rows <- function(conn, table) {
  a <- DBI::dbExecute(conn, glue::glue({
    "DELETE FROM {table};"
  }))
}

drop_rows_where <- function(conn, table, condition) {
  t0 <- Sys.time()
  a <- DBI::dbExecute(conn, glue::glue({
    "DELETE FROM {table} WHERE {condition};"
  }))
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  if(config$verbose) message(glue::glue("Deleted rows in {dif} seconds from {table}"))
}


#' get_db_connection
#' @param driver driver
#' @param server server
#' @param port port
#' @param user user
#' @param password password
#' @param db_config A list containing driver, server, port, user, password
#' @export get_db_connection
get_db_connection <- function(
                              driver = NULL,
                              server = NULL,
                              port = NULL,
                              user = NULL,
                              password = NULL,
                              db_config = config$db_config
                              ) {

  if(!is.null(db_config) & is.null(driver)){
    driver <- db_config$driver
  }
  if(!is.null(db_config) & is.null(server)){
    server <- db_config$server
  }
  if(!is.null(db_config) & is.null(port)){
    port <- db_config$port
  }
  if(!is.null(db_config) & is.null(user)){
    user <- db_config$user
  }
  if(!is.null(db_config) & is.null(password)){
    password <- db_config$password
  }

  if(db_config$driver %in% c("ODBC Driver 17 for SQL Server")){
    return(
      DBI::dbConnect(
        odbc::odbc(),
        driver = driver,
        server = server,
        port = port,
        uid = user,
        Pwd = password,
        encoding = "utf8"
      ))
  } else {
    return(
      DBI::dbConnect(
        odbc::odbc(),
        driver = driver,
        server = server,
        port = port,
        user = user,
        password = password,
        encoding = "utf8"
      ))
  }
}

#' tbl
#' @param table table
#' @param db db
#' @export
tbl <- function(table, db = "sykdomspuls") {
  if (is.null(connections[[db]])) {
    connections[[db]] <- get_db_connection()
    use_db(connections[[db]], db)
  }
  return(dplyr::tbl(connections[[db]], table))
}

#' list_tables
#' @param db db
#' @export
list_tables <- function(db = "sykdomspuls") {
  if (is.null(connections[[db]])) {
    connections[[db]] <- get_db_connection()
    use_db(connections[[db]], db)
  }
  return(DBI::dbListTables(connections[[db]]))
}


#' drop_table
#' @param table table
#' @param db db
#' @export
drop_table <- function(table, db = "sykdomspuls") {
  if (is.null(connections[[db]])) {
    connections[[db]] <- get_db_connection()
    use_db(connections[[db]], db)
  }
  return(try(DBI::dbRemoveTable(connections[[db]], name = table), TRUE))
}


#' schema class description
#'
#' @import data.table
#' @import R6
#' @export schema
schema <- R6Class("schema",
  public = list(
    dt = NULL,
    conn = NULL,
    db_config = NULL,
    db_table = NULL,
    db_field_types = NULL,
    db_load_folder = NULL,
    keys = NULL,
    keys_with_length = NULL,
    check_fields_match = FALSE,
    initialize = function(dt = NULL, conn = NULL, db_config = NULL, db_table, db_field_types, db_load_folder, keys, check_fields_match = TRUE) {
      self$dt <- dt
      self$conn <- conn
      self$db_config <- db_config
      self$db_table <- db_table
      self$db_field_types <- db_field_types
      self$db_load_folder <- db_load_folder
      self$keys <- keys
      self$keys_with_length <- keys
      self$check_fields_match <- check_fields_match

      ind <- self$db_field_types[self$keys] == "TEXT"
      if (sum(ind) > 0) {
        self$keys_with_length[ind] <- paste0(self$keys_with_length[ind], " (40)")
      }
      if (!is.null(self$conn)) self$db_create_table()
    },
    db_connect = function(db_config = self$db_config) {
      self$conn <- get_db_connection(db_config=db_config)
      use_db(self$conn, db_config$db)
      self$db_create_table()
    },
    db_disconnect = function() {
      if (!is.null(self$conn)) DBI::dbDisconnect(self$conn)
    },
    db_add_constraint = function(){
      add_constraint(
        conn = self$conn,
        table = self$db_table,
        keys = self$keys
        )
    },
    db_drop_constraint = function(){
      drop_constraint(
        conn = self$conn,
        table = self$db_table
      )
    },
    db_create_table = function() {
      create_tab <- TRUE
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        if (self$check_fields_match & !self$db_check_fields_match()) {
          message(glue::glue("Dropping table {self$db_table} because fields dont match"))
          self$db_drop_table()
        } else {
          create_tab <- FALSE
        }
      }

      if (create_tab) {
        message(glue::glue("Creating table {self$db_table}"))
        create_table(self$conn, self$db_table, self$db_field_types, self$keys)
        self$db_add_constraint()
      }
    },
    db_drop_table = function() {
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        DBI::dbRemoveTable(self$conn, self$db_table)
      }
    },
    db_check_fields_match = function() {
      fields <- DBI::dbListFields(self$conn, self$db_table)
      if (sum(!fields %in% names(self$db_field_types)) > 0 | sum(!names(self$db_field_types) %in% fields) > 0) {
        return(FALSE)
      }
      return(TRUE)
    },
    db_load_data_infile = function(newdata, verbose = TRUE) {
      infile <- random_file(self$db_load_folder)
      load_data_infile(
        conn = self$conn,
        db_config = self$db_config,
        table = self$db_table,
        dt = newdata,
        file = infile
      )
    },
    db_upsert_load_data_infile = function(newdata, drop_indexes = NULL, verbose = TRUE) {
      infile <- random_file(self$db_load_folder)

      upsert_load_data_infile(
        # conn = self$conn,
        db_config = self$db_config,
        table = self$db_table,
        dt = newdata[, names(self$db_field_types), with = F],
        file = infile,
        fields = names(self$db_field_types),
        keys = self$keys,
        drop_indexes = drop_indexes
      )
    },
    db_drop_all_rows = function() {
      drop_all_rows(self$conn, self$db_table)
    },
    db_drop_rows_where = function(condition){
      drop_rows_where(self$conn, self$db_table, condition)
    },
    get_data = function(...) {
      dots <- dplyr::quos(...)
      params <- c()

      for (i in seq_along(dots)) {
        temp <- rlang::quo_text(dots[[i]])
        temp <- stringr::str_extract(temp, "^[a-zA-Z0-9]+")
        params <- c(params, temp)
      }

      if (length(params) > length(keys)) {
        stop("Too many requests")
      }
      if (sum(!params %in% keys)) {
        stop("names(...) not in keys")
      }
      if (nrow(self$dt) > 0 | ncol(self$dt) > 0) {
        x <- self$get_data_dt(...)
      } else {
        x <- self$get_data_db(...)
      }
      return(x)
    },
    get_data_dt = function(...) {
      dots <- dplyr::quos(...)
      txt <- c()
      for (i in seq_along(dots)) {
        txt <- c(txt, rlang::quo_text(dots[[i]]))
      }
      if (length(txt) == 0) {
        return(self$dt)
      } else {
        txt <- paste0(txt, collapse = "&")
        return(self$dt[eval(parse(text = txt))])
      }
    },
    get_data_db = function(...) {
      dots <- dplyr::quos(...)
      retval <- self$conn %>%
        dplyr::tbl(self$db_table) %>%
        dplyr::filter(!!!dots) %>%
        dplyr::collect()
      setDT(retval)
      return(retval)
    },
    dplyr_tbl = function() {
      retval <- self$conn %>%
        dplyr::tbl(self$db_table)
      return(retval)
    },

    add_index_db = function() {
      txt <- glue::glue_collapse(glue::glue("`{self$keys}`(20)"), sep = ",")
      DBI::dbExecute(
        self$conn,
        glue::glue("ALTER TABLE `{self$db_table}` ADD INDEX `ind1` ({txt})")
      )
    },

    identify_dt_that_exists_in_db = function() {
      setkeyv(self$dt, self$keys)
      from_db <- self$get_data_db()
      setkeyv(from_db, self$keys)
      self$dt[, exists_in_db := FALSE]
      self$dt[from_db, exists_in_db := TRUE]
    }
  ),
  private = list(
    finalize = function() {
      # self$db_disconnect()
    }
  )
)

