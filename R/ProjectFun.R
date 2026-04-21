# Project database functions for RQDA

#' Create new project
#' @export
new_proj <- function(path, conName = "qdacon", assignenv = .rqda, ...) {

  # Check write permission
  success <- (file.access(names = dirname(path), mode = 2) == 0)
  if (!success) {
    stop("No write permission to directory: ", dirname(path))
  }

  # Ensure .rqda extension
  path <- paste(gsub("\\.rqda$", "", path), "rqda", sep = ".")

  # Check if file exists
  override <- FALSE
  if (file.exists(path)) {
    # Ask user if they want to overwrite
    override <- gconfirm("A project file already exists at this location. Overwrite it?", icon = "warning")

    if (override && file.access(path, 2) != 0) {
      gmessage("You have no write permission to overwrite the existing file.", icon = "error")
      return(invisible(NULL))
    }

    if (!override) {
      return(invisible(NULL))
    }
  }

  if (!file.exists(path) || override) {
    # Close any existing connection
    tryCatch(
      closeProject(conName = conName, assignenv = assignenv),
      error = function(e) {}
    )

    # Create database connection
    library(RSQLite)
    library(DBI)

    if (Encoding(path) == "UTF-8") {
      Encoding(path) <- "unknown"
    }

    assign(conName,
           dbConnect(drv = dbDriver("SQLite"), dbname = path),
           envir = assignenv)

    con <- get(conName, assignenv)

    # Create all tables with complete schema

    # Source table - files/documents
    if (dbExistsTable(con, "source")) dbRemoveTable(con, "source")
    rqda_exe(paste(
      "CREATE TABLE source (",
      "name TEXT, id INTEGER, file TEXT, memo TEXT, ",
      "owner TEXT, date TEXT, dateM TEXT, status INTEGER)"
    ))

    # Freecode table - list of codes
    if (dbExistsTable(con, "freecode")) dbRemoveTable(con, "freecode")
    rqda_exe(paste(
      "CREATE TABLE freecode (",
      "name TEXT, memo TEXT, owner TEXT, date TEXT, dateM TEXT, ",
      "id INTEGER, status INTEGER, color TEXT)"
    ))

    # Treecode table - code categories hierarchy
    if (dbExistsTable(con, "treecode")) dbRemoveTable(con, "treecode")
    rqda_exe(paste(
      "CREATE TABLE treecode (",
      "cid INTEGER, catid INTEGER, date TEXT, dateM TEXT, ",
      "memo TEXT, status INTEGER, owner TEXT)"
    ))

    # Treefile table - file categories hierarchy
    if (dbExistsTable(con, "treefile")) dbRemoveTable(con, "treefile")
    rqda_exe(paste(
      "CREATE TABLE treefile (",
      "fid INTEGER, catid INTEGER, date TEXT, dateM TEXT, ",
      "memo TEXT, status INTEGER, owner TEXT)"
    ))

    # Filecat table - file categories
    if (dbExistsTable(con, "filecat")) dbRemoveTable(con, "filecat")
    rqda_exe(paste(
      "CREATE TABLE filecat (",
      "name TEXT, fid INTEGER, catid INTEGER, owner TEXT, ",
      "date TEXT, dateM TEXT, memo TEXT, status INTEGER)"
    ))

    # Codecat table - code categories
    if (dbExistsTable(con, "codecat")) dbRemoveTable(con, "codecat")
    rqda_exe(paste(
      "CREATE TABLE codecat (",
      "name TEXT, cid INTEGER, catid INTEGER, owner TEXT, ",
      "date TEXT, dateM TEXT, memo TEXT, status INTEGER)"
    ))

    # Coding table - coded segments
    if (dbExistsTable(con, "coding")) dbRemoveTable(con, "coding")
    rqda_exe(paste(
      "CREATE TABLE coding (",
      "cid INTEGER, fid INTEGER, seltext TEXT, selfirst REAL, ",
      "selend REAL, status INTEGER, owner TEXT, date TEXT, memo TEXT)"
    ))

    # Coding2 table - second coding for inter-rater reliability
    if (dbExistsTable(con, "coding2")) dbRemoveTable(con, "coding2")
    rqda_exe(paste(
      "CREATE TABLE coding2 (",
      "cid INTEGER, fid INTEGER, seltext TEXT, selfirst REAL, ",
      "selend REAL, status INTEGER, owner TEXT, date TEXT, memo TEXT)"
    ))

    # Cases table
    if (dbExistsTable(con, "cases")) dbRemoveTable(con, "cases")
    rqda_exe(paste(
      "CREATE TABLE cases (",
      "name TEXT, memo TEXT, owner TEXT, date TEXT, dateM TEXT, ",
      "id INTEGER, status INTEGER)"
    ))

    # Caselinkage table - links cases to file segments
    if (dbExistsTable(con, "caselinkage")) dbRemoveTable(con, "caselinkage")
    rqda_exe(paste(
      "CREATE TABLE caselinkage (",
      "caseid INTEGER, fid INTEGER, selfirst REAL, selend REAL, ",
      "status INTEGER, owner TEXT, date TEXT, memo TEXT)"
    ))

    # Attributes table
    if (dbExistsTable(con, "attributes")) dbRemoveTable(con, "attributes")
    rqda_exe(paste(
      "CREATE TABLE attributes (",
      "name TEXT, status INTEGER, date TEXT, dateM TEXT, ",
      "owner TEXT, memo TEXT, class TEXT)"
    ))

    # CaseAttr table - case attributes
    if (dbExistsTable(con, "caseAttr")) dbRemoveTable(con, "caseAttr")
    rqda_exe(paste(
      "CREATE TABLE caseAttr (",
      "variable TEXT, value TEXT, caseID INTEGER, ",
      "date TEXT, dateM TEXT, owner TEXT, status INTEGER)"
    ))

    # FileAttr table - file attributes
    if (dbExistsTable(con, "fileAttr")) dbRemoveTable(con, "fileAttr")
    rqda_exe(paste(
      "CREATE TABLE fileAttr (",
      "variable TEXT, value TEXT, fileID INTEGER, ",
      "date TEXT, dateM TEXT, owner TEXT, status INTEGER)"
    ))

    # Journal table
    if (dbExistsTable(con, "journal")) dbRemoveTable(con, "journal")
    rqda_exe(paste(
      "CREATE TABLE journal (",
      "name TEXT, journal TEXT, date TEXT, dateM TEXT, ",
      "owner TEXT, status INTEGER)"
    ))

    # Annotation table
    if (dbExistsTable(con, "annotation")) dbRemoveTable(con, "annotation")
    tryCatch(
      rqda_exe(paste(
        "CREATE TABLE annotation (",
        "fid INTEGER, position INTEGER, annotation TEXT, ",
        "owner TEXT, date TEXT, dateM TEXT, status INTEGER)"
      )),
      error = function(e) {}
    )

    # Image table
    if (dbExistsTable(con, "image")) dbRemoveTable(con, "image")
    rqda_exe(paste(
      "CREATE TABLE image (",
      "name TEXT, id INTEGER, date TEXT, dateM TEXT, ",
      "owner TEXT, status INTEGER)"
    ))

    # ImageCoding table
    if (dbExistsTable(con, "imageCoding")) dbRemoveTable(con, "imageCoding")
    rqda_exe(paste(
      "CREATE TABLE imageCoding (",
      "cid INTEGER, iid INTEGER, x1 INTEGER, y1 INTEGER, ",
      "x2 INTEGER, y2 INTEGER, memo TEXT, date TEXT, ",
      "dateM TEXT, owner TEXT, status INTEGER)"
    ))

    # Project metadata table
    if (dbExistsTable(con, "project")) dbRemoveTable(con, "project")
    rqda_exe(paste(
      "CREATE TABLE project (",
      "databaseversion TEXT, date TEXT, dateM TEXT, ",
      "memo TEXT, about TEXT, imageDir TEXT)"
    ))

    # Insert project metadata
    rqda_exe(sprintf(
      paste(
        "INSERT INTO project (databaseversion, date, about, memo) ",
        "VALUES ('0.4.0', '%s', 'Database created by RQDA4 (RGtk4)', '')"
      ),
      date()
    ))

    message("Project created successfully: ", path)
  }
}

#' Open existing project
#' @export
open_proj <- function(path, conName = "qdacon", assignenv = .rqda, ...) {

  if (!file.exists(path)) {
    stop("Project file does not exist: ", path)
  }

  library(RSQLite)
  library(DBI)

  # Close existing connection if any
  tryCatch({
    if (exists(conName, envir = assignenv)) {
      con <- get(conName, envir = assignenv)
      if (DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    }
  }, error = function(e) {})

  # Check permissions
  if (file.access(path, 2) == 0) {
    # Read and write access
    Encoding(path) <- "unknown"
    assign(conName,
           dbConnect(drv = dbDriver("SQLite"), dbname = path),
           envir = assignenv)
    message("Project opened with read/write access: ", path)
  } else if (file.access(path, 4) == 0) {
    # Read-only access
    Encoding(path) <- "unknown"
    assign(conName,
           dbConnect(drv = dbDriver("SQLite"), dbname = path),
           envir = assignenv)
    warning("Project opened in read-only mode (no write permission)")
  } else {
    stop("No read access to project file: ", path)
  }
}

#' Close project
#' @export
closeProject <- function(conName = "qdacon", assignenv = .rqda, ...) {

  # Check if connection exists
  if (!exists(conName, envir = assignenv, inherits = FALSE)) {
    # No connection to close - this is fine
    return(invisible(NULL))
  }

  tryCatch({
    con <- get(conName, assignenv)

    # Verify connection is valid
    if (!inherits(con, "SQLiteConnection") && !inherits(con, "DBIConnection")) {
      # Not a valid database connection
      rm(list = conName, envir = assignenv)
      return(invisible(NULL))
    }

    # Check if connection is still valid
    is_valid <- tryCatch(DBI::dbIsValid(con), error = function(e) FALSE)

    if (!is_valid) {
      message("Connection is not valid, removing reference")
      rm(list = conName, envir = assignenv)
      return(invisible(NULL))
    }

    # Close any open file viewers
    tryCatch({
      if (exists(".root_edit", envir = assignenv)) {
        dispose(assignenv$.root_edit)
      }
    }, error = function(e) {})

    # Close coding windows
    WidgetList <- ls(envir = assignenv, pattern = "^[.]codingsOf", all.names = TRUE)
    for (i in WidgetList) {
      tryCatch({
        widget <- get(i, envir = assignenv)
        dispose(widget)
      }, error = function(e) {})
    }

    # Update widgets before closing
    tryCatch(closeProjBF(), error = function(e) {})

    # Disconnect database
    disconnected <- tryCatch(
      DBI::dbDisconnect(con),
      error = function(e) {
        message("Error disconnecting database: ", e$message)
        FALSE
      }
    )

    if (!disconnected) {
      warning("Problem closing project connection")
    } else {
      message("Project closed successfully")
    }

    # Remove from environment
    rm(list = conName, envir = assignenv)

  }, error = function(e) {
    message("Error in closeProject: ", e$message)
    # Try to clean up anyway
    if (exists(conName, envir = assignenv, inherits = FALSE)) {
      tryCatch(rm(list = conName, envir = assignenv), error = function(e) {})
    }
  })

  invisible(NULL)
}

#' Check if project is open
#' @export
is_projOpen <- function(envir = .rqda, conName = "qdacon", message = TRUE) {
  open <- FALSE

  tryCatch({
    con <- get(conName, envir)
    if (DBI::dbIsValid(con)) {
      open <- TRUE
    }
  }, error = function(e) {})

  if (!open && message) {
    message("No project is open.")
  }

  return(open)
}

#' Backup project
#' @export
BackupProject <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  con <- .rqda$qdacon
  dbname <- con@dbname
  Encoding(dbname) <- "UTF-8"

  backupname <- sprintf(
    "%s%s.rqda",
    gsub("rqda$", "", dbname),
    format(Sys.time(), "%H%M%S%d%m%Y")
  )

  success <- file.copy(from = dbname, to = backupname, overwrite = FALSE)

  if (success) {
    message("Project backed up to: ", backupname)
  } else {
    warning("Failed to backup project")
  }

  invisible(success)
}

#' Save project as
#' @export
saveAsProject <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  # Get current project path
  current_path <- .rqda$qdacon@dbname

  # Ask for new path
  new_path <- gfile(
    type = "save",
    text = "Save project as..."
  )

  if (identical(new_path, character(0)) || new_path == "") {
    return(invisible(NULL))
  }

  # Ensure .rqda extension
  if (!grepl("\\.rqda$", new_path)) {
    new_path <- paste0(new_path, ".rqda")
  }

  # Copy file
  success <- tryCatch({
    file.copy(current_path, new_path, overwrite = TRUE)
  }, error = function(e) {
    gmessage(paste("Error saving project:", e$message), icon = "error")
    FALSE
  })

  if (success) {
    message("Project saved to: ", new_path)
    gmessage(paste("Project saved to:", new_path), icon = "info")
  }

  invisible(success)
}

#' Upgrade database tables to current version
#' @export
UpgradeTables <- function() {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))

  con <- .rqda$qdacon

  # Check if databaseversion field exists
  Fields <- DBI::dbListFields(con, "project")

  if (!"databaseversion" %in% Fields) {
    rqda_exe("ALTER TABLE project ADD COLUMN databaseversion TEXT")
    rqda_exe("UPDATE project SET databaseversion='0.1.5'")
  }

  # Get current version
  currentVersion <- rqda_sel("SELECT databaseversion FROM project")[[1]]

  if (is.null(currentVersion) || is.na(currentVersion)) {
    currentVersion <- "0.1.5"
  }

  # Perform upgrades based on version
  if (currentVersion < "0.4.0") {
    message("Upgrading database schema to 0.4.0...")

    # Ensure all tables exist
    if (!dbExistsTable(con, "annotation")) {
      tryCatch(
        rqda_exe(paste(
          "CREATE TABLE annotation (",
          "fid INTEGER, position INTEGER, annotation TEXT, ",
          "owner TEXT, date TEXT, dateM TEXT, status INTEGER)"
        )),
        error = function(e) {}
      )
    }

    if (!dbExistsTable(con, "image")) {
      rqda_exe(paste(
        "CREATE TABLE image (",
        "name TEXT, id INTEGER, date TEXT, dateM TEXT, ",
        "owner TEXT, status INTEGER)"
      ))
    }

    if (!dbExistsTable(con, "imageCoding")) {
      rqda_exe(paste(
        "CREATE TABLE imageCoding (",
        "cid INTEGER, iid INTEGER, x1 INTEGER, y1 INTEGER, ",
        "x2 INTEGER, y2 INTEGER, memo TEXT, date TEXT, ",
        "dateM TEXT, owner TEXT, status INTEGER)"
      ))
    }

    # Add missing columns
    tryCatch(rqda_exe("ALTER TABLE freecode ADD COLUMN color TEXT"), error = function(e) {})
    tryCatch(rqda_exe("ALTER TABLE attributes ADD COLUMN class TEXT"), error = function(e) {})
    tryCatch(rqda_exe("ALTER TABLE project ADD COLUMN imageDir TEXT"), error = function(e) {})

    # Update version
    rqda_exe("UPDATE project SET databaseversion='0.4.0'")

    message("Database upgraded to version 0.4.0")
  }
}

#' Project memo
#' @export
ProjectMemo <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  # Get current memo
  memo_data <- rqda_sel("SELECT memo FROM project LIMIT 1")
  current_memo <- if (nrow(memo_data) > 0) memo_data$memo[1] else ""

  # Create window
  win <- gwindow(title = "Project Memo", width = 600, height = 400)

  # Create text view
  txt <- gtext(current_memo, container = win)

  # Save on close
  gSignalConnectR(win$widget, "close-request", function(w) {
    # Get text from buffer
    buffer <- gtkTextViewGetBuffer(txt$textview)
    start_iter <- gtkTextBufferGetStartIter(buffer)
    end_iter <- gtkTextBufferGetEndIter(buffer)
    memo_text <- gtkTextBufferGetText(buffer, start_iter, end_iter, FALSE)

    # Save to database
    tryCatch({
      rqda_exe(sprintf("UPDATE project SET memo = '%s'",
                       gsub("'", "''", memo_text)))  # Escape quotes
      message("Project memo saved")
    }, error = function(e) {
      message("Error saving project memo: ", e$message)
    })

    FALSE  # Allow close
  })
}

#' Close all coding windows
#' @export
CloseAllCoding <- function() {
  # Find all coding windows
  WidgetList <- ls(envir = .rqda, pattern = "^[.]codingsOf", all.names = TRUE)

  if (length(WidgetList) == 0) {
    message("No coding windows open")
    return(invisible(NULL))
  }

  closed_count <- 0
  for (widget_name in WidgetList) {
    tryCatch({
      widget <- get(widget_name, envir = .rqda)
      dispose(widget)
      closed_count <- closed_count + 1
    }, error = function(e) {
      message("Could not close ", widget_name, ": ", e$message)
    })
  }

  message("Closed ", closed_count, " coding window(s)")
}

#' Update code names widget
#' @export
CodeNamesUpdate <- function(sortByTime = FALSE, decreasing = FALSE) {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))

  codes <- rqda_sel("SELECT name, date, id FROM freecode WHERE status = 1 ORDER BY LOWER(name)")

  if (!is.null(codes) && nrow(codes) > 0) {
    cnames <- codes$name
    Encoding(cnames) <- "UTF-8"

    if (sortByTime && "date" %in% names(codes)) {
      cnames <- cnames[OrderByTime(codes$date, decreasing = decreasing)]
    }

    tryCatch({
      if (exists(".codes_rqda", envir = .rqda)) {
        .rqda$.codes_rqda[] <- cnames
      }
    }, error = function(e) {})
  }
}

#' Update file names widget
#' @export
FileNamesUpdate <- function(sortByTime = FALSE, decreasing = FALSE) {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))

  source <- rqda_sel("SELECT name, date, id FROM source WHERE status = 1 ORDER BY LOWER(name)")

  if (!is.null(source) && nrow(source) > 0) {
    fnames <- source$name
    Encoding(fnames) <- "UTF-8"

    if (sortByTime && "date" %in% names(source)) {
      fnames <- fnames[OrderByTime(source$date, decreasing = decreasing)]
    }

    tryCatch({
      if (exists(".fnames_rqda", envir = .rqda)) {
        .rqda$.fnames_rqda[] <- fnames
      }
    }, error = function(e) {})
  }
}

#' Close project before opening new one (updates widgets)
closeProjBF <- function() {
  # Clear all widgets
  tryCatch({
    if (exists(".codes_rqda", envir = .rqda)) .rqda$.codes_rqda[] <- NULL
  }, error = function(e) {})

  tryCatch({
    if (exists(".fnames_rqda", envir = .rqda)) .rqda$.fnames_rqda[] <- NULL
  }, error = function(e) {})
}

#' Order items by time
OrderByTime <- function(date_strings, decreasing = FALSE) {
  # Parse dates and return order
  dates <- as.POSIXct(date_strings)
  order(dates, decreasing = decreasing)
}

#' Execute SQL statement
#' @export
rqda_exe <- function(sql) {
  if (exists("qdacon", envir = .rqda)) {
    con <- .rqda$qdacon
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, sql)
    }
  }
}

#' Select from database
#' @export
rqda_sel <- function(sql) {
  if (exists("qdacon", envir = .rqda)) {
    con <- .rqda$qdacon
    if (DBI::dbIsValid(con)) {
      return(DBI::dbGetQuery(con, sql))
    }
  }
  return(NULL)
}
