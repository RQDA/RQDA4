# Code and File Category functions for RQDA

#' General update table widget function
#' @export
UpdateTableWidget <- function(Widget, FromdbTable, con = .rqda$qdacon,
                              sortByTime = FALSE, decreasing = FALSE) {
  if (!exists("qdacon", envir = .rqda) || !DBI::dbIsValid(.rqda$qdacon)) {
    return(invisible(NULL))
  }

  items <- DBI::dbGetQuery(con,
                           sprintf("SELECT name, date FROM %s WHERE status = 1", FromdbTable))

  if (nrow(items) == 0) {
    item_names <- NULL
  } else {
    Encoding(items$name) <- "UTF-8"

    if (!sortByTime) {
      item_names <- sort(items$name, decreasing = decreasing)
    } else {
      date_order <- order(items$date, decreasing = decreasing)
      item_names <- items$name[date_order]
    }
  }

  tryCatch({
    if (is.character(Widget)) {
      .rqda[[Widget]][] <- item_names
      # Reconnect row-activated on the new listbox
      handler <- switch(Widget,
                        ".CodeCatWidget" = function(box, row) UpdateCodeofCatWidget(),
                        ".FileCatWidget" = function(box, row) UpdateFileofCatWidget(),
                        NULL
      )
      if (!is.null(handler)) {
        tryCatch(
          gSignalConnectR(.rqda[[Widget]]$listbox, "row-activated", handler),
          error = function(e) {}
        )
      }
    } else {
      Widget[] <- item_names
    }
  }, error = function(e) {})
  invisible(item_names)
}

#' General add to database table function
#' @export
AddTodbTable <- function(item, dbTable, Id = "id", field = "name", con = .rqda$qdacon) {
  if (item == "") {
    return(invisible(NULL))
  }

  # Get next ID
  maxid <- DBI::dbGetQuery(con, sprintf("SELECT MAX(%s) as maxid FROM %s", Id, dbTable))
  nextid <- if (is.na(maxid$maxid)) 1 else maxid$maxid + 1

  # Check if exists
  dup <- DBI::dbGetQuery(con,
                         sprintf("SELECT %s FROM %s WHERE name = '%s'", field, dbTable, item))

  if (nrow(dup) > 0) {
    message("Item already exists in ", dbTable)
    return(invisible(NULL))
  }

  # Insert
  DBI::dbExecute(con,
                 sprintf(
                   "INSERT INTO %s (%s, %s, status, date, dateM, owner, memo)
       VALUES ('%s', %d, 1, '%s', '%s', '%s', '')",
                   dbTable,
                   field,
                   Id,
                   item,
                   nextid,
                   date(),
                   date(),
                   .rqda$owner
                 ))

  message("Added to ", dbTable, ": ", item)
  invisible(nextid)
}

## CODE CATEGORY FUNCTIONS ##

#' Update code of category widget
#' @export
UpdateCodeofCatWidget <- function(Widget = .rqda$.CodeofCat, con = .rqda$qdacon) {
  if (!exists("qdacon", envir = .rqda) || !DBI::dbIsValid(.rqda$qdacon)) {
    return(invisible(NULL))
  }

  # Check widget exists before trying to get value
  if (!exists(".CodeCatWidget", envir = .rqda)) {
    return(invisible(NULL))
  }

  # Get selected category - with safety checks
  Selected <- NULL
  tryCatch({
    # Only get value if widget has items
    if (!is.null(.rqda$.CodeCatWidget$items) && length(.rqda$.CodeCatWidget$items) > 0) {
      Selected <- svalue(.rqda$.CodeCatWidget)
    }
  }, error = function(e) {
    # Silently ignore errors
  })

  if (is.null(Selected) || length(Selected) == 0) {
    codes <- NULL
  } else {
    # Get category ID
    catid <- DBI::dbGetQuery(con,
                             sprintf("SELECT catid FROM codecat WHERE status = 1 AND name = '%s'", Selected))

    if (nrow(catid) == 0) {
      codes <- NULL
    } else {
      # Get codes in this category
      code_ids <- DBI::dbGetQuery(con,
                                  sprintf("SELECT cid FROM treecode WHERE status = 1 AND catid = %d", catid$catid[1]))

      if (nrow(code_ids) == 0) {
        codes <- NULL
      } else {
        # Get code names and colors
        codes <- DBI::dbGetQuery(con,
                                 sprintf("SELECT name, color FROM freecode WHERE status = 1 AND id IN (%s)",
                                         paste(code_ids$cid, collapse = ",")))

        if (nrow(codes) > 0) {
          ord <- order(codes$name)
          colors <- codes$color[ord]
          codes  <- codes$name[ord]
          Encoding(codes) <- "UTF-8"
        } else {
          codes  <- NULL
          colors <- NULL
        }
      }
    }
  }

  tryCatch({
    cols <- if (exists("colors")) colors else NULL
    .rqda$.CodeofCat <- `[<-.gtable`(.rqda$.CodeofCat, value = codes, colors = cols)
  }, error = function(e) {})
  invisible(codes)
}

#' Add code to category
#' @export
AddCodeToCategory <- function(codename = NULL, catname = NULL, con = .rqda$qdacon) {
  if (!DBI::dbIsValid(con)) {
    stop("No valid project open")
  }

  # Get code ID
  if (is.null(codename)) {
    selected <- svalue(.rqda$.codes_rqda)
    if (length(selected) == 0) {
      message("No code selected")
      return(invisible(NULL))
    }
    codename <- selected[1]
  }

  cid <- DBI::dbGetQuery(con,
                         sprintf("SELECT id FROM freecode WHERE name = '%s' AND status = 1", codename))

  if (nrow(cid) == 0) {
    message("Code not found")
    return(invisible(NULL))
  }

  # Get category — show popup if not supplied
  if (is.null(catname)) {
    already <- DBI::dbGetQuery(con,
                               sprintf("SELECT catid FROM treecode WHERE cid=%d AND status=1", cid$id[1]))
    exclude <- if (nrow(already)>0) paste(already$catid, collapse=",") else "0"
    avail <- DBI::dbGetQuery(con, sprintf(
      "SELECT name FROM codecat WHERE status=1 AND catid NOT IN (%s) ORDER BY name", exclude))
    if (nrow(avail) == 0) {
      gmessage("Code is already in all categories.", icon="info")
      return(invisible(NULL))
    }
    chosen <- gselect_multi(avail$name,
                            title   = "Add code to category",
                            message = sprintf("Add '%s' to:", codename))
    if (is.null(chosen) || length(chosen)==0) return(invisible(NULL))
    for (cn in chosen) AddCodeToCategory(codename=codename, catname=cn, con=con)
    return(invisible(NULL))
  }

  catid <- DBI::dbGetQuery(con,
                           sprintf("SELECT catid FROM codecat WHERE name='%s' AND status=1", gsub("'","''",catname)))
  if (nrow(catid) == 0) { message("Category not found"); return(invisible(NULL)) }

  exist <- DBI::dbGetQuery(con,
                           sprintf("SELECT cid FROM treecode WHERE cid=%d AND catid=%d AND status=1",
                                   cid$id[1], catid$catid[1]))
  if (nrow(exist) > 0) { message("Code already in category"); return(invisible(NULL)) }

  DBI::dbExecute(con, sprintf(
    "INSERT INTO treecode (cid, catid, date, dateM, memo, status, owner) VALUES (%d,%d,'%s','%s','',1,'%s')",
    cid$id[1], catid$catid[1], date(), date(), .rqda$owner))
  message("Code added to category")
  UpdateCodeofCatWidget()
}

#' Drop code from category
#' @export
DropCodeFromCategory <- function(con = .rqda$qdacon) {
  if (!DBI::dbIsValid(con)) {
    stop("No valid project open")
  }

  # Get selected code from category
  codename <- svalue(.rqda$.CodeofCat)
  if (length(codename) == 0) {
    message("No code selected")
    return(invisible(NULL))
  }

  # Get category
  catname <- svalue(.rqda$.CodeCatWidget)
  if (length(catname) == 0) {
    message("No category selected")
    return(invisible(NULL))
  }

  # Get IDs
  cid <- DBI::dbGetQuery(con,
                         sprintf("SELECT id FROM freecode WHERE name = '%s' AND status = 1", codename))

  catid <- DBI::dbGetQuery(con,
                           sprintf("SELECT catid FROM codecat WHERE name = '%s' AND status = 1", catname))

  # Remove from category
  DBI::dbExecute(con,
                 sprintf("UPDATE treecode SET status = 0 WHERE cid = %d AND catid = %d",
                         cid$id[1], catid$catid[1]))

  message("Code dropped from category")
  UpdateCodeofCatWidget()
}

## FILE CATEGORY FUNCTIONS ##

#' Update file of category widget
#' @export
UpdateFileofCatWidget <- function(Widget = .rqda$.FileofCat, con = .rqda$qdacon) {
  if (!exists("qdacon", envir = .rqda) || !DBI::dbIsValid(.rqda$qdacon)) {
    return(invisible(NULL))
  }

  # Check widget exists before trying to get value
  if (!exists(".FileCatWidget", envir = .rqda)) {
    return(invisible(NULL))
  }

  # Get selected category - with safety checks
  Selected <- NULL
  tryCatch({
    # Only get value if widget has items
    if (!is.null(.rqda$.FileCatWidget$items) && length(.rqda$.FileCatWidget$items) > 0) {
      Selected <- svalue(.rqda$.FileCatWidget)
    }
  }, error = function(e) {
    # Silently ignore errors
  })

  if (is.null(Selected) || length(Selected) == 0) {
    files <- NULL
  } else {
    # Get category ID
    catid <- DBI::dbGetQuery(con,
                             sprintf("SELECT catid FROM filecat WHERE status = 1 AND name = '%s'", Selected))

    if (nrow(catid) == 0) {
      files <- NULL
    } else {
      # Get files in this category
      file_ids <- DBI::dbGetQuery(con,
                                  sprintf("SELECT fid FROM treefile WHERE status = 1 AND catid = %d", catid$catid[1]))

      if (nrow(file_ids) == 0) {
        files <- NULL
      } else {
        # Get file names
        files <- DBI::dbGetQuery(con,
                                 sprintf("SELECT name FROM source WHERE status = 1 AND id IN (%s)",
                                         paste(file_ids$fid, collapse = ",")))

        if (nrow(files) > 0) {
          files <- sort(files$name)
          Encoding(files) <- "UTF-8"
        } else {
          files <- NULL
        }
      }
    }
  }

  tryCatch({
    .rqda$.FileofCat[] <- files
  }, error = function(e) {})
  invisible(files)
}

#' Add file to category
#' @export
AddFileToCategory <- function(filename = NULL, catname = NULL, con = .rqda$qdacon) {
  if (!DBI::dbIsValid(con)) {
    stop("No valid project open")
  }

  # Get file ID
  if (is.null(filename)) {
    selected <- svalue(.rqda$.fnames_rqda)
    if (length(selected) == 0) {
      message("No file selected")
      return(invisible(NULL))
    }
    filename <- selected[1]
  }

  fid <- DBI::dbGetQuery(con,
                         sprintf("SELECT id FROM source WHERE name = '%s' AND status = 1", filename))

  if (nrow(fid) == 0) {
    message("File not found")
    return(invisible(NULL))
  }

  # Get category — show popup if not supplied
  if (is.null(catname)) {
    already <- DBI::dbGetQuery(con,
                               sprintf("SELECT catid FROM treefile WHERE fid=%d AND status=1", fid$id[1]))
    exclude <- if (nrow(already)>0) paste(already$catid, collapse=",") else "0"
    avail <- DBI::dbGetQuery(con, sprintf(
      "SELECT name FROM filecat WHERE status=1 AND catid NOT IN (%s) ORDER BY name", exclude))
    if (nrow(avail) == 0) {
      gmessage("File is already in all categories.", icon="info")
      return(invisible(NULL))
    }
    chosen <- gselect_multi(avail$name,
                            title   = "Add file to category",
                            message = sprintf("Add '%s' to:", filename))
    if (is.null(chosen) || length(chosen)==0) return(invisible(NULL))
    for (cn in chosen) {
      AddFileToCategory(filename=filename, catname=cn, con=con)
    }
    return(invisible(NULL))
  }

  catid <- DBI::dbGetQuery(con,
                           sprintf("SELECT catid FROM filecat WHERE name='%s' AND status=1", gsub("'","''",catname)))
  if (nrow(catid) == 0) { message("Category not found"); return(invisible(NULL)) }

  exist <- DBI::dbGetQuery(con,
                           sprintf("SELECT fid FROM treefile WHERE fid=%d AND catid=%d AND status=1",
                                   fid$id[1], catid$catid[1]))
  if (nrow(exist) > 0) { message("File already in category"); return(invisible(NULL)) }

  DBI::dbExecute(con, sprintf(
    "INSERT INTO treefile (fid, catid, date, dateM, memo, status, owner) VALUES (%d,%d,'%s','%s','',1,'%s')",
    fid$id[1], catid$catid[1], date(), date(), .rqda$owner))
  message("File added to category")
  UpdateFileofCatWidget()
}

#' Drop file from category
#' @export
DropFileFromCategory <- function(con = .rqda$qdacon) {
  if (!DBI::dbIsValid(con)) {
    stop("No valid project open")
  }

  # Get selected file from category
  filename <- svalue(.rqda$.FileofCat)
  if (length(filename) == 0) {
    message("No file selected")
    return(invisible(NULL))
  }

  # Get category
  catname <- svalue(.rqda$.FileCatWidget)
  if (length(catname) == 0) {
    message("No category selected")
    return(invisible(NULL))
  }

  # Get IDs
  fid <- DBI::dbGetQuery(con,
                         sprintf("SELECT id FROM source WHERE name = '%s' AND status = 1", filename))

  catid <- DBI::dbGetQuery(con,
                           sprintf("SELECT catid FROM filecat WHERE name = '%s' AND status = 1", catname))

  # Remove from category
  DBI::dbExecute(con,
                 sprintf("UPDATE treefile SET status = 0 WHERE fid = %d AND catid = %d",
                         fid$id[1], catid$catid[1]))

  message("File dropped from category")
  UpdateFileofCatWidget()
}
