# Case management functions for RQDA

#' Update case names widget
#' @export
CaseNamesUpdate <- function(CaseNamesWidget = NULL, sortByTime = FALSE, decreasing = FALSE) {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(CaseNamesWidget) && exists(".CasesNamesWidget", envir = .rqda)) {
    CaseNamesWidget <- .rqda$.CasesNamesWidget
  }

  if (is.null(CaseNamesWidget)) {
    return(invisible(NULL))
  }

  CaseName <- rqda_sel("SELECT name, id, date FROM cases WHERE status = 1")

  if (is.null(CaseName) || nrow(CaseName) == 0) {
    tryCatch(CaseNamesWidget[] <- NULL, error = function(e) {})
    return(invisible(NULL))
  }

  case <- CaseName$name
  Encoding(case) <- "UTF-8"

  if (!sortByTime) {
    case <- sort(case, decreasing = decreasing)
  } else {
    case <- case[OrderByTime(CaseName$date, decreasing = decreasing)]
  }

  tryCatch(CaseNamesWidget[] <- case, error = function(e) {})

  invisible(NULL)
}

#' Add a new case
#' @export
AddCase <- function(casename = NULL) {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(casename)) {
    casename <- ginput(
      message = "Enter the name for the new case:",
      text = "",
      title = "New Case"
    )
  }

  if (is.null(casename) || identical(casename, character(0)) || !nzchar(casename)) {
    return(invisible(NULL))
  }

  # Escape quotes
  casename_escaped <- gsub("'", "''", casename)

  maxid <- rqda_sel("SELECT MAX(id) FROM cases")[[1]]
  nextid <- ifelse(is.na(maxid), 1, maxid + 1)

  write <- FALSE
  if (nextid == 1) {
    write <- TRUE
  } else {
    dup <- rqda_sel(sprintf("SELECT name FROM cases WHERE name='%s'", casename_escaped))
    if (is.null(dup) || nrow(dup) == 0) {
      write <- TRUE
    } else {
      gmessage("A case with this name already exists!", icon = "warning")
    }
  }

  if (write) {
    rqda_exe(sprintf(
      "INSERT INTO cases (name, id, status, date, owner) VALUES ('%s', %d, 1, '%s', '%s')",
      casename_escaped, nextid, date(), .rqda$owner
    ))

    message("Added case: ", casename)
    CaseNamesUpdate()
  }

  invisible(NULL)
}

#' Delete a case
#' @export
DeleteCase <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".CasesNamesWidget", envir = .rqda)) {
    gmessage("Case widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- (tryCatch(.rqda$selected_case, error=function(e) character(0)) %||% character(0))

  if (identical(Selected, character(0))) {
    gmessage("Please select a case first", icon = "warning")
    return(invisible(NULL))
  }

  if (!gconfirm("Really delete this case?", icon = "question")) {
    return(invisible(NULL))
  }

  Selected_escaped <- gsub("'", "''", Selected)

  # Delete case
  rqda_exe(sprintf(
    "UPDATE cases SET status = 0 WHERE name = '%s'",
    Selected_escaped
  ))

  # Delete case linkages
  caseid <- rqda_sel(sprintf(
    "SELECT id FROM cases WHERE name = '%s'",
    Selected_escaped
  ))

  if (!is.null(caseid) && nrow(caseid) > 0) {
    rqda_exe(sprintf(
      "UPDATE caselinkage SET status = 0 WHERE caseid = %d",
      caseid$id[1]
    ))
  }

  message("Deleted case: ", Selected)
  CaseNamesUpdate()

  invisible(NULL)
}

#' Rename a case
#' @export
RenameCase <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".CasesNamesWidget", envir = .rqda)) {
    gmessage("Case widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- (tryCatch(.rqda$selected_case, error=function(e) character(0)) %||% character(0))

  if (identical(Selected, character(0))) {
    gmessage("Please select a case first", icon = "warning")
    return(invisible(NULL))
  }

  new_name <- ginput(
    message = sprintf("Enter new name for '%s':", Selected),
    text = Selected,
    title = "Rename Case"
  )

  if (is.null(new_name) || length(new_name) == 0) return(invisible(NULL))
  new_name <- as.character(new_name)[1]
  if (is.na(new_name) || !nzchar(new_name)) return(invisible(NULL))

  # Check for duplicates
  existing <- rqda_sel(sprintf(
    "SELECT name FROM cases WHERE name = '%s' AND status = 1 AND name != '%s'",
    gsub("'", "''", new_name), gsub("'", "''", Selected)
  ))

  if (!is.null(existing) && nrow(existing) > 0) {
    gmessage("A case with this name already exists!", icon = "error")
    return(invisible(NULL))
  }

  rqda_exe(sprintf(
    "UPDATE cases SET name = '%s', dateM = '%s' WHERE name = '%s'",
    gsub("'", "''", new_name), date(), gsub("'", "''", Selected)
  ))

  message("Renamed case to: ", new_name)
  CaseNamesUpdate()

  invisible(NULL)
}

#' Case memo
#' @export
CaseMemo <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".CasesNamesWidget", envir = .rqda)) {
    gmessage("Case widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- (tryCatch(.rqda$selected_case, error=function(e) character(0)) %||% character(0))

  if (identical(Selected, character(0))) {
    gmessage("Please select a case first", icon = "warning")
    return(invisible(NULL))
  }

  # Get current memo
  memo_data <- rqda_sel(sprintf(
    "SELECT memo FROM cases WHERE name = '%s'",
    gsub("'", "''", Selected)
  ))

  current_memo <- if (!is.null(memo_data) && nrow(memo_data) > 0) {
    memo_data$memo[1]
  } else {
    ""
  }

  if (is.na(current_memo)) current_memo <- ""

  # Create memo window
  memo_win <- gwindow(
    title = paste("Memo for case:", Selected),
    width = 600,
    height = 400
  )

  memo_group <- ggroup(horizontal = FALSE, container = memo_win)

  # Text area for memo
  memo_text <- gtext(current_memo, container = memo_group)

  # Save button
  save_btn <- gbutton(
    "Save Memo",
    container = memo_group,
    handler = function(h, ...) {
      new_memo <- get_buffer_text(gtkTextViewGetBuffer(memo_text$textview))
      new_memo <- gsub("'", "''", new_memo)

      rqda_exe(sprintf(
        "UPDATE cases SET memo = '%s', dateM = '%s' WHERE name = '%s'",
        new_memo, date(), gsub("'", "''", Selected)
      ))

      message("Memo saved for case: ", Selected)
      dispose(memo_win)
    }
  )

  invisible(memo_win)
}

#' Add files to case linkage
#' @export
AddFileToCase <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".fnames_rqda", envir = .rqda) || !exists(".CasesNamesWidget", envir = .rqda)) {
    gmessage("Required widgets not found", icon = "error")
    return(invisible(NULL))
  }

  # Get selected files
  filenames <- tryCatch(as.character(svalue(.rqda$.fnames_rqda)), error=function(e) character(0))
  filenames <- filenames[nzchar(filenames)]
  if (length(filenames)==0) {
    # No file pre-selected - show picker
    all_files <- rqda_sel("SELECT name FROM source WHERE status=1 ORDER BY name")
    if (is.null(all_files) || nrow(all_files)==0) {
      gmessage("No files in project.", icon="info"); return(invisible(NULL))
    }
    chosen <- gselect_multi(all_files$name, title="Add file to case",
                            message="Select file(s) to add:")
    if (is.null(chosen) || length(chosen)==0) return(invisible(NULL))
    filenames <- chosen
  }

  # Get file IDs and content length
  file_query <- rqda_sel(sprintf(
    "SELECT id, file FROM source WHERE name IN (%s) AND status = 1",
    paste0("'", gsub("'", "''", filenames), "'", collapse = ", ")
  ))

  if (is.null(file_query) || nrow(file_query) == 0) {
    gmessage("Selected files not found", icon = "error")
    return(invisible(NULL))
  }

  fid <- file_query$id
  Encoding(file_query$file) <- "UTF-8"
  selend <- nchar(file_query$file)

  # Get selected case
  casename <- (tryCatch(.rqda$selected_case, error=function(e) character(0)) %||% character(0))

  if (identical(casename, character(0))) {
    gmessage("Please select a case first", icon = "warning")
    return(invisible(NULL))
  }

  # Get case ID
  caseid_result <- rqda_sel(sprintf(
    "SELECT id FROM cases WHERE name = '%s' AND status = 1",
    gsub("'", "''", casename)
  ))

  if (is.null(caseid_result) || nrow(caseid_result) == 0) {
    gmessage("Case not found", icon = "error")
    return(invisible(NULL))
  }

  caseid <- caseid_result$id[1]

  # Check existing linkages
  for (i in seq_along(fid)) {
    exist <- rqda_sel(sprintf(
      "SELECT fid FROM caselinkage WHERE status = 1 AND fid = %d AND caseid = %d",
      fid[i], caseid
    ))

    if (is.null(exist) || nrow(exist) == 0) {
      # Add linkage
      rqda_exe(sprintf(
        "INSERT INTO caselinkage (caseid, fid, selfirst, selend, status, owner, date, memo) VALUES (%d, %d, 0, %d, 1, '%s', '%s', '')",
        caseid, fid[i], selend[i], .rqda$owner, date()
      ))

      message("Linked file to case: ", filenames[i])
    }
  }

  # Update file-of-case widget if it exists
  if (exists(".FileofCase", envir = .rqda)) {
    UpdateFileofCaseWidget()
  }

  invisible(NULL)
}

#' Update file-of-case widget
#' @export
UpdateFileofCaseWidget <- function(sortByTime = FALSE) {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".FileofCase", envir = .rqda) || !exists(".CasesNamesWidget", envir = .rqda)) {
    return(invisible(NULL))
  }

  Selected <- (tryCatch(.rqda$selected_case, error=function(e) character(0)) %||% character(0))

  if (identical(Selected, character(0))) {
    tryCatch(.rqda$.FileofCase[] <- NULL, error = function(e) {})
    return(invisible(NULL))
  }

  caseid <- rqda_sel(sprintf(
    "SELECT id FROM cases WHERE status = 1 AND name = '%s'",
    gsub("'", "''", Selected)
  ))

  if (is.null(caseid) || nrow(caseid) == 0) {
    tryCatch(.rqda$.FileofCase[] <- NULL, error = function(e) {})
    return(invisible(NULL))
  }

  caseid <- caseid$id[1]

  # Get files linked to this case
  Total_fid <- rqda_sel(sprintf(
    "SELECT fid FROM caselinkage WHERE status = 1 AND caseid = %d",
    caseid
  ))

  if (is.null(Total_fid) || nrow(Total_fid) == 0) {
    tryCatch(.rqda$.FileofCase[] <- NULL, error = function(e) {})
    return(invisible(NULL))
  }

  items <- rqda_sel("SELECT name, id, date FROM source WHERE status = 1")

  if (is.null(items) || nrow(items) == 0) {
    tryCatch(.rqda$.FileofCase[] <- NULL, error = function(e) {})
    return(invisible(NULL))
  }

  items <- items[items$id %in% Total_fid$fid, ]

  if (nrow(items) > 0) {
    if (sortByTime) {
      items <- items$name[OrderByTime(items$date)]
    } else {
      items <- sort(items$name)
    }
    Encoding(items) <- "UTF-8"
    tryCatch(.rqda$.FileofCase[] <- items, error = function(e) {})
  } else {
    tryCatch(.rqda$.FileofCase[] <- NULL, error = function(e) {})
  }

  invisible(NULL)
}

#' Highlight case in file
#' @export
HL_Case <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".root_edit", envir = .rqda) || !exists(".CasesNamesWidget", envir = .rqda)) {
    return(invisible(NULL))
  }

  SelectedFile <- svalue(.rqda$.root_edit)

  currentFid <- rqda_sel(sprintf(
    "SELECT id FROM source WHERE name = '%s'",
    gsub("'", "''", SelectedFile)
  ))

  if (is.null(currentFid) || nrow(currentFid) == 0) {
    return(invisible(NULL))
  }

  currentFid <- currentFid$id[1]

  caseName <- (tryCatch(.rqda$selected_case, error=function(e) character(0)) %||% character(0))

  if (identical(caseName, character(0))) {
    return(invisible(NULL))
  }

  caseid <- rqda_sel(sprintf(
    "SELECT id FROM cases WHERE name = '%s'",
    gsub("'", "''", caseName)
  ))

  if (is.null(caseid) || nrow(caseid) == 0) {
    return(invisible(NULL))
  }

  caseid <- caseid$id[1]

  idx <- rqda_sel(sprintf(
    "SELECT selfirst, selend FROM caselinkage WHERE fid = %d AND status = 1 AND caseid = %d",
    currentFid, caseid
  ))

  if (!is.null(idx) && nrow(idx) > 0 && exists(".openfile_gui", envir = .rqda)) {
    # Clear existing highlighting
    buffer <- gtkTextViewGetBuffer(.rqda$.openfile_gui$textview)
    bounds <- gtkTextBufferGetBounds(buffer)
    maxpos <- gtkTextIterGetOffset(bounds$end)

    ClearMark(.rqda$.openfile_gui$textview, 0, maxpos,
              clear.fore.col = FALSE, clear.back.col = TRUE)

    # Apply new highlighting
    HL(.rqda$.openfile_gui$textview, index = idx,
       fore.col = NULL, back.col = .rqda$back.col)
  }

  invisible(NULL)
}
