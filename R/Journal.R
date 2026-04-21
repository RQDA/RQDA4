# Journal system for RQDA

#' Update journal names widget
#' @export
JournalNamesUpdate <- function(sortByTime = TRUE, decreasing = TRUE) {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))
  if (!exists(".JournalNamesWidget", envir = .rqda)) return(invisible(NULL))
  journals <- rqda_sel("SELECT name, date FROM journal WHERE status = 1")
  if (is.null(journals) || nrow(journals) == 0) {
    .rqda$.JournalNamesWidget[] <- NULL
    return(invisible(NULL))
  }
  jnames <- journals$name
  Encoding(jnames) <- "UTF-8"
  if (!sortByTime) jnames <- sort(jnames, decreasing=decreasing)
  else jnames <- jnames[OrderByTime(journals$date, decreasing=decreasing)]
  .rqda$.JournalNamesWidget[] <- jnames
  invisible(NULL)
}

#' Add journal entry
#' @export
AddJournal <- function(entryname = NULL, content = "") {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(entryname)) {
    # Use current date/time as default name
    entryname <- format(Sys.time(), "%Y-%m-%d %H:%M")

    entryname <- ginput(
      message = "Enter name for journal entry:",
      text = entryname,
      title = "New Journal Entry"
    )
  }

  if (is.null(entryname) || identical(entryname, character(0)) || !nzchar(entryname)) {
    return(invisible(NULL))
  }

  entryname_escaped <- gsub("'", "''", entryname)
  content_escaped <- gsub("'", "''", content)

  # Check for duplicates
  dup <- rqda_sel(sprintf(
    "SELECT name, status FROM journal WHERE name = '%s'",
    entryname_escaped
  ))

  # If only deleted record exists, just restore it silently
  if (!is.null(dup) && nrow(dup) > 0 && all(dup$status == 0)) {
    rqda_exe(sprintf(
      "UPDATE journal SET status=1, journal='%s', dateM='%s' WHERE name='%s'",
      content_escaped, date(), entryname_escaped))
    message("Restored journal entry: ", entryname)
    JournalNamesUpdate()
    return(invisible(NULL))
  }

  if (!is.null(dup) && nrow(dup) > 0) {
    if (!gconfirm("A journal entry with this name exists. Overwrite?", icon = "question")) {
      return(invisible(NULL))
    }

    # Update existing - restore status=1 in case it was deleted
    rqda_exe(sprintf(
      "UPDATE journal SET journal = '%s', dateM = '%s', status = 1 WHERE name = '%s'",
      content_escaped, date(), entryname_escaped
    ))
  } else {
    # Insert new
    rqda_exe(sprintf(
      "INSERT INTO journal (name, journal, date, owner, status) VALUES ('%s', '%s', '%s', '%s', 1)",
      entryname_escaped, content_escaped, date(), .rqda$owner
    ))
  }

  message("Added journal entry: ", entryname)
  JournalNamesUpdate()

  invisible(NULL)
}

#' Delete journal entry
#' @export
DeleteJournal <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".JournalNamesWidget", envir = .rqda)) {
    gmessage("Journal widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- (tryCatch(.rqda$selected_journal, error=function(e) character(0)) %||% character(0))

  if (identical(Selected, character(0))) {
    gmessage("Please select a journal entry first", icon = "warning")
    return(invisible(NULL))
  }

  if (!gconfirm("Really delete this journal entry?", icon = "question")) {
    return(invisible(NULL))
  }

  Selected_escaped <- gsub("'", "''", Selected)

  rqda_exe(sprintf(
    "UPDATE journal SET status = 0 WHERE name = '%s'",
    Selected_escaped
  ))

  message("Deleted journal entry: ", Selected)
  JournalNamesUpdate()

  invisible(NULL)
}

#' Open journal entry for editing
#' @export
OpenJournal <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".JournalNamesWidget", envir = .rqda)) {
    gmessage("Journal widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- (tryCatch(.rqda$selected_journal, error=function(e) character(0)) %||% character(0))

  if (identical(Selected, character(0))) {
    gmessage("Please select a journal entry first", icon = "warning")
    return(invisible(NULL))
  }

  # Get journal content
  journal_data <- rqda_sel(sprintf(
    "SELECT journal FROM journal WHERE name = '%s' AND status = 1",
    gsub("'", "''", Selected)
  ))

  if (is.null(journal_data) || nrow(journal_data) == 0) {
    gmessage("Journal entry not found", icon = "error")
    return(invisible(NULL))
  }

  content <- journal_data$journal[1]
  if (is.na(content)) content <- ""
  Encoding(content) <- "UTF-8"

  # Create journal window
  journal_win <- gwindow(
    title = paste("Journal:", Selected),
    width = 700,
    height = 500
  )

  journal_group <- ggroup(horizontal = FALSE, container = journal_win)

  # Text area for journal
  journal_text <- gtext(content, container = journal_group)

  # Save button
  save_btn <- gbutton(
    "Save",
    container = journal_group,
    handler = function(h, ...) {
      new_content <- svalue(journal_text)
      new_content <- gsub("'", "''", new_content)

      rqda_exe(sprintf(
        "UPDATE journal SET journal = '%s', dateM = '%s' WHERE name = '%s'",
        new_content, date(), gsub("'", "''", Selected)
      ))

      message("Journal entry saved: ", Selected)
      gmessage("Journal entry saved!", icon = "info")
    }
  )

  invisible(journal_win)
}

#' Create new journal entry and open editor
#' @export
NewJournal <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  # Generate default name
  default_name <- format(Sys.time(), "%Y-%m-%d %H:%M")

  entryname <- ginput(
    message = "Enter name for new journal entry:",
    text = default_name,
    title = "New Journal Entry"
  )

  if (is.null(entryname) || identical(entryname, character(0)) || !nzchar(entryname)) {
    return(invisible(NULL))
  }

  entryname_escaped <- gsub("'", "''", entryname)

  # Check for duplicates
  dup <- rqda_sel(sprintf(
    "SELECT name FROM journal WHERE name = '%s'",
    entryname_escaped
  ))

  if (!is.null(dup) && nrow(dup) > 0) {
    gmessage("A journal entry with this name already exists!", icon = "warning")
    return(invisible(NULL))
  }

  # Create empty entry
  rqda_exe(sprintf(
    "INSERT INTO journal (name, journal, date, owner, status) VALUES ('%s', '', '%s', '%s', 1)",
    entryname_escaped, date(), .rqda$owner
  ))

  message("Created journal entry: ", entryname)
  JournalNamesUpdate()

  # Open for editing
  if (exists(".JournalNamesWidget", envir = .rqda)) {
    # Select the newly created entry
    tryCatch({
      (tryCatch(.rqda$selected_journal, error=function(e) character(0)) %||% character(0)) <- entryname
    }, error = function(e) {})
  }

  # Create journal window
  journal_win <- gwindow(
    title = paste("Journal:", entryname),
    width = 700,
    height = 500
  )

  journal_group <- ggroup(horizontal = FALSE, container = journal_win)

  # Text area for journal
  journal_text <- gtext("", container = journal_group)

  # Save button
  save_btn <- gbutton(
    "Save",
    container = journal_group,
    handler = function(h, ...) {
      new_content <- svalue(journal_text)
      new_content <- gsub("'", "''", new_content)

      rqda_exe(sprintf(
        "UPDATE journal SET journal = '%s', dateM = '%s' WHERE name = '%s'",
        new_content, date(), entryname_escaped
      ))

      message("Journal entry saved: ", entryname)
      gmessage("Journal entry saved!", icon = "info")
    }
  )

  invisible(journal_win)
}

#' Export all journal entries to text file
#' @export
ExportJournal <- function(filepath = NULL) {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(filepath)) {
    filepath <- gfile(
      type = "save",
      text = "Save journal as..."
    )
  }

  if (is.null(filepath) || identical(filepath, character(0)) || !nzchar(filepath)) {
    return(invisible(NULL))
  }

  # Get all journal entries
  journals <- rqda_sel("SELECT name, journal, date FROM journal WHERE status = 1 ORDER BY date DESC")

  if (is.null(journals) || nrow(journals) == 0) {
    gmessage("No journal entries to export", icon = "info")
    return(invisible(NULL))
  }

  # Create output
  output <- character()

  for (i in 1:nrow(journals)) {
    output <- c(
      output,
      paste(rep("=", 70), collapse = ""),
      paste("Entry:", journals$name[i]),
      paste("Date:", journals$date[i]),
      paste(rep("-", 70), collapse = ""),
      journals$journal[i],
      "",
      ""
    )
  }

  # Write to file
  writeLines(output, filepath, useBytes = TRUE)

  message("Journal exported to: ", filepath)
  gmessage(paste("Journal exported to:", filepath), icon = "info")

  invisible(filepath)
}
