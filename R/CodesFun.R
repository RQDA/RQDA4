.add_code_dialog <- function() {
  res  <- list(val = NULL)
  loop <- gMainLoopNew(NULL, FALSE)

  win <- gtkWindowNew()
  gtkWindowSetTitle(win, "New Code")
  gtkWindowSetDefaultSize(win, 360L, 160L)
  gtkWindowSetModal(win, TRUE)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(win, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 8L)
  gtkWidgetSetMarginTop(vbox, 12L); gtkWidgetSetMarginBottom(vbox, 12L)
  gtkWidgetSetMarginStart(vbox, 12L); gtkWidgetSetMarginEnd(vbox, 12L)
  gtkWindowSetChild(win, vbox)

  # Name row
  name_hbox <- gtkBoxNew(0L, 8L)
  name_lbl  <- gtkLabelNew("Name:")
  gtkWidgetSetSizeRequest(name_lbl, 60L, -1L)
  gtkLabelSetXalign(name_lbl, 0.0)
  name_entry <- gtkEntryNew()
  gtkWidgetSetHexpand(name_entry, TRUE)
  gtkBoxAppend(name_hbox, name_lbl)
  gtkBoxAppend(name_hbox, name_entry)
  gtkBoxAppend(vbox, name_hbox)

  # Color row
  col_hbox  <- gtkBoxNew(0L, 8L)
  col_lbl   <- gtkLabelNew("Color:")
  gtkWidgetSetSizeRequest(col_lbl, 60L, -1L)
  gtkLabelSetXalign(col_lbl, 0.0)
  col_entry  <- gtkEntryNew()
  gtkWidgetSetHexpand(col_entry, TRUE)
  default_col <- if (exists("fore.col", envir=.rqda)) .rqda$fore.col else "blue"
  gtkEntryBufferSetText(gtkEntryGetBuffer(col_entry), default_col, -1L)

  col_swatch <- gtkLabelNew("")
  update_swatch <- function(col) {
    hex <- tryCatch({
      v <- col2rgb(col)/255
      sprintf("#%02X%02X%02X", as.integer(v[1]*255), as.integer(v[2]*255), as.integer(v[3]*255))
    }, error=function(e) "#888888")
    tryCatch(gtkLabelSetMarkup(col_swatch,
                               sprintf('<span foreground="%s" size="x-large">&#x25A0;</span>', hex)),
             error=function(e){})
  }
  update_swatch(default_col)
  gSignalConnectR(col_entry, "changed", function(w) {
    update_swatch(gtkEntryBufferGetText(gtkEntryGetBuffer(w)))
  })

  pick_btn <- gtkButtonNewWithLabel("…")
  gtkWidgetSetVexpand(pick_btn, FALSE); gtkWidgetSetValign(pick_btn, 3L)
  gSignalConnectR(pick_btn, "clicked", function(w) {
    cur <- gtkEntryBufferGetText(gtkEntryGetBuffer(col_entry))
    chosen <- .show_color_picker(cur)
    if (!is.null(chosen)) {
      gtkEntryBufferSetText(gtkEntryGetBuffer(col_entry), chosen, -1L)
      update_swatch(chosen)
    }
  })

  gtkBoxAppend(col_hbox, col_lbl)
  gtkBoxAppend(col_hbox, col_entry)
  gtkBoxAppend(col_hbox, col_swatch)
  gtkBoxAppend(col_hbox, pick_btn)
  gtkBoxAppend(vbox, col_hbox)

  # Buttons
  btn_hbox  <- gtkBoxNew(0L, 8L)
  gtkWidgetSetHalign(btn_hbox, 3L)
  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  ok_btn     <- gtkButtonNewWithLabel("OK")
  gtkBoxAppend(btn_hbox, cancel_btn)
  gtkBoxAppend(btn_hbox, ok_btn)
  gtkBoxAppend(vbox, btn_hbox)

  do_ok <- function() {
    nm  <- gtkEntryBufferGetText(gtkEntryGetBuffer(name_entry))
    col <- gtkEntryBufferGetText(gtkEntryGetBuffer(col_entry))
    if (nzchar(nm)) res$val <<- list(name=nm, color=if(nzchar(col)) col else default_col)
    gMainLoopQuit(loop); gtkWindowDestroy(win)
  }
  gSignalConnectR(ok_btn,     "clicked", function(w) do_ok())
  gSignalConnectR(cancel_btn, "clicked", function(w) {
    gMainLoopQuit(loop); gtkWindowDestroy(win)
  })
  gSignalConnectR(win, "close-request", function(w) {
    gMainLoopQuit(loop); FALSE
  })

  # Enter key confirms
  key_ctrl <- gtkEventControllerKeyNew()
  gSignalConnectR(key_ctrl, "key-pressed", function(ctrl, keyval, keycode, state) {
    if (keyval == 65293L) { do_ok(); return(TRUE) }  # GDK_KEY_Return
    FALSE
  })
  gtkWidgetAddController(win, key_ctrl)

  gtkWindowPresent(win)
  gtkWidgetGrabFocus(name_entry)
  gMainLoopRun(loop)
  res$val
}

# Code management functions for RQDA

#' Add a new code
#' @export
AddCode <- function(codename = NULL, color = NULL) {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(codename)) {
    # Custom dialog with name + color picker
    result <- .add_code_dialog()
    if (is.null(result)) return(invisible(NULL))
    codename <- result$name
    color    <- result$color
  }

  if (identical(codename, character(0)) || is.null(codename) || !nzchar(codename))
    return(invisible(NULL))

  if (is.null(color) || !nzchar(color))
    color <- if (exists("fore.col", envir=.rqda)) .rqda$fore.col else "blue"

  maxid  <- rqda_sel("SELECT MAX(id) FROM freecode")[[1]]
  nextid <- ifelse(is.na(maxid), 1L, maxid + 1L)

  write <- FALSE
  if (nextid == 1L) {
    write <- TRUE
  } else {
    dup <- rqda_sel(sprintf("SELECT name FROM freecode WHERE name='%s'", gsub("'","''",codename)))
    if (is.null(dup) || nrow(dup) == 0) write <- TRUE
    else gmessage("A code with this name already exists!", icon="warning")
  }

  if (write) {
    rqda_exe(sprintf(
      "INSERT INTO freecode (name, id, status, date, owner, color) VALUES ('%s',%d,1,'%s','%s','%s')",
      enc(codename), nextid, date(), .rqda$owner, color
    ))

    message("Added code: ", codename)
    CodeNamesUpdate()
    if (exists(".viewer_reload", envir=.rqda) && is.function(.rqda$.viewer_reload))
      tryCatch(.rqda$.viewer_reload(), error=function(e){})
  }
}

#' Delete a code
#' @export
DeleteCode <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".codes_rqda", envir = .rqda)) {
    gmessage("Code widget not found", icon = "error")
    return(invisible(NULL))
  }

  SelectedCode <- svalue(.rqda$.codes_rqda)

  if (length(SelectedCode) == 0) {
    gmessage("Please select a code first", icon = "warning")
    return(invisible(NULL))
  }

  # Confirm deletion
  confirm_msg <- if (length(SelectedCode) == 1) {
    sprintf("Are you sure you want to delete code '%s'?", SelectedCode[1])
  } else {
    sprintf("Are you sure you want to delete %d codes?", length(SelectedCode))
  }

  if (!gconfirm(confirm_msg, icon = "question")) {
    return(invisible(NULL))
  }

  # Mark as deleted (status = 0)
  for (cname in SelectedCode) {
    rqda_exe(sprintf(
      "UPDATE freecode SET status = 0 WHERE name = '%s'",
      cname
    ))
    message("Deleted code: ", cname)
  }

  CodeNamesUpdate()
}

#' Rename a code
#' @export
RenameCode <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".codes_rqda", envir = .rqda)) {
    gmessage("Code widget not found", icon = "error")
    return(invisible(NULL))
  }

  SelectedCode <- svalue(.rqda$.codes_rqda)

  if (length(SelectedCode) == 0) {
    gmessage("Please select a code first", icon = "warning")
    return(invisible(NULL))
  }

  # Show dialog for new name
  new_name <- ginput(
    message = sprintf("Enter new name for '%s':", SelectedCode[1]),
    text = SelectedCode[1],
    title = "Rename Code"
  )

  if (is.null(new_name) || length(new_name) == 0) return(invisible(NULL))
  new_name <- as.character(new_name)[1]
  if (is.na(new_name) || !nzchar(new_name)) return(invisible(NULL))

  # Check for duplicates
  existing <- rqda_sel(sprintf(
    "SELECT name FROM freecode WHERE name = '%s' AND status = 1 AND name != '%s'",
    new_name, SelectedCode[1]
  ))

  if (!is.null(existing) && nrow(existing) > 0) {
    gmessage("A code with this name already exists!", icon = "error")
    return(invisible(NULL))
  }

  rqda_exe(sprintf(
    "UPDATE freecode SET name = '%s', dateM = '%s' WHERE name = '%s'",
    new_name, date(), SelectedCode[1]
  ))

  message("Renamed code to: ", new_name)
  CodeNamesUpdate()
}

#' Code memo
#' @export
CodeMemo <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".codes_rqda", envir = .rqda)) {
    message("Code widget not found")
    return(invisible(NULL))
  }

  SelectedCode <- svalue(.rqda$.codes_rqda)

  if (length(SelectedCode) == 0) {
    message("Select a code first")
    return(invisible(NULL))
  }

  # Get current memo
  memo_data <- rqda_sel(sprintf(
    "SELECT memo FROM freecode WHERE name = '%s'",
    SelectedCode[1]
  ))

  current_memo <- if (!is.null(memo_data) && nrow(memo_data) > 0) {
    memo_data$memo[1]
  } else {
    ""
  }

  if (is.na(current_memo)) current_memo <- ""

  # Create memo window
  memo_win <- gwindow(
    title = paste("Memo for code:", SelectedCode[1]),
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
      new_memo <- gsub("'", "''", new_memo)  # Escape quotes

      rqda_exe(sprintf(
        "UPDATE freecode SET memo = '%s', dateM = '%s' WHERE name = '%s'",
        new_memo, date(), SelectedCode[1]
      ))

      message("Memo saved for code: ", SelectedCode[1])
      dispose(memo_win)
    }
  )

  invisible(memo_win)
}

#' Mark text with selected code
#' @export
MarkCoding <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  # Check if file is open
  if (!exists(".openfile_gui", envir = .rqda) || !exists(".openfile_fid", envir = .rqda)) {
    gmessage("No file is currently open. Please open a file first.", icon = "warning")
    return(invisible(NULL))
  }

  # Check if code is selected
  if (!exists(".codes_rqda", envir = .rqda)) {
    gmessage("Code widget not found", icon = "error")
    return(invisible(NULL))
  }

  SelectedCode <- svalue(.rqda$.codes_rqda)

  if (length(SelectedCode) == 0) {
    gmessage("Please select a code first", icon = "warning")
    return(invisible(NULL))
  }

  # Get code ID and color
  code_data <- rqda_sel(sprintf(
    "SELECT id, color FROM freecode WHERE name = '%s' AND status = 1",
    SelectedCode[1]
  ))

  if (is.null(code_data) || nrow(code_data) == 0) {
    gmessage("Code not found in database", icon = "error")
    return(invisible(NULL))
  }

  cid <- code_data$id[1]
  color <- code_data$color[1]
  if (is.na(color) || color == "") {
    color <- .rqda$codeMark.col
  }

  fid <- .rqda$.openfile_fid
  textview <- .rqda$.openfile_gui$textview

  # Mark the selected text
  result <- mark(textview = textview, fore.col = color, back.col = NULL)

  if (is.null(result)) {
    gmessage("Please select some text first", icon = "warning")
    return(invisible(NULL))
  }

  # Escape quotes in selected text for SQL
  seltext <- gsub("'", "''", result$text)

  # Insert into database
  tryCatch({
    rqda_exe(sprintf(
      paste(
        "INSERT INTO coding (cid, fid, seltext, selfirst, selend, status, owner, date) ",
        "VALUES (%d, %d, '%s', %g, %g, 1, '%s', '%s')"
      ),
      cid, fid, seltext, result$start, result$end, .rqda$owner, date()
    ))

    message(sprintf(
      "Marked text (%d-%d) with code '%s'",
      result$start, result$end, SelectedCode[1]
    ))
  }, error = function(e) {
    gmessage(sprintf("Error saving coding: %s", e$message), icon = "error")
  })

  invisible(result)
}

#' Unmark coding
#' @export
UnmarkCoding <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  # Check if file is open
  if (!exists(".openfile_gui", envir = .rqda) || !exists(".openfile_fid", envir = .rqda)) {
    gmessage("No file is currently open", icon = "warning")
    return(invisible(NULL))
  }

  # Get current selection
  textview <- .rqda$.openfile_gui$textview
  index <- sindex(textview)

  if (index$seltext == "") {
    gmessage("Please select the coded text you want to unmark", icon = "warning")
    return(invisible(NULL))
  }

  fid <- .rqda$.openfile_fid

  # Find codings that overlap with this selection
  codings <- rqda_sel(sprintf(
    paste(
      "SELECT rowid, cid, selfirst, selend FROM coding ",
      "WHERE fid = %d AND status = 1 ",
      "AND ((selfirst <= %g AND selend >= %g) OR ",
      "     (selfirst >= %g AND selend <= %g))"
    ),
    fid, index$endN, index$startN, index$startN, index$endN
  ))

  if (is.null(codings) || nrow(codings) == 0) {
    gmessage("No coding found at this selection", icon = "info")
    return(invisible(NULL))
  }

  # If multiple codings found, show selection dialog
  if (nrow(codings) > 1) {
    choices <- sprintf("[%s] pos:%g-%g", codings$codename, codings$selfirst, codings$selend)
    chosen  <- gselect_multi(choices, title="Multiple codings", message="Select coding to unmark:")
    if (is.null(chosen) || length(chosen)==0) return(invisible(NULL))
    idx <- match(chosen[1], choices)
    codings <- codings[if (!is.na(idx)) idx else 1L, , drop=FALSE]
  }

  rowid <- codings$rowid[1]

  # Confirm deletion
  if (!gconfirm("Unmark this coded text?", icon = "question")) {
    return(invisible(NULL))
  }

  # Delete from database (soft delete)
  rqda_exe(sprintf(
    "UPDATE coding SET status = 0 WHERE rowid = %d",
    rowid
  ))

  # Clear the highlighting
  ClearMark(
    textview = textview,
    min = codings$selfirst[1],
    max = codings$selend[1],
    clear.fore.col = TRUE,
    clear.back.col = TRUE
  )

  message("Coding unmarked")

  invisible(NULL)
}

#' Retrieve all codings for a code
#' @export
RetrieveCoding <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".codes_rqda", envir = .rqda)) {
    message("Code widget not found")
    return(invisible(NULL))
  }

  SelectedCode <- svalue(.rqda$.codes_rqda)

  if (length(SelectedCode) == 0) {
    message("Select a code first")
    return(invisible(NULL))
  }

  # Get code ID
  code_data <- rqda_sel(sprintf(
    "SELECT id FROM freecode WHERE name = '%s' AND status = 1",
    SelectedCode[1]
  ))

  if (is.null(code_data) || nrow(code_data) == 0) {
    message("Code not found")
    return(invisible(NULL))
  }

  cid <- code_data$id[1]

  # Get all codings for this code
  codings <- rqda_sel(sprintf(
    paste(
      "SELECT c.rowid, c.seltext, c.selfirst, c.selend, s.name as filename ",
      "FROM coding c ",
      "LEFT JOIN source s ON c.fid = s.id ",
      "WHERE c.cid = %d AND c.status = 1 ",
      "ORDER BY s.name, c.selfirst"
    ),
    cid
  ))

  if (is.null(codings) || nrow(codings) == 0) {
    message("No codings found for code: ", SelectedCode[1])
    return(invisible(NULL))
  }

  # Create window to display results
  result_win <- gwindow(
    title = paste("Codings for:", SelectedCode[1]),
    width = 800,
    height = 600
  )

  # Format results as text
  result_text <- paste(
    sprintf("Code: %s\n\nTotal codings: %d\n\n", SelectedCode[1], nrow(codings)),
    paste(
      sapply(1:nrow(codings), function(i) {
        sprintf(
          "File: %s\nPosition: %g - %g\nText: %s\n---\n",
          codings$filename[i],
          codings$selfirst[i],
          codings$selend[i],
          codings$seltext[i]
        )
      }),
      collapse = "\n"
    ),
    sep = ""
  )

  gtext(result_text, container = result_win)

  invisible(result_win)
}

#' Add annotation to text
#' @export
AddAnnotation <- function() {
  if (!is_projOpen()) return(invisible(NULL))
  if (!exists(".openfile_fid", envir=.rqda) || !exists(".openfile_gui", envir=.rqda)) {
    gmessage("Open a file first.", icon="info")
    return(invisible(NULL))
  }
  addAnnotation(.rqda$.openfile_fid, .rqda$.openfile_gui$textview)
}
