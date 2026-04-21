# Attributes system for RQDA - Simplified implementation

#' Update attribute names widget
#' @export
AttrNamesUpdate <- function(Widget = NULL, sortByTime = FALSE, decreasing = FALSE) {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(Widget) && exists(".AttrNamesWidget", envir = .rqda)) {
    Widget <- .rqda$.AttrNamesWidget
  }

  if (is.null(Widget)) {
    return(invisible(NULL))
  }

  attrs <- rqda_sel("SELECT name, date FROM attributes WHERE status = 1")

  if (is.null(attrs) || nrow(attrs) == 0) {
    tryCatch(Widget[] <- NULL, error = function(e) {})
    return(invisible(NULL))
  }

  attrnames <- attrs$name
  Encoding(attrnames) <- "UTF-8"

  if (!sortByTime) {
    attrnames <- sort(attrnames, decreasing = decreasing)
  } else {
    attrnames <- attrnames[OrderByTime(attrs$date, decreasing = decreasing)]
  }

  tryCatch(Widget[] <- attrnames, error = function(e) {})

  invisible(NULL)
}

#' Get currently selected attribute name - reads fresh listbox ref from .rqda
#' @keywords internal
.get_selected_attr <- function() {
  val <- tryCatch(.rqda$selected_attr, error = function(e) NULL)
  if (!is.null(val) && length(val) > 0 && !is.na(val) && nzchar(val))
    as.character(val)[1]
  else
    NULL
}

#' Dialog to add attribute with name and class selection
#' @keywords internal
.add_attribute_dialog <- function() {
  res  <- list(val = NULL)
  loop <- gMainLoopNew(NULL, FALSE)

  win <- gtkWindowNew()
  gtkWindowSetTitle(win, "New Attribute")
  gtkWindowSetDefaultSize(win, 340L, 180L)
  gtkWindowSetModal(win, TRUE)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(win, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 10L)
  gtkWidgetSetMarginTop(vbox, 12L); gtkWidgetSetMarginBottom(vbox, 12L)
  gtkWidgetSetMarginStart(vbox, 12L); gtkWidgetSetMarginEnd(vbox, 12L)
  gtkWindowSetChild(win, vbox)

  # Name row
  name_hbox <- gtkBoxNew(0L, 8L)
  name_lbl  <- gtkLabelNew("Name:")
  gtkWidgetSetSizeRequest(name_lbl, 80L, -1L)
  gtkLabelSetXalign(name_lbl, 0.0)
  name_entry <- gtkEntryNew()
  gtkWidgetSetHexpand(name_entry, TRUE)
  gtkBoxAppend(name_hbox, name_lbl)
  gtkBoxAppend(name_hbox, name_entry)
  gtkBoxAppend(vbox, name_hbox)

  # Class row - radio buttons
  class_hbox <- gtkBoxNew(0L, 12L)
  class_lbl  <- gtkLabelNew("Class:")
  gtkWidgetSetSizeRequest(class_lbl, 80L, -1L)
  gtkLabelSetXalign(class_lbl, 0.0)

  rb_char <- gtkCheckButtonNewWithLabel("character")
  rb_num  <- gtkCheckButtonNewWithLabel("numeric")
  gtkCheckButtonSetActive(rb_char, TRUE)
  # Radio button group - toggling one deactivates other
  state <- new.env(parent=emptyenv()); state$class <- "character"
  gSignalConnectR(rb_char, "toggled", function(w) {
    if (state$class != "character") { state$class <- "character" }
  })
  gSignalConnectR(rb_num, "toggled", function(w) {
    if (state$class != "numeric") { state$class <- "numeric" }
  })
  # Simple toggle - clicking one sets the other off
  gSignalConnectR(rb_char, "toggled", function(w) {
    state$class <- "character"
    gtkCheckButtonSetActive(rb_num, FALSE)
  })
  gSignalConnectR(rb_num, "toggled", function(w) {
    state$class <- "numeric"
    gtkCheckButtonSetActive(rb_char, FALSE)
  })
  gtkBoxAppend(class_hbox, class_lbl)
  gtkBoxAppend(class_hbox, rb_char)
  gtkBoxAppend(class_hbox, rb_num)
  gtkBoxAppend(vbox, class_hbox)

  # Buttons
  btn_hbox   <- gtkBoxNew(0L, 8L)
  gtkWidgetSetHalign(btn_hbox, 3L)
  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  ok_btn     <- gtkButtonNewWithLabel("OK")
  gtkBoxAppend(btn_hbox, cancel_btn)
  gtkBoxAppend(btn_hbox, ok_btn)
  gtkBoxAppend(vbox, btn_hbox)

  do_ok <- function() {
    nm <- gtkEntryBufferGetText(gtkEntryGetBuffer(name_entry))
    if (nzchar(nm)) res$val <<- list(name=nm, class=state$class)
    gMainLoopQuit(loop); gtkWindowDestroy(win)
  }
  gSignalConnectR(ok_btn,     "clicked", function(w) do_ok())
  gSignalConnectR(cancel_btn, "clicked", function(w) {
    gMainLoopQuit(loop); gtkWindowDestroy(win)
  })
  gSignalConnectR(win, "close-request", function(w) { gMainLoopQuit(loop); FALSE })
  key_ctrl <- gtkEventControllerKeyNew()
  gSignalConnectR(key_ctrl, "key-pressed", function(ctrl, keyval, keycode, state2) {
    if (keyval == 65293L) { do_ok(); return(TRUE) }
    FALSE
  })
  gtkWidgetAddController(win, key_ctrl)
  gtkWindowPresent(win)
  gtkWidgetGrabFocus(name_entry)
  gMainLoopRun(loop)
  res$val
}

#' Add attribute
#' @export
AddAttribute <- function(attrname = NULL, class = NULL) {
  if (!is_projOpen()) return(invisible(NULL))

  if (is.null(attrname)) {
    result <- .add_attribute_dialog()
    if (is.null(result)) return(invisible(NULL))
    attrname <- result$name
    class    <- result$class
  }
  if (is.null(class) || !nzchar(class)) class <- "character"

  if (identical(attrname, character(0)) || is.null(attrname) || !nzchar(attrname))
    return(invisible(NULL))

  attrname_escaped <- gsub("'", "''", attrname)
  dup <- rqda_sel(sprintf("SELECT name FROM attributes WHERE name='%s'", attrname_escaped))
  if (!is.null(dup) && nrow(dup) > 0) {
    gmessage("An attribute with this name already exists!", icon="warning")
    return(invisible(NULL))
  }

  rqda_exe(sprintf(
    "INSERT INTO attributes (name, status, date, owner, memo, class) VALUES ('%s',1,'%s','%s','','%s')",
    attrname_escaped, date(), .rqda$owner, class))

  message("Added attribute: ", attrname, " (", class, ")")
  AttrNamesUpdate()
  invisible(NULL)
}

#' Delete attribute
#' @export
DeleteAttribute <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".AttrNamesWidget", envir = .rqda)) {
    gmessage("Attribute widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- .get_selected_attr()

  if (is.null(Selected) || !nzchar(Selected)) {
    gmessage("Please select an attribute first", icon = "warning")
    return(invisible(NULL))
  }

  if (!gconfirm("Really delete this attribute?", icon = "question")) {
    return(invisible(NULL))
  }

  Selected_escaped <- gsub("'", "''", Selected)

  # Delete attribute
  rqda_exe(sprintf(
    "UPDATE attributes SET status = 0 WHERE name = '%s'",
    Selected_escaped
  ))

  # Delete associated case and file attributes
  rqda_exe(sprintf(
    "UPDATE caseAttr SET status = 0 WHERE variable = '%s'",
    Selected_escaped
  ))

  rqda_exe(sprintf(
    "UPDATE fileAttr SET status = 0 WHERE variable = '%s'",
    Selected_escaped
  ))

  message("Deleted attribute: ", Selected)
  AttrNamesUpdate()

  invisible(NULL)
}

#' Rename attribute
#' @export
RenameAttribute <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".AttrNamesWidget", envir = .rqda)) {
    gmessage("Attribute widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- .get_selected_attr()

  if (is.null(Selected) || !nzchar(Selected)) {
    gmessage("Please select an attribute first", icon = "warning")
    return(invisible(NULL))
  }

  new_name <- ginput(
    message = sprintf("Enter new name for '%s':", Selected),
    text = Selected,
    title = "Rename Attribute"
  )

  if (is.null(new_name) || identical(new_name, character(0)) ||
      length(new_name) == 0 || is.na(new_name[1]) || !nzchar(new_name[1])) {
    return(invisible(NULL))
  }
  new_name <- as.character(new_name)[1]

  # Check for duplicates
  existing <- rqda_sel(sprintf(
    "SELECT name FROM attributes WHERE name = '%s' AND status = 1 AND name != '%s'",
    gsub("'", "''", new_name), gsub("'", "''", Selected)
  ))

  if (!is.null(existing) && nrow(existing) > 0) {
    gmessage("An attribute with this name already exists!", icon = "error")
    return(invisible(NULL))
  }

  # Update attribute
  rqda_exe(sprintf(
    "UPDATE attributes SET name = '%s', dateM = '%s' WHERE name = '%s'",
    gsub("'", "''", new_name), date(), gsub("'", "''", Selected)
  ))

  # Update references in caseAttr and fileAttr
  rqda_exe(sprintf(
    "UPDATE caseAttr SET variable = '%s' WHERE variable = '%s'",
    gsub("'", "''", new_name), gsub("'", "''", Selected)
  ))

  rqda_exe(sprintf(
    "UPDATE fileAttr SET variable = '%s' WHERE variable = '%s'",
    gsub("'", "''", new_name), gsub("'", "''", Selected)
  ))

  message("Renamed attribute to: ", new_name)
  AttrNamesUpdate()

  invisible(NULL)
}

#' Attribute memo
#' @export
AttributeMemo <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".AttrNamesWidget", envir = .rqda)) {
    gmessage("Attribute widget not found", icon = "error")
    return(invisible(NULL))
  }

  Selected <- .get_selected_attr()

  if (is.null(Selected) || !nzchar(Selected)) {
    gmessage("Please select an attribute first", icon = "warning")
    return(invisible(NULL))
  }

  # Get current memo
  memo_data <- rqda_sel(sprintf(
    "SELECT memo FROM attributes WHERE name = '%s'",
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
    title = paste("Memo for attribute:", Selected),
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
        "UPDATE attributes SET memo = '%s', dateM = '%s' WHERE name = '%s'",
        new_memo, date(), gsub("'", "''", Selected)
      ))

      message("Memo saved for attribute: ", Selected)
      dispose(memo_win)
    }
  )

  invisible(memo_win)
}

#' Set case attribute value
#' @export
SetCaseAttribute <- function(casename, attrname, value) {
  if (!is_projOpen()) return(invisible(NULL))

  # Get case ID
  caseid_result <- rqda_sel(sprintf(
    "SELECT id FROM cases WHERE name = '%s' AND status = 1",
    gsub("'", "''", casename)
  ))

  if (is.null(caseid_result) || nrow(caseid_result) == 0) {
    message("Case not found: ", casename)
    return(invisible(NULL))
  }

  caseid <- caseid_result$id[1]

  # Check if attribute exists
  attr_check <- rqda_sel(sprintf(
    "SELECT name FROM attributes WHERE name = '%s' AND status = 1",
    gsub("'", "''", attrname)
  ))

  if (is.null(attr_check) || nrow(attr_check) == 0) {
    message("Attribute not found: ", attrname)
    return(invisible(NULL))
  }

  # Check if value already exists
  existing <- rqda_sel(sprintf(
    "SELECT rowid FROM caseAttr WHERE variable = '%s' AND caseID = %d AND status = 1",
    gsub("'", "''", attrname), caseid
  ))

  if (!is.null(existing) && nrow(existing) > 0) {
    # Update existing
    rqda_exe(sprintf(
      "UPDATE caseAttr SET value = '%s', dateM = '%s' WHERE variable = '%s' AND caseID = %d",
      gsub("'", "''", as.character(value)), date(), gsub("'", "''", attrname), caseid
    ))
  } else {
    # Insert new
    rqda_exe(sprintf(
      "INSERT INTO caseAttr (variable, value, caseID, date, owner, status) VALUES ('%s', '%s', %d, '%s', '%s', 1)",
      gsub("'", "''", attrname), gsub("'", "''", as.character(value)), caseid, date(), .rqda$owner
    ))
  }

  message(sprintf("Set %s = %s for case %s", attrname, value, casename))

  invisible(NULL)
}

#' Set file attribute value
#' @export
SetFileAttribute <- function(filename, attrname, value) {
  if (!is_projOpen()) return(invisible(NULL))

  # Get file ID
  fileid_result <- rqda_sel(sprintf(
    "SELECT id FROM source WHERE name = '%s' AND status = 1",
    gsub("'", "''", filename)
  ))

  if (is.null(fileid_result) || nrow(fileid_result) == 0) {
    message("File not found: ", filename)
    return(invisible(NULL))
  }

  fileid <- fileid_result$id[1]

  # Check if attribute exists
  attr_check <- rqda_sel(sprintf(
    "SELECT name FROM attributes WHERE name = '%s' AND status = 1",
    gsub("'", "''", attrname)
  ))

  if (is.null(attr_check) || nrow(attr_check) == 0) {
    message("Attribute not found: ", attrname)
    return(invisible(NULL))
  }

  # Check if value already exists
  existing <- rqda_sel(sprintf(
    "SELECT rowid FROM fileAttr WHERE variable = '%s' AND fileID = %d AND status = 1",
    gsub("'", "''", attrname), fileid
  ))

  if (!is.null(existing) && nrow(existing) > 0) {
    # Update existing
    rqda_exe(sprintf(
      "UPDATE fileAttr SET value = '%s', dateM = '%s' WHERE variable = '%s' AND fileID = %d",
      gsub("'", "''", as.character(value)), date(), gsub("'", "''", attrname), fileid
    ))
  } else {
    # Insert new
    rqda_exe(sprintf(
      "INSERT INTO fileAttr (variable, value, fileID, date, owner, status) VALUES ('%s', '%s', %d, '%s', '%s', 1)",
      gsub("'", "''", attrname), gsub("'", "''", as.character(value)), fileid, date(), .rqda$owner
    ))
  }

  message(sprintf("Set %s = %s for file %s", attrname, value, filename))

  invisible(NULL)
}

#' Get case attributes
#' @export
GetCaseAttributes <- function(casename) {
  if (!is_projOpen()) return(NULL)

  # Get case ID
  caseid_result <- rqda_sel(sprintf(
    "SELECT id FROM cases WHERE name = '%s' AND status = 1",
    gsub("'", "''", casename)
  ))

  if (is.null(caseid_result) || nrow(caseid_result) == 0) {
    return(NULL)
  }

  caseid <- caseid_result$id[1]

  # Get attributes
  attrs <- rqda_sel(sprintf(
    "SELECT variable, value FROM caseAttr WHERE caseID = %d AND status = 1",
    caseid
  ))

  return(attrs)
}

#' Get file attributes
#' @export
GetFileAttributes <- function(filename) {
  if (!is_projOpen()) return(NULL)

  # Get file ID
  fileid_result <- rqda_sel(sprintf(
    "SELECT id FROM source WHERE name = '%s' AND status = 1",
    gsub("'", "''", filename)
  ))

  if (is.null(fileid_result) || nrow(fileid_result) == 0) {
    return(NULL)
  }

  fileid <- fileid_result$id[1]

  # Get attributes
  attrs <- rqda_sel(sprintf(
    "SELECT variable, value FROM fileAttr WHERE fileID = %d AND status = 1",
    fileid
  ))

  return(attrs)
}

#' Show editable attribute value grid for all files or cases
#' @export
SetFileAttrValues <- function() {
  if (!is_projOpen()) return(invisible(NULL))
  .show_attr_values_editor("file")
}

#' @export
SetCaseAttrValues <- function() {
  if (!is_projOpen()) return(invisible(NULL))
  .show_attr_values_editor("case")
}

#' @keywords internal
.show_attr_values_editor <- function(type = "file") {
  attrs <- rqda_sel("SELECT name, class FROM attributes WHERE status=1 ORDER BY name")
  if (is.null(attrs) || nrow(attrs)==0) {
    gmessage("No attributes defined yet. Add attributes first.", icon="info")
    return(invisible(NULL))
  }

  entities <- if (type=="file")
    rqda_sel("SELECT id, name FROM source WHERE status=1 ORDER BY name")
  else
    rqda_sel("SELECT id, name FROM cases WHERE status=1 ORDER BY name")

  if (is.null(entities) || nrow(entities)==0) {
    gmessage(sprintf("No %ss found.", type), icon="info")
    return(invisible(NULL))
  }

  # Fetch all current values
  all_vals <- if (type=="file")
    rqda_sel("SELECT fileID AS eid, variable, value FROM fileAttr WHERE status=1")
  else
    rqda_sel("SELECT caseID AS eid, variable, value FROM caseAttr WHERE status=1")

  win <- gtkWindowNew()
  gtkWindowSetTitle(win, sprintf("Attribute Values — %ss", tools::toTitleCase(type)))
  gtkWindowSetDefaultSize(win, 700L, 500L)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(win, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 4L)
  gtkWindowSetChild(win, vbox)

  # Instruction label
  info_lbl <- gtkLabelNew(sprintf(
    "Set attribute values for each %s. Press Tab to move between cells.", type))
  gtkLabelSetXalign(info_lbl, 0.0)
  gtkWidgetSetMarginStart(info_lbl, 8L); gtkWidgetSetMarginTop(info_lbl, 6L)
  gtkBoxAppend(vbox, info_lbl)

  # Scrolled grid
  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)
  gtkBoxAppend(vbox, sw)

  grid <- gtkGridNew()
  gtkGridSetRowSpacing(grid, 2L)
  gtkGridSetColumnSpacing(grid, 4L)
  gtkWidgetSetMarginStart(grid, 8L); gtkWidgetSetMarginEnd(grid, 8L)
  gtkWidgetSetMarginBottom(grid, 8L)
  gtkScrolledWindowSetChild(sw, grid)

  # Header row: entity name col + one col per attribute
  header_entity <- gtkLabelNew(sprintf("<b>%s</b>", tools::toTitleCase(type)))
  gtkLabelSetUseMarkup(header_entity, TRUE)
  gtkLabelSetXalign(header_entity, 0.0)
  gtkWidgetSetMarginEnd(header_entity, 12L)
  gtkGridAttach(grid, header_entity, 0L, 0L, 1L, 1L)

  for (j in seq_len(nrow(attrs))) {
    cls <- attrs$class[j]
    lbl <- gtkLabelNew(sprintf("<b>%s</b>\n<small>(%s)</small>", attrs$name[j], cls))
    gtkLabelSetUseMarkup(lbl, TRUE)
    gtkLabelSetXalign(lbl, 0.5)
    gtkGridAttach(grid, lbl, as.integer(j), 0L, 1L, 1L)
  }

  # Data rows - one per entity, entries for each attribute
  entries <- list()  # entries[["eid_attrname"]] = entry widget

  for (i in seq_len(nrow(entities))) {
    eid   <- entities$id[i]
    ename <- entities$name[i]

    name_lbl <- gtkLabelNew(ename)
    gtkLabelSetXalign(name_lbl, 0.0)
    gtkWidgetSetMarginEnd(name_lbl, 12L)
    gtkGridAttach(grid, name_lbl, 0L, as.integer(i), 1L, 1L)

    for (j in seq_len(nrow(attrs))) {
      aname <- attrs$name[j]
      aclass <- attrs$class[j]

      # Look up current value
      cur <- ""
      if (!is.null(all_vals) && nrow(all_vals) > 0) {
        row <- all_vals[all_vals$eid == eid & all_vals$variable == aname, ]
        if (nrow(row) > 0) cur <- as.character(row$value[1])
      }

      entry <- gtkEntryNew()
      gtkEntryBufferSetText(gtkEntryGetBuffer(entry), cur, -1L)
      gtkWidgetSetSizeRequest(entry, 120L, -1L)

      # Tooltip shows class
      gtkWidgetSetTooltipText(entry, sprintf("%s (%s)", aname, aclass))

      key <- sprintf("%d__%s", eid, aname)
      entries[[key]] <- list(entry=entry, eid=eid, ename=ename,
                             aname=aname, aclass=aclass)
      gtkGridAttach(grid, entry, as.integer(j), as.integer(i), 1L, 1L)
    }
  }

  # Save button
  bbox     <- gtkBoxNew(0L, 8L)
  gtkWidgetSetHalign(bbox, 3L)
  gtkWidgetSetMarginEnd(bbox, 8L); gtkWidgetSetMarginBottom(bbox, 8L)
  save_btn <- gtkButtonNewWithLabel("Save All")
  gSignalConnectR(save_btn, "clicked", function(w) {
    n_saved <- 0L
    for (key in names(entries)) {
      info  <- entries[[key]]
      val   <- gtkEntryBufferGetText(gtkEntryGetBuffer(info$entry))
      # Validate numeric
      if (info$aclass == "numeric" && nzchar(val)) {
        if (is.na(suppressWarnings(as.numeric(val)))) {
          gmessage(sprintf("'%s' is not numeric for attribute '%s' on %s '%s'",
                           val, info$aname, type, info$ename), icon="warning")
          return()
        }
      }
      if (type == "file")
        SetFileAttribute(info$ename, info$aname, val)
      else
        SetCaseAttribute(info$ename, info$aname, val)
      n_saved <- n_saved + 1L
    }
    message(sprintf("Saved %d attribute values.", n_saved))
    gtkWindowClose(win)
  })
  gtkBoxAppend(bbox, save_btn)
  gtkBoxAppend(vbox, bbox)

  gtkWindowPresent(win)
  invisible(NULL)
}

#' Change class of selected attribute
#' @export
ChangeAttrClass <- function() {
  if (!is_projOpen()) return(invisible(NULL))
  sel <- .get_selected_attr()
  if (is.null(sel) || !nzchar(sel)) { gmessage("Select an attribute first.", icon="warning"); return(invisible(NULL)) }
  sel <- c(sel)  # ensure vector

  cur <- rqda_sel(sprintf("SELECT class FROM attributes WHERE name='%s' AND status=1",
                          gsub("'","''",sel[1])))
  cur_class <- if (!is.null(cur) && nrow(cur)>0) cur$class[1] else "character"

  res  <- list(val=NULL)
  loop <- gMainLoopNew(NULL, FALSE)
  win  <- gtkWindowNew()
  gtkWindowSetTitle(win, sprintf("Change class: %s", sel[1]))
  gtkWindowSetDefaultSize(win, 280L, 130L)
  gtkWindowSetModal(win, TRUE)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(win, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 10L)
  gtkWidgetSetMarginTop(vbox, 12L); gtkWidgetSetMarginBottom(vbox, 12L)
  gtkWidgetSetMarginStart(vbox, 12L); gtkWidgetSetMarginEnd(vbox, 12L)
  gtkWindowSetChild(win, vbox)

  hbox    <- gtkBoxNew(0L, 12L)
  rb_char <- gtkCheckButtonNewWithLabel("character")
  rb_num  <- gtkCheckButtonNewWithLabel("numeric")
  gtkCheckButtonSetActive(rb_char, cur_class == "character")
  gtkCheckButtonSetActive(rb_num,  cur_class == "numeric")
  state <- new.env(parent=emptyenv()); state$class <- cur_class
  gSignalConnectR(rb_char, "toggled", function(w) { state$class <- "character"; gtkCheckButtonSetActive(rb_num, FALSE) })
  gSignalConnectR(rb_num,  "toggled", function(w) { state$class <- "numeric";   gtkCheckButtonSetActive(rb_char, FALSE) })
  gtkBoxAppend(hbox, rb_char); gtkBoxAppend(hbox, rb_num)
  gtkBoxAppend(vbox, hbox)

  bbox <- gtkBoxNew(0L, 8L); gtkWidgetSetHalign(bbox, 3L)
  ok_btn <- gtkButtonNewWithLabel("OK")
  gSignalConnectR(ok_btn, "clicked", function(w) {
    rqda_exe(sprintf("UPDATE attributes SET class='%s', dateM='%s' WHERE name='%s'",
                     state$class, date(), gsub("'","''",sel[1])))
    message(sprintf("Class of '%s' changed to %s", sel[1], state$class))
    gMainLoopQuit(loop); gtkWindowDestroy(win)
  })
  gSignalConnectR(win, "close-request", function(w) { gMainLoopQuit(loop); FALSE })
  gtkBoxAppend(bbox, ok_btn); gtkBoxAppend(vbox, bbox)
  gtkWindowPresent(win); gMainLoopRun(loop)
  invisible(NULL)
}

#' File Attribute button handler - opens value editor for selected file
#' @export
FileAttribute_Button <- function(label, container = NULL, FileWidget = NULL, ...) {
  btn <- gbutton(label, container = container, handler = function(h, ...) {
    if (!is_projOpen()) return(invisible(NULL))
    # Get selected file
    fname <- tryCatch({
      sel <- as.character(svalue(.rqda$.fnames_rqda))
      sel <- sel[nzchar(sel)]
      if (length(sel) == 0) NULL else sel[1]
    }, error = function(e) NULL)
    if (is.null(fname)) {
      gmessage("Select a file first.", icon = "warning")
      return(invisible(NULL))
    }
    .show_attr_editor("file", fname)
  })
  btn
}

#' Open attribute editor for a file by ID (called from right-click menu)
#' @export
FileAttrFun <- function(fileId = NULL, title = NULL) {
  if (!is_projOpen()) return(invisible(NULL))
  if (!is.null(fileId)) {
    res <- rqda_sel(sprintf("SELECT name FROM source WHERE id=%d AND status=1", as.integer(fileId)))
    fname <- if (!is.null(res) && nrow(res)>0) res$name[1] else title
  } else {
    fname <- title
  }
  if (is.null(fname) || !nzchar(fname)) {
    gmessage("File not found.", icon="warning"); return(invisible(NULL))
  }
  .show_attr_editor("file", fname)
}
