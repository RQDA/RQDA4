# File management functions for RQDA

#' Import file into project
#' @export
ImportFileText <- function(paths = NULL, encoding = .rqda$encoding, con = .rqda$qdacon, ...) {

  if (!is_projOpen()) return(invisible(NULL))

  # If no paths provided, show file chooser
  if (is.null(paths)) {
    paths <- gfile(
      text = "Select file(s) to import",
      type = "open",
      multi = TRUE,
      filter = list(
        "Text files" = list(patterns = c("*.txt", "*.md", "*.text")),
        "All files" = list(patterns = c("*"))
      )
    )
  }

  if (identical(paths, character(0))) return(invisible(NULL))

  for (path in paths) {
    # Get filename without extension
    Fname <- gsub("\\.[[:alpha:]]*$", "", basename(path))
    FnameUTF8 <- iconv(Fname, to = "UTF-8")

    if (Fname != "") {
      # Read file content
      file_con <- file(path, open = "r")

      # Handle BOM if needed
      if (isTRUE(.rqda$BOM)) seek(file_con, 3)

      content <- readLines(file_con, warn = FALSE, encoding = encoding)
      close(file_con)

      content <- paste(content, collapse = "\n")

      # Detect encoding and convert to UTF-8
      if (requireNamespace("stringi", quietly = TRUE)) {
        dtct <- stringi::stri_enc_detect(content)[[1]]
        enc_hat <- dtct$Encoding[dtct$Confidence == max(dtct$Confidence)]
        message("File imported with encoding: ", enc_hat)
        content <- stringi::stri_encode(content, from = enc_hat, to = "UTF-8")
      } else {
        # Fallback if stringi not available
        Encoding(content) <- "UTF-8"
      }

      # Escape single quotes for SQL
      content <- gsub("'", "''", content)

      # Get next ID
      maxid <- rqda_sel("SELECT MAX(id) FROM source")[[1]]
      nextid <- ifelse(is.na(maxid), 1, maxid + 1)

      # Check for duplicate names
      write <- FALSE
      if (nextid == 1) {
        write <- TRUE
      } else {
        existing <- rqda_sel(sprintf(
          "SELECT name FROM source WHERE name='%s'", FnameUTF8
        ))
        if (is.null(existing) || nrow(existing) == 0) {
          write <- TRUE
        } else {
          message("A file with the same name exists in database: ", Fname)
        }
      }

      if (write) {
        rqda_exe(sprintf(
          "INSERT INTO source (name, file, id, status, date, owner) VALUES ('%s', '%s', %d, 1, '%s', '%s')",
          Fname, content, nextid, date(), .rqda$owner
        ))
        message("Imported file: ", Fname)
      }
    }
  }

  # Update file list
  if (isTRUE(.rqda$isLaunched)) {
    FileNamesUpdate()
  }
}

#' Delete file from project
#' @export
DeleteFile <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".fnames_rqda", envir = .rqda)) {
    gmessage("File widget not found", icon = "error")
    return(invisible(NULL))
  }

  SelectedFileName <- svalue(.rqda$.fnames_rqda)

  if (length(SelectedFileName) == 0) {
    gmessage("Please select a file first", icon = "warning")
    return(invisible(NULL))
  }

  # Confirm deletion
  confirm_msg <- if (length(SelectedFileName) == 1) {
    sprintf("Are you sure you want to delete '%s'?", SelectedFileName[1])
  } else {
    sprintf("Are you sure you want to delete %d files?", length(SelectedFileName))
  }

  if (!gconfirm(confirm_msg, icon = "question")) {
    return(invisible(NULL))
  }

  # Mark as deleted (status = 0)
  for (fname in SelectedFileName) {
    rqda_exe(sprintf(
      "UPDATE source SET status = 0 WHERE name = '%s'",
      fname
    ))
    message("Deleted file: ", fname)
  }

  FileNamesUpdate()
}

#' View file content
#' @export
ViewFile <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".fnames_rqda", envir = .rqda)) {
    message("File widget not found")
    return(invisible(NULL))
  }

  SelectedFileName <- svalue(.rqda$.fnames_rqda)

  if (length(SelectedFileName) == 0) {
    message("Select a file first")
    return(invisible(NULL))
  }

  ViewFileFunHelper(SelectedFileName[1], highlight = TRUE)
}

#' Helper function to view file
#' Replace file viewer content with editor in the same window
#' @keywords internal
.inline_edit <- function(FileName, fid, frame, viewer_win, codingTable = "coding") {
  # Get current file content from DB
  res <- rqda_sel(sprintf("SELECT file FROM source WHERE id=%d", fid))
  if (is.null(res) || nrow(res) == 0) return(invisible(NULL))
  content <- res$file[1]
  if (is.na(content)) content <- ""
  Encoding(content) <- "UTF-8"

  # Build editor UI inside the same frame
  edit_vbox <- gtkBoxNew(1L, 0L)
  gtkFrameSetChild(frame, edit_vbox)

  # Editor toolbar
  edit_toolbar <- gtkBoxNew(0L, 4L)
  gtkWidgetSetMarginTop(edit_toolbar, 4L); gtkWidgetSetMarginBottom(edit_toolbar, 4L)
  gtkWidgetSetMarginStart(edit_toolbar, 8L); gtkWidgetSetMarginEnd(edit_toolbar, 8L)
  gtkBoxAppend(edit_vbox, edit_toolbar)

  save_btn   <- gtkButtonNewWithLabel("💾 Save")
  cancel_btn <- gtkButtonNewWithLabel("✕ Cancel")
  edit_lbl   <- gtkLabelNew(sprintf("Editing: %s", FileName))
  gtkWidgetSetHexpand(edit_lbl, TRUE)
  gtkLabelSetXalign(edit_lbl, 0.0)
  gtkWidgetSetMarginStart(edit_lbl, 8L)
  gtkWidgetSetSensitive(save_btn, FALSE)
  gtkBoxAppend(edit_toolbar, edit_lbl)
  gtkBoxAppend(edit_toolbar, save_btn)
  gtkBoxAppend(edit_toolbar, cancel_btn)

  # Editor text view
  edit_sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(edit_sw, TRUE)
  gtkBoxAppend(edit_vbox, edit_sw)

  edit_tv <- gtkTextViewNew()
  apply_font_css(edit_tv)
  gtkTextViewSetWrapMode(edit_tv, 2L)
  gtkWidgetSetMarginStart(edit_tv, 12L); gtkWidgetSetMarginEnd(edit_tv, 12L)
  gtkWidgetSetMarginTop(edit_tv, 12L);   gtkWidgetSetMarginBottom(edit_tv, 12L)
  gtkScrolledWindowSetChild(edit_sw, edit_tv)

  edit_buf <- gtkTextViewGetBuffer(edit_tv)
  gtkTextBufferSetText(edit_buf, content, -1L)

  # Place coding marks so they move with edits
  mark_index <- rqda_sel(sprintf(
    "SELECT rowid, selfirst, selend FROM coding WHERE fid=%d AND status=1", fid))
  if (!is.null(mark_index) && nrow(mark_index) > 0) {
    apply(mark_index, 1, function(x) {
      iter1 <- gtkTextBufferGetIterAtOffset(edit_buf, as.integer(x["selfirst"]))
      gtkTextBufferCreateMark(edit_buf, sprintf("%s.1", x["rowid"]), iter1, TRUE)
      iter2 <- gtkTextBufferGetIterAtOffset(edit_buf, as.integer(x["selend"]))
      gtkTextBufferCreateMark(edit_buf, sprintf("%s.2", x["rowid"]), iter2, FALSE)
    })
  }

  gSignalConnectR(edit_buf, "changed", function(b) {
    gtkWidgetSetSensitive(save_btn, TRUE)
  })

  do_save <- function() {
    si <- gtkTextBufferGetStartIter(edit_buf)
    ei <- gtkTextBufferGetEndIter(edit_buf)
    new_content <- gtkTextBufferGetText(edit_buf, si, ei, FALSE)
    if (is.list(new_content)) new_content <- new_content[[1]]
    new_content <- as.character(new_content)
    rqda_exe(sprintf("UPDATE source SET file='%s', dateM='%s' WHERE id=%d",
                     enc(new_content, "UTF-8"), date(), fid))
    # Update coding offsets
    if (!is.null(mark_index) && nrow(mark_index) > 0) {
      for (i in seq_len(nrow(mark_index))) {
        rowid <- mark_index$rowid[i]
        m1 <- gtkTextBufferGetMark(edit_buf, sprintf("%s.1", rowid))
        m2 <- gtkTextBufferGetMark(edit_buf, sprintf("%s.2", rowid))
        if (!is.null(m1) && !is.null(m2)) {
          new_start <- gtkTextIterGetOffset(gtkTextBufferGetIterAtMark(edit_buf, m1))
          new_end   <- gtkTextIterGetOffset(gtkTextBufferGetIterAtMark(edit_buf, m2))
          if (new_start == new_end) {
            rqda_exe(sprintf("UPDATE coding SET status=0 WHERE rowid=%d", rowid))
          } else {
            new_sel <- substr(new_content, new_start+1, new_end)
            rqda_exe(sprintf(
              "UPDATE coding SET seltext='%s', selfirst=%d, selend=%d WHERE rowid=%d",
              enc(new_sel, "UTF-8"), new_start, new_end, rowid))
          }
        }
      }
    }
    message("File saved and codings updated.")
    gtkWidgetSetSensitive(save_btn, FALSE)
  }

  restore_viewer <- function() {
    # Reopen viewer in same window - reload from DB
    gtkWindowSetTitle(viewer_win$widget, sprintf("File: %s", FileName))
    ViewFileFunHelper(FileName, highlight = TRUE, codingTable = codingTable)
    # The new call creates a new window - close this one
    gtkWindowDestroy(viewer_win$widget)
  }

  gSignalConnectR(save_btn, "clicked", function(w) {
    do_save()
    restore_viewer()
  })

  gSignalConnectR(cancel_btn, "clicked", function(w) {
    restore_viewer()
  })

  gtkWindowSetTitle(viewer_win$widget, sprintf("Editing: %s", FileName))
  gtkWidgetGrabFocus(edit_tv)
  invisible(NULL)
}

ViewFileFunHelper <- function(FileName, highlight = TRUE, codingTable = .rqda$codingTable, annotation = TRUE) {

  # 1. Fetch Data
  file_data <- rqda_sel(sprintf(
    "SELECT file, id FROM source WHERE name = '%s' AND status = 1",
    gsub("'", "''", FileName)
  ))
  if (is.null(file_data) || nrow(file_data) == 0) return(invisible(NULL))

  # Force content to be a character string to avoid the ffs error
  content <- as.character(file_data$file[1])
  fid <- file_data$id[1]

  tryCatch({
    # 2. Window Setup
    gw <- gwindow(title = FileName, width = 800, height = 600)

    # 3. Create the Frame with a literal empty string
    # This avoids the NULL pointer error in your C code
    frame <- gtkFrameNew("")

    # Style the Frame (Border)
    gtkWidgetSetMarginTop(frame, 10L)
    gtkWidgetSetMarginBottom(frame, 10L)
    gtkWidgetSetMarginStart(frame, 10L)
    gtkWidgetSetMarginEnd(frame, 10L)

    # Attach Frame to Window
    gtkWindowSetChild(gw$widget, frame)

    # Build complete layout: frame > outer_vbox > toolbar + scrolled
    outer_vbox <- gtkBoxNew(1L, 0L)
    gtkFrameSetChild(frame, outer_vbox)

    toolbar <- gtkBoxNew(0L, 4L)
    gtkWidgetSetMarginTop(toolbar, 4L); gtkWidgetSetMarginBottom(toolbar, 4L)
    gtkWidgetSetMarginStart(toolbar, 8L); gtkWidgetSetMarginEnd(toolbar, 8L)
    gtkBoxAppend(outer_vbox, toolbar)

    scrolled <- gtkScrolledWindowNew()
    gtkWidgetSetVexpand(scrolled, TRUE)
    gtkBoxAppend(outer_vbox, scrolled)

    textview <- gtkTextViewNew()
    gtkTextViewSetEditable(textview, FALSE)
    gtkTextViewSetCursorVisible(textview, TRUE)  # still show cursor for position
    gtkScrolledWindowSetChild(scrolled, textview)

    buffer <- gtkTextViewGetBuffer(textview)
    gtkTextBufferSetText(buffer, content, -1L)
    gtkTextViewSetWrapMode(textview, 2L)
    gtkWidgetSetMarginStart(textview, 12L); gtkWidgetSetMarginEnd(textview, 12L)
    gtkWidgetSetMarginTop(textview, 12L);  gtkWidgetSetMarginBottom(textview, 12L)

    tmp <- structure(list(
      widget = scrolled,
      textview = textview,
      buffer = buffer
    ), class = "gtext")

    assign(".root_edit", gw, envir = .rqda)
    assign(".openfile_gui", tmp, envir = .rqda)
    assign(".openfile_fid", fid, envir = .rqda)
    # Expose do_reload so external code (e.g. AddCode) can refresh the dropdown
    assign(".viewer_reload", function() {}, envir = .rqda)  # placeholder, set after do_reload defined

    if (highlight) {
      LoadCodings(fid, codingTable = codingTable)
    }

    codes_df  <- rqda_sel("SELECT name, color FROM freecode WHERE status=1 ORDER BY name")
    code_store <- gtkStringListNew(NULL)
    if (!is.null(codes_df) && nrow(codes_df) > 0) {
      for (i in seq_len(nrow(codes_df))) {
        col    <- codes_df$color[i]
        gtkStringListAppend(code_store, codes_df$name[i])
      }
    }
    code_drop <- gtkDropDownNew(code_store, NULL)
    gtkWidgetSetTooltipText(code_drop, "Select code to mark with")
    gtkBoxAppend(toolbar, code_drop)

    mark_btn   <- gtkButtonNewWithLabel("Mark")
    unmark_btn <- gtkButtonNewWithLabel("Unmark")
    status_lbl <- gtkLabelNew("")
    gtkLabelSetXalign(status_lbl, 0.0)
    gtkWidgetSetHexpand(status_lbl, TRUE)
    gtkWidgetSetMarginStart(status_lbl, 8L)

    # Edit button (gear icon or fallback text)
    edit_btn <- gtkButtonNewWithLabel("✎ Edit")
    gtkWidgetSetTooltipText(edit_btn, "Switch to file editor")
    gSignalConnectR(edit_btn, "clicked", function(w) {
      .inline_edit(FileName, fid, frame, gw, codingTable)
    })

    anno_btn <- gtkButtonNewWithLabel("✍ Anno")
    gtkWidgetSetTooltipText(anno_btn, "Add/edit annotation for selected text")
    gSignalConnectR(anno_btn, "clicked", function(w) {
      addAnnotation(fid, textview)
    })

    gtkBoxAppend(toolbar, mark_btn)
    gtkBoxAppend(toolbar, unmark_btn)
    gtkBoxAppend(toolbar, status_lbl)
    gtkBoxAppend(toolbar, anno_btn)
    gtkBoxAppend(toolbar, edit_btn)

    # ── View settings hamburger menu ───────────────────────────────────
    menu_btn <- gtkMenuButtonNew()
    # Set label via child widget - avoids gtkButtonSetLabel on GtkMenuButton
    tryCatch({
      lbl_child <- gtkLabelNew("☰") # ☰ hamburger
      gtkMenuButtonSetChild(menu_btn, lbl_child)
    }, error = function(e) {
      tryCatch(gtkMenuButtonSetLabel(menu_btn, "⋮"), error=function(e2){})
    })
    gtkWidgetSetTooltipText(menu_btn, "View options")
    gtkBoxAppend(toolbar, menu_btn)

    # Popover content
    pop_box <- gtkBoxNew(1L, 8L)
    gtkWidgetSetMarginTop(pop_box, 10L); gtkWidgetSetMarginBottom(pop_box, 10L)
    gtkWidgetSetMarginStart(pop_box, 12L); gtkWidgetSetMarginEnd(pop_box, 12L)

    add_pop_row <- function(label_txt, widget) {
      row <- gtkBoxNew(0L, 10L)
      lbl <- gtkLabelNew(label_txt)
      gtkLabelSetXalign(lbl, 0.0)
      gtkWidgetSetHexpand(lbl, TRUE)
      gtkBoxAppend(row, lbl)
      gtkBoxAppend(row, widget)
      gtkBoxAppend(pop_box, row)
      row
    }

    # Reload codings helper (applies changes immediately)
    apply_viewer_settings <- function() {
      tryCatch({
        if (exists(".viewer_reload", envir=.rqda) &&
            is.function(.rqda$.viewer_reload))
          .rqda$.viewer_reload()
      }, error=function(e){})
    }

    # Line spacing
    spacing_box <- gtkBoxNew(0L, 4L)
    sp_minus <- gtkButtonNewWithLabel("−")
    sp_lbl   <- gtkLabelNew("0px")
    sp_plus  <- gtkButtonNewWithLabel("+")
    gtkBoxAppend(spacing_box, sp_minus)
    gtkBoxAppend(spacing_box, sp_lbl)
    gtkBoxAppend(spacing_box, sp_plus)
    spacing <- new.env(parent=emptyenv()); spacing$n <- 0L
    update_spacing <- function() {
      gtkLabelSetText(sp_lbl, sprintf("%dpx", spacing$n))
      tryCatch({
        gtkTextViewSetPixelsAboveLines(textview, as.integer(spacing$n))
        gtkTextViewSetPixelsBelowLines(textview, as.integer(spacing$n %/% 2L))
        gtkTextViewSetPixelsInsideWrap(textview, as.integer(spacing$n))
      }, error=function(e){})
    }
    gSignalConnectR(sp_plus,  "clicked", function(w) { spacing$n <- spacing$n + 1L; update_spacing() })
    gSignalConnectR(sp_minus, "clicked", function(w) { spacing$n <- max(0L, spacing$n - 1L); update_spacing() })
    add_pop_row("Line spacing", spacing_box)

    # Wrap mode toggle
    wrap_check <- gtkCheckButtonNew()
    gtkCheckButtonSetActive(wrap_check, TRUE)
    wrap_state <- new.env(parent=emptyenv()); wrap_state$on <- TRUE
    gSignalConnectR(wrap_check, "toggled", function(w) {
      wrap_state$on <- !wrap_state$on
      gtkTextViewSetWrapMode(textview, if (wrap_state$on) 2L else 0L)
    })
    add_pop_row("Wrap text", wrap_check)

    # Font size (zoom) spinner-style: + and -
    zoom_box <- gtkBoxNew(0L, 4L)
    zoom_minus <- gtkButtonNewWithLabel("−")
    zoom_lbl   <- gtkLabelNew(sprintf("%dpt", as.integer(.rqda$font.size %||% 11L)))
    zoom_plus  <- gtkButtonNewWithLabel("+")
    gtkBoxAppend(zoom_box, zoom_minus)
    gtkBoxAppend(zoom_box, zoom_lbl)
    gtkBoxAppend(zoom_box, zoom_plus)
    zoom_delta <- new.env(parent=emptyenv()); zoom_delta$n <- 0L
    gSignalConnectR(zoom_plus, "clicked", function(w) {
      zoom_delta$n <- zoom_delta$n + 1L
      sz <- max(6L, (.rqda$font.size %||% 11L) + zoom_delta$n)
      gtkLabelSetText(zoom_lbl, sprintf("%dpt", sz))
      apply_font_css(textview, size=sz)
    })
    gSignalConnectR(zoom_minus, "clicked", function(w) {
      zoom_delta$n <- zoom_delta$n - 1L
      sz <- max(6L, (.rqda$font.size %||% 11L) + zoom_delta$n)
      gtkLabelSetText(zoom_lbl, sprintf("%dpt", sz))
      apply_font_css(textview, size=sz)
    })
    add_pop_row("Font size", zoom_box)

    popover <- gtkPopoverNew()
    gtkPopoverSetChild(popover, pop_box)
    gtkPopoverSetAutohide(popover, TRUE)
    gtkMenuButtonSetPopover(menu_btn, popover)

    # Update status label when dropdown selection changes
    gSignalConnectR(code_drop, "notify::selected-item", function(obj, pspec) {
      tryCatch({
        idx  <- gtkDropDownGetSelected(code_drop)
        if (!is.null(idx) && idx >= 0L) {
          item <- gtkStringListGetString(code_store, as.integer(idx))
          if (!is.null(item) && nzchar(item))
            gtkLabelSetText(status_lbl, trimws(item))
        }
      }, error=function(e){})
    })

    get_selected_code <- function() {
      idx  <- gtkDropDownGetSelected(code_drop)
      if (is.null(idx) || idx < 0L) return(NULL)
      item <- gtkStringListGetString(code_store, as.integer(idx))
      if (is.null(item) || item == "") return(NULL)
      item
    }

    do_reload <- function() {
      bounds <- gtkTextBufferGetBounds(buffer)
      # Remove tags for each known coding color explicitly
      tag_table <- gtkTextBufferGetTagTable(buffer)
      tryCatch(
        gtkTextTagTableForeach(tag_table, function(tag) {
          nm <- tryCatch(gObjectGetProperty(tag, "name"), error=function(e) "")
          if (!is.null(nm) && nzchar(nm) && (startsWith(nm,"fg_") || startsWith(nm,"bg_") || startsWith(nm,"tip_")))
            tryCatch(gtkTextBufferRemoveTag(buffer, tag, bounds$start, bounds$end), error=function(e){})
        }),
        error = function(e)
          gtkTextBufferRemoveAllTags(buffer, bounds$start, bounds$end)
      )
      LoadCodings(fid, codingTable = codingTable)
      fresh <- rqda_sel("SELECT name, color FROM freecode WHERE status=1 ORDER BY name")
      new_store <- gtkStringListNew(NULL)
      if (!is.null(fresh) && nrow(fresh) > 0) {
        for (i in seq_len(nrow(fresh))) {
          col    <- fresh$color[i]
          gtkStringListAppend(new_store, fresh$name[i])
        }
      }
      gtkDropDownSetModel(code_drop, new_store)
    }

    do_mark <- function() {
      code_name <- get_selected_code()
      if (is.null(code_name)) { gtkLabelSetText(status_lbl, "Select a code first"); return() }
      code_data <- rqda_sel(sprintf(
        "SELECT id, color FROM freecode WHERE name='%s' AND status=1", gsub("'","''",code_name)))
      if (is.null(code_data) || nrow(code_data)==0) return()
      cid   <- code_data$id[1]
      color <- code_data$color[1]
      if (is.na(color) || color=="") color <- .rqda$fore.col
      result <- mark(textview=textview, fore.col=color, back.col=NULL)
      if (is.null(result)) { gtkLabelSetText(status_lbl, "Select text first"); return() }
      rqda_exe(sprintf(
        "INSERT INTO coding (cid,fid,seltext,selfirst,selend,status,owner,date) VALUES (%d,%d,'%s',%g,%g,1,'%s','%s')",
        cid, fid, gsub("'","''",result$text), result$start, result$end, .rqda$owner, date()))
      do_reload()
      gtkLabelSetText(status_lbl, sprintf("Marked: %s", code_name))
    }

    do_unmark <- function() {
      index <- sindex(textview)
      if (index$seltext=="") { gtkLabelSetText(status_lbl, "Select coded text first"); return() }
      codings <- rqda_sel(sprintf(
        "SELECT rowid, cid FROM coding WHERE fid=%d AND status=1 AND selfirst<=%g AND selend>=%g",
        fid, index$endN, index$startN))
      if (is.null(codings) || nrow(codings)==0) { gtkLabelSetText(status_lbl, "No coding at selection"); return() }
      n <- rqda_exe(sprintf("UPDATE coding SET status=0 WHERE rowid=%d", codings$rowid[1]))
      # Verify update worked
      check <- rqda_sel(sprintf("SELECT status FROM coding WHERE rowid=%d", codings$rowid[1]))
      message("Unmark: rowid=", codings$rowid[1], " new status=", if(!is.null(check)) check$status[1] else "NULL")
      do_reload()
      gtkLabelSetText(status_lbl, "Unmarked")
    }

    gSignalConnectR(mark_btn,   "clicked", function(w) do_mark())
    gSignalConnectR(unmark_btn, "clicked", function(w) do_unmark())

    # Expose do_reload for external refresh (called after AddCode)
    assign(".viewer_reload", do_reload, envir = .rqda)
    # Expose search bar show function for menu access
    assign(".viewer_search_show", function() {
      gtkWidgetSetVisible(search_bar, TRUE)
      gtkWidgetGrabFocus(search_entry)
    }, envir = .rqda)

    gSignalConnectR(buffer, "mark-set", function(buf, iter, mark) {
      offset <- tryCatch(gtkTextIterGetOffset(iter), error=function(e) -1L)
      if (offset < 0L) return()
      at <- rqda_sel(sprintf(
        "SELECT f.name FROM coding c JOIN freecode f ON c.cid=f.id WHERE c.fid=%d AND c.status=1 AND c.selfirst<=%d AND c.selend>%d",
        fid, offset, offset))
      if (!is.null(at) && nrow(at)>0) {
        Encoding(at$name) <- "UTF-8"
        gtkLabelSetMarkup(status_lbl,
                          paste(sprintf("<b>%s</b>",
                                        gsub("&","&amp;",gsub("<","&lt;",at$name))),
                                collapse=", "))
      } else {
        gtkLabelSetText(status_lbl, "")
      }
    })

    # ── Search bar (hidden by default, Cmd+F to show) ──────────────────
    search_bar  <- gtkBoxNew(0L, 4L)
    gtkWidgetSetMarginStart(search_bar, 8L); gtkWidgetSetMarginEnd(search_bar, 8L)
    gtkWidgetSetMarginTop(search_bar, 2L); gtkWidgetSetMarginBottom(search_bar, 2L)
    gtkWidgetSetVisible(search_bar, FALSE)
    gtkBoxAppend(outer_vbox, search_bar)

    search_entry <- gtkEntryNew()
    gtkWidgetSetHexpand(search_entry, TRUE)
    gtkWidgetSetPlaceholderText <- function(e, t) tryCatch(
      gtkEntrySetPlaceholderText(e, t), error=function(e2){})
    gtkWidgetSetPlaceholderText(search_entry, "Search in file...")

    prev_btn <- gtkButtonNewWithLabel("▲")
    next_btn <- gtkButtonNewWithLabel("▼")
    close_btn <- gtkButtonNewWithLabel("✕")
    match_lbl <- gtkLabelNew("")

    gtkBoxAppend(search_bar, search_entry)
    gtkBoxAppend(search_bar, prev_btn)
    gtkBoxAppend(search_bar, next_btn)
    gtkBoxAppend(search_bar, match_lbl)
    gtkBoxAppend(search_bar, close_btn)

    search_state <- new.env(parent=emptyenv())
    search_state$matches <- list()
    search_state$current <- 0L

    # Ensure search highlight tag exists
    .ensure_search_tag <- function() {
      tag_table <- gtkTextBufferGetTagTable(buffer)
      tag <- gtkTextTagTableLookup(tag_table, "search_highlight")
      if (is.null(tag) || is.character(tag)) {
        tag <- gtkTextTagNew("search_highlight")
        gObjectSetString(tag, "background", "#FFE135")
        gObjectSetString(tag, "foreground", "#000000")
        gObjectSetEnum(tag, "weight", 700L)   # PANGO_WEIGHT_BOLD
        gtkTextTagTableAdd(tag_table, tag)
      }
      tag
    }

    highlight_search <- function(query) {
      tag <- .ensure_search_tag()
      bounds <- gtkTextBufferGetBounds(buffer)
      gtkTextBufferRemoveTag(buffer, tag, bounds$start, bounds$end)
      search_state$matches <<- list()
      search_state$current <<- 0L
      if (!nzchar(query)) { gtkLabelSetText(match_lbl, ""); return() }

      # Get full buffer text and find all match positions via R
      si  <- gtkTextBufferGetStartIter(buffer)
      ei  <- gtkTextBufferGetEndIter(buffer)
      full_text <- gtkTextBufferGetText(buffer, si, ei, FALSE)
      if (is.list(full_text)) full_text <- full_text[[1]]
      full_text <- as.character(full_text)

      # Find all occurrences (case-insensitive)
      starts <- gregexpr(query, full_text, fixed=TRUE, ignore.case=TRUE)[[1]]
      if (length(starts)==1 && starts[1]==-1) {
        gtkLabelSetText(match_lbl, "No matches"); return()
      }
      ends <- starts + attr(starts, "match.length") - 1L

      matches <- vector("list", length(starts))
      for (k in seq_along(starts)) {
        # Convert character offset to byte offset for GTK iter
        # GTK uses character count, not byte count
        s_iter <- gtkTextBufferGetIterAtOffset(buffer, as.integer(starts[k]-1L))
        e_iter <- gtkTextBufferGetIterAtOffset(buffer, as.integer(ends[k]))
        gtkTextBufferApplyTag(buffer, tag, s_iter, e_iter)
        matches[[k]] <- list(start=s_iter, end=e_iter)
      }
      search_state$matches <<- matches
      search_state$current <<- 1L
      gtkLabelSetText(match_lbl, sprintf("1/%d", length(matches)))
      gtkTextViewScrollToIter(textview, matches[[1]]$start, 0.1, FALSE, 0, 0)
    }

    jump_to <- function(delta) {
      n <- length(search_state$matches)
      if (n == 0) return()
      search_state$current <<- ((search_state$current - 1L + delta) %% n) + 1L
      m <- search_state$matches[[search_state$current]]
      gtkTextViewScrollToIter(textview, m$start, 0.1, FALSE, 0, 0)
      gtkLabelSetText(match_lbl, sprintf("%d/%d", search_state$current, n))
    }

    gSignalConnectR(search_entry, "changed", function(w) {
      q <- gtkEntryBufferGetText(gtkEntryGetBuffer(w))
      highlight_search(q)
    })
    gSignalConnectR(next_btn,  "clicked", function(w) jump_to(+1L))
    gSignalConnectR(prev_btn,  "clicked", function(w) jump_to(-1L))
    gSignalConnectR(close_btn, "clicked", function(w) {
      gtkWidgetSetVisible(search_bar, FALSE)
      highlight_search("")  # clear highlights
    })

    # ── Right-click menu ───────────────────────────────────────────────
    rc_gesture <- gtkGestureClickNew()
    gtkGestureSingleSetButton(rc_gesture, 0L)
    gtkEventControllerSetPropagationPhase(rc_gesture, 1L)
    gSignalConnectR(rc_gesture, "released", function(g, n_press, x, y) {
      event     <- tryCatch(gtkEventControllerGetCurrentEvent(g), error=function(e) NULL)
      event_btn <- tryCatch(gdkButtonEventGetButton(event), error=function(e) 0L)
      if (!isTRUE(event_btn == 3L)) return()

      menu_model <- gMenuNew()
      gMenuAppend(menu_model, "Mark", "viewer.mark")
      gMenuAppend(menu_model, "Unmark", "viewer.unmark")
      gMenuAppend(menu_model, "Annotate", "viewer.annotate")

      ag <- gSimpleActionGroupNew()
      act_mark <- gSimpleActionNew("mark", NULL)
      gSignalConnectR(act_mark, "activate", function(a, p) do_mark())
      gActionMapAddAction(ag, act_mark)

      act_unmark <- gSimpleActionNew("unmark", NULL)
      gSignalConnectR(act_unmark, "activate", function(a, p) do_unmark())
      gActionMapAddAction(ag, act_unmark)

      act_anno <- gSimpleActionNew("annotate", NULL)
      gSignalConnectR(act_anno, "activate", function(a, p) addAnnotation(fid, textview))
      gActionMapAddAction(ag, act_anno)

      gtkWidgetInsertActionGroup(textview, "viewer", ag)

      popover <- gtkPopoverMenuNewFromModel(menu_model)
      gtkWidgetSetParent(popover, textview)
      tryCatch(gtkPopoverSetOffset(popover,
                                   as.integer(x - gtkWidgetGetWidth(textview)/2),
                                   as.integer(-(gtkWidgetGetHeight(textview) - y) - 10L)),
               error=function(e){})
      gtkPopoverPopup(popover)
    })
    gtkWidgetAddController(textview, rc_gesture)

    # ── Configurable hotkeys ────────────────────────────────────────────
    get_hotkey <- function(action, default) {
      key <- tryCatch(.rqda$hotkeys[[action]], error=function(e) NULL)
      if (is.null(key) || !nzchar(key)) default else key
    }

    # Apply font and add key zoom
    apply_font_css(textview)
    local_zoom <- new.env(parent=emptyenv()); local_zoom$delta <- 0L
    key_ctrl <- gtkEventControllerKeyNew()
    gSignalConnectR(key_ctrl, "key-pressed", function(ctrl, keyval, keycode, state) {
      ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
      mark_key   <- get_hotkey("mark",   "m")
      unmark_key <- get_hotkey("unmark", "u")
      if (ch == mark_key || ch == toupper(mark_key)) {
        do_mark(); return(TRUE)
      } else if (ch == unmark_key || ch == toupper(unmark_key)) {
        do_unmark(); return(TRUE)
      } else if (ch == "f" || ch == "F") {
        gtkWidgetSetVisible(search_bar, TRUE)
        gtkWidgetGrabFocus(search_entry)
        return(TRUE)
      } else if (ch == "") {  # Escape
        gtkWidgetSetVisible(search_bar, FALSE)
        highlight_search("")
        return(TRUE)
      } else if (ch %in% c("+","=")) {
        local_zoom$delta <- local_zoom$delta + 1L
        apply_font_css(textview, size=max(6L, (.rqda$font.size %||% 11L) + local_zoom$delta))
        return(TRUE)
      } else if (ch == "-") {
        local_zoom$delta <- local_zoom$delta - 1L
        apply_font_css(textview, size=max(6L, (.rqda$font.size %||% 11L) + local_zoom$delta))
        return(TRUE)
      } else if (ch == "0") {
        local_zoom$delta <- 0L; apply_font_css(textview); return(TRUE)
      }
      FALSE
    })
    gtkWidgetAddController(textview, key_ctrl)

    gtkWindowPresent(gw$widget)
    invisible(gw)

  }, error = function(e) {
    message("Error: ", e$message)
  })
}

#' Create new empty file
#' @export
NewFile <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  # 1. Blocking Input
  fname <- ginput(
    message = "Enter the name for the new file:",
    text = "",
    title = "New File"
  )

  # 2. Handle Cancel or Empty String
  if (is.null(fname) || fname == "") return(invisible(NULL))

  # 3. Duplicate Check
  existing <- rqda_sel(sprintf("SELECT name FROM source WHERE name = '%s' AND status = 1", fname))
  if (!is.null(existing) && nrow(existing) > 0) {
    gmessage("A file with this name already exists!", icon = "error")
    return(invisible(NULL))
  }

  # 4. Database Insert
  maxid <- rqda_sel("SELECT MAX(id) FROM source")[[1]]
  nextid <- if (is.na(maxid)) 1 else maxid + 1

  rqda_exe(sprintf("INSERT INTO source (name, file, id, status, date, owner) VALUES ('%s', '', %d, 1, '%s', '%s')",
                   fname, as.integer(nextid), date(), .rqda$owner))

  # 5. Force UI Update
  FileNamesUpdate()
  EditFileFun(forceName = fname)
  return(invisible(NULL))
}

#' Rename file
#' @export
RenameFile <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".fnames_rqda", envir = .rqda)) {
    gmessage("File widget not found", icon = "error")
    return(invisible(NULL))
  }

  SelectedFileName <- tryCatch(as.character(svalue(.rqda$.fnames_rqda)), error=function(e) character(0))
  SelectedFileName <- SelectedFileName[!is.na(SelectedFileName) & nzchar(SelectedFileName)]

  if (length(SelectedFileName) == 0) {
    gmessage("Please select a file first", icon = "warning")
    return(invisible(NULL))
  }

  # Show dialog for new name
  new_name <- ginput(
    message = sprintf("Enter new name for '%s':", SelectedFileName[1]),
    text = SelectedFileName[1],
    title = "Rename File"
  )

  if (is.null(new_name) || length(new_name) == 0) return(invisible(NULL))
  new_name <- as.character(new_name)[1]
  if (is.na(new_name) || !nzchar(new_name)) return(invisible(NULL))

  # Check for duplicates
  existing <- rqda_sel(sprintf(
    "SELECT name FROM source WHERE name = '%s' AND status = 1 AND name != '%s'",
    new_name, SelectedFileName[1]
  ))

  if (!is.null(existing) && nrow(existing) > 0) {
    gmessage("A file with this name already exists!", icon = "error")
    return(invisible(NULL))
  }

  rqda_exe(sprintf(
    "UPDATE source SET name = '%s', dateM = '%s' WHERE name = '%s'",
    new_name, date(), SelectedFileName[1]
  ))

  message("Renamed file to: ", new_name)
  FileNamesUpdate()
}

#' File memo
#' @export
FileMemo <- function() {
  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".fnames_rqda", envir = .rqda)) {
    message("File widget not found")
    return(invisible(NULL))
  }

  SelectedFileName <- svalue(.rqda$.fnames_rqda)

  if (length(SelectedFileName) == 0) {
    message("Select a file first")
    return(invisible(NULL))
  }

  # Get current memo
  memo_data <- rqda_sel(sprintf(
    "SELECT memo FROM source WHERE name = '%s'",
    SelectedFileName[1]
  ))

  current_memo <- if (!is.null(memo_data) && nrow(memo_data) > 0) {
    memo_data$memo[1]
  } else {
    ""
  }

  if (is.na(current_memo)) current_memo <- ""

  # Create memo window
  memo_win <- gwindow(
    title = paste("Memo for:", SelectedFileName[1]),
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
        "UPDATE source SET memo = '%s', dateM = '%s' WHERE name = '%s'",
        new_memo, date(), SelectedFileName[1]
      ))

      message("Memo saved for: ", SelectedFileName[1])
      dispose(memo_win)
    }
  )

  invisible(memo_win)
}

#' Edit File Function for GTK4
#' @export
EditFileFun <- function(FileNameWidget = .rqda$.fnames_rqda, forceName = NULL) {
  if (!is_projOpen()) return()

  # Use forceName if provided, otherwise check the widget
  SelectedFileName <- if (!is.null(forceName)) forceName else svalue(FileNameWidget)

  if (is.null(SelectedFileName) || length(SelectedFileName) == 0 || SelectedFileName == "") {
    gmessage("Select a file first.", icon = "error")
    return()
  }

  # 2. Get File ID and Content
  res <- rqda_sel(sprintf("select id, file from source where name='%s'", enc(SelectedFileName, "UTF-8")))
  if (nrow(res) == 0) return()
  fid <- res$id
  content <- res$file
  Encoding(content) <- "UTF-8"

  # 3. Create the Edit Window
  # Close old one if it exists
  if (exists(".root_edit", envir = .rqda)) {
    try(gtkWindowDestroy(get(".root_edit", envir = .rqda)), silent = TRUE)
  }

  edit_win <- gtkWindowNew()
  gtkWindowSetTitle(edit_win, paste("Editing:", SelectedFileName))
  gtkWindowSetDefaultSize(edit_win, as.integer(800), as.integer(600))
  assign(".root_edit", edit_win, envir = .rqda)

  # 4. Layout
  vbox <- gtkBoxNew(1L, as.integer(5)) # Vertical
  gtkWidgetSetMarginStart(vbox, 8L); gtkWidgetSetMarginEnd(vbox, 8L)
  gtkWidgetSetMarginTop(vbox, 6L); gtkWidgetSetMarginBottom(vbox, 4L)
  gtkWindowSetChild(edit_win, vbox)

  # ── Toolbar ───────────────────────────────────────────────────────────
  toolbar <- gtkBoxNew(0L, 8L)
  gtkWidgetSetMarginBottom(toolbar, 2L)
  gtkBoxAppend(vbox, toolbar)

  save_btn <- gtkButtonNewWithLabel("Save")
  gtkWidgetSetSensitive(save_btn, FALSE)
  gtkWidgetSetTooltipText(save_btn, "Save changes to database (Cmd+S / Ctrl+S)")
  gtkBoxAppend(toolbar, save_btn)

  # Modified indicator
  mod_lbl <- gtkLabelNew("")
  gtkLabelSetMarkup(mod_lbl, "<span foreground='#888888'>Saved</span>")
  gtkBoxAppend(toolbar, mod_lbl)

  # Spacer
  spacer <- gtkBoxNew(0L, 0L)
  gtkWidgetSetHexpand(spacer, TRUE)
  gtkBoxAppend(toolbar, spacer)

  # Word/line count label
  count_lbl <- gtkLabelNew("")
  gtkLabelSetXalign(count_lbl, 1.0)
  gtkBoxAppend(toolbar, count_lbl)

  gtkBoxAppend(vbox, gtkSeparatorNew(0L))

  # ── Text View ─────────────────────────────────────────────────────────
  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)
  gtkBoxAppend(vbox, sw)

  textview <- gtkTextViewNew()
  apply_font_css(textview)
  gtkTextViewSetWrapMode(textview, 2L)
  buffer <- gtkTextViewGetBuffer(textview)
  gtkTextBufferSetText(buffer, content, -1L)
  gtkScrolledWindowSetChild(sw, textview)

  gtkBoxAppend(vbox, gtkSeparatorNew(0L))

  # ── Status bar ────────────────────────────────────────────────────────
  status_bar <- gtkBoxNew(0L, 12L)
  gtkWidgetSetMarginTop(status_bar, 2L); gtkWidgetSetMarginBottom(status_bar, 2L)

  pos_lbl    <- gtkLabelNew("Line 1, Col 1")
  sel_lbl    <- gtkLabelNew("")
  coding_lbl <- gtkLabelNew("")
  enc_lbl    <- gtkLabelNew(sprintf("UTF-8  ·  %s", SelectedFileName))

  for (l in list(pos_lbl, sel_lbl, coding_lbl)) {
    gtkLabelSetXalign(l, 0.0)
    gtkBoxAppend(status_bar, l)
    sep <- gtkSeparatorNew(1L)  # vertical
    gtkWidgetSetMarginStart(sep, 2L); gtkWidgetSetMarginEnd(sep, 2L)
    gtkBoxAppend(status_bar, sep)
  }
  enc_spacer <- gtkBoxNew(0L, 0L); gtkWidgetSetHexpand(enc_spacer, TRUE)
  gtkBoxAppend(status_bar, enc_spacer)
  gtkLabelSetXalign(enc_lbl, 1.0)
  gtkBoxAppend(status_bar, enc_lbl)
  gtkBoxAppend(vbox, status_bar)

  # Update count and status labels
  update_counts <- function() {
    si  <- gtkTextBufferGetStartIter(buffer)
    ei  <- gtkTextBufferGetEndIter(buffer)
    raw <- gtkTextBufferGetText(buffer, si, ei, FALSE)
    if (is.list(raw)) raw <- raw[[1]]
    txt <- as.character(raw)
    words <- length(strsplit(trimws(txt), "\\s+")[[1]])
    lines <- length(strsplit(txt, "\n")[[1]])
    chars <- nchar(txt)
    gtkLabelSetText(count_lbl,
                    sprintf("%d words  ·  %d lines  ·  %d chars", words, lines, chars))
  }
  update_counts()

  # 5. Handle Coding Marks (The "Elastic" logic)
  # This grabs existing codings and places invisible marks at their start/end
  mark_index <- rqda_sel(sprintf("select rowid, selfirst, selend from coding where fid=%i and status=1", fid))

  if (nrow(mark_index) > 0) {
    apply(mark_index, 1, function(x) {
      # Start Mark
      iter1 <- gtkTextBufferGetIterAtOffset(buffer, as.integer(x["selfirst"]))
      gtkTextBufferCreateMark(buffer, sprintf("%s.1", x["rowid"]), iter1, TRUE)
      # End Mark
      iter2 <- gtkTextBufferGetIterAtOffset(buffer, as.integer(x["selend"]))
      gtkTextBufferCreateMark(buffer, sprintf("%s.2", x["rowid"]), iter2, FALSE)
    })
  }

  # 6. Save Logic
  gSignalConnectR(save_btn, "clicked", function(w) {
    # A. Get new content
    start_iter <- gtkTextBufferGetStartIter(buffer)
    end_iter <- gtkTextBufferGetEndIter(buffer)
    new_content <- gtkTextBufferGetText(buffer, start_iter, end_iter, FALSE)

    # B. Update Source Table
    rqda_exe(sprintf("update source set file='%s', dateM='%s' where id=%i",
                     enc(new_content, "UTF-8"), date(), fid))

    # C. Update Coding Table Offsets (Recalculate based on marks)
    if (nrow(mark_index) > 0) {
      for (i in 1:nrow(mark_index)) {
        rowid <- mark_index$rowid[i]

        # Find where the marks moved to
        m1 <- gtkTextBufferGetMark(buffer, sprintf("%s.1", rowid))
        iter1 <- gtkTextBufferGetIterAtMark(buffer, m1)
        new_start <- gtkTextIterGetOffset(iter1)

        m2 <- gtkTextBufferGetMark(buffer, sprintf("%s.2", rowid))
        iter2 <- gtkTextBufferGetIterAtMark(buffer, m2)
        new_end <- gtkTextIterGetOffset(iter2)

        # Extract new selected text for the database
        new_seltext <- substr(new_content, new_start + 1, new_end)

        # If the code was deleted (start == end), we mark it as status = 0
        if (new_start == new_end) {
          rqda_exe(sprintf("update coding set status = 0 where rowid=%i", rowid))
        } else {
          rqda_exe(sprintf("update coding set seltext='%s', selfirst=%i, selend=%i where rowid=%i",
                           enc(new_seltext, "UTF-8"), new_start, new_end, rowid))
        }
      }
    }

    gtkWidgetSetSensitive(save_btn, FALSE)
    message("File saved and codings updated.")
  })

  # Cursor/selection status
  gSignalConnectR(buffer, "mark-set", function(buf, iter, mark) {
    tryCatch({
      ins  <- gtkTextBufferGetInsert(buffer)
      it   <- gtkTextBufferGetIterAtMark(buffer, ins)
      line <- gtkTextIterGetLine(it) + 1L
      col  <- gtkTextIterGetLineOffset(it) + 1L
      gtkLabelSetText(pos_lbl, sprintf("Line %d, Col %d", line, col))

      # Selection info
      bs <- gtkTextBufferGetSelectionBound(buffer)
      sit <- gtkTextBufferGetIterAtMark(buffer, bs)
      if (!gtkTextIterEqual(it, sit)) {
        s1 <- min(gtkTextIterGetOffset(it), gtkTextIterGetOffset(sit))
        s2 <- max(gtkTextIterGetOffset(it), gtkTextIterGetOffset(sit))
        sel_text <- gtkTextBufferGetText(buffer,
                                         gtkTextBufferGetIterAtOffset(buffer, s1),
                                         gtkTextBufferGetIterAtOffset(buffer, s2), FALSE)
        if (is.list(sel_text)) sel_text <- sel_text[[1]]
        sel_words <- length(strsplit(trimws(as.character(sel_text)), "\\s+")[[1]])
        nch <- s2 - s1
        gtkLabelSetText(sel_lbl, sprintf("%d chars, %d words selected", nch, sel_words))
      } else {
        gtkLabelSetText(sel_lbl, "")
      }
    }, error=function(e){})
  })

  # Modified indicator + count update on text change
  gSignalConnectR(buffer, "changed", function(w) {
    gtkWidgetSetSensitive(save_btn, TRUE)
    gtkLabelSetMarkup(mod_lbl, "<span foreground='#C0392B'>● Unsaved</span>")
    update_counts()
  })

  # Cmd+S / Ctrl+S to save
  key_ctrl <- gtkEventControllerKeyNew()
  gSignalConnectR(key_ctrl, "key-pressed", function(ctrl, keyval, keycode, state) {
    mod <- bitwAnd(as.integer(state), 12L)
    ch  <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
    if (mod > 0L && (ch == "s" || ch == "S")) {
      if (gtkWidgetGetSensitive(save_btn))
        gSignalEmitByName(save_btn, "clicked")
      return(TRUE)
    }
    FALSE
  })
  gtkWidgetAddController(edit_win, key_ctrl)

  # Reset modified label after save
  gSignalConnectR(save_btn, "clicked", function(w) {
    gtkLabelSetMarkup(mod_lbl, "<span foreground='#888888'>Saved</span>")
  })

  # ── Autosave to temp file every 60s ───────────────────────────────────
  autosave_path <- file.path(tempdir(), sprintf("rqda_autosave_%d.txt", fid))
  autosave_timer <- gTimeoutAddSeconds(60L, function() {
    tryCatch({
      si  <- gtkTextBufferGetStartIter(buffer)
      ei  <- gtkTextBufferGetEndIter(buffer)
      raw <- gtkTextBufferGetText(buffer, si, ei, FALSE)
      if (is.list(raw)) raw <- raw[[1]]
      writeLines(as.character(raw), autosave_path, useBytes=TRUE)
    }, error=function(e){})
    TRUE  # keep firing
  })

  gSignalConnectR(edit_win, "close-request", function(w) {
    tryCatch(gSourceRemove(autosave_timer), error=function(e){})
    tryCatch(file.remove(autosave_path), error=function(e){})
    FALSE
  })

  gtkWindowPresent(edit_win)
}
