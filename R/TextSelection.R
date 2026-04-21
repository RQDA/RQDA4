# Text selection and coding functions for RQDA - GTK4 implementation

#' Get selection information from text widget
#' @param textview GtkTextView widget
#' @param includeAnchor Whether to include anchor positions
#' @param codingTable Which coding table to use
#' @return List with selection details
sindex <- function(textview = NULL, includeAnchor = TRUE, codingTable = "coding") {

  # Get the textview from .rqda if not provided
  if (is.null(textview)) {
    if (!exists(".openfile_gui", envir = .rqda)) {
      stop("No file is currently open")
    }
    textview <- .rqda$.openfile_gui$textview
  }

  # Get buffer
  buffer <- gtkTextViewGetBuffer(textview)

  # Get selection bounds
  bounds <- gtkTextBufferGetSelectionBounds(buffer)

  if (is.null(bounds) || !bounds$result) { # not sure if this can be NULL
    return(list(
      startI = NULL,
      endI = NULL,
      startN = 0,
      endN = 0,
      startMark = NULL,
      endMark = NULL,
      seltext = ""
    ))
  }

  startI <- bounds$start
  endI <- bounds$end

  # Get selected text
  selected <- gtkTextBufferGetText(buffer, startI, endI, FALSE)
  Encoding(selected) <- "UTF-8"

  # Create marks for the selection
  startMark <- gtkTextBufferCreateMark(buffer, NULL, startI, TRUE)
  endMark <- gtkTextBufferCreateMark(buffer, NULL, endI, FALSE)

  # Get character offsets
  startN <- gtkTextIterGetOffset(startI)
  endN <- gtkTextIterGetOffset(endI)

  # TODO: Adjust for anchors if needed
  # if (!includeAnchor) {
  #   startN <- startN - countAnchors(...)
  #   endN <- endN - countAnchors(...)
  # }

  return(list(
    startI = startI,
    endI = endI,
    startN = startN,
    endN = endN,
    startMark = startMark,
    endMark = endMark,
    seltext = selected
  ))
}

#' Mark selected text with a code
#' @param textview GtkTextView widget
#' @param fore.col Foreground color
#' @param back.col Background color
#' @param codingTable Which coding table to use
#' @return List with start, end, and text
mark <- function(textview = NULL, fore.col = .rqda$fore.col, back.col = .rqda$back.col,
                 codingTable = "coding") {

  # Get the textview
  if (is.null(textview)) {
    if (!exists(".openfile_gui", envir = .rqda)) {
      stop("No file is currently open")
    }
    textview <- .rqda$.openfile_gui$textview
  }

  # Get selection information
  index <- sindex(textview, includeAnchor = TRUE, codingTable = codingTable)

  if (index$seltext == "") {
    message("No text selected")
    return(NULL)
  }

  buffer <- gtkTextViewGetBuffer(textview)

  # Get iterators from marks
  startIter <- gtkTextBufferGetIterAtMark(buffer, index$startMark)
  endIter <- gtkTextBufferGetIterAtMark(buffer, index$endMark)

  # Apply highlighting
  # Create or get tag for foreground color
  if (!is.null(fore.col)) {
    tag_name <- sprintf("fg_%s", fore.col)

    # Check if tag exists
    tag_table <- gtkTextBufferGetTagTable(buffer)
    tag <- gtkTextTagTableLookup(tag_table, tag_name)

    if (is.null(tag)) {
      tag <- gtkTextTagNew(tag_name)
      gObjectSetString(tag, "foreground", fore.col)
      gtkTextTagTableAdd(tag_table, tag)
    }

    gtkTextBufferApplyTag(buffer, tag, startIter, endIter)
  }

  # Apply background color
  if (!is.null(back.col)) {
    tag_name <- sprintf("bg_%s", back.col)

    tag_table <- gtkTextBufferGetTagTable(buffer)
    tag <- gtkTextTagTableLookup(tag_table, tag_name)

    if (is.null(tag)) {
      tag <- gtkTextTagNew(tag_name)
      gObjectSetString(tag, "background", back.col)
      gtkTextTagTableAdd(tag_table, tag)
    }

    gtkTextBufferApplyTag(buffer, tag, startIter, endIter)
  }

  return(list(
    start = index$startN,
    end = index$endN,
    text = index$seltext
  ))
}

#' Mark a specific range (used for loading existing codings)
#' @param textview GtkTextView widget
#' @param from Start position
#' @param to End position
#' @param rowid Database rowid for this coding
#' @param fore.col Foreground color
#' @param back.col Background color
markRange <- function(textview = NULL, from, to, rowid,
                      fore.col = .rqda$fore.col, back.col = .rqda$back.col,
                      codingTable = "coding") {

  if (from == to) return(invisible(NULL))

  if (is.null(textview)) {
    if (!exists(".openfile_gui", envir = .rqda)) return(invisible(NULL))
    textview <- .rqda$.openfile_gui$textview
  }

  buffer <- gtkTextViewGetBuffer(textview)

  tryCatch({
    # FIX 1: Use GetIterAtOffset directly.
    # Ensure 'from' and 'to' are passed as integers.
    startIter <- gtkTextBufferGetIterAtOffset(buffer, as.integer(from))
    endIter <- gtkTextBufferGetIterAtOffset(buffer, as.integer(to))

    # Safety check: if the bridge returned NULL, stop here
    if (is.null(startIter) || is.null(endIter)) {
      message("Error: Could not create iterators for range ", from, " to ", to)
      return(invisible(NULL))
    }

    # Create marks
    start_mark_name <- sprintf("coding_%s_start", rowid)
    end_mark_name <- sprintf("coding_%s_end", rowid)
    gtkTextBufferCreateMark(buffer, start_mark_name, startIter, TRUE)
    gtkTextBufferCreateMark(buffer, end_mark_name, endIter, FALSE)

    # Use the proven pattern from mark() and HL()
    apply_color_tag <- function(color, type = "foreground") {
      if (is.null(color)) return()

      clean_col <- gsub("#", "", color)
      tag_name <- sprintf("%s_%s", ifelse(type == "foreground", "fg", "bg"), clean_col)

      tag_table <- gtkTextBufferGetTagTable(buffer)
      tag <- gtkTextTagTableLookup(tag_table, tag_name)

      if (is.null(tag)) {
        tag <- gtkTextTagNew(tag_name)
        gObjectSetString(tag, type, color)
        gtkTextTagTableAdd(tag_table, tag)
      }

      gtkTextBufferApplyTag(buffer, tag, startIter, endIter)
    }

    apply_color_tag(fore.col, "foreground")
    apply_color_tag(back.col, "background")

  }, error = function(e) {
    message("Error in markRange: ", e$message)
  })

  invisible(NULL)
}

#' Clear marks/highlights from a range
#' @param textview GtkTextView widget
#' @param min Start position
#' @param max End position
#' @param clear.fore.col Clear foreground color
#' @param clear.back.col Clear background color
ClearMark <- function(textview = NULL, min = 0, max,
                      clear.fore.col = TRUE, clear.back.col = TRUE) {

  if (is.null(textview)) {
    if (!exists(".openfile_gui", envir = .rqda)) {
      return(invisible(NULL))
    }
    textview <- .rqda$.openfile_gui$textview
  }

  buffer <- gtkTextViewGetBuffer(textview)

  startI <- gtkTextBufferGetIterAtOffset(buffer, as.integer(min))
  endI <- gtkTextBufferGetIterAtOffset(buffer, as.integer(max))

  # Remove all tags in the range
  if (clear.fore.col) {
    # Get all tags that start with "fg_"
    tag_table <- gtkTextBufferGetTagTable(buffer)
    # Note: GTK4 doesn't have an easy way to list all tags
    # We'll remove common ones
    for (col in c(.rqda$fore.col, "blue", "red", "green", "purple", "orange")) {
      tag_name <- sprintf("fg_%s", col)
      tag <- gtkTextTagTableLookup(tag_table, tag_name)
      if (!is.null(tag)) {
        gtkTextBufferRemoveTag(buffer, tag, startI, endI)
      }
    }
  }

  if (clear.back.col) {
    for (col in c(.rqda$back.col, "yellow", "lightblue", "lightgreen", "pink")) {
      tag_name <- sprintf("bg_%s", col)
      tag_table <- gtkTextBufferGetTagTable(buffer)
      tag <- gtkTextTagTableLookup(tag_table, tag_name)
      if (!is.null(tag)) {
        gtkTextBufferRemoveTag(buffer, tag, startI, endI)
      }
    }
  }

  invisible(NULL)
}

#' Highlight a range with specific colors
#' @param textview GtkTextView widget
#' @param index Data frame with columns: start position, end position
#' @param fore.col Foreground color
#' @param back.col Background color
HL <- function(textview = NULL, index, fore.col = .rqda$fore.col, back.col = NULL) {

  if (is.null(textview)) {
    if (!exists(".openfile_gui", envir = .rqda)) {
      return(invisible(NULL))
    }
    textview <- .rqda$.openfile_gui$textview
  }

  buffer <- gtkTextViewGetBuffer(textview)

  # Apply highlighting to each range in index
  apply(index, 1, function(x) {
    start <- gtkTextBufferGetIterAtOffset(buffer, as.integer(x[1]))
    end <- gtkTextBufferGetIterAtOffset(buffer, as.integer(x[2]))

    if (!is.null(fore.col)) {
      tag_name <- sprintf("fg_%s", fore.col)
      tag_table <- gtkTextBufferGetTagTable(buffer)
      tag <- gtkTextTagTableLookup(tag_table, tag_name)

      if (is.null(tag)) {
        tag <- gtkTextTagNew(tag_name)
        gObjectSetString(tag, "foreground", fore.col)
        gtkTextTagTableAdd(tag_table, tag)
      }

      gtkTextBufferApplyTag(buffer, tag, start, end)
    }

    if (!is.null(back.col)) {
      tag_name <- sprintf("bg_%s", back.col)
      tag_table <- gtkTextBufferGetTagTable(buffer)
      tag <- gtkTextTagTableLookup(tag_table, tag_name)

      if (is.null(tag)) {
        tag <- gtkTextTagNew(tag_name)
        gObjectSetString(tag, "background", back.col)
        gtkTextTagTableAdd(tag_table, tag)
      }

      gtkTextBufferApplyTag(buffer, tag, start, end)
    }
  })

  invisible(NULL)
}

#' Load and display existing codings for the current file
#' @param fid File ID
#' @param codingTable Which coding table
LoadCodings <- function(fid, codingTable = "coding") {

  if (!is_projOpen()) return(invisible(NULL))

  if (!exists(".openfile_gui", envir = .rqda)) {
    return(invisible(NULL))
  }

  # Get all codings for this file
  codings <- rqda_sel(sprintf(
    paste(
      "SELECT c.rowid, c.cid, c.selfirst, c.selend, f.color ",
      "FROM %s c ",
      "LEFT JOIN freecode f ON c.cid = f.id ",
      "WHERE c.fid = %d AND c.status = 1"
    ),
    codingTable, fid
  ))

  if (is.null(codings) || nrow(codings) == 0) {
    return(invisible(NULL))
  }

  textview <- .rqda$.openfile_gui$textview

  # Apply highlights for each coding
  for (i in 1:nrow(codings)) {
    color <- codings$color[i]
    if (is.na(color) || color == "") {
      color <- .rqda$codeMark.col
    }

    markRange(
      textview = textview,
      from = codings$selfirst[i],
      to = codings$selend[i],
      rowid = codings$rowid[i],
      fore.col = color,
      back.col = NULL,
      codingTable = codingTable
    )
  }

  invisible(NULL)
}
