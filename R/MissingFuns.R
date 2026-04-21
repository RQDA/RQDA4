# missing_funs.R
# Wrappers and stubs for functions referenced in Menus.R / Handler.R

# ── Reusable card list renderer ──────────────────────────────────────────────
# Renders a list of cards in a GtkListBox with CSS-styled labels.
# Each card: header label (blue), meta label (gray), body label (normal text).
# Labels are selectable. Cards separated by CSS-styled separator rows.
.make_card_listbox <- function(win_widget, cards, on_activate = NULL) {
  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)

  lb <- gtkListBoxNew()
  gtkListBoxSetSelectionMode(lb, 1L)
  gtkListBoxSetActivateOnSingleClick(lb, FALSE)
  gtkScrolledWindowSetChild(sw, lb)

  dark <- isTRUE(tryCatch(.rqda$dark.mode, error=function(e) FALSE))
  hdr_color  <- if (dark) "#7ab3d9" else "#1a5fa8"
  meta_color <- if (dark) "#aaaaaa" else "#666666"

  row_data <- list()

  for (i in seq_along(cards)) {
    card <- cards[[i]]

    esc <- function(s) gsub("&", "&amp;", gsub("<", "&lt;", as.character(s)))

    # Separator line built into markup — no separate separator rows
    markup <- sprintf(
      "<span foreground=\'%s\'><b>%s</b></span>\n<span foreground=\'%s\'><small>%s</small></span>\n%s",
      hdr_color, esc(card$header),
      meta_color, esc(card$meta),
      esc(card$body))

    lbl <- gtkLabelNew("")
    gtkLabelSetMarkup(lbl, markup)
    gtkLabelSetXalign(lbl, 0.0)
    gtkLabelSetWrap(lbl, TRUE)
    gtkLabelSetWrapMode(lbl, 0L)
    gtkWidgetSetMarginTop(lbl, 8L); gtkWidgetSetMarginBottom(lbl, 8L)
    gtkWidgetSetMarginStart(lbl, 10L); gtkWidgetSetMarginEnd(lbl, 10L)
    gtkWidgetSetHexpand(lbl, TRUE)
    tryCatch(apply_font_css(lbl), error=function(e){})

    row <- gtkListBoxRowNew()
    gtkListBoxRowSetChild(row, lbl)
    gtkListBoxAppend(lb, row)
    row_data[[i]] <- card$data
  }

  # Native GTK separator between rows
  tryCatch(
    gtkListBoxSetHeaderFunc(lb, function(row, before) {
      if (is.null(before)) return()
      sep <- gtkSeparatorNew(0L)
      gtkListBoxRowSetHeader(row, sep)
    }),
    error = function(e) {}  # not bound in all Rgtk4 builds
  )

  ready <- FALSE  # block signal during construction

  gSignalConnectR(lb, "row-selected", function(box, row) {
    if (!ready || is.null(row)) return()
    tryCatch({
      lb_idx   <- as.integer(gtkListBoxRowGetIndex(row))
      card_idx <- lb_idx + 1L
      if (card_idx >= 1L && card_idx <= length(row_data) && !is.null(on_activate))
        on_activate(card_idx, row_data[[card_idx]])
    }, error=function(e){})
  })

  # Deselect and mark ready — subsequent user clicks will fire row-selected
  tryCatch(gtkListBoxUnselectAll(lb), error=function(e){})
  ready <- TRUE

  list(scrolled=sw, listbox=lb, row_data=row_data)
}




ViewFileFun <- function(FileNameWidget = .rqda$.fnames_rqda) {
  sel <- tryCatch(as.character(svalue(FileNameWidget)), error = function(e) character(0))
  sel <- sel[nzchar(sel)]
  if (length(sel) == 0) { gmessage("Select a file first.", icon = "error"); return(invisible(NULL)) }
  ViewFileFunHelper(sel[1])
}

AddFileToCaselinkage  <- function() AddFileToCase()
AddToFileCategory     <- function() AddFileToCategory()
AddToCodeCategory     <- function() AddCodeToCategory()
AddNewFileFun         <- function() NewFile()
CodeCatUpdate         <- function() UpdateTableWidget(".CodeCatWidget", "codecat")
FileCatUpdate         <- function() UpdateTableWidget(".FileCatWidget", "filecat")

FileNameWidgetUpdate <- function(FileNamesWidget = NULL, FileId = NULL, sortByTime = FALSE) {
  FileNamesUpdate(sortByTime = sortByTime)
}

CodeNamesWidgetUpdate <- function(CodeNamesWidget = NULL, CodeId = NULL, sortByTime = FALSE) {
  CodeNamesUpdate(sortByTime = sortByTime)
}

isExtant <- function(obj) {
  if (is.null(obj)) return(FALSE)
  tryCatch(!is.null(obj$widget), error = function(e) FALSE)
}

getFileIds <- function(condition = "unconditional", type = "all") {
  if (!is_projOpen(message = FALSE)) return(integer(0))

  fids <- switch(type,
                 selected = {
                   if (!exists(".fnames_rqda", envir = .rqda)) return(integer(0))
                   sel <- tryCatch(as.character(svalue(.rqda$.fnames_rqda)), error = function(e) character(0))
                   sel <- sel[nzchar(sel)]
                   if (length(sel) == 0) return(integer(0))
                   res <- rqda_sel(sprintf("SELECT id FROM source WHERE status=1 AND name IN (%s)",
                                           paste0("'", gsub("'","''",sel), "'", collapse=",")))
                   if (is.null(res) || nrow(res)==0) integer(0) else res$id
                 },
                 coded = {
                   res <- rqda_sel("SELECT DISTINCT fid FROM coding WHERE status=1")
                   if (is.null(res) || nrow(res)==0) integer(0) else res$fid
                 },
                 uncoded = {
                   all   <- rqda_sel("SELECT id FROM source WHERE status=1")$id
                   coded <- rqda_sel("SELECT DISTINCT fid FROM coding WHERE status=1")$fid
                   setdiff(all, coded)
                 },
                 {
                   res <- rqda_sel("SELECT id FROM source WHERE status=1")
                   if (is.null(res) || nrow(res)==0) integer(0) else res$id
                 }
  )

  if (condition == "unconditional") return(fids)

  if (condition == "case") {
    sel <- tryCatch(as.character(svalue(.rqda$.CasesNamesWidget)), error = function(e) character(0))
    sel <- sel[nzchar(sel)]
    if (length(sel) == 0) return(fids)
    cid <- rqda_sel(sprintf("SELECT id FROM cases WHERE name='%s' AND status=1", gsub("'","''",sel[1])))
    if (is.null(cid) || nrow(cid)==0) return(integer(0))
    linked <- rqda_sel(sprintf("SELECT DISTINCT fid FROM caselinkage WHERE caseid=%d AND status=1", cid$id[1]))
    if (is.null(linked) || nrow(linked)==0) return(integer(0))
    intersect(fids, linked$fid)
  } else if (condition == "filecategory") {
    sel <- tryCatch(as.character(svalue(.rqda$.FileCatWidget)), error = function(e) character(0))
    sel <- sel[nzchar(sel)]
    if (length(sel) == 0) return(fids)
    cid <- rqda_sel(sprintf("SELECT catid FROM filecat WHERE name='%s' AND status=1", gsub("'","''",sel[1])))
    if (is.null(cid) || nrow(cid)==0) return(integer(0))
    linked <- rqda_sel(sprintf("SELECT DISTINCT fid FROM treefile WHERE catid=%d AND status=1", cid$catid[1]))
    if (is.null(linked) || nrow(linked)==0) return(integer(0))
    intersect(fids, linked$fid)
  } else {
    fids
  }
}

retrieval <- function(Fid = NULL, CodeNameWidget = .rqda$.codes_rqda) {
  if (!is_projOpen()) return(invisible(NULL))
  sel <- tryCatch(as.character(svalue(CodeNameWidget)), error = function(e) character(0))
  sel <- sel[nzchar(sel)]
  if (length(sel) == 0) { message("Select a code first"); return(invisible(NULL)) }

  fid_clause <- if (!is.null(Fid) && length(Fid) > 0)
    sprintf("AND c.fid IN (%s)", paste(Fid, collapse=",")) else ""

  cids <- rqda_sel(sprintf(
    "SELECT id, name FROM freecode WHERE name IN (%s) AND status=1",
    paste0("'", gsub("'","''",sel), "'", collapse=",")))
  if (is.null(cids) || nrow(cids)==0) return(invisible(NULL))

  codings <- rqda_sel(sprintf(
    "SELECT c.rowid, c.cid, c.selfirst, c.selend, c.seltext,
            s.name AS filename, f.name AS codename
     FROM coding c
     LEFT JOIN source s ON c.fid=s.id
     LEFT JOIN freecode f ON c.cid=f.id
     WHERE c.cid IN (%s) AND c.status=1 %s
     ORDER BY f.name, s.name, c.selfirst",
    paste(cids$id, collapse=","), fid_clause))

  if (is.null(codings) || nrow(codings)==0) {
    gmessage(sprintf("No codings found for '%s'", paste(sel, collapse=", ")), icon="info")
    return(invisible(NULL))
  }

  print.codingsByOne(codings)
}

HL_AllCodings <- function() {
  # Toggle background highlight for the currently selected code(s)
  # in the open file viewer. Re-calling toggles it off.
  if (!exists(".openfile_fid", envir=.rqda) || !exists(".openfile_gui", envir=.rqda)) {
    message("Open a file first"); return(invisible(NULL))
  }

  sel <- tryCatch(as.character(svalue(.rqda$.codes_rqda)), error=function(e) character(0))
  sel <- sel[nzchar(sel)]
  if (length(sel) == 0) {
    message("Select a code first"); return(invisible(NULL))
  }

  if (!exists("highlighted_codes", envir=.rqda)) .rqda$highlighted_codes <- character(0)

  fid      <- .rqda$.openfile_fid
  textview <- .rqda$.openfile_gui$textview
  buffer   <- gtkTextViewGetBuffer(textview)
  tag_table <- gtkTextBufferGetTagTable(buffer)

  for (codename in sel) {
    tname <- sprintf("hlbg_%s", gsub("[^A-Za-z0-9]","_",codename))

    if (codename %in% .rqda$highlighted_codes) {
      # Toggle OFF - remove the highlight tag
      tag <- tryCatch(gtkTextTagTableLookup(tag_table, tname), error=function(e) NULL)
      if (!is.null(tag) && !is.character(tag)) {
        bounds <- gtkTextBufferGetBounds(buffer)
        gtkTextBufferRemoveTag(buffer, tag, bounds$start, bounds$end)
      }
      .rqda$highlighted_codes <- setdiff(.rqda$highlighted_codes, codename)
      message("Highlight OFF: ", codename)
    } else {
      # Toggle ON - apply pastel background for this code
      cid_res <- rqda_sel(sprintf(
        "SELECT id, color FROM freecode WHERE name='%s' AND status=1",
        gsub("'","''",codename)))
      if (is.null(cid_res) || nrow(cid_res)==0) next
      col <- cid_res$color[1]
      if (is.na(col) || col=="") col <- .rqda$fore.col %||% "blue"
      bg <- tryCatch({
        v <- col2rgb(col)/255
        sprintf("#%02X%02X%02X",
                as.integer((v[1]*0.3+0.7)*255),
                as.integer((v[2]*0.3+0.7)*255),
                as.integer((v[3]*0.3+0.7)*255))
      }, error=function(e) "#FFFFCC")

      tag <- tryCatch(gtkTextTagTableLookup(tag_table, tname), error=function(e) NULL)
      if (is.null(tag) || is.character(tag)) {
        tag <- gtkTextTagNew(tname)
        gObjectSetString(tag, "background", bg)
        gtkTextTagTableAdd(tag_table, tag)
      }

      codings <- rqda_sel(sprintf(
        "SELECT selfirst, selend FROM coding WHERE fid=%d AND cid=%d AND status=1",
        fid, cid_res$id[1]))
      if (!is.null(codings) && nrow(codings) > 0) {
        for (i in seq_len(nrow(codings))) {
          s_iter <- gtkTextBufferGetIterAtOffset(buffer, as.integer(codings$selfirst[i]))
          e_iter <- gtkTextBufferGetIterAtOffset(buffer, as.integer(codings$selend[i]))
          gtkTextBufferApplyTag(buffer, tag, s_iter, e_iter)
        }
      }
      .rqda$highlighted_codes <- union(.rqda$highlighted_codes, codename)
      message("Highlight ON: ", codename)
    }
  }
  invisible(NULL)
}

HL_CodingWithMemo <- function() {
  if (!exists(".openfile_fid", envir=.rqda) || !exists(".openfile_gui", envir=.rqda)) return(invisible(NULL))
  fid      <- .rqda$.openfile_fid
  textview <- .rqda$.openfile_gui$textview
  buffer   <- gtkTextViewGetBuffer(textview)
  # Only highlight codings that have a memo - additive, don't clear existing
  codings <- rqda_sel(sprintf(
    "SELECT c.rowid, c.cid, c.selfirst, c.selend, f.color, c.memo
     FROM coding c LEFT JOIN freecode f ON c.cid=f.id
     WHERE c.fid=%d AND c.status=1 AND c.memo IS NOT NULL AND c.memo != ''", fid))
  if (is.null(codings) || nrow(codings)==0) {
    gmessage("No codings with memos in this file.", icon="info")
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(codings))) {
    col <- codings$color[i]
    if (is.na(col) || col=="") col <- .rqda$codeMark.col %||% "blue"
    markRange(textview=textview, from=codings$selfirst[i], to=codings$selend[i],
              rowid=codings$rowid[i], fore.col=col, back.col="lightyellow")
  }
}

# LoadCodings variant that adds a background highlight color
LoadCodingsHighlighted <- function(fid, codingTable = "coding") {
  if (!is_projOpen()) return(invisible(NULL))
  if (!exists(".openfile_gui", envir=.rqda)) return(invisible(NULL))
  codings <- rqda_sel(sprintf(
    "SELECT c.rowid, c.cid, c.selfirst, c.selend, f.color
     FROM %s c LEFT JOIN freecode f ON c.cid=f.id
     WHERE c.fid=%d AND c.status=1", codingTable, fid))
  if (is.null(codings) || nrow(codings)==0) return(invisible(NULL))
  textview <- .rqda$.openfile_gui$textview
  for (i in seq_len(nrow(codings))) {
    col <- codings$color[i]
    if (is.na(col) || col=="") col <- .rqda$codeMark.col %||% "blue"
    # Lighten the code color for background
    bg <- tryCatch({
      rgb_vals <- col2rgb(col) / 255
      # Mix with white: 30% color, 70% white
      r <- rgb_vals[1]*0.3 + 0.7; g <- rgb_vals[2]*0.3 + 0.7; b <- rgb_vals[3]*0.3 + 0.7
      sprintf("#%02X%02X%02X", as.integer(r*255), as.integer(g*255), as.integer(b*255))
    }, error = function(e) "lightyellow")
    markRange(textview=textview, from=codings$selfirst[i], to=codings$selend[i],
              rowid=codings$rowid[i], fore.col=col, back.col=bg)
  }
  invisible(NULL)
}


getCodingsOfCodes <- function(fid = NULL) {
  if (!is_projOpen(message = FALSE)) return(data.frame())
  sel <- tryCatch(as.character(svalue(.rqda$.codes_rqda)), error = function(e) character(0))
  sel <- sel[nzchar(sel)]
  if (length(sel) == 0) return(data.frame())
  cids <- rqda_sel(sprintf(
    "SELECT id FROM freecode WHERE name IN (%s) AND status=1",
    paste0("'", gsub("'","''",sel), "'", collapse=",")))
  if (is.null(cids) || nrow(cids)==0) return(data.frame())
  fid_clause <- if (!is.null(fid) && length(fid)>0)
    sprintf(" AND c.fid IN (%s)", paste(fid, collapse=",")) else ""
  rqda_sel(sprintf(
    "SELECT c.rowid, c.seltext, c.selfirst, c.selend,
            s.name AS filename, f.name AS codename
     FROM coding c
     LEFT JOIN source s ON c.fid=s.id
     LEFT JOIN freecode f ON c.cid=f.id
     WHERE c.cid IN (%s) AND c.status=1%s
     ORDER BY s.name, c.selfirst",
    paste(cids$id, collapse=","), fid_clause))
}

.zoom_card_listbox <- function(lb, size) {
  row <- gtkListBoxGetRowAtIndex(lb, 0L)
  idx <- 0L
  while (!is.null(row)) {
    child <- gtkListBoxRowGetChild(row)
    if (!is.null(child))
      tryCatch(apply_font_css(child, size=size), error=function(e){})
    idx <- idx + 1L
    row <- gtkListBoxGetRowAtIndex(lb, idx)
  }
}

.embed_recode_editor <- function(coding_row, outer, do_recode) {
  # Hide current children
  child  <- gtkWidgetGetFirstChild(outer)
  hidden <- list()
  while (!is.null(child)) {
    nxt <- gtkWidgetGetNextSibling(child)
    gtkWidgetSetVisible(child, FALSE)
    hidden <- c(hidden, list(child))
    child  <- nxt
  }

  restore <- function() {
    tryCatch(gtkBoxRemove(outer, editor_box), error=function(e){})
    for (w in hidden) gtkWidgetSetVisible(w, TRUE)
  }

  all_codes <- rqda_sel("SELECT name FROM freecode WHERE status=1 ORDER BY name")
  if (is.null(all_codes)||nrow(all_codes)==0) { restore(); return() }
  codes_vec <- all_codes$name

  # Fetch categories for current code to suggest candidates
  cur_code  <- as.character(coding_row$codename)
  cats <- tryCatch(rqda_sel(sprintf(
    "SELECT codecat.name AS cat FROM treecode
     JOIN freecode ON treecode.cid=freecode.id
     JOIN codecat  ON treecode.catid=codecat.catid
     WHERE freecode.name='%s' AND treecode.status=1",
    gsub("'","''",cur_code)))$cat, error=function(e) character(0))

  # Codes in same categories as current code = candidates
  candidates <- if (length(cats)>0) {
    tryCatch(rqda_sel(sprintf(
      "SELECT DISTINCT freecode.name FROM treecode
       JOIN freecode ON treecode.cid=freecode.id
       JOIN codecat  ON treecode.catid=codecat.catid
       WHERE codecat.name IN (%s) AND treecode.status=1
         AND freecode.name != '%s'
       ORDER BY freecode.name",
      paste(shQuote(cats), collapse=","), gsub("'","''",cur_code)))$name,
      error=function(e) character(0))
  } else character(0)

  editor_box <- gtkBoxNew(1L, 8L)
  gtkWidgetSetMarginTop(editor_box, 10L); gtkWidgetSetMarginBottom(editor_box, 10L)
  gtkWidgetSetMarginStart(editor_box, 10L); gtkWidgetSetMarginEnd(editor_box, 10L)
  gtkWidgetSetVexpand(editor_box, TRUE)

  # Header
  hdr <- gtkLabelNew("")
  gtkLabelSetMarkup(hdr, sprintf(
    "<b>Recode</b>: %s
<small>%s</small>",
    gsub("&","&amp;",gsub("<","&lt;",cur_code)),
    gsub("&","&amp;",gsub("<","&lt;",
                          paste0("“", substr(as.character(coding_row$seltext),1,120), "”")))))
  gtkLabelSetXalign(hdr, 0.0)
  gtkLabelSetWrap(hdr, TRUE)
  gtkBoxAppend(editor_box, hdr)

  # Search entry
  search_box <- gtkBoxNew(0L, 4L)
  search_lbl <- gtkLabelNew("Search:")
  search_entry <- gtkEntryNew()
  gtkWidgetSetHexpand(search_entry, TRUE)
  gtkBoxAppend(search_box, search_lbl)
  gtkBoxAppend(search_box, search_entry)
  gtkBoxAppend(editor_box, search_box)

  # Code listbox
  sw <- gtkScrolledWindowNew(); gtkWidgetSetVexpand(sw, TRUE)
  lb <- gtkListBoxNew()
  gtkListBoxSetSelectionMode(lb, 1L)
  gtkListBoxSetActivateOnSingleClick(lb, FALSE)
  gtkScrolledWindowSetChild(sw, lb)
  gtkBoxAppend(editor_box, sw)

  # Populate: candidates first (with separator), then all
  selected_code <- new.env(parent=emptyenv()); selected_code$name <- NULL
  all_items     <- unique(c(if(length(candidates)>0) candidates else character(0), codes_vec))

  populate_list <- function(filter = "") {
    # Clear
    repeat {
      r <- gtkListBoxGetRowAtIndex(lb, 0L)
      if (is.null(r)) break
      gtkListBoxRemove(lb, r)
    }
    shown <- if (nzchar(filter))
      all_items[grepl(filter, all_items, ignore.case=TRUE)]
    else all_items

    # Show candidates highlighted at top
    cands_shown <- intersect(candidates, shown)
    rest_shown  <- setdiff(shown, candidates)

    add_row <- function(name, is_candidate) {
      markup <- if (is_candidate)
        sprintf("<b>%s</b> <small>(same category)</small>",
                gsub("&","&amp;",gsub("<","&lt;",name)))
      else
        gsub("&","&amp;",gsub("<","&lt;",name))
      lbl <- gtkLabelNew("")
      gtkLabelSetMarkup(lbl, markup)
      gtkLabelSetXalign(lbl, 0.0)
      gtkWidgetSetMarginTop(lbl, 4L); gtkWidgetSetMarginBottom(lbl, 4L)
      gtkWidgetSetMarginStart(lbl, 8L)
      row <- gtkListBoxRowNew()
      gtkListBoxRowSetChild(row, lbl)
      gtkListBoxAppend(lb, row)
    }

    for (n in cands_shown) add_row(n, TRUE)
    if (length(cands_shown)>0 && length(rest_shown)>0) {
      sep_row <- gtkListBoxRowNew()
      gtkListBoxRowSetSelectable(sep_row, FALSE)
      gtkListBoxRowSetActivatable(sep_row, FALSE)
      sep <- gtkSeparatorNew(0L)
      gtkListBoxRowSetChild(sep_row, sep)
      gtkListBoxAppend(lb, sep_row)
    }
    for (n in rest_shown) add_row(n, FALSE)
  }

  all_names_flat <- unique(c(candidates, codes_vec))
  ready_recode   <- FALSE
  populate_list()
  ready_recode   <- TRUE

  gSignalConnectR(lb, "row-selected", function(box, row) {
    if (!ready_recode || is.null(row)) return()
    tryCatch({
      lbl  <- gtkListBoxRowGetChild(row)
      if (is.null(lbl)) return()
      # Extract plain text from label (strip markup)
      raw  <- gtkLabelGetText(lbl)
      if (!is.null(raw) && nzchar(raw)) selected_code$name <- raw
    }, error=function(e){})
  })

  # Search filter
  gSignalConnectR(gtkEntryGetBuffer(search_entry), "inserted-text", function(buf, pos, text, len) {
    tryCatch({ ready_recode <<- FALSE; populate_list(gtkEntryBufferGetText(buf)); ready_recode <<- TRUE }, error=function(e){})
  })
  gSignalConnectR(gtkEntryGetBuffer(search_entry), "deleted-text", function(buf, pos, n) {
    tryCatch({ ready_recode <<- FALSE; populate_list(gtkEntryBufferGetText(buf)); ready_recode <<- TRUE }, error=function(e){})
  })

  # Buttons
  bbox       <- gtkBoxNew(0L, 6L); gtkWidgetSetHalign(bbox, 3L)
  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  recode_btn_ok <- gtkButtonNewWithLabel("Recode")
  gtkBoxAppend(bbox, cancel_btn); gtkBoxAppend(bbox, recode_btn_ok)
  gtkBoxAppend(editor_box, bbox)
  gtkBoxAppend(outer, editor_box)

  gSignalConnectR(cancel_btn,   "clicked", function(w) restore())
  gSignalConnectR(recode_btn_ok,"clicked", function(w) {
    nm <- selected_code$name
    if (is.null(nm)||!nzchar(nm)) { message("Select a code first."); return() }
    restore()
    do_recode(nm)
  })

  gtkWidgetGrabFocus(search_entry)
  invisible(NULL)
}


#' @exportS3Method
print.codingsByOne <- function(ct, ...) {
  if (is.null(ct) || nrow(ct)==0) {
    gmessage("No codings found", icon="info"); return(invisible(NULL))
  }

  win <- gtkWindowNew()
  gtkWindowSetTitle(win, sprintf("Codings (%d)", nrow(ct)))
  gtkWindowSetDefaultSize(win, 720L, 560L)
  # Not transient - transient non-modal windows go behind parent on macOS

  # Cmd+W close
  wkey <- gtkEventControllerKeyNew()
  gSignalConnectR(wkey, "key-pressed", function(ctrl, keyval, keycode, state) {
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
    if (ch %in% c("w","W")) { gtkWindowClose(win); return(TRUE) }
    FALSE
  })
  gtkWidgetAddController(win, wkey)

  outer <- gtkBoxNew(1L, 0L)
  gtkWindowSetChild(win, outer)

  # Toolbar
  toolbar <- gtkBoxNew(0L, 6L)
  gtkWidgetSetMarginTop(toolbar, 6L); gtkWidgetSetMarginBottom(toolbar, 6L)
  gtkWidgetSetMarginStart(toolbar, 8L)
  gtkBoxAppend(outer, toolbar)
  unmark_btn <- gtkButtonNewWithLabel("\u2298 Unmark selected")
  recode_btn <- gtkButtonNewWithLabel("\u21c4 Recode selected")
  gtkBoxAppend(toolbar, unmark_btn)
  gtkBoxAppend(toolbar, recode_btn)
  gtkBoxAppend(outer, gtkSeparatorNew(0L))

  # Build cards
  cards <- lapply(seq_len(nrow(ct)), function(i) {
    fname <- as.character(ct$filename[i]); Encoding(fname) <- "UTF-8"
    cname <- as.character(ct$codename[i]); Encoding(cname) <- "UTF-8"
    sel   <- as.character(ct$seltext[i]);  Encoding(sel)   <- "UTF-8"
    list(
      header = cname,
      meta   = sprintf("%s  \u00b7  pos %g\u2013%g", fname, ct$selfirst[i], ct$selend[i]),
      body   = sel,
      data   = i
    )
  })

  selected_idx <- new.env(parent=emptyenv()); selected_idx$i <- 0L

  jump_to <- function(card_idx, data_idx) {
    selected_idx$i <- data_idx
    row      <- ct[data_idx, ]
    fname    <- as.character(row$filename)
    selfirst <- as.integer(row$selfirst)
    selend   <- as.integer(row$selend)

    # Check if this file is already open
    fid_res <- rqda_sel(sprintf("SELECT id FROM source WHERE name='%s' AND status=1",
                                gsub("'","''",fname)))
    if (is.null(fid_res) || nrow(fid_res)==0) return()
    fid_target <- fid_res$id[1]

    already_open <- tryCatch(
      isTRUE(.rqda$.openfile_fid == fid_target), error=function(e) FALSE)

    if (!already_open) {
      ViewFileFunHelper(fname, highlight=TRUE)
      # Poll until viewer is ready
      for (k in seq_len(80L)) {
        gMainContextIteration(NULL, FALSE)
        if (tryCatch(isTRUE(.rqda$.openfile_fid==fid_target) &&
                     !is.null(.rqda$.openfile_gui$textview),
                     error=function(e) FALSE)) break
      }
    }

    tryCatch({
      tv2  <- .rqda$.openfile_gui$textview
      buf2 <- gtkTextViewGetBuffer(tv2)
      s_it <- gtkTextBufferGetIterAtOffset(buf2, selfirst)
      e_it <- gtkTextBufferGetIterAtOffset(buf2, selend)
      # Scroll to position — no extra highlight tag, codings are already marked
      gtkTextViewScrollToIter(tv2, s_it, 0.1, TRUE, 0.0, 0.3)
      gtkTextBufferPlaceCursor(buf2, s_it)
    }, error=function(e) message("Jump error: ", e$message))
  }

  result <- .make_card_listbox(win, cards,
                               on_activate = function(card_idx, data_idx) {
                                 selected_idx$i <- data_idx
                               })

  # row-activated fires on double-click (ActivateOnSingleClick=TRUE means Enter key,
  # double-click still emits row-activated)
  gSignalConnectR(result$listbox, "row-activated", function(box, row) {
    if (selected_idx$i >= 1L) jump_to(selected_idx$i, selected_idx$i)
  })
  gtkBoxAppend(outer, result$scrolled)

  gSignalConnectR(unmark_btn, "clicked", function(w) {
    i <- selected_idx$i
    if (i < 1L || i > nrow(ct)) return()
    rqda_exe(sprintf("UPDATE coding SET status=0 WHERE rowid=%d", ct$rowid[i]))
    message("Unmarked: ", ct$codename[i])
    ct <<- ct[-i, ]
    if (nrow(ct)==0) { gtkWindowClose(win); return() }
    # Rebuild - simplest approach
    gtkWindowClose(win)
    print.codingsByOne(ct)
  })

  gSignalConnectR(recode_btn, "clicked", function(w) {
    i <- selected_idx$i
    if (i < 1L || i > nrow(ct)) { message("Select a coding first."); return() }
    .embed_recode_editor(ct[i,], outer, do_recode = function(new_code_name) {
      cid <- rqda_sel(sprintf("SELECT id FROM freecode WHERE name='%s' AND status=1",
                              gsub("'","''", new_code_name)))
      if (is.null(cid)||nrow(cid)==0) return()
      rqda_exe(sprintf("UPDATE coding SET cid=%d WHERE rowid=%d", cid$id[1], ct$rowid[i]))
      ct$codename[i] <<- new_code_name
      # Rebuild cards with updated data
      gtkWindowClose(win)
      print.codingsByOne(ct)
    })
  })

  # Zoom: Cmd+/Ctrl+ and Cmd-/Ctrl-
  zoom_delta <- new.env(parent=emptyenv()); zoom_delta$n <- 0L
  zoom_key <- gtkEventControllerKeyNew()
  gSignalConnectR(zoom_key, "key-pressed", function(ctrl, keyval, keycode, state) {
    # Check Cmd (macOS) or Ctrl (Win/Linux) modifier
    # state: 4 = Ctrl, 8 = Cmd (Super) on macOS via GTK
    mod <- bitwAnd(as.integer(state), 12L)  # 4|8
    if (mod == 0L) return(FALSE)
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
    sz <- if (ch %in% c("+","=")) {
      zoom_delta$n <- zoom_delta$n + 1L
      max(6L, (.rqda$font.size %||% 11L) + zoom_delta$n)
    } else if (ch == "-") {
      zoom_delta$n <- zoom_delta$n - 1L
      max(6L, (.rqda$font.size %||% 11L) + zoom_delta$n)
    } else if (ch == "0") {
      zoom_delta$n <- 0L
      .rqda$font.size %||% 11L
    } else return(FALSE)
    .zoom_card_listbox(result$listbox, sz)
    return(TRUE)
    FALSE
  })
  gtkWidgetAddController(win, zoom_key)

  gtkWindowPresent(win)
  invisible(win)
}
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

plotCodeCategory <- function(parent = NULL) {
  if (is.null(parent))
    parent <- tryCatch(as.character(svalue(.rqda$.CodeCatWidget)), error = function(e) character(0))
  if (length(parent) == 0 || !nzchar(parent[1])) {
    gmessage("Select a code category first.", icon = "info")
    return(invisible(NULL))
  }
  ans <- rqda_sel(sprintf(
    "SELECT codecat.name AS parent, freecode.name AS child
     FROM treecode, codecat, freecode
     WHERE treecode.status=1 AND codecat.status=1 AND freecode.status=1
       AND treecode.catid=codecat.catid AND freecode.id=treecode.cid
       AND codecat.name IN (%s)",
    paste(shQuote(parent), collapse=",")))
  if (is.null(ans) || nrow(ans) == 0) {
    gmessage("No codes in this category.", icon = "info")
    return(invisible(NULL))
  }
  Encoding(ans$parent) <- "UTF-8"
  Encoding(ans$child)  <- "UTF-8"
  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph.data.frame(ans)
    igraph::plot.igraph(g, vertex.label = igraph::V(g)$name,
                        vertex.color = "lightblue", vertex.size = 30,
                        edge.arrow.size = 0.5, main = paste(parent, collapse=", "))
  } else {
    message("igraph not installed - install.packages('igraph')")
  }
}

# Alias used in Menus.R
plotCodeCat <- plotCodeCategory

d3CodeCategory <- function(parent = NULL) {
  if (is.null(parent))
    parent <- tryCatch(as.character(svalue(.rqda$.CodeCatWidget)), error = function(e) character(0))
  if (length(parent) == 0 || !nzchar(parent[1])) {
    gmessage("Select a code category first.", icon = "info")
    return(invisible(NULL))
  }
  ans <- rqda_sel(sprintf(
    "SELECT codecat.name AS parent, freecode.name AS child
     FROM treecode, codecat, freecode
     WHERE treecode.status=1 AND codecat.status=1 AND freecode.status=1
       AND treecode.catid=codecat.catid AND freecode.id=treecode.cid
       AND codecat.name IN (%s)",
    paste(shQuote(parent), collapse=",")))
  if (is.null(ans) || nrow(ans) == 0) {
    gmessage("No codes in this category.", icon = "info")
    return(invisible(NULL))
  }
  Encoding(ans$parent) <- "UTF-8"
  Encoding(ans$child)  <- "UTF-8"
  if (requireNamespace("d3Network", quietly = TRUE)) {
    file <- paste(tempfile(), "html", sep = ".")
    d3Network::d3SimpleNetwork(ans, width = 1200, height = 800,
                               file = file(file, encoding = "UTF-8"))
    browseURL(file)
  } else {
    message("d3Network not installed - install.packages('d3Network')")
  }
}

# exportCodings - exports all codings to HTML (original RQDA function)
exportCodings <- function(file, Fid = NULL) {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))

  fid_clause <- if (!is.null(Fid) && length(Fid) > 0)
    sprintf(" AND c.fid IN (%s)", paste(Fid, collapse = ",")) else ""

  codings <- rqda_sel(sprintf(
    "SELECT f.name AS codename, s.name AS filename,
            c.seltext, c.selfirst, c.selend
     FROM coding c
     JOIN freecode f ON c.cid = f.id
     JOIN source   s ON c.fid = s.id
     WHERE c.status=1%s
     ORDER BY f.name, s.name, c.selfirst",
    fid_clause))

  if (is.null(codings) || nrow(codings) == 0) {
    gmessage("No codings found.", icon = "info")
    return(invisible(NULL))
  }

  # Build HTML
  rows <- tapply(seq_len(nrow(codings)), codings$codename, function(idx) {
    code <- codings$codename[idx[1]]
    items <- paste(sapply(idx, function(i)
      sprintf("<li><b>%s</b> [%g-%g]<br><pre>%s</pre></li>",
              htmltools_escape(codings$filename[i]),
              codings$selfirst[i], codings$selend[i],
              htmltools_escape(codings$seltext[i]))), collapse = "\n")
    sprintf("<h2>%s</h2><ul>%s</ul>", htmltools_escape(code), items)
  }, simplify = FALSE)

  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
    "<title>RQDA Codings Export</title>",
    "<style>body{font-family:sans-serif;max-width:900px;margin:auto;padding:2em}",
    "pre{background:#f4f4f4;padding:1em;white-space:pre-wrap}</style></head><body>",
    "<h1>RQDA Codings Export</h1>",
    paste(unlist(rows), collapse = "\n"),
    "</body></html>")

  writeLines(html, con = file, useBytes = FALSE)
  message("Exported ", nrow(codings), " codings to ", file)
  browseURL(file)
  invisible(file)
}

htmltools_escape <- function(x) {
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

# CrossCases - case profiling / cross-tabulation
CrossCases <- function(.type = "profile") {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))

  cases <- rqda_sel("SELECT id, name FROM cases WHERE status=1 ORDER BY name")
  if (is.null(cases) || nrow(cases) == 0) {
    gmessage("No cases found.", icon = "info"); return(invisible(NULL))
  }

  attrs <- rqda_sel("SELECT name FROM attributes WHERE status=1 ORDER BY name")

  # Build cross-tabulation: cases x attributes + files + codings
  build_table <- function() {
    rows <- lapply(seq_len(nrow(cases)), function(i) {
      cid  <- cases$id[i]
      cname <- cases$name[i]

      files <- rqda_sel(sprintf(
        "SELECT count(*) as n FROM caselinkage WHERE caseid=%d AND status=1", cid))
      codings <- rqda_sel(sprintf(
        "SELECT count(*) as n FROM coding c JOIN caselinkage cl ON c.fid=cl.fid
         WHERE cl.caseid=%d AND cl.status=1 AND c.status=1", cid))

      row <- c(Case=cname,
               Files=as.character(files$n[1]),
               Codings=as.character(codings$n[1]))

      if (!is.null(attrs) && nrow(attrs) > 0) {
        attr_vals <- rqda_sel(sprintf(
          "SELECT variable, value FROM caseAttr WHERE caseID=%d AND status=1", cid))
        for (a in attrs$name) {
          v <- if (!is.null(attr_vals) && a %in% attr_vals$variable)
            attr_vals$value[attr_vals$variable == a] else ""
          row <- c(row, setNames(as.character(v[1]), a))
        }
      }
      row
    })
    do.call(rbind, rows)
  }

  tbl <- build_table()

  # Display in a GTK window as a grid
  win <- gtkWindowNew()
  gtkWindowSetTitle(win, "Case Profiles")
  gtkWindowSetDefaultSize(win, 700L, 400L)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(win, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 4L)
  gtkWindowSetChild(win, vbox)

  # Toolbar
  tbar <- gtkBoxNew(0L, 6L)
  gtkWidgetSetMarginTop(tbar, 4L); gtkWidgetSetMarginBottom(tbar, 4L)
  gtkWidgetSetMarginStart(tbar, 8L); gtkWidgetSetMarginEnd(tbar, 8L)
  export_btn <- gtkButtonNewWithLabel("Export CSV")
  gtkBoxAppend(tbar, export_btn)
  gtkBoxAppend(vbox, tbar)

  # Scrolled text view showing the table
  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)
  gtkBoxAppend(vbox, sw)

  tv <- gtkTextViewNew()
  gtkTextViewSetEditable(tv, FALSE)
  gtkTextViewSetMonospace(tv, TRUE)
  tryCatch(apply_font_css(tv), error=function(e){})
  gtkScrolledWindowSetChild(sw, tv)
  buf <- gtkTextViewGetBuffer(tv)

  render <- function(t) {
    cols <- colnames(t)
    widths <- sapply(cols, function(c) max(nchar(c), max(nchar(t[,c]))) + 2L)
    header <- paste(mapply(formatC, cols, widths, MoreArgs=list(flag="-")), collapse=" | ")
    sep    <- paste(sapply(widths, function(w) strrep("-", w)), collapse="-+-")
    rows   <- apply(t, 1, function(r)
      paste(mapply(formatC, r, widths, MoreArgs=list(flag="-")), collapse=" | "))
    paste(c(header, sep, rows), collapse="
")
  }

  gtkTextBufferSetText(buf, render(tbl), -1L)

  gSignalConnectR(export_btn, "clicked", function(w) {
    path <- gfile(type="save", text="Save case profile as CSV")
    if (length(path) > 0 && nzchar(path)) {
      if (!grepl("\\.csv$", path, ignore.case=TRUE)) path <- paste0(path, ".csv")
      write.csv(as.data.frame(tbl), path, row.names=FALSE)
      message("Exported: ", path)
    }
  })

  gtkWindowPresent(win)
  invisible(tbl)
}

# System font size detection
get_system_font_size <- function() {
  tryCatch({
    settings <- gtkSettingsGetDefault()
    font_str  <- gObjectGetProperty(settings, "gtk-font-name")
    if (!is.null(font_str) && nzchar(font_str)) {
      parts <- strsplit(trimws(font_str), "\\s+")[[1]]
      sz <- suppressWarnings(as.integer(tail(parts, 1)))
      if (!is.na(sz) && sz > 0) return(sz)
    }
  }, error = function(e) {})
  11L
}

get_system_font_family <- function() {
  tryCatch({
    settings <- gtkSettingsGetDefault()
    font_str  <- gObjectGetProperty(settings, "gtk-font-name")
    if (!is.null(font_str) && nzchar(font_str)) {
      parts <- strsplit(trimws(font_str), "\\s+")[[1]]
      # Family is everything except the last token (size)
      if (length(parts) > 1) return(paste(head(parts, -1), collapse=" "))
    }
  }, error = function(e) {})
  "Sans"
}


CodeWithCoding <- function(condition = "unconditional") {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))
  cids <- rqda_sel("SELECT DISTINCT cid FROM coding WHERE status=1")
  if (is.null(cids) || nrow(cids) == 0) return(invisible(NULL))
  codes <- rqda_sel(sprintf(
    "SELECT name FROM freecode WHERE status=1 AND id IN (%s) ORDER BY name",
    paste(cids$cid, collapse=",")))
  if (is.null(codes) || nrow(codes) == 0) return(invisible(NULL))
  Encoding(codes$name) <- "UTF-8"
  .rqda$.codes_rqda[] <- codes$name
  invisible(NULL)
}

CodeWithoutCoding <- function(condition = "unconditional") {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))
  codes <- rqda_sel(
    "SELECT name FROM freecode WHERE status=1
     AND id NOT IN (SELECT DISTINCT cid FROM coding WHERE status=1) ORDER BY name")
  if (is.null(codes) || nrow(codes) == 0) return(invisible(NULL))
  Encoding(codes$name) <- "UTF-8"
  .rqda$.codes_rqda[] <- codes$name
  invisible(NULL)
}

plotFileCat <- function(catname) {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))
  catname <- as.character(catname)[1]
  if (is.null(catname) || is.na(catname) || !nzchar(catname)) {
    gmessage("Select a file category first.", icon = "info")
    return(invisible(NULL))
  }
  ans <- rqda_sel(sprintf(
    "SELECT filecat.name AS parent, source.name AS child
     FROM treefile, filecat, source
     WHERE treefile.status=1 AND filecat.status=1 AND source.status=1
       AND treefile.catid=filecat.catid AND source.id=treefile.fid
       AND filecat.name IN (%s)",
    paste(shQuote(catname), collapse=",")))
  if (is.null(ans) || nrow(ans) == 0) {
    gmessage("No files in this category.", icon = "info")
    return(invisible(NULL))
  }
  Encoding(ans$parent) <- "UTF-8"
  Encoding(ans$child)  <- "UTF-8"
  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph.data.frame(ans)
    igraph::plot.igraph(g, vertex.label = igraph::V(g)$name,
                        vertex.color = "lightsalmon", vertex.size = 30,
                        edge.arrow.size = 0.5, main = catname)
  } else {
    message("igraph not installed - install.packages('igraph')")
  }
}

searchCases <- function(pattern, Widget = ".CasesNamesWidget") {
  if (!is_projOpen(message = FALSE)) return(invisible(NULL))
  if (is.null(pattern) || length(pattern) == 0 || is.na(pattern)) return(invisible(NULL))
  Encoding(pattern) <- "UTF-8"
  results <- rqda_sel(sprintf(
    "SELECT name FROM cases WHERE status=1 AND %s ORDER BY name",
    gsub("%%", "%", pattern)))
  if (is.null(results) || nrow(results) == 0) {
    gmessage("No cases found matching pattern.", icon = "info")
    return(invisible(NULL))
  }
  Encoding(results$name) <- "UTF-8"
  tryCatch(.rqda[[Widget]][] <- results$name, error = function(e) {})
  invisible(results)
}

# Remove only coding tags (fg_/bg_ prefixed by rowid) from buffer
# Leaves search_highlight and hl_ (user highlight) tags intact
.remove_coding_tags <- function(buffer) {
  tag_table <- gtkTextBufferGetTagTable(buffer)
  bounds    <- gtkTextBufferGetBounds(buffer)
  n_tags    <- tryCatch(gtkTextTagTableGetSize(tag_table), error = function(e) 0L)
  if (n_tags == 0L) return(invisible(NULL))
  # Collect tags to remove (can't modify table while iterating)
  to_remove <- character(0)
  tryCatch(
    gtkTextTagTableForeach(tag_table, function(tag) {
      # Tag name pattern: fg_COLOR_ROWID or bg_COLOR_ROWID
      nm <- tryCatch(gObjectGetProperty(tag, "name"), error = function(e) "")
      if (!is.null(nm) && nzchar(nm) &&
          (startsWith(nm, "fg_") || startsWith(nm, "bg_")) &&
          !startsWith(nm, "hl_")) {
        to_remove <<- c(to_remove, nm)
        tryCatch(gtkTextBufferRemoveTag(buffer, tag, bounds$start, bounds$end),
                 error = function(e) {})
      }
    }),
    error = function(e) {
      # Fallback: RemoveAllTags if foreach not available
      gtkTextBufferRemoveAllTags(buffer, bounds$start, bounds$end)
    }
  )
  invisible(NULL)
}

reload_codings_for_file <- function(fid, textview) {
  buffer <- gtkTextViewGetBuffer(textview)
  bounds <- gtkTextBufferGetBounds(buffer)
  gtkTextBufferRemoveAllTags(buffer, bounds$start, bounds$end)
  # Clear highlight tracking since we wiped all tags
  if (exists("highlighted_codes", envir=.rqda)) .rqda$highlighted_codes <- character(0)
  LoadCodings(fid)
}

# ── MERGE CODES ─────────────────────────────────────────────────────────────
mergeCodes <- function(cid1, cid2) {
  if (!is_projOpen(message=FALSE)) return(invisible(NULL))
  # Move all codings from cid2 to cid1
  rqda_exe(sprintf("UPDATE coding SET cid=%d WHERE cid=%d AND status=1", cid1, cid2))
  # Move treecode memberships
  rqda_exe(sprintf("UPDATE treecode SET cid=%d WHERE cid=%d AND status=1", cid1, cid2))
  # Soft-delete cid2
  rqda_exe(sprintf("UPDATE freecode SET status=0 WHERE id=%d", cid2))
  message("Codes merged.")
  CodeNamesUpdate(sortByTime=FALSE)
  invisible(NULL)
}

# ── ATTRIBUTES UI ────────────────────────────────────────────────────────────
viewFileAttr <- function() {
  if (!is_projOpen(message=FALSE)) return(invisible(NULL))
  sel <- tryCatch(as.character(svalue(.rqda$.fnames_rqda)), error=function(e) character(0))
  sel <- sel[nzchar(sel)]
  if (length(sel)==0) { gmessage("Select a file first.", icon="error"); return(invisible(NULL)) }
  .show_attr_editor("file", sel[1])
}

viewCaseAttr <- function() {
  if (!is_projOpen(message=FALSE)) return(invisible(NULL))
  sel <- tryCatch(as.character(svalue(.rqda$.CasesNamesWidget)), error=function(e) character(0))
  sel <- sel[nzchar(sel)]
  if (length(sel)==0) { gmessage("Select a case first.", icon="error"); return(invisible(NULL)) }
  .show_attr_editor("case", sel[1])
}

CaseAttrFun <- viewCaseAttr

.show_attr_editor <- function(type, name) {
  # Get all defined attributes
  attrs <- rqda_sel("SELECT name FROM attributes WHERE status=1 ORDER BY name")
  if (is.null(attrs) || nrow(attrs)==0) {
    gmessage("No attributes defined. Add attributes first.", icon="info")
    return(invisible(NULL))
  }

  # Get current values for this file/case
  if (type == "file") {
    fid <- rqda_sel(sprintf("SELECT id FROM source WHERE name='%s' AND status=1", enc(name)))
    if (is.null(fid) || nrow(fid)==0) return(invisible(NULL))
    vals <- rqda_sel(sprintf("SELECT variable, value FROM fileAttr WHERE fileID=%d AND status=1", fid$id[1]))
  } else {
    cid <- rqda_sel(sprintf("SELECT id FROM cases WHERE name='%s' AND status=1", enc(name)))
    if (is.null(cid) || nrow(cid)==0) return(invisible(NULL))
    vals <- rqda_sel(sprintf("SELECT variable, value FROM caseAttr WHERE caseID=%d AND status=1", cid$id[1]))
  }

  cur_vals <- if (!is.null(vals) && nrow(vals)>0) setNames(vals$value, vals$variable) else character(0)

  win <- gtkWindowNew()
  gtkWindowSetTitle(win, sprintf("Attributes: %s", name))
  gtkWindowSetDefaultSize(win, 450L, 400L)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(win, .rqda$.rqda_window)

  outer <- gtkBoxNew(1L, 6L)
  gtkWidgetSetMarginTop(outer, 8L); gtkWidgetSetMarginBottom(outer, 8L)
  gtkWidgetSetMarginStart(outer, 12L); gtkWidgetSetMarginEnd(outer, 12L)
  gtkWindowSetChild(win, outer)

  grid <- gtkGridNew()
  gtkGridSetRowSpacing(grid, 6L)
  gtkGridSetColumnSpacing(grid, 12L)
  gtkBoxAppend(outer, grid)

  entries <- list()
  for (i in seq_len(nrow(attrs))) {
    attr_name <- attrs$name[i]
    lbl <- gtkLabelNew(paste0(attr_name, ":"))
    gtkLabelSetXalign(lbl, 0.0)
    gtkGridAttach(grid, lbl, 0L, as.integer(i-1), 1L, 1L)

    entry <- gtkEntryNew()
    cur <- cur_vals[attr_name]
    if (!is.null(cur) && !is.na(cur)) gtkEntryBufferSetText(gtkEntryGetBuffer(entry), cur, -1L)
    gtkWidgetSetHexpand(entry, TRUE)
    gtkGridAttach(grid, entry, 1L, as.integer(i-1), 1L, 1L)
    entries[[attr_name]] <- entry
  }

  save_btn <- gtkButtonNewWithLabel("Save")
  gtkWidgetSetHalign(save_btn, 3L)
  gtkBoxAppend(outer, save_btn)

  gSignalConnectR(save_btn, "clicked", function(w) {
    for (attr_name in names(entries)) {
      val <- gtkEntryBufferGetText(gtkEntryGetBuffer(entries[[attr_name]]))
      if (type == "file") {
        SetFileAttribute(name, attr_name, val)
      } else {
        SetCaseAttribute(name, attr_name, val)
      }
    }
    message("Attributes saved for: ", name)
    gtkWindowClose(win)
  })

  gtkWindowPresent(win)
}



getCodingsFromFiles <- function(Fid = NULL) {
  if (!is_projOpen(message=FALSE)) return(data.frame())
  fid_clause <- if (!is.null(Fid) && length(Fid)>0)
    sprintf("AND c.fid IN (%s)", paste(Fid, collapse=",")) else ""
  codings <- rqda_sel(sprintf(
    "SELECT c.rowid, c.cid, c.selfirst, c.selend, c.seltext,
            s.name AS filename, f.name AS codename
     FROM coding c
     LEFT JOIN source s ON c.fid=s.id
     LEFT JOIN freecode f ON c.cid=f.id
     WHERE c.status=1 %s ORDER BY s.name, c.selfirst", fid_clause))
  if (!is.null(codings) && nrow(codings)>0) {
    Encoding(codings$seltext) <- "UTF-8"
    Encoding(codings$filename) <- "UTF-8"
    Encoding(codings$codename) <- "UTF-8"
  }
  codings
}
