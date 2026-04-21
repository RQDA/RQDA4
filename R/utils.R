DefaultCodeColor <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")

rename <- function(from, to, table = c("source", "freecode", "cases", "codecat", "filecat", "journal")) {
  ## rename name field in table source and freecode (other tables can be added futher)
  ## source is the file name, freecode is the free code name
  table <- match.arg(table)
  if (to != "") { ## if to is "", makes no sense to rename
    exists <- rqda_sel(sprintf("select * from %s where name = '%s' ", table, enc(to)))
    ## should check it there is any dupliation in the table
    if (nrow(exists) > 0) {
      gmessage(gettext("The new name is duplicated. Please use another new name.", domain = "R-RQDA"), container = TRUE)
    } else {
      rqda_exe(sprintf("update '%s' set name = '%s' where name = '%s' ", table, enc(to), enc(from)))
    }
  }
}

UpdateWidget <- function(widget, from, to = NULL) {
  ## widget is character of length 1.
  items <- eval(parse(text = sprintf(".rqda$%s[]", widget)))

  if (length(items) != 0) {
    Encoding(items) <- "UTF-8"

    ## note the position, before manipulation of items
    idx <- as.character(which(items %in%  from[1]))

    if (is.null(to)) {
      items <- items[! items %in% from]
    } else {
      if (length(from) == length(to))
        items[items %in% from] <- to
    }


    ## eval(parse(text = sprintf(".rqda$%s[] <- items", widget)))
    tryCatch(eval(parse(text = sprintf(".rqda$%s[] <- items", widget))),
             error = function(e) cat("warning msg from the replacement.\n"))


    # was length > 0 which will throw an error
    if (length(idx) > 1) {
      path <- gtkTreePathNewFromString(idx)

      # GTK4: gtkTreeViewScrollToCell signature may be different
      # Try with just path parameter
      tryCatch({
        gtkTreeViewScrollToCell(get(widget, envir = .rqda)$widget, path)
      }, error = function(e) {
        # If that fails, silently ignore - scrolling is not critical
      })
    }}
}

ScrollToItem <- function(widget, item = svalue(widget)) {
  items <- widget[]
  if (length(items) !=  0) {
    Encoding(items) <- "UTF-8"
    idx <- as.character(which(items %in% item) - 1)
    if (length(idx) != 0) {
      path <- gtkTreePathNewFromString(idx)
      # GTK4: Try scrolling without alignment parameters
      tryCatch({
        gtkTreeViewScrollToCell(widget$widget, path)
      }, error = function(e) {
        # Silently ignore if scrolling fails
      })
    }}}

enc <- function(x, encoding = "UTF-8") {
  ## replace " with two '. to make insert smoothly.
  ## encoding is the encoding of x (character vector).
  Encoding(x) <- encoding
  x <- gsub("'", "''", x)
  if (all(Encoding(x) != "UTF-8")) {
    x <- iconv(x, to = "UTF-8")
  }
  x
}

OrderByTime <- function(date, decreasing = FALSE) {
  ## return tbe permutation of the date which is get by sql "select date from ..."
  ## see order for the meaning of permutation. It can be used as index to sort vector or date frame
  ##   if (getRversion()<"2.8.0") {
  ##     permutation <- ifelse(decreasing, 1:length(date), length(date):1)
  ##     ## should rewrite it when project merge is provided.
  ##   } else{
  ## Work for R.2.8.0 or above for Dateclass, so convert to character
  oldLCTIME <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", oldLCTIME))
  Newdate <- as.character(strptime(date, "%a %b %d %H:%M:%S %Y"))
  permutation <- order(Newdate, decreasing = decreasing)
  ##  }
}
## dd <- rqda_sel("select date from source")$date
## sort(dd) == dd[order(dd)] ## but the order is not correct.
## dd[OrderByTime(dd)]

save_memo <- function(W = W, dbTable = dbTable, Selected = Selected, prefix = prefix) {
  # Get text from gtext widget via buffer
  newcontent <- tryCatch({
    if (inherits(W, "gtext")) {
      buf <- gtkTextViewGetBuffer(W$textview)
      si  <- gtkTextBufferGetStartIter(buf)
      ei  <- gtkTextBufferGetEndIter(buf)
      raw <- gtkTextBufferGetText(buf, si, ei, FALSE)
      if (is.list(raw)) raw <- raw[[1]]
      as.character(raw)
    } else {
      v <- svalue(W)
      if (is.null(v) || length(v) == 0) "" else as.character(v[1])
    }
  }, error = function(e) "")
  newcontent <- enc(newcontent, encoding = "UTF-8")
  rqda_exe(sprintf("update %s set memo='%s' where name='%s'",
                   dbTable, newcontent, enc(as.character(Selected[1]))))
  tryCatch({
    mbut <- get(sprintf("buttonOf.%smemo", prefix), envir = button)
    enabled(mbut) <- FALSE
  }, error = function(e) {})
}

get_buffer_text <- function(buffer) {
  si  <- gtkTextBufferGetStartIter(buffer)
  ei  <- gtkTextBufferGetEndIter(buffer)
  raw <- gtkTextBufferGetText(buffer, si, ei, FALSE)
  if (is.list(raw)) raw <- raw[[1]]
  txt <- as.character(raw)
  if (length(txt) == 0) txt <- ""
  txt
}

MemoWidget <- function(prefix, widget, dbTable) {
  if (!is_projOpen(envir = .rqda, "qdacon")) return(invisible(NULL))

  # Try tracked selection first (more reliable than svalue for multi-select widgets)
  Selected <- tryCatch({
    key <- switch(dbTable,
                  cases      = .rqda$selected_case,
                  freecode   = .rqda$selected_code %||% .rqda$selected_codeof_cat,
                  codecat    = .rqda$selected_codecat,
                  filecat    = .rqda$selected_filecat,
                  attributes = .rqda$selected_attr,
                  journal    = .rqda$selected_journal,
                  NULL)
    if (!is.null(key) && nzchar(key)) key else as.character(svalue(widget))[1]
  }, error = function(e) character(0))

  if (length(Selected) == 0 || is.null(Selected) || is.na(Selected) || !nzchar(Selected)) {
    gmessage(gettext("Select first.", domain = "R-RQDA"), icon = "error", container = TRUE)
    return(invisible(NULL))
  }
  Selected <- as.character(Selected)[1]
  Encoding(Selected) <- "UTF-8"

  prvcontent <- rqda_sel(
    sprintf("select memo from %s where name='%s'", dbTable, enc(Selected)))[1, 1]
  if (is.null(prvcontent) || is.na(prvcontent)) prvcontent <- ""
  Encoding(prvcontent) <- "UTF-8"

  gw <- gwindow(
    title   = sprintf(gettext("%s Memo: %s", domain = "R-RQDA"), prefix, Selected),
    width   = getOption("widgetSize")[1],
    height  = getOption("widgetSize")[2]
  )

  vbox <- ggroup(horizontal = FALSE, container = gw)
  gtkWidgetSetVexpand(vbox$widget, TRUE)

  txt <- gtext(prvcontent, container = vbox)
  gtkWidgetSetVexpand(txt$widget, TRUE)
  apply_font_css(txt$textview)

  buffer <- gtkTextViewGetBuffer(txt$textview)

  do_save <- function() {
    content <- enc(get_buffer_text(buffer), "UTF-8")
    sel_enc <- enc(Selected)
    rqda_exe(sprintf("update %s set memo='%s' where name='%s'",
                     dbTable, content, sel_enc))
    enabled(mbut) <- FALSE
  }

  mbut <- gbutton(rqda_txt("Save Memo"), container = vbox,
                  handler = function(h, ...) do_save())
  enabled(mbut) <- FALSE

  gSignalConnectR(buffer, "changed", function(b) {
    enabled(mbut) <- TRUE
  })

  # Zoom: +/- keys
  memo_zoom <- new.env(parent=emptyenv()); memo_zoom$delta <- 0L
  memo_key  <- gtkEventControllerKeyNew()
  gSignalConnectR(memo_key, "key-pressed", function(ctrl, keyval, keycode, state) {
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
    if (ch %in% c("+","=")) {
      memo_zoom$delta <- memo_zoom$delta + 1L
      apply_font_css(txt$textview, size=max(6L, (.rqda$font.size %||% 11L) + memo_zoom$delta))
      return(TRUE)
    } else if (ch == "-") {
      memo_zoom$delta <- memo_zoom$delta - 1L
      apply_font_css(txt$textview, size=max(6L, (.rqda$font.size %||% 11L) + memo_zoom$delta))
      return(TRUE)
    } else if (ch == "0") {
      memo_zoom$delta <- 0L; apply_font_css(txt$textview); return(TRUE)
    }
    FALSE
  })
  gtkWidgetAddController(txt$textview, memo_key)
  # Also add to window so zoom works regardless of focus
  memo_key2 <- gtkEventControllerKeyNew()
  gSignalConnectR(memo_key2, "key-pressed", function(ctrl, keyval, keycode, state) {
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
    if (ch %in% c("+","=")) {
      memo_zoom$delta <- memo_zoom$delta + 1L
      apply_font_css(txt$textview, size=max(6L, (.rqda$font.size %||% 11L) + memo_zoom$delta))
      return(TRUE)
    } else if (ch == "-") {
      memo_zoom$delta <- memo_zoom$delta - 1L
      apply_font_css(txt$textview, size=max(6L, (.rqda$font.size %||% 11L) + memo_zoom$delta))
      return(TRUE)
    } else if (ch == "0") {
      memo_zoom$delta <- 0L; apply_font_css(txt$textview); return(TRUE)
    }
    FALSE
  })
  gtkWidgetAddController(gw$widget, memo_key2)

  gSignalConnectR(gw$widget, "close-request", function(w) {
    current <- get_buffer_text(buffer)
    saved   <- rqda_sel(
      sprintf("select memo from %s where name='%s'", dbTable, enc(Selected)))[1, 1]
    if (is.null(saved) || is.na(saved)) saved <- ""
    if (!identical(current, saved)) {
      if (gconfirm(gettext("The memo has been changed. Save before closing?",
                           domain = "R-RQDA"))) do_save()
    }
    FALSE
  })

  invisible(gw)
}

getAnnos <- function(type = "file") {
  annos <- rqda_sel("select annotation.rowid, source.id, source.name, annotation.annotation, annotation.date, annotation.position from annotation join source on annotation.fid = source.id where  annotation.status == 1 and annotation not in ('NA', '')")
  if (nrow(annos) > 0) {
    Encoding(annos$annotation) <- "UTF-8"
    Encoding(annos$name) <- "UTF-8"
  }
  attr(annos, "field.name") <- "annotation"
  attr(annos, "descr") <- sprintf("%i %s", nrow(annos), ngettext(nrow(annos), "annotation", "annotations", domain = "R-RQDA"))
  class(annos) <- c("annotations", "Info4Widget", "data.frame")
  annos
}

#' @export
getMemos <- function(type = "codes") {
  memos <- rqda_sel("select memo, name, id, date, dateM from freecode where status == 1 and memo not in ('NA', '')")
  if (nrow(memos) > 0) {
    Encoding(memos$memo) <- "UTF-8"
    Encoding(memos$name) <- "UTF-8"
  }
  class(memos) <- c("memos", "Info4Widget", "data.frame")
  attr(memos, "field.name") <- "memo"
  attr(memos, "descr") <- sprintf("%i code %s", nrow(memos), ngettext(nrow(memos), "memo", "memos", domain = "R-RQDA"))
  memos
}

#' @method print Info4Widget
#' @export
print.Info4Widget <- function(x, ...) {
  if (nrow(x)==0) {
    gmessage(gettext("No Information is collected.", domain="R-RQDA"), container=TRUE)
    return(invisible(NULL))
  }

  field.name    <- attr(x, "field.name")
  is_annotation <- inherits(x, "annotations") || field.name == "annotation"
  title         <- attr(x, "descr")

  win <- gtkWindowNew()
  gtkWindowSetTitle(win, title)
  gtkWindowSetDefaultSize(win, 700L, 520L)

  wkey <- gtkEventControllerKeyNew()
  gSignalConnectR(wkey, "key-pressed", function(ctrl, keyval, keycode, state) {
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error=function(e) "")
    if (ch %in% c("w","W")) { gtkWindowClose(win); return(TRUE) }
    FALSE
  })
  gtkWidgetAddController(win, wkey)

  outer <- gtkBoxNew(1L, 0L)
  gtkWindowSetChild(win, outer)

  # Toolbar (annotations only)
  selected_idx <- new.env(parent=emptyenv()); selected_idx$i <- 1L

  if (is_annotation) {
    toolbar <- gtkBoxNew(0L, 6L)
    gtkWidgetSetMarginTop(toolbar, 6L); gtkWidgetSetMarginBottom(toolbar, 6L)
    gtkWidgetSetMarginStart(toolbar, 8L)
    gtkBoxAppend(outer, toolbar)
    goto_btn   <- gtkButtonNewWithLabel("\u2197 Go to")
    edit_btn   <- gtkButtonNewWithLabel("\u270e Edit")
    delete_btn <- gtkButtonNewWithLabel("\u00d7 Delete")
    for (b in list(goto_btn, edit_btn, delete_btn)) gtkBoxAppend(toolbar, b)
    gtkBoxAppend(outer, gtkSeparatorNew(0L))

    do_jump <- function(i) {
      if (i<1||i>nrow(x)) return()
      row <- x[i,]
      fname <- as.character(row[["name"]])
      pos   <- tryCatch(as.integer(row[["position"]]), error=function(e) NA)
      if (is.na(pos)) return()
      ViewFileFunHelper(fname, highlight=FALSE)
      tryCatch({
        fid_t <- rqda_sel(sprintf("SELECT id FROM source WHERE name='%s' AND status=1",
                                  gsub("'","''",fname)))$id[1]
        for (k in 1:50) {
          gMainContextIteration(NULL, FALSE)
          if (tryCatch(isTRUE(.rqda$.openfile_fid==fid_t),error=function(e)FALSE)) break
        }
        tv2  <- .rqda$.openfile_gui$textview
        buf2 <- gtkTextViewGetBuffer(tv2)
        s_it <- gtkTextBufferGetIterAtOffset(buf2, pos)
        tt   <- gtkTextBufferGetTagTable(buf2)
        jtag <- gtkTextTagTableLookup(tt, "jump_highlight")
        if (is.null(jtag)||is.character(jtag)) {
          jtag <- gtkTextTagNew("jump_highlight")
          gObjectSetString(jtag, "background", "#FFD700")
          gtkTextTagTableAdd(tt, jtag)
        }
        e_it <- gtkTextBufferGetIterAtOffset(buf2, pos+1L)
        gtkTextBufferApplyTag(buf2, jtag, s_it, e_it)
        gtkTextViewScrollToIter(tv2, s_it, 0.1, TRUE, 0.0, 0.3)
      }, error=function(e) message("Jump: ", e$message))
    }

    gSignalConnectR(goto_btn,   "clicked", function(w) do_jump(selected_idx$i))
    gSignalConnectR(delete_btn, "clicked", function(w) {
      i <- selected_idx$i
      if (i<1||i>nrow(x)) return()
      if (!gconfirm("Delete this annotation?")) return()
      rqda_exe(sprintf("UPDATE annotation SET status=0 WHERE rowid=%d", as.integer(x$rowid[i])))
      x <<- x[-i,]
      if (nrow(x)==0) { gtkWindowClose(win); return() }
      selected_idx$i <<- min(i, nrow(x))
      gtkWindowClose(win); print.Info4Widget(x)
    })
    gSignalConnectR(edit_btn, "clicked", function(w) {
      i <- selected_idx$i
      if (i<1||i>nrow(x)) return()
      row   <- x[i,]
      rid   <- as.integer(row[["rowid"]])
      body  <- as.character(row[[field.name]])
      pos   <- tryCatch(as.integer(row[["position"]]), error=function(e) 0L)
      fname <- as.character(row[["name"]])
      fid_res <- rqda_sel(sprintf("SELECT id FROM source WHERE name='%s' AND status=1",
                                  gsub("'","''",fname)))
      fid <- if (!is.null(fid_res)&&nrow(fid_res)>0) fid_res$id[1] else NA
      tryCatch(editAnnotation(rid, body, pos, fid, parent_win=win, parent_box=outer),
               error=function(e) message(e$message))
    })
  }

  # Build cards
  cards <- lapply(seq_len(nrow(x)), function(i) {
    row    <- x[i,]
    header <- as.character(row[["name"]]); Encoding(header) <- "UTF-8"
    body   <- as.character(row[[field.name]]); Encoding(body) <- "UTF-8"
    meta   <- if (is_annotation && "position" %in% colnames(x))
      sprintf("pos %s  \u00b7  %s", row[["position"]], row[["date"]])
    else
      as.character(row[["date"]])
    list(header=header, meta=meta, body=body, data=i)
  })

  on_act <- function(card_idx, data_idx) {
    selected_idx$i <- data_idx
  }

  result <- .make_card_listbox(win, cards, on_activate=on_act)

  if (is_annotation) {
    gSignalConnectR(result$listbox, "row-activated", function(box, row) {
      if (selected_idx$i >= 1L) do_jump(selected_idx$i)
    })
  }
  gtkBoxAppend(outer, result$scrolled)



  # Zoom: Cmd+/Ctrl+ and Cmd-/Ctrl-
  zoom_delta <- new.env(parent=emptyenv()); zoom_delta$n <- 0L
  zoom_key <- gtkEventControllerKeyNew()
  gSignalConnectR(zoom_key, "key-pressed", function(ctrl, keyval, keycode, state) {
    mod <- bitwAnd(as.integer(state), 12L)
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


## summary coding information
#' @export
getCodingTable <- function() {
  ## test when any table is empty
  ## http://archives.postgresql.org/pgsql-sql/2004-01/msg00160.php
  if (is_projOpen()) {
    ## Codings <- rqda_sel("select freecode.name as codename, freecode.id as cid,
    ##         coding.cid as cid2, coding.fid as fid, source.id as fid2, source.name as filename,
    ##         coding.selend - coding.selfirst as CodingLength, coding.selend, coding.selfirst
    ##         from coding, freecode, source
    ##         where coding.status == 1 and freecode.id = coding.cid and coding.fid = source.id")
    Codings <- rqda_sel("select coding.rowid as rowid, coding.cid, coding.fid, freecode.name as codename, source.name as filename,
                                       coding.selfirst as index1, coding.selend as index2,
                                       coding.selend - coding.selfirst as CodingLength
                                      from coding left join freecode on (coding.cid = freecode.id)
                                                  left join source on (coding.fid = source.id)
                                      where coding.status == 1 and source.status = 1 and freecode.status = 1")

    if (nrow(Codings) != 0) {
      Encoding(Codings$codename) <- Encoding(Codings$filename) <- "UTF-8"
    }
    ## if (!all (all.equal(Codings$cid, Codings$cid2), all.equal(Codings$fid, Codings$fid2))) {
    ##   stop("Errors!", domain = "R-RQDA") ## check to make sure the sql is correct
    ## }
    class(Codings) <- c("codingTable", "data.frame")
    Codings
  } else cat("Open a project first.\n")
}

#' @export
summaryCodings <- function(byFile = FALSE, ...) {
  if (is_projOpen()) {
    Codings <- getCodingTable()
    if (nrow(Codings) > 0) {
      NumOfCoding <- table(Codings$codename, ...) ## how many coding for each code
      AvgLength <- tapply(Codings$CodingLength, Codings$codename, FUN = mean, ...) # Average of words for each code
      NumOfFile <- tapply(Codings$fid, Codings$codename, FUN = function(ii)length(unique(ii))) # Number of files for each code
      if (byFile) {
        CodingOfFile <- tapply(Codings$codename, Codings$filename, FUN = table, ...) # summary of codings for each file
      } else CodingOfFile <- NULL
      ans <- list(NumOfCoding = NumOfCoding, AvgLength = AvgLength, NumOfFile = NumOfFile, CodingOfFile = CodingOfFile)
      class(ans) <- "summaryCodings"
      ans
    } else {
      cat("No coding.\n")
    }
  } else {
    cat("Open a project first.\n")
  }
}

#' @method print summaryCodings
#' @export
print.summaryCodings <- function(x, ...) {
  class(x)
  if (!is.null(x$CodingOfFile)) {
    cat("----------------\n")
    cat("Number of codings for each file.\n")
    print(x$CodingOfFile)
  }
  cat("----------------\n")
  cat("Number of codings for each code.\n")
  print(x$NumOfCoding)
  cat("----------------\n")
  cat("Average number of characters associated with each code.\n\n")
  print(x$AvgLength)
  cat("----------------\n")
  cat("Number of files associated with each code.\n\n")
  print(x$NumOfFile)
}

#' Search files
#'
#' Search files according to the pattern.
#'
#' @usage
#' searchFiles(pattern, content = FALSE, Fid = NULL, Widget = NULL, is.UTF8 = FALSE)
#'
#' @param pattern The criterion of search, see examples section for examples.
#' @param content When it is TRUE, the content of files fitting the pattern
#' will be returned as well.
#' @param Fid integer vector, the ids of subset of files to search.
#' @param Widget Character, name of a gtable widget. If it is not NULL,
#' the file names fitting the pattern will pushed to that gtable widget
#' using \code{svalue} method. One useful value is ".fnames_rqda", so
#' the file names will be pushed to the Files Tab of RQDA. Others are
#' ".FileofCat" and ".FileofCase".
#' @param is.UTF8 If the coding of pattern is UTF-8. If you are not sure,
#' always use FALSE.
#'
#' @details
#' This function uses select statement of sql to search files (from source
#' database table). The pattern is the WHERE clause (without the keyword WHERE).
#' For more information, please refer to the website of SQLite syntax. All data
#' in *.rqda use UTF-8 encoding, so the encoding of pattern matters. It will be
#' converted to UTF-8 if it is not (is.UTF8 = FALSE).
#'
#' @return
#' A data frame with variables (which is \code{invisible} and you need to print
#' it explicitly):
#' \item{id }{The file id.}
#' \item{name }{The file name.}
#' \item{file }{The file content. Only return when content is TRUE.}
#'
#' @references
#'  \url{http://www.sqlite.org/lang_expr.html}
#'
#' @seealso
#' \code{\link[gWidgets2]{gtable}}
#' \code{\link[utils]{localeToCharset}}
#'
#' @examples
#' \dontrun{
#'  ## search for files who contain the word of "keyword"
#'  searchFiles("file like '%keyword%'")
#'
#'  ## search for files whose content begin with the word of "keyword"
#'  searchFiles("file like 'keyword%'")
#'
#'  ## search for files whose name end with the word of "keyword"
#'  searchFiles("name like '%keyword'")
#'
#'  ## combined conditions
#'  searchFiles("name like '%keyword one' and file like '%keyword tow%'")
#'  }
#' @export
searchFiles <- function(pattern, content = FALSE, Fid = NULL, Widget = NULL, is.UTF8 = FALSE) {
  ##searchFiles("file like '%Xin Min Wan Bao%'")
  ##searchFiles("name like '%Wu Quan Fa%'")
  ##searchFiles("file like '%Xin Min Wan Bao%'", Widget = .rqda$.fnames_rqda)
  if (is_projOpen()) {
    if (!is.UTF8) {
      pattern <- iconv(pattern, to = "UTF-8")
    }
    Encoding(pattern) <- "unknown"
    if (!is.null(Fid)) pattern <- sprintf("(%s) and id in (%s)", pattern, paste(shQuote(Fid), collapse = ", "))
    if (content) {
      ans <- rqda_sel(sprintf("select id, name, file from source where status = 1 and %s", pattern))
    } else {
      ans <- rqda_sel(sprintf("select id, name from source where status = 1 and %s", pattern))
    }
    if (nrow(ans) > 0) Encoding(ans$name) <- "UTF-8"
    if (!is.null(ans$file)) Encoding(ans$file) <- "UTF-8"
    if (!is.null(Widget))  {
      eval(parse(text = sprintf(".rqda$%s[] <- ans$name", Widget)))
      ## eval(substitute(widget[] <- ans$name, list(widget = quote(Widget))))
    }
    cat(sprintf(gettext("%i retrieved file(s).", domain = "R-RQDA"), nrow(ans)))
    invisible(ans)
  } else cat(gettext("Open a project first.\n", domain = "R-RQDA"))
}

RunOnSelected <- function(x, multiple = TRUE, expr, enclos = parent.frame(), title = NULL,
                          hpos = ifelse(is.null(getOption("widgetCoordinate")[1]),
                                        420, getOption("widgetCoordinate")[1]),
                          vpos = ifelse(is.null(getOption("widgetCoordinate")[2]),
                                        2, getOption("widgetCoordinate")[2]),
                          ...) {
  ## expr used the return of Selected as an argument
  if (is.null(title)) title <- ifelse(multiple, "Select one or more", "Select one")
  g <- gwindow(title = title,
               width = getOption("widgetSize")[1], height = getOption("widgetSize")[2], parent = c(hpos, vpos))
  addHandlerKeystroke(g, function(h, ...) {
    if (h$key == "\027") dispose(g)
  })
  x1 <- ggroup(FALSE, container = g)
  ##x1$parent$parent$parent$SetTitle(title)
  ##x1$parent$parent$parent$SetDefaultSize(200, 500)
  x2 <- gtable(x, multiple = multiple, container = x1, expand = TRUE)
  gbutton(gettext("Cancel", domain = "R-RQDA"), container = x1, handler = function(h, ...) {
    dispose(x1)
  })
  gbutton(gettext("OK", domain = "R-RQDA"), container = x1, handler = function(h, ...) {
    Selected <- svalue(x2)
    if (Selected != "") {
      eval(h$action$expr, envir = pairlist(Selected = Selected), enclos = h$action$enclos)
      ## evaluate expr in env
      ## Variable Selected will be found in env
      ## because env is parilist and there are variables not there, which will be found in enclos.
      dispose(g)
    } else gmessage(gettext("Select before Click OK.\n", domain = "R-RQDA"), container = TRUE, icon = "error")
  },
  action = list(expr = substitute(expr), enclos = enclos)
  )
  invisible()
}


#' Select Items from a List
#'
#' @description
#' Select item(s) from a character vector.
#'
#' @param list character vector. A list of items.
#' @param multiple logical: can more than one item be selected?
#' @param title optional character string for window title.
#' @param width integer. Width of the widget.
#' @param height integer. Height of the widget.
#' @param \dots Not used currently.
#'
#' @details
#' GTK version of \code{\link[utils]{select.list}}.
#'
#' @note
#' The license of this function is subject to interpretation of the first author.
#'
#' @return
#' A character vector of selected items with UTF-8 encoding. If no
#' item was selected and 'OK' is clicked, it returns length 0 character
#' vector. If 'Cancel' is clicked, '""' is returned.
#'
#' @seealso
#' \code{\link[utils]{select.list}}
#'
#' @examples
#' \dontrun{
#' select.list(sort(.packages(all.available = TRUE)))
#' }
#' @export
gselect.list <- function(list, multiple = TRUE, title = NULL, height = getOption("widgetSize")[2], width = getOption("widgetSize")[1], ...) {
  ## gtk version of select.list(), revised on 21 Apr. 2010 to fix a bug (crash R with 2.18 or newer libgtk2).
  ## Thanks go to John Verzani for his help.
  if (is.null(title)) title <- ifelse(multiple, "Select one or more", "Select one")
  helper <- function() {
    ans <- new.env()
    dlg <- gbasicdialog(title = title, handler = function(h, ...) {
      value <- svalue(x2)
      assign("selected", value, envir = h$action$env)
    }, action = list(envir = ans))
    x2 <- gtable(list, multiple = multiple, container = dlg, expand = TRUE)
    #dlg$widget$Move(size(.rqda$.root_rqdagui)[1], 2)
    size(dlg) <- c(width, height)
    visible(dlg, set = TRUE)
    ans
  }## end helper function
  items <- helper()$selected
  if (is.null(items)) items <- ""
  items
}

#' @export
getFileNames <- function(fid = getFileIds()) {
  ans <- rqda_sel(sprintf("select name from source where status = 1 and id in (%s)", paste(shQuote(fid), collapse = ", ")))$name
  if (length(ans) > 0) Encoding(ans) <- "UTF-8"
  class(ans) <- c("RQDA.vector", "fileName")
  ans
}

#' @export
getFiles <- function(condition = c("unconditional", "case", "filecategory", "both"),
                     type = c("all", "coded", "uncoded", "selected"), names = TRUE) {
  ans <- getFileIds(condition, type)
  if (names) {
    ans <- getFileNames(ans)
  }
  ans
}

getCaseIds <- function(fid = getFileIds(), nFiles = FALSE) {
  ## if (caseName) {
  if (nFiles) {
    ## ans <- rqda_sel(sprintf(" select name, id from cases where status = 1 and id in (select caseid from caselinkage where status = 1 and fid in (%s) group by caseid)", paste(shQuote(fid), collapse = ", ")))
    ## if (nrow(ans) > 0) Encoding(ans$name) <- "UTF-8"
    ans <- rqda_sel(sprintf("select caseid, count(caseid) as nFiles from caselinkage where status = 1 and fid in (%s) group by caseid", paste(shQuote(fid), collapse = ", ")))
  } else {
    ans <- rqda_sel(sprintf("select caseid from caselinkage where status = 1 and fid in (%s) group by caseid", paste(shQuote(fid), collapse = ", ")))$caseid
  }
  ## attr(ans, "caseName") <- caseName
  class(ans) <- c("RQDA.vector", "caseId")
  ans
}

#'Get the Case ID and Case Name.
#' @aliases getCases getCaseIds getCaseNames getCases
#'
#' @description
#' Return cases IDs or names which a set of files belong to.
#' @usage
#' getCaseIds(fid = getFileIds(), nFiles = FALSE)
#'
#' getCaseNames(caseId = getCaseIds(nFiles = FALSE))
#'
#' getCases(fid, names = TRUE)
#'
#' @param fid numeric vector, the file IDs.
#' @param nFiles logical, return the number of files that belong to a case.
#' @param caseId numeric vector, the case IDs.
#' @param names logical.
#'
#' @details
#' \code{getCaseIds} returns the case IDs which a file belongs to given the file
#'  IDs.
#' \code{getCaseNames} returns the case Names given the case IDs.
#' \code{getCases} returns the case Names or IDs depending on the argument of
#'  names. It is a wrapper of \code{getCaseIds} and \code{getCaseNames}.
#'
#' @return
#' \code{getCaseIds} returns a data frame of two columns when nFiles is
#' TRUE, and a numeric vector when FALSE.
#'
#' \code{getCaseNames} returns a character vector or NULL if no cases are
#' associated with the file IDs.
#'
#' \code{getNames} returns the names of cases when names is TRUE, id of files
#' when FALSE.
#'
#' @seealso \code{\link{getFileIds}}
#'
#' @examples
#' \dontrun{
#' getCaseNames(getCaseIds(getFileIds("filecategory")))
#' }
#' @export
getCases <- function(fid, names = TRUE) {
  ans <- getCaseIds(fid, nFiles = FALSE)
  if (names) {
    ans <- getCaseNames(ans)
  }
  ans
}

#' @export
getCaseNames <- function(caseId = getCaseIds(nFiles = FALSE)) {
  ans <- rqda_sel(sprintf("select name from cases where status = 1 and id in (%s)", paste(shQuote(caseId), collapse = ", ")))$name
  if (length(ans) > 0) Encoding(ans) <- "UTF-8"
  class(ans) <- c("RQDA.vector", "caseName")
  ans
}

#' @export
casesCodedByAnd <- function(cid) {
  ## cid can be splitted across files, but still on the same case
  Ncid <- length(cid)
  cid <- paste(cid, collapse = ", ")
  fid <- rqda_sel(sprintf("select fid, cid from coding where status = 1 and cid in (%s)", cid))
  if (nrow(fid) > 0) {
    fidUnique <- unique(fid$fid)
    fidUnique <- paste(fidUnique, collapse = ", ")
    case <- rqda_sel(sprintf("select fid, caseid from caselinkage where status = 1 and fid in (%s)", fidUnique))
    codes <- tapply(case$fid, case$caseid, FUN = function(x) unique(fid[fid$fid %in% unique(x), ]$cid))
    ans <- sapply(codes, length)
    ans <- as.numeric(names(ans)[ans == Ncid])
  }
  class(ans) <- c("RQDA.vector", "caseId")
  ans
}

#' @export
casesCodedByNot <- function(cid) {
  fid <- filesCodedByOr(cid)
  codedcaseId <- getCaseIds(fid)
  allcaseid <- getCaseIds(getFileIds("unconditional", "coded"))
  ans <- setdiff(allcaseid, codedcaseId)
  class(ans) <- c("RQDA.vector", "caseId")
  ans
}

#' @export
casesCodedByOr <- function(cid) {
  fid <- filesCodedByOr(cid)
  if (length(fid) != 0) {
    ans <- getCaseIds(fid)
  } else ans <- integer(0)
  class(ans) <- c("RQDA.vector", "caseId")
  ans
}

#' @export
RQDAQuery <- function(sql) {
  if (is_projOpen()) {
    rqda_sel(sql)
  } else (cat("open a project first\n."))
}

#' Wrapper for dbGetQuery
#' @param sql sql-text
#' @importFrom DBI dbGetQuery
#' @export
rqda_sel <- function(sql) {
  if (is_projOpen()) {
    dbGetQuery(.rqda$qdacon, sql)
  } else (cat("open a project first\n."))
}

#' Wrapper for dbWriteTable
#' @param what character
#' @param obj object
#' @importFrom DBI dbWriteTable
#' @export
rqda_wrt <- function(what, obj) {
  if (is_projOpen()) {
    dbWriteTable(.rqda$qdacon, what, obj, row.names = FALSE, append = TRUE)
  } else (cat("open a project first\n."))

}

#' Wrapper for dbExecute
#' @param sql sql-text
#' @importFrom DBI dbExecute
#' @export
rqda_exe <- function(sql) {
  if (is_projOpen()) {
    dbExecute(.rqda$qdacon, sql)
  } else (cat("open a project first\n."))
}


#' @export
showSubset <- function(x, ...) {
  UseMethod("showSubset")
}

#' @method showSubset fileName
#' @export
showSubset.fileName <- function(x, widget = ".fnames_rqda", envir = .rqda, ...) {
  widget <- get(widget, envir = envir)
  class(x) <- NULL
  widget[] <- x
}

#' @method showSubset CaseAttr
#' @export
showSubset.CaseAttr <- function(x, ...) {
  tryCatch(.rqda$.CasesNamesWidget[] <- x$case, error = function(e) {
  })
}

#' @method showSubset FileAttr
#' @export
showSubset.FileAttr <- function(x, ...) {
  tryCatch(.rqda$.fnames_rqda[] <- x$file, error = function(e) {
  })
}

#' @method showSubset caseName
#' @export
showSubset.caseName <- function(x, ...) {
  class(x) <- NULL
  .rqda$.CasesNamesWidget[] <- x
}

ShowFileProperty <- function(Fid = getFileIds(type = "selected"), focus = TRUE) {
  if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {

    if (is.null(Fid)) {
      val <- "No files are selected."
      return(FALSE)
    }

    if (length(Fid) == 1) {
      Fcat <- rqda_sel(sprintf("select name from filecat where catid in (select catid from treefile where fid=%i and status = 1) and status = 1", Fid))$name
      Case <- rqda_sel(sprintf("select name from cases where id in (select caseid from caselinkage where fid=%i and status = 1) and status = 1", Fid))$name
      if (!is.null(Fcat)) Encoding(Fcat) <- "UTF-8"
      if (!is.null(Case)) Encoding(Case) <- "UTF-8"
      fcat <- paste(strwrap(sprintf(gettext("File Category is %s", domain = "R-RQDA"), paste(shQuote(Fcat), collapse = ", ")), 105, exdent = 4), collapse = "\n")
      Encoding(fcat) <- "UTF-8"
      val <- sprintf(gettext(" File ID is %i \n %s \nCase is %s", domain = "R-RQDA"), Fid, fcat, paste(shQuote(Case), collapse = ", "))
    }

    if (length(Fid) > 1) {
      val <- gettext("Please select one file only.", domain = "R-RQDA")
      return(FALSE)
    }

    # not sure what the tryCatch below is supposed to catch.
    #tryCatch(svalue(.rqda$.sfp) <- val, error = function(e) {
    gw <- gwindow(gettext("File Property", domain = "R-RQDA"),
                  width = getOption("widgetSize")[1] * .5,
                  height = getOption("widgetSize")[2] * .5)
    addHandlerKeystroke(gw, function(h, ...) {
      if (h$key == "\027") dispose(gw)
    })
    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw$set_icon(mainIcon)
    sfp <- glabel(val, container = gw)
    assign(".sfp", sfp, envir = .rqda)
    # "focus <-" (gw, value = focus) # what is this supposed to do?
    #})

  }}

#' @export
filesCodedByAnd <- function(cid, codingTable = c("coding", "coding2")) {
  cid <- paste(cid, collapse = ", ")
  fid <- rqda_sel(sprintf("select fid, cid from %s where status = 1 and cid in (%s)", codingTable, cid))
  if (nrow(fid) > 0) {
    fidList <- by(fid, factor(fid$cid), FUN = function(x) unique(x$fid))
    fid <- Reduce(intersect, fidList)
  } else {
    fid <- integer(0)
  }
  class(fid) <- c("RQDA.vector", "fileId")
  fid
}

#' @export
filesCodedByOr <- function(cid, codingTable = c("coding", "coding2")) {
  cid <- paste(cid, collapse = ", ")
  fid <- rqda_sel(sprintf("select fid from %s where status = 1 and cid in (%s)", codingTable, cid))$fid
  if (length(fid) == 0) {
    fid <- integer(0)
  }
  class(fid) <- c("RQDA.vector", "fileId")
  fid
}

#' @export
filesCodedByNot <- function(cid, codingTable = c("coding", "coding2")) {
  codedfid <- filesCodedByOr(cid)
  allfid <- rqda_sel(sprintf("select fid from %s where status = 1 group by fid", codingTable))$fid
  fid <- setdiff(allfid, codedfid)
  if (length(fid) == 0) {
    fid <- integer(0)
  }
  class(fid) <- c("RQDA.vector", "fileId")
  fid
}

#' @export
nCodedByTwo <- function(FUN, codeList = NULL, print = TRUE, ...) {
  ## codeList is character vector of codes.
  if (!is_projOpen(message = FALSE)) stop("No project is opened.", domain = "R-RQDA")
  FUN <- match.fun(FUN)
  Cid_Name <- rqda_sel("select id, name from freecode where status = 1")
  if (is.null(codeList)) {
    codeList <- gselect.list(Cid_Name$name, multiple = TRUE)
  }
  if (length(codeList) < 2) {
    stop("The codeList should be a vector of length 2 or greater.", domain = "R-RQDA")
  } else {
    cidList <- Cid_Name$id[match(codeList, Cid_Name$name)]
    ans <- matrix(nrow = length(codeList), ncol = length(codeList), dimnames = list(
      sprintf("%s(%s)", codeList, cidList),
      cidList))
    for (i in 1:length(cidList)) {
      for (j in i:length(cidList)) {
        ans[i, j] <- length(do.call(FUN, list(cidList[c(i, j)])))
      }
    }
    if (print) {
      print(ans, na.print = "")
    }
    invisible(ans)
  }
}


#' @export
"%and%" <- function(e1, e2) {
  UseMethod("%and%")
}

#' @export
"%or%" <- function(e1, e2) {
  UseMethod("%or%")
}

#' @export
"%not%" <- function(e1, e2) {
  UseMethod("%not%")
}

#' @method %and% RQDA.vector
#' @export
"%and%.RQDA.vector" <- function(e1, e2) {
  cls <- class(e1)
  ans <- intersect(e1, e2)
  class(ans) <- cls
  ans
}

#' @method %not% RQDA.vector
#' @export
"%not%.RQDA.vector" <- function(e1, e2) {
  cls <- class(e1)
  ans <- setdiff(e1, e2)
  class(ans) <- cls
  ans
}

#' @method %or% RQDA.vector
#' @export
"%or%.RQDA.vector" <- function(e1, e2) {
  cls <- class(e1)
  ans <- union(e1, e2)
  class(ans) <- cls
  ans
}

#' @export
queryFiles <- function(or = NULL, and = NULL, not = NULL, names = TRUE) {
  fid.or <- fid.and <- fid.not <- integer(0)
  if (!is.null(or)) fid.or <- filesCodedByOr(or)
  if (!is.null(and)) fid.and <- filesCodedByAnd(and)
  if (!is.null(not)) fid.or <- filesCodedByOr(not)
  ans <- setdiff(intersect(fid.or, fid.and), fid.not)
  class(ans) <- c("RQDA.vector", "fileId")
  if (names) {
    if (length(ans) > 0) {
      ans <- rqda_sel(sprintf("select name from source where status = 1 and id in (%s)", paste(ans, collapse = ", ")))$name
      Encoding(ans) <- "UTF-8"
    } else {
      ans <- character(0)
    }
    class(ans) <- c("RQDA.vector", "fileName")
    .rqda$.fnames_rqda[] <- ans
  }
  ans
}

UpdateCoding <- function() {
  rowid <- rqda_sel("select rowid from coding")$rowid
  for (i in rowid) {
    rqda_exe(sprintf("update coding set seltext=(select substr(source.file, coding.selfirst + 1, coding.selend-coding.selfirst)
        from coding inner join source on coding.fid = source.id where coding.ROWID=%i) where coding.ROWID=%i", i, i))
  }}
#UpdateCoding()

#' @export
filesByCodes <- function(codingTable = c("coding", "coding2")) {
  codingTable <- match.arg(codingTable)
  if (codingTable == "coding") {
    ans <- rqda_sel("select coding.fid as fid, freecode.name as codename, source.name as filename from coding left join freecode on (coding.cid = freecode.id)left join source on (coding.fid = source.id) where coding.status = 1 and source.status = 1 and freecode.status = 1")
  }
  if (codingTable == "coding2") {
    ans <- rqda_sel("select coding2.fid as fid, freecode.name as codename, source.name as filename from coding2 left join freecode on (coding2.cid = freecode.id)left join source on (coding2.fid = source.id) where coding2.status = 1 and source.status = 1 and freecode.status = 1")
  }
  if (nrow(ans) != 0) {
    Encoding(ans$codename) <- Encoding(ans$filename) <- "UTF-8"
    ans$codedBy <- 1
    ansWide <- reshape(ans, idvar = "fid", timevar = "codename", v.names = "codedBy", direction = "wide")
    ansWide[is.na(ansWide)] <- 0
  }
  ansWide
}

#' translate string
#'
#' used internally to avoid long abundance of code
#' @param text_string string input
rqda_txt <- function(text_string) {
  gettext(text_string, domain = "R-RQDA")
}


# ── ANNOTATIONS ─────────────────────────────────────────────────────────────
# getAnnos is in utils.R - print.Info4Widget handles display
# Annotation widget in file viewer (add/view annotations on text selections)

editAnnotation <- function(rowid, annotation_text, position, fid,
                           parent_win = NULL, parent_box = NULL) {
  # If we have the parent outer box, embed edit UI inside the same window
  # instead of opening a new window (avoids macOS window ordering issues)
  if (!is.null(parent_box)) {
    .embed_annotation_editor(rowid, annotation_text, position, parent_box)
    return(invisible(NULL))
  }

  # Fallback: separate window (used when parent_box not available)
  if (exists(".anno_edit_win", envir=.rqda) && !is.null(.rqda$.anno_edit_win)) {
    tryCatch(gtkWindowPresent(.rqda$.anno_edit_win), error=function(e){})
    return(invisible(NULL))
  }
  anno_win <- gtkWindowNew()
  gtkWindowSetTitle(anno_win, sprintf("Edit annotation (pos %d)", position))
  gtkWindowSetDefaultSize(anno_win, 420L, 260L)
  assign(".anno_edit_win", anno_win, envir=.rqda)

  vbox <- gtkBoxNew(1L, 6L)
  gtkWidgetSetMarginTop(vbox, 10L); gtkWidgetSetMarginBottom(vbox, 10L)
  gtkWidgetSetMarginStart(vbox, 10L); gtkWidgetSetMarginEnd(vbox, 10L)
  gtkWindowSetChild(anno_win, vbox)

  sw  <- gtkScrolledWindowNew(); gtkWidgetSetVexpand(sw, TRUE)
  tv  <- gtkTextViewNew(); gtkTextViewSetWrapMode(tv, 2L)
  tryCatch(apply_font_css(tv), error=function(e){})
  gtkScrolledWindowSetChild(sw, tv)
  buf <- gtkTextViewGetBuffer(tv)
  gtkTextBufferSetText(buf, as.character(annotation_text), -1L)
  gtkBoxAppend(vbox, sw)

  bbox       <- gtkBoxNew(0L, 6L); gtkWidgetSetHalign(bbox, 3L)
  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  del_btn    <- gtkButtonNewWithLabel("Delete")
  save_btn   <- gtkButtonNewWithLabel("Save")
  for (b in list(cancel_btn, del_btn, save_btn)) gtkBoxAppend(bbox, b)
  gtkBoxAppend(vbox, bbox)

  do_close <- function(action = "cancel") {
    if (action == "save") {
      si   <- gtkTextBufferGetStartIter(buf)
      ei   <- gtkTextBufferGetEndIter(buf)
      text <- as.character(gtkTextBufferGetText(buf, si, ei, FALSE))
      rqda_exe(sprintf("UPDATE annotation SET annotation='%s', dateM='%s' WHERE rowid=%d",
                       enc(text), date(), as.integer(rowid)))
      message("Annotation updated")
    } else if (action == "delete") {
      rqda_exe(sprintf("UPDATE annotation SET status=0 WHERE rowid=%d", as.integer(rowid)))
      message("Annotation deleted")
    }
    assign(".anno_edit_win", NULL, envir=.rqda)
    gtkWindowClose(anno_win)
  }

  gSignalConnectR(save_btn,   "clicked", function(w) do_close("save"))
  gSignalConnectR(del_btn,    "clicked", function(w) do_close("delete"))
  gSignalConnectR(cancel_btn, "clicked", function(w) do_close("cancel"))
  gSignalConnectR(anno_win, "close-request", function(w) { do_close(); FALSE })
  gtkWindowPresent(anno_win)
  invisible(NULL)
}

.embed_annotation_editor <- function(rowid, annotation_text, position, outer) {
  # Replace contents of outer vbox with an inline editor, restore on done
  # outer is the GtkBox that contains the card list

  # Hide current children
  child <- gtkWidgetGetFirstChild(outer)
  hidden <- list()
  while (!is.null(child)) {
    nxt <- gtkWidgetGetNextSibling(child)
    gtkWidgetSetVisible(child, FALSE)
    hidden <- c(hidden, list(child))
    child <- nxt
  }

  # Build inline editor
  editor_box <- gtkBoxNew(1L, 6L)
  gtkWidgetSetMarginTop(editor_box, 10L); gtkWidgetSetMarginBottom(editor_box, 10L)
  gtkWidgetSetMarginStart(editor_box, 10L); gtkWidgetSetMarginEnd(editor_box, 10L)
  gtkWidgetSetVexpand(editor_box, TRUE)

  title_lbl <- gtkLabelNew(sprintf("Editing annotation at position %d", position))
  gtkLabelSetXalign(title_lbl, 0.0)
  gtkBoxAppend(editor_box, title_lbl)

  sw  <- gtkScrolledWindowNew(); gtkWidgetSetVexpand(sw, TRUE)
  tv  <- gtkTextViewNew(); gtkTextViewSetWrapMode(tv, 2L)
  tryCatch(apply_font_css(tv), error=function(e){})
  gtkScrolledWindowSetChild(sw, tv)
  buf <- gtkTextViewGetBuffer(tv)
  gtkTextBufferSetText(buf, as.character(annotation_text), -1L)
  gtkBoxAppend(editor_box, sw)

  bbox       <- gtkBoxNew(0L, 6L); gtkWidgetSetHalign(bbox, 3L)
  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  del_btn    <- gtkButtonNewWithLabel("Delete")
  save_btn   <- gtkButtonNewWithLabel("Save")
  for (b in list(cancel_btn, del_btn, save_btn)) gtkBoxAppend(bbox, b)
  gtkBoxAppend(editor_box, bbox)
  gtkBoxAppend(outer, editor_box)

  restore <- function() {
    gtkBoxRemove(outer, editor_box)
    for (w in hidden) gtkWidgetSetVisible(w, TRUE)
  }

  gSignalConnectR(cancel_btn, "clicked", function(w) restore())
  gSignalConnectR(save_btn, "clicked", function(w) {
    si   <- gtkTextBufferGetStartIter(buf)
    ei   <- gtkTextBufferGetEndIter(buf)
    text <- as.character(gtkTextBufferGetText(buf, si, ei, FALSE))
    rqda_exe(sprintf("UPDATE annotation SET annotation='%s', dateM='%s' WHERE rowid=%d",
                     enc(text), date(), as.integer(rowid)))
    message("Annotation updated")
    restore()
  })
  gSignalConnectR(del_btn, "clicked", function(w) {
    rqda_exe(sprintf("UPDATE annotation SET status=0 WHERE rowid=%d", as.integer(rowid)))
    message("Annotation deleted")
    restore()
  })

  gtkWidgetGrabFocus(tv)
  invisible(NULL)
}



addAnnotation <- function(fid, textview) {
  if (!is_projOpen(message=FALSE)) return(invisible(NULL))

  # Get cursor position - annotation is point-based not range-based
  buffer   <- gtkTextViewGetBuffer(textview)
  ins_mark <- gtkTextBufferGetInsert(buffer)
  ins_iter <- gtkTextBufferGetIterAtMark(buffer, ins_mark)
  position <- gtkTextIterGetOffset(ins_iter)

  # Check for existing annotation near this position (+/- 2 chars)
  exist <- rqda_sel(sprintf(
    "SELECT rowid, annotation FROM annotation WHERE fid=%d AND status=1 AND position BETWEEN %d AND %d",
    fid, position - 2L, position + 2L))
  current <- if (!is.null(exist) && nrow(exist)>0) as.character(exist$annotation[1]) else ""

  anno_win <- gtkWindowNew()
  gtkWindowSetTitle(anno_win, sprintf("Annotation at position %d", position))
  gtkWindowSetDefaultSize(anno_win, 400L, 250L)
  if (exists(".rqda_window", envir=.rqda))
    gtkWindowSetTransientFor(anno_win, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 4L)
  gtkWidgetSetMarginTop(vbox, 8L); gtkWidgetSetMarginBottom(vbox, 8L)
  gtkWidgetSetMarginStart(vbox, 8L); gtkWidgetSetMarginEnd(vbox, 8L)
  gtkWindowSetChild(anno_win, vbox)

  # Show a snippet of context around the cursor
  ctx_start <- gtkTextBufferGetIterAtOffset(buffer, max(0L, position - 30L))
  ctx_end   <- gtkTextBufferGetIterAtOffset(buffer, position + 30L)
  ctx_text  <- as.character(gtkTextBufferGetText(buffer, ctx_start, ctx_end, FALSE))
  ctx_lbl <- gtkLabelNew(sprintf("...%s...", ctx_text))
  gtkLabelSetXalign(ctx_lbl, 0.0)
  gtkLabelSetWrap(ctx_lbl, TRUE)
  gtkBoxAppend(vbox, ctx_lbl)

  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)
  gtkBoxAppend(vbox, sw)
  tv <- gtkTextViewNew()
  gtkTextViewSetWrapMode(tv, 2L)
  tryCatch(apply_font_css(tv), error=function(e){})
  gtkScrolledWindowSetChild(sw, tv)
  buf <- gtkTextViewGetBuffer(tv)
  if (nzchar(current)) gtkTextBufferSetText(buf, current, -1L)

  bbox <- gtkBoxNew(0L, 4L)
  gtkWidgetSetHalign(bbox, 3L)
  del_btn  <- gtkButtonNewWithLabel("Delete")
  save_btn <- gtkButtonNewWithLabel("Save")
  gtkBoxAppend(bbox, del_btn)
  gtkBoxAppend(bbox, save_btn)
  gtkBoxAppend(vbox, bbox)

  gSignalConnectR(save_btn, "clicked", function(w) {
    si   <- gtkTextBufferGetStartIter(buf)
    ei   <- gtkTextBufferGetEndIter(buf)
    text <- as.character(gtkTextBufferGetText(buf, si, ei, FALSE))
    if (!is.null(exist) && nrow(exist)>0) {
      rqda_exe(sprintf("UPDATE annotation SET annotation='%s', dateM='%s' WHERE rowid=%d",
                       enc(text), date(), exist$rowid[1]))
    } else {
      rqda_exe(sprintf(
        "INSERT INTO annotation (annotation, fid, position, status, owner, date, dateM) VALUES ('%s',%d,%d,1,'%s','%s','%s')",
        enc(text), fid, position, .rqda$owner, date(), date()))
    }
    gtkWindowClose(anno_win)
  })

  gSignalConnectR(del_btn, "clicked", function(w) {
    if (!is.null(exist) && nrow(exist)>0)
      rqda_exe(sprintf("UPDATE annotation SET status=0 WHERE rowid=%d", exist$rowid[1]))
    gtkWindowClose(anno_win)
  })

  gtkWindowPresent(anno_win)
}
