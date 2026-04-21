# R/Setting.R
# R/Setting.R

# Show a scrollable color picker populated from R's named colors.
# Returns the chosen color name (character) or NULL if cancelled.
.show_color_picker <- function(current = "blue") {
  res  <- list(val = NULL)
  loop <- gMainLoopNew(NULL, FALSE)

  dialog <- gtkWindowNew()
  gtkWindowSetTitle(dialog, "Pick a Color")
  gtkWindowSetDefaultSize(dialog, 520L, 480L)
  gtkWindowSetModal(dialog, TRUE)
  if (exists(".rqda_window", envir = .rqda))
    gtkWindowSetTransientFor(dialog, .rqda$.rqda_window)

  vbox <- gtkBoxNew(1L, 6L)
  gtkWidgetSetMarginTop(vbox, 8L); gtkWidgetSetMarginBottom(vbox, 8L)
  gtkWidgetSetMarginStart(vbox, 8L); gtkWidgetSetMarginEnd(vbox, 8L)
  gtkWindowSetChild(dialog, vbox)

  # Search entry
  search_box <- gtkBoxNew(0L, 6L)
  search_lbl <- gtkLabelNew("Filter:")
  search_entry <- gtkEntryNew()
  gtkWidgetSetHexpand(search_entry, TRUE)
  gtkBoxAppend(search_box, search_lbl)
  gtkBoxAppend(search_box, search_entry)
  gtkBoxAppend(vbox, search_box)

  # Selected color display
  sel_box    <- gtkBoxNew(0L, 8L)
  sel_swatch <- gtkLabelNew("")
  sel_lbl    <- gtkLabelNew(sprintf("Selected: %s", current))
  gtkBoxAppend(sel_box, sel_swatch)
  gtkBoxAppend(sel_box, sel_lbl)
  gtkBoxAppend(vbox, sel_box)

  update_sel_display <- function(col) {
    hex <- .col_to_hex(col)
    markup <- sprintf('<span foreground="%s" size="xx-large">&#x25A0;</span>', hex)
    tryCatch(gtkLabelSetMarkup(sel_swatch, markup), error = function(e) {})
    gtkLabelSetText(sel_lbl, sprintf("Selected: %s", col))
    res$val <<- col
  }
  update_sel_display(current)

  # Scrollable flow box of colors
  sw <- gtkScrolledWindowNew()
  gtkWidgetSetVexpand(sw, TRUE)
  gtkBoxAppend(vbox, sw)

  flow <- gtkFlowBoxNew()
  gtkFlowBoxSetMaxChildrenPerLine(flow, 12L)
  gtkFlowBoxSetSelectionMode(flow, 0L)  # NONE - handle via gesture
  gtkScrolledWindowSetChild(sw, flow)

  all_cols <- colors()

  make_swatch_btn <- function(col) {
    hex    <- .col_to_hex(col)
    markup <- sprintf('<span foreground="%s" size="x-large">&#x25A0;</span>', hex)
    lbl    <- gtkLabelNew("")
    tryCatch(gtkLabelSetMarkup(lbl, markup), error = function(e) gtkLabelSetText(lbl, "■"))
    gtkWidgetSetTooltipText(lbl, col)
    btn <- gtkButtonNew()
    gtkButtonSetChild(btn, lbl)
    gtkWidgetSetSizeRequest(btn, 44L, 44L)
    gSignalConnectR(btn, "clicked", function(w) update_sel_display(col))
    btn
  }

  populate <- function(filter = "") {
    # Remove existing children
    child <- gtkWidgetGetFirstChild(flow)
    while (!is.null(child)) {
      nxt <- gtkWidgetGetNextSibling(child)
      gtkFlowBoxRemove(flow, child)
      child <- nxt
    }
    cols <- if (nzchar(filter)) all_cols[grepl(filter, all_cols, ignore.case=TRUE)] else all_cols
    for (col in cols) {
      btn <- make_swatch_btn(col)
      gtkFlowBoxAppend(flow, btn)
    }
  }
  populate()

  gSignalConnectR(search_entry, "changed", function(w) {
    f <- gtkEntryBufferGetText(gtkEntryGetBuffer(w))
    populate(f)
  })

  # OK / Cancel
  bbox       <- gtkBoxNew(0L, 8L)
  gtkWidgetSetHalign(bbox, 3L)
  cancel_btn <- gtkButtonNewWithLabel("Cancel")
  ok_btn     <- gtkButtonNewWithLabel("OK")
  gSignalConnectR(cancel_btn, "clicked", function(w) {
    res$val <<- NULL; gMainLoopQuit(loop); gtkWindowDestroy(dialog)
  })
  gSignalConnectR(ok_btn, "clicked", function(w) {
    gMainLoopQuit(loop); gtkWindowDestroy(dialog)
  })
  gSignalConnectR(dialog, "close-request", function(w) {
    res$val <<- NULL; gMainLoopQuit(loop); FALSE
  })
  gtkBoxAppend(bbox, cancel_btn)
  gtkBoxAppend(bbox, ok_btn)
  gtkBoxAppend(vbox, bbox)

  gtkWindowPresent(dialog)
  gMainLoopRun(loop)
  res$val
}


.rqda_config_file <- function() {
  dir <- tools::R_user_dir("RQDA", which = "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "settings.csv")
}

#' Load persisted settings into .rqda
#' @keywords internal
load_rqda_settings <- function() {
  path <- .rqda_config_file()
  if (!file.exists(path)) return(invisible(NULL))
  tryCatch({
    s <- read.csv(path, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(s))) {
      nm  <- s$key[i]
      val <- s$value[i]
      # Coerce known logical and integer fields
      if (nm %in% c("BOM", "SFP", "dark.mode")) val <- isTRUE(val == "TRUE")
      if (nm == "font.zoom") val <- suppressWarnings(as.numeric(val))
      if (nm %in% c("hotkey.mark","hotkey.unmark")) {
        key <- if (nm == "hotkey.mark") "mark" else "unmark"
        if (!exists("hotkeys", envir=.rqda)) .rqda$hotkeys <- list()
        .rqda$hotkeys[[key]] <- val
        next
      }
      if (nm == "font.size")        val <- suppressWarnings(as.integer(val))
      assign(nm, val, envir = .rqda)
    }
  }, error = function(e) message("Could not load settings: ", e$message))
}

#' Save current .rqda settings to disk
#' @keywords internal
save_rqda_settings <- function() {
  s <- data.frame(
    key   = c("owner","encoding","back.col","fore.col",
              "codingTable","BOM","SFP","font.family","font.size",
              "dark.mode","font.zoom","tab.pos","hotkey.mark","hotkey.unmark"),
    value = c(.rqda$owner, .rqda$encoding, .rqda$back.col, .rqda$fore.col,
              .rqda$codingTable, as.character(.rqda$BOM),
              as.character(.rqda$SFP), .rqda$font.family,
              as.character(.rqda$font.size),
              as.character(.rqda$dark.mode),
              as.character(.rqda$font.zoom),
              as.character(.rqda$tab.pos),
              tryCatch(.rqda$hotkeys[["mark"]],   error=function(e) "m"),
              tryCatch(.rqda$hotkeys[["unmark"]], error=function(e) "u")),
    stringsAsFactors = FALSE
  )
  tryCatch(write.csv(s, .rqda_config_file(), row.names = FALSE),
           error = function(e) message("Could not save settings: ", e$message))
}

#' Apply current font settings via CSS to a GTK widget
#' Zoom factor (.rqda$font.zoom) scales the point size.
#' @keywords internal
apply_font_css <- function(widget, family = .rqda$font.family, size = .rqda$font.size) {
  if (is.null(family) || !nzchar(family)) family <- "Sans"
  if (is.null(size)   || size <= 0)      size   <- 11
  zoom <- if (!is.null(.rqda$font.zoom) && .rqda$font.zoom > 0) .rqda$font.zoom else 1.0
  effective_size <- max(6L, as.integer(round(size * zoom)))
  css <- sprintf("* { font-family: \"%s\"; font-size: %dpt; }", family, effective_size)
  provider <- gtkCssProviderNew()
  tryCatch({
    gtkCssProviderLoadFromString(provider, css)
    ctx <- gtkWidgetGetStyleContext(widget)
    gtkStyleContextAddProvider(ctx, provider, 600L)
  }, error = function(e) {})
}

#' Toggle dark mode via GTK settings
#' @keywords internal
.apply_dark_mode <- function(dark) {
  tryCatch({
    settings <- gtkSettingsGetDefault()
    gObjectSetBoolean(settings, "gtk-application-prefer-dark-theme", isTRUE(dark))
  }, error = function(e) message("Dark mode not supported: ", e$message))
}

#' Apply dark mode at startup if saved setting requires it
#' @keywords internal
apply_startup_settings <- function() {
  tryCatch(.apply_dark_mode(isTRUE(.rqda$dark.mode)), error = function(e) {})
}

# Color string -> R rgb vector (works with named colors and hex)
.col_to_hex <- function(col) {
  tryCatch({
    rgb_vals <- col2rgb(col) / 255
    sprintf("#%02X%02X%02X",
            as.integer(rgb_vals[1]*255),
            as.integer(rgb_vals[2]*255),
            as.integer(rgb_vals[3]*255))
  }, error = function(e) "#888888")
}

#' Build the Settings tab UI
#' @export
addSettingGUI <- function(container) {
  # Initialize defaults
  if (!exists("owner",       envir=.rqda)) .rqda$owner       <- Sys.info()["user"]
  if (!exists("encoding",    envir=.rqda)) .rqda$encoding    <- "UTF-8"
  if (!exists("back.col",    envir=.rqda)) .rqda$back.col    <- "gold"
  if (!exists("fore.col",    envir=.rqda)) .rqda$fore.col    <- "blue"
  if (!exists("codingTable", envir=.rqda)) .rqda$codingTable <- "coding"
  if (!exists("BOM",         envir=.rqda)) .rqda$BOM         <- FALSE
  if (!exists("SFP",         envir=.rqda)) .rqda$SFP         <- FALSE
  if (!exists("font.family", envir=.rqda))
    .rqda$font.family <- tryCatch(get_system_font_family(), error=function(e) "Sans")
  if (!exists("font.size",   envir=.rqda))
    .rqda$font.size   <- tryCatch(get_system_font_size(),   error=function(e) 11L)
  if (!exists("font",        envir=.rqda)) .rqda$font        <- "Sans 11"
  if (!exists("dark.mode",  envir=.rqda)) .rqda$dark.mode  <- FALSE
  if (!exists("font.zoom",  envir=.rqda)) .rqda$font.zoom  <- 1.0
  if (!exists("tab.pos",    envir=.rqda)) .rqda$tab.pos    <- "right"
  if (!exists("hotkeys",    envir=.rqda)) .rqda$hotkeys    <- list(mark="m", unmark="u")

  # Load any previously saved settings
  load_rqda_settings()

  vbox <- container$widget
  gtkWidgetSetMarginStart(vbox, 8L)
  gtkWidgetSetMarginEnd(vbox, 8L)
  gtkWidgetSetMarginTop(vbox, 6L)

  # ── helper: section label ──────────────────────────────────────────────
  add_section <- function(text) {
    lbl <- gtkLabelNew(sprintf("<b>%s</b>", text))
    gtkLabelSetUseMarkup(lbl, TRUE)
    gtkLabelSetXalign(lbl, 0.0)
    gtkWidgetSetMarginTop(lbl, 12L)
    gtkBoxAppend(vbox, lbl)
  }

  # ── helper: text entry row ─────────────────────────────────────────────
  add_entry_row <- function(label_text, get_val, set_val) {
    hbox <- gtkBoxNew(0L, 10L)
    lbl  <- gtkLabelNew(label_text)
    gtkLabelSetXalign(lbl, 0.0)
    gtkWidgetSetSizeRequest(lbl, 180L, -1L)
    entry  <- gtkEntryNew()
    buffer <- gtkEntryGetBuffer(entry)
    gtkEntryBufferSetText(buffer, as.character(get_val()), -1L)
    gSignalConnectR(entry, "changed", function(w) {
      set_val(gtkEntryBufferGetText(gtkEntryGetBuffer(w)))
    })
    gtkWidgetSetHexpand(entry, TRUE)
    gtkBoxAppend(hbox, lbl)
    gtkBoxAppend(hbox, entry)
    gtkBoxAppend(vbox, hbox)
    entry
  }

  # ── helper: color row (entry + swatch + palette picker) ──────────────
  add_color_row <- function(label_text, get_val, set_val) {
    hbox   <- gtkBoxNew(0L, 10L)
    lbl    <- gtkLabelNew(label_text)
    gtkLabelSetXalign(lbl, 0.0)
    gtkWidgetSetSizeRequest(lbl, 180L, -1L)

    entry  <- gtkEntryNew()
    buf    <- gtkEntryGetBuffer(entry)
    gtkEntryBufferSetText(buf, as.character(get_val()), -1L)
    gtkWidgetSetHexpand(entry, TRUE)

    swatch <- gtkLabelNew("")
    update_swatch <- function(col_str) {
      hex <- .col_to_hex(col_str)
      markup <- sprintf('<span foreground="%s" size="xx-large">&#x25A0;</span>', hex)
      tryCatch(gtkLabelSetMarkup(swatch, markup),
               error = function(e) gtkLabelSetText(swatch, "■"))
    }
    update_swatch(get_val())

    gSignalConnectR(entry, "changed", function(w) {
      val <- gtkEntryBufferGetText(gtkEntryGetBuffer(w))
      set_val(val)
      update_swatch(val)
    })

    pick_btn <- gtkButtonNewWithLabel("…")
    gtkWidgetSetTooltipText(pick_btn, "Pick from R color palette")
    gSignalConnectR(pick_btn, "clicked", function(w) {
      chosen <- .show_color_picker(get_val())
      if (!is.null(chosen)) {
        gtkEntryBufferSetText(gtkEntryGetBuffer(entry), chosen, -1L)
        set_val(chosen)
        update_swatch(chosen)
      }
    })

    gtkBoxAppend(hbox, lbl)
    gtkBoxAppend(hbox, entry)
    gtkBoxAppend(hbox, swatch)
    gtkBoxAppend(hbox, pick_btn)
    gtkBoxAppend(vbox, hbox)
    entry
  }

  # ── helper: checkbox row ───────────────────────────────────────────────
  add_check_row <- function(label_text, get_val, set_val) {
    hbox <- gtkBoxNew(0L, 10L)
    lbl  <- gtkLabelNew(label_text)
    gtkLabelSetXalign(lbl, 0.0)
    gtkWidgetSetSizeRequest(lbl, 180L, -1L)
    cb <- gtkCheckButtonNew()
    gtkCheckButtonSetActive(cb, isTRUE(get_val()))
    # Track state manually - gtkCheckButtonGetActive returns externalptr
    state <- new.env(parent = emptyenv())
    state$val <- isTRUE(get_val())
    gSignalConnectR(cb, "toggled", function(w) {
      state$val <- !state$val
      set_val(state$val)
    })
    gtkBoxAppend(hbox, lbl)
    gtkBoxAppend(hbox, cb)
    gtkBoxAppend(vbox, hbox)
  }

  # ── Font section ───────────────────────────────────────────────────────
  add_section("Font")

  hbox_font <- gtkBoxNew(0L, 10L)
  lbl_font  <- gtkLabelNew("Font:")
  gtkLabelSetXalign(lbl_font, 0.0)
  gtkWidgetSetSizeRequest(lbl_font, 180L, -1L)
  gtkBoxAppend(hbox_font, lbl_font)

  current_font_str <- sprintf("%s %d", .rqda$font.family, .rqda$font.size)

  # Try gtkFontButtonNew - native font picker
  font_btn <- tryCatch(gtkFontButtonNew(), error = function(e) NULL)
  if (!is.null(font_btn)) {
    # Set the displayed font using the combined string "Family Size"
    tryCatch(gtkFontChooserSetFont(font_btn, current_font_str), error = function(e)
      tryCatch(gtkFontButtonSetFontName(font_btn, current_font_str), error = function(e2) {}))
    gSignalConnectR(font_btn, "font-set", function(w) {
      chosen <- tryCatch(gtkFontChooserGetFont(w), error = function(e)
        tryCatch(gtkFontButtonGetFontName(w), error = function(e2) NULL))
      if (!is.null(chosen) && nzchar(chosen)) {
        parts <- strsplit(trimws(chosen), "\\s+")[[1]]
        sz <- suppressWarnings(as.integer(tail(parts, 1)))
        if (!is.na(sz) && sz > 0) {
          fam <- paste(head(parts, -1), collapse = " ")
        } else {
          sz  <- .rqda$font.size
          fam <- chosen
        }
        .rqda$font.family <- fam
        .rqda$font.size   <- sz
        .rqda$font        <- sprintf("%s %d", fam, sz)
      }
    })
    gtkBoxAppend(hbox_font, font_btn)
  } else {
    # Fallback: manual entries
    entry_fam <- gtkEntryNew()
    gtkEntryBufferSetText(gtkEntryGetBuffer(entry_fam), .rqda$font.family, -1L)
    gtkWidgetSetHexpand(entry_fam, TRUE)
    lbl_sz   <- gtkLabelNew("Size:")
    entry_sz <- gtkEntryNew()
    gtkEntryBufferSetText(gtkEntryGetBuffer(entry_sz), as.character(.rqda$font.size), -1L)
    gtkWidgetSetSizeRequest(entry_sz, 60L, -1L)
    update_font <- function() {
      fam <- gtkEntryBufferGetText(gtkEntryGetBuffer(entry_fam))
      sz  <- suppressWarnings(as.integer(gtkEntryBufferGetText(gtkEntryGetBuffer(entry_sz))))
      if (is.na(sz) || sz <= 0) sz <- 11L
      .rqda$font.family <- fam; .rqda$font.size <- sz
      .rqda$font <- sprintf("%s %d", fam, sz)
    }
    gSignalConnectR(entry_fam, "changed", function(w) update_font())
    gSignalConnectR(entry_sz,  "changed", function(w) update_font())
    gtkBoxAppend(hbox_font, entry_fam)
    gtkBoxAppend(hbox_font, lbl_sz)
    gtkBoxAppend(hbox_font, entry_sz)
  }
  gtkBoxAppend(vbox, hbox_font)

  # ── General section ────────────────────────────────────────────────────
  add_section("General")
  add_entry_row("Name of Coder:",
                function() .rqda$owner,
                function(v) { .rqda$owner <- v })
  add_entry_row("File Encoding:",
                function() .rqda$encoding,
                function(v) { .rqda$encoding <- v })
  add_entry_row("Coding table:",
                function() .rqda$codingTable,
                function(v) { .rqda$codingTable <- v })
  add_check_row("Byte Order Mark (BOM):",
                function() .rqda$BOM,
                function(v) { .rqda$BOM <- v })
  add_check_row("Show File Property:",
                function() .rqda$SFP,
                function(v) { .rqda$SFP <- v })

  # ── Colors section ─────────────────────────────────────────────────────
  add_section("Colors")
  add_color_row("Color for Coding:",
                function() .rqda$fore.col,
                function(v) { .rqda$fore.col <- v })
  add_color_row("Color for Case:",
                function() .rqda$back.col,
                function(v) { .rqda$back.col <- v })

  # ── Display section ────────────────────────────────────────────────────
  add_section("Display")

  # Tab position
  tab_hbox <- gtkBoxNew(0L, 10L)
  tab_lbl  <- gtkLabelNew("Tab position:")
  gtkLabelSetXalign(tab_lbl, 0.0)
  gtkWidgetSetSizeRequest(tab_lbl, 180L, -1L)

  tab_store <- gtkStringListNew(NULL)
  tab_opts  <- c("top", "bottom", "left", "right")
  for (o in tab_opts) gtkStringListAppend(tab_store, o)
  tab_drop  <- gtkDropDownNew(tab_store, NULL)
  cur_idx   <- match(.rqda$tab.pos, tab_opts) - 1L
  if (!is.na(cur_idx) && cur_idx >= 0L) gtkDropDownSetSelected(tab_drop, as.integer(cur_idx))
  apply_tab_pos <- function() {
    idx <- gtkDropDownGetSelected(tab_drop)
    if (is.null(idx) || idx < 0L) return()
    pos <- tab_opts[as.integer(idx) + 1L]
    if (is.na(pos)) return()
    .rqda$tab.pos <- pos
    if (exists(".nb_rqdagui", envir = .GlobalEnv)) {
      pos_map <- c(top=2L, bottom=3L, left=0L, right=1L)
      tryCatch(gtkNotebookSetTabPos(.nb_rqdagui$widget, unname(as.integer(pos_map[pos]))),
               error = function(e) message("SetTabPos: ", e$message))
      message("Tab position: ", pos)
    }
  }
  tab_apply_btn <- gtkButtonNewWithLabel("Apply")
  gtkWidgetSetVexpand(tab_apply_btn, FALSE)
  gtkWidgetSetValign(tab_apply_btn, 3L)
  gSignalConnectR(tab_apply_btn, "clicked", function(w) apply_tab_pos())

  gtkBoxAppend(tab_hbox, tab_lbl)
  gtkBoxAppend(tab_hbox, tab_drop)
  gtkBoxAppend(tab_hbox, tab_apply_btn)
  gtkBoxAppend(vbox, tab_hbox)

  # Dark mode toggle
  add_check_row("Dark mode:",
                function() isTRUE(.rqda$dark.mode),
                function(v) {
                  .rqda$dark.mode <- v
                  .apply_dark_mode(v)
                })

  # Zoom slider
  zoom_hbox <- gtkBoxNew(0L, 10L)
  zoom_lbl  <- gtkLabelNew("File viewer zoom:")
  gtkLabelSetXalign(zoom_lbl, 0.0)
  gtkWidgetSetSizeRequest(zoom_lbl, 180L, -1L)

  zoom_scale <- gtkScaleNewWithRange(0L, 0.5, 3.0, 0.1)
  gtkRangeSetValue(zoom_scale, .rqda$font.zoom)
  gtkScaleSetDigits(zoom_scale, 1L)
  gtkScaleSetDrawValue(zoom_scale, TRUE)
  gtkWidgetSetHexpand(zoom_scale, TRUE)
  gSignalConnectR(zoom_scale, "value-changed", function(w) {
    .rqda$font.zoom <- gtkRangeGetValue(w)
  })

  gtkBoxAppend(zoom_hbox, zoom_lbl)
  gtkBoxAppend(zoom_hbox, zoom_scale)
  gtkBoxAppend(vbox, zoom_hbox)

  # ── Hotkeys section ────────────────────────────────────────────────────
  add_section("Hotkeys (single key, used in file viewer)")
  if (!exists("hotkeys", envir=.rqda)) .rqda$hotkeys <- list(mark="m", unmark="u")

  add_entry_row("Mark key (default: m):",
                function() .rqda$hotkeys[["mark"]] %||% "m",
                function(v) { if (!exists("hotkeys",envir=.rqda)) .rqda$hotkeys <- list()
                .rqda$hotkeys[["mark"]] <- substr(v,1,1) })
  add_entry_row("Unmark key (default: u):",
                function() .rqda$hotkeys[["unmark"]] %||% "u",
                function(v) { if (!exists("hotkeys",envir=.rqda)) .rqda$hotkeys <- list()
                .rqda$hotkeys[["unmark"]] <- substr(v,1,1) })

  # ── Spacer + buttons ───────────────────────────────────────────────────
  spacer <- gtkBoxNew(0L, 0L)
  gtkWidgetSetVexpand(spacer, TRUE)
  gtkBoxAppend(vbox, spacer)

  btn_hbox <- gtkBoxNew(0L, 10L)
  gtkWidgetSetMarginBottom(btn_hbox, 8L)
  gtkWidgetSetHalign(btn_hbox, 2L)  # END

  reset_btn <- gtkButtonNewWithLabel("Reset to Defaults")
  gSignalConnectR(reset_btn, "clicked", function(w) {
    .rqda$owner       <- Sys.info()["user"]
    .rqda$encoding    <- "UTF-8"
    .rqda$back.col    <- "gold"
    .rqda$fore.col    <- "blue"
    .rqda$codingTable <- "coding"
    .rqda$BOM         <- FALSE
    .rqda$SFP         <- FALSE
    .rqda$font.family <- "Sans"
    .rqda$font.size   <- 11L
    .rqda$font        <- "Sans 11"
    .rqda$dark.mode   <- FALSE
    .rqda$font.zoom   <- 1.0
    .rqda$tab.pos     <- "right"
    .apply_dark_mode(FALSE)
    save_rqda_settings()
    message("Settings reset to defaults.")
  })

  save_btn <- gtkButtonNewWithLabel("Save Settings")
  gSignalConnectR(save_btn, "clicked", function(w) {
    apply_tab_pos()
    save_rqda_settings()
    message(sprintf("Settings saved to: %s", .rqda_config_file()))
  })

  gtkBoxAppend(btn_hbox, reset_btn)
  gtkBoxAppend(btn_hbox, save_btn)
  gtkBoxAppend(vbox, btn_hbox)
}
