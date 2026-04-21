# Main GUI for RQDA - RGtk4 version

#' Launch RQDA Qualitative Data Analysis Application
#'
#' Starts the RQDA graphical user interface for qualitative data analysis.
#' On first call, creates the application and window. On subsequent calls,
#' brings the existing window to the front. The application stays in the
#' dock/taskbar even when the window is closed - click the dock icon or
#' call RQDA() again to restore the window.
#'
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' library(RQDA)
#' RQDA()  # Launch RQDA
#' }
RQDA <- function() {
  if (!requireNamespace("Rgtk4", quietly = TRUE)) {
    stop("Rgtk4 package is required but not installed")
  }

  library(Rgtk4)
  gtkInit()
  gtkStartEventLoop()
  tryCatch(gSetPrgname("RQDA"), error = function(e) {})
  tryCatch(load_rqda_settings(), error = function(e) {})


  tryCatch(apply_startup_settings(), error = function(e) {})

  if (!exists(".rqda_app", envir = .rqda)) {
    app <- gtkApplicationNew("org.rqda.app", 0L)
    assign(".rqda_app", app, envir = .rqda)
    assign(".rqda_window", NULL, envir = .rqda)

    # Intercept macOS dock Quit - hide window instead of letting GTK destroy it
    # This prevents the event loop from crashing when macOS tears down the app
    gSignalConnectR(app, "shutdown", function(application) {
      tryCatch({
        if (!is.null(.rqda$.rqda_window))
          gtkWidgetSetVisible(.rqda$.rqda_window, FALSE)
        save_rqda_settings()
      }, error = function(e) {})
    })

    gSignalConnectR(app, "activate", function(application) {
      # Dock click while running - bring window to front
      tryCatch({
        gtkForceForeground()
        if (!is.null(.rqda$.rqda_window))
          gtkWindowPresent(.rqda$.rqda_window)
      }, error = function(e) {})

      has_valid_window <- FALSE
      if (!is.null(.rqda$.rqda_window)) {
        has_valid_window <- tryCatch({
          gtkWindowPresent(.rqda$.rqda_window)
          TRUE
        }, error = function(e) FALSE)
      }

      if (!has_valid_window) {
        build_rqda_gui(application)
      }

      # SYNC POINT: Update UI if project is already open via console
      if (is_projOpen(message = FALSE)) {

        # 2. Update Window Title
        gtkWindowSetTitle(.rqda$.rqda_window, sprintf("RQDA - %s", tryCatch(basename(.rqda$qdacon@dbname), error=function(e) "Untitled")))

        # 3. Switch to Files Tab (Index 1)
        if (exists(".nb_rqdagui", envir = .GlobalEnv)) {
          gtkNotebookSetCurrentPage(.nb_rqdagui$widget, 1L)
        }

        # 4. Refresh file list since data is already in DB
        # This ensures the 'Files' table isn't empty when the tab opens
        try(UpdateFileNamesWidget(), silent = TRUE)

        if (TRUE) { # should be a function
          gtkLabelSetMarkup(.rqda$.currentProj$widget, sprintf("<span foreground='#888888' size='small'>%s</span>", rqda_txt("Opening ...")))

          # Update tables
          tryCatch(UpgradeTables(), error = function(e) {
            message("Error upgrading tables: ", e$message)
          })
          tryCatch(CodeNamesUpdate(sortByTime = FALSE), error = function(e) {
            message("Error updating code names: ", e$message)
          })
          tryCatch(FileNamesUpdate(sortByTime = FALSE), error = function(e) {
            message("Error updating file names: ", e$message)
          })

          # Now these widgets exist - can call their updates
          tryCatch(CaseNamesUpdate(), error = function(e) {
            message("Error updating case names: ", e$message)
          })
          tryCatch(UpdateTableWidget(Widget = ".CodeCatWidget", FromdbTable = "codecat"), error = function(e) {
            message("Error updating code categories: ", e$message)
          })
          tryCatch(UpdateCodeofCatWidget(), error = function(e) {
            message("Error updating codes of category: ", e$message)
          })
          tryCatch(UpdateTableWidget(Widget = ".FileCatWidget", FromdbTable = "filecat"), error = function(e) {
            message("Error updating file categories: ", e$message)
          })
          tryCatch(UpdateFileofCatWidget(), error = function(e) {
            message("Error updating files of category: ", e$message)
          })
          tryCatch(AttrNamesUpdate(), error = function(e) {
            message("Error updating attributes: ", e$message)
          })
          tryCatch(JournalNamesUpdate(), error = function(e) {
            message("Error updating journals: ", e$message)
          })

          # Update project path display
          path <- .rqda$qdacon@dbname
          Encoding(path) <- "UTF-8"
          path <- gsub("\\\\", "/", path, fixed = TRUE)
          path <- gsub("/", "/ ", path, fixed = TRUE)

          .proj_path_val <- gsub(
            "/ ", "/",
            paste(strwrap(path, 60), collapse = "\n"),
            fixed = TRUE
          )
          gtkLabelSetMarkup(.rqda$.currentProj$widget,
                            sprintf("<span foreground='#888888' size='small'>%s</span>",
                                    gsub("&","&amp;", .proj_path_val)))

          # Enable UI elements - only if they exist
          if (exists("button", envir = .GlobalEnv)) {
            button_env <- get("button", envir = .GlobalEnv)
            # Only enable widgets that actually exist
            if (exists("cloprob", envir = button_env)) enabled(button_env$cloprob) <- TRUE
            if (exists("BacProjB", envir = button_env)) enabled(button_env$BacProjB) <- TRUE
            if (exists("saveAsB", envir = button_env)) enabled(button_env$saveAsB) <- TRUE
            if (exists("proj_memo", envir = button_env)) enabled(button_env$proj_memo) <- TRUE
            if (exists("CleProB", envir = button_env)) enabled(button_env$CleProB) <- TRUE
            if (exists("CloAllCodB", envir = button_env)) enabled(button_env$CloAllCodB) <- TRUE
            if (exists("ImpFilB", envir = button_env)) enabled(button_env$ImpFilB) <- TRUE
            if (exists("NewFilB", envir = button_env)) enabled(button_env$NewFilB) <- TRUE
            if (exists("AddJouB", envir = button_env)) enabled(button_env$AddJouB) <- TRUE
            if (exists("AddCodB", envir = button_env)) enabled(button_env$AddCodB) <- TRUE
            if (exists("AddCodCatB", envir = button_env)) enabled(button_env$AddCodCatB) <- TRUE
            if (exists("AddCasB", envir = button_env)) enabled(button_env$AddCasB) <- TRUE
            if (exists("AddAttB", envir = button_env)) enabled(button_env$AddAttB) <- TRUE
            if (exists("AddFilCatB", envir = button_env)) enabled(button_env$AddFilCatB) <- TRUE
          }

          # Widgets are always sensitive - no need to enable them explicitly
        }


      }
    })

    gApplicationRegister(app, NULL)
    gApplicationActivate(app)
  } else {
    gApplicationActivate(.rqda$.rqda_app)
  }

  invisible(NULL)
}

#' Build RQDA GUI (called by application activate)
#' @keywords internal
build_rqda_gui <- function(application) {

  # Create application window (not standalone window!)
  # Use plain gtkWindowNew instead of gtkApplicationWindowNew
  # gtkApplicationWindowNew ties the window to GtkApplication lifecycle which
  # causes crashes when macOS sends applicationWillTerminate to the GTK app
  window <- gtkWindowNew()
  gtkWindowSetTitle(window, rqda_txt("RQDA: Qualitative Data Analysis"))

  # GTK4: Use fixed default size since gdkScreenWidth/Height removed
  gtkWindowSetDefaultSize(window, 900L, 650L)
  gtkWidgetSetSizeRequest(window, 600L, 400L)

  # Ensure window appears in taskbar and switchers
  tryCatch({
    gtkWindowSetDecorated(window, TRUE)
    gtkWindowSetDeletable(window, TRUE)
    gtkWindowSetResizable(window, TRUE)
  }, error = function(e) {
    message("Could not set window properties: ", e$message)
  })

  # Store window reference in .rqda environment for later retrieval
  assign(".rqda_window", window, envir = .rqda)

  # Store window reference for global access
  .root_rqdagui <- structure(list(widget = window), class = "gwindow")
  assign(".root_rqdagui", .root_rqdagui, envir = .rqda)

  # Setup window close handler
  gSignalConnectR(window, "close-request", function(w) {
    # Hide window (minimize to dock) - project stays open
    # Call RQDA() again or click dock icon to restore
    gtkWidgetSetVisible(w, FALSE)
    TRUE  # prevent destroy
  })

  # Cmd+Q / Ctrl+Q to quit entirely
  key_ctrl <- gtkEventControllerKeyNew()
  gSignalConnectR(key_ctrl, "key-pressed", function(ctrl, keyval, keycode, state) {
    ch <- tryCatch(rawToChar(as.raw(keyval %% 256L)), error = function(e) "")
    if (ch == "q" || ch == "Q") {
      assign("isLaunched", FALSE, envir = .rqda)
      gtkWidgetSetVisible(window, FALSE)
    }
    FALSE
  })
  gtkWidgetAddController(window, key_ctrl)

  # Setup macOS native features
  setup_native_window(window)
  setup_macos_app(window)
  setup_app_icon(window)

  # Create menubar for macOS
  menubar <- create_macos_menu(window)

  # Create main vertical box to hold menubar and content
  main_vbox <- gtkBoxNew(1L, 4L)  # Vertical, 4px spacing
  gtkWidgetSetMarginStart(main_vbox, 4L)
  gtkWidgetSetMarginEnd(main_vbox, 4L)
  gtkWindowSetChild(window, main_vbox)

  # Add menubar if created (macOS only)
  if (!is.null(menubar)) {
    gtkBoxAppend(main_vbox, menubar)
  }

  # Create notebook for tabs and add to main vbox
  notebook_widget <- gtkNotebookNew()
  # Tab position from settings: top=2, bottom=3, left=0, right=1
  tab_pos_map <- c(top=2L, bottom=3L, left=0L, right=1L)
  nb_tab_pos  <- tab_pos_map[if (!is.null(.rqda$tab.pos) && .rqda$tab.pos %in% names(tab_pos_map))
    .rqda$tab.pos else "right"]
  gtkNotebookSetTabPos(notebook_widget, unname(as.integer(nb_tab_pos)))
  # Fix wiggle on first tab switch: pre-allocate notebook size
  gtkWidgetSetVexpand(notebook_widget, TRUE)
  gtkWidgetSetHexpand(notebook_widget, TRUE)
  gtkBoxAppend(main_vbox, notebook_widget)

  # Homogeneous tabs prevent size changes when switching
  tryCatch(gtkNotebookSetGroupName(notebook_widget, "rqda"), error = function(e) {})
  .nb_rqdagui <- structure(list(widget = notebook_widget), class = "gnotebook")

  # Assign to global for access from button handlers
  assign(".root_rqdagui", .root_rqdagui, envir = .GlobalEnv)
  assign(".nb_rqdagui", .nb_rqdagui, envir = .GlobalEnv)

  #### PROJECT TAB ####
  .proj_gui <- ggroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Project")
  )

  # Project buttons
  NewProjectButton(container = .proj_gui)
  OpenProjectButton(container = .proj_gui)
  CloseProjectButton(container = .proj_gui)

  Proj_MemoButton(
    label = rqda_txt("Project Memo"),
    container = .proj_gui
  )

  BackupProjectButton(container = .proj_gui)

  saveAsButt(
    label = rqda_txt("Save Project As"),
    container = .proj_gui
  )

  CleanProjButton(container = .proj_gui)
  CloseAllCodingsButton(container = .proj_gui)

  gseparator(container = .proj_gui)

  # Warning banner
  warn_lbl <- gtkLabelNew("")
  gtkLabelSetMarkup(warn_lbl, paste0(
    "<span foreground='#C0392B' weight='bold'>",
    "⚠ Pre-alpha / Rgtk4 showcase — not for production use.",
    "</span>\n",
    "<span foreground='#888888' size='small'>",
    "Importing and editing files may corrupt data. Always keep backups.",
    "</span>"))
  gtkLabelSetXalign(warn_lbl, 0.0)
  gtkLabelSetWrap(warn_lbl, TRUE)
  gtkWidgetSetMarginStart(warn_lbl, 6L); gtkWidgetSetMarginEnd(warn_lbl, 6L)
  gtkWidgetSetMarginTop(warn_lbl, 6L); gtkWidgetSetMarginBottom(warn_lbl, 4L)
  gtkBoxAppend(.proj_gui$widget, warn_lbl)

  gseparator(container = .proj_gui)

  # Current project path (gray, low emphasis)
  path_hdr <- gtkLabelNew("")
  gtkLabelSetMarkup(path_hdr, "<span foreground='#888888' size='small'>Current project:</span>")
  gtkLabelSetXalign(path_hdr, 0.0)
  gtkWidgetSetMarginStart(path_hdr, 6L); gtkWidgetSetMarginTop(path_hdr, 4L)
  gtkBoxAppend(.proj_gui$widget, path_hdr)

  .currentProj <- glabel(
    rqda_txt("No project is open."),
    container = .proj_gui
  )
  gtkLabelSetMarkup(.currentProj$widget,
                    "<span foreground='#888888' size='small'>No project is open.</span>")
  gtkWidgetSetMarginStart(.currentProj$widget, 6L)
  gtkWidgetSetMarginBottom(.currentProj$widget, 4L)

  assign(".currentProj", .currentProj, envir = .rqda)

  gseparator(container = .proj_gui)

  .add_info_label <- function(text, handler = NULL) {
    lbl <- glabel(text, container = .proj_gui, handler = handler)
    gtkWidgetSetMarginStart(lbl$widget, 6L)
    lbl
  }

  .add_info_label(rqda_txt("Author: <ronggui.huang@gmail.com>"))

  .add_info_label(
    rqda_txt("Help: click to join rqda-help mailing list"),
    handler = function(h, ...) {
      browseURL(paste0(
        "https://lists.r-forge.r-project.org/cgi-bin/",
        "mailman/listinfo/rqda-help"
      ))
    }
  )

  gseparator(container = .proj_gui)

  .add_info_label(
    rqda_txt("License: BSD"),
    handler = function(h, ...) {
      license_text <- "BSD 3-Clause License\n\nCopyright (c) RQDA Development Team\n\nAll rights reserved."
      win <- gwindow(title = "License", width = 600, height = 400)
      gtext(license_text, container = win)
    }
  )

  version_text <- paste(
    rqda_txt("Version:"),
    packageDescription("RQDA")$Version,
    rqda_txt(" Year:"),
    substr(packageDescription("RQDA")$Date, 1, 4)
  )

  .add_info_label(
    version_text,
    handler = function(h, ...) {
      citation_text <- format(citation("RQDA"), "textVersion")
      win <- gwindow(
        title = rqda_txt("Please cite this package."),
        width = 600, height = 300
      )
      gtext(citation_text, container = win)
    }
  )

  .add_info_label(
    rqda_txt("About"),
    handler = function(h, ...) browseURL("http://rqda.r-forge.r-project.org/")
  )

  #### FILES TAB ####
  .files_pan <- gpanedgroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Files")
  )

  .files_button <- ggroup(
    container = .files_pan,
    horizontal = TRUE
  )

  .fnames_rqda <- gtable(
    character(0),
    container = .files_pan,
    multiple = TRUE,
    handler = function(h, ...) {
      val <- h$obj$value
      if (!is.null(val) && nzchar(val)) assign("selected_file", val, envir=.rqda)
    }
  )
  names(.fnames_rqda) <- rqda_txt("Files")
  tryCatch({ lbl<-gtkLabelNew(""); gtkLabelSetMarkup(lbl,"<span foreground='gray' style='italic'>No files — import or create one</span>"); gtkWidgetSetMarginTop(lbl,16L); gtkListBoxSetPlaceholder(.fnames_rqda$listbox,lbl) }, error=function(e){})

  # Push pane divider to bottom - files tab has only one content area
  tryCatch(gtkPanedSetPosition(.files_pan$widget, 2000L), error=function(e){})
  assign(".files_button", .files_button, envir = .rqda)
  assign(".fnames_rqda", .fnames_rqda, envir = .rqda)

  # Open file on click - use gesture on scrolled window (survives listbox swaps)
  .fnames_open_gesture <- gtkGestureClickNew()
  gSignalConnectR(.fnames_open_gesture, "released", function(g, n_press, x, y) {
    # Only open on double-click (n_press == 2)
    if (n_press < 2L) return()
    sel <- tryCatch({
      lb  <- .rqda$.fnames_rqda$listbox
      row <- gtkListBoxGetSelectedRow(lb)
      if (is.null(row)) return()
      hbox  <- gtkListBoxRowGetChild(row)
      child <- gtkWidgetGetFirstChild(hbox)
      last  <- NULL
      while (!is.null(child)) {
        t <- tryCatch(gtkLabelGetText(child), error = function(e) NULL)
        if (!is.null(t) && nzchar(t)) last <- t
        child <- gtkWidgetGetNextSibling(child)
      }
      last
    }, error = function(e) NULL)
    if (!is.null(sel) && nzchar(sel)) ViewFileFunHelper(sel)
  })
  gtkWidgetAddController(.fnames_rqda$widget, .fnames_open_gesture)

  # File buttons
  ImportFileButton(
    rqda_txt("Import"),
    container = .files_button
  )

  NewFileButton(
    rqda_txt("New"),
    container = .files_button
  )

  DeleteFileButton(
    rqda_txt("Delete"),
    container = .files_button
  )

  ViewFileButton(
    rqda_txt("Open"),
    container = .files_button
  )

  File_MemoButton(
    label = rqda_txt("Memo"),
    container = .files_button,
    FileWidget = .fnames_rqda
  )

  File_RenameButton(
    label = rqda_txt("Rename"),
    container = .files_button,
    FileWidget = .fnames_rqda
  )

  FileAttribute_Button(
    label = rqda_txt("Attribute"),
    container = .files_button,
    FileWidget = .fnames_rqda
  )

  #### CODES TAB ####
  .codes_pan <- gpanedgroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Codes")
  )

  .codes_button <- glayout(container = .codes_pan)

  .codes_rqda <- gtable(
    character(0),
    container = .codes_pan
  )
  names(.codes_rqda) <- rqda_txt("Codes List")
  tryCatch({ lbl<-gtkLabelNew(""); gtkLabelSetMarkup(lbl,"<span foreground='gray' style='italic'>No codes yet</span>"); gtkWidgetSetMarginTop(lbl,16L); gtkListBoxSetPlaceholder(.codes_rqda$listbox,lbl) }, error=function(e){})

  tryCatch(gtkPanedSetPosition(.codes_pan$widget, 2000L), error=function(e){})
  assign(".codes_button", .codes_button, envir = .rqda)
  assign(".codes_rqda", .codes_rqda, envir = .rqda)

  # Code buttons - using layout
  .codes_button[1, 1] <- AddCodeButton()
  .codes_button[1, 2] <- DeleteCodeButton()
  .codes_button[1, 3] <- FreeCode_RenameButton(
    label = rqda_txt("Rename"),
    CodeNamesWidget = .codes_rqda
  )
  .codes_button[1, 4] <- CodeMemoButton(label = rqda_txt("Memo"))

  .codes_button[2, 1] <- AnnotationButton(rqda_txt("Anno"))
  .codes_button[2, 2] <- RetrievalButton(rqda_txt("Coding"))
  .codes_button[2, 3] <- Unmark_Button(name = "UnMarB1")
  .codes_button[2, 4] <- Mark_Button(name = "MarCodB1")

  #### CODE CATEGORIES TAB ####
  # Tab page: vbox with buttons on top, resizable paned below
  .codecat_page <- ggroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Code Categories")
  )

  .codecat_button <- ggroup(container = .codecat_page, horizontal = TRUE)
  gtkWidgetSetMarginTop(.codecat_button$widget, 4L)
  gtkWidgetSetMarginStart(.codecat_button$widget, 2L)

  .codecat_inner <- gtkPanedNew(1L)
  gtkWidgetSetVexpand(.codecat_inner, TRUE)
  gtkPanedSetPosition(.codecat_inner, 180L)
  tryCatch(gtkPanedSetShrinkStartChild(.codecat_inner, FALSE), error = function(e) {})
  tryCatch(gtkPanedSetShrinkEndChild(.codecat_inner, FALSE), error = function(e) {})
  gtkBoxAppend(.codecat_page$widget, .codecat_inner)

  .codecat_top <- gtkBoxNew(1L, 0L)
  gtkWidgetSetVexpand(.codecat_top, TRUE)
  hdr_cc <- gtkLabelNew(rqda_txt("Code Categories"))
  tryCatch(gtkWidgetAddCssClass(hdr_cc, "dim-label"), error = function(e) {})
  gtkWidgetSetMarginTop(hdr_cc, 4L)
  gtkWidgetSetMarginBottom(hdr_cc, 2L)
  gtkBoxAppend(.codecat_top, hdr_cc)

  .CodeCatWidget <- gtable(character(0), multiple = FALSE,
                           handler = function(h, ...) { val <- h$obj$value; if (!is.null(val) && nzchar(val)) assign("selected_codecat", val, envir=.rqda) })
  gtkWidgetSetVexpand(.CodeCatWidget$widget, TRUE)
  gtkBoxAppend(.codecat_top, .CodeCatWidget$widget)
  gtkPanedSetStartChild(.codecat_inner, .codecat_top)

  .codecat_bot <- gtkBoxNew(1L, 0L)
  gtkWidgetSetVexpand(.codecat_bot, TRUE)
  hdr_coc <- gtkLabelNew(rqda_txt("Codes in Category"))
  tryCatch(gtkWidgetAddCssClass(hdr_coc, "dim-label"), error = function(e) {})
  gtkWidgetSetMarginTop(hdr_coc, 4L)
  gtkWidgetSetMarginBottom(hdr_coc, 2L)
  gtkBoxAppend(.codecat_bot, hdr_coc)

  .CodeofCat <- gtable(character(0), multiple = TRUE,
                       handler = function(h, ...) { val <- h$obj$value; if (!is.null(val) && nzchar(val)) assign("selected_codeof_cat", val, envir=.rqda) })
  gtkWidgetSetVexpand(.CodeofCat$widget, TRUE)
  gtkBoxAppend(.codecat_bot, .CodeofCat$widget)
  gtkPanedSetEndChild(.codecat_inner, .codecat_bot)

  assign(".codecat_button", .codecat_button, envir = .rqda)
  assign(".CodeCatWidget", .CodeCatWidget, envir = .rqda)
  assign(".CodeofCat", .CodeofCat, envir = .rqda)

  gSignalConnectR(.CodeCatWidget$listbox, "row-activated", function(box, row) {
    UpdateCodeofCatWidget()
  })

  # Category buttons (from Categories.R)
  AddCodeCatButton(
    label = rqda_txt("Add"),
    container = .codecat_button
  )
  DeleteCodeCatButton(
    label = rqda_txt("Delete"),
    container = .codecat_button
  )
  AddCodeToCatButton(
    label = rqda_txt("Add Code"),
    container = .codecat_button
  )
  DropCodeFromCatButton(
    label = rqda_txt("Drop Code"),
    container = .codecat_button
  )
  CodeCatMemoButton(
    label = rqda_txt("Memo"),
    container = .codecat_button
  )

  #### FILE CATEGORIES TAB ####
  .filecat_page <- ggroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("File Categories")
  )

  .filecat_button <- ggroup(container = .filecat_page, horizontal = TRUE)
  gtkWidgetSetMarginTop(.filecat_button$widget, 4L)
  gtkWidgetSetMarginStart(.filecat_button$widget, 2L)

  .filecat_inner <- gtkPanedNew(1L)
  gtkWidgetSetVexpand(.filecat_inner, TRUE)
  gtkPanedSetPosition(.filecat_inner, 180L)
  tryCatch(gtkPanedSetShrinkStartChild(.filecat_inner, FALSE), error = function(e) {})
  tryCatch(gtkPanedSetShrinkEndChild(.filecat_inner, FALSE), error = function(e) {})
  gtkBoxAppend(.filecat_page$widget, .filecat_inner)

  .filecat_top <- gtkBoxNew(1L, 0L)
  gtkWidgetSetVexpand(.filecat_top, TRUE)
  hdr_fc <- gtkLabelNew(rqda_txt("File Categories"))
  tryCatch(gtkWidgetAddCssClass(hdr_fc, "dim-label"), error = function(e) {})
  gtkWidgetSetMarginTop(hdr_fc, 4L)
  gtkWidgetSetMarginBottom(hdr_fc, 2L)
  gtkBoxAppend(.filecat_top, hdr_fc)

  .FileCatWidget <- gtable(character(0), multiple = FALSE,
                           handler = function(h, ...) { val <- h$obj$value; if (!is.null(val) && nzchar(val)) assign("selected_filecat", val, envir=.rqda) })
  gtkWidgetSetVexpand(.FileCatWidget$widget, TRUE)
  gtkBoxAppend(.filecat_top, .FileCatWidget$widget)
  gtkPanedSetStartChild(.filecat_inner, .filecat_top)

  .filecat_bot <- gtkBoxNew(1L, 0L)
  gtkWidgetSetVexpand(.filecat_bot, TRUE)
  hdr_foc <- gtkLabelNew(rqda_txt("Files in Category"))
  tryCatch(gtkWidgetAddCssClass(hdr_foc, "dim-label"), error = function(e) {})
  gtkWidgetSetMarginTop(hdr_foc, 4L)
  gtkWidgetSetMarginBottom(hdr_foc, 2L)
  gtkBoxAppend(.filecat_bot, hdr_foc)

  .FileofCat <- gtable(character(0), multiple = TRUE,
                       handler = function(h, ...) { val <- h$obj$value; if (!is.null(val) && nzchar(val)) assign("selected_fileof_cat", val, envir=.rqda) })
  gtkWidgetSetVexpand(.FileofCat$widget, TRUE)
  gtkBoxAppend(.filecat_bot, .FileofCat$widget)
  gtkPanedSetEndChild(.filecat_inner, .filecat_bot)

  assign(".filecat_button", .filecat_button, envir = .rqda)
  assign(".FileCatWidget", .FileCatWidget, envir = .rqda)
  assign(".FileofCat", .FileofCat, envir = .rqda)

  gSignalConnectR(.FileCatWidget$listbox, "row-activated", function(box, row) {
    UpdateFileofCatWidget()
  })

  # File category buttons
  AddFileCatButton(
    label = rqda_txt("Add"),
    container = .filecat_button
  )
  DeleteFileCatButton(
    label = rqda_txt("Delete"),
    container = .filecat_button
  )
  AddFileToFileCatButton(
    label = rqda_txt("Add File"),
    container = .filecat_button
  )
  DropFileFromFileCatButton(
    label = rqda_txt("Drop File"),
    container = .filecat_button
  )
  FileCatMemoButton(
    label = rqda_txt("Memo"),
    container = .filecat_button
  )

  #### ATTRIBUTES TAB ####
  .attr_pan <- gpanedgroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Attributes")
  )

  .attr_button <- ggroup(
    container = .attr_pan,
    horizontal = TRUE
  )
  gtkWidgetSetVexpand(.attr_button$widget, FALSE)

  .AttrNamesWidget <- gtable(
    character(0),
    container = .attr_pan,
    multiple = FALSE,
    handler = function(h, ...) {
      val <- h$obj$value
      if (!is.null(val) && nzchar(val)) assign("selected_attr", val, envir=.rqda)
    }
  )
  names(.AttrNamesWidget) <- rqda_txt("Attributes")

  tryCatch(gtkPanedSetPosition(.attr_pan$widget, 2000L), error=function(e){})
  assign(".attr_button", .attr_button, envir = .rqda)
  assign(".AttrNamesWidget", .AttrNamesWidget, envir = .rqda)
  tryCatch({ lbl<-gtkLabelNew(""); gtkLabelSetMarkup(lbl,"<span foreground='gray' style='italic'>No attributes defined</span>"); gtkWidgetSetMarginTop(lbl,16L); gtkListBoxSetPlaceholder(.AttrNamesWidget$listbox,lbl) }, error=function(e){})

  # Right-click on attributes list
  addRightClickMenu(.AttrNamesWidget, list(
    "Set File Values..."  = function() tryCatch(SetFileAttrValues(), error=function(e) message(e$message)),
    "Set Case Values..."  = function() tryCatch(SetCaseAttrValues(), error=function(e) message(e$message))
  ), ".AttrNamesWidget")

  # Attributes tab: Add / Delete / Rename / Memo / Class
  AddAttrButton(label = rqda_txt("Add"),    container = .attr_button)
  DeleteAttrButton(label = rqda_txt("Delete"), container = .attr_button)

  gbutton(rqda_txt("Rename"), container = .attr_button,
          handler = function(h, ...) tryCatch(RenameAttribute(), error=function(e) message(e$message)))
  gbutton(rqda_txt("Memo"), container = .attr_button,
          handler = function(h, ...) tryCatch(AttributeMemo(), error=function(e) message(e$message)))
  gbutton(rqda_txt("Class"), container = .attr_button,
          handler = function(h, ...) tryCatch(ChangeAttrClass(), error=function(e) message(e$message)))

  #### CASES TAB ####
  .cases_pan <- gpanedgroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Cases")
  )

  .cases_button <- ggroup(
    container = .cases_pan,
    horizontal = TRUE
  )

  .CasesNamesWidget <- gtable(
    character(0),
    container = .cases_pan,
    multiple = TRUE,
    handler = function(h, ...) {
      val <- h$obj$value
      if (!is.null(val) && nzchar(val)) assign("selected_case", val, envir=.rqda)
    }
  )
  names(.CasesNamesWidget) <- rqda_txt("Cases")

  # FileofCase - files linked to selected case (second pane)
  .FileofCase <- gtable(
    character(0),
    container = .cases_pan,
    multiple = FALSE,
    handler = function(h, ...) {
      val <- h$obj$value
      if (!is.null(val) && nzchar(val)) assign("selected_fileof_case", val, envir=.rqda)
    }
  )
  names(.FileofCase) <- rqda_txt("Files in Case")
  tryCatch({ lbl<-gtkLabelNew(""); gtkLabelSetMarkup(lbl,"<span foreground='gray' style='italic'>Select a case to see linked files</span>"); gtkWidgetSetMarginTop(lbl,16L); gtkListBoxSetPlaceholder(.FileofCase$listbox,lbl) }, error=function(e){})

  assign(".cases_button", .cases_button, envir = .rqda)
  assign(".CasesNamesWidget", .CasesNamesWidget, envir = .rqda)
  assign(".FileofCase", .FileofCase, envir = .rqda)
  tryCatch({ lbl<-gtkLabelNew(""); gtkLabelSetMarkup(lbl,"<span foreground='gray' style='italic'>No cases yet</span>"); gtkWidgetSetMarginTop(lbl,16L); gtkListBoxSetPlaceholder(.CasesNamesWidget$listbox,lbl) }, error=function(e){})

  # Case buttons - these return buttons that need to be added to container
  add_case_btn <- AddCaseButton(label = rqda_txt("Add"))
  .compact_append <- function(box, widget) {
    gtkWidgetSetVexpand(widget, FALSE)
    gtkWidgetSetValign(widget, 3L)
    gtkBoxAppend(box, widget)
  }

  .compact_append(.cases_button$widget, add_case_btn$widget)

  delete_case_btn <- DeleteCaseButton(label = rqda_txt("Delete"))
  .compact_append(.cases_button$widget, delete_case_btn$widget)

  case_memo_btn <- Case_MemoButton(label = rqda_txt("Memo"))
  .compact_append(.cases_button$widget, case_memo_btn$widget)

  case_addfile_btn <- Case_AddFileButton(label = rqda_txt("Add File"))
  .compact_append(.cases_button$widget, case_addfile_btn$widget)

  case_unlink_btn <- Case_UnlinkButton(label = rqda_txt("Unlink"))
  .compact_append(.cases_button$widget, case_unlink_btn$widget)

  #### JOURNAL TAB ####
  .journal_pan <- gpanedgroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Journals")
  )

  .journal_button <- ggroup(
    container = .journal_pan,
    horizontal = TRUE
  )
  gtkWidgetSetVexpand(.journal_button$widget, FALSE)

  .JournalNamesWidget <- gtable(
    character(0),
    container = .journal_pan,
    multiple = FALSE,
    handler = function(h, ...) {
      val <- h$obj$value
      if (!is.null(val) && nzchar(val)) assign("selected_journal", val, envir=.rqda)
    }
  )
  names(.JournalNamesWidget) <- rqda_txt("Journal Entries")

  tryCatch(gtkPanedSetPosition(.journal_pan$widget, 2000L), error=function(e){})
  assign(".journal_button", .journal_button, envir = .rqda)
  assign(".JournalNamesWidget", .JournalNamesWidget, envir = .rqda)
  tryCatch({ lbl<-gtkLabelNew(""); gtkLabelSetMarkup(lbl,"<span foreground='gray' style='italic'>No journals yet</span>"); gtkWidgetSetMarginTop(lbl,16L); gtkListBoxSetPlaceholder(.JournalNamesWidget$listbox,lbl) }, error=function(e){})

  # Journal buttons (from Journal.R)
  AddJournalButton(
    label = rqda_txt("Add"),
    container = .journal_button
  )
  DeleteJournalButton(
    label = rqda_txt("Delete"),
    container = .journal_button
  )
  OpenJournalButton(
    label = rqda_txt("Open"),
    container = .journal_button
  )

  #### SETTINGS TAB ####
  .setting_vbox <- ggroup(
    container = .nb_rqdagui,
    horizontal = FALSE,
    label = rqda_txt("Settings")
  )

  # Call the new function to populate the tab
  addSettingGUI(.setting_vbox)

  # Use your function to check state silently
  if (is_projOpen(message = FALSE)) {

    # 2. Switch focus to the Files Tab
    # Index 0 = Project, Index 1 = Files
    gtkNotebookSetCurrentPage(notebook_widget, 1L)

    # 3. Update Window Title
    gtkWindowSetTitle(window, sprintf("RQDA - %s", tryCatch(basename(.rqda$qdacon@dbname), error=function(e) "Untitled")))

  } else {
    # No project open: ensure we are on the Project tab
    gtkNotebookSetCurrentPage(notebook_widget, 0L)
  }

  # Force dock icon to appear before showing window (macOS)
  gtkForceForeground()

  # Show window
  gtkWindowPresent(.root_rqdagui$widget)

  # Mark as launched
  assign("isLaunched", TRUE, envir = .rqda)

  tryCatch(AddHandler(), error = function(e) message("AddHandler: ", e$message))

  invisible(.root_rqdagui)
}
