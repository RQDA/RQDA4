# macOS native application support for RQDA

#' Setup macOS-specific application features
#' @keywords internal
setup_macos_app <- function(window) {

  # Only run on macOS
  if (Sys.info()["sysname"] != "Darwin") {
    return(invisible(NULL))
  }

  tryCatch({
    # Set application name for macOS
    gtkWindowSetTitle(window, "RQDA")

    # Try to set dock icon if GTK is built with macOS integration
    # This requires gtk-mac-integration or similar
    if (requireNamespace("RGtk2", quietly = TRUE)) {
      # If we have access to GTK macOS integration
      tryCatch({
        # Set application name in menu bar
        app <- gtkApplicationNew("org.rqda.app", 0L)

        # This would set the dock icon (requires proper bundle)
        # For now, we'll rely on the system's default GTK icon

      }, error = function(e) {
        # Silently fail if GTK macOS integration not available
      })
    }

    # Set window properties for better macOS integration
    gtkWindowSetDecorated(window, TRUE)
    gtkWindowSetResizable(window, TRUE)

    # Make window appear in the application switcher
    # Note: These may not exist in GTK4
    tryCatch({
      gtkWindowSetSkipTaskbarHint(window, FALSE)
    }, error = function(e) {})
  }, error = function(e) {
    warning("Could not setup macOS integration: ", e$message)
  })

  invisible(NULL)
}

#' Create macOS application menu
#' @keywords internal
create_macos_menu <- function(window) {
  if (Sys.info()["sysname"] != "Darwin") return(invisible(NULL))
  if (!exists(".rqda_app", envir = .rqda)) return(invisible(NULL))

  app <- .rqda$.rqda_app

  tryCatch({
    menubar <- gMenuNew()

    # File menu
    file_menu <- gMenuNew()
    gMenuAppend(file_menu, "New Project",    "app.newproj")
    gMenuAppend(file_menu, "Open Project...", "app.openproj")
    gMenuAppend(file_menu, "Close Project",  "app.closeproj")
    gMenuAppend(file_menu, "Save Project As...", "app.saveas")
    gMenuAppendSubmenu(menubar, "File", file_menu)

    # Edit menu
    edit_menu <- gMenuNew()
    gMenuAppend(edit_menu, "New File",    "app.newfile")
    gMenuAppend(edit_menu, "Import File", "app.importfile")
    gMenuAppend(edit_menu, "Add Code",    "app.addcode")
    gMenuAppend(edit_menu, "Add Case",    "app.addcase")
    gMenuAppendSubmenu(menubar, "Edit", edit_menu)

    # View menu
    view_menu <- gMenuNew()
    gMenuAppend(view_menu, "Project",         "app.tab0")
    gMenuAppend(view_menu, "Files",           "app.tab1")
    gMenuAppend(view_menu, "Codes",           "app.tab2")
    gMenuAppend(view_menu, "Code Categories", "app.tab3")
    gMenuAppend(view_menu, "File Categories", "app.tab4")
    gMenuAppend(view_menu, "Attributes",      "app.tab5")
    gMenuAppend(view_menu, "Cases",           "app.tab6")
    gMenuAppend(view_menu, "Journals",        "app.tab7")
    gMenuAppend(view_menu, "Settings",        "app.tab8")
    gMenuAppendSubmenu(menubar, "View", view_menu)

    gtkApplicationSetMenubar(app, menubar)

    # Wire actions
    make_action <- function(name, fn) {
      act <- gSimpleActionNew(name, NULL)
      gSignalConnectR(act, "activate", function(a, p) tryCatch(fn(), error=function(e){}))
      gActionMapAddAction(app, act)
    }

    make_action("newproj",   function() NewProjectButton(container=NULL))
    make_action("openproj",  function() OpenProjectButton(container=NULL))
    make_action("closeproj", function() closeProject(assignenv=.rqda))
    make_action("saveas",    function() tryCatch(saveAsButt(label="", container=NULL), error=function(e){}))
    make_action("newfile",   function() NewFile())
    make_action("importfile",function() ImportFileText())
    make_action("addcode",   function() AddCode())
    make_action("addcase",   function() AddCase())

    for (i in 0:8) {
      local({
        tab_i <- i
        make_action(sprintf("tab%d", tab_i), function() {
          if (exists(".nb_rqdagui", envir=.GlobalEnv))
            gtkNotebookSetCurrentPage(.nb_rqdagui$widget, tab_i)
        })
      })
    }

  }, error = function(e) {
    message("macOS menu setup failed: ", e$message)
  })

  invisible(NULL)
}

#' Setup application icon for macOS
#' @keywords internal
setup_app_icon <- function(window) {

  if (Sys.info()["sysname"] != "Darwin") {
    return(invisible(NULL))
  }

  # Try to set icon from various possible locations
  icon_paths <- c(
    system.file("icons", "rqda.png", package = "RQDA"),
    system.file("icons", "rqda.icns", package = "RQDA"),
    file.path(R.home(), "library", "RQDA", "icons", "rqda.png"),
    "/Applications/RQDA.app/Contents/Resources/rqda.icns"
  )

  for (icon_path in icon_paths) {
    if (file.exists(icon_path)) {
      tryCatch({
        # For PNG files, we can try to load them
        if (grepl("\\.png$", icon_path)) {
          pixbuf <- gdkPixbufNewFromFile(icon_path)
          gtkWindowSetIcon(window, pixbuf)
          message("Loaded icon from: ", icon_path)
          return(invisible(NULL))
        }
      }, error = function(e) {
        # Continue to next path
      })
    }
  }

  # If no icon found, that's OK - system will use default GTK icon
  invisible(NULL)
}

#' Make window behave like native macOS application
#' @keywords internal
setup_native_window <- function(window) {

  if (Sys.info()["sysname"] != "Darwin") {
    return(invisible(NULL))
  }

  tryCatch({
    # Window should be deletable (closeable)
    gtkWindowSetDeletable(window, TRUE)

    # Window should be resizable
    gtkWindowSetResizable(window, TRUE)

    # Set minimum size to prevent too-small windows
    gtkWindowSetDefaultSize(window, 800L, 600L)

    # Enable window controls (minimize, maximize, close)
    gtkWindowSetDecorated(window, TRUE)

    # Window should appear in Exposé/Mission Control
    # Note: These functions may not exist in GTK4
    tryCatch({
      gtkWindowSetSkipTaskbarHint(window, FALSE)
    }, error = function(e) {})

    tryCatch({
      gtkWindowSetSkipPagerHint(window, FALSE)
    }, error = function(e) {})

    # Set window type hint for proper window manager handling
    # gtkWindowSetTypeHint(window, GDK_WINDOW_TYPE_HINT_NORMAL)

    # Handle Cmd+W to close window (in addition to Ctrl+W)
    # GTK on macOS should handle this automatically

  }, error = function(e) {
    warning("Could not setup native window behavior: ", e$message)
  })

  invisible(NULL)
}

#' Setup Cmd key shortcuts for macOS
#' @keywords internal
setup_macos_shortcuts <- function(window) {

  if (Sys.info()["sysname"] != "Darwin") {
    return(invisible(NULL))
  }

  # On macOS, GTK4 should automatically map:
  # Cmd+Q -> Quit
  # Cmd+W -> Close Window
  # Cmd+N -> New (if we set it up)
  # Cmd+O -> Open (if we set it up)

  # For now, GTK4's default handling should work
  # If needed, we can add accelerator groups

  invisible(NULL)
}
