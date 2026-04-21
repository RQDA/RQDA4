# Complete RQDA Menu Conversion for GTK4
# Replace all old gWidgets2 menu builders with GTK4-compatible versions
#
# Usage: Source this file, then use:
#   addRightClickMenu(.rqda$.fnames_rqda, GetFileNamesWidgetMenu())
#   addRightClickMenu(.rqda$.codes_rqda, GetCodesNamesWidgetMenu())
#   etc.

# Helper function for text translation (keeping rqda_txt/gettext pattern)

# ============================================================================
# File Names Widget Menu
# ============================================================================
GetFileNamesWidgetMenu <- function() {
  list(
    "Add New File ..." = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        AddNewFileFun()
      }
    },

    "Add To Case ..." = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        AddFileToCaselinkage()
        UpdateFileofCaseWidget()
      }
    },

    "Add To File Category ..." = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        AddToFileCategory()
        UpdateFileofCatWidget()
      }
    },

    "Add/modify Attributes..." = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        # Use selected file from list or open file viewer
        fname <- tryCatch({
          sel <- as.character(svalue(.rqda$.fnames_rqda))
          sel <- sel[nzchar(sel)]
          if (length(sel) > 0) sel[1]
          else if (exists(".openfile_fid", envir=.rqda)) {
            res <- rqda_sel(sprintf("SELECT name FROM source WHERE id=%d AND status=1",
                                    .rqda$.openfile_fid))
            if (!is.null(res) && nrow(res)>0) res$name[1] else NULL
          } else NULL
        }, error = function(e) NULL)
        if (!is.null(fname)) FileAttrFun(title = fname)
        else gmessage("Select a file first.", icon="warning")
      }
    },

    "View Attributes" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        viewFileAttr()
      }
    },

    "Codings of selected file(s)" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        fid = getFileIds(type = "selected")
        if (length(fid) > 0) {
          getCodingsFromFiles(Fid = fid)
        } else {
          gmessage(rqda_txt("No coded file is selected."))
        }
      }
    },

    "Export File Attributes" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        fName <- gfile(type = "save", filter = list("csv" = list(pattern = c("*.csv"))))
        Encoding(fName) <- "UTF-8"
        if (length(grep(".csv$", fName)) == 0) fName <- sprintf("%s.csv", fName)
        write.csv(getAttr("file"), row.names = FALSE, file = fName, na = "")
      }
    },

    "Edit Selected File" = function() {
      EditFileFun()
    },

    "Export Coded file as HTML" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        path = gfile(type = "save", text = rqda_txt("Type a name for the exported codings and click OK."))
        if (!identical(path, character(0)) && !is.na(path)) {
          Encoding(path) <- "UTF-8"
          path <- sprintf("%s.html", path)
          exportCodedFile(file = path, getFileIds(type = "selected")[1])
        }
      }
    },

    "File Annotations" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        print(getAnnos())
      }
    },

    "File Memo" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        MemoWidget(rqda_txt("File"), .rqda$.fnames_rqda, "source")
      }
    },

    "Import PDF Highlights (selector)" = function() {
      importPDFHL(engine = "rjpod")
    },

    "Import PDF Highlights (path)" = function() {
      fpath = ginput(rqda_txt("Enter a pdf file path"))
      importPDFHL(file = fpath, engine = "rjpod")
    },

    "Open Selected File" = function() {
      ViewFileFun(FileNameWidget = .rqda$.fnames_rqda)
    },

    "Open Previous Coded File" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fname <- rqda_sel(
          paste("select name from source where id in (select fid from",
                "coding where rowid in (select max(rowid) from coding where status = 1))"))$name
        if (length(fname) != 0) fname <- enc(fname, "UTF-8")
        ViewFileFunHelper(FileName = fname)
      }
    },

    "Search for a Word" = function() {
      if (exists(".openfile_gui", envir=.rqda) && !is.null(.rqda$.openfile_gui)) {
        tryCatch({
          if (exists(".viewer_search_show", envir=.rqda) && is.function(.rqda$.viewer_search_show))
            .rqda$.viewer_search_show()
          else {
            gtkWidgetGrabFocus(.rqda$.openfile_gui$textview)
            message("Press f in the file viewer to open search bar")
          }
        }, error=function(e){})
      } else gmessage("Open a file first.", icon="info")
    },

    "Search all files ..." = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        pattern <- if (is.null(.rqda$lastsearch)) "file like '%%'" else .rqda$lastsearch
        pattern <- ginput(rqda_txt("Please input a search pattern."), text = pattern)
        if (!is.null(pattern) && length(pattern) > 0 && !is.na(pattern)) {
          tryCatch(
            searchFiles(pattern, Widget = ".fnames_rqda", is.UTF8 = TRUE),
            error = function(e) message("Error: ", e$message))
          Encoding(pattern) <- "UTF-8"
          assign("lastsearch", pattern, envir = .rqda)
        }
      }
    },

    "Show All Files" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        FileNamesUpdate()
      }
    },

    "Show Coded Files" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = getFileIds(condition = "unconditional", type = "coded"))
      }
    },

    "Show Uncoded Files" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        FileNameWidgetUpdate(FileNamesWidget = .rqda$.fnames_rqda,
                             FileId = getFileIds(condition = "unconditional", type = "uncoded"))
      }
    },

    "Show File Property" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        ShowFileProperty()
      }
    }
  )
}

# ============================================================================
# Codes Names Widget Menu
# ============================================================================
GetCodesNamesWidgetMenu <- function() {
  list(
    "Add To Code Category..." = function() {
      AddToCodeCategory()
    },

    "All Code Memos" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        print(getMemos())
      }
    },

    "All Annotations" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        print(getAnnos())
      }
    },

    "Code Memo" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        MemoWidget(rqda_txt("code"), .rqda$.codes_rqda, "freecode")
      }
    },

    "Codings of Multiple Codes" = function() {
      ct <- getCodingsOfCodes(fid = getFileIds(condition = .rqda$TOR))
      print.codingsByOne(ct)
    },

    "Export Codings as HTML" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        path = gfile(type = "save", text = rqda_txt("Type a name for the exported codings and click OK."))
        if (!identical(path, character(0)) && !is.na(path)) {
          Encoding(path) <- "UTF-8"
          path <- sprintf("%s.html", path)
          fid <- if (.rqda$TOR == "uncondition") NULL else getFileIds(condition = .rqda$TOR)
          exportCodings(file = path, Fid = fid)
        }
      }
    },

    "Highlight All Codings" = function() {
      HL_AllCodings()
    },

    "Highlight Codings with Memo" = function() {
      HL_CodingWithMemo()
    },

    "Merge Selected with..." = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        Selected1 <- tryCatch(as.character(svalue(.rqda$.codes_rqda)), error=function(e) character(0))
        Selected1 <- Selected1[nzchar(Selected1)]
        if (length(Selected1)==0) { gmessage("Select a code first.", icon="error"); return() }
        cid1 <- rqda_sel(sprintf("SELECT id FROM freecode WHERE name='%s' AND status=1",
                                 gsub("'","''",Selected1[1])))
        if (is.null(cid1) || nrow(cid1)==0) return()
        all_codes <- rqda_sel("SELECT name FROM freecode WHERE status=1 ORDER BY name")
        avail <- all_codes$name[all_codes$name != Selected1[1]]
        Selected2 <- gselect_multi(avail, title="Merge into:", message=sprintf("Merge '%s' into:", Selected1[1]))
        if (is.null(Selected2) || length(Selected2)==0) return()
        cid2 <- rqda_sel(sprintf("SELECT id FROM freecode WHERE name='%s' AND status=1",
                                 gsub("'","''",Selected2[1])))
        if (!is.null(cid2) && nrow(cid2)>0)
          mergeCodes(cid2$id[1], cid1$id[1])  # merge Selected1 INTO Selected2
        CodeNamesUpdate(sortByTime=FALSE)
      }
    },

    "Show Codes With Codings" = function() {
      CodeWithCoding(.rqda$TOR)
    },

    "Show Codes Without Codings" = function() {
      CodeWithoutCoding(condition = .rqda$TOR)
    },

    "Show Codes With Category" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        cid <- rqda_sel("select id from freecode where status = 1 and id in (select cid from treecode where status = 1)")
        if (nrow(cid) != 0) {
          CodeNamesWidgetUpdate(CodeNamesWidget = .rqda$.codes_rqda, CodeId = cid[[1]], sortByTime = FALSE)
        } else {
          gmessage(rqda_txt("All codes are assigned to code category."), container = TRUE)
        }
      }
    },

    "Show Codes Without Category" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        cid <- rqda_sel("select id from freecode where status = 1 and id not in (select cid from treecode where status = 1)")
        if (nrow(cid) != 0) {
          CodeNamesWidgetUpdate(CodeNamesWidget = .rqda$.codes_rqda, CodeId = cid[[1]], sortByTime = FALSE)
        } else {
          gmessage(rqda_txt("All codes are assigned to code category."), container = TRUE)
        }
      }
    }
  )
}

# ============================================================================
# Code Category Widget Menu
# ============================================================================
GetCodeCatWidgetMenu <- function() {
  list(
    "Code Category Memo" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        MemoWidget(rqda_txt("Code Category"), .rqda$.CodeCatWidget, "codecat")
      }
    },

    "Export Code Categories" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        path = gfile(type = "save", text = rqda_txt("Type a name and select save."))
        if (!identical(path, character(0)) && !is.na(path)) {
          Encoding(path) <- "UTF-8"
          path <- sprintf("%s.txt", path)
          exportCodeCat(file = path)
        }
      }
    },

    "Import Code Categories" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        file <- gfile(text = rqda_txt("Select a file."))
        Encoding(file) <- "UTF-8"
        importCodeCat(file)
        RQDAQuery("UPDATE codecat SET date = date('now'), dateM = datetime('now'), memo = '', owner = '', status = 1")
        CodeCatUpdate()
      }
    },

    "Plot Selected Code Category" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
        if (length(SelectedCodeCat) > 0) {
          plotCodeCat(SelectedCodeCat)
        }
      }
    }
  )
}

# ============================================================================
# Code of Category Widget Menu
# ============================================================================
GetCodeofCatWidgetMenu <- function() {
  list(
    "Retrieval" = function() {
      retrieval(Fid = getFileIds(condition = .rqda$TOR, type = "coded"),
                CodeNameWidget = .rqda$.CodeofCat)
    }
  )
}

# ============================================================================
# File Category Widget Menu
# ============================================================================
GetFileCatWidgetMenu <- function() {
  list(
    "Export File Categories" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        path = gfile(type = "save", text = rqda_txt("Type a name and select save."))
        if (!identical(path, character(0)) && !is.na(path)) {
          Encoding(path) <- "UTF-8"
          path <- sprintf("%s.txt", path)
          exportFileCat(file = path)
        }
      }
    },

    "File Category Memo" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        MemoWidget(rqda_txt("File Category"), .rqda$.FileCatWidget, "filecat")
      }
    },

    "Import File Categories" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        file <- gfile(text = rqda_txt("Select a file."))
        Encoding(file) <- "UTF-8"
        importFileCat(file)
        RQDAQuery("UPDATE filecat SET date = date('now'), dateM = datetime('now'), memo = '', owner = '', status = 1")
        FileCatUpdate()
      }
    },

    "Plot Selected File Category" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        SelectedFileCat <- svalue(.rqda$.FileCatWidget)
        if (length(SelectedFileCat) > 0) {
          plotFileCat(SelectedFileCat)
        }
      }
    }
  )
}

# ============================================================================
# File of Category Widget Menu
# ============================================================================
GetFileofCatWidgetMenu <- function() {
  list(
    "Open Selected File" = function() {
      ViewFileFun(FileNameWidget = .rqda$.FileofCat)
    }
  )
}

# ============================================================================
# Case Names Widget Menu
# ============================================================================
GetCaseNamesWidgetMenu <- function() {
  list(
    "Attributes of a Selected Case" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        CaseAttrFun()
      }
    },

    "Case Memo" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        MemoWidget(rqda_txt("Case"), .rqda$.CasesNamesWidget, "cases")
      }
    },

    "Export Case Attributes" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        fName <- gfile(type = "save", filter = list("csv" = list(pattern = c("*.csv"))))
        Encoding(fName) <- "UTF-8"
        if (length(grep(".csv$", fName)) == 0) fName <- sprintf("%s.csv", fName)
        write.csv(getAttr("case"), row.names = FALSE, file = fName, na = "")
      }
    },

    "Profiling" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        CrossCases(.type = "profile")
      }
    },

    "Search Cases" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        pattern <- if (is.null(.rqda$lastsearchcases)) "name like '%%'" else .rqda$lastsearchcases
        pattern <- ginput(rqda_txt("Please input search pattern."), text = pattern)
        if (!is.null(pattern) && length(pattern) > 0 && !is.na(pattern)) {
          tryCatch(searchCases(pattern, Widget = ".CasesNamesWidget"),
                   error = function(e) message("Error: ", e$message))
          Encoding(pattern) <- "UTF-8"
          assign("lastsearchcases", pattern, envir = .rqda)
        }
      }
    },

    "Show All Cases" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
        CaseNamesUpdate()
      }
    },

    "View Attributes" = function() {
      if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        viewCaseAttr()
      }
    }
  )
}

# ============================================================================
# File of Case Widget Menu
# ============================================================================
GetFileofCaseWidgetMenu <- function() {
  list(
    "Open Selected File" = function() {
      ViewFileFun(FileNameWidget = .rqda$.FileofCase)
      HL_Case()
      enabled(button$CasUnMarB) <- TRUE
      enabled(button$CasMarB) <- TRUE
    }
  )
}
