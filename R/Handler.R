AddHandler <- function() {
  ## add handler function for GUIs

  gSignalConnectR(.rqda$.root_rqdagui$widget, "close-request", function(window) {
    val <- gconfirm(
      rqda_txt(paste("Really EXIT?\n\n",
                     "You can use RQDA() to start this program again.")),
      parent = window)
    if (as.logical(val)) {
      assign("isLaunched", FALSE, envir = .rqda)
      return(FALSE)  # Allow close
    } else {
      return(TRUE)   # Prevent close
    }
  })

  ## handler for .fnames_rqda (gtable holding the file names)
  addHandlerClicked(.rqda$.fnames_rqda, handler = function(h, ...) {
    if (isTRUE(.rqda$SFP))
      ShowFileProperty(focus = FALSE)

    Fid <- getFileIds(type = "selected")
    if (!is.null(Fid) && length(Fid) == 1) {
      names(.rqda$.fnames_rqda) <- sprintf(
        rqda_txt("Selected File id is %s"), Fid)
      # buttons enabled via GTK4 - no legacy button refs needed
      ## dynamically change the label of attribute(s)
      if ((nattr <- length(.rqda$.AttrNamesWidget[])) !=  0) {
        tryCatch(enabled(button$FileAttrB) <- TRUE, error=function(e){})
        if (length(tryCatch(.rqda$selected_attr %||% character(0), error=function(e) character(0))) > 1 || nattr > 1) {
          tryCatch(svalue(button$FileAttrB) <- rqda_txt("Attributes"), error=function(e){})
        }
      }
    }
  })

  ## right click to add file to a case category
  addRightClickMenu(.rqda$.fnames_rqda, GetFileNamesWidgetMenu(), ".fnames_rqda")

  addHandlerDoubleclick(.rqda$.fnames_rqda, handler = function(h, ...) {
    tryCatch(ViewFileFunHelper(svalue(.rqda$.fnames_rqda)[1]), error=function(e){})
  })

  ## handler for .codes_rqda
  addHandlerDoubleclick(.rqda$.codes_rqda, handler = function(h, ...) {
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
      if (length(Fid <- getFileIds(condition = .rqda$TOR, type = "coded")) > 0) {
        retrieval(Fid = Fid, CodeNameWidget = .rqda$.codes_rqda)
      } else {
        gmessage(
          rqda_txt("No coding associated with this code."),
          container = TRUE)
      }
    }
  })

  addRightClickMenu(.rqda$.codes_rqda, GetCodesNamesWidgetMenu(), ".codes_rqda")

  addHandlerClicked(.rqda$.codes_rqda, handler = function(h, ...) {
    invisible(NULL)
    if (length(svalue(.rqda$.codes_rqda)) == 1) {
      tryCatch(enabled(button$RetB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$DelCodB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$codememobuton) <- TRUE, error=function(e){})
      tryCatch(enabled(button$FreCodRenB) <- TRUE, error=function(e){})
    }
  })

  ## handler for .CodeofCat
  addHandlerClicked(.rqda$.CodeofCat, handler = function(h, ...) {
    invisible(NULL)
    if (length(tryCatch(.rqda$selected_codeof_cat %||% character(0), error=function(e) character(0))) > 0) {
      tryCatch(enabled(button$CodCatADroFromB) <- TRUE, error=function(e){})
    }
  })

  addHandlerDoubleclick(.rqda$.CasesNamesWidget, handler = function(h, ...) {
    MemoWidget(rqda_txt("Case"),
               .rqda$.CasesNamesWidget, "cases")
  })

  addHandlerClicked(.rqda$.CasesNamesWidget, handler = function(h, ...) {

    SelectedCase <- tryCatch(.rqda$selected_case %||% character(0), error=function(e) character(0))

    if (length(SelectedCase) !=  0) {
      tryCatch(enabled(button$DelCasB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$CasRenB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$profmatB) <- TRUE, error=function(e){})
      if ((nattr <- length(.rqda$.AttrNamesWidget[])) !=  0) {
        tryCatch(enabled(button$CasAttrB) <- TRUE, error=function(e){})
        if (length(tryCatch(.rqda$selected_attr %||% character(0), error=function(e) character(0))) > 1 || nattr > 1) {
          tryCatch(svalue(button$CasAttrB) <- rqda_txt("Attributes"), error=function(e){})
        }
      }
      enabled(.rqda$.FileofCase) <- TRUE
      enabled(button$CasMarB) <-
        (exists(".root_edit", envir = .rqda) && tryCatch(!is.null(.rqda$.root_edit), error=function(e) FALSE))
      Encoding(SelectedCase) <- "UTF-8"
      currentCid <- rqda_sel(sprintf("select id from cases where name = '%s'",
                                     enc(SelectedCase)))[, 1]

      freq <- rqda_sel(
        sprintf(paste("select count(distinct fid) as freq from caselinkage",
                      "where status = 1 and caseid = %s"), currentCid))$freq
      names(.rqda$.CasesNamesWidget) <- sprintf(
        rqda_txt("Selected case id is %i__%i files"),
        currentCid, freq)

      if (exists(".root_edit", envir = .rqda) && tryCatch(!is.null(.rqda$.root_edit), error=function(e) FALSE)) {
        SelectedFile <- tryCatch(.rqda$selected_file %||% character(0), error=function(e) character(0))
        Encoding(SelectedFile) <- "UTF-8"
        currentFid <- rqda_sel(sprintf(
          "select id from source where name = '%s'",
          enc(SelectedFile)))[, 1]

        ## following code: Only mark the text chuck according to
        ## the current code.
        coding.idx <- rqda_sel(
          sprintf(paste("select selfirst, selend from coding where",
                        "fid = %i and status = 1"), currentFid))
        anno.idx <- rqda_sel(
          sprintf(paste("select position from annotation where",
                        "fid = %i and status = 1"), currentFid))$position
        allidx <- c(unlist(coding.idx), anno.idx)

        sel_index <- rqda_sel(
          sprintf(paste("select selfirst, selend from caselinkage where",
                        "caseid = %i and fid = %i and status = 1"),
                  currentCid, currentFid))

        Maxindex <- rqda_sel(
          sprintf("select max(selend) from caselinkage where fid = %i",
                  currentFid))[1, 1]

        if (!is.null(allidx) && length(allidx) > 0)
          Maxindex <- Maxindex + sum(allidx <= Maxindex)

        tryCatch(ClearMark(.rqda$.openfile_gui, min=0, max=Maxindex, clear.fore.col=FALSE, clear.back.col=TRUE), error=function(e){})

        if (nrow(sel_index) > 0) {
          if (!is.null(allidx)) {
            sel_index[, "selfirst"] <- sapply(
              sel_index[, "selfirst"], FUN = function(x) {
                x + sum(allidx <= x)
              })
            sel_index[, "selend"] <- sapply(
              sel_index[, "selend"], FUN = function(x) {
                x + sum(allidx <= x)
              })
          }
          tryCatch(HL(.rqda$.openfile_gui, index=sel_index, fore.col=NULL, back.col=.rqda$back.col), error=function(e){})

          enabled(button$CasUnMarB) <-
            (exists(".root_edit", envir = .rqda) && tryCatch(!is.null(.rqda$.root_edit), error=function(e) FALSE))
          ## end of mark text chuck
        }}

      UpdateFileofCaseWidget()
    }
  })

  addHandlerClicked(.rqda$.CodeCatWidget, handler = function(h, ...) {

    Selected <- tryCatch(.rqda$selected_codecat %||% character(0), error=function(e) character(0))
    if (identical(Selected, character(0))) {
      return(invisible(NULL))
    }

    if ((ncc <- length(Selected)) != 0) {
      enabled(.rqda$.CodeofCat)    <- TRUE
      tryCatch(enabled(button$DelCodCatB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$CodCatMemB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$CodCatRenB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$CodCatAddToB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$MarCodB2) <- FALSE, error=function(e){})
      tryCatch(enabled(button$UnMarB2) <- FALSE, error=function(e){})

      # obtain one or more category ids
      sql <- paste0("select catid from codecat where name in ('",
                    paste(enc(Selected), collapse = "', '"), "')")

      catid <- rqda_sel(sql)$catid

      if (!is.null(catid) && length(catid) == 1) {
        names(.rqda$.CodeCatWidget) <- sprintf(
          rqda_txt("Selected category id is %s"), catid)

        UpdateCodeofCatWidget(con = .rqda$qdacon, Widget = .rqda$.CodeofCat)
      }

    }


    ## if (ncc > 1) {
    ##     psccItem <- CodeCatWidgetMenu$"Plot Selected Code Category"
    ##     svalue(psccItem) <- "Plot Selected Code Categories"
    ## }

  })

  addHandlerDoubleclick(.rqda$.AttrNamesWidget, handler = function(h, ...) {
    MemoWidget(rqda_txt("Attributes"), .rqda$.AttrNamesWidget, "attributes")
  })

  addHandlerClicked(.rqda$.AttrNamesWidget, handler = function(h, ...) {
    if (length(tryCatch(.rqda$selected_attr %||% character(0), error=function(e) character(0))) !=  0) {
      tryCatch(enabled(button$DelAttB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$RenAttB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$AttMemB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$SetAttClsB) <- TRUE, error=function(e){})
      if (length(tryCatch(.rqda$selected_attr %||% character(0), error=function(e) character(0))) > 1) {
        tryCatch({ svalue(button$CasAttrB) <- rqda_txt("Attributes"); svalue(button$FileAttrB) <- rqda_txt("Attributes") }, error=function(e){})
      } else {
        tryCatch({ svalue(button$CasAttrB) <- rqda_txt("Attribute");  svalue(button$FileAttrB) <- rqda_txt("Attribute")  }, error=function(e){})
      }
    }
  })

  addHandlerDoubleclick(.rqda$.CodeCatWidget, handler = function(h, ...) {
    MemoWidget(rqda_txt("Code Category"), .rqda$.CodeCatWidget, "codecat")
  })

  addRightClickMenu(.rqda$.CodeCatWidget, GetCodeCatWidgetMenu(), ".CodeCatWidget")

  addHandlerDoubleclick(.rqda$.CodeofCat, handler = function(h, ...) {
    retrieval(Fid = getFileIds(condition = .rqda$TOR, type = "coded"),
              CodeNameWidget = .rqda$.CodeofCat)
  })

  addRightClickMenu(.rqda$.CodeofCat, GetCodeofCatWidgetMenu(), ".CodeofCat")

  addHandlerClicked(.rqda$.FileCatWidget, handler = function(h, ...) {

    if (length(tryCatch(.rqda$selected_filecat %||% character(0), error=function(e) character(0))) > 0) {

      UpdateFileofCatWidget(con = .rqda$qdacon, Widget = .rqda$.FileofCat)

      tryCatch(enabled(button$DelFilCatB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$FilCatRenB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$FilCatMemB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$FilCatAddToB) <- TRUE, error=function(e){})
      enabled(.rqda$.FileofCat) <- TRUE
    }})

  addHandlerDoubleclick(.rqda$.FileCatWidget, handler = function(h, ...) {
    MemoWidget(rqda_txt("File Category"), .rqda$.FileCatWidget, "filecat")
  })

  addRightClickMenu(.rqda$.FileCatWidget, GetFileCatWidgetMenu(), ".FileCatWidget")

  addHandlerDoubleclick(.rqda$.FileofCat, handler = function(h, ...) {
    tryCatch(ViewFileFunHelper(tryCatch(.rqda$selected_fileof_cat %||% character(0), error=function(e) character(0))[1]), error=function(e){})
  })

  addHandlerClicked(.rqda$.FileofCat, handler = function(h, ...) {
    if (length(tryCatch(.rqda$selected_fileof_cat %||% character(0), error=function(e) character(0))) > 0) {
      tryCatch(enabled(button$FilCatDroFromB) <- TRUE, error=function(e){})
      names(.rqda$.FileofCat) <- sprintf(rqda_txt("Selected file id is %s"),
                                         getFileIds("filecat", "selected"))
      if (isTRUE(.rqda$SFP)) {
        ShowFileProperty(Fid = getFileIds("file", "selected"), focus = FALSE)
      }
    }
  })

  addRightClickMenu(.rqda$.FileofCat, GetFileofCatWidgetMenu(), ".FileofCat")

  addRightClickMenu(.rqda$.CasesNamesWidget, GetCaseNamesWidgetMenu(), ".CasesNamesWidget")
  ## popup menu by right-click on CaseNamesWidget

  addRightClickMenu(.rqda$.FileofCase, GetFileofCaseWidgetMenu(), ".FileofCase")

  addHandlerDoubleclick(.rqda$.FileofCase, handler = function(h, ...) {
    tryCatch(ViewFileFunHelper(tryCatch(.rqda$selected_fileof_case %||% character(0), error=function(e) character(0))[1]), error=function(e){})
    tryCatch(HL_Case(), error=function(e){})
    tryCatch(enabled(button$CasUnMarB) <- TRUE, error=function(e){})
    tryCatch(enabled(button$CasMarB) <- TRUE, error=function(e){})
  })

  addHandlerClicked(.rqda$.FileofCase, handler = function(h, ...) {
    if (length(tryCatch(.rqda$selected_fileof_case %||% character(0), error=function(e) character(0))) > 0) {
      names(.rqda$.FileofCase) <- sprintf(rqda_txt("Selected File id is %s"),
                                          getFileIds("case", "selected"))
    }
    if (isTRUE(.rqda$SFP))
      ShowFileProperty(Fid = getFileIds("case", "selected"), focus = FALSE)
  })

  addHandlerDoubleclick(.rqda$.JournalNamesWidget, handler = function(h, ...) {
    OpenJournal()
  })

  addHandlerClicked(.rqda$.JournalNamesWidget, handler = function(h, ...) {
    if (length(svalue(.rqda$.JournalNamesWidget)) !=  0) {
      tryCatch(enabled(button$DelJouB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$RenJouB) <- TRUE, error=function(e){})
      tryCatch(enabled(button$OpeJouB) <- TRUE, error=function(e){})
    }
  })

} ## end of AddHandler()
