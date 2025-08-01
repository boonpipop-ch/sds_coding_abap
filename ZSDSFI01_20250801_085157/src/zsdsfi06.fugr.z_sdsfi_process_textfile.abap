FUNCTION Z_SDSFI_PROCESS_TEXTFILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_IT_TEXT) TYPE  ZSDSFIS033_TT
*"     REFERENCE(IM_WA_INF) TYPE  ZSDSFIS049
*"     REFERENCE(IM_LOGD) TYPE  FLAG OPTIONAL
*"     REFERENCE(IM_PC_DIR) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_SAVE_PC) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(EX_WA_TRANS) TYPE  ZSDSFIS041
*"     REFERENCE(EX_IT_TEXT) TYPE  ZSDSFIS033_TT
*"     REFERENCE(EX_IT_LOG) TYPE  ZSDSFIS071_TT
*"     REFERENCE(EX_IT_BAPIRET2) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA: LT_TEXT    TYPE ZSDSFIS033_TT,
        LT_TAX_LOG TYPE TABLE OF ZSDSFIT019.

  DATA: LWA_INF_PARAM01 TYPE ZSDSFIS049,
        LWA_MEM_INF01   TYPE ZSDSFIS050,
        LWA_RETURN_TF   TYPE ZSDSFIS051,
        LWA_TRANS       TYPE ZSDSFIS041.

  DATA: LV_OK           TYPE FLAG,
        LV_COUNT_PDF_OK TYPE I.

  DATA: LV_FILENAME_ZIP         TYPE STRING,
        LV_FTPPATH_FILENAME_ZIP TYPE STRING,
        LV_REVERSE_FLAG         TYPE C.

  GV_LOGD   = IM_LOGD.
  LT_TEXT[] = IM_IT_TEXT[].

  SORT LT_TEXT BY GJAHR BUKRS SAP_DOC_NO DOCUMENT_NO
                  SAP_DOC_TYPE RD_DOC_TYPE. "ref_type.

*  SORT ex_it_signing_result BY fyear comp sap_doc_no
*                               sap_doc_type doc_type.

*  PERFORM initial_data USING ex_it_signing_result.
  PERFORM GET_FILE_PATH.

*  PERFORM copy_head_fm_inf USING im_it_head_fm_inf
*                        CHANGING lt_head_fm_inf.
*
*  PERFORM check_reverse_doc TABLES lt_bkpf
*                             USING lt_text.

  PERFORM FTP_PDF_ZIP "TABLES lt_bkpf
                      "       lt_pdf_skip
                       USING LT_TEXT
                    CHANGING LV_FILENAME_ZIP
                             LV_FTPPATH_FILENAME_ZIP
                             LV_COUNT_PDF_OK
                             LV_REVERSE_FLAG.

  IF IM_SAVE_PC = ABAP_ON AND
     SY-BATCH IS INITIAL AND
     LV_REVERSE_FLAG = 'N'.   "กรณไม่ส่งกรม

    PERFORM DOWNLOAD_TO_PC USING IM_WA_INF-FULLPATH_SERVER
                                 LV_FILENAME_ZIP
                                 IM_PC_DIR.
  ENDIF.

  IF LV_COUNT_PDF_OK > 0.

    PERFORM FTP_TXT  USING IM_WA_INF-FULLPATH_SERVER
                           IM_WA_INF-EXE_TYPE
                  CHANGING LT_TEXT
                           LWA_MEM_INF01
                           LWA_RETURN_TF.

    IF IM_SAVE_PC = ABAP_ON AND
       SY-BATCH IS INITIAL.
      PERFORM DOWNLOAD_TO_PC USING GS_FILE_PATH-SAP_DIR_ARCH
                                   LWA_MEM_INF01-FILENAME
                                   IM_PC_DIR.
    ENDIF.

*    BREAK-POINT.

*   Generate log data (Internal tables)
    PERFORM GEN_LOG_DATA TABLES LT_TEXT
                                LT_TAX_LOG
                          USING IM_WA_INF
                                LWA_RETURN_TF.

*   Generate transaction data (Internal tables)
    PERFORM GEN_TRANSACTION_DATA TABLES LT_TAX_LOG
                                        LT_TEXT
                                  USING LWA_MEM_INF01
                                        IM_WA_INF
                                        LWA_RETURN_TF
                               CHANGING LWA_TRANS.

    PERFORM CALL_WS_SIGN_DOCUMENT_ZIP USING LWA_MEM_INF01
                                            LV_FILENAME_ZIP
                                            LV_FTPPATH_FILENAME_ZIP
                                   CHANGING LT_TEXT
                                            LWA_RETURN_TF.

*    PERFORM update_lt_trans_itm USING lt_text
*                             CHANGING lt_trans_itm.

*   Update status (Internal tables)
    PERFORM UPDATE_STATUS TABLES LT_TAX_LOG
                                 LT_TEXT
                           USING LWA_RETURN_TF
                        CHANGING LWA_TRANS.
    "lt_pdf_skip.

*   Update log (ZTable)
    PERFORM UPDATE_LOG TABLES LT_TAX_LOG
                              EX_IT_BAPIRET2
                        USING LWA_TRANS.

  ENDIF.

* Move PDF file from OUT to ARCHIVE (if any)
  PERFORM ARCHIVE_PDF_FILES USING LT_TEXT.

  EX_WA_TRANS      = LWA_TRANS.
  EX_IT_LOG        = LT_TAX_LOG[].
*  ex_it_log_result = gth_log_result[].
  EX_IT_TEXT[]     = LT_TEXT[].





ENDFUNCTION.
