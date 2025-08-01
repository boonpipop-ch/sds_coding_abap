*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0790_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_UTIL DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      CONVERT_ALPHA_IN  IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      CONVERT_ALPHA_OUT IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY.

ENDCLASS.
CLASS LCL_UTIL IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_IN.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_OUT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_outPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
ENDCLASS.
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR,
      START_PROCESS.
    CLASS-METHODS :
      GET_DATA,
      GET_ADDTIONAL_DATA,
      SHOW_REPORT,
      SET_LAYOUT_OUTPUT,
      BUILD_FCAT,
      SET_SORT,
      SET_ALV_GRID,
      HTML_TOP_OF_PAGE,
      SELECT_INPUT_FILE_NAME.
    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD GET_DATA.
    IF LO IS INITIAL.
      CREATE OBJECT LO.
    ENDIF.

    LO->START_PROCESS( ).
  ENDMETHOD.
  METHOD START_PROCESS.
    DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    DATA : LV_WINDOW TYPE STRING.

    DATA : LV_STRING TYPE STRING,
           LT_STRING TYPE TABLE OF STRING.

    DATA : LV_LENGTH LIKE SY-TABIX.

    DATA : LV_FILENAME TYPE STRING.

    DATA : BEGIN OF LS_UPLOAD,
             FIELD(255),
           END OF LS_UPLOAD.
    DATA LT_UPLOAD LIKE TABLE OF LS_UPLOAD.

    DATA LT_TMP LIKE LT_UPLOAD.

    IF LCL_FTP IS NOT BOUND.
      CREATE OBJECT LCL_FTP.
    ENDIF.

    DATA : LV_STATUS TYPE C.

    DATA : LV_LINE TYPE I.

    DATA : LV_NAME TYPE STRING.

    DATA : LV_TEXT TYPE C LENGTH 255.

    DATA : LR_BUDAT TYPE RANGE OF BKPF-BUDAT,
           LS_BUDAT LIKE LINE OF LR_BUDAT.

    DATA : LV_year TYPE C LENGTH 4.

    DATA : LV_YEAR_C TYPE C LENGTH 4.

    LV_YEAR = P_GJAHR + 1.

    DO 12 TIMES.
      IF SY-INDEX EQ 1.
        IF CB_M1 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR LS_BUDAT.
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }0401|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }0430|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 2.
        IF CB_M2 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }0501|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }0531|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 3.
        IF CB_M3 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }0601|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }0630|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 4.
        IF CB_M4 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }0701|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }0731|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 5.
        IF CB_M5 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }0801|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }0831|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 6.
        IF CB_M6 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }0901|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }0930|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 7.
        IF CB_M7 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }1001|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }1031|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 8.
        IF CB_M8 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }1101|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }1130|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 9.
        IF CB_M9 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ P_GJAHR }1201|.
        LS_BUDAT-HIGH   = |{ P_GJAHR }1231|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 10.
        IF CB_M10 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ LV_YEAR }0101|.
        LS_BUDAT-HIGH   = |{ LV_YEAR }0131|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 11.
        IF CB_M11 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ LV_YEAR }0201|.

        CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
          EXPORTING
            DAY_IN            = LS_BUDAT-LOW
          IMPORTING
            LAST_DAY_OF_MONTH = LS_BUDAT-HIGH
          EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.
        IF SY-SUBRC <> 0.
        ENDIF.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ELSEIF SY-INDEX EQ 12.
        IF CB_M12 NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
        CLEAR : LS_BUDAT,LR_BUDAT[].
        LS_BUDAT-SIGN   = 'I'.
        LS_BUDAT-OPTION = 'BT'.
        LS_BUDAT-LOW    = |{ LV_YEAR }0301|.
        LS_BUDAT-HIGH   = |{ LV_YEAR }0331|.
        LV_YEAR_C       = LS_BUDAT-LOW.
        APPEND LS_BUDAT TO LR_BUDAT.
      ENDIF.

      CLEAR : LT_TMP[],LT_UPLOAD[].
      CONCATENATE
      '"' 'Document No.'
      '","' 'Line Item'
      '","' 'Document Type'
      '","' 'Reference'
      '","' 'Posting Date'
      '","' 'Document Date'
      '","' 'Amount'
      '","' 'Clear Doc'
      '","' 'Assignment Number'
      '","' 'G/L'
      '","' 'Profit Center'
      '","' 'CREATED By'
      '","' 'CREATED Date'
      '","' 'Text Line Item'
      '"'
           INTO LS_UPLOAD-FIELD.
      APPEND LS_UPLOAD TO LT_TMP.

*      IF P_RLDNR EQ '0L'.
*        SELECT BUKRS
*               BELNR
*               GJAHR
*               WAERS
*               BLART
*               XBLNR
*               BUDAT
*               BLDAT
*               USNAM
*               CPUDT
*          FROM BKPF
*          INTO GS_HEADER
*          WHERE BUDAT IN LR_BUDAT[]
*            AND BSTAT NE 'S'.
*
*          SELECT BSEG~BUKRS
*                 BSEG~BELNR
*                 BSEG~GJAHR
*                 BSEG~BUZEI
*                 BSEG~SHKZG
*                 BSEG~DMBTR
*                 BSEG~AUGBL
*                 BSEG~SGTXT
*                 BSEG~HKONT
*                 BSEG~ZUONR
*            FROM BSEG
*            INTO CORRESPONDING FIELDS OF GS_RESULT
*            WHERE BUKRS EQ GS_HEADER-BUKRS
*              AND BELNR EQ GS_HEADER-BELNR
*              AND GJAHR EQ GS_HEADER-GJAHR.
*
*            LV_TEXT = GS_RESULT-DMBTR.
*            REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_TEXT WITH ''.
*            IF GS_RESULT-SHKZG EQ 'H'.
*              CONCATENATE '-' LV_TEXT INTO LV_TEXT.
*            ENDIF.
*
*            REPLACE ALL OCCURRENCES OF '-' IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF '|' IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF ',' IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF ';' IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF '''' IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF '"' IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF IN GS_RESULT-SGTXT WITH ''.
*            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN GS_RESULT-SGTXT WITH ''.
*
*            CLEAR : LS_UPLOAD.
*            CONCATENATE '"'
*                  GS_HEADER-BELNR
*            '","' GS_RESULT-BUZEI
*            '","' GS_HEADER-BLART
*            '","' GS_HEADER-XBLNR
*            '","' GS_HEADER-BUDAT
*            '","' GS_HEADER-BLDAT
*            '","' LV_TEXT
*            '","' GS_RESULT-AUGBL
*            '","' GS_RESULT-ZUONR
*            '","' GS_RESULT-HKONT
*            '","' GS_HEADER-USNAM
*            '","' GS_HEADER-CPUDT
*            '","' GS_RESULT-SGTXT
*            '"'
*                 INTO LS_UPLOAD-FIELD.
*            APPEND LS_UPLOAD TO LT_TMP.
*          ENDSELECT.
*        ENDSELECT.
*      ELSE.
        SELECT ACDOCA~RBUKRS AS BUKRS,
               ACDOCA~BELNR,
               ACDOCA~GJAHR,
               ACDOCA~BUZEI,
               ACDOCA~DRCRK AS SHKZG,
               ACDOCA~HSL AS DMBTR ,
               ACDOCA~AUGBL,
               ACDOCA~SGTXT,
               ACDOCA~RACCT AS HKONT,
               ACDOCA~ZUONR,
               ACDOCA~PRCTR
          FROM ACDOCA
          INTO CORRESPONDING FIELDS OF @GS_RESULT
          WHERE BUDAT IN @LR_BUDAT[]
            AND RLDNR EQ @P_RLDNR.

          SELECT BUKRS
                 BELNR
                 GJAHR
                 WAERS
                 BLART
                 XBLNR
                 BUDAT
                 BLDAT
                 USNAM
                 CPUDT
            FROM BKPF
            INTO GS_HEADER
            WHERE BUKRS EQ GS_RESULT-BUKRS
              AND BELNR EQ GS_RESULT-BELNR
              AND GJAHR EQ GS_RESULT-GJAHR
              AND BSTAT NE 'S'.
            IF SY-SUBRC EQ 0.
              LV_TEXT = GS_RESULT-DMBTR.
              REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_TEXT WITH ''.
              IF GS_RESULT-SHKZG EQ 'H'.
                REPLACE ALL OCCURRENCES OF PCRE '-'    IN LV_TEXT WITH ''.
                REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_TEXT WITH ''.
                CONCATENATE '-' LV_TEXT INTO LV_TEXT.
              ENDIF.

              REPLACE ALL OCCURRENCES OF '-' IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF '|' IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF ',' IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF ';' IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF '''' IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF '"' IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF IN GS_RESULT-SGTXT WITH ''.
              REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN GS_RESULT-SGTXT WITH ''.

              CLEAR : LS_UPLOAD.
              CONCATENATE '"'
                    GS_HEADER-BELNR
              '","' GS_RESULT-BUZEI
              '","' GS_HEADER-BLART
              '","' GS_HEADER-XBLNR
              '","' GS_HEADER-BUDAT
              '","' GS_HEADER-BLDAT
              '","' LV_TEXT
              '","' GS_RESULT-AUGBL
              '","' GS_RESULT-ZUONR
              '","' GS_RESULT-HKONT
              '","' GS_RESULT-PRCTR
              '","' GS_HEADER-USNAM
              '","' GS_HEADER-CPUDT
              '","' GS_RESULT-SGTXT
              '"'
                   INTO LS_UPLOAD-FIELD.
              APPEND LS_UPLOAD TO LT_TMP.
            ENDIF.
          ENDSELECT.
        ENDSELECT.

*      ENDIF.


      LOOP AT LT_TMP INTO LS_UPLOAD.
        APPEND LS_UPLOAD TO LT_UPLOAD.
        ADD 1 TO LV_LINE.
        IF SY-INDEX EQ 1.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '04' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 2.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '05' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 3.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '06' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 4.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '07' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 5.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '08' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 6.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '09' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 7.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '10' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 8.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '11' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 9.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '12' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 10.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '01' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 11.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '02' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 12.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '03' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ENDIF.
        IF LV_LINE EQ 9999999.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              FILENAME                = LV_FILENAME
              FILETYPE                = 'ASC'
              CODEPAGE                = '4103'
            IMPORTING
              FILELENGTH              = LV_LENGTH
            TABLES
              DATA_TAB                = LT_UPLOAD
            EXCEPTIONS
              FILE_OPEN_ERROR         = 1
              FILE_READ_ERROR         = 2
              NO_BATCH                = 3
              GUI_REFUSE_FILETRANSFER = 4
              INVALID_TYPE            = 5
              NO_AUTHORITY            = 6
              UNKNOWN_ERROR           = 7
              BAD_DATA_FORMAT         = 8
              HEADER_NOT_ALLOWED      = 9
              SEPARATOR_NOT_ALLOWED   = 10
              HEADER_TOO_LONG         = 11
              UNKNOWN_DP_ERROR        = 12
              ACCESS_DENIED           = 13
              DP_OUT_OF_MEMORY        = 14
              DISK_FULL               = 15
              DP_TIMEOUT              = 16
              OTHERS                  = 17.
          CLEAR : LV_LINE,LT_UPLOAD.
        ENDIF.
      ENDLOOP.
      IF LV_LINE NE 0 AND
         LV_LINE LT 9999999.
        IF SY-INDEX EQ 1.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '04' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 2.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '05' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 3.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '06' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 4.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '07' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 5.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '08' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 6.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '09' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 7.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '10' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 8.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '11' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 9.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '12' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 10.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '01' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 11.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '02' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ELSEIF SY-INDEX EQ 12.
          CONCATENATE P_FILE '\' 'DATA_GL' LV_YEAR_C '03' SY-DATUM SY-UZEIT '.txt' INTO LV_FILENAME.
        ENDIF.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME                = LV_FILENAME
            FILETYPE                = 'ASC'
            CODEPAGE                = '4103'
          IMPORTING
            FILELENGTH              = LV_LENGTH
          TABLES
            DATA_TAB                = LT_UPLOAD
          EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            OTHERS                  = 17.

      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.

    ENDLOOP.
  ENDMETHOD.
  METHOD SHOW_REPORT.
    SET_LAYOUT_OUTPUT( ).
    BUILD_FCAT( ).
    SET_SORT( ).
    SET_ALV_GRID( ).
  ENDMETHOD.
  METHOD SET_LAYOUT_OUTPUT.
*    CONSTANTS : BEGIN OF LC_CON,
*                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
*                END OF LC_CON.
    GS_LAYOUT-ZEBRA             = GC_X.
    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
*    GS_LAYOUT-BOX_FIELDNAME     = LC_CON-CHK_FILED.
  ENDMETHOD.
  METHOD BUILD_FCAT.
    DATA:
       LS_FCAT TYPE SLIS_FIELDCAT_ALV.

*    CONSTANTS : BEGIN OF LC_CON,
*                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
*                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
*                END OF LC_CON.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = LC_CON-CHK_FILED.
*    LS_FCAT-SELTEXT_S   = LC_CON-CHK_NAME.
*    LS_FCAT-SELTEXT_M   = LC_CON-CHK_NAME.
*    LS_FCAT-SELTEXT_L   = LC_CON-CHK_FILED.
*    LS_FCAT-CHECKBOX    = ABAP_TRUE.
*    LS_FCAT-INPUT       = ABAP_TRUE.
*    LS_FCAT-EDIT        = ABAP_TRUE.
*    APPEND LS_FCAT TO GT_FCAT.

    DATA : LV_RUNNING  TYPE I,
           LV_DATA     TYPE C LENGTH 6 VALUE 'TEXT-',
           LV_RUN_TEXT TYPE C LENGTH 2.

    CONSTANTS : LC_F TYPE C VALUE 'F',
                LC_T TYPE C VALUE 'T',
                LC_d TYPE C VALUE 'D'.

    FIELD-SYMBOLS <LFS> TYPE ANY.

    DATA : LV_TEXT TYPE C LENGTH 8.
*Field
    CLEAR : LS_FCAT.
    DO 99 TIMES.
      ADD 1 TO LV_RUNNING.
      LV_RUN_TEXT = LV_RUNNING.

      LCL_UTIL=>CONVERT_ALPHA_IN( EXPORTING I_DATA = LV_RUN_TEXT
                                  IMPORTING E_Data = LV_RUN_TEXT ).

      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_F LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS NOT ASSIGNED.
        EXIT.
      ENDIF.
      LS_FCAT-FIELDNAME = <LFS>.
*Teble Ref
      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_T LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS ASSIGNED.
        LS_FCAT-REF_TABNAME = <LFS>.
      ENDIF.
*Description
      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_D LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS ASSIGNED.
        LS_FCAT-SELTEXT_S = <LFS>.
        LS_FCAT-SELTEXT_M = <LFS>.
        LS_FCAT-SELTEXT_L = <LFS>.
      ENDIF.
      APPEND LS_FCAT TO GT_FCAT.
      CLEAR LS_FCAT.
    ENDDO.

  ENDMETHOD.
  METHOD SET_SORT.
**  CLEAR gs_sort.
**  gs_sort-fieldname = 'LIFNR'.
**  gs_sort-spos = '1'.
**  gs_sort-up = 'X'.
***  gs_sort-subtot = 'X'.
**  APPEND gs_sort TO gt_sort.
  ENDMETHOD.
  METHOD SET_ALV_GRID.
*SAPLKKBL
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = SY-REPID
        "I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
        "I_callback_user_command  = 'USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       i_html_height_top  = 12
*       I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    =
        IS_LAYOUT          = GS_LAYOUT
        IT_FIELDCAT        = GT_FCAT
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        IT_SORT            = GT_SORT
*       IT_FILTER          =
*       IS_SEL_HIDE        =
        I_DEFAULT          = GC_X
        I_SAVE             = GC_A
*       IS_VARIANT         =
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  = 0
*       I_HTML_HEIGHT_END  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        T_OUTTAB           = GT_RESULT
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.
  METHOD HTML_TOP_OF_PAGE.
*  DATA: text TYPE sdydo_text_element.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 100.
*  text =  'Company Code Data'.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'HEADING'.
*
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
*
*  text = 'User Name : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-uname.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 50.
*
*
*  text = 'Date : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-datum.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 50.
*
*  text = 'Time : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-uzeit.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
  ENDMETHOD.
  METHOD SELECT_INPUT_FILE_NAME.
*    CALL FUNCTION 'F4_FILENAME'
*      EXPORTING
*        PROGRAM_NAME  = SY-REPID
*        DYNPRO_NUMBER = SY-DYNNR
*        FIELD_NAME    = 'PATH'
*      IMPORTING
*        FILE_NAME     = GV_TMP_FILE_PATH.
    DATA : LV_STRING TYPE STRING.

    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE( CHANGING SELECTED_FOLDER = LV_STRING ).
    GV_TMP_FILE_PATH = LV_STRING.

  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS DEFINITION.
*Handling double click
  PUBLIC SECTION.
    METHODS:
    HANDLE_DOUBLE_CLICK
    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS. "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD. "handle_double_click
ENDCLASS. "lcl_event_receiver IMPLEMENTATION
