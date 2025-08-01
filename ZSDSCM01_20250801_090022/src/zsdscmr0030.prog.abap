*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0030
*  Creation Date      : 01.11.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is a migration program to upload warranty
*                       master data into table ZSDSCMT003.
*  Purpose            : Upload Warranty Master
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCMR0030.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_DATA,
         EQUNR       TYPE ZSDSCMT003-EQUNR,
         MATNR       TYPE ZSDSCMT003-MATNR,
         SERNR       TYPE ZSDSCMT003-SERNR,
         WRTPK       TYPE ZSDSCMT003-WRTPK,
         WRTLT_FLAG  TYPE ZSDSCMT003-WRTLT_FLAG,
         WRTLT       TYPE ZSDSCMT003-WRTLT,
         VBELN_VL    TYPE ZSDSCMT003-VBELN_VL,
         FKDAT       TYPE ZSDSCMT003-FKDAT,
         WADAT_IST   TYPE ZSDSCMT003-WADAT_IST,
         VBELN_VA    TYPE ZSDSCMT003-VBELN_VA,
         KUNNR       TYPE ZSDSCMT003-KUNNR,
         VKBUR       TYPE ZSDSCMT003-VKBUR,
         VKGRP       TYPE ZSDSCMT003-VKGRP,
         INSDT       TYPE ZSDSCMT003-INSDT,
         COMDT       TYPE ZSDSCMT003-COMDT,
         INSSVO      TYPE ZSDSCMT003-INSSVO,
         COMSVO      TYPE ZSDSCMT003-COMSVO,
         GI_WRT_BEG  TYPE ZSDSCMT003-GI_WRT_BEG,
         GI_WRT_END  TYPE ZSDSCMT003-GI_WRT_END,
         STD_WRT_BEG TYPE ZSDSCMT003-STD_WRT_BEG,
         STD_WRT_END TYPE ZSDSCMT003-STD_WRT_END,
         CUS_REG_BEG TYPE ZSDSCMT003-CUS_REG_BEG,
         CUS_REG_END TYPE ZSDSCMT003-CUS_REG_END,
         EXT_WRT_BEG TYPE ZSDSCMT003-EXT_WRT_BEG,
         EXT_WRT_END TYPE ZSDSCMT003-EXT_WRT_END,
         MAT_POSID   TYPE ZSDSCMT003-MAT_POSID,
         STD_POSID   TYPE ZSDSCMT003-STD_POSID,
         EXT_POSID   TYPE ZSDSCMT003-EXT_POSID,
         MIGFLG      TYPE ZSDSCMT003-MIGFLG,
         UPDFLG      TYPE ZSDSCMT003-UPDFLG,
         ERNAM       TYPE ZSDSCMT003-ERNAM,
         ERDAT       TYPE ZSDSCMT003-ERDAT,
         ERZET       TYPE ZSDSCMT003-ERZET,
         AENAM       TYPE ZSDSCMT003-AENAM,
         AEDAT       TYPE ZSDSCMT003-AEDAT,
         AEZET       TYPE ZSDSCMT003-AEZET,
       END OF TS_DATA.
TYPES: TT_DATA  TYPE  STANDARD TABLE OF TS_DATA.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_DATA  TYPE  TT_DATA ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  GF_EQUNR  TYPE  CHAR18 ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
PARAMETERS:
  P_IFILE  TYPE  STRING LOWER CASE.

SELECT-OPTIONS:
  S_EQUNR  FOR GF_EQUNR.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*  PERFORM F_GET_CONSTANTS.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF S_EQUNR[] IS NOT INITIAL.
    DELETE FROM ZSDSCMT003 WHERE EQUNR IN S_EQUNR.
    COMMIT WORK.
    WRITE 'Deleted' ##NO_TEXT.
    RETURN.
  ELSE.
    PERFORM F_READ_FILE  USING  P_IFILE
                       CHANGING GT_DATA.
    IF GT_DATA IS INITIAL.
*     Message: No data found.
      MESSAGE S001(ZSDSCA01).
      RETURN.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_UPLOAD_DATA USING GT_DATA.

*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
    LT_FILE     TYPE  FILETABLE.

  DATA:
    LF_RC     TYPE  I,
    LF_ACTION TYPE  I.

  FIELD-SYMBOLS:
    <L_FILE>  TYPE  FILE_TABLE.


* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Text file(TXT)|*.TXT' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LF_RC
      USER_ACTION             = LF_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

* Read Selected
  IF NOT ( LF_ACTION IS INITIAL AND
           LF_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <L_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FILENAME = <L_FILE>-FILENAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_read_file
*----------------------------------------------------------------------*
*  Read File Into Internal table
*----------------------------------------------------------------------*
FORM F_READ_FILE  USING  UF_IFILE TYPE  STRING
                CHANGING CT_DATA  TYPE  TT_DATA.

  DATA:
    LF_ANS  TYPE  CHAR1.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = TEXT-A01
      TEXT_BUTTON_1         = 'Yes'(001)
      TEXT_BUTTON_2         = 'No'(002)
      DEFAULT_BUTTON        = '2'
      DISPLAY_CANCEL_BUTTON = ' '
    IMPORTING
      ANSWER                = LF_ANS
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    CLEAR LF_ANS.
  ENDIF.

  IF LF_ANS NE '1'.
    RETURN.
  ENDIF.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME                = UF_IFILE
      FILETYPE                = 'ASC'
      HAS_FIELD_SEPARATOR     = 'X'
    CHANGING
      DATA_TAB                = CT_DATA
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
      NOT_SUPPORTED_BY_GUI    = 17
      ERROR_NO_GUI            = 18
      OTHERS                  = 19.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UPLOAD_DATA
*----------------------------------------------------------------------*
*  Upload Data into table
*----------------------------------------------------------------------*
FORM F_UPLOAD_DATA  USING  UT_DATA TYPE TT_DATA.

  DATA:
    LT_SAVE TYPE STANDARD TABLE OF ZSDSCMT003,
    LS_SAVE TYPE ZSDSCMT003.


  DATA(LF_LINE) = LINES( UT_DATA ).

  DATA(LF_TEXT) = |Uploading data { LF_LINE } entries...| ##NO_TEXT.
  MC_SHOW_PROGRESS 40 LF_TEXT.

  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    CLEAR LS_SAVE.
    MOVE-CORRESPONDING <L_DATA> TO LS_SAVE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-EQUNR
      IMPORTING
        OUTPUT = LS_SAVE-EQUNR.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-MATNR
      IMPORTING
        OUTPUT = LS_SAVE-MATNR.
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-SERNR
      IMPORTING
        OUTPUT = LS_SAVE-SERNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-VBELN_VL
      IMPORTING
        OUTPUT = LS_SAVE-VBELN_VL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-VBELN_VA
      IMPORTING
        OUTPUT = LS_SAVE-VBELN_VA.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-KUNNR
      IMPORTING
        OUTPUT = LS_SAVE-KUNNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-INSSVO
      IMPORTING
        OUTPUT = LS_SAVE-INSSVO.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-COMSVO
      IMPORTING
        OUTPUT = LS_SAVE-COMSVO.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-MAT_POSID
      IMPORTING
        OUTPUT = LS_SAVE-MAT_POSID.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-STD_POSID
      IMPORTING
        OUTPUT = LS_SAVE-STD_POSID.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-EXT_POSID
      IMPORTING
        OUTPUT = LS_SAVE-EXT_POSID.




    INSERT LS_SAVE INTO TABLE LT_SAVE.

  ENDLOOP.

  MODIFY ZSDSCMT003 FROM TABLE LT_SAVE.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.                                     "#EC CI_ROLLBACK
    WRITE 'Upload Error' ##NO_TEXT.
    RETURN.
  ENDIF.

  COMMIT WORK AND WAIT.
  LF_TEXT = |Upload completed: { LF_LINE } entries| ##NO_TEXT.
  WRITE LF_TEXT.

ENDFORM.
