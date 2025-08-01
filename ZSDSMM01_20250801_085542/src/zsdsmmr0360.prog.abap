*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0360
*  Creation Date      : 30.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          :
*  Description        : Upload Equipment data
*  Purpose            : After Logistic dept. finish create billing,
*                       document has checked status and will send to
*                       Credit  dept. to check for compleness
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0360.
*--------------------------------------------------------------------*
*  MACRO
*--------------------------------------------------------------------*
DEFINE SET_MESSAGE.
  IF &1 IS not INITIAL.
    CONCATENATE &1 &2 INTO &1 SEPARATED BY ','.
  ELSE.
    &1 = &2.
  ENDIF.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TS_TEXT,
    EQUNR           TYPE STRING,
    ERDAT           TYPE STRING,
    EQTYP           TYPE STRING,
    EQKTX           TYPE STRING,
    GROES           TYPE STRING,
    BRGEW           TYPE STRING,
    GEWEI           TYPE STRING,
    SERGE           TYPE STRING,
    MATNR           TYPE STRING,
    SERNR           TYPE STRING,
    KUNDE           TYPE STRING,
    DATLWB          TYPE STRING,
    ZZNAME1         TYPE STRING,
    ZZADDR1         TYPE STRING,
    ZZADDR2         TYPE STRING,
    ZZADDR3         TYPE STRING,
    ZZPROVIC        TYPE STRING,
    ZZZIPCD         TYPE STRING,
    ZZPHONE         TYPE STRING,
    ZZCOMMISSIONING TYPE STRING,
    LBBSA           TYPE STRING,
    B_WERK          TYPE STRING,
    B_LAGER         TYPE STRING,
    SOBKZ           TYPE STRING,
    KUNNR           TYPE STRING,
    LIFNR           TYPE STRING,
    DATAB           TYPE STRING,
    DATBI           TYPE STRING,
    EQLFN           TYPE STRING,
    TPLNR           TYPE STRING,
    MGANR           TYPE STRING,
    GAART           TYPE STRING,
    GWLDT           TYPE STRING,
    GWLEN           TYPE STRING,
    GAERB           TYPE STRING,
    WAGET           TYPE STRING,
    STATUS          TYPE STRING,
    STATUS_TX       TYPE STRING,
    PARVW1          TYPE STRING,
    PARNR1          TYPE STRING,
    PARVW2          TYPE STRING,
    PARNR2          TYPE STRING,
    PARVW3          TYPE STRING,
    PARNR3          TYPE STRING,
    PARVW4          TYPE STRING,
    PARNR4          TYPE STRING,
  END OF TS_TEXT,
  TT_TEXT TYPE STANDARD TABLE OF TS_TEXT,

  BEGIN OF TS_LOG,
    ROWNO   TYPE text10,
    MSGTY   TYPE STRING,
    MSGTX   TYPE STRING,
    EQUNR_N TYPE STRING,
    DUMMY   TYPE C.
    INCLUDE TYPE TS_TEXT.
*    PARNR1_N TYPE STRING,
*    PARNR2_N TYPE STRING,
*    PARNR3_N TYPE STRING,
*    PARNR4_N TYPE STRING,
TYPES:
END OF TS_LOG,
TT_LOG TYPE STANDARD TABLE OF TS_LOG,

  BEGIN OF TS_DATA,
    EQUNR     TYPE EQUI-EQUNR,
    ERDAT     TYPE EQUI-ERDAT,
    EQTYP     TYPE EQUI-EQTYP,
    EQKTX     TYPE EQKT-EQKTX,
    GROES     TYPE EQUI-GROES,
    BRGEW     TYPE EQUI-BRGEW,
    GEWEI     TYPE EQUI-GEWEI,
    SERGE     TYPE EQUI-SERGE,
    MATNR     TYPE EQUI-MATNR,
    SERNR     TYPE EQUI-SERNR,
    KUNDE     TYPE EQUI-KUNDE,
    DATLWB    TYPE EQUI-DATLWB,
*    ZZNAME1         TYPE EQUI-ZZNAME1,
*    ZZADDR1         TYPE EQUI-ZZADDR1,
*    ZZADDR2         TYPE EQUI-ZZADDR2,
*    ZZADDR3         TYPE EQUI-ZZADDR3,
*    ZZPROVIC        TYPE EQUI-ZZPROVIC,
*    ZZZIPCD         TYPE EQUI-ZZZIPCD,
*    ZZPHONE         TYPE EQUI-ZZPHONE,
*    ZZCOMMISSIONING TYPE EQUI-ZZCOMMISSIONING,
    LBBSA     TYPE EQBS-LBBSA,
    B_WERK    TYPE EQBS-B_WERK,
    B_LAGER   TYPE EQBS-B_LAGER,
    SOBKZ     TYPE EQBS-SOBKZ,
    KUNNR     TYPE EQBS-KUNNR,
    LIFNR     TYPE EQBS-LIFNR,
    KDAUF     TYPE EQBS-KDAUF,
    KDPOS     TYPE EQBS-KDPOS,
    DATAB     TYPE EQUZ-DATAB,
    DATBI     TYPE EQUZ-DATBI,
    EQLFN     TYPE EQUZ-EQLFN,
    TPLNR     TYPE ILOA-TPLNR,
    MGANR     TYPE BGMKOBJ-MGANR,
    GAART     TYPE BGMKOBJ-GAART,
    GWLDT     TYPE BGMKOBJ-GWLDT,
    GWLEN     TYPE BGMKOBJ-GWLEN,
    GAERB     TYPE BGMKOBJ-GAERB,
    WAGET     TYPE BGMKOBJ-WAGET,
    STATUS    TYPE TEXT50,
    STATUS_TX TYPE TEXT100,
    PARVW1    TYPE IHPA-PARVW,
    PARNR1    TYPE IHPA-PARNR,
    PARVW2    TYPE IHPA-PARVW,
    PARNR2    TYPE IHPA-PARNR,
    PARVW3    TYPE IHPA-PARVW,
    PARNR3    TYPE IHPA-PARNR,
    PARVW4    TYPE IHPA-PARVW,
    PARNR4    TYPE IHPA-PARNR,
    ROWNO     TYPE NUM6,
    EQUNR_N   TYPE EQUI-EQUNR,
    PARNR1_N  TYPE IHPA-PARNR,
    PARNR2_N  TYPE IHPA-PARNR,
    PARNR3_N  TYPE IHPA-PARNR,
    PARNR4_N  TYPE IHPA-PARNR,
  END OF TS_DATA,
  TT_DATA TYPE STANDARD TABLE OF TS_DATA,

  BEGIN OF TS_KNB1_KEY,
    ALTKN TYPE KNB1-ALTKN,
  END OF TS_KNB1_KEY,
  TT_KNB1_KEY TYPE SORTED TABLE OF TS_KNB1_KEY WITH UNIQUE KEY ALTKN,
  BEGIN OF TS_KNB1,
    ALTKN TYPE KNB1-ALTKN,
    KUNNR TYPE KNB1-KUNNR,
  END OF TS_KNB1,
  TT_KNB1 TYPE SORTED TABLE OF TS_KNB1 WITH NON-UNIQUE KEY ALTKN,
  BEGIN OF TS_PA0001_KEY,
    PERNR TYPE PA0001-PERNR,
  END OF TS_PA0001_KEY,
  TT_PA0001_KEY TYPE SORTED TABLE OF TS_PA0001_KEY WITH UNIQUE KEY PERNR,
  BEGIN OF TS_IHPA_UP,
    PARVW TYPE IHPA-PARVW,
    PARNR TYPE IHPA-PARNR,
  END OF TS_IHPA_UP,
  TT_IHPA_UP TYPE STANDARD TABLE OF TS_IHPA_UP.
TYPES: BEGIN OF TS_FILE_INFO,
         DIRECTORY TYPE STRING,
         FILENAME  TYPE STRING,
         FULLNAME  TYPE STRING,
       END OF TS_FILE_INFO.
TYPES: BEGIN OF TS_SUM,
         TOTAL TYPE I, "Total Entries
         SUCCS TYPE I, "Success Entries
         WARN  TYPE I, "Warning Entries
         ERROR TYPE I, "Error Entries
       END OF TS_SUM.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA:
  GT_DATA       TYPE TT_DATA,
  GT_LOG        TYPE TT_LOG,
  GT_HEADERLOG  TYPE TT_LOG,
  GT_KNB1_KEY   TYPE TT_KNB1_KEY,
  GT_PA0001_KEY TYPE TT_PA0001_KEY.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_LOGFILE TYPE  TS_FILE_INFO,
  GS_SUM     TYPE  TS_SUM.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Upload Info Record
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
      P_LFILE  TYPE  STRING LOWER CASE MODIF ID LOC.
  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s02: Start Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S02 FOR FIELD P_BEGROW.
    PARAMETERS:
      P_BEGROW TYPE I DEFAULT 3 MODIF ID LOC.
*     Text-s03: Start Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S03 FOR FIELD P_BEGCOL.
    PARAMETERS:
      P_BEGCOL TYPE I DEFAULT 1 MODIF ID LOC.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s04: End Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S04 FOR FIELD P_ENDROW.
    PARAMETERS:
      P_ENDROW TYPE I DEFAULT 9999 MODIF ID LOC.
*     Text-s05: End Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S05 FOR FIELD P_ENDCOL.
    PARAMETERS:
      P_ENDCOL TYPE I DEFAULT 46 MODIF ID LOC.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS:
    P_TEST   TYPE FLAG AS CHECKBOX DEFAULT 'X',
    P_LOG    TYPE FLAG AS CHECKBOX DEFAULT 'X',
    P_LOGFIL TYPE STRING LOWER CASE DEFAULT 'C:\Temp'.

SELECTION-SCREEN END OF BLOCK B1.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LFILE.
* List Local input File
  PERFORM F_LIST_IFILE CHANGING P_LFILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LOGFIL.
* Get folder location for log file
  PERFORM F_GET_FOLDER_NAME CHANGING P_LOGFIL.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_READ_INPUT USING P_LFILE
                             P_BEGROW
                             P_BEGCOL
                             P_ENDROW
                             P_ENDCOL
                      CHANGING GT_DATA
                               GT_LOG
                               GT_KNB1_KEY
                               GT_PA0001_KEY.
  GS_SUM-TOTAL = LINES( GT_DATA ).
  PERFORM F_PROCESS_DATA USING    GT_KNB1_KEY
                                  GT_PA0001_KEY
                         CHANGING GT_DATA
                                  GT_LOG.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF P_LOG EQ ABAP_TRUE .

    PERFORM F_ASSIGN_LOGFILE  USING  P_LOGFIL
                            CHANGING GS_LOGFILE.
    PERFORM F_CREATE_LOGFILE  USING  GS_SUM
                                     GS_LOGFILE
                                     GT_LOG.
  ENDIF.
*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING CF_FILENAME  TYPE  STRING.

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
      FILE_FILTER             = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
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
*&---------------------------------------------------------------------*
*&      Form  F_GET_FOLDER_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_FOLDER_NAME CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
    LF_FOLDER TYPE STRING,
    LF_INIT   TYPE STRING.

  LF_INIT = P_LOGFIL.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      INITIAL_FOLDER       = LF_INIT
    CHANGING
      SELECTED_FOLDER      = LF_FOLDER
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC EQ 0 AND LF_FOLDER IS NOT INITIAL.
    CF_FILENAME = LF_FOLDER.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_get_directory_of_file
*----------------------------------------------------------------------*
*  Get Directory of File
*----------------------------------------------------------------------*
FORM F_GET_DIRECTORY_OF_FILE  USING  UF_FILE  TYPE CLIKE
                            CHANGING CF_DIR   TYPE CLIKE.

  DATA:
    LF_SEP  TYPE  HCS_DIRECTORY_SEPARATOR,
    LF_OFFS TYPE  I.


* Initialize Output
  CLEAR: CF_DIR.

* Get Separator
  CALL METHOD CL_HCS_DIRECTORY_ACCESS=>GET_SEPARATOR
    RECEIVING
      R_SEPARATOR = LF_SEP.

* Find last separator
  FIND ALL OCCURRENCES OF LF_SEP IN UF_FILE
                                 MATCH OFFSET LF_OFFS.

  LF_OFFS = LF_OFFS + 1.

* Assign Output
  CF_DIR = UF_FILE(LF_OFFS).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_INPUT
*&---------------------------------------------------------------------*
FORM F_READ_INPUT  USING    UF_LFILE TYPE STRING
                            UF_BEGROW TYPE I
                            UF_BEGCOL TYPE I
                            UF_ENDROW TYPE I
                            UF_ENDCOL TYPE I
                   CHANGING CT_DATA TYPE TT_DATA
                            CT_LOG TYPE TT_LOG
                            CT_KNB1_KEY TYPE TT_KNB1_KEY
                            CT_PA0001_KEY TYPE TT_PA0001_KEY.
  DATA:     LT_TEXT   TYPE TABLE OF TS_TEXT.

  PERFORM F_READ_EXCEL USING UF_LFILE
                            UF_BEGROW
                            UF_BEGCOL
                            UF_ENDROW
                            UF_ENDCOL
                   CHANGING LT_TEXT.
  PERFORM F_COVERT_TEXT_TO_DATA USING LT_TEXT
                                CHANGING CT_DATA
                                         CT_LOG
                                         CT_KNB1_KEY
                                         CT_PA0001_KEY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_EXCEL
*&---------------------------------------------------------------------*
FORM F_READ_EXCEL  USING    UF_LFILE TYPE STRING
                            UF_BEGROW TYPE I
                            UF_BEGCOL TYPE I
                            UF_ENDROW TYPE I
                            UF_ENDCOL TYPE I
                   CHANGING CT_TEXT TYPE TT_TEXT.
  DATA: LT_INTERN   TYPE  TABLE OF ALSMEX_TABLINE,
        LS_INTERN   LIKE LINE OF LT_INTERN,
        LS_TEXT     TYPE TS_TEXT,
        LF_FILENAME TYPE  RLGRAP-FILENAME,
        LS_LOG      TYPE TS_LOG.

  FIELD-SYMBOLS <LFS_FIELD> TYPE ANY.
  LF_FILENAME = UF_LFILE.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = LF_FILENAME
      I_BEGIN_COL             = UF_BEGCOL
      I_BEGIN_ROW             = 1
      I_END_COL               = UF_ENDCOL
      I_END_ROW               = UF_ENDROW
    TABLES
      INTERN                  = LT_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
*   Append Error
    RETURN.
  ENDIF.
  LOOP AT LT_INTERN INTO LS_INTERN.
    UNASSIGN <LFS_FIELD>.
    ASSIGN COMPONENT LS_INTERN-COL OF STRUCTURE LS_TEXT TO <LFS_FIELD>.
    IF <LFS_FIELD> IS ASSIGNED.
      <LFS_FIELD> = LS_INTERN-VALUE.
    ENDIF.
    AT END OF ROW.
      IF LS_TEXT IS NOT INITIAL.
        IF LS_INTERN-ROW < UF_BEGROW.
          MOVE-CORRESPONDING LS_TEXT TO LS_LOG.
          APPEND LS_LOG TO GT_HEADERLOG.
        ELSE.
          APPEND LS_TEXT TO CT_TEXT.
        ENDIF.
      ENDIF.
      CLEAR LS_TEXT.
    ENDAT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COVERT_TEXT_TO_DATA
*&---------------------------------------------------------------------*
FORM F_COVERT_TEXT_TO_DATA  USING    UT_TEXT TYPE TT_TEXT
                            CHANGING CT_DATA TYPE TT_DATA
                                     CT_LOG TYPE TT_LOG
                                     CT_KNB1_KEY TYPE TT_KNB1_KEY
                                     CT_PA0001_KEY TYPE TT_PA0001_KEY.

  DATA: LS_DATA  TYPE TS_DATA,
        LS_LOG   TYPE TS_LOG,
        LF_ROWNO TYPE NUM06,
        LF_ERROR TYPE FLAG.

  LOOP AT UT_TEXT ASSIGNING FIELD-SYMBOL(<L_TEXT>).
    CLEAR: LS_LOG,
           LS_DATA.
    LF_ROWNO = LF_ROWNO + 1.
    MOVE-CORRESPONDING   <L_TEXT> TO LS_LOG.
    MOVE-CORRESPONDING   <L_TEXT> TO LS_DATA.
    LS_LOG-ROWNO = LF_ROWNO.
    LS_DATA-ROWNO = LF_ROWNO.

    "--------------validate data type ---

    IF <L_TEXT>-MATNR IS INITIAL.
      SET_MESSAGE LS_LOG-MSGTX 'MATNR is required'(m15).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = LS_DATA-MATNR
        IMPORTING
          OUTPUT       = LS_DATA-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.

      IF SY-SUBRC <> 0.
        SET_MESSAGE LS_LOG-MSGTX 'MATNR is invalid format'(m02).
      ENDIF.
    ENDIF.

    IF <L_TEXT>-MATNR IS INITIAL.
      SET_MESSAGE LS_LOG-MSGTX 'SERNR is required'(m16).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
        EXPORTING
          INPUT  = LS_DATA-SERNR
        IMPORTING
          OUTPUT = LS_DATA-SERNR.
    ENDIF.

    IF <L_TEXT>-DATAB IS INITIAL.
      SET_MESSAGE LS_LOG-MSGTX 'DATAB is required'(m17).
    ELSE.
      PERFORM VALIDATE_DATE USING LS_DATA-DATAB
                            CHANGING LF_ERROR.
      IF LF_ERROR = ABAP_TRUE.
        SET_MESSAGE LS_LOG-MSGTX 'DATAB is invalid date format'(m03).
      ENDIF.
    ENDIF.

    IF <L_TEXT>-DATBI IS INITIAL.
      SET_MESSAGE LS_LOG-MSGTX 'DATBI is required'(m18).
    ELSE.
      PERFORM VALIDATE_DATE USING LS_DATA-DATBI
                            CHANGING LF_ERROR.
      IF LF_ERROR = ABAP_TRUE.
        SET_MESSAGE LS_LOG-MSGTX 'DATBI is invalid date format'(m04).
      ENDIF.
    ENDIF.

    IF <L_TEXT>-MGANR IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_DATA-MGANR
        IMPORTING
          OUTPUT = LS_DATA-MGANR.
    ENDIF.
    PERFORM VALIDATE_DATE USING LS_DATA-GWLDT
                          CHANGING LF_ERROR.
    IF LF_ERROR = ABAP_TRUE.
      SET_MESSAGE LS_LOG-MSGTX 'GWLDT is invalid date format'(m05).
    ENDIF.

    PERFORM VALIDATE_DATE USING LS_DATA-GWLEN
                          CHANGING LF_ERROR.
    IF LF_ERROR = ABAP_TRUE.
      SET_MESSAGE LS_LOG-MSGTX 'GWLEN is invalid date format'(m06).
    ENDIF.

    PERFORM CONVERT_PARVW USING <L_TEXT>-PARVW1
                           CHANGING LS_DATA-PARVW1
                                    LF_ERROR.
    IF LF_ERROR = ABAP_TRUE.
      SET_MESSAGE LS_LOG-MSGTX 'PARVW1 is invalid'(m07).
    ENDIF.
    IF <L_TEXT>-PARNR1 IS NOT INITIAL.
      PERFORM CONVERT_PARNR USING  <L_TEXT>-PARVW1
                                   <L_TEXT>-PARNR1
                            CHANGING LS_DATA-PARNR1
                                     LF_ERROR
                                     CT_KNB1_KEY
                                     CT_PA0001_KEY .
      IF LF_ERROR = ABAP_TRUE.
        SET_MESSAGE LS_LOG-MSGTX 'PARNR1 is invalid format'(m11).
      ENDIF.
    ENDIF.

    PERFORM CONVERT_PARVW USING <L_TEXT>-PARVW2
                           CHANGING LS_DATA-PARVW2
                                    LF_ERROR.
    IF LF_ERROR = ABAP_TRUE.
      SET_MESSAGE LS_LOG-MSGTX 'PARVW2 is invalid'(m08).
    ENDIF.
    IF <L_TEXT>-PARNR2 IS NOT INITIAL.
      PERFORM CONVERT_PARNR USING  <L_TEXT>-PARVW2
                                   <L_TEXT>-PARNR2
                            CHANGING LS_DATA-PARNR2
                                     LF_ERROR
                                     CT_KNB1_KEY
                                     CT_PA0001_KEY .
      IF LF_ERROR = ABAP_TRUE.
        SET_MESSAGE LS_LOG-MSGTX 'PARNR2 is invalid format'(m12).
      ENDIF.
    ENDIF.

    PERFORM CONVERT_PARVW USING <L_TEXT>-PARVW3
                           CHANGING LS_DATA-PARVW3
                                    LF_ERROR.
    IF LF_ERROR = ABAP_TRUE.
      SET_MESSAGE LS_LOG-MSGTX 'PARVW3 is invalid'(m09).
    ENDIF.
    IF <L_TEXT>-PARNR3 IS NOT INITIAL.
      PERFORM CONVERT_PARNR USING  <L_TEXT>-PARVW3
                                   <L_TEXT>-PARNR3
                            CHANGING LS_DATA-PARNR3
                                     LF_ERROR
                                     CT_KNB1_KEY
                                     CT_PA0001_KEY .
      IF LF_ERROR = ABAP_TRUE.
        SET_MESSAGE LS_LOG-MSGTX 'PARNR3 is invalid format'(m13).
      ENDIF.
    ENDIF.

    PERFORM CONVERT_PARVW USING <L_TEXT>-PARVW4
                           CHANGING LS_DATA-PARVW4
                                    LF_ERROR.
    IF LF_ERROR = ABAP_TRUE.
      SET_MESSAGE LS_LOG-MSGTX 'PARVW4 is invalid'(m10).
    ENDIF.
    IF <L_TEXT>-PARNR4 IS NOT INITIAL.
      PERFORM CONVERT_PARNR USING  <L_TEXT>-PARVW4
                                   <L_TEXT>-PARNR4
                            CHANGING LS_DATA-PARNR4
                                     LF_ERROR
                                     CT_KNB1_KEY
                                     CT_PA0001_KEY .
      IF LF_ERROR = ABAP_TRUE.
        SET_MESSAGE LS_LOG-MSGTX 'PARNR4 is invalid format'(m14).
      ENDIF.
    ENDIF.

    IF LS_LOG-MSGTX IS NOT INITIAL.
      LS_LOG-MSGTY = 'E'.
    ENDIF.

    APPEND LS_LOG TO CT_LOG.
    APPEND LS_DATA TO CT_DATA.
  ENDLOOP.
ENDFORM.
**---------------------------------------------------------------------*
**      Form  SET_AMOUNT
**---------------------------------------------------------------------*
*FORM SET_AMOUNT  USING    UF_TX_AMOUNT TYPE STRING
*                 CHANGING CF_AMOUNT TYPE ANY
*                          CF_ERROR  TYPE FLAG.
*  DATA: LF_TX_AMOUNT TYPE STRING.
*  LF_TX_AMOUNT = UF_TX_AMOUNT.
*  CLEAR CF_ERROR.
*  REPLACE ALL OCCURRENCES OF ',' IN LF_TX_AMOUNT WITH SPACE.
*  CONDENSE LF_TX_AMOUNT NO-GAPS.
*  TRY.
*      CF_AMOUNT  = LF_TX_AMOUNT.
*    CATCH CX_SY_CONVERSION_NO_NUMBER.
*      CF_ERROR = 'X'.
*  ENDTRY.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_date
*&---------------------------------------------------------------------*
FORM VALIDATE_DATE  USING    UF_DATAB TYPE ANY
                    CHANGING CF_ERROR TYPE FLAG.
  CLEAR CF_ERROR .
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      DATE                      = UF_DATAB
    EXCEPTIONS
      PLAUSIBILITY_CHECK_FAILED = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    CF_ERROR = ABAP_TRUE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form convert_parvw
*&---------------------------------------------------------------------*
FORM CONVERT_PARVW  USING    UF_PARVW TYPE STRING
                    CHANGING CF_PARVW_DATA TYPE IHPA-PARVW
                             CF_ERROR TYPE FLAG.

  CLEAR CF_ERROR.
  IF UF_PARVW IS INITIAL.
    RETURN.
  ENDIF.
  CASE UF_PARVW .
    WHEN 'D1'.
      CF_PARVW_DATA = 'SP' . " 'AG'.
    WHEN 'D2'.
      CF_PARVW_DATA = 'SH' . " 'WE'
    WHEN 'PE'.
      CF_PARVW_DATA = 'PE' . " 'VE'
    WHEN OTHERS.
      CF_ERROR = ABAP_TRUE.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA  USING    UT_KNB1_KEY TYPE TT_KNB1_KEY
                              UT_PA0001_KEY TYPE TT_PA0001_KEY
                     CHANGING CT_DATA     TYPE TT_DATA
                              CT_LOG      TYPE TT_LOG.
  DATA: LT_MARA_KEY TYPE TT_DATA.
  DATA: LS_GENERAL  TYPE BAPI_ITOB,
        LS_SPECIFIC TYPE BAPI_ITOB_EQ_ONLY,
        LS_RETURN   TYPE BAPIRET2,
        LV_EQUNR    TYPE EQUI-EQUNR,
        LT_BGMKOBJ  TYPE TABLE OF BGMKOBJ_RFC,
        LS_BGMKOBJ  TYPE BGMKOBJ_RFC,
        LT_IHPA     TYPE TT_IHPA_UP,
        LS_IHPA     LIKE LINE OF LT_IHPA,
        LF_ERROR    TYPE FLAG,
        LT_KNB1     TYPE TT_KNB1,
        LT_PA0001   TYPE TT_PA0001_KEY.
  LT_MARA_KEY[] = CT_DATA[].
  DELETE LT_MARA_KEY WHERE MATNR IS INITIAL.
  SORT LT_MARA_KEY BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_MARA_KEY COMPARING MATNR.
  IF LT_MARA_KEY[] IS NOT INITIAL.
    SELECT MATNR
    INTO TABLE @DATA(LT_MARA)
    FROM MARA
    FOR ALL ENTRIES IN @LT_MARA_KEY
    WHERE MATNR = @LT_MARA_KEY-MATNR.
    SORT LT_MARA BY MATNR.
  ENDIF.
  IF UT_KNB1_KEY[] IS NOT INITIAL.
    SELECT ALTKN, KUNNR
    INTO TABLE @LT_KNB1
    FROM KNB1
    FOR ALL ENTRIES IN @UT_KNB1_KEY
    WHERE BUKRS = '1000'
    AND   ALTKN = @UT_KNB1_KEY-ALTKN.
  ENDIF.
  IF UT_PA0001_KEY[] IS NOT INITIAL.
    SELECT PERNR
    INTO TABLE @LT_PA0001
    FROM PA0001
    FOR ALL ENTRIES IN @UT_PA0001_KEY
    WHERE PERNR = @UT_PA0001_KEY-PERNR.
  ENDIF.
  SORT CT_LOG BY ROWNO.
  LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CLEAR: LS_GENERAL,
           LS_SPECIFIC,
           LS_RETURN,
           LV_EQUNR ,
           LT_BGMKOBJ,
           LS_BGMKOBJ,
           LT_IHPA   ,
           LS_IHPA    .

    READ TABLE CT_LOG ASSIGNING FIELD-SYMBOL(<L_LOG>) WITH KEY ROWNO = <L_DATA>-ROWNO
                                                      BINARY SEARCH.

    IF <L_DATA>-MATNR IS NOT INITIAL.
      READ TABLE LT_MARA TRANSPORTING NO FIELDS WITH KEY MATNR = <L_DATA>-MATNR
                                                         BINARY SEARCH.
      IF SY-SUBRC <> 0.
        SET_MESSAGE <L_LOG>-MSGTX 'MATNR value is not found in system'(m19).
      ENDIF.
    ENDIF.

    PERFORM F_VALIDATE_PARTNER USING <L_DATA>-PARVW1
                                     <L_DATA>-PARNR1
                                     'PARNR1'
                                     LT_KNB1
                                     LT_PA0001
                               CHANGING <L_LOG>-MSGTX
                                        <L_DATA>-PARNR1_N
                                        LT_IHPA.

    PERFORM F_VALIDATE_PARTNER USING <L_DATA>-PARVW2
                                     <L_DATA>-PARNR2
                                     'PARNR2'
                                     LT_KNB1
                                     LT_PA0001
                               CHANGING <L_LOG>-MSGTX
                                        <L_DATA>-PARNR2_N
                                        LT_IHPA.

    PERFORM F_VALIDATE_PARTNER USING <L_DATA>-PARVW3
                                     <L_DATA>-PARNR3
                                     'PARNR3'
                                     LT_KNB1
                                     LT_PA0001
                               CHANGING <L_LOG>-MSGTX
                                        <L_DATA>-PARNR3_N
                                        LT_IHPA.
    PERFORM F_VALIDATE_PARTNER USING <L_DATA>-PARVW4
                                     <L_DATA>-PARNR4
                                     'PARNR1'
                                     LT_KNB1
                                     LT_PA0001
                               CHANGING <L_LOG>-MSGTX
                                        <L_DATA>-PARNR4_N
                                        LT_IHPA.
    IF <L_LOG>-MSGTX IS NOT INITIAL.
      PERFORM F_SET_ERROR USING <L_LOG>-MSGTX
                          CHANGING <L_LOG>-MSGTY
                                   <L_LOG>-MSGTX.
      CONTINUE.
    ENDIF.


    LS_GENERAL-DESCRIPT  = <L_DATA>-EQKTX.
    LS_GENERAL-MANSERNO  = <L_DATA>-SERGE.
    LS_GENERAL-START_FROM = <L_DATA>-DATAB .

    LS_SPECIFIC-EQUICATGRY = <L_DATA>-EQTYP.
    LS_SPECIFIC-MATERIAL_LONG =  <L_DATA>-MATNR.
    LS_SPECIFIC-SERIALNO = <L_DATA>-SERNR.

    CALL FUNCTION 'BAPI_EQUI_CREATE'
      EXPORTING
        DATA_GENERAL  = LS_GENERAL
        DATA_SPECIFIC = LS_SPECIFIC
        VALID_DATE    = <L_DATA>-DATAB
      IMPORTING
        EQUIPMENT     = LV_EQUNR
        RETURN        = LS_RETURN.

    IF LS_RETURN-TYPE = 'E'.
      PERFORM F_SET_ERROR USING LS_RETURN-MESSAGE
                          CHANGING <L_LOG>-MSGTY
                                   <L_LOG>-MSGTX.
      CONTINUE.
    ELSE.
      IF P_TEST = ABAP_TRUE.
        PERFORM F_SET_SUCCESS CHANGING <L_LOG>-MSGTY
                                       <L_LOG>-MSGTX.
        CONTINUE.
      ENDIF.
*     ------------ warranty -------
      IF <L_DATA>-MGANR IS NOT INITIAL
      OR <L_DATA>-GAART IS NOT INITIAL
      OR <L_DATA>-GWLDT IS NOT INITIAL
      OR <L_DATA>-GWLEN IS NOT INITIAL
      OR <L_DATA>-GAERB IS NOT INITIAL
      OR <L_DATA>-WAGET IS NOT INITIAL.
        CONCATENATE 'IE' LV_EQUNR INTO LS_BGMKOBJ-OBJNR .
        LS_BGMKOBJ-MGANR  = <L_DATA>-MGANR.
        LS_BGMKOBJ-GAART  = <L_DATA>-GAART.
        LS_BGMKOBJ-GWLDT  = <L_DATA>-GWLDT.
        LS_BGMKOBJ-GWLEN  = <L_DATA>-GWLEN.
        LS_BGMKOBJ-GAERB  = <L_DATA>-GAERB.
        LS_BGMKOBJ-WAGET  = <L_DATA>-WAGET.
        APPEND LS_BGMKOBJ TO LT_BGMKOBJ.
        CALL FUNCTION 'WARRANTY_ASSIGNMENT_RFC'
          EXPORTING
            UPDATE_WAGET            = 'X'
            UPDATE_GAERB            = 'X'
            CHANGE_ALLOWED          = 'X'
          TABLES
            I_OBJ_WA                = LT_BGMKOBJ
          EXCEPTIONS
            INVALID_OBJECT_NUMBER   = 1
            INVALID_WARRANTY_NUMBER = 2
            NO_ENTRY                = 3
            UPDATE_ERROR            = 4
            INVALID_WATYPE          = 5
            OTHERS                  = 6.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                     INTO <L_LOG>-MSGTX.
          PERFORM F_SET_ERROR USING <L_LOG>-MSGTX
                          CHANGING <L_LOG>-MSGTY
                                   <L_LOG>-MSGTX.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          CONTINUE.
        ELSE.
          <L_DATA>-EQUNR_N = LV_EQUNR.
          <L_LOG>-EQUNR_N  = LV_EQUNR.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
*      ------------ partner -------
          IF LT_IHPA[] IS NOT INITIAL.
            PERFORM F_UPDATE_IHPA USING    LV_EQUNR
                                           LT_IHPA
                                  CHANGING <L_LOG>-MSGTY
                                           <L_LOG>-MSGTX.
          ENDIF.
        ENDIF.
      ENDIF.


      IF <L_LOG>-MSGTY IS INITIAL.
        PERFORM F_SET_SUCCESS CHANGING <L_LOG>-MSGTY
                                      <L_LOG>-MSGTX.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form convert_parnr
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM CONVERT_PARNR  USING    UF_PARVW TYPE STRING
                             UF_PARNR TYPE STRING
                    CHANGING CF_PARNR TYPE IHPA-PARNR
                             CF_ERROR TYPE FLAG
                             CT_KNB1_KEY TYPE TT_KNB1_KEY
                             CT_PA0001_KEY TYPE TT_PA0001_KEY.

  DATA: LF_PERNR TYPE PA0001-PERNR.
  DATA: LS_KNB1_KEY   LIKE LINE OF GT_KNB1_KEY,
        LS_PA0001_KEY LIKE LINE OF GT_PA0001_KEY.

  CLEAR CF_ERROR.
  IF UF_PARVW IS INITIAL.
    RETURN.
  ENDIF.
  IF NOT UF_PARNR CO ' 0123456789'.
    CF_ERROR = ABAP_TRUE.
  ENDIF.
  IF UF_PARVW = 'D1'
  OR UF_PARVW = 'D2'.
    CF_PARNR = UF_PARNR.
    LS_KNB1_KEY-ALTKN = CF_PARNR.
    INSERT LS_KNB1_KEY INTO TABLE CT_KNB1_KEY.
  ELSEIF UF_PARVW = 'PE'.
    LF_PERNR = UF_PARNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LF_PERNR
      IMPORTING
        OUTPUT = LF_PERNR.
    CF_PARNR = LF_PERNR.
    LS_KNB1_KEY-ALTKN = CF_PARNR.
    INSERT LS_PA0001_KEY INTO TABLE CT_PA0001_KEY.
  ELSE.
    RETURN.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_assign_logfile
*----------------------------------------------------------------------*
*  Assign Log filename
*----------------------------------------------------------------------*
FORM F_ASSIGN_LOGFILE  USING  CF_DIR  TYPE  CLIKE
                     CHANGING CS_LOGFILE TYPE  TS_FILE_INFO.
  DATA: LV_DATLO TYPE SY-DATLO,
        LV_TIMLO TYPE SY-TIMLO.
  LV_DATLO = SY-DATLO.
  LV_TIMLO = SY-TIMLO.
* Initialize Output
  CLEAR: CS_LOGFILE.

  CS_LOGFILE-DIRECTORY = CF_DIR.

* File name format:  Upload_Info_Record_Log_YYYYMMDD_HHMMSS.txt
  CONCATENATE '\Upload_Info_Record_Log_'  ##NO_TEXT
               LV_DATLO
               '_'
               LV_TIMLO
               '.xls'
         INTO CS_LOGFILE-FILENAME.

* Assign Fullname
  CONCATENATE CS_LOGFILE-DIRECTORY
              CS_LOGFILE-FILENAME
         INTO CS_LOGFILE-FULLNAME.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_create_logfile
*----------------------------------------------------------------------*
*  Generate Log File
*----------------------------------------------------------------------*
FORM F_CREATE_LOGFILE  USING  US_SUM TYPE TS_SUM
                              US_LOGFILE  TYPE  TS_FILE_INFO
                              UT_LOG TYPE  TT_LOG.



  DATA:
        LF_TEXT TYPE STRING.

  DATA:
    LF_SUCC  TYPE STRING,
    LF_ERR   TYPE STRING,
    LF_WARN  TYPE STRING,
    LV_TABIX TYPE SY-TABIX.

  LOOP AT GT_HEADERLOG ASSIGNING FIELD-SYMBOL(<L_LOG>).
    LV_TABIX = SY-TABIX.
    IF LV_TABIX = 1.
      <L_LOG>-ROWNO  = 'Row no'(c01).
      <L_LOG>-MSGTY = 'Message type'(c02).
      <L_LOG>-MSGTX = 'Message text'(c03).
      <L_LOG>-EQUNR_N = 'Created Equipment'(c04).
    ELSEIF LV_TABIX = 2.
    ENDIF.
  ENDLOOP.
  INSERT LINES OF GT_HEADERLOG INTO UT_LOG INDEX 1.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = US_LOGFILE-FULLNAME
      FILETYPE                = 'ASC'
      WRITE_FIELD_SEPARATOR   = 'X'
      TRUNC_TRAILING_BLANKS   = 'X'
      CODEPAGE                = '4103'
      WRITE_BOM               = 'X'
    TABLES
      DATA_TAB                = UT_LOG
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC EQ 0.
    LF_SUCC = US_SUM-SUCCS.
    LF_ERR = US_SUM-ERROR.
    LF_WARN = US_SUM-WARN.
    CONCATENATE 'Log file' US_LOGFILE-FILENAME 'has been created with'
                LF_SUCC 'success,'
                LF_WARN 'warning and'
                LF_ERR 'error'
                INTO LF_TEXT SEPARATED BY SPACE.
    MESSAGE LF_TEXT TYPE 'S'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_error
*&---------------------------------------------------------------------*

FORM F_SET_ERROR  USING  UV_MSGTX TYPE ANY
                  CHANGING CV_MSGTY TYPE TS_LOG-MSGTY
                           CV_MSGTX TYPE TS_LOG-MSGTX.
  CV_MSGTY = 'E'.
  CV_MSGTX = UV_MSGTX.
  GS_SUM-ERROR = GS_SUM-ERROR + 1.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_set_success
*&---------------------------------------------------------------------*

FORM F_SET_SUCCESS  CHANGING CV_MSGTY TYPE TS_LOG-MSGTY
                             CV_MSGTX TYPE TS_LOG-MSGTX.
  CV_MSGTY = 'S'.
  IF P_TEST = ABAP_TRUE.
    CV_MSGTX = 'Successfully test run'.
  ELSE.
    CV_MSGTX = 'Successfully run'.
  ENDIF.
  GS_SUM-SUCCS = GS_SUM-SUCCS + 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_partner
*&---------------------------------------------------------------------*
FORM F_VALIDATE_PARTNER  USING    UF_PARVW     TYPE TS_DATA-PARVW1
                                  UF_PARNR     TYPE TS_DATA-PARNR1
                                  UF_FIELDNAME TYPE ANY
                                  UT_KNB1      TYPE TT_KNB1
                                  UT_PA0001    TYPE TT_PA0001_KEY
                         CHANGING CF_MSGTX     TYPE TS_LOG-MSGTX
                                  CF_PARNR_N   TYPE TS_DATA-PARNR1
                                  CT_IHPA      TYPE TT_IHPA_UP.
  DATA: LF_MSGTX TYPE TS_LOG-MSGTX,
        LS_IHPA  LIKE LINE OF CT_IHPA.

  IF UF_PARNR IS NOT INITIAL
  AND UF_PARVW IS NOT INITIAL.
    IF UF_PARVW = 'SP' OR UF_PARVW = 'SH'.
      READ TABLE UT_KNB1 INTO DATA(LS_KNB1) WITH KEY ALTKN = UF_PARNR
                                                     BINARY SEARCH.
      IF SY-SUBRC <> 0.
        CONCATENATE UF_FIELDNAME 'value is not found in system'(m20) INTO LF_MSGTX
        SEPARATED BY SPACE.
        SET_MESSAGE CF_MSGTX LF_MSGTX.
      ELSE.
        CF_PARNR_N = LS_KNB1-KUNNR.
      ENDIF.
    ELSEIF UF_PARVW = 'PE'.
      READ TABLE UT_PA0001 TRANSPORTING NO FIELDS WITH KEY PERNR = UF_PARNR.
      IF SY-SUBRC <> 0.
        CONCATENATE UF_FIELDNAME 'value is not found in system'(m20) INTO LF_MSGTX
        SEPARATED BY SPACE.
        SET_MESSAGE CF_MSGTX LF_MSGTX.
      ELSE.
        CF_PARNR_N = UF_PARNR.
      ENDIF.
    ENDIF.
    READ TABLE CT_IHPA TRANSPORTING NO FIELDS WITH KEY PARVW = UF_PARVW.
    IF SY-SUBRC = 0.
      SET_MESSAGE CF_MSGTX 'Duplicate partner function'(m24).
    ELSE.
      LS_IHPA-PARVW = UF_PARVW.
      LS_IHPA-PARNR = CF_PARNR_N.
      APPEND LS_IHPA TO CT_IHPA.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_ihpa
*&---------------------------------------------------------------------*
FORM F_UPDATE_IHPA  USING    UF_EQUNR TYPE TS_DATA-EQUNR_N
                             UT_IHPA TYPE TT_IHPA_UP
                    CHANGING CF_MSGTY TYPE TS_LOG-MSGTY
                             CF_MSGTX TYPE TS_LOG-MSGTX.
  DATA: LT_BDCDATA TYPE TAB_BDCDATA,
        LT_MSG     TYPE TABLE OF BDCMSGCOLL,
        LV_INDEX   TYPE NUM02,
        LV_FNAM    TYPE BDCDATA-FNAM,
        LV_MODE    TYPE C.

  "------screen 0100---
  PERFORM BDC_DYNPRO      USING 'SAPMIEQ0' '0100'
                          CHANGING LT_BDCDATA.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE' '/00'
                          CHANGING LT_BDCDATA.
  PERFORM BDC_FIELD       USING 'RM63E-EQUNR' UF_EQUNR
                          CHANGING LT_BDCDATA.
  "------screen 0101---
  PERFORM BDC_DYNPRO      USING 'SAPMIEQ0' '0101'
                          CHANGING LT_BDCDATA.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE' '=PART'
                          CHANGING LT_BDCDATA.
  "------screen 0200---
  PERFORM BDC_DYNPRO      USING 'SAPLIPAR' '0200'
                          CHANGING LT_BDCDATA.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE' '/00'
                          CHANGING LT_BDCDATA.
  LOOP AT UT_IHPA ASSIGNING FIELD-SYMBOL(<L_IHPA>).
    LV_INDEX = LV_INDEX + 1.
    CONCATENATE 'IHPA-PARVW(' LV_INDEX ')' INTO LV_FNAM.
    PERFORM BDC_FIELD       USING LV_FNAM <L_IHPA>-PARVW
                            CHANGING LT_BDCDATA.
    CONCATENATE 'IHPA-PARNR(' LV_INDEX ')' INTO LV_FNAM.
    PERFORM BDC_FIELD       USING LV_FNAM <L_IHPA>-PARNR
                            CHANGING LT_BDCDATA.
  ENDLOOP.
  "------screen 0200---
  PERFORM BDC_DYNPRO      USING 'SAPLIPAR' '0200'
                          CHANGING LT_BDCDATA.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE' '=BACK'
                          CHANGING LT_BDCDATA.
  "------screen 0101---
  PERFORM BDC_DYNPRO      USING 'SAPMIEQ0' '0101'
                         CHANGING LT_BDCDATA.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE' '=BU'
                         CHANGING LT_BDCDATA.

  LV_MODE = 'N'.
  CALL TRANSACTION 'IE02' USING     LT_BDCDATA
                          MODE      LV_MODE
                          UPDATE    'S'
                          MESSAGES INTO LT_MSG.
  READ TABLE LT_MSG INTO DATA(LS_MSG) WITH KEY MSGID = 'IS'
                                               MSGNR = '817'.
  IF SY-SUBRC = 0.
    RETURN.
  ELSE.
    READ TABLE LT_MSG INTO LS_MSG WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CF_MSGTY = 'W'.
      MESSAGE ID LS_MSG-MSGID TYPE LS_MSG-MSGTYP NUMBER LS_MSG-MSGNR
      WITH LS_MSG-MSGV1 LS_MSG-MSGV2 LS_MSG-MSGV3 LS_MSG-MSGV4
      INTO CF_MSGTX.
    ELSE.
      CF_MSGTY = 'W'.
      CF_MSGTX = 'Error when creating partner function'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING PU_PROGRAM  TYPE ANY
                      PU_DYNPRO TYPE ANY
                CHANGING PT_BDCDATA TYPE TAB_BDCDATA.
  DATA: LS_BDCDATA LIKE LINE OF PT_BDCDATA.
  CLEAR LS_BDCDATA.
  LS_BDCDATA-PROGRAM  = PU_PROGRAM.
  LS_BDCDATA-DYNPRO   = PU_DYNPRO.
  LS_BDCDATA-DYNBEGIN = 'X'.
  APPEND LS_BDCDATA TO PT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING PU_FNAM  TYPE ANY
                     PU_FVAL  TYPE ANY
               CHANGING PT_BDCDATA TYPE TAB_BDCDATA.
  DATA: LS_BDCDATA LIKE LINE OF PT_BDCDATA.
  CLEAR LS_BDCDATA.

  LS_BDCDATA-FNAM = PU_FNAM.
  LS_BDCDATA-FVAL = PU_FVAL.
  APPEND LS_BDCDATA TO PT_BDCDATA.

ENDFORM.                    "BDC_FIELD
