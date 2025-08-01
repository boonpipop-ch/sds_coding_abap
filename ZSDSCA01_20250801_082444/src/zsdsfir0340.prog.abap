*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0340
*  Creation Date      : 20.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : GL Balance Conversion
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0340.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
*--------------------------------------
* Types for File Processing
*--------------------------------------
TYPES:
  BEGIN OF TS_RAW,
    TLINE TYPE  STRING,
  END OF TS_RAW.
TYPES:
  GTTY_RAW  TYPE  STANDARD TABLE OF TS_RAW.

TYPES:
  BEGIN OF TS_FILE.
    INCLUDE TYPE ZSDSFIS110.
TYPES:
    FILENAME TYPE FILENAMECI-FILEEXTERN,
  END OF TS_FILE.

*--------------------------------------
* Types for Data Processing
*--------------------------------------
TYPES:
  BEGIN OF TS_DATA.
    INCLUDE  TYPE ZSDSFIS111.
TYPES:
    MSG_TYPE   TYPE BAPI_MTYPE,
    ACCTTYP    TYPE BSEG-KOART,
    DCIND      TYPE BSEG-SHKZG,
    GLACCT     TYPE BSEG-HKONT,
    CUSTNO     TYPE BSEG-KUNNR,
    VENDNO     TYPE BSEG-LIFNR,
    NETWORK    TYPE COBL-NPLNR,
    NETITEM    TYPE COBL-VORNR,
    LINENO     TYPE SY-TABIX,
    AUTO       TYPE FLAG,
    USED       TYPE FLAG,
    WHTCONV    TYPE FLAG,
    AMTCAL_DOC TYPE BSEG-WRBTR,
    AMTCAL_LOC TYPE BSEG-DMBTR,
    AMTORG_DOC TYPE BSEG-WRBTR,
    AMTORG_LOC TYPE BSEG-DMBTR,
    APAR_LINE  TYPE SY-TABIX,
    SUMGL_DOC  TYPE BSEG-WRBTR,
    SUMGL_LOC  TYPE BSEG-DMBTR,
  END OF TS_DATA.

TYPES:
  BEGIN OF TS_TOTAMT,
    AMT_LOC TYPE BSEG-DMBTR,
    AMT_DOC TYPE BSEG-WRBTR,
  END OF TS_TOTAMT.

*--------------------------------------
* Types for master validation
*--------------------------------------
*->Key for 'FOR ALL ENTRIES'
TYPES:
  BEGIN OF TS_BUKRS,
    BUKRS TYPE T001-BUKRS,
  END OF TS_BUKRS.

TYPES:
  BEGIN OF TS_BLART,
    BLART TYPE T003-BLART,
  END OF TS_BLART.

TYPES:
  BEGIN OF TS_WAERS,
    WAERS TYPE TCURC-WAERS,
  END OF TS_WAERS.

TYPES:
  BEGIN OF TS_BRANCH,
    BUKRS  TYPE J_1BBRANCH-BUKRS,
    BRANCH TYPE J_1BBRANCH-BRANCH,
  END   OF TS_BRANCH.

TYPES:
  BEGIN OF TS_LIFNR,
    BUKRS TYPE LFB1-BUKRS,
    LIFNR TYPE LFB1-LIFNR,
  END   OF TS_LIFNR.

TYPES:
  BEGIN OF TS_KUNNR,
    BUKRS TYPE KNB1-BUKRS,
    KUNNR TYPE KNB1-KUNNR,
  END OF TS_KUNNR.

TYPES:
  BEGIN OF TS_SAKNR,
    BUKRS TYPE SKB1-BUKRS,
    SAKNR TYPE SKB1-SAKNR,
  END OF TS_SAKNR.

TYPES:
  BEGIN OF TS_KOSTL,
    KOKRS TYPE CSKS-KOKRS,
    KOSTL TYPE CSKS-KOSTL,
  END   OF TS_KOSTL.

TYPES:
  BEGIN OF TS_PRCTR,
    PRCTR TYPE CEPC-PRCTR,
    KOKRS TYPE CEPC-KOKRS,
  END OF TS_PRCTR.

TYPES:
  BEGIN OF TS_AUFNR,
    AUFNR TYPE AUFK-AUFNR,
  END OF TS_AUFNR.

TYPES:
  BEGIN OF TS_POSID,
    POSID TYPE PRPS-POSID,
  END OF TS_POSID.

TYPES:
  BEGIN OF TS_MWSKZ,
    MWSKZ TYPE T007A-MWSKZ,
  END OF TS_MWSKZ.

TYPES:
  BEGIN OF TS_EBELN,
    EBELN TYPE EKPO-EBELN,
    EBELP TYPE EKPO-EBELP,
  END OF TS_EBELN.

*->Master data
TYPES:
  BEGIN OF TS_T001,
    BUKRS TYPE T001-BUKRS,
    BUTXT TYPE T001-BUTXT,
    WAERS TYPE T001-WAERS,
    LAND1 TYPE T001-LAND1,
    KTOPL TYPE T001-KTOPL,
    KOKRS TYPE TKA02-KOKRS,
    KALSM TYPE T005-KALSM,
  END   OF TS_T001.

TYPES:
  BEGIN OF TS_LFB1,
    LIFNR TYPE LFB1-LIFNR,
    BUKRS TYPE LFB1-BUKRS,
    KTOKK TYPE LFA1-KTOKK,
    XCPDK TYPE LFA1-XCPDK,
    ZTERM TYPE LFB1-ZTERM,
  END   OF TS_LFB1.

TYPES:
  BEGIN OF TS_KNB1,
    KUNNR TYPE KNB1-KUNNR,
    BUKRS TYPE KNB1-BUKRS,
    ZTERM TYPE KNB1-ZTERM,
    LAND1 TYPE KNA1-LAND1,
    XCPDK TYPE KNA1-XCPDK,
  END OF TS_KNB1.

TYPES:
  BEGIN OF TS_SKB1,
    BUKRS TYPE SKB1-BUKRS,
    SAKNR TYPE SKB1-SAKNR,
  END OF TS_SKB1.

TYPES:
  BEGIN OF TS_CSKS,
    KOKRS TYPE CSKS-KOKRS,
    KOSTL TYPE CSKS-KOSTL,
    DATBI TYPE CSKS-DATBI,
    DATAB TYPE CSKS-DATAB,
  END   OF TS_CSKS.

TYPES:
  BEGIN OF TS_CEPC,
    PRCTR TYPE CEPC-PRCTR,
    KOKRS TYPE CEPC-KOKRS,
    DATBI TYPE CEPC-DATBI,
  END OF TS_CEPC.

TYPES:
  BEGIN OF TS_EKPO,
    EBELN TYPE EKPO-EBELN,
    EBELP TYPE EKPO-EBELP,
  END OF TS_EKPO.

TYPES:
  BEGIN OF TS_AUFK,
    AUFNR TYPE AUFK-AUFNR,
  END OF TS_AUFK.

TYPES:
  BEGIN OF TS_TAXLINE,
    KEY         TYPE CHAR13,
    TAXCODE     TYPE BSEG-MWSKZ,
    DCIND       TYPE BSEG-SHKZG,
    MWDAT       TYPE RTAX1U15,
    TAXDOC      TYPE BSEG-WMWST,
    TAXLOC      TYPE BSEG-MWSTS,
    REFKEY1     TYPE BSEG-XREF1,
    REFKEY2     TYPE BSEG-XREF2,
    ITEMTEXT    TYPE BSEG-SGTXT,
    CUSTNO      TYPE BSEG-KUNNR,
    VENDNO      TYPE BSEG-LIFNR,
    GLACCT      TYPE BSEG-HKONT,
    AMTBASE_DOC TYPE BSEG-WRBTR,
    AMTBASE_LOC TYPE BSEG-DMBTR,
    DIRECT_TAX  TYPE BAPIACTX09-DIRECT_TAX,
  END OF TS_TAXLINE.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1       VALUE 'X',
  GC_TCODE TYPE  SY-TCODE    VALUE 'ZSDSFI036',
* Splitter used in Raw data
  GC_SPLIT TYPE  CHAR1
                          VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.


*----------------------------------------------------------------------*
* VARIABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GV_ACTTYP TYPE BSEG-KOART.

*----------------------------------------------------------------------*
* INTERNAL STRUCTURE
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GV_CONTRA_GL TYPE BSEG-HKONT.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GT_FILE    TYPE TABLE OF TS_FILE,
  GT_DATA    TYPE TABLE OF TS_DATA,
  GT_TAXLINE TYPE TABLE OF TS_TAXLINE.

##NEEDED
DATA:
  GT_BUKRS TYPE TABLE OF TS_BUKRS,
  GT_BLART TYPE TABLE OF TS_BLART,
  GT_WAERS TYPE TABLE OF TS_WAERS,
  GT_BRNCH TYPE TABLE OF TS_BRANCH,
  GT_LIFNR TYPE TABLE OF TS_LIFNR,
  GT_KUNNR TYPE TABLE OF TS_KUNNR,
  GT_SAKNR TYPE TABLE OF TS_SAKNR,
  GT_KOSTL TYPE TABLE OF TS_KOSTL,
  GT_PRCTR TYPE TABLE OF TS_PRCTR,
  GT_AUFNR TYPE TABLE OF TS_AUFNR,
  GT_POSID TYPE TABLE OF TS_POSID,
  GT_MWSKZ TYPE TABLE OF TS_MWSKZ,
  GT_EBELN TYPE TABLE OF TS_EBELN.

##NEEDED
DATA:
  GT_T001       TYPE TABLE OF TS_T001,
  GT_TBSL       TYPE TABLE OF TBSL,
  GT_T003       TYPE TABLE OF T003,
  GT_TCURC      TYPE TABLE OF TCURC,
  GT_LFB1       TYPE TABLE OF TS_LFB1,
  GT_KNB1       TYPE TABLE OF TS_KNB1,
  GT_SKB1       TYPE TABLE OF TS_SKB1,
  GT_J_1BBRANCH TYPE TABLE OF J_1BBRANCH,
  GT_CSKS       TYPE TABLE OF TS_CSKS,
  GT_CEPC       TYPE TABLE OF TS_CEPC,
  GT_AUFK       TYPE TABLE OF TS_AUFK,
  GT_T007A      TYPE TABLE OF T007A,
  GT_T030K      TYPE TABLE OF T030K,
  GT_EKPO       TYPE TABLE OF TS_EKPO.


*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
##NEEDED
CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 22,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 78.

*----------------------------------------------------------------------*
* BAPI Variables
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GV_ITEMNO   TYPE BAPIACAP09-ITEMNO_ACC,
  GV_SPDOWN   TYPE FLAG,
  GV_POSTINTF TYPE FLAG.

##NEEDED
DATA:
  GS_DOCUMENTHEADER TYPE BAPIACHE09,
  GS_CUSTOMERCPD    TYPE BAPIACPA09,            "One Time Customer/Vendor
  GT_ACCOUNTGL      TYPE TABLE OF BAPIACGL09,   "GL
  GT_ACCOUNTAP      TYPE TABLE OF BAPIACAP09,   "AP
  GT_ACCOUNTAR      TYPE TABLE OF BAPIACAR09,   "AP
  GT_ACCOUNTTX      TYPE TABLE OF BAPIACTX09,   "Tax
  GT_ACCOUNTWT      TYPE TABLE OF BAPIACWT09,   "WHT
  GT_CURRENCYAMOUNT TYPE TABLE OF BAPIACCR09,   "Currency
  GT_COPA           TYPE TABLE OF BAPIACKEC9,   "COPA
  GT_EXTENSION2     TYPE TABLE OF BAPIPAREX,
  GT_CRITERIA       TYPE TABLE OF BAPIACKEC9,
  GT_RETURN         TYPE TABLE OF BAPIRET2  .   "Return


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
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
*  PARAMETERS:
*    RB_LOCAL TYPE  FLAG RADIOBUTTON GROUP G1 DEFAULT 'X'
*                         USER-COMMAND DMY.
* Text-s02: Local File
* SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
    PARAMETERS:
      P_LFILE  TYPE  STRING LOWER CASE MODIF ID LOC.
    SELECTION-SCREEN BEGIN OF LINE.
*     Text-s03: Start Row
      SELECTION-SCREEN COMMENT 1(29) TEXT-S03 FOR FIELD P_BEGROW.
      PARAMETERS:
        P_BEGROW TYPE I DEFAULT 11 MODIF ID LOC.
*     Text-s04: Start Column
      SELECTION-SCREEN COMMENT 50(15) TEXT-S04 FOR FIELD P_BEGCOL.
      PARAMETERS:
        P_BEGCOL TYPE I DEFAULT 2 MODIF ID LOC.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
*     Text-s05: End Row
      SELECTION-SCREEN COMMENT 1(29) TEXT-S05 FOR FIELD P_ENDROW .
*      MODIF ID NAC.
      PARAMETERS:
        P_ENDROW TYPE I DEFAULT 9999  .
*        MODIF ID NAC.
*     Text-s06: End Column
      SELECTION-SCREEN COMMENT 50(15) TEXT-S06 FOR FIELD P_ENDCOL .
*      MODIF ID NAC.
      PARAMETERS:
        P_ENDCOL TYPE I DEFAULT 72 .
*        MODIF ID NAC.
    SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN END OF BLOCK B2.
*  PARAMETERS:
*    RB_APPSV TYPE  FLAG RADIOBUTTON GROUP G1.
*
** Text-s07: Application Server File
*  SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S07.
*    PARAMETERS:
*      P_AFPATH TYPE  TEXT1000 VISIBLE LENGTH 50 MODIF ID APP,
*      P_AFNAME TYPE  FILENAMECI-FILEEXTERN MODIF ID APP,
*      P_AFILE  TYPE  STRING LOWER CASE NO-DISPLAY.
*  SELECTION-SCREEN END OF BLOCK B3.

  PARAMETERS:
    CB_TEST TYPE  FLAG AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK_TCODE USING GC_TCODE.
*  PERFORM F_GET_GENC.
*  PERFORM F_INIT_SELECTION_SCR.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LFILE.
* List Local input File
  PERFORM F_LIST_IFILE CHANGING P_LFILE.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_AFNAME.
** List Application Server File
*  PERFORM F_LIST_AFILE  USING  P_AFPATH
*                      CHANGING P_AFNAME.

AT SELECTION-SCREEN OUTPUT.
* Set Screen format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'ONLI'.
      PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDCASE.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Upload FI documment
  PERFORM F_PROCESS_DATA.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

* Display ALV Report
  PERFORM F_DISPLAY_RESULT USING GT_DATA.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.


*----------------------------------------------------------------------*
* FORM Routines
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_authorize_check
*&---------------------------------------------------------------------*
*& Check Authorization on t-code
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK_TCODE  USING PV_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD PV_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH PV_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_set_selection_screen_format
*----------------------------------------------------------------------*
*  Set Selection Screen format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .

  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.

*     --------------------------
*     For Local File Criteria
*     --------------------------
*      WHEN 'LOC'.
*        IF RB_LOCAL EQ GC_TRUE.
*          SCREEN-INPUT = 1.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
*        MODIFY SCREEN.
*
**     --------------------------
**     For AppServer File Criteria
**     --------------------------
*      WHEN 'APP'.
*        IF RB_APPSV EQ GC_TRUE.
*          SCREEN-INPUT = 1.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
*        MODIFY SCREEN.

*     --------------------------
*     For Display only fields
*     --------------------------
      WHEN 'DIS'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.

*     --------------------------
*     For No-Display fields
*     --------------------------
      WHEN 'NAC'.
        SCREEN-INVISIBLE = 1.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.

    ENDCASE.


  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_list_ifile
*&---------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING PV_FILENAME  TYPE  STRING.

  DATA:
    LT_FILE     TYPE  FILETABLE.

  DATA:
    LV_RC     TYPE  I,
    LV_ACTION TYPE  I.

  FIELD-SYMBOLS:
    <LFS_FILE>  TYPE  FILE_TABLE.


* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LV_RC
      USER_ACTION             = LV_ACTION
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
  IF NOT ( LV_ACTION IS INITIAL AND
           LV_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <LFS_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  PV_FILENAME = <LFS_FILE>-FILENAME.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_selection_screen
*&---------------------------------------------------------------------*
*& Validate Selection Screen
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  PERFORM F_VALIDATE_LOCAL_PARAM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_local_param
*&---------------------------------------------------------------------*
FORM F_VALIDATE_LOCAL_PARAM .

  DATA:
    LV_EXIST TYPE  FLAG.

* Input file is required
  IF P_LFILE IS INITIAL.
    SET CURSOR FIELD 'P_LFILE'.
*   Text-E01 Please enter a valid filename.
    MESSAGE E000(ZSDSCA01) WITH TEXT-E01 '' '' ''.
    RETURN.
  ENDIF.
* Check File Exists?
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
    EXPORTING
      FILE                 = P_LFILE
    RECEIVING
      RESULT               = LV_EXIST
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.
  IF SY-SUBRC <> 0.
    CLEAR LV_EXIST.
  ENDIF.
  IF LV_EXIST IS INITIAL.
    SET CURSOR FIELD 'P_LFILE'.
*   Text-E02: File does not exist.
    MESSAGE E000(ZSDSCA01) WITH TEXT-E02.
    RETURN.
  ENDIF.
* Ranges are required
  IF P_BEGROW IS INITIAL OR
     P_BEGCOL IS INITIAL OR
     P_ENDROW IS INITIAL OR
     P_ENDCOL IS INITIAL.
    SET CURSOR FIELD 'P_BEGROW'.
*   Text-E03: Please specified range to read excel file.
    MESSAGE E000(ZSDSCA01) WITH TEXT-E03 '' '' ''.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_read_input_file
*----------------------------------------------------------------------*
*  Read input file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_INPUT_FILE.

  DATA:
    LS_FILE   TYPE TS_FILE,
    LS_RETURN TYPE  BAPIRET2,
    LT_RAW    TYPE  GTTY_RAW.

  FIELD-SYMBOLS:
    <LFS_RAW>   TYPE  TS_RAW.

* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

*  CASE GC_TRUE.
*    WHEN RB_LOCAL.
  PERFORM F_READ_EXCEL USING   P_LFILE
                               P_BEGROW
                               P_BEGCOL
                               P_ENDROW
                               P_ENDCOL
                      CHANGING LT_RAW
                               LS_RETURN.
  LOOP AT LT_RAW ASSIGNING <LFS_RAW>.
    CLEAR LS_FILE.
*       Translate Raw line into internal Structure
    PERFORM F_TRANSLATE_RAW  USING  <LFS_RAW>
                          CHANGING  LS_FILE.
    LS_FILE-FILENAME = P_LFILE.
    APPEND LS_FILE TO GT_FILE.
  ENDLOOP.

  IF LS_RETURN IS NOT INITIAL.
    MESSAGE LS_RETURN-MESSAGE TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF LT_RAW[] IS INITIAL.
*   Text-E04: No data found.
    MESSAGE S000(ZSDSCA01) WITH TEXT-E04 '' '' ''.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_read_excel
*----------------------------------------------------------------------*
*  Read Excel file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_EXCEL  USING  PV_IFILE   TYPE CLIKE
                          PV_BEGROW  TYPE I
                          PV_BEGCOL  TYPE I
                          PV_ENDROW  TYPE I
                          PV_ENDCOL  TYPE I
                 CHANGING PT_RAW     TYPE GTTY_RAW
                          PS_RETURN  TYPE BAPIRET2.

  DATA:
    LT_TAB   TYPE  STANDARD TABLE OF ZSDSCAS016.

  DATA:
    LS_RAW       TYPE  TS_RAW.

  DATA:
    LV_FILENAME TYPE  STRING,
    LV_BEGROW   TYPE  I,
    LV_BEGCOL   TYPE  I,
    LV_ROW      TYPE  I,
    LV_COL      TYPE  I.

  FIELD-SYMBOLS:
    <LFS_TAB>  TYPE  ZSDSCAS016.


* Initialize Output
  CLEAR: PT_RAW.
  CLEAR PS_RETURN.

* Assign File name
  LV_FILENAME = PV_IFILE.

  LV_BEGCOL = PV_BEGCOL.
  LV_BEGROW = PV_BEGROW.

  CALL FUNCTION 'Z_SDSCA_EXCEL_TO_ITAB'
    EXPORTING
      FILENAME                = LV_FILENAME
      I_BEGIN_COL             = LV_BEGCOL
      I_BEGIN_ROW             = LV_BEGROW
      I_END_COL               = PV_ENDCOL
      I_END_ROW               = PV_ENDROW
    TABLES
      INTERN                  = LT_TAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    PERFORM F_SET_RET_MSG USING 'E' 'ZTEC' '021'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_RETURN.
    RETURN.
  ENDIF.

  SORT LT_TAB BY ROW ASCENDING
                 COL ASCENDING.

  LOOP AT LT_TAB ASSIGNING <LFS_TAB>.

*   -----------
*   On New Row
*   -----------
    IF <LFS_TAB>-ROW NE LV_ROW.

      IF LV_ROW IS NOT INITIAL.
        INSERT LS_RAW INTO TABLE PT_RAW.
      ENDIF.

*     Initialize New Line
      LV_ROW = <LFS_TAB>-ROW.
      LV_COL = 1.
      CLEAR LS_RAW.

    ENDIF.

*   -----------
*   Add Blank Cell
*   -----------
    WHILE LV_COL LT <LFS_TAB>-COL.
      IF LV_COL GT 1.
        CONCATENATE LS_RAW-TLINE GC_SPLIT
               INTO LS_RAW-TLINE.
      ENDIF.
      LV_COL = LV_COL + 1.
    ENDWHILE.

*   -----------
*   Assign column value
*   -----------
    IF LV_COL EQ 1.
      LS_RAW-TLINE = <LFS_TAB>-VALUE.
    ELSE.
      CONCATENATE LS_RAW-TLINE <LFS_TAB>-VALUE
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

    LV_COL = LV_COL + 1.

*   Insert last line
    AT LAST.
      INSERT LS_RAW INTO TABLE PT_RAW.
    ENDAT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_translate_raw
*----------------------------------------------------------------------*
*  Convert Raw to Data
*----------------------------------------------------------------------*
FORM F_TRANSLATE_RAW  USING  US_RAW     TYPE  TS_RAW
                    CHANGING CS_FILE    TYPE  TS_FILE.

  DATA:
    LT_SPLIT TYPE STANDARD TABLE OF STRING,
*    LS_SPLIT LIKE LINE OF LT_SPLIT,
    LV_SPLIT TYPE CHAR1.

  FIELD-SYMBOLS:
    <LFS_VALUE>  TYPE ANY.

*  IF RB_LOCAL EQ GC_TRUE.
*    LV_SPLIT = GC_SPLIT.
*  ELSE.
*    LV_SPLIT = GC_SPLIT_APP.
*  ENDIF.
  LV_SPLIT = GC_SPLIT.

* Split Into Fields
  SPLIT US_RAW-TLINE AT LV_SPLIT INTO TABLE LT_SPLIT.
  LOOP AT LT_SPLIT ASSIGNING FIELD-SYMBOL(<L_SPLIT>).
    ASSIGN COMPONENT SY-TABIX OF STRUCTURE CS_FILE TO <LFS_VALUE>.
    IF SY-SUBRC EQ 0.
      <LFS_VALUE> = <L_SPLIT>.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*       Set return message
*       1. Fill status in ZSFAP004_LIS-STATUS
*       2. Fill message in ZSFAP004_LIS-MESSAGE
*----------------------------------------------------------------------*
FORM F_SET_MESSAGE  USING PV_TYPE    TYPE BAPI_MTYPE
                          PV_ID      TYPE SYMSGID
                          PV_NUMBER  TYPE SYMSGNO
                          PV_MSG1    TYPE ANY
                          PV_MSG2    TYPE ANY
                          PV_MSG3    TYPE ANY
                          PV_MSG4    TYPE ANY
                 CHANGING PV_STATUS  TYPE ICON_D
                          PV_MESSAGE TYPE BAPI_MSG.

  MESSAGE ID PV_ID TYPE PV_TYPE NUMBER PV_NUMBER
          INTO PV_MESSAGE
          WITH PV_MSG1 PV_MSG2 PV_MSG3 PV_MSG4.

  CASE PV_TYPE.
    WHEN 'E'.
      PV_STATUS      = ICON_RED_LIGHT.
    WHEN 'W'.
      PV_STATUS      = ICON_YELLOW_LIGHT.
    WHEN 'S'.
      PV_STATUS      = ICON_GREEN_LIGHT.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " f_set_message
*&---------------------------------------------------------------------*
*&      Form  F_SET_RET_MSG
*&---------------------------------------------------------------------*
*       Set return message
*----------------------------------------------------------------------*
FORM F_SET_RET_MSG  USING    PV_MSTSP   TYPE BAPI_MTYPE
                             PV_MSGID    TYPE SYMSGID
                             PV_MSGNO    TYPE SYMSGNO
                             PV_MSGV1    TYPE ANY
                             PV_MSGV2    TYPE ANY
                             PV_MSGV3    TYPE ANY
                             PV_MSGV4    TYPE ANY
                    CHANGING PS_RTN_MSG  TYPE BAPIRET2.

  PS_RTN_MSG-TYPE   = PV_MSTSP.
  PS_RTN_MSG-ID     = PV_MSGID.
  PS_RTN_MSG-NUMBER = PV_MSGNO.
  MESSAGE ID PV_MSGID  TYPE PV_MSTSP NUMBER PV_MSGNO
                      INTO PS_RTN_MSG-MESSAGE
                      WITH PV_MSGV1 PV_MSGV2 PV_MSGV3 PV_MSGV4.

ENDFORM.                    " F_SET_RET_MSG
*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*       Conversion Exit
*----------------------------------------------------------------------*
FORM F_CONVERSION_EXIT USING VALUE(PV_EXIT)  TYPE ANY
                             VALUE(PV_TYPE)  TYPE ANY
                             VALUE(PV_INPUT) TYPE ANY
                       CHANGING PV_OUTPUT    TYPE ANY.
  DATA:
    LV_CONEXIT TYPE CHAR30.

  CONCATENATE 'CONVERSION_EXIT_' PV_EXIT '_' PV_TYPE INTO LV_CONEXIT.

  PV_OUTPUT = PV_INPUT.
  CALL FUNCTION LV_CONEXIT
    EXPORTING
      INPUT  = PV_OUTPUT
    IMPORTING
      OUTPUT = PV_OUTPUT.

ENDFORM.                    "F_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATE_INTERNAL
*&---------------------------------------------------------------------*
*       Conversion Exit
*----------------------------------------------------------------------*
FORM F_CONVERT_DATE_INTERNAL USING VALUE(PV_INPUT)  TYPE ANY
                          CHANGING PV_OUTPUT TYPE ANY.
* File Date format DD.MM.YYYY
  CLEAR PV_OUTPUT.
  PV_OUTPUT+0(4) = PV_INPUT+6(4).
  PV_OUTPUT+4(2) = PV_INPUT+3(2).
  PV_OUTPUT+6(2) = PV_INPUT+0(2).

ENDFORM.                    "F_CONVERT_DATE_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  F_CAL_TAX_FROM_GROSS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CAL_TAX_FROM_GROSS  USING PV_CCODE   TYPE BKPF-BUKRS
                                 PV_TAXCODE TYPE BSEG-MWSKZ
                                 PV_CRNCY   TYPE BKPF-WAERS
                                 PV_AMT     TYPE BSEG-WRBTR
                        CHANGING PS_MWDAT   TYPE RTAX1U15.

  DATA:
    LT_MWDAT TYPE TABLE OF RTAX1U15,
    LS_MWDAT TYPE          RTAX1U15.

  CLEAR PS_MWDAT.

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      I_BUKRS                 = PV_CCODE
      I_MWSKZ                 = PV_TAXCODE
      I_WAERS                 = PV_CRNCY
      I_WRBTR                 = PV_AMT
    TABLES
      T_MWDAT                 = LT_MWDAT
    EXCEPTIONS
      BUKRS_NOT_FOUND         = 1
      COUNTRY_NOT_FOUND       = 2
      MWSKZ_NOT_DEFINED       = 3
      MWSKZ_NOT_VALID         = 4
      ACCOUNT_NOT_FOUND       = 5
      DIFFERENT_DISCOUNT_BASE = 6
      DIFFERENT_TAX_BASE      = 7
      TXJCD_NOT_VALID         = 8
      NOT_FOUND               = 9
      KTOSL_NOT_FOUND         = 10
      KALSM_NOT_FOUND         = 11
      PARAMETER_ERROR         = 12
      KNUMH_NOT_FOUND         = 13
      KSCHL_NOT_FOUND         = 14
      UNKNOWN_ERROR           = 15
      OTHERS                  = 16.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ELSE.
    CLEAR LS_MWDAT.
    READ TABLE LT_MWDAT INTO LS_MWDAT INDEX 1.
    PS_MWDAT = LS_MWDAT.
  ENDIF.

ENDFORM.                    " F_CAL_TAX

*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CF
*&---------------------------------------------------------------------*
*       Popup to confirm to proceed furhter step
*----------------------------------------------------------------------*
FORM F_POPUP_TO_CF USING PV_QUEST  TYPE ANY
                CHANGING PV_ANSWER TYPE C.

  DATA:
    LV_ANSWER     TYPE C.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = PV_QUEST
      ICON_BUTTON_1         = 'ICON_OKAY'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = SPACE
      POPUP_TYPE            = 'ICON_MESSAGE_WARNING'
    IMPORTING
      ANSWER                = LV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    PV_ANSWER = LV_ANSWER.
  ENDIF.

ENDFORM.                    " POPUP_TO_CF
*&---------------------------------------------------------------------*
*& Form F_CAL_WITHAMT
*&---------------------------------------------------------------------*
*FORM F_CAL_WITHAMT   USING PV_WITHT   TYPE WITHT
*                           PV_WITHCD  TYPE WT_WITHCD
*                           PV_CCODE   TYPE T001-BUKRS
*                           PV_WITHBASE TYPE WITH_ITEM-WT_QSSHB
*                  CHANGING PV_WITHAMT  TYPE WITH_ITEM-WT_QBSHB.
*
*  DATA:
*    LT_T059Z TYPE TABLE OF T059Z,
*    LS_T059Z TYPE T059Z.
*
*  CLEAR PV_WITHAMT.
*  CALL FUNCTION 'FI_WT_READ_T059Z'
*    EXPORTING
*      I_BUKRS     = PV_CCODE
*      I_TYPE      = PV_WITHT
*      I_WT_WITHCD = PV_WITHCD
*    TABLES
*      T_T059Z     = LT_T059Z
*    EXCEPTIONS
*      NOT_FOUND   = 1
*      OTHERS      = 2.
*  IF SY-SUBRC EQ 0.
*    READ TABLE LT_T059Z INTO LS_T059Z INDEX 1.
*    PV_WITHAMT = PV_WITHBASE * LS_T059Z-QSATZ / 100.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA.

  DATA:
    LV_QUEST  TYPE STRING,
    LV_ANSWER TYPE C.

  DATA:
    LW_DATA TYPE TS_DATA               ##NEEDED.

*--------------------------------------
* Confirm 'Actual Run'
*--------------------------------------
  IF CB_TEST EQ SPACE.
    " Are you sure to confirm posting?
    LV_QUEST = TEXT-T01.
*   Confirm to &1?
    PERFORM F_POPUP_TO_CF   USING LV_QUEST
                         CHANGING LV_ANSWER.
    CASE LV_ANSWER.
      WHEN '1'.               "Yes (button1)
*       Continue execute report
      WHEN '2'.               "No
        LEAVE LIST-PROCESSING.
      WHEN 'A'.               "Cancel
        LEAVE LIST-PROCESSING.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_INPUT_FILE.

* --------------------------------
* Step2: Convert data from file into SAP format
* --------------------------------
  PERFORM F_PREPARE_DATA.

* --------------------------------
* Step3: Validate file data
* --------------------------------
  PERFORM F_VALIDATE_DATA.

* --------------------------------
* Step4: Upload data ('Test Run'/'Actual Run')
* --------------------------------
  IF CB_TEST EQ 'X'.
    PERFORM F_UPLOAD_DATA USING CB_TEST.
  ELSE.
    PERFORM F_UPLOAD_DATA USING 'X'.
*   Only post when all items are green
    READ TABLE GT_DATA WITH KEY STATUS = ICON_RED_LIGHT
                                INTO LW_DATA.
    IF SY-SUBRC NE 0.
      PERFORM F_UPLOAD_DATA USING CB_TEST.
    ENDIF.
  ENDIF.

  DELETE GT_DATA WHERE AUTO = 'X'.
  DELETE GT_DATA WHERE WHTCONV = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING PT_RESULT TYPE TABLE.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Soft refresh data
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT USING 'ZSDSFIS113'
                            CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING PS_LAYOUT  TYPE  LVC_S_LAYO
                           PS_VARIANT TYPE  DISVARIANT
                           PS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  PS_LAYOUT, PS_VARIANT, PS_PRINT.

* determine layout
  PS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  PS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  PS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  PS_VARIANT-REPORT  = SY-REPID.

  PS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT USING PV_STRUCTURE TYPE TABNAME
                       CHANGING PT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT      TYPE TEXT60,
    LV_FIELDNAME TYPE CHAR35,
    LV_NUMC2     TYPE NUMC2.

  DATA:
    LT_FIELDCAT TYPE LVC_T_FCAT.
*    LW_FIELDCAT TYPE LVC_S_FCAT.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT> TYPE  LVC_S_FCAT,
    <LFS_VALUE>    TYPE ANY.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  PV_STRUCTURE
                              CHANGING PT_FIELDCAT.

*--------------------------------------
* Assign Text Elements to Columns Texts
*--------------------------------------
* Numbering columns from ZSFI_FI_UPLOAD_ALV
* Get List of Data File fields (for Text element numbering)
  PERFORM F_PREPARE_FIELDCAT_O  USING  'ZSDSFIS113'
                              CHANGING LT_FIELDCAT.
* Start Column1 at Data field
* TEXT-A01 for column1
* TEXT-A02 for column2
* Exclude Columns (which defined below)
  LOOP AT LT_FIELDCAT ASSIGNING FIELD-SYMBOL(<L_FIELDCAT>).
    LV_NUMC2 = SY-TABIX.
    CONCATENATE 'TEXT-A' LV_NUMC2
      INTO LV_FIELDNAME.
    ASSIGN (LV_FIELDNAME) TO <LFS_VALUE>.
    IF SY-SUBRC EQ 0.
      LV_TEXT = <LFS_VALUE>.

      READ TABLE PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>
        WITH KEY FIELDNAME = <L_FIELDCAT>-FIELDNAME.
      IF SY-SUBRC EQ 0.
        <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
        <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>.
    CASE <LFS_FIELDCAT>-FIELDNAME.
      WHEN 'STATUS'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'FIDOC'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'FIYEAR'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'TRANS_NO'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'GRPNO'.
        <LFS_FIELDCAT>-NO_OUT = 'X'.
      WHEN OTHERS.
    ENDCASE.

    IF <LFS_FIELDCAT>-INTTYPE EQ 'P'.
      <LFS_FIELDCAT>-NO_ZERO = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING PT_SORT TYPE LVC_T_SORT.

*  DATA:
*    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING PV_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LV_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LV_TEMP1     TYPE  TEXT50,
    LV_TEMP2     TYPE  TEXT50,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD PV_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Program:
  LV_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  CONCATENATE SY-REPID SY-TITLE
         INTO LV_TEXT
    SEPARATED BY '-'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Processing Date/Time:
  LV_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : System/Client:
  LV_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Process By:
  LV_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  LV_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line 5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
  IF CB_TEST EQ 'X'.
*   Text-h08 : Test Run
    LV_TEXT = TEXT-H08.
  ELSE.
*   Text-h11 : Update Run
    LV_TEXT = TEXT-H11.
  ENDIF.
  CALL METHOD LREF_TABLE->SPAN_COLUMNS
    EXPORTING
      COL_START_SPAN = LREF_COL_KEY
      NO_OF_COLS     = 2.
  CALL METHOD LREF_TABLE->SET_ROW_STYLE
    EXPORTING
      ROW_NO       = 9
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LV_COL01 TYPE  I VALUE 25,
    LV_COL02 TYPE  I VALUE 35,
    LV_TEXT  TYPE  TEXT50,
    LV_TEMP1 TYPE  TEXT50,
    LV_TEMP2 TYPE  TEXT50.


  CONCATENATE SY-REPID SY-TITLE
         INTO LV_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  WRITE AT: /1(LV_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  WRITE AT: /1(LV_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  WRITE AT: /1(LV_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

* Text-h04 : Process By:
  WRITE AT: /1(LV_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LV_COL02)    SY-UNAME NO-GAP.

  IF CB_TEST EQ 'X'.
*   Text-h08 : Test Run
    LV_TEXT = TEXT-H08.
  ELSE.
*   Text-h11 : Update Run
    LV_TEXT = TEXT-H11.
  ENDIF.
  WRITE AT: /1(20) LV_TEXT CENTERED COLOR COL_TOTAL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_1
*&---------------------------------------------------------------------*
*       TO handle User command
*----------------------------------------------------------------------*
*      -->PFD_UCOMM  text
*----------------------------------------------------------------------*
##CALLED
FORM F_USER_COMMAND_1 USING    PV_UCOMM TYPE SY-UCOMM ##NEEDED.


ENDFORM.                    " F_HANDLE_USER_COMMAND

*&---------------------------------------------------------------------*
*& Form f_prepare_header
*&---------------------------------------------------------------------*
FORM F_PREPARE_HEADER CHANGING PS_FILE TYPE TS_FILE.

  DATA:
    LW_BUKRS TYPE TS_BUKRS,
    LW_BLART TYPE TS_BLART,
    LW_WAERS TYPE TS_WAERS,
    LW_BRNCH TYPE TS_BRANCH.

  REPLACE ALL OCCURRENCES OF ',' IN:
    PS_FILE-EXCHRATE WITH ''.

*--------------------------------------
* Convert to SAP format
*--------------------------------------
  PERFORM F_CONVERT_DATE_INTERNAL USING PS_FILE-DOCDATE
                               CHANGING PS_FILE-DOCDATE.
  PERFORM F_CONVERT_DATE_INTERNAL USING PS_FILE-POSDATE
                               CHANGING PS_FILE-POSDATE.
  IF PS_FILE-CCODE IS NOT INITIAL.
    LW_BUKRS-BUKRS = PS_FILE-CCODE.
    COLLECT LW_BUKRS INTO GT_BUKRS.
  ENDIF.

  IF PS_FILE-DOCTYP IS NOT INITIAL.
    LW_BLART-BLART = PS_FILE-DOCTYP.
    COLLECT LW_BLART INTO GT_BLART.
  ENDIF.

  IF PS_FILE-CRNCY_DOC IS NOT INITIAL.
    LW_WAERS-WAERS = PS_FILE-CRNCY_DOC.
    COLLECT LW_WAERS INTO GT_WAERS.
  ENDIF.

  IF PS_FILE-BRNCH IS NOT INITIAL.
    LW_BRNCH-BUKRS  = PS_FILE-CCODE.
    LW_BRNCH-BRANCH = PS_FILE-BRNCH.
    COLLECT LW_BRNCH INTO GT_BRNCH.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_item
*&---------------------------------------------------------------------*
FORM F_PREPARE_ITEM  CHANGING PS_FILE TYPE TS_FILE
                              PS_DATA TYPE TS_DATA.

  DATA:
    LW_MWSKZ TYPE TS_MWSKZ,
    LW_LIFNR TYPE TS_LIFNR,
    LW_KUNNR TYPE TS_KUNNR,
    LW_SAKNR TYPE TS_SAKNR,
    LW_KOSTL TYPE TS_KOSTL,
    LW_PRCTR TYPE TS_PRCTR,
    LW_AUFNR TYPE TS_AUFNR,
    LW_POSID TYPE TS_POSID,
    LW_TBSL  TYPE TBSL,
    LW_EBELN TYPE TS_EBELN.

  REPLACE ALL OCCURRENCES OF ',' IN:
    PS_FILE-AMT_DOC WITH '',
    PS_FILE-AMT_LOC WITH '',
    PS_FILE-TAXBASE_LOC WITH '',
    PS_FILE-TAXAMT_LOC WITH '',
    PS_FILE-TAXBASE_DOC WITH '',
    PS_FILE-TAXAMT_DOC WITH '',
    PS_FILE-WHTBASE1 WITH ',',
    PS_FILE-WHTAMT1 WITH ',',
    PS_FILE-WHTBASE2 WITH ',',
    PS_FILE-WHTAMT2 WITH ','.

*-------------------------------
* Convert to SAP Format
*-------------------------------
  IF PS_FILE-VALDATE IS NOT INITIAL.
    PERFORM F_CONVERT_DATE_INTERNAL USING PS_FILE-VALDATE
                                 CHANGING PS_FILE-VALDATE.
  ENDIF.
  IF PS_FILE-BLDATE IS NOT INITIAL.
    PERFORM F_CONVERT_DATE_INTERNAL USING PS_FILE-BLDATE
                                 CHANGING PS_FILE-BLDATE.
  ENDIF.

  READ TABLE GT_TBSL WITH KEY BSCHL = PS_FILE-POSKEY
                              INTO LW_TBSL.
  CASE LW_TBSL-KOART.
    WHEN 'D'.
      IF PS_FILE-SAPACCT IS NOT INITIAL.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-SAPACCT
                               CHANGING PS_DATA-CUSTNO.
      ENDIF.
    WHEN 'K'.
      IF PS_FILE-SAPACCT IS NOT INITIAL.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-SAPACCT
                               CHANGING PS_DATA-VENDNO.
      ENDIF.
    WHEN 'S'.
      IF PS_FILE-SAPACCT IS NOT INITIAL.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-SAPACCT
                               CHANGING PS_DATA-GLACCT.
      ENDIF.
  ENDCASE.
  PS_DATA-ACCTTYP = LW_TBSL-KOART.

  IF PS_FILE-PONO IS NOT INITIAL.
    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-PONO
                           CHANGING PS_FILE-PONO.
  ENDIF.

  IF PS_FILE-TRAD_PARTNER IS NOT INITIAL.
    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-TRAD_PARTNER
                           CHANGING PS_FILE-TRAD_PARTNER.
  ENDIF.
  IF PS_FILE-COSTCTR IS NOT INITIAL.
    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-COSTCTR
                           CHANGING PS_FILE-COSTCTR.
  ENDIF.
  IF PS_FILE-PROFITCTR IS NOT INITIAL.
    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-PROFITCTR
                           CHANGING PS_FILE-PROFITCTR.
  ENDIF.
  IF PS_FILE-WBS_ELEM IS NOT INITIAL.
    PERFORM F_CONVERSION_EXIT USING 'ABPSN' 'INPUT' PS_FILE-WBS_ELEM
                           CHANGING PS_FILE-WBS_ELEM.
  ENDIF.
  IF PS_FILE-ORDERNO IS NOT INITIAL.
    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-ORDERNO
                           CHANGING PS_FILE-ORDERNO.
  ENDIF.

  IF PS_FILE-PSEGMENT IS NOT INITIAL .
    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' PS_FILE-PSEGMENT
                         CHANGING PS_FILE-PSEGMENT.
  ENDIF.

*--------------------------------------
* Keep to select data for validation
*--------------------------------------
  IF PS_DATA-VENDNO IS NOT INITIAL.
    LW_LIFNR-LIFNR = PS_DATA-VENDNO.
    COLLECT LW_LIFNR INTO GT_LIFNR.
  ENDIF.
  IF PS_DATA-CUSTNO IS NOT INITIAL.
    LW_KUNNR-KUNNR = PS_DATA-CUSTNO.
    COLLECT LW_KUNNR INTO GT_KUNNR.
  ENDIF.
  IF PS_DATA-GLACCT IS NOT INITIAL.
    LW_SAKNR-SAKNR = PS_DATA-GLACCT.
    COLLECT LW_SAKNR INTO GT_SAKNR.
  ENDIF.

  IF PS_FILE-TAXCODE IS NOT INITIAL.
    LW_MWSKZ-MWSKZ = PS_FILE-TAXCODE.
    COLLECT LW_MWSKZ INTO GT_MWSKZ.
  ENDIF.

  IF PS_FILE-COSTCTR IS NOT INITIAL.
    LW_KOSTL-KOSTL = PS_FILE-COSTCTR.
    COLLECT LW_KOSTL INTO GT_KOSTL.
  ENDIF.
  IF PS_FILE-PROFITCTR IS NOT INITIAL.
    LW_PRCTR-PRCTR = PS_FILE-PROFITCTR.
    COLLECT LW_PRCTR INTO GT_PRCTR.
  ENDIF.
  IF PS_FILE-WBS_ELEM IS NOT INITIAL.
    LW_POSID-POSID = PS_FILE-WBS_ELEM.
    COLLECT LW_POSID INTO GT_POSID.
  ENDIF.
  IF PS_FILE-ORDERNO IS NOT INITIAL.
    LW_AUFNR-AUFNR = PS_FILE-ORDERNO.
    COLLECT LW_AUFNR INTO GT_AUFNR.
  ENDIF.
  IF PS_FILE-PONO IS NOT INITIAL.
    LW_EBELN-EBELN = PS_FILE-PONO.
    LW_EBELN-EBELP = PS_FILE-POITEM.
    COLLECT LW_EBELN INTO GT_EBELN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_PREPARE_DATA.

  DATA:
    LW_FILE TYPE TS_FILE,
    LW_DATA TYPE TS_DATA.

  DATA:
    LREF_EXCEPT  TYPE  REF TO CX_ROOT.

* Show Progress
* Text-p02 : Converting data from file into SAP format...
  MC_SHOW_PROGRESS 20 TEXT-P02.

* Posting key
  SELECT *           "#EC CI_NOORDER
  FROM TBSL
  INTO TABLE GT_TBSL.
  SORT GT_TBSL BY BSCHL.

* Prepare data into SAP format
  LOOP AT GT_FILE INTO LW_FILE.

    CLEAR LW_DATA.

    PERFORM F_PREPARE_HEADER CHANGING LW_FILE.

    PERFORM F_PREPARE_ITEM CHANGING LW_FILE
                                    LW_DATA.

    TRY.

        MOVE-CORRESPONDING LW_FILE TO LW_DATA.

      CATCH CX_SY_CONVERSION_NO_NUMBER
        INTO LREF_EXCEPT.
*       Unable to interpret value as a number.
        PERFORM F_SET_MESSAGE USING 'E'  'ZTEC' '033'
                                    SPACE
                                    SPACE
                                    SPACE
                                    SPACE
                           CHANGING LW_DATA-STATUS
                                    LW_DATA-MESSAGE.
        LW_DATA-MESSAGE =       LREF_EXCEPT->GET_TEXT( ).
    ENDTRY.

    APPEND LW_DATA TO GT_DATA.
  ENDLOOP.

  SORT GT_BUKRS BY BUKRS.
  SORT GT_BLART BY BLART.
  SORT GT_WAERS BY WAERS.
  SORT GT_BRNCH BY BRANCH.

  SORT GT_SAKNR BY SAKNR.
  SORT GT_LIFNR BY LIFNR.
  SORT GT_KUNNR BY KUNNR.
  SORT GT_KOSTL BY KOKRS KOSTL.
  SORT GT_PRCTR BY PRCTR KOKRS.
  SORT GT_MWSKZ BY MWSKZ.
  SORT GT_EBELN BY EBELN EBELP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_data
*----------------------------------------------------------------------*
*  Validate File data
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATA.

  DATA:
    LW_DATA     TYPE TS_DATA,
    LT_DATA     TYPE TABLE OF TS_DATA,
    LW_DATA_TMP TYPE TS_DATA,
    LS_TOTAMT   TYPE TS_TOTAMT.

  DATA:
    LV_TABIX  TYPE SY-TABIX,
    LV_GRPNO  TYPE NUMC4,
    LV_ITEMNO TYPE I.

  FIELD-SYMBOLS:
    <LW_DATA> TYPE TS_DATA.

* Show Progress
* Text-p03 : Validating File data...
  MC_SHOW_PROGRESS 30 TEXT-P03.

*--------------------------------------
* Get master data
*--------------------------------------
  PERFORM F_GET_ADDITIONAL.

*--------------------------------------
* Validate data
*--------------------------------------
  LOOP AT GT_DATA INTO LW_DATA.

    LV_TABIX = SY-TABIX.

    PERFORM F_VALIDATE_HEADER CHANGING LW_DATA.

    IF LW_DATA-STATUS NE ICON_RED_LIGHT.
      PERFORM F_VALIDATE_ITEM CHANGING LW_DATA.
    ENDIF.

    MODIFY GT_DATA FROM LW_DATA INDEX LV_TABIX
                         TRANSPORTING DCIND
                                      CRNCY_LOC
                                      STATUS
                                      MESSAGE.

  ENDLOOP.

*--------------------------------------
* Apply error into all items within same trans_no
*--------------------------------------
  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA_TMP>).

    LW_DATA = <L_DATA_TMP>.

    LV_TABIX = SY-TABIX.

    AT NEW TRANS_NO.
      LT_DATA[] = GT_DATA[].
      DELETE LT_DATA WHERE NOT ( TRANS_NO EQ LW_DATA-TRANS_NO ).
    ENDAT.

    IF LW_DATA-STATUS IS INITIAL.
      CLEAR LW_DATA_TMP.
      READ TABLE LT_DATA INTO LW_DATA_TMP WITH KEY STATUS = ICON_RED_LIGHT .

      IF SY-SUBRC EQ 0.
        LW_DATA-STATUS  = LW_DATA_TMP-STATUS.
*       lw_data-message = lw_data_tmp-message.
        MODIFY GT_DATA FROM LW_DATA INDEX LV_TABIX
                             TRANSPORTING STATUS
                                          MESSAGE.
      ENDIF.
    ENDIF.

  ENDLOOP.

*------------------------------------------------
* Create FI document maximum 900 lines + 1 temp GL 199999999
*------------------------------------------------
  LT_DATA = GT_DATA.
  CLEAR:
    GT_DATA.
  LOOP AT LT_DATA ASSIGNING <L_DATA_TMP>.

    LW_DATA = <L_DATA_TMP>.

    LV_ITEMNO = LV_ITEMNO + 1.

    AT NEW TRANS_NO.
      LV_GRPNO  = 1.
      LV_ITEMNO = 1.
      CLEAR LS_TOTAMT.
    ENDAT.
    IF LV_ITEMNO GT 900 ##NUMBER_OK.
*     Fill temporary gl line
      PERFORM F_APPEND_TEMPGL USING LV_GRPNO
                                    LW_DATA
                                    LS_TOTAMT.
      LV_GRPNO = LV_GRPNO + 1.
      LV_ITEMNO = 1.
      CLEAR LS_TOTAMT.
    ENDIF.

    LW_DATA-GRPNO = LV_GRPNO.
    APPEND LW_DATA TO GT_DATA.

    LS_TOTAMT-AMT_LOC = LS_TOTAMT-AMT_LOC + LW_DATA-AMT_LOC.
    LS_TOTAMT-AMT_DOC = LS_TOTAMT-AMT_DOC + LW_DATA-AMT_DOC.
  ENDLOOP.

  LOOP AT GT_DATA ASSIGNING <LW_DATA>.
    <LW_DATA>-LINENO = SY-TABIX.
  ENDLOOP.

*--------------------------------------
* Map GL with it's AP,AR lines
*--------------------------------------
  PERFORM F_CAL_TAXBASE.

*--------------------------------------
* AP Case: WHT 0*, doctype Z2 only
* Do not create WHT line, but add GL(Conversion) line with WHT amount
*--------------------------------------
*  PERFORM F_APPEND_WHTCONV.  "JJ comment

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_tempgl
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_APPEND_TEMPGL USING PV_GRPNO TYPE TS_DATA-GRPNO
                           PS_DATA TYPE TS_DATA
                           PS_TOTAMT TYPE TS_TOTAMT.

  DATA:
    LW_DATA TYPE TS_DATA,
    LS_KEY1 TYPE ZSDSFIS112.

* move all header fields
  MOVE-CORRESPONDING PS_DATA TO LS_KEY1.

  CLEAR LW_DATA.
  MOVE-CORRESPONDING LS_KEY1 TO LW_DATA.
  LW_DATA-TRANS_NO  = PS_DATA-TRANS_NO.
  LW_DATA-GRPNO     = PV_GRPNO.
  LW_DATA-SAPACCT   = GV_CONTRA_GL.
  LW_DATA-GLACCT    = GV_CONTRA_GL.
  LW_DATA-ACCTTYP   = 'S'.
  LW_DATA-CRNCY_DOC = PS_DATA-CRNCY_DOC.
  LW_DATA-CRNCY_LOC = PS_DATA-CRNCY_LOC.
  LW_DATA-AMT_DOC   = PS_TOTAMT-AMT_DOC * -1.
  LW_DATA-AMT_LOC   = PS_TOTAMT-AMT_LOC * -1.
  IF LW_DATA-AMT_DOC LT 0.
    LW_DATA-POSKEY = '50'.
    LW_DATA-DCIND  = 'H'.
  ELSE.
    LW_DATA-POSKEY = '40'.
    LW_DATA-DCIND  = 'S'.
  ENDIF.
  LW_DATA-AUTO      = 'X'.
  APPEND LW_DATA TO GT_DATA.

  CLEAR LW_DATA.
  MOVE-CORRESPONDING LS_KEY1 TO LW_DATA.
  LW_DATA-TRANS_NO  = PS_DATA-TRANS_NO.
  LW_DATA-GRPNO     = PV_GRPNO + 1.
  LW_DATA-SAPACCT   = GV_CONTRA_GL.
  LW_DATA-GLACCT    = GV_CONTRA_GL.
  LW_DATA-ACCTTYP   = 'S'.
  LW_DATA-CRNCY_DOC = PS_DATA-CRNCY_DOC.
  LW_DATA-CRNCY_LOC = PS_DATA-CRNCY_LOC.
  LW_DATA-AMT_DOC   = PS_TOTAMT-AMT_DOC.
  LW_DATA-AMT_LOC   = PS_TOTAMT-AMT_LOC.
  IF LW_DATA-AMT_DOC LT 0.
    LW_DATA-POSKEY = '50'.
    LW_DATA-DCIND  = 'H'.
  ELSE.
    LW_DATA-POSKEY = '40'.
    LW_DATA-DCIND  = 'S'.
  ENDIF.
  LW_DATA-AUTO      = 'X'.
  APPEND LW_DATA TO GT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDITIONAL
*&---------------------------------------------------------------------*
*       Get master data to validate input data
*----------------------------------------------------------------------*
FORM F_GET_ADDITIONAL .

* Company Code
  IF GT_BUKRS[] IS NOT INITIAL.
    SELECT T001~BUKRS
           T001~BUTXT
           T001~WAERS
           T001~LAND1
           T001~KTOPL
           TKA02~KOKRS
           T005~KALSM
    FROM T001
    INNER JOIN TKA02 ON TKA02~BUKRS EQ T001~BUKRS
    INNER JOIN T005 ON T005~LAND1 EQ T001~LAND1   "#EC CI_BUFFJOIN
    INTO TABLE GT_T001
    FOR ALL ENTRIES IN GT_BUKRS
    WHERE T001~BUKRS EQ GT_BUKRS-BUKRS.
    SORT GT_T001 BY BUKRS.
  ENDIF.

* Document Typ
  IF GT_BLART[] IS NOT INITIAL.
    SELECT *
    FROM T003
    INTO TABLE GT_T003
    FOR ALL ENTRIES IN GT_BLART
    WHERE BLART EQ GT_BLART-BLART.
    SORT GT_T003 BY BLART.
  ENDIF.

* Currency
  IF GT_WAERS[] IS NOT INITIAL.
    SELECT *
    FROM TCURC
    INTO TABLE GT_TCURC
    FOR ALL ENTRIES IN GT_WAERS
    WHERE WAERS EQ GT_WAERS-WAERS.
    SORT GT_TCURC BY WAERS.
  ENDIF.

* Business Place
  IF GT_BRNCH[] IS NOT INITIAL.
    SELECT *
    FROM J_1BBRANCH
    INTO TABLE GT_J_1BBRANCH
    FOR ALL ENTRIES IN GT_BRNCH
    WHERE BUKRS  EQ GT_BRNCH-BUKRS
      AND BRANCH EQ GT_BRNCH-BRANCH.
    SORT GT_J_1BBRANCH BY BUKRS BRANCH.
  ENDIF.

* Vendor master
  IF GT_LIFNR[] IS NOT INITIAL.
    SELECT LFB1~LIFNR
           LFB1~BUKRS
           LFA1~KTOKK
           LFA1~XCPDK
           LFB1~ZTERM
    FROM LFB1
      INNER JOIN LFA1 ON LFA1~LIFNR EQ LFB1~LIFNR
    INTO TABLE GT_LFB1
    FOR ALL ENTRIES IN GT_LIFNR
    WHERE LFB1~LIFNR EQ GT_LIFNR-LIFNR.
    SORT GT_LFB1 BY LIFNR BUKRS.
  ENDIF.

* Customer
  IF GT_KUNNR[] IS NOT INITIAL.
    SELECT KNB1~KUNNR
           KNB1~BUKRS
           KNB1~ZTERM
           KNA1~LAND1
           KNA1~XCPDK
    FROM KNB1
      INNER JOIN KNA1 ON KNA1~KUNNR EQ KNB1~KUNNR
    INTO TABLE GT_KNB1
    FOR ALL ENTRIES IN GT_KUNNR
    WHERE KNB1~KUNNR EQ GT_KUNNR-KUNNR.
    SORT GT_KNB1 BY KUNNR BUKRS.
  ENDIF.

* GL account
  IF GT_SAKNR[] IS NOT INITIAL.
    SELECT  BUKRS
            SAKNR
    FROM SKB1   "#EC CI_SGLSELECT
    INTO TABLE GT_SKB1
    FOR ALL ENTRIES IN GT_SAKNR
    WHERE SAKNR EQ GT_SAKNR-SAKNR.
    SORT GT_SKB1 BY BUKRS SAKNR.
  ENDIF.

* Cost Center
  IF GT_KOSTL[] IS NOT INITIAL.
    SELECT KOKRS
           KOSTL
           DATBI
           DATAB
    FROM CSKS          "#EC CI_SGLSELECT
    INTO TABLE GT_CSKS
    FOR ALL ENTRIES IN GT_KOSTL
    WHERE KOSTL EQ GT_KOSTL-KOSTL
      AND DATBI GE SY-DATUM
      AND DATAB LE SY-DATUM.
    SORT GT_CSKS BY KOKRS KOSTL.
  ENDIF.

* Profit center
  IF GT_PRCTR[] IS NOT INITIAL.
    SELECT  PRCTR
            KOKRS
            DATBI
    FROM CEPC
    INTO TABLE GT_CEPC
    FOR ALL ENTRIES IN GT_PRCTR
    WHERE PRCTR EQ GT_PRCTR-PRCTR
      AND DATBI GE SY-DATUM
      AND DATAB LE SY-DATUM.
    SORT GT_CEPC BY PRCTR KOKRS.
  ENDIF.

* Order
  IF GT_AUFNR[] IS NOT INITIAL.
    SELECT AUFNR
    FROM AUFK
    INTO TABLE GT_AUFK
    FOR ALL ENTRIES IN GT_AUFNR
    WHERE AUFNR EQ GT_AUFNR-AUFNR.
    SORT GT_AUFK BY AUFNR.
  ENDIF.

* Tax Code
  IF GT_MWSKZ[] IS NOT INITIAL.
    SELECT *
    FROM T007A
    INTO TABLE GT_T007A
    FOR ALL ENTRIES IN GT_MWSKZ
    WHERE KALSM EQ 'TAXTH'
      AND MWSKZ EQ GT_MWSKZ-MWSKZ.
    SORT GT_T007A BY MWSKZ.

* Tax Accounts Determination
    SELECT *
    FROM T030K
    INTO TABLE GT_T030K
    FOR ALL ENTRIES IN GT_MWSKZ
    WHERE MWSKZ EQ GT_MWSKZ-MWSKZ.  "#EC CI_NOORDER
    SORT GT_T030K BY KTOPL KTOSL MWSKZ.
  ENDIF.

* Purchasing order
  IF GT_EBELN[] IS NOT INITIAL.
    SELECT  EBELN
            EBELP
    FROM EKPO
    INTO TABLE GT_EKPO
    FOR ALL ENTRIES IN GT_EBELN
    WHERE EBELN EQ GT_EBELN-EBELN
      AND EBELP EQ GT_EBELN-EBELP.
    SORT GT_EKPO BY EBELN EBELP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_header
*&---------------------------------------------------------------------*
FORM F_VALIDATE_HEADER CHANGING PS_DATA TYPE TS_DATA.

  DATA:
    LW_T001  TYPE TS_T001    ##NEEDED,
    LW_T003  TYPE T003        ##NEEDED,
    LW_TCURC TYPE TCURC       ##NEEDED.

  DATA:
    LT_DATA TYPE TABLE OF TS_DATA.
*    LW_DATA TYPE TS_DATA.

  DATA:
    LS_KEY1 TYPE ZSDSFIS112,
    LS_KEY2 TYPE ZSDSFIS112.

*//Transaction No.
  IF PS_DATA-TRANS_NO IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Transaction No.'(m01)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ENDIF.

*//Company Code
  IF PS_DATA-CCODE IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Company Code'(m02)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    READ TABLE GT_T001 WITH KEY BUKRS = PS_DATA-CCODE
                                INTO LW_T001
                                BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'F5' '165'
                                  PS_DATA-CCODE
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ELSE.
      PS_DATA-CRNCY_LOC  = LW_T001-WAERS.
    ENDIF.
  ENDIF.

*//Doc Type
  IF PS_DATA-DOCTYP IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Doc Type'(m03)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    CLEAR LW_T003.
    READ TABLE GT_T003 WITH KEY BLART = PS_DATA-DOCTYP
                                INTO LW_T003
                                BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'F5' '814'
                                  PS_DATA-DOCTYP
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Document Date
  IF PS_DATA-DOCDATE IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Document Date'(m04)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = PS_DATA-DOCDATE
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED = 1
        OTHERS                    = 2.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '102'
                                  SPACE
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Posting Date
  IF PS_DATA-POSDATE IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Posting Date'(m05)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = PS_DATA-POSDATE
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED = 1
        OTHERS                    = 2.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '102'
                                  SPACE
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Currency
  IF PS_DATA-CRNCY_DOC IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Currency'(m06)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    CLEAR LW_TCURC.
    READ TABLE GT_TCURC WITH KEY WAERS = PS_DATA-CRNCY_DOC
                                 INTO LW_TCURC
                                 BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'F5A' '100'
                                  PS_DATA-CRNCY_DOC
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

*--------------------------------------
* Check all Header fields in same group diferrent?
*--------------------------------------
  MOVE-CORRESPONDING PS_DATA TO LS_KEY1.

  LT_DATA[] = GT_DATA[].
  DELETE LT_DATA WHERE TRANS_NO NE PS_DATA-TRANS_NO.


  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<LW_DATA>).
    MOVE-CORRESPONDING <LW_DATA> TO LS_KEY2.
    IF LS_KEY1 NE LS_KEY2.
      PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '107'
                                  SPACE
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_item
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ITEM CHANGING PS_DATA TYPE TS_DATA.

  DATA:
    LW_T001       TYPE TS_T001,
    LW_TBSL       TYPE TBSL,
    LW_SKB1       TYPE TS_SKB1    ##NEEDED,
    LW_LFB1       TYPE TS_LFB1    ##NEEDED,
    LW_KNB1       TYPE TS_KNB1    ##NEEDED,
    LW_J_1BBRANCH TYPE J_1BBRANCH  ##NEEDED,
    LW_CSKS       TYPE TS_CSKS    ##NEEDED,
    LW_CEPC       TYPE TS_CEPC    ##NEEDED,
    LW_AUFK       TYPE TS_AUFK    ##NEEDED,
    LW_T007A      TYPE T007A       ##NEEDED,
    LW_T030K      TYPE T030K       ##NEEDED,
    LW_EKPO       TYPE TS_EKPO    ##NEEDED.

  CLEAR LW_T001.
  READ TABLE GT_T001 WITH KEY BUKRS = PS_DATA-CCODE
                              INTO LW_T001
                              BINARY SEARCH.

* //Posting key with Credit/Debit Ind.
  IF PS_DATA-POSKEY IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Posting key'(m15)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    CLEAR LW_TBSL.
    READ TABLE GT_TBSL WITH KEY BSCHL = PS_DATA-POSKEY
                                INTO LW_TBSL
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      PS_DATA-DCIND = LW_TBSL-SHKZG.
    ELSE.
      PERFORM F_SET_MESSAGE USING 'E' 'F5' '034'
                                  PS_DATA-POSKEY
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Account
  IF PS_DATA-SAPACCT IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'SAP Account'(m16)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ELSE.
    CASE PS_DATA-ACCTTYP.
      WHEN 'D'.
        CLEAR LW_KNB1.
        READ TABLE GT_KNB1 WITH KEY KUNNR = PS_DATA-CUSTNO
                                    BUKRS = PS_DATA-CCODE
                                    INTO LW_KNB1
                                    BINARY SEARCH.
        IF SY-SUBRC NE 0.
          PERFORM F_SET_MESSAGE USING 'E' 'F5' '102'
                                      PS_DATA-CUSTNO
                                      PS_DATA-CCODE
                                      SPACE
                                      SPACE
                             CHANGING PS_DATA-STATUS
                                      PS_DATA-MESSAGE.
          RETURN.
        ENDIF.
      WHEN 'K'.
        CLEAR LW_LFB1.
        READ TABLE GT_LFB1 WITH KEY LIFNR = PS_DATA-VENDNO
                                    BUKRS = PS_DATA-CCODE
                                    INTO LW_LFB1
                                    BINARY SEARCH.
        IF SY-SUBRC NE 0.
          PERFORM F_SET_MESSAGE USING 'E' 'F5' '104'
                                      PS_DATA-VENDNO
                                      PS_DATA-CCODE
                                      SPACE
                                      SPACE
                             CHANGING PS_DATA-STATUS
                                      PS_DATA-MESSAGE.
          RETURN.
        ENDIF.
      WHEN 'S'.
        CLEAR LW_SKB1.
        READ TABLE GT_SKB1 WITH KEY BUKRS = PS_DATA-CCODE
                                    SAKNR = PS_DATA-GLACCT
                                    INTO LW_SKB1
                                    BINARY SEARCH.
        IF SY-SUBRC NE 0.
          PERFORM F_SET_MESSAGE USING 'E' 'F5' '506'
                                      PS_DATA-GLACCT
                                      PS_DATA-CCODE
                                      SPACE
                                      SPACE
                             CHANGING PS_DATA-STATUS
                                      PS_DATA-MESSAGE.
          RETURN.
        ENDIF.
      WHEN OTHERS.
        PERFORM F_SET_MESSAGE USING 'E' 'F5' '506'
                                    PS_DATA-GLACCT
                                    PS_DATA-CCODE
                                    SPACE
                                    SPACE
                           CHANGING PS_DATA-STATUS
                                    PS_DATA-MESSAGE.
        RETURN.
    ENDCASE.
  ENDIF.

* //Branch
  IF PS_DATA-BRNCH IS NOT INITIAL.
    CLEAR LW_J_1BBRANCH.
    READ TABLE GT_J_1BBRANCH WITH KEY BUKRS  = PS_DATA-CCODE
                                      BRANCH = PS_DATA-BRNCH
                                      INTO LW_J_1BBRANCH
                                      BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'F5' '893'
                                  PS_DATA-BRNCH
                                  PS_DATA-CCODE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Amount in Doc Curr
  IF PS_DATA-AMT_DOC EQ 0.
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                'Amount in Doc Curr'(m19)
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ENDIF.

* //Amount with sign
  IF ( PS_DATA-DCIND EQ 'H' AND PS_DATA-AMT_DOC GT 0 ) OR
     ( PS_DATA-DCIND EQ 'S' AND PS_DATA-AMT_DOC LT 0 ).
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '103'
                                PS_DATA-POSKEY
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ENDIF.

*//Tax Base
  IF ( PS_DATA-TAXAMT_DOC NE 0 AND
       PS_DATA-TAXBASE_DOC EQ 0 ) OR
     ( PS_DATA-TAXAMT_LOC NE 0 AND
       PS_DATA-TAXBASE_LOC EQ 0 ).
    "Please enter Tax Base Amount
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '106'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ENDIF.

* //Tax Amount in Doc
  IF PS_DATA-TAXAMT_LOC NE 0 AND
     PS_DATA-TAXAMT_DOC EQ 0.
    "Please enter Tax Amount in Doc. Cur
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '104'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ENDIF.
  IF PS_DATA-TAXBASE_LOC NE 0 AND
     PS_DATA-TAXBASE_DOC EQ 0.
    "Please enter Tax Base Amount in Doc.Curr
    PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '105'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_DATA-STATUS
                                PS_DATA-MESSAGE.
    RETURN.
  ENDIF.


* //Tax code
  IF PS_DATA-TAXCODE IS NOT INITIAL.
    CLEAR LW_T007A.
    READ TABLE GT_T007A WITH KEY MWSKZ = PS_DATA-TAXCODE
                                 INTO LW_T007A
                                 BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'FF' '773'
                                  PS_DATA-TAXCODE
                                  PS_DATA-CCODE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
*    CLEAR lw_t030k.
*    READ TABLE gt_t030k WITH KEY mwskz = ps_data-taxcode
*                                 INTO lw_t030k
*                                 BINARY SEARCH.
*    IF sy-subrc NE 0.
*      PERFORM f_set_message USING 'E' 'FICORE' '717'
*                                  ps_data-taxcode
*                                  lw_t001-kalsm
*                                  space
*                                  space
*                         CHANGING ps_data-status
*                                  ps_data-message.
*    ENDIF.
  ENDIF.

* //Cost center
  IF PS_DATA-COSTCTR IS NOT INITIAL.
    CLEAR LW_CSKS.
    READ TABLE GT_CSKS WITH KEY KOKRS = LW_T001-KOKRS
                                KOSTL = PS_DATA-COSTCTR
                                INTO LW_CSKS
                                BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'KI' '222'
                                  PS_DATA-CCODE
                                  PS_DATA-COSTCTR
                                  SY-DATUM
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Profit center
  IF PS_DATA-PROFITCTR IS INITIAL.
    IF PS_DATA-ACCTTYP NE 'K'.
      PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '101'
                                  'Profit center'(m40)
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ELSE.
    CLEAR LW_CEPC.
    READ TABLE GT_CEPC WITH KEY PRCTR = PS_DATA-PROFITCTR
                                INTO LW_CEPC
                                BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'KI' '623'
                                  PS_DATA-PROFITCTR
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

*//Order
  IF PS_DATA-ORDERNO IS NOT INITIAL.
    CLEAR LW_AUFK.
    READ TABLE GT_AUFK WITH KEY AUFNR = PS_DATA-ORDERNO
                                INTO LW_AUFK
                                BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'KI' '253'
                                  PS_DATA-ORDERNO
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //WHT Type
  CASE PS_DATA-ACCTTYP.
    WHEN 'D'.
      IF PS_DATA-WHTTYP1 IS NOT INITIAL.
        PERFORM F_CHECK_WITHT_CUST USING PS_DATA-CUSTNO
                                         PS_DATA-CCODE
                                         PS_DATA-WHTTYP1
                                CHANGING PS_DATA-STATUS
                                         PS_DATA-MESSAGE.
        IF PS_DATA-STATUS IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
      IF PS_DATA-WHTTYP2 IS NOT INITIAL.
        PERFORM F_CHECK_WITHT_CUST USING PS_DATA-CUSTNO
                                         PS_DATA-CCODE
                                         PS_DATA-WHTTYP2
                                CHANGING PS_DATA-STATUS
                                         PS_DATA-MESSAGE.
        IF PS_DATA-STATUS IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
    WHEN 'K'.
      IF PS_DATA-WHTTYP1 IS NOT INITIAL.
        PERFORM F_CHECK_WITHT_VED USING PS_DATA-VENDNO
                                        PS_DATA-CCODE
                                        PS_DATA-WHTTYP1
                               CHANGING PS_DATA-STATUS
                                        PS_DATA-MESSAGE.
        IF PS_DATA-STATUS IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
      IF PS_DATA-WHTTYP2 IS NOT INITIAL.
        PERFORM F_CHECK_WITHT_VED USING PS_DATA-VENDNO
                                        PS_DATA-CCODE
                                        PS_DATA-WHTTYP2
                               CHANGING PS_DATA-STATUS
                                        PS_DATA-MESSAGE.
        IF PS_DATA-STATUS IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
  ENDCASE.
*
* //Witholding tax code
  IF PS_DATA-WHTCODE1 IS NOT INITIAL.
    PERFORM F_CHECK_WITHCD USING PS_DATA-WHTTYP1
                                 PS_DATA-WHTCODE1
                                 PS_DATA-CCODE
                            CHANGING PS_DATA-STATUS
                                     PS_DATA-MESSAGE.
    IF PS_DATA-STATUS IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDIF.
  IF PS_DATA-WHTCODE2 IS NOT INITIAL.
    PERFORM F_CHECK_WITHCD USING PS_DATA-WHTTYP2
                                 PS_DATA-WHTCODE2
                                 PS_DATA-CCODE
                            CHANGING PS_DATA-STATUS
                                     PS_DATA-MESSAGE.
    IF PS_DATA-STATUS IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

* //PO No
  IF PS_DATA-PONO IS NOT INITIAL.
    CLEAR LW_EKPO.
    READ TABLE GT_EKPO WITH KEY EBELN = PS_DATA-PONO
                                EBELP = PS_DATA-POITEM
                                INTO LW_EKPO
                                BINARY SEARCH.
    IF SY-SUBRC NE 0.
      PERFORM F_SET_MESSAGE USING 'E' 'ME' '706'
                                  PS_DATA-POITEM
                                  PS_DATA-PONO
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

* //Tax Code I7 require Tax Base and Tax amt
  IF PS_DATA-TAXCODE EQ 'I7'.
    IF PS_DATA-TAXBASE_DOC EQ 0 OR
       PS_DATA-TAXAMT_DOC EQ 0.
*     Please enter Tax Base and Tax Amount for I7
      PERFORM F_SET_MESSAGE USING 'E' 'ZFI001' '108'
                                  SPACE
                                  SPACE
                                  SPACE
                                  SPACE
                         CHANGING PS_DATA-STATUS
                                  PS_DATA-MESSAGE.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_WITHT
*&---------------------------------------------------------------------*
FORM F_CHECK_WITHT_VED  USING PV_VENDNO TYPE LFBW-LIFNR
                              PV_CCODE  TYPE LFB1-BUKRS
                              PV_WITHT  TYPE WITH_ITEM-WITHT
                     CHANGING PV_STATUS  TYPE TS_DATA-STATUS
                              PV_MESSAGE TYPE TS_DATA-MESSAGE.

  DATA:
    LT_LFBW TYPE TABLE OF LFBW.

  CALL FUNCTION 'FI_WT_READ_LFBW'
    EXPORTING
      I_LIFNR   = PV_VENDNO
      I_BUKRS   = PV_CCODE
      I_TYPE    = PV_WITHT
    TABLES
      T_LFBW    = LT_LFBW
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.
  IF SY-SUBRC NE 0.
    PERFORM F_SET_MESSAGE USING 'E'
                                '7Q'
                                '021'
                                PV_WITHT
                                PV_VENDNO
                                SPACE
                                SPACE
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_WITHT
*&---------------------------------------------------------------------*
FORM F_CHECK_WITHT_CUST USING PV_CUSTNO TYPE KNBW-KUNNR
                              PV_CCODE  TYPE LFB1-BUKRS
                              PV_WITHT  TYPE WITH_ITEM-WITHT
                     CHANGING PV_STATUS  TYPE TS_DATA-STATUS
                              PV_MESSAGE TYPE TS_DATA-MESSAGE.

  DATA:
    LT_KNBW TYPE TABLE OF KNBW.

  CALL FUNCTION 'FI_WT_READ_KNBW'
    EXPORTING
      I_KUNNR   = PV_CUSTNO
      I_BUKRS   = PV_CCODE
      I_TYPE    = PV_WITHT
    TABLES
      T_KNBW    = LT_KNBW
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.
  IF SY-SUBRC NE 0.
    PERFORM F_SET_MESSAGE USING 'E'
                                'CMD_API'
                                '095'
                                PV_WITHT
                                PV_CUSTNO
                                SPACE
                                SPACE
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_WITHCD
*&---------------------------------------------------------------------*
FORM F_CHECK_WITHCD  USING PV_WITHT   TYPE WITHT
                           PV_WITHCD  TYPE WT_WITHCD
                           PV_CCODE   TYPE T001-BUKRS
                  CHANGING PV_STATUS  TYPE TS_DATA-STATUS
                           PV_MESSAGE TYPE TS_DATA-MESSAGE.

  DATA: LT_T059Z TYPE TABLE OF T059Z.

  CALL FUNCTION 'FI_WT_READ_T059Z'
    EXPORTING
      I_BUKRS     = PV_CCODE
      I_TYPE      = PV_WITHT
      I_WT_WITHCD = PV_WITHCD
    TABLES
      T_T059Z     = LT_T059Z
    EXCEPTIONS
      NOT_FOUND   = 1
      OTHERS      = 2.
  IF SY-SUBRC NE 0.
    PERFORM F_SET_MESSAGE USING 'E'
                                '7Q'
                                '302'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_upload_data
*&---------------------------------------------------------------------*

FORM F_UPLOAD_DATA  USING PV_TEST TYPE FLAG.

  DATA:
    LW_DATA     TYPE TS_DATA,
*    LW_DATA_TMP TYPE TS_DATA,
    LT_DATA_ONE TYPE TABLE OF TS_DATA.
*    LW_DATA_ONE TYPE TS_DATA.

  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<LW_DATA_TMP>).

    CHECK <LW_DATA_TMP>-STATUS NE ICON_RED_LIGHT.

    LW_DATA = <LW_DATA_TMP>.
    CLEAR: LW_DATA-FIDOC,
           LW_DATA-FIYEAR,
           LW_DATA-STATUS,
           LW_DATA-MESSAGE.

    AT NEW GRPNO.
      CLEAR:
        GV_ITEMNO,
        GV_SPDOWN,
        GV_ACTTYP.

      CLEAR:
        GT_TAXLINE.

      CLEAR:
        GS_DOCUMENTHEADER,
        GS_CUSTOMERCPD,
        GT_ACCOUNTGL,
        GT_ACCOUNTAP,
        GT_ACCOUNTAR,
        GT_ACCOUNTTX,
        GT_ACCOUNTWT,
        GT_CURRENCYAMOUNT,
        gt_criteria,
        GT_EXTENSION2,
        GT_RETURN.

      PERFORM F_FILL_HEADER USING LW_DATA.
    ENDAT.

*   Fill item data
    PERFORM F_FILL_ITEM USING LW_DATA.

    AT END OF GRPNO.

*     Fill tax line
      PERFORM F_FILL_TAX USING LW_DATA.

*     Check if PO/Item required in Customer line
*     If yes, need to update PO history
      CLEAR GV_POSTINTF.
      LT_DATA_ONE = GT_DATA.
      DELETE LT_DATA_ONE WHERE NOT ( TRANS_NO EQ LW_DATA-TRANS_NO AND
                                     GRPNO EQ LW_DATA-GRPNO ).
      LOOP AT LT_DATA_ONE ASSIGNING FIELD-SYMBOL(<L_DATA_ONE>) WHERE ACCTTYP EQ 'K'.
        IF <L_DATA_ONE>-PONO IS NOT INITIAL.
          GV_POSTINTF = 'X'.
        ENDIF.
      ENDLOOP.

*       Call BAPI to check/post FI document data
      IF PV_TEST EQ 'X'.
        PERFORM F_CALL_BAPI_CHECK CHANGING  LW_DATA-FIDOC
                                            LW_DATA-FIYEAR
                                            LW_DATA-STATUS
                                            LW_DATA-MESSAGE.
      ELSE.
        PERFORM F_CALL_BAPI_POST CHANGING   LW_DATA-FIDOC
                                            LW_DATA-FIYEAR
                                            LW_DATA-STATUS
                                            LW_DATA-MESSAGE.
*        IF LW_DATA-FIDOC IS NOT INITIAL.
**         Update Long texts
*          PERFORM F_UPDATE_TEXT USING LW_DATA
*                            CHANGING  LW_DATA-STATUS
*                                      LW_DATA-MESSAGE.
*        ENDIF.
      ENDIF.

      MODIFY GT_DATA  FROM LW_DATA
              TRANSPORTING FIDOC
                           FIYEAR
                           STATUS
                           MESSAGE
                     WHERE ( TRANS_NO EQ LW_DATA-TRANS_NO
                         AND GRPNO EQ LW_DATA-GRPNO ).
    ENDAT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_HEADER
*&---------------------------------------------------------------------*
*       Fill header data for sending to BAPI
*----------------------------------------------------------------------*
FORM F_FILL_HEADER USING PS_DATA TYPE TS_DATA.

*  DATA:
*    LW_EXTENSION2     TYPE BAPIPAREX.

  ##NEEDED
  DATA:
    LW_DATA TYPE TS_DATA.

  GS_DOCUMENTHEADER-OBJ_TYPE   = 'BKPFF'.
  GS_DOCUMENTHEADER-BUS_ACT    = 'RFBU'.
* Noted items
  IF PS_DATA-POSKEY EQ '39' AND
   ( PS_DATA-SGLIND EQ 'Z' OR
     PS_DATA-SGLIND EQ 'A' OR
     PS_DATA-SGLIND EQ 'B' OR
     PS_DATA-SGLIND EQ 'C' OR
     PS_DATA-SGLIND EQ 'L' ).
    GS_DOCUMENTHEADER-OBJ_TYPE   = 'BKPFF'.
    GS_DOCUMENTHEADER-BUS_ACT    = 'RFST'.
  ENDIF.

  GS_DOCUMENTHEADER-DOC_DATE   = PS_DATA-DOCDATE.
  GS_DOCUMENTHEADER-PSTNG_DATE = PS_DATA-POSDATE.
  GS_DOCUMENTHEADER-DOC_TYPE   = PS_DATA-DOCTYP.
  GS_DOCUMENTHEADER-COMP_CODE  = PS_DATA-CCODE.
  GS_DOCUMENTHEADER-HEADER_TXT = PS_DATA-DOCHEAD_TXT.
  GS_DOCUMENTHEADER-REF_DOC_NO = PS_DATA-REFERENCE.
  GS_DOCUMENTHEADER-USERNAME   = SY-UNAME.

*-------------------------------------------
* Append Extension
*-------------------------------------------
*  lw_extension2-structure   = 'C_ACCHD'.
*  lw_extension2-valuepart1  = 'XREF1_HD'.
*  lw_extension2-valuepart2  = ps_data-refkey1_h.
*  APPEND lw_extension2 TO gt_extension2.
*  lw_extension2-structure   = 'C_ACCHD'.
*  lw_extension2-valuepart1  = 'XREF2_HD'.
*  lw_extension2-valuepart2  = ps_data-refkey2_h.
*  APPEND lw_extension2 TO gt_extension2.

* //One time
  IF PS_DATA-INDIVIDUAL EQ 'X'.
    GS_CUSTOMERCPD-TITLE  = PS_DATA-TITLE.
    GS_CUSTOMERCPD-NAME   = PS_DATA-NAME1.
    GS_CUSTOMERCPD-NAME_2 = PS_DATA-NAME2.
    GS_CUSTOMERCPD-NAME_3 = PS_DATA-NAME3.
    GS_CUSTOMERCPD-NAME_4 = PS_DATA-NAME4.
    GS_CUSTOMERCPD-STREET = PS_DATA-STREET.
    GS_CUSTOMERCPD-CITY   = PS_DATA-CITY.
    GS_CUSTOMERCPD-POSTL_CODE = PS_DATA-POSTCODE.
    GS_CUSTOMERCPD-COUNTRY    = PS_DATA-COUNTRY.
    GS_CUSTOMERCPD-TAX_NO_3   = PS_DATA-TAXNO3.
    GS_CUSTOMERCPD-BANK_NO    = PS_DATA-BANKKEY.
    GS_CUSTOMERCPD-BANK_ACCT  = PS_DATA-BANKACCT.
    GS_CUSTOMERCPD-LANGU_ISO  = PS_DATA-LANGU.
  ENDIF.

* //Check AR,AR,GL transaction
  READ TABLE GT_DATA WITH KEY TRANS_NO  = PS_DATA-TRANS_NO
                              GRPNO     = PS_DATA-GRPNO
                              ACCTTYP   = 'K'
                              INTO LW_DATA.
  IF SY-SUBRC EQ 0.
    GV_ACTTYP = 'K'.
  ELSE.
    READ TABLE GT_DATA WITH KEY TRANS_NO  = PS_DATA-TRANS_NO
                                GRPNO     = PS_DATA-GRPNO
                                ACCTTYP   = 'D'
                                INTO LW_DATA.
    IF SY-SUBRC EQ 0.
      GV_ACTTYP = 'K'.
    ELSE.
      GV_ACTTYP = 'S'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ITEM
*&---------------------------------------------------------------------*
*       Fill items
*----------------------------------------------------------------------*
FORM F_FILL_ITEM USING PS_DATA TYPE TS_DATA.

  DATA:
    LW_ACCOUNTAR      TYPE BAPIACAR09,
    LW_ACCOUNTAP      TYPE BAPIACAP09,
    LW_ACCOUNTWT      TYPE BAPIACWT09,
    LW_ACCOUNTGL      TYPE BAPIACGL09,
    LW_ACCOUNTTX      TYPE BAPIACTX09,
*    LW_COPA           TYPE BAPIACKEC9,
    LW_CURRENCYAMOUNT TYPE BAPIACCR09.
*    LW_EXTENSION2     TYPE BAPIPAREX.

  DATA:
    LW_TAXLINE   TYPE TS_TAXLINE,
    LS_MWDAT     TYPE RTAX1U15,
    LW_DATA      TYPE TS_DATA,
    LT_ACCOUNTWT TYPE TABLE OF BAPIACWT09.

  DATA:
    LV_DATE   TYPE CHAR10,
    LV_AMTDOC TYPE BSEG-WRBTR.

  ##NEEDED
  FIELD-SYMBOLS:
    <LW_DATA> TYPE TS_DATA.

*--------------------------------------
* Check if "Down payment SPECIAL" case
* need to gen +,- undue vat
*--------------------------------------
  IF ( PS_DATA-ACCTTYP EQ 'K' OR
       PS_DATA-ACCTTYP EQ 'D' ).
    CLEAR:
      GV_SPDOWN.
  ENDIF.
  IF ( PS_DATA-ACCTTYP EQ 'K' OR
       PS_DATA-ACCTTYP EQ 'D' ) AND
     ( PS_DATA-SGLIND IS NOT INITIAL ).
    READ TABLE GT_DATA WITH KEY TRANS_NO  = PS_DATA-TRANS_NO
                                GRPNO     = PS_DATA-GRPNO
                                TAXCODE   = PS_DATA-TAXCODE
                                ACCTTYP   = 'S'
                                INTO LW_DATA.
    IF SY-SUBRC EQ 0.
      IF LW_DATA-GLACCT CP '99999999*' AND
         LW_DATA-TAXAMT_DOC NE 0.
        GV_SPDOWN = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

* To use sign from Deb/Crd ind in posting ley
  PS_DATA-AMT_DOC = ABS( PS_DATA-AMT_DOC ).
  PS_DATA-AMT_LOC = ABS( PS_DATA-AMT_LOC ).

  PS_DATA-TAXBASE_LOC = ABS( PS_DATA-TAXBASE_LOC ).
  PS_DATA-TAXAMT_LOC  = ABS( PS_DATA-TAXAMT_LOC ).
  PS_DATA-TAXBASE_DOC = ABS( PS_DATA-TAXBASE_DOC ).
  PS_DATA-TAXAMT_DOC  = ABS( PS_DATA-TAXAMT_DOC ).

  PS_DATA-WHTBASE1 = ABS( PS_DATA-WHTBASE1 ).
  PS_DATA-WHTAMT1  = ABS( PS_DATA-WHTAMT1 ).
  PS_DATA-WHTBASE2 = ABS( PS_DATA-WHTBASE2 ).
  PS_DATA-WHTAMT2  = ABS( PS_DATA-WHTAMT2 ).

  PS_DATA-AMTCAL_DOC = ABS( PS_DATA-AMTCAL_DOC ).
  PS_DATA-AMTCAL_LOC = ABS( PS_DATA-AMTCAL_LOC ).

  PS_DATA-AMTORG_DOC = ABS( PS_DATA-AMTORG_DOC ).

*-------------------------------------------
* Append item
*-------------------------------------------
  GV_ITEMNO = GV_ITEMNO + 1.

  CASE PS_DATA-ACCTTYP.
    WHEN 'D'.
*     Append AR line item
      LW_ACCOUNTAR-ITEMNO_ACC     = GV_ITEMNO.
      LW_ACCOUNTAR-COMP_CODE      = PS_DATA-CCODE.
      LW_ACCOUNTAR-BUSINESSPLACE  = PS_DATA-BRNCH.
      LW_ACCOUNTAR-CUSTOMER       = PS_DATA-CUSTNO.
      LW_ACCOUNTAR-SP_GL_IND      = PS_DATA-SGLIND.
      LW_ACCOUNTAR-PROFIT_CTR     = PS_DATA-PROFITCTR.
      LW_ACCOUNTAR-TAX_CODE       = PS_DATA-TAXCODE.
      LW_ACCOUNTAR-PART_BUSINESSPLACE = PS_DATA-BRNCH_CODE.
      LW_ACCOUNTAR-ALLOC_NMBR     = PS_DATA-ASSIGNMENT.
      LW_ACCOUNTAR-ITEM_TEXT      = PS_DATA-ITEMTEXT.
      LW_ACCOUNTAR-REF_KEY_1      = PS_DATA-REFKEY1.
      LW_ACCOUNTAR-REF_KEY_2      = PS_DATA-REFKEY2.
      LW_ACCOUNTAR-REF_KEY_3      = PS_DATA-REFKEY3.
      LW_ACCOUNTAR-PMNTTRMS       = PS_DATA-PAYTERM.
      LW_ACCOUNTAR-BLINE_DATE     = PS_DATA-BLDATE.
      APPEND LW_ACCOUNTAR TO GT_ACCOUNTAR.

      IF PS_DATA-SGLIND IS NOT INITIAL AND   "Case down payment special
         GV_SPDOWN EQ 'X'.
*       Data from file will be Gross amt
*       Enter Net amount by pick from Base amt for Customer Sp GL line
        IF PS_DATA-TAXCODE IS NOT INITIAL.
          PS_DATA-AMT_DOC = PS_DATA-TAXBASE_DOC.
          PS_DATA-AMT_LOC = PS_DATA-TAXBASE_LOC.
        ENDIF.
      ENDIF.

    WHEN 'K'.
*     Append AP line item
      LW_ACCOUNTAP-ITEMNO_ACC    = GV_ITEMNO.
      LW_ACCOUNTAP-COMP_CODE     = PS_DATA-CCODE.
      LW_ACCOUNTAP-BUSINESSPLACE = PS_DATA-BRNCH.
      LW_ACCOUNTAP-VENDOR_NO     = PS_DATA-VENDNO.
      LW_ACCOUNTAP-SP_GL_IND     = PS_DATA-SGLIND.
      LW_ACCOUNTAP-PROFIT_CTR    = PS_DATA-PROFITCTR.
      LW_ACCOUNTAP-TAX_CODE      = PS_DATA-TAXCODE.
      LW_ACCOUNTAP-PART_BUSINESSPLACE = PS_DATA-BRNCH_CODE.
      LW_ACCOUNTAP-ALLOC_NMBR    = PS_DATA-ASSIGNMENT.
      LW_ACCOUNTAP-ITEM_TEXT     = PS_DATA-ITEMTEXT.
      LW_ACCOUNTAP-REF_KEY_1     = PS_DATA-REFKEY1.
      LW_ACCOUNTAP-REF_KEY_2     = PS_DATA-REFKEY2.
      LW_ACCOUNTAP-REF_KEY_3     = PS_DATA-REFKEY3.
      LW_ACCOUNTAP-PMNTTRMS      = PS_DATA-PAYTERM.
      LW_ACCOUNTAP-PMNT_BLOCK    = PS_DATA-PAYBLOCK.
      LW_ACCOUNTAP-PYMT_METH     = PS_DATA-PAYMETH.
      LW_ACCOUNTAP-PMTMTHSUPL    = PS_DATA-PAYSUPL.
      LW_ACCOUNTAP-BLINE_DATE    = PS_DATA-BLDATE.
      IF PS_DATA-SGLIND IS NOT INITIAL AND
         PS_DATA-PONO IS NOT INITIAL.
        PERFORM F_DERIVE_COOBJ USING PS_DATA-PONO
                                     PS_DATA-POITEM
                            CHANGING PS_DATA.
        LW_ACCOUNTAP-PROFIT_CTR = PS_DATA-PROFITCTR.

*        lw_extension2-structure   = 'ACCIT'.
*        lw_extension2-valuepart1  = 'EBELN'.
*        lw_extension2-valuepart2  = gv_itemno.
*        lw_extension2-valuepart3  = ps_data-pono.
*        APPEND lw_extension2 TO gt_extension2.
*
*        lw_extension2-structure   = 'ACCIT'.
*        lw_extension2-valuepart1  = 'EBELP'.
*        lw_extension2-valuepart2  = gv_itemno.
*        lw_extension2-valuepart3  = ps_data-poitem.
*        APPEND lw_extension2 TO gt_extension2.
*
*        lw_extension2-structure   = 'ACCIT'.
*        lw_extension2-valuepart1  = 'KOSTL'.
*        lw_extension2-valuepart2  = gv_itemno.
*        lw_extension2-valuepart3  = ps_data-costctr.
*        APPEND lw_extension2 TO gt_extension2.
*
*        lw_extension2-structure   = 'ACCIT'.
*        lw_extension2-valuepart1  = 'PS_PSP_PNR'.
*        lw_extension2-valuepart2  = gv_itemno.
*        lw_extension2-valuepart3  = ps_data-wbs_elem.
*        APPEND lw_extension2 TO gt_extension2.
*
*        lw_extension2-structure   = 'ACCIT'.
*        lw_extension2-valuepart1  = 'NPLNR'.
*        lw_extension2-valuepart2  = gv_itemno.
*        lw_extension2-valuepart3  = ps_data-network.
*        APPEND lw_extension2 TO gt_extension2.
*
*        lw_extension2-structure   = 'ACCIT'.
*        lw_extension2-valuepart1  = 'VORNR'.
*        lw_extension2-valuepart2  = gv_itemno.
*        lw_extension2-valuepart3  = ps_data-netitem.
*        APPEND lw_extension2 TO gt_extension2.
      ENDIF.
      APPEND LW_ACCOUNTAP TO GT_ACCOUNTAP.

      IF PS_DATA-SGLIND IS NOT INITIAL AND   "Case down payment special
         GV_SPDOWN EQ 'X'.
*       Data from file will be Gross amt
*       Enter Net amount by pick from Base amt for Customer Sp GL line
        IF PS_DATA-TAXCODE IS NOT INITIAL.
          PS_DATA-AMT_DOC = PS_DATA-TAXBASE_DOC.
          PS_DATA-AMT_LOC = PS_DATA-TAXBASE_LOC.
        ENDIF.
      ENDIF.

    WHEN 'S'.

      IF ( GV_ACTTYP EQ 'S' AND                   "JV Case
           PS_DATA-TAXCODE IS NOT INITIAL ).
*       Calculate TAX from Gross
        PERFORM F_CAL_TAX_FROM_GROSS USING PS_DATA-CCODE
                                           PS_DATA-TAXCODE
                                           PS_DATA-CRNCY_DOC
                                           PS_DATA-AMT_DOC
                                  CHANGING LS_MWDAT.
        LW_ACCOUNTTX-ITEMNO_ACC = GV_ITEMNO.
        LW_ACCOUNTTX-GL_ACCOUNT = PS_DATA-GLACCT.
        LW_ACCOUNTTX-COND_KEY   = LS_MWDAT-KSCHL.
        LW_ACCOUNTTX-ACCT_KEY   = LS_MWDAT-KTOSL.
        LW_ACCOUNTTX-TAX_CODE   = PS_DATA-TAXCODE.
        LW_ACCOUNTTX-TAX_RATE   = LS_MWDAT-MSATZ.
        LW_ACCOUNTTX-DIRECT_TAX = SPACE.
        APPEND LW_ACCOUNTTX TO GT_ACCOUNTTX.

      ELSE.
*       Case AP SPGL ref PO no, require same Profit all items
        READ TABLE GT_DATA INTO LW_DATA INDEX PS_DATA-APAR_LINE.
        IF LW_DATA-ACCTTYP EQ 'K' AND
           LW_DATA-SGLIND IS NOT INITIAL AND
           LW_DATA-PONO IS NOT INITIAL.
          CLEAR PS_DATA-PROFITCTR.
        ENDIF.
        LW_ACCOUNTGL-ITEMNO_ACC     = GV_ITEMNO.
        LW_ACCOUNTGL-GL_ACCOUNT     = PS_DATA-GLACCT.
        LW_ACCOUNTGL-VALUE_DATE     = PS_DATA-VALDATE.
        LW_ACCOUNTGL-PROFIT_CTR     = PS_DATA-PROFITCTR.
        LW_ACCOUNTGL-COSTCENTER     = PS_DATA-COSTCTR.
        LW_ACCOUNTGL-ORDERID        = PS_DATA-ORDERNO.
        LW_ACCOUNTGL-WBS_ELEMENT    = PS_DATA-WBS_ELEM.
        LW_ACCOUNTGL-TAX_CODE       = PS_DATA-TAXCODE.
        LW_ACCOUNTGL-ALLOC_NMBR     = PS_DATA-ASSIGNMENT.
        LW_ACCOUNTGL-ITEM_TEXT      = PS_DATA-ITEMTEXT.
        LW_ACCOUNTGL-REF_KEY_1      = PS_DATA-REFKEY1.
        LW_ACCOUNTGL-REF_KEY_2      = PS_DATA-REFKEY2.
        LW_ACCOUNTGL-REF_KEY_3      = PS_DATA-REFKEY3.
        LW_ACCOUNTGL-TRADE_ID       = PS_DATA-TRAD_PARTNER.
        LW_ACCOUNTGL-PO_NUMBER      = PS_DATA-PONO.
        LW_ACCOUNTGL-PO_ITEM        = PS_DATA-POITEM.
        LW_ACCOUNTGL-PARTNER_SEGMENT = PS_DATA-PSEGMENT.
        APPEND LW_ACCOUNTGL TO GT_ACCOUNTGL.
      ENDIF.
*--------------------------------------
*     Data from file will be Gross amt
*     Cal Tax amt to enter Net amount for GL line (for AP,AR transactions)
*     Separate logic between AP,AR
*     See Calculation in f_cal_taxbase
*--------------------------------------
      IF PS_DATA-TAXCODE IS NOT INITIAL.
        IF GV_ACTTYP EQ 'K' OR GV_ACTTYP EQ 'D'.
          IF PS_DATA-TAXBASE_DOC NE 0.
            PS_DATA-AMT_DOC = PS_DATA-TAXBASE_DOC.
            PS_DATA-AMT_LOC = PS_DATA-TAXBASE_LOC.
          ELSE.
            PS_DATA-AMT_DOC = PS_DATA-AMTCAL_DOC.
            PS_DATA-AMT_LOC = PS_DATA-AMTCAL_LOC.
          ENDIF.
*        ELSE.                         "JV case
*           No need Cal Tax amt for GL transction even with Tax code
*           This is JV case.
*           Amount enter in excel file can be used
        ENDIF.
      ENDIF.

*--- Add Coding Block for profit segment
*      PERFORM F_SET_CRITERIA_ITEM USING :
*        GV_ITEMNO  'SEGMENT'      PS_DATA-PSEGMENT .

    WHEN OTHERS.
  ENDCASE.

*-------------------------------------------
* Append WHT item (AP,AR)
*-------------------------------------------
  IF PS_DATA-ACCTTYP EQ 'D' OR
     PS_DATA-ACCTTYP EQ 'K'.
*   Append WHT line item
    IF PS_DATA-ACCTTYP EQ 'K'.
*      IF ( ps_data-whttyp1 IS NOT INITIAL AND
*           ps_data-whtcode1 IS NOT INITIAL ) OR
*         ( ps_data-whttyp2 IS NOT INITIAL AND
*           ps_data-whtcode2 IS NOT INITIAL ).
      PERFORM F_READ_WITHT_VED USING PS_DATA-VENDNO
                                     PS_DATA-CCODE
                            CHANGING LT_ACCOUNTWT.
      LOOP AT LT_ACCOUNTWT INTO LW_ACCOUNTWT.
        LW_ACCOUNTWT-ITEMNO_ACC  = GV_ITEMNO.
        IF PS_DATA-WHTTYP1 EQ LW_ACCOUNTWT-WT_TYPE.
          LW_ACCOUNTWT-WT_CODE     = PS_DATA-WHTCODE1.
          LW_ACCOUNTWT-BAS_AMT_TC  = PS_DATA-WHTBASE1.
          LW_ACCOUNTWT-MAN_AMT_TC  = PS_DATA-WHTAMT1.
        ELSEIF PS_DATA-WHTTYP2 EQ LW_ACCOUNTWT-WT_TYPE.
          LW_ACCOUNTWT-WT_CODE     = PS_DATA-WHTCODE2.
          LW_ACCOUNTWT-BAS_AMT_TC  = PS_DATA-WHTBASE2.
          LW_ACCOUNTWT-MAN_AMT_TC  = PS_DATA-WHTAMT2.
        ENDIF.
        APPEND LW_ACCOUNTWT TO GT_ACCOUNTWT.
      ENDLOOP.
*      ENDIF.
    ELSE.
      IF PS_DATA-WHTTYP1 IS NOT INITIAL AND
         PS_DATA-WHTCODE1 IS NOT INITIAL.
        LW_ACCOUNTWT-ITEMNO_ACC  = GV_ITEMNO.
        LW_ACCOUNTWT-WT_TYPE     = PS_DATA-WHTTYP1.
        LW_ACCOUNTWT-WT_CODE     = PS_DATA-WHTCODE1.
        LW_ACCOUNTWT-BAS_AMT_TC  = PS_DATA-WHTBASE1.
        LW_ACCOUNTWT-MAN_AMT_TC  = PS_DATA-WHTAMT1.
        APPEND LW_ACCOUNTWT TO GT_ACCOUNTWT.
      ENDIF.
      IF PS_DATA-WHTTYP2 IS NOT INITIAL AND
         PS_DATA-WHTCODE2 IS NOT INITIAL.
        LW_ACCOUNTWT-ITEMNO_ACC  = GV_ITEMNO.
        LW_ACCOUNTWT-WT_TYPE     = PS_DATA-WHTTYP2.
        LW_ACCOUNTWT-WT_CODE     = PS_DATA-WHTCODE2.
        LW_ACCOUNTWT-BAS_AMT_TC  = PS_DATA-WHTBASE2.
        LW_ACCOUNTWT-MAN_AMT_TC  = PS_DATA-WHTAMT2.
        APPEND LW_ACCOUNTWT TO GT_ACCOUNTWT.
      ENDIF.
    ENDIF.
  ENDIF.

*-------------------------------------------
* Append currency amount
*-------------------------------------------
  LW_CURRENCYAMOUNT-ITEMNO_ACC = GV_ITEMNO.
  LW_CURRENCYAMOUNT-CURR_TYPE  = '00'.
  LW_CURRENCYAMOUNT-CURRENCY   = PS_DATA-CRNCY_DOC.
  IF PS_DATA-DCIND EQ 'H'.
    LW_CURRENCYAMOUNT-AMT_DOCCUR = PS_DATA-AMT_DOC * -1.
  ELSE.
    LW_CURRENCYAMOUNT-AMT_DOCCUR = PS_DATA-AMT_DOC.
  ENDIF.
  IF PS_DATA-SGLIND IS NOT INITIAL AND            "SPDown
     GV_SPDOWN EQ 'X'.
    IF PS_DATA-DCIND EQ 'H'.
      LW_CURRENCYAMOUNT-TAX_AMT    = PS_DATA-TAXAMT_DOC * -1.
    ELSE.
      LW_CURRENCYAMOUNT-TAX_AMT    = PS_DATA-TAXAMT_DOC.
    ENDIF.
  ENDIF.
  IF GV_ACTTYP EQ 'S' AND                         "JV Case
     PS_DATA-TAXBASE_DOC NE 0.
    IF PS_DATA-DCIND EQ 'H'.
      LW_CURRENCYAMOUNT-AMT_BASE = PS_DATA-TAXBASE_DOC * -1.
    ELSE.
      LW_CURRENCYAMOUNT-AMT_BASE = PS_DATA-TAXBASE_DOC.
    ENDIF.
  ENDIF.
  APPEND LW_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

  IF PS_DATA-AMT_LOC NE 0.
    LW_CURRENCYAMOUNT-ITEMNO_ACC = GV_ITEMNO.
    LW_CURRENCYAMOUNT-CURR_TYPE  = '10'.
    LW_CURRENCYAMOUNT-CURRENCY   = PS_DATA-CRNCY_LOC.
    LW_CURRENCYAMOUNT-EXCH_RATE  = PS_DATA-EXCHRATE.
    IF PS_DATA-DCIND EQ 'H'.
      LW_CURRENCYAMOUNT-AMT_DOCCUR = PS_DATA-AMT_LOC * -1.
    ELSE.
      LW_CURRENCYAMOUNT-AMT_DOCCUR = PS_DATA-AMT_LOC.
    ENDIF.
    IF PS_DATA-SGLIND IS NOT INITIAL AND          "SPDown
       GV_SPDOWN EQ 'X'.
      IF PS_DATA-DCIND EQ 'H'.
        LW_CURRENCYAMOUNT-TAX_AMT    = PS_DATA-TAXAMT_LOC * -1.
      ELSE.
        LW_CURRENCYAMOUNT-TAX_AMT    = PS_DATA-TAXAMT_LOC.
      ENDIF.
    ENDIF.
    IF GV_ACTTYP EQ 'S' AND                       "JV Case
       PS_DATA-TAXBASE_LOC NE 0.
      IF PS_DATA-DCIND EQ 'H'.
        LW_CURRENCYAMOUNT-AMT_BASE = PS_DATA-TAXBASE_LOC * -1.
      ELSE.
        LW_CURRENCYAMOUNT-AMT_BASE = PS_DATA-TAXBASE_LOC.
      ENDIF.
    ENDIF.
    APPEND LW_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.
  ENDIF.

**-------------------------------------------
** Append Extension
**-------------------------------------------
*  lw_extension2-structure   = 'ACCIT'.
*  lw_extension2-valuepart1  = 'VERTT'.
*  lw_extension2-valuepart2  = gv_itemno.
*  lw_extension2-valuepart3  = ps_data-cont_type.
*  APPEND lw_extension2 TO gt_extension2.
*
*  lw_extension2-structure   = 'ACCIT'.
*  lw_extension2-valuepart1  = 'VERTN'.
*  lw_extension2-valuepart2  = gv_itemno.
*  lw_extension2-valuepart3  = ps_data-cont_no.
*  APPEND lw_extension2 TO gt_extension2.
*
*  lw_extension2-structure   = 'ACCIT'.
*  lw_extension2-valuepart1  = 'VBEWA'.
*  lw_extension2-valuepart2  = gv_itemno.
*  lw_extension2-valuepart3  = ps_data-flowtyp.
*  APPEND lw_extension2 TO gt_extension2.
*
*  lw_extension2-structure   = 'ACCIT'.
*  lw_extension2-valuepart1  = 'HZUON'.
*  lw_extension2-valuepart2  = gv_itemno.
*  lw_extension2-valuepart3  = ps_data-spgl_assgn.
*  APPEND lw_extension2 TO gt_extension2.
*
** G/L item have no field BUPLA
*  lw_extension2-structure   = 'ACCIT'.
*  lw_extension2-valuepart1  = 'BUPLA'.
*  lw_extension2-valuepart2  = gv_itemno.
*  lw_extension2-valuepart3  = ps_data-brnch.
*  APPEND lw_extension2 TO gt_extension2.
*
*  lw_extension2-structure   = 'ACCIT'.
*  lw_extension2-valuepart1  = 'J_1TPBUPL'.
*  lw_extension2-valuepart2  = gv_itemno.
*  lw_extension2-valuepart3  = ps_data-brnch_code.
*  APPEND lw_extension2 TO gt_extension2.

*-------------------------------------------
* Prepare Tax line
*-------------------------------------------
* Opposite sign of AR,AP
  IF PS_DATA-TAXCODE IS NOT INITIAL AND
    ( PS_DATA-ACCTTYP EQ 'D' OR
      PS_DATA-ACCTTYP EQ 'K' OR
      ( PS_DATA-ACCTTYP EQ 'S' AND
        GV_SPDOWN EQ 'X' ) ).
*   Calculate TAX from Gross
    IF PS_DATA-AMTORG_DOC NE 0.
      LV_AMTDOC = PS_DATA-AMTORG_DOC.
    ELSE.
      LV_AMTDOC = PS_DATA-AMT_DOC.
    ENDIF.
    PERFORM F_CAL_TAX_FROM_GROSS USING PS_DATA-CCODE
                                       PS_DATA-TAXCODE
                                       PS_DATA-CRNCY_DOC
                                       LV_AMTDOC
                              CHANGING LS_MWDAT.
    LW_TAXLINE-TAXCODE = PS_DATA-TAXCODE.
    IF PS_DATA-DCIND EQ 'H'.
      LW_TAXLINE-DCIND  = 'S'.
    ELSE.
      LW_TAXLINE-DCIND  = 'H'.
    ENDIF.
    LW_TAXLINE-MWDAT  = LS_MWDAT.
    LW_TAXLINE-TAXDOC = PS_DATA-TAXAMT_DOC.
    LW_TAXLINE-TAXLOC = PS_DATA-TAXAMT_LOC.
    LW_TAXLINE-AMTBASE_DOC = PS_DATA-TAXBASE_DOC.
    LW_TAXLINE-AMTBASE_LOC = PS_DATA-TAXBASE_LOC.
    IF PS_DATA-TAXCODE EQ 'U7' AND
       PS_DATA-TAXAMT_DOC EQ 0.
      LW_TAXLINE-TAXDOC       = LS_MWDAT-WMWST.
      LW_TAXLINE-AMTBASE_DOC  = LV_AMTDOC - LS_MWDAT-WMWST.
    ENDIF.
    IF ( PS_DATA-ACCTTYP EQ 'D' OR
         PS_DATA-ACCTTYP EQ 'K' ) AND
        GV_SPDOWN EQ 'X'.
      LW_TAXLINE-DIRECT_TAX = 'X'.
    ENDIF.
    CASE PS_DATA-ACCTTYP.
      WHEN 'D'.
        LW_TAXLINE-KEY = PS_DATA-CUSTNO.
      WHEN 'K'.
        LW_TAXLINE-KEY = PS_DATA-VENDNO.
      WHEN 'S'.
        LW_TAXLINE-KEY = PS_DATA-GLACCT.
    ENDCASE.
    CONCATENATE LW_TAXLINE-KEY PS_DATA-TAXCODE
      INTO LW_TAXLINE-KEY.

    IF PS_DATA-ACCTTYP EQ 'K'.
      LV_DATE = |{ PS_DATA-POSDATE+6(2) }{ PS_DATA-POSDATE+4(2) }{ PS_DATA-POSDATE+0(4) }|.
      LW_TAXLINE-REFKEY1  = PS_DATA-REFKEY1.
      LW_TAXLINE-REFKEY2  = PS_DATA-REFKEY2.
*     Text-T01 : Conversion data as at
      CONCATENATE TEXT-T01 '{' LV_DATE '}' INTO LW_TAXLINE-ITEMTEXT .
    ENDIF.
    APPEND LW_TAXLINE TO GT_TAXLINE.
  ENDIF.

ENDFORM.                    " F_FILL_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_FILL_TAX
*&---------------------------------------------------------------------*
FORM F_FILL_TAX USING PS_DATA TYPE TS_DATA.

  DATA:
    LW_TAXLINE        TYPE TS_TAXLINE,
    LT_TAXLINE        TYPE TABLE OF TS_TAXLINE,
    LW_ACCOUNTTX      TYPE BAPIACTX09,
    LW_CURRENCYAMOUNT TYPE BAPIACCR09.
*    LW_EXTENSION2     TYPE BAPIPAREX.

  FIELD-SYMBOLS:
    <LW_TAXLINE> TYPE TS_TAXLINE.

* Group tax line base on Vendor/Customer,Tax Code
  LT_TAXLINE = GT_TAXLINE.
  CLEAR GT_TAXLINE.
  LOOP AT LT_TAXLINE INTO LW_TAXLINE.
    IF LW_TAXLINE-DCIND EQ 'H'.
      LW_TAXLINE-TAXDOC = LW_TAXLINE-TAXDOC * -1.
      LW_TAXLINE-TAXLOC = LW_TAXLINE-TAXLOC * -1.
      LW_TAXLINE-AMTBASE_DOC = LW_TAXLINE-AMTBASE_DOC * -1.
      LW_TAXLINE-AMTBASE_LOC = LW_TAXLINE-AMTBASE_LOC * -1.
    ENDIF.
    READ TABLE GT_TAXLINE WITH KEY KEY = LW_TAXLINE-KEY
                                   ASSIGNING <LW_TAXLINE>.
    IF SY-SUBRC EQ 0.
      <LW_TAXLINE>-TAXDOC = <LW_TAXLINE>-TAXDOC + LW_TAXLINE-TAXDOC.
      <LW_TAXLINE>-TAXLOC = <LW_TAXLINE>-TAXLOC + LW_TAXLINE-TAXLOC.
      <LW_TAXLINE>-AMTBASE_DOC = <LW_TAXLINE>-AMTBASE_DOC + LW_TAXLINE-AMTBASE_DOC.
      <LW_TAXLINE>-AMTBASE_LOC = <LW_TAXLINE>-AMTBASE_LOC + LW_TAXLINE-AMTBASE_LOC.
    ELSE.
      APPEND LW_TAXLINE TO GT_TAXLINE.
    ENDIF.
  ENDLOOP.
  LOOP AT GT_TAXLINE ASSIGNING <LW_TAXLINE>.
    IF <LW_TAXLINE>-TAXDOC LT 0.
      <LW_TAXLINE>-DCIND = 'H'.
    ELSE.
      <LW_TAXLINE>-DCIND = 'S'.
    ENDIF.
    <LW_TAXLINE>-TAXDOC = ABS( <LW_TAXLINE>-TAXDOC ).
    <LW_TAXLINE>-TAXLOC = ABS( <LW_TAXLINE>-TAXLOC ).
    <LW_TAXLINE>-AMTBASE_DOC = ABS( <LW_TAXLINE>-AMTBASE_DOC ).
    <LW_TAXLINE>-AMTBASE_LOC = ABS( <LW_TAXLINE>-AMTBASE_LOC ).
  ENDLOOP.

*-------------------------------------------
* Append tax item
*-------------------------------------------
  LOOP AT GT_TAXLINE INTO LW_TAXLINE  ##INTO_OK .

    GV_ITEMNO = GV_ITEMNO + 1.

    LW_ACCOUNTTX-ITEMNO_ACC = GV_ITEMNO.
    LW_ACCOUNTTX-GL_ACCOUNT = LW_TAXLINE-MWDAT-HKONT.
    LW_ACCOUNTTX-COND_KEY   = LW_TAXLINE-MWDAT-KSCHL.
    LW_ACCOUNTTX-ACCT_KEY   = LW_TAXLINE-MWDAT-KTOSL.
    LW_ACCOUNTTX-TAX_CODE   = LW_TAXLINE-TAXCODE.
    LW_ACCOUNTTX-TAX_RATE   = LW_TAXLINE-MWDAT-MSATZ.
    LW_ACCOUNTTX-DIRECT_TAX = LW_TAXLINE-DIRECT_TAX.
    APPEND LW_ACCOUNTTX TO GT_ACCOUNTTX.

*-------------------------------------------
*   Append currency amount
*-------------------------------------------
    LW_CURRENCYAMOUNT-ITEMNO_ACC = GV_ITEMNO.
    LW_CURRENCYAMOUNT-CURR_TYPE  = '00'.
    LW_CURRENCYAMOUNT-CURRENCY   = PS_DATA-CRNCY_DOC.
    IF LW_TAXLINE-DCIND EQ 'H'.
      LW_CURRENCYAMOUNT-AMT_DOCCUR = LW_TAXLINE-TAXDOC * -1.
      LW_CURRENCYAMOUNT-AMT_BASE   = LW_TAXLINE-AMTBASE_DOC * -1.
    ELSE.
      LW_CURRENCYAMOUNT-AMT_DOCCUR = LW_TAXLINE-TAXDOC.
      LW_CURRENCYAMOUNT-AMT_BASE   = LW_TAXLINE-AMTBASE_DOC.
    ENDIF.
    APPEND LW_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

    IF LW_TAXLINE-TAXLOC NE 0.
      LW_CURRENCYAMOUNT-ITEMNO_ACC = GV_ITEMNO.
      LW_CURRENCYAMOUNT-CURR_TYPE  = '10'.
      LW_CURRENCYAMOUNT-CURRENCY   = PS_DATA-CRNCY_LOC.
      IF LW_TAXLINE-DCIND EQ 'H'.
        LW_CURRENCYAMOUNT-AMT_DOCCUR = LW_TAXLINE-TAXLOC * -1.
        LW_CURRENCYAMOUNT-AMT_BASE   = LW_TAXLINE-AMTBASE_LOC * -1.
      ELSE.
        LW_CURRENCYAMOUNT-AMT_DOCCUR = LW_TAXLINE-TAXLOC.
        LW_CURRENCYAMOUNT-AMT_BASE   = LW_TAXLINE-AMTBASE_LOC.
      ENDIF.
      APPEND LW_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.
    ENDIF.

*-------------------------------------------
* Append Extension
*-------------------------------------------
*    IF lw_taxline-itemtext IS NOT INITIAL.
*      TRANSLATE lw_taxline-refkey1 TO UPPER CASE.
*      TRANSLATE lw_taxline-refkey2 TO UPPER CASE.
*      lw_extension2-structure   = 'ACCIT'.
*      lw_extension2-valuepart1  = 'XREF1'.
*      lw_extension2-valuepart2  = gv_itemno.
*      lw_extension2-valuepart3  = lw_taxline-refkey1.
*      APPEND lw_extension2 TO gt_extension2.
*
*      lw_extension2-structure   = 'ACCIT'.
*      lw_extension2-valuepart1  = 'XREF2'.
*      lw_extension2-valuepart2  = gv_itemno.
*      lw_extension2-valuepart3  = lw_taxline-refkey2.
*      APPEND lw_extension2 TO gt_extension2.
*
*      lw_extension2-structure   = 'ACCIT'.
*      lw_extension2-valuepart1  = 'SGTXT'.
*      lw_extension2-valuepart2  = gv_itemno.
*      lw_extension2-valuepart3  = lw_taxline-itemtext.
*      APPEND lw_extension2 TO gt_extension2.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_FILL_TAX
*&---------------------------------------------------------------------*
*&      Form  F_CALL_BAPI_CHECK
*&---------------------------------------------------------------------*
*       Call BAPI to post FI Document data
*----------------------------------------------------------------------*
FORM F_CALL_BAPI_CHECK CHANGING  PV_DOCNO   TYPE BKPF-BELNR
                                 PV_DOCYR     TYPE BKPF-GJAHR
                                 PV_STATUS  TYPE TS_DATA-STATUS
                                 PV_MESSAGE TYPE TS_DATA-MESSAGE.

  DATA:
    LV_OBJ_KEY TYPE BAPIACHE09-OBJ_KEY.

  DATA:
    LS_RETURN TYPE BAPIRET2.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      DOCUMENTHEADER    = GS_DOCUMENTHEADER
      CUSTOMERCPD       = GS_CUSTOMERCPD
    TABLES
      ACCOUNTGL         = GT_ACCOUNTGL
      ACCOUNTPAYABLE    = GT_ACCOUNTAP
      ACCOUNTRECEIVABLE = GT_ACCOUNTAR
      ACCOUNTTAX        = GT_ACCOUNTTX
      ACCOUNTWT         = GT_ACCOUNTWT
      CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
      EXTENSION2        = GT_EXTENSION2
      CRITERIA          = GT_CRITERIA
      RETURN            = GT_RETURN.

  CLEAR LS_RETURN.
  LOOP AT GT_RETURN INTO LS_RETURN
                   WHERE ( TYPE EQ 'E' OR
                           TYPE EQ 'A' )
*                    exclude "Error in document: BKPFF $ R3DCLNT200"
                     AND NOT ( ID EQ 'RW' AND NUMBER EQ '609' ).
    EXIT.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    PERFORM F_SET_MESSAGE USING LS_RETURN-TYPE
                                LS_RETURN-ID
                                LS_RETURN-NUMBER
                                LS_RETURN-MESSAGE_V1
                                LS_RETURN-MESSAGE_V2
                                LS_RETURN-MESSAGE_V3
                                LS_RETURN-MESSAGE_V4
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ELSE.
    PV_DOCNO = '$'.
    PV_DOCYR = '0000'.
    PERFORM F_SET_MESSAGE USING 'S'
                                'RW'
                                '614'
                                LV_OBJ_KEY(10)
                                LV_OBJ_KEY+10(4)
                                LV_OBJ_KEY+14(4)
                                SPACE
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ENDIF.

ENDFORM.                    " F_CALL_BAPI_CHECK
*&---------------------------------------------------------------------*
*&      Form  F_CALL_BAPI_POST
*&---------------------------------------------------------------------*
*       Call BAPI to post FI Document data
*----------------------------------------------------------------------*
FORM F_CALL_BAPI_POST CHANGING  PV_DOCNO   TYPE BKPF-BELNR
                                PV_DOCYR    TYPE BKPF-GJAHR
                                PV_STATUS  TYPE TS_DATA-STATUS
                                PV_MESSAGE TYPE TS_DATA-MESSAGE.

  DATA:
    LV_OBJ_KEY TYPE BAPIACHE09-OBJ_KEY,
    LV_BAPI    TYPE FLAG.

  DATA:
    LS_RETURN TYPE BAPIRET2.

  IF GV_POSTINTF EQ 'X'.
    LV_BAPI = 'X'.
    EXPORT LV_BAPI = LV_BAPI TO MEMORY ID 'BAPI_ACC'.
  ENDIF.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = GS_DOCUMENTHEADER
      CUSTOMERCPD       = GS_CUSTOMERCPD
    IMPORTING
      OBJ_KEY           = LV_OBJ_KEY
    TABLES
      ACCOUNTGL         = GT_ACCOUNTGL
      ACCOUNTPAYABLE    = GT_ACCOUNTAP
      ACCOUNTRECEIVABLE = GT_ACCOUNTAR
      ACCOUNTTAX        = GT_ACCOUNTTX
      ACCOUNTWT         = GT_ACCOUNTWT
      CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
      EXTENSION2        = GT_EXTENSION2
      CRITERIA          = GT_CRITERIA
      RETURN            = GT_RETURN.

  CLEAR LS_RETURN.
  READ TABLE GT_RETURN WITH KEY TYPE = 'E'
                                INTO LS_RETURN
                                BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        RETURN = LS_RETURN.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT   = 'X'
      IMPORTING
        RETURN = LS_RETURN.
  ENDIF.

  CLEAR LS_RETURN.
  LOOP AT GT_RETURN INTO LS_RETURN
                   WHERE ( TYPE EQ 'E' OR
                           TYPE EQ 'A' )
*                    exclude "Error in document: BKPFF $ R3DCLNT200"
                     AND NOT ( ID EQ 'RW' AND NUMBER EQ '609' ).
    EXIT.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    PERFORM F_SET_MESSAGE USING LS_RETURN-TYPE
                                LS_RETURN-ID
                                LS_RETURN-NUMBER
                                LS_RETURN-MESSAGE_V1
                                LS_RETURN-MESSAGE_V2
                                LS_RETURN-MESSAGE_V3
                                LS_RETURN-MESSAGE_V4
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ELSE.
    PV_DOCNO = LV_OBJ_KEY(10).
    PV_DOCYR = LV_OBJ_KEY+14(4).
    PERFORM F_SET_MESSAGE USING 'S'
                                'RW'
                                '605'
                                LV_OBJ_KEY(10)
                                LV_OBJ_KEY+10(4)
                                LV_OBJ_KEY+14(4)
                                SPACE
                       CHANGING PV_STATUS
                                PV_MESSAGE.
  ENDIF.

  FREE MEMORY ID 'BAPI_ACC'.
ENDFORM.                    " F_CALL_BAPI_POST
*&---------------------------------------------------------------------*
*& Form f_cal_taxbase
*&---------------------------------------------------------------------*
FORM F_CAL_TAXBASE .

  DATA:
    LV_TABIX      TYPE SY-TABIX,
    LV_SUMCAL_DOC TYPE BSEG-WRBTR,
    LV_SUMCAL_LOC TYPE BSEG-DMBTR,
    LV_DIFF_DOC   TYPE BSEG-WRBTR,
    LV_DIFF_LOC   TYPE BSEG-DMBTR.

  FIELD-SYMBOLS:
    <LW_DATA>    TYPE TS_DATA,
    <LW_DATA_GL> TYPE TS_DATA.

*--------------------------------------
*  For GL line:
*  Data from file will be Gross amt
*  Cal Tax amt to enter Net amount for GL line (for AP,AR transactions)
*  Separate logic between AP,AR
*--------------------------------------

*--------------------------------------
* AP Case: Look down to all GL lines maped to the AP line
*--------------------------------------
* 1. Calcuate summation GL amount
* 2. Use ratio to get base amt for each GL line
* 3. If diff amt occurred, the diff amt will be +/- in last item
  LOOP AT GT_DATA ASSIGNING <LW_DATA> WHERE ACCTTYP EQ 'K'.
    LV_TABIX = SY-TABIX.

*   1. Calcuate summation GL amount
    LOOP AT GT_DATA ASSIGNING <LW_DATA_GL>
                  FROM LV_TABIX + 1.
      IF <LW_DATA_GL>-ACCTTYP NE 'S'.
        EXIT.
      ENDIF.
      IF NOT ( <LW_DATA_GL>-TAXCODE EQ 'IX' OR
               <LW_DATA_GL>-TAXCODE EQ 'U7' OR
               <LW_DATA_GL>-TAXCODE EQ 'VX' ).
        <LW_DATA>-SUMGL_DOC = <LW_DATA>-SUMGL_DOC + <LW_DATA_GL>-AMT_DOC.
        <LW_DATA>-SUMGL_LOC = <LW_DATA>-SUMGL_LOC + <LW_DATA_GL>-AMT_LOC.
      ENDIF.
      <LW_DATA_GL>-APAR_LINE = LV_TABIX.
    ENDLOOP.

    CLEAR:
      LV_SUMCAL_DOC,
      LV_SUMCAL_LOC,
      LV_DIFF_DOC,
      LV_DIFF_LOC.
*   2. Use ratio to get base amt for each GL line
    LOOP AT GT_DATA ASSIGNING <LW_DATA_GL> WHERE APAR_LINE EQ LV_TABIX.
      IF <LW_DATA_GL>-TAXCODE EQ 'IX' OR
         <LW_DATA_GL>-TAXCODE EQ 'U7' OR
         <LW_DATA_GL>-TAXCODE EQ 'VX'  .
        <LW_DATA_GL>-AMTCAL_DOC = <LW_DATA_GL>-AMT_DOC.
        <LW_DATA_GL>-AMTCAL_LOC = <LW_DATA_GL>-AMT_LOC.
      ELSE.
        <LW_DATA_GL>-AMTCAL_DOC = ( <LW_DATA_GL>-AMT_DOC / <LW_DATA>-SUMGL_DOC )
                                  * <LW_DATA>-TAXBASE_DOC.
        <LW_DATA_GL>-AMTCAL_LOC = ( <LW_DATA_GL>-AMT_LOC / <LW_DATA>-SUMGL_LOC )
                                  * <LW_DATA>-TAXBASE_LOC.
        LV_SUMCAL_DOC = LV_SUMCAL_DOC + <LW_DATA_GL>-AMTCAL_DOC.
        LV_SUMCAL_LOC = LV_SUMCAL_LOC + <LW_DATA_GL>-AMTCAL_LOC.
      ENDIF.
    ENDLOOP.

*   3. If diff amt occurred, the diff amt will be +/- in last item
    IF <LW_DATA>-TAXBASE_DOC NE 0.
      IF LV_SUMCAL_DOC NE <LW_DATA>-TAXBASE_DOC.
        LV_DIFF_DOC = <LW_DATA>-TAXBASE_DOC - LV_SUMCAL_DOC.
        <LW_DATA_GL>-AMTCAL_DOC = <LW_DATA_GL>-AMTCAL_DOC + LV_DIFF_DOC.
      ENDIF.
      IF LV_SUMCAL_LOC NE <LW_DATA>-TAXBASE_LOC.
        LV_DIFF_LOC = LV_SUMCAL_LOC - <LW_DATA>-TAXBASE_LOC.
        <LW_DATA_GL>-AMTCAL_LOC = <LW_DATA_GL>-AMTCAL_LOC + LV_DIFF_LOC.
      ENDIF.
    ENDIF.

  ENDLOOP.

*--------------------------------------
* AR Case: Look down to all GL lines maped to the AR line
*--------------------------------------
* Only AR Transaction (1 to 1 AR/GL line)
  LOOP AT GT_DATA ASSIGNING <LW_DATA> WHERE ACCTTYP EQ 'D'.
    LV_TABIX = SY-TABIX.

    LOOP AT GT_DATA ASSIGNING <LW_DATA_GL>
                  FROM LV_TABIX + 1.
      IF <LW_DATA_GL>-ACCTTYP NE 'S'.
        EXIT.
      ENDIF.
      <LW_DATA_GL>-APAR_LINE = LV_TABIX.

      IF <LW_DATA>-TAXBASE_DOC NE 0.
        <LW_DATA_GL>-AMTCAL_DOC = <LW_DATA>-TAXBASE_DOC.
        <LW_DATA_GL>-AMTCAL_LOC = <LW_DATA>-TAXBASE_LOC.
      ELSE.
        <LW_DATA_GL>-AMTCAL_DOC = <LW_DATA>-AMT_DOC.
        <LW_DATA_GL>-AMTCAL_LOC = <LW_DATA>-AMT_LOC.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_whtconv
*&---------------------------------------------------------------------*
*FORM F_APPEND_WHTCONV .
*
*  DATA:
*    LV_WHTAMT TYPE WITH_ITEM-WT_QSSHB.
**    LV_TABIX  TYPE SY-TABIX.
*
*  DATA:
*    LW_DATA     TYPE TS_DATA,
*    LT_DATA     TYPE TABLE OF TS_DATA,
**    LW_DATA_TMP TYPE TS_DATA,
*    LT_DATA_ONE TYPE TABLE OF TS_DATA,
*    LS_DATA     TYPE TS_DATA.
*
*  FIELD-SYMBOLS:
*    <LW_DATA>    TYPE TS_DATA.
*
**--------------------------------------
** AP Case: WHT  0*, doctype Z* only
** Do not create WHT line, but add GL(Conversion) line with WHT amount
**--------------------------------------
*  LOOP AT GT_DATA INTO LW_DATA WHERE ACCTTYP EQ 'K'
*                                 AND DOCTYP  EQ 'Z2'
*                                 AND WHTTYP1 CP '0*' .
*    EXIT.
*  ENDLOOP.
*
*  IF SY-SUBRC EQ 0.
*
*    LT_DATA = GT_DATA.
*    CLEAR:
*      GT_DATA.
*    LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<LW_DATA_TMP>).
*
*      LW_DATA = <LW_DATA_TMP>.
*
*      AT NEW TRANS_NO.
*        CLEAR LT_DATA_ONE.
*      ENDAT.
*      APPEND LW_DATA TO LT_DATA_ONE.
*
*      AT END OF TRANS_NO.
*
*        LOOP AT LT_DATA_ONE ASSIGNING <LW_DATA> WHERE ACCTTYP EQ 'K'
*                                                  AND DOCTYP EQ 'Z2'.
**       1. Adjust AP amount by minus WHT amt
*          IF <LW_DATA>-WHTTYP1 CP '0*' .
*            IF <LW_DATA>-WHTAMT1 NE 0.
*              LV_WHTAMT = <LW_DATA>-WHTAMT1.
*            ELSE.
*              PERFORM F_CAL_WITHAMT USING <LW_DATA>-WHTTYP1
*                                          <LW_DATA>-WHTCODE1
*                                          <LW_DATA>-CCODE
*                                          <LW_DATA>-WHTBASE1
*                                 CHANGING LV_WHTAMT.
*            ENDIF.
*            IF <LW_DATA>-DCIND EQ 'H'.
*              LV_WHTAMT = LV_WHTAMT * -1.
*            ENDIF.
*            <LW_DATA>-AMTORG_DOC = <LW_DATA>-AMT_DOC.
*            <LW_DATA>-AMT_DOC = <LW_DATA>-AMT_DOC - LV_WHTAMT.
*            CLEAR: <LW_DATA>-WHTTYP1,
*                   <LW_DATA>-WHTCODE1.
*
**        2. Append GL line
*            READ TABLE LT_DATA WITH KEY APAR_LINE = <LW_DATA>-LINENO
*                                   INTO LS_DATA.
*            IF SY-SUBRC EQ 0.
*              LS_DATA-AMT_DOC = LV_WHTAMT.
*              LS_DATA-DCIND   = <LW_DATA>-DCIND.
*              LS_DATA-TAXCODE = SPACE.
*              LS_DATA-WHTCONV = 'X'.
*              APPEND LS_DATA TO LT_DATA_ONE.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*
*        APPEND LINES OF LT_DATA_ONE TO GT_DATA.
*      ENDAT.
*
*    ENDLOOP.
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_derive_coobj
*&---------------------------------------------------------------------*
FORM F_DERIVE_COOBJ  USING    PV_PONO TYPE EKPO-EBELN
                              PV_POITEM TYPE EKPO-EBELP
                     CHANGING PS_DATA TYPE TS_DATA.
  TYPES:
    BEGIN OF TS_EKKN,
      EBELN      TYPE EKKN-EBELN,
      EBELP      TYPE EKKN-EBELP,
      ZEKKN      TYPE EKKN-ZEKKN,
      KOSTL      TYPE EKKN-KOSTL,
      PRCTR      TYPE EKKN-PRCTR,
      PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
      NPLNR      TYPE EKKN-NPLNR,
      AUFPL      TYPE EKKN-AUFPL,
      APLZL      TYPE AFVC-APLZL,
      VORNR      TYPE AFVC-VORNR,
    END OF TS_EKKN.

  DATA:
    LT_EKKN TYPE TABLE OF TS_EKKN,
    LW_EKKN TYPE TS_EKKN.

  SELECT  EKKN~EBELN
          EKKN~EBELP
          EKKN~ZEKKN
          EKKN~KOSTL
          EKKN~PRCTR
          EKKN~PS_PSP_PNR
          EKKN~NPLNR
          EKKN~AUFPL
          AFVC~APLZL
          AFVC~VORNR
  FROM EKKN
    LEFT JOIN AFVC ON AFVC~AUFPL EQ EKKN~AUFPL
  INTO TABLE LT_EKKN
  WHERE EKKN~EBELN EQ PV_PONO
    AND EKKN~EBELP EQ PV_POITEM.
  SORT LT_EKKN BY EBELN EBELP ZEKKN.
  READ TABLE LT_EKKN INTO LW_EKKN INDEX 1.
  IF SY-SUBRC EQ 0.
    PS_DATA-COSTCTR   = LW_EKKN-KOSTL.
    PS_DATA-PROFITCTR = LW_EKKN-PRCTR.
    PS_DATA-WBS_ELEM  = LW_EKKN-PS_PSP_PNR.
    PS_DATA-NETWORK   = LW_EKKN-NPLNR.
    PS_DATA-NETITEM   = LW_EKKN-VORNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_WITHT
*&---------------------------------------------------------------------*
FORM F_READ_WITHT_VED   USING PV_VENDNO TYPE LFBW-LIFNR
                              PV_CCODE  TYPE LFB1-BUKRS
                    CHANGING PT_ACCOUNTWT TYPE TABLE.

  DATA:
    LT_LFBW      TYPE TABLE OF LFBW,
*    LW_LFBW      TYPE LFBW,
    LW_ACCOUNTWT TYPE BAPIACWT09.

  CALL FUNCTION 'FI_WT_READ_LFBW'
    EXPORTING
      I_LIFNR   = PV_VENDNO
      I_BUKRS   = PV_CCODE
*     I_TYPE    =
    TABLES
      T_LFBW    = LT_LFBW
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ELSE.
    LOOP AT LT_LFBW ASSIGNING FIELD-SYMBOL(<L_LFBW>).
      LW_ACCOUNTWT-WT_TYPE = <L_LFBW>-WITHT.
      LW_ACCOUNTWT-WT_CODE = <L_LFBW>-WT_WITHCD.
      APPEND LW_ACCOUNTWT TO PT_ACCOUNTWT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_CRITERIA_ITEM
*&---------------------------------------------------------------------*
*& Set detail of criteria
*&---------------------------------------------------------------------*
*FORM F_SET_CRITERIA_ITEM  USING UF_ITEMNO    TYPE BAPIACKEC9-ITEMNO_ACC
*                                UF_FIELDNAME TYPE BAPIACKEC9-FIELDNAME
*                                UF_CHARACTER TYPE ANY.
*
*  DATA: LS_CRITERIA  TYPE BAPIACKEC9,
*        LF_CHARACTER TYPE BAPIACKEC9-CHARACTER.
*
*  IF UF_CHARACTER IS NOT INITIAL.
*    LF_CHARACTER = UF_CHARACTER .
*
*    LS_CRITERIA-ITEMNO_ACC = UF_ITEMNO .
*    LS_CRITERIA-FIELDNAME   = UF_FIELDNAME.
*    LS_CRITERIA-CHARACTER  = LF_CHARACTER.
*    APPEND LS_CRITERIA TO GT_CRITERIA .
*  ENDIF.
*
*ENDFORM.
