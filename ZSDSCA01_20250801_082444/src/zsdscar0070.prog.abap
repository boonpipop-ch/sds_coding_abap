*-----------------------------------------------------------------------
*  Program ID         : ZSDSCAR0070
*  Creation Date      : 12.07.2024
*  Author             : Waraporn S.(Eviden)
*  Add-on ID          : N/A
*  Description        : Create or change customer sales view,
*                       assign partner function and create contact
*                       person relationship to BP
*  Purpose            : For data migration only
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCAR0070.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS  ##NEEDED.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_XLSX TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.

TYPES: BEGIN OF TS_COUNT,
         TOTAL TYPE  I,
         SUCCS TYPE  I,
         ERROR TYPE  I,
       END OF TS_COUNT.

TYPES: BEGIN OF TS_PROC_INFO,
         DATUM TYPE  SY-DATUM,
         UZEIT TYPE  SY-UZEIT,
         UNAME TYPE  SY-UNAME,
         IFILE TYPE  STRING,
         MODE  TYPE  TEXT50,
         COUNT TYPE  TS_COUNT,
       END OF TS_PROC_INFO.

TYPES: BEGIN OF TS_MESSG,
         MSGTY TYPE SY-MSGTY,
         MSGID TYPE SY-MSGID,
         MSGNO TYPE SY-MSGNO,
         MSGTX TYPE BAPI_MSG,
       END OF TS_MESSG.
TYPES: TT_MESSG TYPE STANDARD TABLE OF TS_MESSG
                WITH DEFAULT KEY.

TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSCAS020.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                 WITH DEFAULT KEY.

* Customer Sales View
TYPES: BEGIN OF TS_KEY_SALES_VIEW,
         RLTYP        TYPE BUT100-RLTYP,
         PARTNER      TYPE BUT000-PARTNER,
         PARTNER_GUID TYPE BUT000-PARTNER_GUID,
         KUNNR        TYPE KNVV-KUNNR,
         VKORG        TYPE KNVV-VKORG,
         VTWEG        TYPE KNVV-VTWEG,
         SPART        TYPE KNVV-SPART,
         RLTYP_EXIST  TYPE FLAG,
         KEY_EXIST    TYPE FLAG,
       END OF TS_KEY_SALES_VIEW.

TYPES: BEGIN OF TS_MAIN_SALES_VIEW,
         BU_GROUP TYPE BUT000-BU_GROUP,
         KDGRP    TYPE KNVV-KDGRP,
         VKBUR    TYPE KNVV-VKBUR,
         VKGRP    TYPE KNVV-VKGRP,
         KLABC    TYPE KNVV-KLABC,
         WAERS    TYPE KNVV-WAERS,
         KONDA    TYPE KNVV-KONDA,
         KALKS    TYPE KNVV-KALKS,
         VERSG    TYPE KNVV-VERSG,
         KZAZU    TYPE KNVV-KZAZU,
         VWERK    TYPE KNVV-VWERK,
         VSBED    TYPE KNVV-VSBED,
         INCO1    TYPE KNVV-INCO1,
         INCO2_L  TYPE KNVV-INCO2_L,
         ZTERM    TYPE KNVV-ZTERM,
         KTGRD    TYPE KNVV-KTGRD,
         TAXKD    TYPE KNVI-TAXKD,
         KVGR1    TYPE KNVV-KVGR1,
         KVGR2    TYPE KNVV-KVGR2,
         AUFSD    TYPE KNVV-AUFSD,
         LIFSD    TYPE KNVV-LIFSD,
         FAKSD    TYPE KNVV-FAKSD,
       END OF TS_MAIN_SALES_VIEW.

TYPES: BEGIN OF TS_MAINX_SALES_VIEW,
         BU_GROUP TYPE FLAG,
         KDGRP    TYPE FLAG,
         VKBUR    TYPE FLAG,
         VKGRP    TYPE FLAG,
         KLABC    TYPE FLAG,
         WAERS    TYPE FLAG,
         KONDA    TYPE FLAG,
         KALKS    TYPE FLAG,
         VERSG    TYPE FLAG,
         KZAZU    TYPE FLAG,
         VWERK    TYPE FLAG,
         VSBED    TYPE FLAG,
         INCO1    TYPE FLAG,
         INCO2_L  TYPE FLAG,
         ZTERM    TYPE FLAG,
         KTGRD    TYPE FLAG,
         TAXKD    TYPE FLAG,
         KVGR1    TYPE FLAG,
         KVGR2    TYPE FLAG,
         AUFSD    TYPE FLAG,
         LIFSD    TYPE FLAG,
         FAKSD    TYPE FLAG,
       END OF TS_MAINX_SALES_VIEW.

TYPES: BEGIN OF TS_SALES_VIEW,
         ROWNO TYPE FPM_ROW,
         KEY   TYPE TS_KEY_SALES_VIEW,
         MAIN  TYPE TS_MAIN_SALES_VIEW,
         MAINX TYPE TS_MAINX_SALES_VIEW,
         MESSG TYPE TT_MESSG,
       END OF TS_SALES_VIEW.
TYPES: TT_SALES_VIEW TYPE STANDARD TABLE OF TS_SALES_VIEW.

* Partner Function
TYPES: BEGIN OF TS_KEY_PARTNER_FUNC,
         RLTYP        TYPE BUT100-RLTYP,
         PARTNER      TYPE BUT000-PARTNER,
         PARTNER_GUID TYPE BUT000-PARTNER_GUID,
         KUNNR        TYPE KNVV-KUNNR,
         VKORG        TYPE KNVV-VKORG,
         VTWEG        TYPE KNVV-VTWEG,
         SPART        TYPE KNVV-SPART,
         RLTYP_EXIST  TYPE FLAG,
         KEY_EXIST    TYPE FLAG,
       END OF TS_KEY_PARTNER_FUNC.

TYPES: BEGIN OF TS_MAIN_PARTNER_FUNC,
         PARZA TYPE KNVP-PARZA,
         PTYPE TYPE CHAR1, "A-Add,M-Modify,D-Delete
         PARVW TYPE KNVP-PARVW,
         NRART TYPE TPAR-NRART,
         GPANR TYPE GPANR,
       END OF TS_MAIN_PARTNER_FUNC.

TYPES: BEGIN OF TS_PARTNER_FUNC,
         ROWNO TYPE FPM_ROW,
         KEY   TYPE TS_KEY_PARTNER_FUNC,
         MAIN  TYPE TS_MAIN_PARTNER_FUNC,
         MESSG TYPE TT_MESSG,
       END OF TS_PARTNER_FUNC.
TYPES: TT_PARTNER_FUNC TYPE STANDARD TABLE OF TS_PARTNER_FUNC.

* Contact Person Relationship
TYPES: BEGIN OF TS_MAIN_RELATIONSHIP,
         PARTNER      TYPE BUT000-PARTNER,
         PARTNER_GUID TYPE BUT000-PARTNER_GUID,
         PARTNER2     TYPE BUT050-PARTNER2,
         DEPARTMENT   TYPE BU_ABTNR,
       END OF TS_MAIN_RELATIONSHIP.

TYPES: BEGIN OF TS_RELATIONSHIP,
         ROWNO TYPE FPM_ROW,
         MAIN  TYPE TS_MAIN_RELATIONSHIP,
         MESSG TYPE TT_MESSG,
       END OF TS_RELATIONSHIP.
TYPES: TT_RELATIONSHIP TYPE STANDARD TABLE OF TS_RELATIONSHIP.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
TYPES: BEGIN OF TS_TEMPLATE1 ##NEEDED,
         PARTNER TYPE BUT000-PARTNER,
         VKORG   TYPE KNVV-VKORG,
         VTWEG   TYPE KNVV-VTWEG,
         SPART   TYPE KNVV-SPART,
         KDGRP   TYPE KNVV-KDGRP,
         VKBUR   TYPE KNVV-VKBUR,
         VKGRP   TYPE KNVV-VKGRP,
         KLABC   TYPE KNVV-KLABC,
         WAERS   TYPE KNVV-WAERS,
         KONDA   TYPE KNVV-KONDA,
         KALKS   TYPE KNVV-KALKS,
         VERSG   TYPE KNVV-VERSG,
         KZAZU   TYPE KNVV-KZAZU,
         VWERK   TYPE KNVV-VWERK,
         VSBED   TYPE KNVV-VSBED,
         INCO1   TYPE KNVV-INCO1,
         INCO2_L TYPE KNVV-INCO2_L,
         ZTERM   TYPE KNVV-ZTERM,
         KTGRD   TYPE KNVV-KTGRD,
         TAXKD   TYPE KNVI-TAXKD,
         KVGR1   TYPE KNVV-KVGR1,
         KVGR2   TYPE KNVV-KVGR2,
         AUFSD   TYPE KNVV-AUFSD,
         LIFSD   TYPE KNVV-LIFSD,
         FAKSD   TYPE KNVV-FAKSD,
       END OF TS_TEMPLATE1.

TYPES: BEGIN OF TS_TEMPLATE2 ##NEEDED,
         PARTNER TYPE BUT000-PARTNER,
         VKORG   TYPE KNVV-VKORG,
         VTWEG   TYPE KNVV-VTWEG,
         SPART   TYPE KNVV-SPART,
         PARVW   TYPE KNVP-PARVW,
         GPANR   TYPE GPANR,
       END OF TS_TEMPLATE2.

TYPES: BEGIN OF TS_TEMPLATE3 ##NEEDED,
         PARTNER    TYPE BUT000-PARTNER,
         PARTNER2   TYPE BU_PARTNER,
         DEPARTMENT TYPE BU_ABTNR,
       END OF TS_TEMPLATE3.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE     TYPE CHAR1     VALUE 'X',
  GC_TCODE    TYPE SY-TCODE  VALUE 'ZSDSCA003',
  GC_BP_ROLE  TYPE BUT100-RLTYP VALUE 'FLCU01',
  GC_RELATION TYPE BUT050-RELTYP VALUE 'BUR001',
  GC_ALAND    TYPE KNVI-ALAND VALUE 'TH',
  GC_TATYP    TYPE KNVI-TATYP VALUE 'MWST'.

CONSTANTS: BEGIN OF GC_PTYPE,
             ADD TYPE CHAR1 VALUE 'A',
             DEL TYPE CHAR1 VALUE 'D',
           END OF GC_PTYPE.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_PROC TYPE TS_PROC_INFO                                  ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
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

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCAS020'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 20,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 80.

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
* Mode
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
* Create (Customer Sales View)
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(58) TEXT-S04 FOR FIELD RB_CRE.
    PARAMETERS: RB_CRE RADIOBUTTON GROUP G1.
  SELECTION-SCREEN END OF LINE.

* Change (Sales View, Assign Partner Function, Relationship)
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(58) TEXT-S05 FOR FIELD RB_CHG.
    PARAMETERS: RB_CHG RADIOBUTTON GROUP G1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

* Option
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  PARAMETERS: RB_SALES RADIOBUTTON GROUP G2 USER-COMMAND U1,
              RB_PARTN RADIOBUTTON GROUP G2,
              RB_RELAT RADIOBUTTON GROUP G2.
SELECTION-SCREEN END OF BLOCK B2.

* Process Type for partner function
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-S10.
  PARAMETERS: RB_PADD RADIOBUTTON GROUP G3 MODIF ID PAR,
              RB_PDEL RADIOBUTTON GROUP G3 MODIF ID PAR.
SELECTION-SCREEN END OF BLOCK B4.

* Input File
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03 .
  PARAMETERS:
  P_FILE TYPE  STRING LOWER CASE.

  SELECTION-SCREEN BEGIN OF LINE.
*   Text-s03: Start Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S06 FOR FIELD P_BEGROW.
    PARAMETERS:
      P_BEGROW TYPE I DEFAULT 2.
*   Text-s04: Start Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S07 FOR FIELD P_BEGCOL.
    PARAMETERS:
      P_BEGCOL TYPE I DEFAULT 1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
*   Text-s05: End Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S08 FOR FIELD P_ENDROW.
    PARAMETERS:
      P_ENDROW TYPE I DEFAULT 9999.
*   Text-s06: End Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S09 FOR FIELD P_ENDCOL.
    PARAMETERS:
      P_ENDCOL TYPE I DEFAULT 25.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

PARAMETERS:
  CB_TEST TYPE FLAG AS CHECKBOX DEFAULT 'X'.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Set Selection Screen Format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_FILE.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CASE GC_TRUE.
    WHEN RB_SALES.
      PERFORM F_PROCESS_DATA_SALES_VIEW   USING CB_TEST
                                       CHANGING GT_RESULT
                                                GS_PROC.
    WHEN RB_PARTN.
      PERFORM F_PROCESS_DATA_PARTNER_FUNC USING CB_TEST
                                       CHANGING GT_RESULT
                                                GS_PROC.
    WHEN RB_RELAT.
      PERFORM F_PROCESS_DATA_RELATIONSHIP USING CB_TEST
                                       CHANGING GT_RESULT
                                                GS_PROC.
  ENDCASE.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
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
      FILE_FILTER             = 'Excel File(XLSX)|*.XLSX' ##NO_TEXT
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
*  Form F_SET_SELECTION_SCREEN_FORMAT
*----------------------------------------------------------------------*
*  Set Selection Screen Format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .

  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
*     Process type for partner function; show only when RB_PARTN = 'X'
      WHEN 'PAR'.
        IF RB_PARTN EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen input
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  IF P_FILE IS INITIAL.
*   Error: Please enter a valid filename.
    MESSAGE E002(ZSDSCA01).
    RETURN.
  ENDIF.

  IF RB_PARTN EQ GC_TRUE AND
     RB_CRE EQ GC_TRUE.
*   Error: Please select change mode for assign partner function
    MESSAGE E000(ZSDSCA01) WITH TEXT-E13 SPACE SPACE SPACE.
    RETURN.
  ENDIF.

  IF RB_RELAT EQ GC_TRUE AND
     RB_CRE EQ GC_TRUE.
*   Error: Please select change mode for maintain relationship
    MESSAGE E000(ZSDSCA01) WITH TEXT-E15 SPACE SPACE SPACE.
    RETURN.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PROCESS_DATA_SALES_VIEW
*---------------------------------------------------------------------*
*  Processing data: Customer Sales View
*---------------------------------------------------------------------*
FORM F_PROCESS_DATA_SALES_VIEW  USING  UF_TEST   TYPE FLAG
                              CHANGING CT_RESULT TYPE TT_RESULT
                                       CS_PROC TYPE TS_PROC_INFO.

  DATA:
    LT_XLSX   TYPE TT_XLSX,
    LT_DATA   TYPE TT_SALES_VIEW,
    LT_RESULT TYPE TT_RESULT,
    LS_COUNT  TYPE TS_COUNT.

* Initialize Output
  CLEAR: CT_RESULT, CS_PROC.

* -----------------------------
* Read File into Text table
* -----------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

* -----------------------------
* Prepare data to internal table
* -----------------------------
  PERFORM F_ASSIGN_DATA_SALES_VIEW   USING LT_XLSX
                                  CHANGING LT_DATA
                                           LT_RESULT.
  IF LT_RESULT IS NOT INITIAL.
*   Collect Final Result
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
    CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
  ENDIF.

* -----------------------------
* Create or Change Customer Sales View
* -----------------------------
  PERFORM F_PREPARE_SALES_VIEW USING LT_DATA
                                     UF_TEST
                            CHANGING LT_RESULT
                                     LS_COUNT.

* -----------------------------
* Collect Final Result
* -----------------------------
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LS_COUNT-ERROR.
  CS_PROC-COUNT-SUCCS = CS_PROC-COUNT-SUCCS + LS_COUNT-SUCCS.

  PERFORM F_ASSIGN_PROCESSING_INFO CHANGING CS_PROC.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_READ_FILE
*----------------------------------------------------------------------*
*  Read File into Text Table
*----------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = P_FILE
      IF_READ_ACTIVE_WORKSHEET    = 'X'
    IMPORTING
      ET_XLSX_DATA                = CT_XLSX
    EXCEPTIONS
      MISSING_FILENAME_OR_XSTRING = 1
      ERROR_READ_FILENAME         = 2
      NO_DATA_FOUND               = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
    IF SY-SUBRC EQ 2.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO DISPLAY LIKE 'E'
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    RETURN.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_DATA_SALES_VIEW
*---------------------------------------------------------------------*
*  Assign Data from XLSX Sheet to Internal table
*---------------------------------------------------------------------*
FORM F_ASSIGN_DATA_SALES_VIEW   USING  UT_XLSX TYPE TT_XLSX
                              CHANGING CT_DATA TYPE TT_SALES_VIEW
                                       CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_DATA  TYPE TS_SALES_VIEW,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_COL_START TYPE I VALUE 2,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE CHAR40,
    LF_STRING    TYPE STRING,
    LF_MSGTX     TYPE BAPI_MSG.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.

* Initialize Output
  CLEAR: CT_DATA,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 30 TEXT-P02.

* Read Active Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  ASSIGN <L_SHEET>-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LF_COL_START = P_BEGCOL.
  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE P_BEGROW AND
          LF_ROWNO LE P_ENDROW.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.

    LS_DATA-ROWNO = LF_ROWNO.

    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CLEAR: LF_STRING.

      CHECK SY-TABIX GE P_BEGCOL AND
            SY-TABIX LE P_ENDCOL.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE1'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      LF_STRING = <L_FIELD>.

      CASE LF_FNAME.
        WHEN 'PARTNER'.
          IF <L_FIELD> IS INITIAL.
*           Text-e01: Missing BP Number in the file
            LF_MSGTX = TEXT-E01.
            EXIT.
          ENDIF.

          LS_DATA-KEY-PARTNER = <L_FIELD>.
          LS_DATA-KEY-RLTYP = GC_BP_ROLE.

*         Get BP Group and check PARTNER number valid?
          PERFORM F_VALIDATE_PARTNER USING LF_STRING
                                           LS_DATA-KEY-RLTYP      "Role type FLCU01
                                  CHANGING LS_DATA-KEY-PARTNER
                                           LS_DATA-KEY-PARTNER_GUID
                                           LS_DATA-KEY-KUNNR
                                           LS_DATA-KEY-RLTYP_EXIST
                                           LS_DATA-MAIN-BU_GROUP
                                           LF_MSGTX.
          IF LF_MSGTX IS NOT INITIAL.
            EXIT.
          ENDIF.

        WHEN 'VKORG'.
          IF <L_FIELD> IS INITIAL.
*           Text-e02: Missing Sales Organization in the file
            LF_MSGTX = TEXT-E02.
            EXIT.
          ENDIF.
          LS_DATA-KEY-VKORG = <L_FIELD>.

        WHEN 'VTWEG'.
          IF <L_FIELD> IS INITIAL.
*           Text-e03: Missing Distribution Channel in the file
            LF_MSGTX = TEXT-E03.
            EXIT.
          ENDIF.
          LS_DATA-KEY-VTWEG = <L_FIELD>.

        WHEN 'SPART'.
          IF <L_FIELD> IS INITIAL.
*           Text-e04: Missing Division in the file
            LF_MSGTX = TEXT-E04.
            EXIT.
          ENDIF.
          LS_DATA-KEY-SPART = <L_FIELD>.

        WHEN 'KDGRP'.
          LS_DATA-MAIN-KDGRP = <L_FIELD>.
          LS_DATA-MAINX-KDGRP = GC_TRUE.
        WHEN 'VKBUR'.
          LS_DATA-MAIN-VKBUR = <L_FIELD>.
          LS_DATA-MAINX-VKBUR  = GC_TRUE.
        WHEN 'VKGRP'.
          LS_DATA-MAIN-VKGRP = <L_FIELD>.
          LS_DATA-MAINX-VKGRP  = GC_TRUE.
        WHEN 'KLABC'.
          LS_DATA-MAIN-KLABC = <L_FIELD>.
          LS_DATA-MAINX-KLABC  = GC_TRUE.
        WHEN 'WAERS'.
          LS_DATA-MAIN-WAERS = <L_FIELD>.
          LS_DATA-MAINX-WAERS  = GC_TRUE.
        WHEN 'KONDA'.
          LS_DATA-MAIN-KONDA = <L_FIELD>.
          LS_DATA-MAINX-KONDA  = GC_TRUE.
        WHEN 'KALKS'.
          LS_DATA-MAIN-KALKS = <L_FIELD>.
          LS_DATA-MAINX-KALKS  = GC_TRUE.
        WHEN 'VERSG'.
          LS_DATA-MAIN-VERSG = <L_FIELD>.
          LS_DATA-MAINX-VERSG  = GC_TRUE.
        WHEN 'KZAZU'.
          LS_DATA-MAIN-KZAZU = <L_FIELD>.
          LS_DATA-MAINX-KZAZU  = GC_TRUE.
        WHEN 'VWERK'.
          LS_DATA-MAIN-VWERK = <L_FIELD>.
          LS_DATA-MAINX-VWERK  = GC_TRUE.
        WHEN 'VSBED'.
          LS_DATA-MAIN-VSBED = <L_FIELD>.
          LS_DATA-MAINX-VSBED  = GC_TRUE.
        WHEN 'INCO1'.
          LS_DATA-MAIN-INCO1 = <L_FIELD>.
          LS_DATA-MAINX-INCO1  = GC_TRUE.
        WHEN 'INCO2_L'.
          LS_DATA-MAIN-INCO2_L = <L_FIELD>.
          LS_DATA-MAINX-INCO2_L  = GC_TRUE.
        WHEN 'ZTERM'.
          LS_DATA-MAIN-ZTERM = <L_FIELD>.
          LS_DATA-MAINX-ZTERM  = GC_TRUE.
        WHEN 'KTGRD'.
          LS_DATA-MAIN-KTGRD = <L_FIELD>.
          LS_DATA-MAINX-KTGRD  = GC_TRUE.
        WHEN 'TAXKD'.
          IF <L_FIELD> IS NOT INITIAL.
            LS_DATA-MAIN-TAXKD = <L_FIELD>.
            LS_DATA-MAINX-TAXKD  = GC_TRUE.
          ENDIF.
        WHEN 'KVGR1'.
          LS_DATA-MAIN-KVGR1 = <L_FIELD>.
          LS_DATA-MAINX-KVGR1  = GC_TRUE.
        WHEN 'KVGR2'.
          LS_DATA-MAIN-KVGR2 = <L_FIELD>.
          LS_DATA-MAINX-KVGR2  = GC_TRUE.
        WHEN 'AUFSD'.
          LS_DATA-MAIN-AUFSD = <L_FIELD>.
          LS_DATA-MAINX-AUFSD  = GC_TRUE.
        WHEN 'LIFSD'.
          LS_DATA-MAIN-LIFSD = <L_FIELD>.
          LS_DATA-MAINX-LIFSD  = GC_TRUE.
        WHEN 'FAKSD'.
          LS_DATA-MAIN-FAKSD = <L_FIELD>.
          LS_DATA-MAINX-FAKSD  = GC_TRUE.
      ENDCASE.
    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
*     Validate data Row Level
      PERFORM F_VALIDATE_SALES_VIEW CHANGING LS_DATA
                                             LF_MSGTX.
    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.

      INSERT LS_MESSG INTO TABLE LS_DATA-MESSG.

*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT_SALES_VIEW USING LS_DATA
                                       CHANGING CT_RESULT.

      CONTINUE.
    ENDIF.

*   Collect Data
    INSERT LS_DATA INTO TABLE CT_DATA.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_TARGET_FIELD
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  UF_TYPE   TYPE  CHAR20
                                UF_INDEX  TYPE  I
                       CHANGING CF_FIELD  TYPE  CHAR40.

  STATICS:
    LF_TYPE   TYPE  CHAR20,
    LT_FIELDS TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <L_FIELD>  TYPE  ABAP_COMPDESCR.

* Initialize Output
  CLEAR: CF_FIELD.

  IF LT_FIELDS IS INITIAL OR
     LF_TYPE NE UF_TYPE.
    CLEAR: LF_TYPE,
           LT_FIELDS.
    LF_TYPE = UF_TYPE.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( LF_TYPE ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <L_FIELD>
                       INDEX UF_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FIELD = <L_FIELD>-NAME.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT_SALES_VIEW
*----------------------------------------------------------------------*
*  Collect result from Sales View
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT_SALES_VIEW  USING  US_DATA TYPE TS_SALES_VIEW
                                CHANGING CT_RESULT TYPE  TT_RESULT.

  DATA:
    LS_RESULT TYPE TS_RESULT.

  FIELD-SYMBOLS:
    <L_MESSG> TYPE TS_MESSG.

  CLEAR LS_RESULT.
  LS_RESULT-ROWNO = US_DATA-ROWNO.
  MOVE-CORRESPONDING US_DATA-KEY TO LS_RESULT.
  MOVE-CORRESPONDING US_DATA-MAIN TO LS_RESULT.

  LOOP AT US_DATA-MESSG ASSIGNING <L_MESSG>.
    LS_RESULT-MSGTY = <L_MESSG>-MSGTY.
    LS_RESULT-MSGID = <L_MESSG>-MSGID.
    LS_RESULT-MSGNO = <L_MESSG>-MSGNO.
    LS_RESULT-MSGTX = <L_MESSG>-MSGTX.
    CASE LS_RESULT-MSGTY.
      WHEN 'S'.
        LS_RESULT-STATU = ICON_LED_GREEN.
      WHEN 'W'.
        LS_RESULT-STATU = ICON_LED_YELLOW.
      WHEN 'E' OR 'A'.
        LS_RESULT-STATU = ICON_LED_RED.
      WHEN OTHERS.
        LS_RESULT-STATU = ICON_LED_INACTIVE.
    ENDCASE.
    EXIT.
  ENDLOOP.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_PROCESSING_INFO
*---------------------------------------------------------------------*
* Assign Processing Information
*---------------------------------------------------------------------*
FORM F_ASSIGN_PROCESSING_INFO  CHANGING CS_PROC TYPE TS_PROC_INFO.

* Assign Processing Info
  CS_PROC-DATUM = SY-DATUM.
  CS_PROC-UZEIT = SY-UZEIT.
  CS_PROC-UNAME = SY-UNAME.
  CS_PROC-IFILE = P_FILE.

  CASE GC_TRUE.
    WHEN RB_SALES.
      IF RB_CRE EQ GC_TRUE.
*       Text-X03: Create Customer Sales View
        CS_PROC-MODE = TEXT-X03.
      ELSEIF RB_CHG EQ GC_TRUE.
*       Text-X04: Change Customer Sales View
        CS_PROC-MODE = TEXT-X04.
      ENDIF.

    WHEN RB_PARTN.
      IF RB_PADD EQ GC_TRUE.
*       Text-X05: Assign Partner Function: Add
        CS_PROC-MODE = TEXT-X05.
      ELSEIF RB_PDEL EQ GC_TRUE.
*       Text-X06: Assign Partner Function: Delete
        CS_PROC-MODE = TEXT-X06.
      ENDIF.

    WHEN RB_RELAT.
*     Text-X07: Contact Person Relationship
      CS_PROC-MODE = TEXT-X07.
  ENDCASE.

  IF CB_TEST EQ GC_TRUE.
*   Text-x01: Test Run
    CS_PROC-MODE  = |{ CS_PROC-MODE }-{ TEXT-X01 }|.
  ELSE.
*   Text-x02: Production Run
    CS_PROC-MODE  = |{ CS_PROC-MODE }-{ TEXT-X02 }|.
  ENDIF.

  CS_PROC-COUNT-TOTAL = CS_PROC-COUNT-SUCCS + CS_PROC-COUNT-ERROR.

* Show Message Complete
* Text-i01: Processing completed.
  MESSAGE S000(ZSDSCA01) WITH TEXT-001 SPACE SPACE SPACE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_PARTNER
*---------------------------------------------------------------------*
* Validate partner number with BUT000
*   - Get Business Partner Grouping
*   - Get Business Partner GUID
*   - Check Role type FLCU01 exist? GC_BP_ROLE
*---------------------------------------------------------------------*
FORM F_VALIDATE_PARTNER  USING UF_STRING TYPE STRING
                               UF_BP_ROLE TYPE BUT100-RLTYP
                      CHANGING CF_PARTNER TYPE BUT000-PARTNER
                               CF_PARTNER_GUID TYPE BUT000-PARTNER_GUID
                               CF_KUNNR TYPE KNA1-KUNNR
                               CF_RLTYP_EXIST TYPE FLAG
                               CF_BU_GROUP TYPE BUT000-BU_GROUP
                               CF_MSGTX TYPE BAPI_MSG.

  DATA: LV_PARTNER TYPE BUT000-PARTNER.

  CLEAR: CF_PARTNER,
         CF_PARTNER_GUID,
         CF_KUNNR,
         CF_RLTYP_EXIST,
         CF_BU_GROUP,
         CF_MSGTX.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LV_PARTNER.

* Assign Output
  CF_PARTNER = LV_PARTNER.

  SELECT SINGLE BU_GROUP PARTNER_GUID
    INTO (CF_BU_GROUP, CF_PARTNER_GUID)
    FROM BUT000
   WHERE PARTNER EQ CF_PARTNER.

  IF SY-SUBRC NE 0.
*   Text-e05 : Invalid BP Number:
    CONCATENATE TEXT-E05 CF_PARTNER
      INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  PERFORM F_CHECK_ROLE_EXIST USING CF_PARTNER
                                   UF_BP_ROLE
                          CHANGING CF_RLTYP_EXIST.

  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  CF_PARTNER
                                     CHANGING CF_KUNNR.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_CHECK_ROLE_EXIST
*----------------------------------------------------------------------*
*  Check Partner role already exist?
*----------------------------------------------------------------------*
FORM F_CHECK_ROLE_EXIST  USING  UF_PARTNER  TYPE  BUT000-PARTNER
                                UF_RLTYP    TYPE  BUT100-RLTYP
                       CHANGING CF_EXIST    TYPE  FLAG.

  DATA:
    LV_PARTNER  TYPE  BUT100-PARTNER ##NEEDED.

* Initialize Output
  CLEAR: CF_EXIST.

  SELECT PARTNER
      UP TO 1 ROWS
    INTO LV_PARTNER
    FROM BUT100
   WHERE PARTNER EQ UF_PARTNER
     AND RLTYP   EQ UF_RLTYP
   ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    CF_EXIST = GC_TRUE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PREPARE_SALES_VIEW
*---------------------------------------------------------------------*
* Prepare data for maintain BP Sales View
*---------------------------------------------------------------------*
FORM F_PREPARE_SALES_VIEW USING UT_DATA TYPE TT_SALES_VIEW
                                UF_TEST TYPE FLAG
                       CHANGING CT_RESULT TYPE TT_RESULT
                                CS_COUNT TYPE TS_COUNT.

  DATA: LT_MESSG TYPE TT_MESSG.
  DATA: LS_DATA TYPE TS_SALES_VIEW.
  DATA: LV_ERROR TYPE FLAG.

  CLEAR: CT_RESULT, CS_COUNT.

* Show progress
* Text-p03 : Uploading data...
  MC_SHOW_PROGRESS 40 TEXT-P03.

* Processing each record
  LOOP AT UT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_MAINTAIN_SALES_VIEW  USING LS_DATA
                                           UF_TEST
                                  CHANGING LT_MESSG
                                           LV_ERROR.
      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.

    CS_COUNT-TOTAL = CS_COUNT-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      CS_COUNT-ERROR = CS_COUNT-ERROR + 1.
    ELSE.
      CS_COUNT-SUCCS = CS_COUNT-SUCCS + 1.
    ENDIF.

*   Collect Validation Error Result
    PERFORM F_COLLECT_RESULT_SALES_VIEW USING LS_DATA
                                     CHANGING CT_RESULT.

  ENDLOOP.

* Show Final Message for Processing completed
* Text-i01: Processing completed.
  MESSAGE S000(ZSDSCA01) WITH TEXT-001 SPACE SPACE SPACE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_MAINTAIN_SALES_VIEW
*---------------------------------------------------------------------*
*  Maintain Customer Partner Sales view
*---------------------------------------------------------------------*
FORM F_MAINTAIN_SALES_VIEW  USING    US_DATA TYPE TS_SALES_VIEW
                                    UF_TEST TYPE FLAG
                           CHANGING CT_MESSG TYPE TT_MESSG
                                    CF_ERROR TYPE FLAG.

  DATA:
    LT_BAPI_DATA TYPE  CVIS_EI_EXTERN_T,
    LT_MSGMAP    TYPE  MDG_BS_BP_MSGMAP_T,
    LT_RETURN    TYPE  BAPIRETM.

  DATA:
    LS_BAPI_DATA TYPE  CVIS_EI_EXTERN,
    LS_BAPI_ROLE TYPE  BUS_EI_BUPA_ROLES.

  DATA:
    LV_RLTYP_EXIST TYPE FLAG,
    LV_KUNNR_EXIST TYPE FLAG.


* Initialize Output
  CLEAR: CT_MESSG,
         CF_ERROR.

  CLEAR LS_BAPI_DATA.

* ---------------------------
* Assign Header Data
* ---------------------------
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER = US_DATA-KEY-PARTNER.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = US_DATA-KEY-PARTNER_GUID.

* ---------------------------
* Assign Role
* ---------------------------
* Re-check again since previous post may create role already
  PERFORM F_CHECK_ROLE_EXIST  USING  US_DATA-KEY-PARTNER
                                     US_DATA-KEY-RLTYP
                            CHANGING LV_RLTYP_EXIST.

  IF LV_RLTYP_EXIST IS INITIAL.
    LS_BAPI_ROLE-TASK = 'I'.
  ELSE.
    LS_BAPI_ROLE-TASK = 'U'.
  ENDIF.
  LS_BAPI_ROLE-DATA_KEY          = US_DATA-KEY-RLTYP.
  LS_BAPI_ROLE-DATA-ROLECATEGORY = US_DATA-KEY-RLTYP.
  LS_BAPI_ROLE-DATA-VALID_FROM   = SY-DATUM.
  LS_BAPI_ROLE-DATA-VALID_TO     = '99991231'.
  LS_BAPI_ROLE-DATAX-VALID_FROM  = GC_TRUE.
  LS_BAPI_ROLE-DATAX-VALID_TO    = GC_TRUE.
  INSERT LS_BAPI_ROLE INTO TABLE LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ROLE-ROLES.

* ---------------------------
* Assign Customer
* ---------------------------
  LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_INSTANCE = US_DATA-KEY-PARTNER.
  PERFORM F_CHECK_CUSTOMER_EXIST   USING US_DATA-KEY-PARTNER
                                CHANGING LV_KUNNR_EXIST.
  IF LV_KUNNR_EXIST IS INITIAL.
    LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_TASK = 'I'.
  ELSE.
    LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_TASK = 'U'.
  ENDIF.
  PERFORM F_ASSIGN_BAPI_SALES_VIEW  USING  US_DATA-KEY
                                           US_DATA-MAIN
                                           US_DATA-MAINX
                                  CHANGING LS_BAPI_DATA-CUSTOMER.

* Validate Data
  CALL METHOD CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
    EXPORTING
      I_DATA        = LS_BAPI_DATA
    IMPORTING
      ET_RETURN_MAP = LT_MSGMAP.

  IF LT_MSGMAP IS INITIAL.

    INSERT LS_BAPI_DATA INTO TABLE LT_BAPI_DATA.

*   Post Data
    CALL METHOD CL_MD_BP_MAINTAIN=>MAINTAIN
      EXPORTING
        I_DATA     = LT_BAPI_DATA
        I_TEST_RUN = UF_TEST
      IMPORTING
        E_RETURN   = LT_RETURN.

    PERFORM F_CHECK_BAPI_RETURN  USING  LT_RETURN
                                        UF_TEST
                               CHANGING CF_ERROR
                                        CT_MESSG.

    IF UF_TEST IS INITIAL AND
       CF_ERROR IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP  USING  LT_MSGMAP
                                  CHANGING CF_ERROR
                                           CT_MESSG.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ASSIGN_BAPI_SALES_VIEW
*----------------------------------------------------------------------*
*  Assign BAPI Customer Sales View Data
*----------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_SALES_VIEW  USING  US_KEY      TYPE  TS_KEY_SALES_VIEW
                                      US_MAIN     TYPE  TS_MAIN_SALES_VIEW
                                      US_MAINX    TYPE  TS_MAINX_SALES_VIEW
                             CHANGING CS_BAPI_CUST TYPE CMDS_EI_EXTERN.

  DATA:
    LS_SALES TYPE  CMDS_EI_SALES,
    LS_TAX   TYPE  CMDS_EI_TAX_IND.


  CLEAR: LS_SALES.

  IF US_KEY-KEY_EXIST EQ GC_TRUE.
    LS_SALES-TASK = 'U'.
  ELSE.
    LS_SALES-TASK = 'I'.
  ENDIF.
  LS_SALES-DATA_KEY-VKORG = US_KEY-VKORG.
  LS_SALES-DATA_KEY-VTWEG = US_KEY-VTWEG.
  LS_SALES-DATA_KEY-SPART = US_KEY-SPART.

  LS_SALES-DATA-KDGRP  = US_MAIN-KDGRP.
  LS_SALES-DATAX-KDGRP = US_MAINX-KDGRP.

  LS_SALES-DATA-VKBUR  = US_MAIN-VKBUR.
  LS_SALES-DATAX-VKBUR = US_MAINX-VKBUR.

  LS_SALES-DATA-VKGRP  = US_MAIN-VKGRP.
  LS_SALES-DATAX-VKGRP = US_MAINX-VKGRP.

  LS_SALES-DATA-KLABC = US_MAIN-KLABC.
  LS_SALES-DATAX-KLABC = US_MAINX-KLABC.

  LS_SALES-DATA-WAERS  = US_MAIN-WAERS.
  LS_SALES-DATAX-WAERS = US_MAINX-WAERS.

  LS_SALES-DATA-KONDA  = US_MAIN-KONDA.
  LS_SALES-DATAX-KONDA = US_MAINX-KONDA.

  LS_SALES-DATA-KALKS = US_MAIN-KALKS.
  LS_SALES-DATAX-KALKS = US_MAINX-KALKS.

  LS_SALES-DATA-VERSG  = US_MAIN-VERSG.
  LS_SALES-DATAX-VERSG = US_MAINX-VERSG.

  LS_SALES-DATA-KZAZU  = US_MAIN-KZAZU.
  LS_SALES-DATAX-KZAZU = US_MAINX-KZAZU.

  LS_SALES-DATA-VWERK  = US_MAIN-VWERK.
  LS_SALES-DATAX-VWERK = US_MAINX-VWERK.

  LS_SALES-DATA-VSBED  = US_MAIN-VSBED.
  LS_SALES-DATAX-VSBED = US_MAINX-VSBED.

  LS_SALES-DATA-INCO1 = US_MAIN-INCO1.
  LS_SALES-DATAX-INCO1 = US_MAINX-INCO1.

  LS_SALES-DATA-INCO2_L  = US_MAIN-INCO2_L.
  LS_SALES-DATAX-INCO2_L = US_MAINX-INCO2_L.

  LS_SALES-DATA-ZTERM  = US_MAIN-ZTERM.
  LS_SALES-DATAX-ZTERM = US_MAINX-ZTERM.

  LS_SALES-DATA-KTGRD  = US_MAIN-KTGRD.
  LS_SALES-DATAX-KTGRD = US_MAINX-KTGRD.

  LS_SALES-DATA-KVGR1  = US_MAIN-KVGR1.
  LS_SALES-DATAX-KVGR1 = US_MAINX-KVGR1.

  LS_SALES-DATA-KVGR2  = US_MAIN-KVGR2.
  LS_SALES-DATAX-KVGR2 = US_MAINX-KVGR2.

  LS_SALES-DATA-AUFSD = US_MAIN-AUFSD.
  LS_SALES-DATAX-AUFSD = US_MAINX-AUFSD.

  LS_SALES-DATA-LIFSD  = US_MAIN-LIFSD.
  LS_SALES-DATAX-LIFSD = US_MAINX-LIFSD.

  LS_SALES-DATA-FAKSD = US_MAIN-FAKSD.
  LS_SALES-DATAX-FAKSD = US_MAINX-FAKSD.

  INSERT LS_SALES INTO TABLE CS_BAPI_CUST-SALES_DATA-SALES.

  IF US_MAINX-TAXKD IS NOT INITIAL.
    CLEAR: LS_TAX.
    LS_TAX-TASK = 'I'.
    LS_TAX-DATA_KEY-ALAND = GC_ALAND.
    LS_TAX-DATA_KEY-TATYP = GC_TATYP.
    LS_TAX-DATA-TAXKD     = US_MAIN-TAXKD.
    LS_TAX-DATAX-TAXKD    = US_MAINX-TAXKD.

    INSERT LS_TAX INTO TABLE CS_BAPI_CUST-CENTRAL_DATA-TAX_IND-TAX_IND.
    CS_BAPI_CUST-CENTRAL_DATA-TAX_IND-CURRENT_STATE = GC_TRUE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_CUSTOMER_EXIST
*---------------------------------------------------------------------*
* Check Customer Exist
*---------------------------------------------------------------------*
FORM F_CHECK_CUSTOMER_EXIST  USING UF_PARTNER TYPE BUT000-PARTNER
                          CHANGING CF_EXIST   TYPE FLAG.

  DATA:
    LV_KUNNR  TYPE  KNA1-KUNNR ##NEEDED.

* Initlialize Output
  CLEAR: CF_EXIST.

* Get Customer from Partner
  PERFORM F_GET_CUSTOMER_FROM_PARTNER  USING  UF_PARTNER
                                     CHANGING LV_KUNNR.
  IF LV_KUNNR IS NOT INITIAL.
    CF_EXIST = GC_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CUSTOMER_FROM_PARTNER
*&---------------------------------------------------------------------*
*& Get Customer From Partner
*&---------------------------------------------------------------------*
FORM F_GET_CUSTOMER_FROM_PARTNER USING UF_PARTNER  TYPE BUT000-PARTNER
                              CHANGING CF_KUNNR    TYPE KNA1-KUNNR.

* Initlialize Output
  CLEAR: CF_KUNNR.

  SELECT SINGLE KUNNR
    INTO CF_KUNNR
    FROM KNA1
   WHERE KUNNR EQ UF_PARTNER.

  IF SY-SUBRC NE 0.
*   Try to find Customer Code
    SELECT A~CUSTOMER
      UP TO 1 ROWS
      INTO CF_KUNNR
      FROM CVI_CUST_LINK AS A
      INNER JOIN BUT000 AS B
      ON  B~PARTNER_GUID EQ A~PARTNER_GUID
      WHERE B~PARTNER EQ UF_PARTNER
      ORDER BY A~CUSTOMER ASCENDING.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_BAPI_RETURN
*&---------------------------------------------------------------------*
*& Check BAPI Return result
*&---------------------------------------------------------------------*
FORM F_CHECK_BAPI_RETURN USING UT_RETURN TYPE BAPIRETM
                               UF_TEST   TYPE FLAG
                      CHANGING CF_ERROR  TYPE FLAG
                               CT_MESSG  TYPE TT_MESSG.

  DATA:
    LS_MESSG TYPE TS_MESSG.

  FIELD-SYMBOLS:
    <L_RETURN> TYPE BAPIRETI,
    <L_MSG>    TYPE BAPIRETC.


* Initialize Output
  CLEAR: CF_ERROR,
         CT_MESSG.

  LOOP AT UT_RETURN ASSIGNING <L_RETURN>.

    LOOP AT <L_RETURN>-OBJECT_MSG ASSIGNING <L_MSG>
                                  WHERE TYPE EQ 'A'
                                     OR TYPE EQ 'E'.
      CF_ERROR = GC_TRUE.

      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = <L_MSG>-ID.
      LS_MESSG-MSGNO = <L_MSG>-NUMBER.
      MESSAGE ID <L_MSG>-ID
              TYPE <L_MSG>-TYPE
              NUMBER <L_MSG>-NUMBER
              WITH <L_MSG>-MESSAGE_V1
                   <L_MSG>-MESSAGE_V2
                   <L_MSG>-MESSAGE_V3
                   <L_MSG>-MESSAGE_V4
              INTO LS_MESSG-MSGTX.
      INSERT LS_MESSG INTO TABLE CT_MESSG.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF CT_MESSG IS INITIAL.
    IF UF_TEST EQ GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE CT_MESSG.
    ELSE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
*     Text-i02: Upload successfully.
      LS_MESSG-MSGTX = TEXT-I02.
      INSERT LS_MESSG INTO TABLE CT_MESSG.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_BAPI_RETURNMAP
*&---------------------------------------------------------------------*
*& Check BAPI Return result
*&---------------------------------------------------------------------*
FORM F_CHECK_BAPI_RETURNMAP   USING UT_RETURN TYPE MDG_BS_BP_MSGMAP_T
                           CHANGING CF_ERROR  TYPE FLAG
                                    CT_MESSG  TYPE TT_MESSG.

  DATA:
    LS_MESSG TYPE  TS_MESSG.

  FIELD-SYMBOLS:
    <L_RETURN>  TYPE  MDG_BS_BP_MSGMAP.


* Initialize Output
  CLEAR: CF_ERROR,
         CT_MESSG.

  LOOP AT UT_RETURN ASSIGNING <L_RETURN>
                    WHERE TYPE EQ 'A'
                       OR TYPE EQ 'E'.
    CF_ERROR = GC_TRUE.
    CLEAR LS_MESSG.
    LS_MESSG-MSGTY = 'E'.
    LS_MESSG-MSGID = <L_RETURN>-ID.
    LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
    MESSAGE ID <L_RETURN>-ID
            TYPE <L_RETURN>-TYPE
            NUMBER <L_RETURN>-NUMBER
            WITH <L_RETURN>-MESSAGE_V1
                 <L_RETURN>-MESSAGE_V2
                 <L_RETURN>-MESSAGE_V3
                 <L_RETURN>-MESSAGE_V4
            INTO LS_MESSG-MSGTX.
    INSERT LS_MESSG INTO TABLE CT_MESSG.
    EXIT.
  ENDLOOP.

  IF CT_MESSG IS INITIAL.
*   Get 1st Message
    LOOP AT UT_RETURN ASSIGNING <L_RETURN>.
      CF_ERROR = GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = <L_RETURN>-ID.
      LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
      MESSAGE ID <L_RETURN>-ID
              TYPE <L_RETURN>-TYPE
              NUMBER <L_RETURN>-NUMBER
              WITH <L_RETURN>-MESSAGE_V1
                   <L_RETURN>-MESSAGE_V2
                   <L_RETURN>-MESSAGE_V3
                   <L_RETURN>-MESSAGE_V4
              INTO LS_MESSG-MSGTX.
      INSERT LS_MESSG INTO TABLE CT_MESSG.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
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
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* Determine layout
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  CASE GC_TRUE.
    WHEN RB_SALES.
      PERFORM F_FIELDCAT_SALES_VIEW CHANGING CT_FIELDCAT.
    WHEN RB_PARTN.
      PERFORM F_FIELDCAT_PARTNER_FUNC CHANGING CT_FIELDCAT.
    WHEN RB_RELAT.
      PERFORM F_FIELDCAT_RELATIONSHIP CHANGING CT_FIELDCAT.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_FIELDCAT_SALES_VIEW
*---------------------------------------------------------------------*
* ALV Field Catalog for Sales View
*---------------------------------------------------------------------*
FORM F_FIELDCAT_SALES_VIEW  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'ROWNO'.
        <L_FIELDCAT>-OUTPUTLEN = 10.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
      WHEN 'STATU'.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
*       Text-c00 : Status
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGID'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGNO'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 60   ##NUMBER_OK.

      WHEN 'PARVW' OR
           'GPANR'.
        <L_FIELDCAT>-TECH = GC_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_FIELDCAT_PARTNER_FUNC
*---------------------------------------------------------------------*
* ALV Field Catalog for Partner function
*---------------------------------------------------------------------*
FORM F_FIELDCAT_PARTNER_FUNC  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA:
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'ROWNO'.
        <L_FIELDCAT>-OUTPUTLEN = 10.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
      WHEN 'STATU'.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
*       Text-c00 : Status
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGID'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGNO'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 60   ##NUMBER_OK.

      WHEN 'PARTNER'.
      WHEN 'VKORG'.
      WHEN 'VTWEG'.
      WHEN 'SPART'.
      WHEN 'PARVW'.
      WHEN 'GPANR'.
*        <L_FIELDCAT>-NO_CONVEXT = GC_TRUE.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_FIELDCAT_RELATIONSHIP
*---------------------------------------------------------------------*
* ALV Field Catalog for Relationship
*---------------------------------------------------------------------*
FORM F_FIELDCAT_RELATIONSHIP  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA:
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'ROWNO'.
        <L_FIELDCAT>-OUTPUTLEN = 10.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
      WHEN 'STATU'.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
*       Text-c00 : Status
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGID'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGNO'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 60   ##NUMBER_OK.

      WHEN 'PARTNER'.
      WHEN 'PARTNER2'.
      WHEN 'DEPARTMENT'.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_FNAME VALUE 'ROWNO',
    LC_SORT2 TYPE  LVC_FNAME VALUE 'PARTNER'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.

* Initialize Output
  CLEAR: CT_SORT.

* Sort by Sheet
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by Row Number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '18%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '82%'.

  DATA:
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD UREF_DYNDOC_ID->ADD_TABLE
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
* Text-h01 : Date/Time:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-DATUM TO LF_TEMP1.
  WRITE GS_PROC-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : User:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : Filename:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-IFILE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Mode:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-MODE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

*-----------------------
* Add value in Line5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total record(s):
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-COUNT-TOTAL TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_HEADING_INT.

*-----------------------
* Add value in Line6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success record(s):
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-COUNT-SUCCS TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Error record(s):
  LF_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-COUNT-ERROR TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_PRINT_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 20,
    LF_COL02 TYPE  I VALUE 40,
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50.

* Text-h01 : Date/Time:
  WRITE GS_PROC-DATUM TO LF_TEMP1.
  WRITE GS_PROC-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h02 : User:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    GS_PROC-UNAME NO-GAP.

* Text-h03 : Filename:
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (150)         GS_PROC-IFILE NO-GAP ##NUMBER_OK.

* Text-h04 : Mode:
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    GS_PROC-MODE NO-GAP COLOR COL_TOTAL.

* Text-h05 : Total record(s):
  WRITE GS_PROC-COUNT-TOTAL TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_HEADING.

* Text-h06 : Success record(s):
  WRITE GS_PROC-COUNT-SUCCS TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h07 : Error record(s):
  WRITE GS_PROC-COUNT-ERROR TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_NEGATIVE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_KNVV_EXIST
*---------------------------------------------------------------------*
* Check customer sales view exist? (Table KNVV)
*---------------------------------------------------------------------*
FORM F_CHECK_KNVV_EXIST  USING    US_KEY TYPE TS_KEY_SALES_VIEW
                         CHANGING CF_EXIST TYPE FLAG.

  DATA: LV_KUNNR TYPE KNA1-KUNNR.

  CLEAR: CF_EXIST.

* Check Key Existing
  SELECT SINGLE KUNNR
    INTO LV_KUNNR
    FROM KNVV
   WHERE KUNNR EQ US_KEY-KUNNR
     AND VKORG EQ US_KEY-VKORG
     AND VTWEG EQ US_KEY-VTWEG
     AND SPART EQ US_KEY-SPART.

  IF SY-SUBRC EQ 0.
    CF_EXIST = GC_TRUE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_SALES_VIEW
*---------------------------------------------------------------------*
* Validate single row for option Customer Sales View
*---------------------------------------------------------------------*
FORM F_VALIDATE_SALES_VIEW  CHANGING CS_DATA TYPE TS_SALES_VIEW
                                     CF_MSGTX TYPE BAPI_MSG.

  CLEAR: CF_MSGTX.

  PERFORM F_CHECK_KNVV_EXIST USING CS_DATA-KEY
                             CHANGING CS_DATA-KEY-KEY_EXIST.

  CASE ABAP_TRUE.
*   Case Create: Customer sales view must not exist before create
    WHEN RB_CRE.
      IF CS_DATA-KEY-KEY_EXIST EQ GC_TRUE.
*       Text e06: Customer Sales View already exist
        CF_MSGTX = TEXT-E06.
        RETURN.
      ENDIF.

*   Case Change: Customer sales view must exist before change
    WHEN RB_CHG.
      IF CS_DATA-KEY-KEY_EXIST EQ SPACE.
*       Text e07: Customer Sales View does not exist
        CF_MSGTX = TEXT-E07.
        RETURN.
      ENDIF.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PROCESS_DATA_PARTNER_FUNC
*---------------------------------------------------------------------*
*  Processing data: Assign Partner Function
*---------------------------------------------------------------------*
FORM F_PROCESS_DATA_PARTNER_FUNC  USING UF_TEST   TYPE FLAG
                               CHANGING CT_RESULT TYPE TT_RESULT
                                        CS_PROC TYPE TS_PROC_INFO.

  DATA:
    LT_XLSX   TYPE TT_XLSX,
    LT_DATA   TYPE TT_PARTNER_FUNC,
    LT_RESULT TYPE TT_RESULT,
    LS_COUNT  TYPE TS_COUNT.

* Initialize Output
  CLEAR: CT_RESULT, CS_PROC.

* -----------------------------
* Read File into Text table
* -----------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

* -----------------------------
* Prepare data to internal table
* -----------------------------
  PERFORM F_ASSIGN_DATA_PARTNER_FUNC   USING LT_XLSX
                                    CHANGING LT_DATA
                                             LT_RESULT.
  IF LT_RESULT IS NOT INITIAL.
*   Collect Final Result
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
    CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
  ENDIF.

* -----------------------------
* Assign Partner Function
* -----------------------------
  PERFORM F_PREPARE_PARTNER_FUNC USING LT_DATA
                                       UF_TEST
                              CHANGING LT_RESULT
                                       LS_COUNT.

* -----------------------------
* Collect Final Result
* -----------------------------
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LS_COUNT-ERROR.
  CS_PROC-COUNT-SUCCS = CS_PROC-COUNT-SUCCS + LS_COUNT-SUCCS.

  PERFORM F_ASSIGN_PROCESSING_INFO CHANGING CS_PROC.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_DATA_PARTNER_FUNC
*---------------------------------------------------------------------*
*  Assign Data from XLSX Sheet to Internal table
*---------------------------------------------------------------------*
FORM F_ASSIGN_DATA_PARTNER_FUNC  USING UT_XLSX TYPE TT_XLSX
                              CHANGING CT_DATA TYPE TT_PARTNER_FUNC
                                       CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_DATA  TYPE TS_PARTNER_FUNC,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_BU_GROUP  TYPE    BUT000-BU_GROUP,
    LF_COL_START TYPE I VALUE 2,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE CHAR40,
    LF_STRING    TYPE STRING,
    LF_MSGTX     TYPE BAPI_MSG.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.

* Initialize Output
  CLEAR: CT_DATA,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 30 TEXT-P02.

* Read Active Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  ASSIGN <L_SHEET>-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LF_COL_START = P_BEGCOL.
  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE P_BEGROW AND
          LF_ROWNO LE P_ENDROW.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.

    LS_DATA-ROWNO = LF_ROWNO.

*   Process type of partner function (A-Add, D-Delete)
    CASE GC_TRUE.
      WHEN RB_PADD.
        LS_DATA-MAIN-PTYPE = GC_PTYPE-ADD.
      WHEN RB_PDEL.
        LS_DATA-MAIN-PTYPE = GC_PTYPE-DEL.
    ENDCASE.

    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CLEAR: LF_STRING.

      CHECK SY-TABIX GE P_BEGCOL AND
            SY-TABIX LE P_ENDCOL.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE2'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      LF_STRING = <L_FIELD>.

      CASE LF_FNAME.
        WHEN 'PARTNER'.
          IF <L_FIELD> IS INITIAL.
*           Text-e01: Missing BP Number in the file
            LF_MSGTX = TEXT-E01.
            EXIT.
          ENDIF.

          LS_DATA-KEY-PARTNER = <L_FIELD>.
          LS_DATA-KEY-RLTYP = GC_BP_ROLE.

*         Get BP Group and check PARTNER number valid?
          PERFORM F_VALIDATE_PARTNER USING LF_STRING
                                           LS_DATA-KEY-RLTYP      "Role type FLCU01
                                  CHANGING LS_DATA-KEY-PARTNER
                                           LS_DATA-KEY-PARTNER_GUID
                                           LS_DATA-KEY-KUNNR
                                           LS_DATA-KEY-RLTYP_EXIST
                                           LF_BU_GROUP
                                           LF_MSGTX.
          IF LF_MSGTX IS NOT INITIAL.
            EXIT.
          ENDIF.

        WHEN 'VKORG'.
          IF <L_FIELD> IS INITIAL.
*           Text-e02: Missing Sales Organization in the file
            LF_MSGTX = TEXT-E02.
            EXIT.
          ENDIF.
          LS_DATA-KEY-VKORG = <L_FIELD>.

        WHEN 'VTWEG'.
          IF <L_FIELD> IS INITIAL.
*           Text-e03: Missing Distribution Channel in the file
            LF_MSGTX = TEXT-E03.
            EXIT.
          ENDIF.
          LS_DATA-KEY-VTWEG = <L_FIELD>.

        WHEN 'SPART'.
          IF <L_FIELD> IS INITIAL.
*           Text-e04: Missing Division in the file
            LF_MSGTX = TEXT-E04.
            EXIT.
          ENDIF.
          LS_DATA-KEY-SPART = <L_FIELD>.

        WHEN 'PARVW'.
          IF <L_FIELD> IS INITIAL.
*           Text-e08: Missing Partner Function in the file
            LF_MSGTX = TEXT-E08.
            EXIT.
          ENDIF.

          PERFORM F_VALIDATE_PARVW USING LF_STRING
                                   CHANGING LS_DATA-MAIN-PARVW
                                            LS_DATA-MAIN-NRART
                                            LF_MSGTX.
          IF LF_MSGTX IS NOT INITIAL.
            EXIT.
          ENDIF.

        WHEN 'GPANR'.
          IF <L_FIELD> IS INITIAL.
*           Text-e09: Missing Personnel Number in the file
            LF_MSGTX = TEXT-E09.
            EXIT.
          ENDIF.

          PERFORM F_VALIDATE_GPANR USING LF_STRING
                                         LS_DATA-MAIN-NRART
                                CHANGING LS_DATA-MAIN-GPANR
                                         LF_MSGTX.

          IF LF_MSGTX IS NOT INITIAL.
            EXIT.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
*     Validate data Row Level
      PERFORM F_VALIDATE_PARTNER_FUNC CHANGING LS_DATA
                                               LF_MSGTX.
    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.

      INSERT LS_MESSG INTO TABLE LS_DATA-MESSG.

*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT_PARTNER_FUNC USING LS_DATA
                                         CHANGING CT_RESULT.

      CONTINUE.
    ENDIF.

*   Collect Data
    INSERT LS_DATA INTO TABLE CT_DATA.

  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_PARTNER_FUNC
*---------------------------------------------------------------------*
* Validate single row for option Partner Function
*---------------------------------------------------------------------*
FORM F_VALIDATE_PARTNER_FUNC  CHANGING CS_DATA TYPE TS_PARTNER_FUNC
                                       CF_MSGTX TYPE BAPI_MSG.

  DATA: LV_RLTYP_EXIST        TYPE FLAG,
        LV_PARTNER_FUNC_EXIST TYPE FLAG.

  CLEAR: CF_MSGTX.

  PERFORM F_CHECK_ROLE_EXIST  USING  CS_DATA-KEY-PARTNER
                                     CS_DATA-KEY-RLTYP
                            CHANGING LV_RLTYP_EXIST.

  IF LV_RLTYP_EXIST EQ SPACE.
*   Text e11: BP Role does not exist:
    CONCATENATE TEXT-E11
                CS_DATA-KEY-RLTYP
      INTO CF_MSGTX SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  PERFORM F_CHECK_KNVV_EXIST USING CS_DATA-KEY
                             CHANGING CS_DATA-KEY-KEY_EXIST.

  IF CS_DATA-KEY-KEY_EXIST EQ SPACE.
*   Text e10: Sales View for Partner does not exist
    CF_MSGTX = TEXT-E10.
    RETURN.
  ENDIF.

  PERFORM F_CHECK_EXISTING_PARTNER_FUNC USING CS_DATA
                                     CHANGING CS_DATA-MAIN-PARZA
                                              LV_PARTNER_FUNC_EXIST.

  CASE CS_DATA-MAIN-PTYPE.
    WHEN GC_PTYPE-ADD.    "Add new partner function
      IF LV_PARTNER_FUNC_EXIST EQ GC_TRUE.
*       Text e16: Partner function already exist
        CF_MSGTX = TEXT-E16.
        RETURN.
      ENDIF.

    WHEN GC_PTYPE-DEL.    "Delete existing partner function
      IF LV_PARTNER_FUNC_EXIST EQ SPACE.
*       Text e17: Cannot find partner data to be deleted
        CF_MSGTX = TEXT-E17.
        RETURN.
      ENDIF.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_PARVW
*---------------------------------------------------------------------*
* Check partner function is valid?
*---------------------------------------------------------------------*
FORM F_VALIDATE_PARVW  USING    UF_STRING TYPE STRING
                       CHANGING CF_PARVW TYPE KNVP-PARVW
                                CF_NRART TYPE TPAR-NRART
                                CF_MSGTX TYPE BAPI_MSG.

  TYPES: BEGIN OF LTY_TPAR,
           PARVW TYPE  TPAR-PARVW,
           NRART TYPE  TPAR-NRART,
         END OF LTY_TPAR.
  TYPES: LTTY_TPAR TYPE SORTED TABLE OF LTY_TPAR
                        WITH UNIQUE KEY PARVW.

  STATICS:
    LS_TPAR TYPE  LTY_TPAR,
    LT_TPAR TYPE  LTTY_TPAR.

  DATA:
    LV_PARVW TYPE  LTY_TPAR-PARVW.

* Initialize Output
  CLEAR: CF_PARVW,
         CF_NRART,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 2
  IF STRLEN( UF_STRING ) GT 2.
*   Text-e12 : Invalid Partner Function:
    CONCATENATE TEXT-E12 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LV_PARVW.

* Check Buffer
  IF LS_TPAR-PARVW NE LV_PARVW.
*   Validate with Memory
    READ TABLE LT_TPAR INTO LS_TPAR
                        WITH KEY PARVW = LV_PARVW
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TPAR.
*     Validate with Database
      SELECT SINGLE PARVW NRART
        INTO LS_TPAR
        FROM TPAR
       WHERE PARVW EQ LV_PARVW.

      IF SY-SUBRC NE 0.
*       Text-e12 : Invalid Partner Function:
        CONCATENATE TEXT-E12 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TPAR INTO TABLE LT_TPAR.
    ENDIF.
  ENDIF.

* Assign Output
  CF_PARVW = LS_TPAR-PARVW.
  CF_NRART = LS_TPAR-NRART.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_GPANR
*---------------------------------------------------------------------*
* Check partner number to be assigned partner function is valid?
*---------------------------------------------------------------------*
FORM F_VALIDATE_GPANR   USING   UF_STRING TYPE STRING
                                UF_NRART TYPE TPAR-NRART
                       CHANGING CF_GPANR TYPE GPANR
                                CF_MSGTX TYPE BAPI_MSG.

  DATA:
    LV_KUNNR TYPE  KNA1-KUNNR,
    LV_PERNR TYPE  PA0003-PERNR.

* Initialize Output
  CLEAR: CF_GPANR,
         CF_MSGTX.

  CASE UF_NRART.
    WHEN 'KU'.
      PERFORM F_VALIDATE_CUSTOMER  USING  UF_STRING
                                 CHANGING LV_KUNNR
                                          CF_MSGTX.
      CF_GPANR = LV_KUNNR.

    WHEN 'PE'.
      PERFORM F_VALIDATE_PERSON  USING  UF_STRING
                               CHANGING LV_PERNR
                                        CF_MSGTX.
      CF_GPANR = LV_PERNR.

    WHEN OTHERS.
      CF_GPANR = UF_STRING.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_CUSTOMER
*&---------------------------------------------------------------------*
*& Validate Customer Number
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CUSTOMER   USING UF_STRING TYPE STRING
                        CHANGING CF_KUNNR  TYPE KNA1-KUNNR
                                 CF_MSGTX  TYPE BAPI_MSG.

  TYPES: BEGIN OF LTY_KNA1,
           KUNNR TYPE  KNA1-KUNNR,
         END OF LTY_KNA1.
  TYPES: LTTY_KNA1  TYPE  SORTED TABLE OF LTY_KNA1
                           WITH UNIQUE KEY KUNNR.

  STATICS:
    LS_KNA1 TYPE  LTY_KNA1,
    LT_KNA1 TYPE  LTTY_KNA1.

  DATA:
    LV_LEN   TYPE  I,
    LV_KUNNR TYPE  LTY_KNA1-KUNNR.

* Initialize Output
  CLEAR: CF_KUNNR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LV_LEN = STRLEN( UF_STRING ).
  IF LV_LEN GT 10.
*   Text-e19 : Invalid Customer no.:
    CONCATENATE TEXT-E19 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LV_KUNNR.

* Check Buffer
  IF LS_KNA1-KUNNR NE LV_KUNNR.

*   Validate with Memory
    READ TABLE LT_KNA1 INTO LS_KNA1
                       WITH KEY KUNNR = LV_KUNNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_KNA1.
*     Validate with Database
      SELECT SINGLE KUNNR
        INTO LS_KNA1
        FROM KNA1
       WHERE KUNNR  EQ LV_KUNNR.                     "#EC CI_SEL_NESTED
      IF SY-SUBRC NE 0.
*       Text-e19 : Invalid Customer no.:
        CONCATENATE TEXT-E19 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_KNA1 INTO TABLE LT_KNA1.
    ENDIF.

  ENDIF.

* Assign Output
  CF_KUNNR = LS_KNA1-KUNNR.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_PERSON
*----------------------------------------------------------------------*
*  Validate Person
*----------------------------------------------------------------------*
FORM F_VALIDATE_PERSON  USING  UF_STRING TYPE STRING
                      CHANGING CF_PERNR  TYPE PA0003-PERNR
                               CF_MSGTX  TYPE BAPI_MSG.


  TYPES: BEGIN OF LTY_PA0003,
           PERNR TYPE PA0003-PERNR,
         END OF LTY_PA0003.
  TYPES: LTTY_PA0003 TYPE SORTED TABLE OF LTY_PA0003
                          WITH UNIQUE KEY PERNR.

  STATICS:
    LS_PA0003 TYPE  LTY_PA0003,
    LT_PA0003 TYPE  LTTY_PA0003.

  DATA:
    LV_LEN   TYPE  I,
    LV_PERNR TYPE  LTY_PA0003-PERNR.

* Initialize Output
  CLEAR: CF_PERNR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 8
  LV_LEN = STRLEN( UF_STRING ).
  IF LV_LEN GT 8.
*   Text-e20 : Invalid Person no.:
    CONCATENATE TEXT-E20 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_PERNR = UF_STRING.

* Check Buffer
  IF LS_PA0003-PERNR NE LV_PERNR.

*   Validate with Memory
    READ TABLE LT_PA0003 INTO LS_PA0003
                        WITH KEY PERNR = LV_PERNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_PA0003.
*     Validate with Database
      SELECT SINGLE PERNR
        INTO LS_PA0003
        FROM PA0003
       WHERE PERNR  EQ LV_PERNR.                     "#EC CI_SEL_NESTED
      IF SY-SUBRC NE 0.
*       Text-e67 : Invalid Person no.:
        CONCATENATE TEXT-E20 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_PA0003 INTO TABLE LT_PA0003.
    ENDIF.

  ENDIF.

* Assign Output
  CF_PERNR = LS_PA0003-PERNR.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT_PARTNER_FUNC
*----------------------------------------------------------------------*
*  Collect result from Partner Function
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT_PARTNER_FUNC USING US_DATA TYPE TS_PARTNER_FUNC
                                CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_RESULT TYPE TS_RESULT.

  FIELD-SYMBOLS:
    <L_MESSG> TYPE TS_MESSG.

  CLEAR LS_RESULT.
  LS_RESULT-ROWNO = US_DATA-ROWNO.
  MOVE-CORRESPONDING US_DATA-KEY TO LS_RESULT.
  MOVE-CORRESPONDING US_DATA-MAIN TO LS_RESULT.

  LOOP AT US_DATA-MESSG ASSIGNING <L_MESSG>.
    LS_RESULT-MSGTY = <L_MESSG>-MSGTY.
    LS_RESULT-MSGID = <L_MESSG>-MSGID.
    LS_RESULT-MSGNO = <L_MESSG>-MSGNO.
    LS_RESULT-MSGTX = <L_MESSG>-MSGTX.
    CASE LS_RESULT-MSGTY.
      WHEN 'S'.
        LS_RESULT-STATU = ICON_LED_GREEN.
      WHEN 'W'.
        LS_RESULT-STATU = ICON_LED_YELLOW.
      WHEN 'E' OR 'A'.
        LS_RESULT-STATU = ICON_LED_RED.
      WHEN OTHERS.
        LS_RESULT-STATU = ICON_LED_INACTIVE.
    ENDCASE.
    EXIT.
  ENDLOOP.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PREPARE_PARTNER_FUNC
*---------------------------------------------------------------------*
* Prepare data for assign partner function to BP
*---------------------------------------------------------------------*
FORM F_PREPARE_PARTNER_FUNC  USING UT_DATA TYPE TT_PARTNER_FUNC
                                   UF_TEST TYPE FLAG
                          CHANGING CT_RESULT TYPE TT_RESULT
                                   CS_COUNT TYPE TS_COUNT.

  DATA: LT_MESSG TYPE TT_MESSG.
  DATA: LS_DATA TYPE TS_PARTNER_FUNC.
  DATA: LV_ERROR TYPE FLAG.

  CLEAR: CT_RESULT, CS_COUNT.

* Show progress
* Text-p03 : Uploading data...
  MC_SHOW_PROGRESS 40 TEXT-P03.

* Processing each record
  LOOP AT UT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_MAINTAIN_PARTNER_FUNC  USING LS_DATA
                                             UF_TEST
                                    CHANGING LT_MESSG
                                             LV_ERROR.
      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.

    CS_COUNT-TOTAL = CS_COUNT-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      CS_COUNT-ERROR = CS_COUNT-ERROR + 1.
    ELSE.
      CS_COUNT-SUCCS = CS_COUNT-SUCCS + 1.
    ENDIF.

*   Collect Validation Error Result
    PERFORM F_COLLECT_RESULT_PARTNER_FUNC USING LS_DATA
                                       CHANGING CT_RESULT.

  ENDLOOP.

* Show Final Message for Processing completed
* Text-i01: Processing completed.
  MESSAGE S000(ZSDSCA01) WITH TEXT-001 SPACE SPACE SPACE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_MAINTAIN_PARTNER_FUNC
*---------------------------------------------------------------------*
*  Assign partner function to customer sales view
*---------------------------------------------------------------------*
FORM F_MAINTAIN_PARTNER_FUNC  USING US_DATA TYPE TS_PARTNER_FUNC
                                    UF_TEST TYPE FLAG
                           CHANGING CT_MESSG TYPE TT_MESSG
                                    CF_ERROR TYPE FLAG.
  DATA:
    LT_BAPI_DATA TYPE  CVIS_EI_EXTERN_T,
    LT_MSGMAP    TYPE  MDG_BS_BP_MSGMAP_T,
    LT_RETURN    TYPE  BAPIRETM.

  DATA:
    LS_BAPI_DATA TYPE  CVIS_EI_EXTERN,
    LS_BAPI_ROLE TYPE  BUS_EI_BUPA_ROLES.

  DATA:
    LV_RLTYP_EXIST TYPE FLAG,
    LV_KUNNR_EXIST TYPE FLAG.


* Initialize Output
  CLEAR: CT_MESSG,
         CF_ERROR.

  CLEAR LS_BAPI_DATA.

* ---------------------------
* Assign Header Data
* ---------------------------
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER = US_DATA-KEY-PARTNER.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = US_DATA-KEY-PARTNER_GUID.

* ---------------------------
* Assign Role
* ---------------------------
* Re-check again since previous post may create role already
  PERFORM F_CHECK_ROLE_EXIST  USING  US_DATA-KEY-PARTNER
                                     US_DATA-KEY-RLTYP
                            CHANGING LV_RLTYP_EXIST.
  IF LV_RLTYP_EXIST IS INITIAL.
    LS_BAPI_ROLE-TASK = 'I'.
  ELSE.
    LS_BAPI_ROLE-TASK = 'U'.
  ENDIF.
  LS_BAPI_ROLE-DATA_KEY          = US_DATA-KEY-RLTYP.
  LS_BAPI_ROLE-DATA-ROLECATEGORY = US_DATA-KEY-RLTYP.
  LS_BAPI_ROLE-DATA-VALID_FROM   = SY-DATUM.
  LS_BAPI_ROLE-DATA-VALID_TO     = '99991231'.
  LS_BAPI_ROLE-DATAX-VALID_FROM  = GC_TRUE.
  LS_BAPI_ROLE-DATAX-VALID_TO    = GC_TRUE.
  INSERT LS_BAPI_ROLE INTO TABLE LS_BAPI_DATA-PARTNER-CENTRAL_DATA-ROLE-ROLES.

* ---------------------------
* Assign Customer
* ---------------------------
  LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_INSTANCE = US_DATA-KEY-PARTNER.
  PERFORM F_CHECK_CUSTOMER_EXIST   USING US_DATA-KEY-PARTNER
                                CHANGING LV_KUNNR_EXIST.
  IF LV_KUNNR_EXIST IS INITIAL.
    LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_TASK = 'I'.
  ELSE.
    LS_BAPI_DATA-CUSTOMER-HEADER-OBJECT_TASK = 'U'.
  ENDIF.

  PERFORM F_ASSIGN_BAPI_PARTNER_FUNC USING US_DATA-KEY
                                           US_DATA-MAIN
                                  CHANGING LS_BAPI_DATA-CUSTOMER.

* Validate Data
  CALL METHOD CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
    EXPORTING
      I_DATA        = LS_BAPI_DATA
    IMPORTING
      ET_RETURN_MAP = LT_MSGMAP.

  IF LT_MSGMAP IS INITIAL.

    INSERT LS_BAPI_DATA INTO TABLE LT_BAPI_DATA.

*   Post Data
    CALL METHOD CL_MD_BP_MAINTAIN=>MAINTAIN
      EXPORTING
        I_DATA     = LT_BAPI_DATA
        I_TEST_RUN = UF_TEST
      IMPORTING
        E_RETURN   = LT_RETURN.

    PERFORM F_CHECK_BAPI_RETURN  USING  LT_RETURN
                                        UF_TEST
                               CHANGING CF_ERROR
                                        CT_MESSG.

    IF UF_TEST IS INITIAL AND
       CF_ERROR IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP  USING  LT_MSGMAP
                                  CHANGING CF_ERROR
                                           CT_MESSG.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_BAPI_PARTNER_FUNC
*---------------------------------------------------------------------*
*  Assign BAPI Partner Function
*---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_PARTNER_FUNC  USING  US_KEY TYPE TS_KEY_PARTNER_FUNC
                                        US_MAIN TYPE TS_MAIN_PARTNER_FUNC
                               CHANGING CS_BAPI_CUST TYPE CMDS_EI_EXTERN.


  DATA:
    LS_SALES TYPE CMDS_EI_SALES,
    LS_FUNC  TYPE CMDS_EI_FUNCTIONS.

  DATA:
    LV_PARZA  TYPE KNVP-PARZA.

  CLEAR: LS_SALES.

  LS_SALES-TASK = 'U'.

  LS_SALES-DATA_KEY-VKORG = US_KEY-VKORG.
  LS_SALES-DATA_KEY-VTWEG = US_KEY-VTWEG.
  LS_SALES-DATA_KEY-SPART = US_KEY-SPART.

  CLEAR LS_FUNC.

  LV_PARZA = US_MAIN-PARZA.
  CASE US_MAIN-PTYPE.
    WHEN GC_PTYPE-ADD.
      LS_FUNC-TASK = 'I'.
*     Get New Running no
      PERFORM F_GET_NEW_PARZA  USING  US_KEY
                                      US_MAIN
                             CHANGING LV_PARZA.

    WHEN GC_PTYPE-DEL.
      LS_FUNC-TASK = 'D'.
  ENDCASE.

  LS_FUNC-DATA_KEY-PARVW = US_MAIN-PARVW.
  LS_FUNC-DATA_KEY-PARZA = LV_PARZA.

  LS_FUNC-DATA-PARTNER  = US_MAIN-GPANR.
  LS_FUNC-DATAX-PARTNER = GC_TRUE.

  INSERT LS_FUNC INTO TABLE LS_SALES-FUNCTIONS-FUNCTIONS.

  INSERT LS_SALES INTO TABLE CS_BAPI_CUST-SALES_DATA-SALES.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_NEW_PARZA
*----------------------------------------------------------------------*
*  Get New Partner function Running no
*----------------------------------------------------------------------*
FORM F_GET_NEW_PARZA  USING  US_KEY TYPE TS_KEY_PARTNER_FUNC
                             US_MAIN TYPE TS_MAIN_PARTNER_FUNC
                    CHANGING CF_PARZA TYPE KNVP-PARZA.

* Initialize Output
  CLEAR: CF_PARZA.

* Get Max Number
  SELECT MAX( PARZA )
    INTO CF_PARZA
    FROM KNVP
   WHERE KUNNR EQ US_KEY-KUNNR
     AND VKORG EQ US_KEY-VKORG
     AND VTWEG EQ US_KEY-VTWEG
     AND SPART EQ US_KEY-SPART
     AND PARVW EQ US_MAIN-PARVW.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_PARZA = CF_PARZA + 1.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_EXISTING_PARTNER_FUNC
*---------------------------------------------------------------------*
* Check existing partner function + partner number
*---------------------------------------------------------------------*
FORM F_CHECK_EXISTING_PARTNER_FUNC  USING    US_DATA TYPE TS_PARTNER_FUNC
                                    CHANGING CF_PARZA TYPE KNVP-PARZA
                                             CF_FOUND TYPE FLAG.
  DATA: LV_KUNNR TYPE KNVP-KUNN2,
        LV_PERNR TYPE KNVP-PERNR,
        LV_LIFNR TYPE KNVP-LIFNR,
        LV_PARNR TYPE KNVP-PARNR.

  CLEAR: CF_PARZA,
         CF_FOUND.

  CASE US_DATA-MAIN-NRART.
    WHEN 'KU'.    "Customer
      LV_KUNNR = US_DATA-MAIN-GPANR.

*     Get Partner Counter
      SELECT PARZA
        UP TO 1 ROWS
        INTO CF_PARZA
        FROM KNVP
        WHERE KUNNR EQ US_DATA-KEY-KUNNR
          AND VKORG EQ US_DATA-KEY-VKORG
          AND VTWEG EQ US_DATA-KEY-VTWEG
          AND SPART EQ US_DATA-KEY-SPART
          AND PARVW EQ US_DATA-MAIN-PARVW
          AND KUNN2 EQ LV_KUNNR
        ORDER BY PRIMARY KEY.
      ENDSELECT.

    WHEN 'LI'.    "Vendor
      LV_LIFNR = US_DATA-MAIN-GPANR.

*     Get Partner Counter
      SELECT PARZA
        UP TO 1 ROWS
        INTO CF_PARZA
        FROM KNVP
        WHERE KUNNR EQ US_DATA-KEY-KUNNR
          AND VKORG EQ US_DATA-KEY-VKORG
          AND VTWEG EQ US_DATA-KEY-VTWEG
          AND SPART EQ US_DATA-KEY-SPART
          AND PARVW EQ US_DATA-MAIN-PARVW
          AND LIFNR EQ LV_LIFNR
        ORDER BY PRIMARY KEY.
      ENDSELECT.

    WHEN 'PE'.    "Personnel Number
      LV_PERNR = US_DATA-MAIN-GPANR.

*     Get Partner Counter
      SELECT PARZA
        UP TO 1 ROWS
        INTO CF_PARZA
        FROM KNVP
        WHERE KUNNR EQ US_DATA-KEY-KUNNR
          AND VKORG EQ US_DATA-KEY-VKORG
          AND VTWEG EQ US_DATA-KEY-VTWEG
          AND SPART EQ US_DATA-KEY-SPART
          AND PARVW EQ US_DATA-MAIN-PARVW
          AND PERNR EQ LV_PERNR
        ORDER BY PRIMARY KEY.
      ENDSELECT.

    WHEN 'AP'.    "Contact Persons
      LV_PARNR = US_DATA-MAIN-GPANR.

*     Get Partner Counter
      SELECT PARZA
        UP TO 1 ROWS
        INTO CF_PARZA
        FROM KNVP
        WHERE KUNNR EQ US_DATA-KEY-KUNNR
          AND VKORG EQ US_DATA-KEY-VKORG
          AND VTWEG EQ US_DATA-KEY-VTWEG
          AND SPART EQ US_DATA-KEY-SPART
          AND PARVW EQ US_DATA-MAIN-PARVW
          AND PARNR EQ LV_PARNR
        ORDER BY PRIMARY KEY.
      ENDSELECT.
  ENDCASE.

  IF SY-SUBRC EQ 0.
    CF_FOUND = GC_TRUE.
  ELSE.
    CLEAR: CF_PARZA, CF_FOUND.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PROCESS_DATA_RELATIONSHIP
*---------------------------------------------------------------------*
*  Processing data: Relationship
*---------------------------------------------------------------------*
FORM F_PROCESS_DATA_RELATIONSHIP  USING UF_TEST   TYPE FLAG
                               CHANGING CT_RESULT TYPE TT_RESULT
                                        CS_PROC TYPE TS_PROC_INFO.

  DATA:
    LT_XLSX   TYPE TT_XLSX,
    LT_DATA   TYPE TT_RELATIONSHIP,
    LT_RESULT TYPE TT_RESULT,
    LS_COUNT  TYPE TS_COUNT.

* Initialize Output
  CLEAR: CT_RESULT, CS_PROC.

* -----------------------------
* Read File into Text table
* -----------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

* -----------------------------
* Prepare data to internal table
* -----------------------------
  PERFORM F_ASSIGN_DATA_RELATIONSHIP   USING LT_XLSX
                                    CHANGING LT_DATA
                                             LT_RESULT.
  IF LT_RESULT IS NOT INITIAL.
*   Collect Final Result
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
    CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
  ENDIF.

* -----------------------------
* Create Contact Person Relationship
* -----------------------------
  PERFORM F_PREPARE_RELATIONSHIP USING LT_DATA
                                       UF_TEST
                              CHANGING LT_RESULT
                                       LS_COUNT.

* -----------------------------
* Collect Final Result
* -----------------------------
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LS_COUNT-ERROR.
  CS_PROC-COUNT-SUCCS = CS_PROC-COUNT-SUCCS + LS_COUNT-SUCCS.

  PERFORM F_ASSIGN_PROCESSING_INFO CHANGING CS_PROC.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_DATA_RELATIONSHIP
*---------------------------------------------------------------------*
*  Assign Data from XLSX Sheet to Internal table
*---------------------------------------------------------------------*
FORM F_ASSIGN_DATA_RELATIONSHIP  USING UT_XLSX TYPE TT_XLSX
                              CHANGING CT_DATA TYPE TT_RELATIONSHIP
                                       CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_DATA  TYPE TS_RELATIONSHIP,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_COL_START TYPE I VALUE 2,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE CHAR40,
    LF_STRING    TYPE STRING,
    LF_MSGTX     TYPE BAPI_MSG.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.

* Initialize Output
  CLEAR: CT_DATA,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 30 TEXT-P02.

* Read Active Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  ASSIGN <L_SHEET>-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LF_COL_START = P_BEGCOL.
  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE P_BEGROW AND
          LF_ROWNO LE P_ENDROW.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.

    LS_DATA-ROWNO = LF_ROWNO.


    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CLEAR: LF_STRING.

      CHECK SY-TABIX GE P_BEGCOL AND
            SY-TABIX LE P_ENDCOL.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE3'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      LF_STRING = <L_FIELD>.

      CASE LF_FNAME.
        WHEN 'PARTNER'.
          IF <L_FIELD> IS INITIAL.
*           Text-e01: Missing BP Number in the file
            LF_MSGTX = TEXT-E01.
            EXIT.
          ENDIF.

          LS_DATA-MAIN-PARTNER = <L_FIELD>.

*         Get BP Group and check PARTNER number valid?
          PERFORM F_CHECK_PARTNER_EXIST USING LF_STRING
                                     CHANGING LS_DATA-MAIN-PARTNER
                                              LS_DATA-MAIN-PARTNER_GUID
                                              LF_MSGTX.
          IF LF_MSGTX IS NOT INITIAL.
            EXIT.
          ENDIF.

        WHEN 'PARTNER2'.
          IF <L_FIELD> IS INITIAL.
*           Text-e18: Missing Contact Person Number in the file
            LF_MSGTX = TEXT-E18.
            EXIT.
          ENDIF.
          LS_DATA-MAIN-PARTNER2 = <L_FIELD>.

        WHEN 'DEPARTMENT'.
          LS_DATA-MAIN-DEPARTMENT = <L_FIELD>.
      ENDCASE.
    ENDLOOP.

*    IF LF_MSGTX IS INITIAL.
**     Validate data Row Level
*
*    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.

      INSERT LS_MESSG INTO TABLE LS_DATA-MESSG.

*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT_RELATIONSHIP USING LS_DATA
                                         CHANGING CT_RESULT.

      CONTINUE.
    ENDIF.

*   Collect Data
    INSERT LS_DATA INTO TABLE CT_DATA.

  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_PARTNER_EXIST
*---------------------------------------------------------------------*
* Check Business Partner exist?
*---------------------------------------------------------------------*
FORM F_CHECK_PARTNER_EXIST  USING    UF_STRING TYPE STRING
                            CHANGING CF_PARTNER TYPE BUT000-PARTNER
                                     CF_PARTNER_GUID TYPE BUT000-PARTNER_GUID
                                     CF_MSGTX TYPE BAPI_MSG.

  DATA: LV_PARTNER TYPE BUT000-PARTNER.

  CLEAR: CF_PARTNER,
         CF_PARTNER_GUID,
         CF_MSGTX.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LV_PARTNER.

* Assign Output
  CF_PARTNER = LV_PARTNER.

  SELECT SINGLE PARTNER_GUID
    INTO CF_PARTNER_GUID
    FROM BUT000
   WHERE PARTNER EQ CF_PARTNER.

  IF SY-SUBRC NE 0.
*   Text-e05 : Invalid BP Number:
    CONCATENATE TEXT-E05 CF_PARTNER
      INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT_RELATIONSHIP
*----------------------------------------------------------------------*
*  Collect result from Relationship
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT_RELATIONSHIP USING US_DATA TYPE TS_RELATIONSHIP
                                CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_RESULT TYPE TS_RESULT.

  FIELD-SYMBOLS:
    <L_MESSG> TYPE TS_MESSG.

  CLEAR LS_RESULT.
  LS_RESULT-ROWNO = US_DATA-ROWNO.
  MOVE-CORRESPONDING US_DATA-MAIN TO LS_RESULT.

  LOOP AT US_DATA-MESSG ASSIGNING <L_MESSG>.
    LS_RESULT-MSGTY = <L_MESSG>-MSGTY.
    LS_RESULT-MSGID = <L_MESSG>-MSGID.
    LS_RESULT-MSGNO = <L_MESSG>-MSGNO.
    LS_RESULT-MSGTX = <L_MESSG>-MSGTX.
    CASE LS_RESULT-MSGTY.
      WHEN 'S'.
        LS_RESULT-STATU = ICON_LED_GREEN.
      WHEN 'W'.
        LS_RESULT-STATU = ICON_LED_YELLOW.
      WHEN 'E' OR 'A'.
        LS_RESULT-STATU = ICON_LED_RED.
      WHEN OTHERS.
        LS_RESULT-STATU = ICON_LED_INACTIVE.
    ENDCASE.
    EXIT.
  ENDLOOP.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PREPARE_RELATIONSHIP
*---------------------------------------------------------------------*
* Prepare data for create contact person relationship
*---------------------------------------------------------------------*
FORM F_PREPARE_RELATIONSHIP  USING UT_DATA TYPE TT_RELATIONSHIP
                                   UF_TEST TYPE FLAG
                          CHANGING CT_RESULT TYPE TT_RESULT
                                   CS_COUNT TYPE TS_COUNT.

  DATA: LT_MESSG TYPE TT_MESSG.
  DATA: LS_DATA TYPE TS_RELATIONSHIP.
  DATA: LV_ERROR TYPE FLAG.

  CLEAR: CT_RESULT, CS_COUNT.

* Show progress
* Text-p03 : Uploading data...
  MC_SHOW_PROGRESS 40 TEXT-P03.

* Processing each record
  LOOP AT UT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_MAINTAIN_RELATIONSHIP  USING LS_DATA
                                             UF_TEST
                                    CHANGING LT_MESSG
                                             LV_ERROR.
      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.

    CS_COUNT-TOTAL = CS_COUNT-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      CS_COUNT-ERROR = CS_COUNT-ERROR + 1.
    ELSE.
      CS_COUNT-SUCCS = CS_COUNT-SUCCS + 1.
    ENDIF.

*   Collect Validation Error Result
    PERFORM F_COLLECT_RESULT_RELATIONSHIP USING LS_DATA
                                       CHANGING CT_RESULT.

  ENDLOOP.

* Show Final Message for Processing completed
* Text-i01: Processing completed.
  MESSAGE S000(ZSDSCA01) WITH TEXT-001 SPACE SPACE SPACE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_MAINTAIN_RELATIONSHIP
*---------------------------------------------------------------------*
*  Create Contact Person Relationship
*---------------------------------------------------------------------*
FORM F_MAINTAIN_RELATIONSHIP  USING US_DATA TYPE TS_RELATIONSHIP
                                    UF_TEST TYPE FLAG
                           CHANGING CT_MESSG TYPE TT_MESSG
                                    CF_ERROR TYPE FLAG.
  DATA:
    LT_BAPI_DATA TYPE  CVIS_EI_EXTERN_T,
    LT_MSGMAP    TYPE  MDG_BS_BP_MSGMAP_T,
    LT_RETURN    TYPE  BAPIRETM.

  DATA:
    LS_BAPI_DATA TYPE  CVIS_EI_EXTERN.

* Initialize Output
  CLEAR: CT_MESSG,
         CF_ERROR.

  CLEAR LS_BAPI_DATA.

* ---------------------------
* Assign Header Data
* ---------------------------
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_TASK = 'U'.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER = US_DATA-MAIN-PARTNER.
  LS_BAPI_DATA-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = US_DATA-MAIN-PARTNER_GUID.

* ---------------------------
* Assign Contact Person Relationship
* ---------------------------
  PERFORM F_ASSIGN_BAPI_RELATIONSHIP USING US_DATA-MAIN
                                  CHANGING LS_BAPI_DATA-PARTNER_RELATION.

* Validate Data
  CALL METHOD CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
    EXPORTING
      I_DATA        = LS_BAPI_DATA
    IMPORTING
      ET_RETURN_MAP = LT_MSGMAP.

  IF LT_MSGMAP IS INITIAL.

    INSERT LS_BAPI_DATA INTO TABLE LT_BAPI_DATA.

*   Post Data
    CALL METHOD CL_MD_BP_MAINTAIN=>MAINTAIN
      EXPORTING
        I_DATA     = LT_BAPI_DATA
        I_TEST_RUN = UF_TEST
      IMPORTING
        E_RETURN   = LT_RETURN.

    PERFORM F_CHECK_BAPI_RETURN  USING  LT_RETURN
                                        UF_TEST
                               CHANGING CF_ERROR
                                        CT_MESSG.

    IF UF_TEST IS INITIAL AND
       CF_ERROR IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ELSE.
    PERFORM F_CHECK_BAPI_RETURNMAP  USING  LT_MSGMAP
                                  CHANGING CF_ERROR
                                           CT_MESSG.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_BAPI_RELATIONSHIP
*---------------------------------------------------------------------*
* Assign BAPI Relationship data
*---------------------------------------------------------------------*
FORM F_ASSIGN_BAPI_RELATIONSHIP  USING US_MAIN TYPE TS_MAIN_RELATIONSHIP
                                 CHANGING CT_RELATION TYPE BURS_EI_EXTERN_T.

  DATA:
    LS_RELATION     TYPE BURS_EI_EXTERN,
    LS_ADDRESS_GUID TYPE BAPIBUS1006_ADDRESSES_INT-ADDRGUID,
    LT_ADDRESS      TYPE BURS_EI_REL_ADDRESS_T,
    LS_ADDRESS      TYPE BURS_EI_REL_ADDRESS.

  CLEAR LS_RELATION.
  LS_RELATION-HEADER-OBJECT_TASK = 'I'.

  LS_RELATION-HEADER-OBJECT_INSTANCE-PARTNER1 = US_MAIN-PARTNER.
  LS_RELATION-HEADER-OBJECT_INSTANCE-PARTNER2 = US_MAIN-PARTNER2.

  LS_RELATION-HEADER-OBJECT_INSTANCE-RELAT_CATEGORY = GC_RELATION.
  LS_RELATION-HEADER-OBJECT_INSTANCE-DATE_TO        = '99991231'.

* Mark Standard Relationship
  LS_RELATION-CENTRAL_DATA-MAIN-DATA-DEFAULTRELATIONSHIP = 'X'.
  LS_RELATION-CENTRAL_DATA-MAIN-DATAX-DEFAULTRELATIONSHIP = 'X'.

* Department
  IF US_MAIN-DEPARTMENT IS NOT INITIAL.
    LS_RELATION-CENTRAL_DATA-CONTACT-CENTRAL_DATA-DATA-DEPARTMENT = US_MAIN-DEPARTMENT.
    LS_RELATION-CENTRAL_DATA-CONTACT-CENTRAL_DATA-DATAX-DEPARTMENT = 'X'.
  ENDIF.

* Get address GUID of partner1
  CLEAR: LS_ADDRESS_GUID.
  CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
    EXPORTING
      BUSINESSPARTNER     = US_MAIN-PARTNER
    IMPORTING
      STANDARDADDRESSGUID = LS_ADDRESS_GUID.

  IF LS_ADDRESS_GUID IS NOT INITIAL.
    LS_ADDRESS-TASK = 'I'.
    LS_ADDRESS-DATA_KEY-GUID = LS_ADDRESS_GUID.
    APPEND LS_ADDRESS TO LT_ADDRESS.

    LS_RELATION-CENTRAL_DATA-ADDRESS-ADDRESSES = LT_ADDRESS[].
  ENDIF.

  INSERT LS_RELATION INTO TABLE CT_RELATION.

ENDFORM.
