*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0320
*  Creation Date      : 24.09.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : conversion
*  Description        : Rebate conversion
*  Purpose            : Rebate conversion
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0320.
*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

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
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSSD027',
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  BEGIN OF GC_SHEET,
    MC TYPE STRING VALUE 'Main Contract'     ##NO_TEXT,
    BC TYPE STRING VALUE 'Business Criteria' ##NO_TEXT,
    ST TYPE STRING VALUE 'Settlement Date'   ##NO_TEXT,
  END OF GC_SHEET,
  BEGIN OF GC_DATTY,
    DATE TYPE ABAP_TYPEKIND VALUE 'D',
    PACK TYPE ABAP_TYPEKIND VALUE 'P',
    NUM  TYPE ABAP_TYPEKIND VALUE 'N',
    CHAR TYPE ABAP_TYPEKIND VALUE 'C',
  END OF GC_DATTY.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TF_FNAME  TYPE  CHAR40.
TYPES: TF_MSGTX  TYPE  ZSDSDE_MSGTX.
TYPES: TS_XLSX TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA.
TYPES: TT_XLSX TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.
TYPES: BEGIN OF TS_MESSG,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  TF_MSGTX,
       END OF TS_MESSG.
TYPES:BEGIN OF TS_RESULT.
        INCLUDE TYPE ZSDSSDS100.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                         WITH DEFAULT KEY.
TYPES: BEGIN OF TS_MAIN,
         INDEX_KEY TYPE ZSDSSDS100-INDEX_KEY,
       END OF TS_MAIN.
TYPES: TT_MAIN TYPE SORTED TABLE OF TS_MAIN WITH UNIQUE KEY INDEX_KEY.
* -- main contract
TYPES: BEGIN OF TS_TEMPLATE_MC,
         INDEX_KEY          TYPE ZSDSSDS100-INDEX_KEY,  "Index
         CONTRACT_TYPE      TYPE BAPICCHEAD-CONTRACT_TYPE,  "Condition Contract Type
         VALIDITY_DATE_FROM TYPE BAPICCHEAD-VALIDITY_DATE_FROM,  "Valid From Date
         VALIDITY_DATE_TO   TYPE BAPICCHEAD-VALIDITY_DATE_TO  , "Valid To Date
         SALESORG           TYPE BAPICCHEAD-SALESORG,  "Sales Organization
         DISTR_CHAN         TYPE BAPICCHEAD-DISTR_CHAN,  "Distribution Channel
         DIVISION           TYPE BAPICCHEAD-DIVISION,  "Division
         SALES_OFF          TYPE BAPICCHEAD-SALES_OFF,  "Sales Office
         SALES_GRP          TYPE BAPICCHEAD-SALES_GRP,  "Sales Group
         "--- cond data
         TABLE_NO           TYPE BAPICCCONDITIONKEY-TABLE_NO,  "Condition Table
         COND_TYPE          TYPE BAPICCCONDITIONKEY-COND_TYPE,  "Condition type
         CUSTOMER           TYPE BAPICCCONDITIONKEY-CUSTOMER,  "Customer no.
         COND_SALES_GRP     TYPE BAPICCCONDITIONKEY-SALES_GRP,  "Sales Grp.1
         KVGR1              TYPE BAPI_TE_KOMG-KVGR1,  "Cust. Grp.1
         PROD_HIER1         TYPE BAPICCCONDITIONKEY-PROD_HIER1,  "PH.1
         PROD_HIER2         TYPE BAPICCCONDITIONKEY-PROD_HIER2,  "PH.2
         PROD_HIER3         TYPE BAPICCCONDITIONKEY-PROD_HIER3,  "PH.3
         VALID_FROM         TYPE BAPICCCONDITIONKEY-VALID_FROM,  "Condition Valid From
         VALID_TO           TYPE BAPICCCONDITIONKEY-VALID_TO,  "Condition Valid To

         CALC_TYPE          TYPE BAPICCCONDITIONITEM-CALC_TYPE,	 "Calculation Type
         SCALE_EXISTS       TYPE WCB_COND_DISP-SCALE_EXISTS,   "Scale Basis Indicator
         LINE_NO            TYPE BAPICCSCALEO-LINE_NO,   "Sequent no. of scale ( not fill in bapi )
         SCALETYPE          TYPE BAPICCCONDITIONITEM-SCALETYPE,	 "Scale Type
         SCALEBASIS         TYPE BAPICCCONDITIONITEM-SCALEBASIS,   "Scale Base Type
         AMOUNT             TYPE BAPICCCONDITIONITEM-AMOUNT,   "Rate Amount/Percentage
         SCALE_BASE_VALUE   TYPE BAPICCSCALE-SCALE_BASE_VALUE,   "Scale Basis Value
         CONDCURR           TYPE BAPICCCONDITIONITEM-CONDCURR,   "Currency
         COND_P_UNT         TYPE BAPICCCONDITIONITEM-COND_P_UNT,   "Condition scale quantity
         COND_UNIT          TYPE BAPICCCONDITIONITEM-COND_UNIT,  "Condition Scale Unit of Measure
       END OF TS_TEMPLATE_MC.
TYPES: BEGIN OF TS_MC.
         INCLUDE TYPE TS_TEMPLATE_MC.
TYPES:   SHEET TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA-WORKSHEET_NAME,
         ROWNO TYPE TS_RESULT-ROWNO.
TYPES  END OF TS_MC.
TYPES: TT_MC TYPE SORTED TABLE OF TS_MC WITH NON-UNIQUE  KEY INDEX_KEY ROWNO.
TYPES: BEGIN OF TS_COND_COMPARE,
         TABLE_NO       TYPE BAPICCCONDITIONKEY-TABLE_NO,  "Condition Table
         COND_TYPE      TYPE BAPICCCONDITIONKEY-COND_TYPE,  "Condition type
         CUSTOMER       TYPE BAPICCCONDITIONKEY-CUSTOMER,  "Customer no.
         COND_SALES_GRP TYPE BAPICCCONDITIONKEY-SALES_GRP,  "Sales Grp.1
         KVGR1          TYPE BAPI_TE_KOMG-KVGR1, " BAPICCCONDITIONKEY-CUST_GROUP,  "Cust. Grp.1
         PROD_HIER1     TYPE BAPICCCONDITIONKEY-PROD_HIER1,  "PH.1
         PROD_HIER2     TYPE BAPICCCONDITIONKEY-PROD_HIER2,  "PH.2
         PROD_HIER3     TYPE BAPICCCONDITIONKEY-PROD_HIER3,  "PH.3
         VALID_FROM     TYPE BAPICCCONDITIONKEY-VALID_FROM,  "Condition Valid From
         VALID_TO       TYPE BAPICCCONDITIONKEY-VALID_TO,  "Condition Valid To
         CALC_TYPE      TYPE BAPICCCONDITIONITEM-CALC_TYPE,	 "Calculation Type
         SCALETYPE      TYPE BAPICCCONDITIONITEM-SCALETYPE,	 "Scale Type
         SCALEBASIS     TYPE BAPICCCONDITIONITEM-SCALEBASIS,   "Scale Base Type
       END OF TS_COND_COMPARE.

*--Business Criteria
TYPES: BEGIN OF TS_TEMPLATE_BC,
         INDEX_KEY        TYPE    ZSDSSDS100-INDEX_KEY,  "Index
         VALID_FROM_NEW   TYPE    BAPICCBVB-VALID_FROM_NEW,
         VALID_TO_NEW     TYPE    BAPICCBVB-VALID_TO_NEW,
         FIELDCOMBINATION TYPE    BAPICCBVB-FIELDCOMBINATION,
         INCLUDE_EXCLUDE  TYPE    BAPICCBVB-INCLUDE_EXCLUDE,
         VALUE            TYPE    TEXT255,
       END OF TS_TEMPLATE_BC.
TYPES: BEGIN OF TS_BC,
         INDEX_KEY TYPE ZSDSSDS100-INDEX_KEY,
         BVB       TYPE BAPICCBVB,
         BVBX      TYPE BAPICCBVBX,
         ZBVB      TYPE BAPI_TE_BVB,
         SHEET     TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA-WORKSHEET_NAME,
         ROWNO     TYPE TS_RESULT-ROWNO,
       END OF TS_BC.
TYPES: TT_BC TYPE SORTED TABLE OF TS_BC WITH NON-UNIQUE  KEY INDEX_KEY ROWNO.

*-- Settlement Date
TYPES: BEGIN OF TS_TEMPLATE_ST,
         INDEX_KEY            TYPE ZSDSSDS100-INDEX_KEY,  "Index
         SETTLEMENT_DATE      TYPE BAPICCCALO-SETTLEMENT_DATE,
         SETTLEMENT_DATE_TYPE TYPE BAPICCCALO-SETTLEMENT_DATE_TYPE,
       END OF TS_TEMPLATE_ST.
TYPES: BEGIN OF TS_ST.
         INCLUDE TYPE TS_TEMPLATE_ST.
TYPES:   SHEET TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA-WORKSHEET_NAME,
         ROWNO TYPE TS_RESULT-ROWNO.
TYPES  END OF TS_ST.
TYPES: TT_ST TYPE SORTED TABLE OF TS_ST WITH NON-UNIQUE  KEY INDEX_KEY ROWNO.

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
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT  TYPE  TT_RESULT                                   ##NEEDED.
*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
DATA: GF_AF_GROUP TYPE WCOCOH-AF_GROUP                          ##NEEDED.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_PROC    TYPE  TS_PROC_INFO                                ##NEEDED.
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS100'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 25,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 75.

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
* Text-s01: File Selection
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    RB_LOCAL TYPE CHAR1 RADIOBUTTON GROUP G2 DEFAULT 'X'
                                    USER-COMMAND DMY,
    RB_APPSV TYPE CHAR1 RADIOBUTTON GROUP G2.
  PARAMETERS:
    P_IFILE  TYPE  STRING LOWER CASE MODIF ID LOC,
    P_AFILE  TYPE  STRING LOWER CASE MODIF ID ASV,
    P_FNAME  TYPE I DEFAULT 3,
    P_CHKREQ TYPE I DEFAULT 6.

  SELECTION-SCREEN BEGIN OF LINE.
*     Text-c01: Start Row
    SELECTION-SCREEN COMMENT (31) TEXT-C01 FOR FIELD P_BEGROW.
    SELECTION-SCREEN POSITION 33.
    PARAMETERS: P_BEGROW TYPE I DEFAULT 8 .
*     Text-c02: Start Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-C02 FOR FIELD P_BEGCOL.
    PARAMETERS: P_BEGCOL TYPE I DEFAULT 2 .
  SELECTION-SCREEN END OF LINE.
  PARAMETERS: P_END TYPE CHAR20 DEFAULT 'END' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B2.
* Text-s03: Processing Options
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
  PARAMETERS:
    CB_TEST  TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B3.
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get Constant
  PERFORM F_GET_CONSTANTS.
* Processing Data
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT
                                 GS_PROC.
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.

  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
*  Form F_SET_SELECTION_SCREEN_FORMAT
*----------------------------------------------------------------------*
*  Set Selection Screen Format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .
  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.
*     Local Fields
      WHEN 'LOC'.
        IF RB_LOCAL EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.

*     AppServer Fields
      WHEN 'ASV'.
        IF RB_APPSV EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDFORM.
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
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  IF RB_LOCAL EQ GC_TRUE.
    IF P_IFILE IS INITIAL.
      SET CURSOR FIELD 'P_IFILE'.
*     Error: Please enter a valid filename.
      MESSAGE E002(ZSDSCA01).
      RETURN.
    ENDIF.
  ELSE.
    IF P_AFILE IS INITIAL.
      SET CURSOR FIELD 'P_AFILE'.
*     Error: Please enter a valid filename.
      MESSAGE E002(ZSDSCA01).
      RETURN.
    ENDIF.
  ENDIF.

  IF SY-BATCH EQ GC_TRUE AND
     RB_LOCAL EQ GC_TRUE.
*   Error: Cannot process local file in background mode.
    MESSAGE E012(ZSDSCA01).
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_PROCESS_DATA
*----------------------------------------------------------------------*
*  Processing data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA  CHANGING CT_RESULT TYPE TT_RESULT
                            CS_PROC   TYPE TS_PROC_INFO.

  DATA:
    LT_XLSX TYPE  TT_XLSX,
    LT_MC   TYPE  TT_MC,
    LT_BC   TYPE  TT_BC,
    LT_ST   TYPE  TT_ST,
    LT_MAIN TYPE  TT_MAIN.



* Initialize Output
  CLEAR: CT_RESULT,
         CS_PROC.

* -----------------------------
* Read File into Text table
* -----------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.
  READ TABLE LT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>)
                     WITH KEY WORKSHEET_NAME = GC_SHEET-MC.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZSDSCA01) WITH 'Not found sheet'(m01) GC_SHEET-MC.
  ELSE.
    PERFORM F_ASSIGN_MAIN_CONTRACT USING <L_SHEET>
                                   CHANGING LT_MC
                                            CT_RESULT
                                            LT_MAIN.
  ENDIF.

  READ TABLE LT_XLSX ASSIGNING <L_SHEET>
                     WITH KEY WORKSHEET_NAME = GC_SHEET-BC.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZSDSCA01) WITH 'Not found sheet'(m01) GC_SHEET-BC.
  ELSE.
    PERFORM F_ASSIGN_BUSINESS_CRITERIA USING <L_SHEET>
                                       CHANGING LT_BC
                                                CT_RESULT
                                                LT_MAIN.

  ENDIF.
  READ TABLE LT_XLSX ASSIGNING <L_SHEET>
                     WITH KEY WORKSHEET_NAME = GC_SHEET-ST.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZSDSCA01) WITH 'Not found sheet'(m01) GC_SHEET-ST.
  ELSE.
    PERFORM F_ASSIGN_SETTLEMENT USING <L_SHEET>
                                CHANGING LT_ST
                                         CT_RESULT
                                         LT_MAIN.
  ENDIF.

  IF LT_MAIN IS INITIAL.
    RETURN.
  ENDIF.
  IF CT_RESULT IS NOT INITIAL.
*       Collect Final Result
    DATA(LT_RESULT) = CT_RESULT.
    SORT LT_RESULT BY INDEX_KEY.
    DELETE ADJACENT DUPLICATES FROM LT_RESULT COMPARING INDEX_KEY.
    CS_PROC-COUNT-ERROR =  LINES( LT_RESULT ).
  ENDIF.
  PERFORM F_CONDITION_CONTRACT_CREATE USING  LT_MC
                                             LT_BC
                                             LT_ST
                                             LT_MAIN
                                      CHANGING CT_RESULT
                                               CS_PROC.

* Assign Processing Info
  CS_PROC-DATUM = SY-DATUM.
  CS_PROC-UZEIT = SY-UZEIT.
  CS_PROC-UNAME = SY-UNAME.
  CASE GC_TRUE.
    WHEN RB_LOCAL.
      CS_PROC-IFILE = P_IFILE.
    WHEN RB_APPSV.
      CS_PROC-IFILE = P_AFILE.
  ENDCASE.

* Text-X03: Create Condition Contract
  CS_PROC-MODE = TEXT-X03.
  IF CB_TEST EQ GC_TRUE.
*   Text-x01: Test Run
    CS_PROC-MODE  = |{ CS_PROC-MODE }-{ TEXT-X01 }|.
  ELSE.
*   Text-x02: Production Run
    CS_PROC-MODE  = |{ CS_PROC-MODE }-{ TEXT-X02 }|.
  ENDIF.

  CS_PROC-COUNT-TOTAL = CS_PROC-COUNT-SUCCS + CS_PROC-COUNT-ERROR.

* Show Message Complete
* Text-i07: Processing completed.
  MESSAGE S000(ZSDSSD01) WITH TEXT-I07 SPACE SPACE SPACE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_READ_FILE
*----------------------------------------------------------------------*
*  Read File into Text Table
*----------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

  DATA:
    LT_SHEET   TYPE  IF_FDT_DOC_SPREADSHEET=>T_WORKSHEET_NAMES.

  DATA:
    LF_FILENAME     TYPE  STRING,
    LF_APPSV_FLAG   TYPE  FLAG,
    LF_ACTIVE_SHEET TYPE  FLAG.


* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 15 TEXT-P01.

  CLEAR: LF_FILENAME,
         LF_APPSV_FLAG.
  CASE GC_TRUE.
    WHEN RB_LOCAL.
      LF_FILENAME = P_IFILE.
    WHEN RB_APPSV.
      LF_FILENAME   = P_AFILE.
      LF_APPSV_FLAG = GC_TRUE.
  ENDCASE.

* Assign Sheet to read
  CLEAR: LT_SHEET,
         LF_ACTIVE_SHEET.

  APPEND GC_SHEET-MC TO LT_SHEET.
  APPEND GC_SHEET-BC TO LT_SHEET.
  APPEND GC_SHEET-ST TO LT_SHEET.


  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = LF_FILENAME
      IF_APPSV_FLAG               = LF_APPSV_FLAG
      IF_READ_ACTIVE_WORKSHEET    = LF_ACTIVE_SHEET
      IT_WORKSHEET                = LT_SHEET
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
*&---------------------------------------------------------------------*
*& Form F_ASSIGN_MAIN_CONTRACT
*&---------------------------------------------------------------------*
FORM F_ASSIGN_MAIN_CONTRACT  USING    US_XLSX TYPE TS_XLSX
                             CHANGING CT_MC TYPE TT_MC
                                      CT_RESULT TYPE TT_RESULT
                                      CT_MAIN TYPE TT_MAIN.
  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION,
    LS_DATA       TYPE TS_MC,
    LS_MAIN       TYPE TS_MAIN,
    LS_MESSG      TYPE TS_MESSG,
    LF_ROWNO      TYPE TS_RESULT-ROWNO,
    LF_MSGTX      TYPE TF_MSGTX,
    LF_TXT        TYPE TF_MSGTX,
    LF_FNAME      TYPE TF_FNAME,
    LF_TYPE_KIND  TYPE ABAP_TYPEKIND,
    LF_COL_INDEX  TYPE I.
  FIELD-SYMBOLS: <L_REQ>   TYPE ANY,
                 <L_FNAME> TYPE ANY.

  CLEAR CT_MC.

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.
  CREATE OBJECT LREF_VALIDATE.
  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>) .
    IF <L_ROW> IS INITIAL.
      EXIT.
    ENDIF.
*   Keep Row no
    LF_ROWNO = SY-TABIX.
    IF LF_ROWNO = P_FNAME .
      ASSIGN <L_ROW> TO <L_FNAME>.
    ENDIF.

    IF LF_ROWNO = P_CHKREQ .
      ASSIGN <L_ROW> TO <L_REQ>.
    ENDIF.
    CHECK LF_ROWNO GE P_BEGROW.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.
    LS_DATA-SHEET = US_XLSX-WORKSHEET_NAME.
    LS_DATA-ROWNO = LF_ROWNO.
    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).
      CLEAR LF_TXT.
      CHECK SY-TABIX GE P_BEGCOL.

      LF_COL_INDEX = SY-TABIX - P_BEGCOL + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE_MC'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME
                                         LF_TYPE_KIND.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_INPUT>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      IF LF_FNAME = 'INDEX_KEY'.
        IF <L_FIELD_INPUT> = P_END .
          EXIT.
        ENDIF.
        LS_MAIN-INDEX_KEY = <L_FIELD_INPUT>.
        INSERT LS_MAIN INTO TABLE CT_MAIN.
      ENDIF.
*     Assign fieldname record
      ASSIGN <L_FNAME>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_FNAME>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Assign required check record
      ASSIGN <L_REQ>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_REQ>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Assign target Field
      ASSIGN LS_DATA-(LF_FNAME) TO FIELD-SYMBOL(<L_FIELD_DATA>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
* --check required
      IF   <L_FIELD_REQ> = 'M'. "mandatory.
        IF <L_FIELD_INPUT> IS INITIAL.
          LF_TXT = | Required value "{ <L_FIELD_FNAME> }"| ##NO_TEXT.
          SET_MESSAGE LF_MSGTX LF_TXT.
          CONTINUE.
        ENDIF.
      ENDIF.
*-- check format data type
      PERFORM F_VALIDATE_DATA_TYPE USING LF_TYPE_KIND
                                         LREF_VALIDATE
                                         <L_FIELD_INPUT>
                                         <L_FIELD_FNAME>
                                   CHANGING <L_FIELD_DATA>
                                            LF_TXT.
      IF LF_TXT IS NOT INITIAL.
        SET_MESSAGE LF_MSGTX LF_TXT.
        CONTINUE.
      ENDIF.
      IF LF_FNAME = 'CUSTOMER'.
        IF  <L_FIELD_DATA> IS NOT INITIAL.
          PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT'
                                    <L_FIELD_DATA>
                            CHANGING <L_FIELD_DATA>.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSSD01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.
*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT1  USING  LS_DATA
                                        LS_MESSG
                               CHANGING CT_RESULT.
      CONTINUE.
    ENDIF.
    READ TABLE CT_RESULT TRANSPORTING NO FIELDS WITH KEY INDEX_KEY = LS_DATA-INDEX_KEY.
    IF SY-SUBRC <> 0. "not found error
*   Collect Data
      INSERT LS_DATA INTO TABLE  CT_MC.
    ENDIF.
  ENDLOOP.

  LOOP AT CT_MC ASSIGNING FIELD-SYMBOL(<L_MC>).
    READ TABLE CT_RESULT TRANSPORTING NO FIELDS WITH KEY INDEX_KEY = LS_DATA-INDEX_KEY.
    IF SY-SUBRC = 0. "found error , delete all the same key
      DELETE CT_MC.
    ENDIF.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_get_target_field
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  UF_TYPE   TYPE  CHAR20
                                UF_INDEX  TYPE  I
                       CHANGING CF_FIELD  TYPE  TF_FNAME
                                CF_TYPE_KIND TYPE  ABAP_TYPEKIND.


  STATICS:
    LF_TYPE   TYPE  CHAR20,
    LT_FIELDS TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <L_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: CF_FIELD,
         CF_TYPE_KIND.

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
  CF_TYPE_KIND = <L_FIELD>-TYPE_KIND.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_collect_result1
*----------------------------------------------------------------------*
*  Collect Project Definition Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT1  USING  US_DATA  TYPE ANY
                               US_MESSG TYPE TS_MESSG
                      CHANGING CT_RESULT TYPE TT_RESULT.
  DATA:
    LS_RESULT  TYPE  TS_RESULT.


  CLEAR LS_RESULT.
  MOVE-CORRESPONDING US_DATA TO LS_RESULT.
  LS_RESULT-MSGTY = US_MESSG-MSGTY.
  LS_RESULT-MSGID = US_MESSG-MSGID.
  LS_RESULT-MSGNO = US_MESSG-MSGNO.
  LS_RESULT-MSGTX = US_MESSG-MSGTX.

  CASE LS_RESULT-MSGTY.
    WHEN 'S'.
      LS_RESULT-STATU = ICON_LED_GREEN.
    WHEN 'W'.
      LS_RESULT-STATU = ICON_LED_YELLOW.
    WHEN OTHERS.
      LS_RESULT-STATU = ICON_LED_RED.
  ENDCASE.

  APPEND LS_RESULT TO  CT_RESULT.

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
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

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

* determine layout
  CS_LAYOUT-CWIDTH_OPT = SPACE.
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

  DATA:
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SHEET'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.

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
      WHEN 'INDEX_KEY'.
*       Text-c01 : Index
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTY'.
      WHEN 'MSGID'.
      WHEN 'MSGNO'.
      WHEN 'NUM'.
        IF CB_TEST = ABAP_TRUE.
          <L_FIELDCAT>-NO_OUT = GC_TRUE.
        ENDIF.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 60 ##NUMBER_OK.

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
    LC_SORT1 TYPE  LVC_FNAME VALUE 'SHEET',
    LC_SORT2 TYPE  LVC_FNAME VALUE 'ROWNO',
    LC_SORT3 TYPE  LVC_FNAME VALUE 'INDEX_KEY'.

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

* Sort by Index key
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 3.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ASSIGN_BUSINESS_CRITERIA
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BUSINESS_CRITERIA  USING    US_XLSX TYPE TS_XLSX
                                 CHANGING CT_BC TYPE TT_BC
                                          CT_RESULT TYPE TT_RESULT
                                          CT_MAIN TYPE TT_MAIN.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION,
    LS_DATA       TYPE TS_TEMPLATE_BC,
    LS_BC         TYPE TS_BC,
    LS_MESSG      TYPE TS_MESSG,
    LS_BVB        TYPE BAPICCBVB,
    LS_ZBVB       TYPE BAPI_TE_BVB,
    LS_MAIN       TYPE TS_MAIN,
    LF_ROWNO      TYPE TS_RESULT-ROWNO,
    LF_MSGTX      TYPE TF_MSGTX,
    LF_TXT        TYPE TF_MSGTX,
    LF_FNAME      TYPE TF_FNAME,
    LF_TYPE_KIND  TYPE ABAP_TYPEKIND,
    LF_COL_INDEX  TYPE I.


  FIELD-SYMBOLS: <L_REQ>   TYPE ANY,
                 <L_FNAME> TYPE ANY.

  CLEAR CT_BC.

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.
  CREATE OBJECT LREF_VALIDATE.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).
    IF <L_ROW> IS INITIAL.
      EXIT.
    ENDIF.
*   Keep Row no
    LF_ROWNO = SY-TABIX.
    IF LF_ROWNO = P_FNAME .
      ASSIGN <L_ROW> TO <L_FNAME>.
    ENDIF.

    IF LF_ROWNO = P_CHKREQ .
      ASSIGN <L_ROW> TO <L_REQ>.
    ENDIF.
    CHECK LF_ROWNO GE P_BEGROW.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX,
           LS_BVB,
           LS_ZBVB.
    LS_BC-SHEET = US_XLSX-WORKSHEET_NAME.
    LS_BC-ROWNO = LF_ROWNO.
    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).
      CLEAR LF_TXT.
      CHECK SY-TABIX GE P_BEGCOL.

      LF_COL_INDEX = SY-TABIX - P_BEGCOL + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE_BC'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME
                                         LF_TYPE_KIND.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_INPUT>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      IF LF_FNAME = 'INDEX_KEY'.
        IF <L_FIELD_INPUT> = P_END .
          EXIT.
        ENDIF.
        LS_MAIN-INDEX_KEY = <L_FIELD_INPUT>.
        INSERT LS_MAIN INTO TABLE CT_MAIN.
      ENDIF.
*     Assign fieldname record
      ASSIGN <L_FNAME>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_FNAME>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Assign required check record
      ASSIGN <L_REQ>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_REQ>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.



*     check required
      IF   <L_FIELD_REQ> = 'M'. "mandatory.
        IF <L_FIELD_INPUT> IS INITIAL.
          LF_TXT = | Required value "{ <L_FIELD_FNAME> }"| ##NO_TEXT.
          SET_MESSAGE LF_MSGTX LF_TXT.
          CONTINUE.
        ENDIF.
      ENDIF.

* ========================other field not value ===================
      IF LF_FNAME <> 'VALUE'.
*     Assign target Field
        ASSIGN LS_DATA-(LF_FNAME) TO FIELD-SYMBOL(<L_FIELD_DATA>).
        IF SY-SUBRC NE 0.
          CONTINUE.
        ENDIF.
        PERFORM F_VALIDATE_DATA_TYPE USING LF_TYPE_KIND
                                           LREF_VALIDATE
                                           <L_FIELD_INPUT>
                                           <L_FIELD_FNAME>
                                     CHANGING <L_FIELD_DATA>
                                              LF_TXT.
        IF LF_TXT IS NOT INITIAL.
          SET_MESSAGE LF_MSGTX LF_TXT.
        ENDIF.
* ========================value field ===================
      ELSE. "VALUE ( FIELD TYPE DEPEND ON FIELDCOMBINATION
        IF LS_DATA-FIELDCOMBINATION IS NOT INITIAL.
          "Only one field set
          SELECT FIELDNAME                              "#EC CI_NOORDER
          INTO @DATA(LF_BVB_FNAME)
          UP TO 1 ROWS
          FROM WB2_C_BVB_FCF
          WHERE FIELDCOMB = @LS_DATA-FIELDCOMBINATION.
          ENDSELECT.
          IF SY-SUBRC NE 0.
            LF_TXT = 'Invalid Field Combination' ##NO_TEXT.
            SET_MESSAGE LF_MSGTX LF_TXT.
            CONTINUE.
          ENDIF.

          PERFORM F_ASSIGN_BVB USING LF_BVB_FNAME
                                     <L_FIELD_INPUT>
                               CHANGING LS_BVB
                                        LS_ZBVB.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSSD01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.
*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT1  USING  LS_BC
                                        LS_MESSG
                               CHANGING CT_RESULT.
      CONTINUE.
    ENDIF.
    READ TABLE CT_RESULT TRANSPORTING NO FIELDS WITH KEY INDEX_KEY = LS_DATA-INDEX_KEY.
    IF SY-SUBRC <> 0. "not found error
      LS_BC-INDEX_KEY = LS_DATA-INDEX_KEY.
      LS_BC-BVB = LS_BVB.
      LS_BC-BVB-VALID_FROM_NEW   = LS_DATA-VALID_FROM_NEW.
      LS_BC-BVB-VALID_TO_NEW     = LS_DATA-VALID_TO_NEW.
      LS_BC-BVB-FIELDCOMBINATION = LS_DATA-FIELDCOMBINATION.
      LS_BC-BVB-INCLUDE_EXCLUDE  = LS_DATA-INCLUDE_EXCLUDE.
      IF LS_ZBVB IS NOT INITIAL.
        LS_BC-ZBVB = LS_ZBVB.
      ENDIF.
*   Collect Data
      INSERT LS_BC INTO TABLE CT_BC.
    ENDIF.
  ENDLOOP.

  LOOP AT CT_BC ASSIGNING FIELD-SYMBOL(<L_BC>).
    READ TABLE CT_RESULT TRANSPORTING NO FIELDS WITH KEY INDEX_KEY = LS_DATA-INDEX_KEY.
    IF SY-SUBRC = 0. "found error , delete all the same key
      DELETE CT_BC.
    ENDIF.
  ENDLOOP.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_assign_bvb
*&---------------------------------------------------------------------*
FORM F_ASSIGN_BVB  USING    UF_FNAME TYPE ANY
                            UF_VALUE TYPE ANY
                   CHANGING CS_BVB TYPE BAPICCBVB
                            CS_ZBVB TYPE BAPI_TE_BVB.
  DATA: LF_KUNNR   TYPE KUNNR.
  CASE UF_FNAME.
    WHEN 'KUNNR' OR 'KUNWE' OR 'KUNRG'.
      PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT'
                                UF_VALUE
                        CHANGING LF_KUNNR.

      IF UF_FNAME = 'KUNNR'.
        CS_BVB-CUSTOMER_NEW = LF_KUNNR.
      ELSEIF UF_FNAME = 'KUNWE'.
        CS_BVB-SHIP_TO_NEW = LF_KUNNR.
      ELSEIF UF_FNAME = 'KUNRG'.
        CS_BVB-PAYER_NEW = LF_KUNNR.
      ENDIF.
    WHEN 'VKORG'.
      CS_BVB-SALESORG_NEW = UF_VALUE.
    WHEN 'PRODH'.
      CS_BVB-PROD_HIER_NEW = UF_VALUE.
    WHEN 'MATKL'.
      CS_BVB-MATL_GROUP_NEW = UF_VALUE.
    WHEN 'MATNR'.
      PERFORM F_CONVERSION_EXIT USING 'MATN1' 'INPUT'
                                      UF_VALUE
                              CHANGING CS_BVB-MATERIAL_NEW_LONG.
    WHEN 'MVGR1'.
      CS_BVB-PRC_GROUP1_NEW = UF_VALUE.
    WHEN 'MVGR2'.
      CS_BVB-PRC_GROUP2_NEW = UF_VALUE.
    WHEN 'WERKS'.
      CS_BVB-PLANT_NEW = UF_VALUE.
    WHEN 'VKBUR'.
      CS_BVB-SALES_OFF_NEW = UF_VALUE.
    WHEN 'VKGRP'.
      CS_BVB-SALES_GRP_NEW = UF_VALUE.
    WHEN 'VTWEG'.
      CS_BVB-DISTR_CHAN_NEW = UF_VALUE.
    WHEN 'SPART'.
      CS_BVB-DIVISION_NEW = UF_VALUE.
    WHEN 'KVGR1'.
      CS_BVB-CUST_GRP1_NEW = UF_VALUE.
    WHEN 'KVGR2'.
      CS_BVB-CUST_GRP2_NEW = UF_VALUE.
    WHEN 'KDGRP'.
      CS_BVB-CUST_GROUP_KEY = UF_VALUE.
    WHEN 'FKART'.
      CS_ZBVB-FKART = UF_VALUE.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DATA_TYPE
*&---------------------------------------------------------------------*
FORM F_VALIDATE_DATA_TYPE  USING    UF_TYPE_KIND TYPE ABAP_TYPEKIND
                                    UREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION
                                    UF_INPUT TYPE ANY
                                    UF_FNAME TYPE ANY
                           CHANGING CF_DATA TYPE ANY
                                    CF_MSG TYPE ANY.
  DATA: LF_INVALID .

  CLEAR: CF_DATA,
         CF_MSG.
  IF UF_INPUT IS INITIAL.
    RETURN.
  ENDIF.
*       check format data type
  CASE UF_TYPE_KIND.
    WHEN GC_DATTY-DATE.
      UREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = UF_INPUT
                                              IF_FORMAT = SPACE
                                    IMPORTING EF_OUTPUT = CF_DATA
                                              EF_INVALID = LF_INVALID ).
      IF LF_INVALID = GC_TRUE.
        CF_MSG = | Invalid Date field "{ UF_FNAME }"| ##NO_TEXT.
      ENDIF.
    WHEN GC_DATTY-PACK
    OR   GC_DATTY-NUM.
      CALL METHOD UREF_VALIDATE->VALIDATE_NUMBER
        EXPORTING
          IF_INPUT   = UF_INPUT
          IF_FORMAT  = 'A' "amount
        IMPORTING
          EF_OUTPUT  = CF_DATA
          EF_INVALID = LF_INVALID.
      IF LF_INVALID = GC_TRUE.
        CF_MSG = | Invalid number field "{ UF_FNAME }"| ##NO_TEXT.
      ENDIF.
    WHEN OTHERS.
      CF_DATA = UF_INPUT.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_conversion_exit
*&---------------------------------------------------------------------*
FORM F_CONVERSION_EXIT USING VALUE(PV_EXIT)  TYPE ANY
                     VALUE(PV_TYPE)  TYPE ANY
                     VALUE(PV_INPUT) TYPE ANY
               CHANGING PV_OUTPUT    TYPE ANY.

  DATA: LV_CONEXIT TYPE CHAR30.

  CONCATENATE 'CONVERSION_EXIT_' PV_EXIT '_' PV_TYPE INTO LV_CONEXIT.
  CONDENSE LV_CONEXIT NO-GAPS.

  PV_OUTPUT = PV_INPUT.

  CALL FUNCTION LV_CONEXIT ##FM_SUBRC_OK
    EXPORTING
      INPUT  = PV_OUTPUT
    IMPORTING
      OUTPUT = PV_OUTPUT
               EXCEPTIONS
               OTHERS.

ENDFORM.                    "f_conversion_exit
*&---------------------------------------------------------------------*
*& Form F_ASSIGN_Settlement
*&---------------------------------------------------------------------*
FORM F_ASSIGN_SETTLEMENT  USING    US_XLSX TYPE TS_XLSX
                          CHANGING CT_ST TYPE TT_ST
                                   CT_RESULT TYPE TT_RESULT
                                   CT_MAIN TYPE TT_MAIN.
  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION,
    LS_DATA       TYPE TS_ST,
    LS_MESSG      TYPE TS_MESSG,
    LS_MAIN       TYPE TS_MAIN,
    LF_ROWNO      TYPE TS_RESULT-ROWNO,
    LF_MSGTX      TYPE TF_MSGTX,
    LF_TXT        TYPE TF_MSGTX,
    LF_FNAME      TYPE TF_FNAME,
    LF_TYPE_KIND  TYPE ABAP_TYPEKIND,
    LF_COL_INDEX  TYPE I.
  FIELD-SYMBOLS: <L_REQ>   TYPE ANY,
                 <L_FNAME> TYPE ANY.

  CLEAR CT_ST.

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.
  CREATE OBJECT LREF_VALIDATE.
  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>) .
    IF <L_ROW> IS INITIAL.
      EXIT.
    ENDIF.
*   Keep Row no
    LF_ROWNO = SY-TABIX.
    IF LF_ROWNO = P_FNAME .
      ASSIGN <L_ROW> TO <L_FNAME>.
    ENDIF.

    IF LF_ROWNO = P_CHKREQ .
      ASSIGN <L_ROW> TO <L_REQ>.
    ENDIF.
    CHECK LF_ROWNO GE P_BEGROW.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.
    LS_DATA-SHEET = US_XLSX-WORKSHEET_NAME.
    LS_DATA-ROWNO = LF_ROWNO.
    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).
      CLEAR LF_TXT.
      CHECK SY-TABIX GE P_BEGCOL.

      LF_COL_INDEX = SY-TABIX - P_BEGCOL + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE_ST'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME
                                         LF_TYPE_KIND.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_INPUT>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      IF LF_FNAME = 'INDEX_KEY'.
        IF <L_FIELD_INPUT> = P_END .
          EXIT.
        ENDIF.
        LS_MAIN-INDEX_KEY = <L_FIELD_INPUT>.
        INSERT LS_MAIN INTO TABLE CT_MAIN.
      ENDIF.
*     Assign fieldname record
      ASSIGN <L_FNAME>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_FNAME>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Assign required check record
      ASSIGN <L_REQ>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD_REQ>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Assign target Field
      ASSIGN LS_DATA-(LF_FNAME) TO FIELD-SYMBOL(<L_FIELD_DATA>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
* --check required
      IF   <L_FIELD_REQ> = 'M'. "mandatory.
        IF <L_FIELD_INPUT> IS INITIAL.
          LF_TXT = | Required value "{ <L_FIELD_FNAME> }"| ##NO_TEXT.
          SET_MESSAGE LF_MSGTX LF_TXT.
          CONTINUE.
        ENDIF.
      ENDIF.
*-- check format data type
      PERFORM F_VALIDATE_DATA_TYPE USING LF_TYPE_KIND
                                         LREF_VALIDATE
                                         <L_FIELD_INPUT>
                                         <L_FIELD_FNAME>
                                   CHANGING <L_FIELD_DATA>
                                            LF_TXT.
      IF LF_TXT IS NOT INITIAL.
        SET_MESSAGE LF_MSGTX LF_TXT.
      ENDIF.
    ENDLOOP.
    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSSD01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.
*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT1  USING  LS_DATA
                                        LS_MESSG
                               CHANGING CT_RESULT.
      CONTINUE.
    ENDIF.
    READ TABLE CT_RESULT TRANSPORTING NO FIELDS WITH KEY INDEX_KEY = LS_DATA-INDEX_KEY.
    IF SY-SUBRC <> 0. "not found error
*   Collect Data
      INSERT LS_DATA INTO TABLE CT_ST.
    ENDIF.
  ENDLOOP.

  LOOP AT CT_ST ASSIGNING FIELD-SYMBOL(<L_ST>).
    READ TABLE CT_RESULT TRANSPORTING NO FIELDS WITH KEY INDEX_KEY = LS_DATA-INDEX_KEY.
    IF SY-SUBRC = 0. "found error , delete all the same key
      DELETE CT_ST.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_CONDITION_CONTRACT_CREATE
*&---------------------------------------------------------------------*
FORM F_CONDITION_CONTRACT_CREATE  USING    UT_MC TYPE TT_MC
                                           UT_BC TYPE TT_BC
                                           UT_ST TYPE TT_ST
                                           UT_MAIN TYPE TT_MAIN
                                  CHANGING CT_RESULT TYPE TT_RESULT
                                           CS_PROC   TYPE TS_PROC_INFO.
  DATA: LS_HEAD         TYPE BAPICCHEAD,
        LS_HEADX        TYPE BAPICCHEADX,
        LS_CONDKEY      TYPE BAPICCCONDITIONKEY,
        LS_CONDKEYX     TYPE BAPICCCONDITIONKEYX,
        LS_CONDITEM     TYPE BAPICCCONDITIONITEM,
        LS_CONDITEMX    TYPE BAPICCCONDITIONITEMX,
        LS_TE_KOMG      TYPE BAPI_TE_KOMG,
        LS_TE_KOMGX     TYPE BAPI_TE_KOMGX,
        LS_SCALE        TYPE BAPICCSCALE,
        LS_BVB          TYPE BAPICCBVB,
        LS_BVBX         TYPE BAPICCBVBX,
        LS_EXTENSION    TYPE BAPIPAREX,
        LS_CAL          TYPE BAPICCCAL,
        LS_CALX         TYPE BAPICCCALX,
        LS_COND_COMPARE TYPE TS_COND_COMPARE,
        LS_COND_OLD     TYPE TS_COND_COMPARE,
        LF_SUCCESS      TYPE FLAG,
        LS_RESULT       TYPE TS_RESULT.
  DATA: LT_CONDKEY   TYPE STANDARD TABLE OF BAPICCCONDITIONKEY,
        LT_CONDKEYX  TYPE STANDARD TABLE OF BAPICCCONDITIONKEYX,
        LT_CONDITEM  TYPE STANDARD TABLE OF BAPICCCONDITIONITEM,
        LT_CONDITEMX TYPE STANDARD TABLE OF BAPICCCONDITIONITEMX,
        LT_SCALE     TYPE STANDARD TABLE OF BAPICCSCALE,
        LT_BVB       TYPE STANDARD TABLE OF BAPICCBVB,
        LT_BVBX      TYPE STANDARD TABLE OF BAPICCBVBX,
        LT_EXTENSION TYPE STANDARD TABLE OF BAPIPAREX,
        LT_CAL       TYPE STANDARD TABLE OF BAPICCCAL,
        LT_CALX      TYPE STANDARD TABLE OF BAPICCCALX,
        LT_RETURN    TYPE STANDARD TABLE OF  BAPIRET2.

  DATA: LF_TABIX     TYPE SY-TABIX,
        LF_ORDER_KEY TYPE NUM10,
        LF_NUMBER    TYPE BAPICCKEY-CONDITION_CONTRACT_NUMBER.

  IF UT_MC[] IS NOT INITIAL.
    SELECT A~CONTRACT_TYPE,                            "#EC CI_BUFFJOIN
           A~BUSVOLBASE_TYPE,
           B~FIELDCOMB,
           B~VALIDITY_ACTIVE
    INTO TABLE @DATA(LT_BVBTYPE)
    FROM TWCBCONTRTYPE AS A INNER JOIN WB2_C_BVBTYPE_FC AS B
    ON   A~BUSVOLBASE_TYPE = B~BUSVOLBASE_TYPE
    FOR ALL ENTRIES IN @UT_MC
    WHERE CONTRACT_TYPE  =  @UT_MC-CONTRACT_TYPE.
    SORT LT_BVBTYPE BY CONTRACT_TYPE FIELDCOMB.
  ENDIF.

  LOOP AT UT_MAIN ASSIGNING FIELD-SYMBOL(<L_MAIN>).
    READ TABLE CT_RESULT INTO LS_RESULT WITH KEY INDEX_KEY = <L_MAIN>-INDEX_KEY
                                                 MSGTY = 'E'.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.
    CLEAR:  LT_CONDKEY   ,
            LT_CONDKEYX  ,
            LT_CONDITEM  ,
            LT_CONDITEMX ,
            LT_SCALE     ,
            LT_BVB       ,
            LT_BVBX      ,
            LT_EXTENSION ,
            LT_CAL      ,
            LT_CALX     ,
            LT_RETURN   ,
            LS_HEAD     ,
            LS_HEADX    .
    CLEAR: LF_TABIX,
           LF_ORDER_KEY,
           LF_NUMBER,
           LS_RESULT.

* =========================Main Contract
    LOOP AT UT_MC ASSIGNING FIELD-SYMBOL(<L_MC>) WHERE INDEX_KEY = <L_MAIN>-INDEX_KEY.
      MOVE-CORRESPONDING <L_MC> TO LS_COND_COMPARE .
      LF_TABIX = LF_TABIX + 1.

      IF LF_TABIX = 1.
*  -- head data => HEADDATAIN, HEADDATAINX
        CLEAR: LS_HEAD,
               LS_HEADX.
        MOVE-CORRESPONDING <L_MC> TO LS_HEAD.
        LS_HEAD-AF_GROUP = GF_AF_GROUP .
        PERFORM F_MARK_STRC_DATA_X USING 'BAPICCHEADX'
                                    LS_HEAD
                             CHANGING LS_HEADX.

      ENDIF.
      IF LS_COND_COMPARE <> LS_COND_OLD.
        LF_ORDER_KEY  = LF_ORDER_KEY + 1.

*  -- Condition key => CONDITIONKEYDATAIN, CONDITIONKEYDATAINX
        CLEAR: LS_CONDKEY,
               LS_CONDKEYX,
               LS_TE_KOMG,
               LS_TE_KOMGX.
        MOVE-CORRESPONDING <L_MC> TO LS_CONDKEY.
        PERFORM F_MARK_STRC_DATA_X USING 'BAPICCCONDITIONKEYX'
                                         LS_CONDKEY
                             CHANGING LS_CONDKEYX.
        LS_CONDKEY-ORDER_KEY = LF_ORDER_KEY.
        LS_CONDKEYX-ORDER_KEY = LF_ORDER_KEY.
        INSERT LS_CONDKEY INTO TABLE LT_CONDKEY.
        INSERT LS_CONDKEYX INTO TABLE LT_CONDKEYX.
*   --extension
        IF  <L_MC>-KVGR1 IS NOT INITIAL.
          MOVE-CORRESPONDING LS_CONDKEY TO LS_TE_KOMG ##ENH_OK.
          LS_TE_KOMG-KVGR1 = <L_MC>-KVGR1.
          LS_EXTENSION-STRUCTURE = 'BAPI_TE_KOMG'.
          LS_EXTENSION-VALUEPART1 = LS_TE_KOMG  ##ENH_OK.
          APPEND LS_EXTENSION TO LT_EXTENSION.

          MOVE-CORRESPONDING  LS_TE_KOMG TO LS_TE_KOMGX ##ENH_OK.
          LS_TE_KOMGX-KVGR1 = ABAP_TRUE.
          LS_EXTENSION-STRUCTURE = 'BAPI_TE_KOMGX'.
          LS_EXTENSION-VALUEPART1 = LS_TE_KOMGX  ##ENH_OK.
          APPEND LS_EXTENSION TO LT_EXTENSION.
        ENDIF.
*   --Condition item => CONDITIONITEMDATAIN , CONDITIONITEMDATAINX
        CLEAR: LS_CONDITEM,
               LS_CONDITEMX.
        MOVE-CORRESPONDING <L_MC> TO LS_CONDITEM.
        LS_CONDITEM-COND_COUNT = 1.

        IF <L_MC>-SCALE_EXISTS =  ABAP_TRUE.
          IF <L_MC>-SCALEBASIS = 'C' . "quantity
            LS_CONDITEM-SCALE_UNIT = LS_CONDITEM-COND_UNIT.
          ELSE.
            LS_CONDITEM-SCALECURR = LS_CONDITEM-CONDCURR.
          ENDIF.
        ENDIF.
        IF LS_CONDITEM-CALC_TYPE = 'A'.
          LS_CONDITEM-CONDCURR = '%'.
        ENDIF.
        PERFORM F_MARK_STRC_DATA_X USING 'BAPICCCONDITIONITEMX'
                                         LS_CONDITEM
                             CHANGING LS_CONDITEMX.
        LS_CONDITEM-ORDER_KEY = LF_ORDER_KEY.
        LS_CONDITEMX-ORDER_KEY = LF_ORDER_KEY.
        LS_CONDITEMX-COND_COUNT = 1.
        LS_CONDITEMX-UPDATEFLAG = 'I'.
        INSERT LS_CONDITEM INTO TABLE LT_CONDITEM.
        INSERT LS_CONDITEMX INTO TABLE LT_CONDITEMX.


      ENDIF.

*     -- sacle => SCALEDATAIN
      IF <L_MC>-SCALE_EXISTS =  ABAP_TRUE.
        CLEAR LS_SCALE.
        MOVE-CORRESPONDING <L_MC> TO LS_SCALE.
        LS_SCALE-COND_COUNT = 1.
        LS_SCALE-SCALE_BASE_QTY = <L_MC>-COND_P_UNT.
        LS_SCALE-ORDER_KEY = LF_ORDER_KEY.
        IF <L_MC>-SCALEBASIS = 'C' . "quantity
          LS_SCALE-SCALE_BASE_QTY = <L_MC>-SCALE_BASE_VALUE.
          CLEAR LS_SCALE-SCALE_BASE_VALUE.
        ENDIF.
        INSERT LS_SCALE INTO TABLE LT_SCALE.
      ENDIF.
      MOVE-CORRESPONDING <L_MC> TO LS_COND_OLD.
    ENDLOOP.

    CLEAR LF_ORDER_KEY.
* =========================Main Contract
    LOOP AT UT_BC ASSIGNING FIELD-SYMBOL(<L_BC>) WHERE INDEX_KEY = <L_MAIN>-INDEX_KEY.
      LF_ORDER_KEY  = LF_ORDER_KEY + 1.
      CLEAR: LS_BVB,
             LS_BVBX,
             LS_EXTENSION.
      LS_BVB = <L_BC>-BVB.
      READ TABLE LT_BVBTYPE INTO DATA(LS_BVBTYPE) WITH KEY CONTRACT_TYPE = <L_MC>-CONTRACT_TYPE
                                                           FIELDCOMB = LS_BVB-FIELDCOMBINATION
                                                           BINARY SEARCH.
      IF SY-SUBRC = 0
      AND LS_BVBTYPE-VALIDITY_ACTIVE = ABAP_FALSE.
        CLEAR: LS_BVB-VALID_FROM_NEW,
               LS_BVB-VALID_TO_NEW.
      ENDIF.
      PERFORM F_MARK_STRC_DATA_X USING 'BAPICCBVBX'
                                       LS_BVB
                                CHANGING LS_BVBX.
      LS_BVB-ORDER_KEY = LF_ORDER_KEY.
      LS_BVBX-ORDER_KEY = LF_ORDER_KEY.
      LS_BVBX-UPDATEFLAG = 'I'.

      INSERT LS_BVB INTO TABLE LT_BVB.
      INSERT LS_BVBX INTO TABLE LT_BVBX.
      IF <L_BC>-ZBVB IS NOT INITIAL.
        <L_BC>-ZBVB-ORDER_KEY = LF_ORDER_KEY.
        LS_EXTENSION-STRUCTURE = 'BAPI_TE_BVB'.
        LS_EXTENSION-VALUEPART1 = <L_BC>-ZBVB ##ENH_OK.
        APPEND LS_EXTENSION TO LT_EXTENSION.
      ENDIF.
    ENDLOOP.

* =========================Settlement Date
    CLEAR: LF_ORDER_KEY.
    LOOP AT UT_ST ASSIGNING FIELD-SYMBOL(<L_ST>) WHERE INDEX_KEY = <L_MAIN>-INDEX_KEY.

      LF_ORDER_KEY  =   LF_ORDER_KEY + 1.

      CLEAR: LS_CAL,
             LS_CALX.
      MOVE-CORRESPONDING   <L_ST> TO LS_CAL.
      LS_CAL-ORDER_KEY = LF_ORDER_KEY.
      LS_CALX-ORDER_KEY = LF_ORDER_KEY.
      LS_CALX-UPDATEFLAG = 'I'.
      INSERT LS_CAL INTO TABLE LT_CAL.
      INSERT LS_CALX INTO TABLE LT_CALX.
    ENDLOOP.

    SET UPDATE TASK LOCAL.
    CLEAR LF_NUMBER.
    CALL FUNCTION 'BAPI_CONDITION_CONTRACT_CREATE'
      EXPORTING
        HEADDATAIN              = LS_HEAD
        HEADDATAINX             = LS_HEADX
        MIGRATIONTESTRUN        = CB_TEST
      IMPORTING
        CONDITIONCONTRACTNUMBER = LF_NUMBER
      TABLES
        CONDITIONKEYDATAIN      = LT_CONDKEY
        CONDITIONKEYDATAINX     = LT_CONDKEYX
        CONDITIONITEMDATAIN     = LT_CONDITEM
        CONDITIONITEMDATAINX    = LT_CONDITEMX
        EXTENSIONIN             = LT_EXTENSION
        RETURN                  = LT_RETURN
        SCALEDATAIN             = LT_SCALE
        BVBDATAIN               = LT_BVB
        BVBDATAINX              = LT_BVBX
        CALENDARDATAIN          = LT_CAL
        CALENDARDATAINX         = LT_CALX.

    LS_RESULT-INDEX_KEY = <L_MAIN>-INDEX_KEY.
    LS_RESULT-NUM = LF_NUMBER.
    CLEAR LF_SUCCESS.
    PERFORM F_READ_RETURN_CREATE USING LT_RETURN
                                       LS_RESULT
                                 CHANGING CT_RESULT
                                          CS_PROC
                                          LF_SUCCESS.
    IF LF_SUCCESS = ABAP_TRUE
    AND CB_TEST = ABAP_FALSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_MARK_STRC_DATA_X
*&---------------------------------------------------------------------*
FORM F_MARK_STRC_DATA_X  USING US_STRC TYPE DFIES-TABNAME
                               US_INPUT_DATA TYPE ANY
                         CHANGING CH_STRC_X TYPE ANY.

  DATA: LT_FIELDS TYPE TABLE OF DFIES.
  CLEAR CH_STRC_X.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = US_STRC
    TABLES
      DFIES_TAB      = LT_FIELDS
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
  ENDIF.

  LOOP AT LT_FIELDS INTO DATA(LS_FIELD) ##INTO_OK.

    ASSIGN COMPONENT LS_FIELD-FIELDNAME OF STRUCTURE US_INPUT_DATA TO
    FIELD-SYMBOL(<L_SOURCE>).
    CHECK <L_SOURCE> IS ASSIGNED.

    IF <L_SOURCE> IS NOT INITIAL.

      ASSIGN COMPONENT LS_FIELD-FIELDNAME OF STRUCTURE CH_STRC_X TO
      FIELD-SYMBOL(<LFS_MARK_X>).
      CHECK <LFS_MARK_X> IS ASSIGNED.

      <LFS_MARK_X> = GC_TRUE.

      UNASSIGN <LFS_MARK_X>.

    ENDIF.

    UNASSIGN <L_SOURCE>.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_return_create
*&---------------------------------------------------------------------*
FORM F_READ_RETURN_CREATE  USING    UT_RETURN TYPE BAPIRET2_TAB
                                    US_RESULT TYPE TS_RESULT
                           CHANGING CT_RESULT TYPE TT_RESULT
                                    CS_PROC  TYPE TS_PROC_INFO
                                    CS_SUCCESS TYPE FLAG.
  DATA: LS_MESSG TYPE TS_MESSG.

  READ TABLE UT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                   WITH KEY TYPE = 'S'.
  IF SY-SUBRC = 0.
    LS_MESSG-MSGTY = 'S'.
    LS_MESSG-MSGID = 'ZSDSSD01'.
    LS_MESSG-MSGNO = '000'.
    IF CB_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
      LS_MESSG-MSGTX = TEXT-I01.
    ELSE.
*       Text-i02: Condition contract has been created.
      LS_MESSG-MSGTX = TEXT-I02.
    ENDIF.
    PERFORM F_COLLECT_RESULT1  USING  US_RESULT
                                      LS_MESSG
                             CHANGING CT_RESULT.
    CS_PROC-COUNT-SUCCS = CS_PROC-COUNT-SUCCS + 1.
    CS_SUCCESS = ABAP_TRUE.
  ELSE.
    LOOP AT UT_RETURN ASSIGNING <L_RETURN>.

      IF <L_RETURN>-TYPE CA 'EAX' AND
        " Document $1 is incorrect, saving is not possible
        ( <L_RETURN>-TYPE NE 'W_CB' AND <L_RETURN>-NUMBER <> '078' ).

        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.

        PERFORM F_COLLECT_RESULT1  USING  US_RESULT
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDIF.
    ENDLOOP.
    CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + 1.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
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
*&---------------------------------------------------------------------*
*& Form f_get_constants
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANTS .
  DATA: LT_GEN_C TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = SY-REPID
    IMPORTING
      ET_GEN_C = LT_GEN_C.
  LOOP AT LT_GEN_C  ASSIGNING FIELD-SYMBOL(<L_GEN_C>).
    CASE <L_GEN_C>-PARAM.
      WHEN 'AMOUNT_FIELDS_GROUP'.
        GF_AF_GROUP = <L_GEN_C>-VALUE_LOW.
    ENDCASE.
  ENDLOOP.
ENDFORM.
