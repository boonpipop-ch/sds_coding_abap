*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0250
*  Creation Date      : 01.08.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          :
*  Description        : This is a program to upload Sales Order Doc from ECC6
*  Purpose            : To upload Sales Order Doc from ECC6
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0250.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
##NEEDED
TABLES:
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TS_FNAME  TYPE  CHAR40.
TYPES: TS_MSG_TXT  TYPE  ZSDSDE_MSGTX.

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

TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSSDS051.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_MESSG,
         ROWNO TYPE  TS_RESULT-ROWNO,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  TS_MSG_TXT,
       END OF TS_MESSG.

TYPES: TS_XLSX TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA.
TYPES: TT_XLSX TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.

TYPES: BEGIN OF TS_SUM,
         TOTAL TYPE  I, "Total Entries
         SUCCS TYPE  I, "Success Entries
         ERROR TYPE  I, "Error Entries
       END   OF TS_SUM.

TYPES: TT_MESSG  TYPE  STANDARD TABLE OF TS_MESSG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_HEAD,
         QUOTATION     TYPE VBELN_VA,
         SALESORDER    TYPE VBELN_VA,
         DOC_TYPE      TYPE AUART,
         SALES_ORG     TYPE VKORG,
         DISTR_CHAN    TYPE VTWEG,
         DIVISION      TYPE SPART,
         SALES_OFF     TYPE VKBUR,
         SALES_GRP     TYPE VKGRP,
         PURCH_NO_C    TYPE BSTNK,
         CT_VALID_F    TYPE ANGDT_V,
         CT_VALID_T    TYPE BNDDT,
         PARTN_NUMB_AG TYPE KUNNR,
         PARTN_NUMB_WE TYPE KUNNR,
         PARTN_NUMB_RE TYPE KUNNR,
         PARTN_NUMB_RG TYPE KUNNR,
         PARTN_NUMB_VE TYPE KUNNR,
         NAME          TYPE TEXT100,
         NAME_2        TYPE TEXT100,
         NAME_3        TYPE TEXT100,
         NAME_4        TYPE TEXT100,
         STREET        TYPE TEXT100,
         STR_SUPPL3    TYPE TEXT100,
         LOCATION      TYPE TEXT100,
         STR_SUPPL1    TYPE TEXT100,
         STR_SUPPL2    TYPE TEXT100,
         DISTRICT      TYPE TEXT100,
         CITY          TYPE TEXT100,
         POSTL_COD1    TYPE TEXT100,
         STCD3         TYPE TEXT100,
         PMNTTRMS      TYPE DZTERM,
         BNAME         TYPE BNAME_V,
         REF_1         TYPE IHREZ,
         USAGE         TYPE ABRVW,
         CONDTYPE_ZDH1 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZDH2 TYPE KBETR,
         CONDTYPE_ZDH3 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZDH4 TYPE KBETR,
         CONDTYPE_ZDH5 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZDH6 TYPE KBETR,
         HDTXT_ZH01    TYPE TEXT8192,
         HDTXT_ZH03    TYPE TEXT8192,
         HDTXT_ZH06    TYPE TEXT8192,
         HDTXT_ZH10    TYPE TEXT8192,
*         HDTXT_ZH13    TYPE TEXT8192, "Swap ZH13 <--> ZH09
         HDTXT_ZH09    TYPE TEXT8192,
         HDTXT_ZH15    TYPE TEXT8192,
         HDTXT_ZH19    TYPE TEXT8192,
*         HDTXT_ZH09    TYPE TEXT8192, "Swap ZH13 <--> ZH09
         HDTXT_ZH13    TYPE TEXT8192,
       END OF TS_HEAD.

TYPES: BEGIN OF TS_SCHED,
         SALESORDER2  TYPE VBELN_VA,
         ITM_NUMBER2  TYPE POSNR,
         SCHED_LINE_1 TYPE ETENR,
         REQ_DATE_1   TYPE EDATU,
         REQ_QTY_1    TYPE WMENG,
       END OF TS_SCHED.

TYPES: TT_SCHED  TYPE  STANDARD TABLE OF TS_SCHED ##NEEDED
                        WITH DEFAULT KEY.

TYPES: BEGIN OF TS_ITEM,
*-----Item-------
         ITM_NUMBER1   TYPE POSNR,
         HG_LV_ITEM    TYPE UEPOS,
         MATERIAL      TYPE MATNR,
         TARGET_QTY    TYPE DZMENG,
         TARGET_QU     TYPE DZIEME,
         PLANT         TYPE WERKS_EXT,
         STORE_LOC     TYPE LGORT_D,
         ITEM_CATE     TYPE PSTYV,
         ORDERID       TYPE AUFNR,
         WBS_ELEM      TYPE TEXT100,
         ZLOB          TYPE TEXT100,

*---Schedule Line
*         SCHED         TYPE TT_SCHED,
         ITM_NUMBER2   TYPE POSNR,
         SCHED_LINE_1  TYPE ETENR,
         REQ_DATE_1    TYPE EDATU,
         REQ_QTY_1     TYPE WMENG,

*---Condition
         CONDTYPE_ZPR1 TYPE KBETR,
         CONDTYPE_ZDI1 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZDI2 TYPE KBETR,
         CONDTYPE_ZDI3 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZDI4 TYPE KBETR,
         CONDTYPE_ZD01 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZD02 TYPE KBETR,
         CONDTYPE_ZD03 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZD04 TYPE KBETR,
         CONDTYPE_ZD05 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZD06 TYPE KBETR,
         CONDTYPE_ZD07 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZD08 TYPE KBETR,
         CONDTYPE_ZD09 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZD10 TYPE KBETR,
         CONDTYPE_ZF01 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZPS1 TYPE KBETR,
         CONDTYPE_ZPS2 TYPE KBETR,
         CONDTYPE_ZPS3 TYPE KBETR,
         CONDTYPE_ZWR1 TYPE BIDT_A_DEC_N, "KBETR,
         CONDTYPE_ZWR2 TYPE KBETR,
         ROWNO         TYPE TS_RESULT-ROWNO,
       END OF TS_ITEM.

TYPES: TT_ITEM  TYPE  STANDARD TABLE OF TS_ITEM
                        WITH DEFAULT KEY.

TYPES: BEGIN OF TS_DOC,
         VBELN TYPE  VBAK-VBELN,
       END OF TS_DOC.

TYPES: BEGIN OF TS_KEY,
         DOCNO TYPE VBAK-VBELN,
       END OF TS_KEY.

TYPES: BEGIN OF TS_DATA,
         KEY    TYPE  TS_KEY,
         HEAD   TYPE  TS_HEAD,
         ITEM   TYPE  TT_ITEM,
         MESSG  TYPE  TT_MESSG,
         RESULT TYPE  TS_DOC,
       END OF TS_DATA.
TYPES: TT_DATA  TYPE  STANDARD TABLE OF TS_DATA.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
TYPES: BEGIN OF TS_TEMPLATE ##NEEDED,
*--Header-----------------
         QUOTATION     TYPE VBELN_VA,
         SALESORDER    TYPE VBELN_VA,
         DOC_TYPE      TYPE AUART,
         SALES_ORG     TYPE VKORG,
         DISTR_CHAN    TYPE VTWEG,
         DIVISION      TYPE SPART,
         SALES_OFF     TYPE VKBUR,
         SALES_GRP     TYPE VKGRP,
         PURCH_NO_C    TYPE BSTNK,
         CT_VALID_F    TYPE ANGDT_V,
         CT_VALID_T    TYPE BNDDT,
         PARTN_NUMB_AG TYPE KUNNR,
         PARTN_NUMB_WE TYPE KUNNR,
         PARTN_NUMB_RE TYPE KUNNR,
         PARTN_NUMB_RG TYPE KUNNR,
         PARTN_NUMB_VE TYPE KUNNR,
         NAME          TYPE TEXT100,
         NAME_2        TYPE TEXT100,
         NAME_3        TYPE TEXT100,
         NAME_4        TYPE TEXT100,
         STREET        TYPE TEXT100,
         STR_SUPPL3    TYPE TEXT100,
         LOCATION      TYPE TEXT100,
         STR_SUPPL1    TYPE TEXT100,
         STR_SUPPL2    TYPE TEXT100,
         DISTRICT      TYPE TEXT100,
         CITY          TYPE TEXT100,
         POSTL_COD1    TYPE TEXT100,
         STCD3         TYPE TEXT100,
         PMNTTRMS      TYPE DZTERM,
         BNAME         TYPE BNAME_V,
         REF_1         TYPE IHREZ,
         USAGE         TYPE ABRVW,
         CONDTYPE_ZDH1 TYPE KBETR,
         CONDTYPE_ZDH2 TYPE KBETR,
         CONDTYPE_ZDH3 TYPE KBETR,
         CONDTYPE_ZDH4 TYPE KBETR,
         CONDTYPE_ZDH5 TYPE KBETR,
         CONDTYPE_ZDH6 TYPE KBETR,
         HDTXT_ZH01    TYPE TEXT8192,
         HDTXT_ZH03    TYPE TEXT8192,
         HDTXT_ZH06    TYPE TEXT8192,
         HDTXT_ZH10    TYPE TEXT8192,
*         HDTXT_ZH13    TYPE TEXT8192, Swap ZH13 <--> HDTXT_ZH09
         HDTXT_ZH09    TYPE TEXT8192,
         HDTXT_ZH15    TYPE TEXT8192,
         HDTXT_ZH19    TYPE TEXT8192,
*         HDTXT_ZH09    TYPE TEXT8192, "Swap ZH13 <--> HDTXT_ZH09
         HDTXT_ZH13    TYPE TEXT8192,
*-----Item-------
         ITM_NUMBER1   TYPE POSNR,
         HG_LV_ITEM    TYPE UEPOS,
         MATERIAL      TYPE MATNR,
         TARGET_QTY    TYPE DZMENG,
         TARGET_QU     TYPE DZIEME,
         PLANT         TYPE WERKS_EXT,
         STORE_LOC     TYPE LGORT_D,
         ITEM_CATE     TYPE PSTYV,
         ORDERID       TYPE AUFNR,
         WBS_ELEM      TYPE TEXT100,
         ZLOB          TYPE TEXT100,

*---Schedule Line
         ITM_NUMBER2   TYPE POSNR,
         SCHED_LINE_1  TYPE ETENR,
         REQ_DATE_1    TYPE EDATU,
         REQ_QTY_1     TYPE WMENG,

*---Condition
         CONDTYPE_ZPR1 TYPE KBETR,
         CONDTYPE_ZDI1 TYPE KBETR,
         CONDTYPE_ZDI2 TYPE KBETR,
         CONDTYPE_ZDI3 TYPE KBETR,
         CONDTYPE_ZDI4 TYPE KBETR,
         CONDTYPE_ZD01 TYPE KBETR,
         CONDTYPE_ZD02 TYPE KBETR,
         CONDTYPE_ZD03 TYPE KBETR,
         CONDTYPE_ZD04 TYPE KBETR,
         CONDTYPE_ZD05 TYPE KBETR,
         CONDTYPE_ZD06 TYPE KBETR,
         CONDTYPE_ZD07 TYPE KBETR,
         CONDTYPE_ZD08 TYPE KBETR,
         CONDTYPE_ZD09 TYPE KBETR,
         CONDTYPE_ZD10 TYPE KBETR,
         CONDTYPE_ZF01 TYPE KBETR,
         CONDTYPE_ZPS1 TYPE KBETR,
         CONDTYPE_ZPS2 TYPE KBETR,
         CONDTYPE_ZPS3 TYPE KBETR,
         CONDTYPE_ZWR1 TYPE KBETR,
         CONDTYPE_ZWR2 TYPE KBETR,
       END OF TS_TEMPLATE.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSSD017',
  GC_ZO01  TYPE  AUART     VALUE 'ZO01',
  GC_NOCHG TYPE CHAR5      VALUE '-'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GT_RESULT TYPE TT_RESULT.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GS_PROC TYPE  TS_PROC_INFO,
  GS_SUM  TYPE  TS_SUM.
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
CONSTANTS:
  GC_VA03 TYPE  SY-TCODE    VALUE 'VA03',
  GC_Z060 TYPE  KNA1-KTOKD  VALUE 'Z060'.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS051'.

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

DEFINE CONVERT_ALPHA_INPUT ##NEEDED.

  if &1 is not initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &2.
  endif.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s02: File Selection
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME .
  PARAMETERS:
    P_IFILE TYPE  STRING LOWER CASE MODIF ID LOC,
    P_BGCOL TYPE I DEFAULT 1,
    P_BGROW TYPE I DEFAULT 4.
SELECTION-SCREEN END OF BLOCK B2.
* Text-s03: Processing Options
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    CB_TEST TYPE CHAR1 RADIOBUTTON GROUP G2 DEFAULT 'X'
                                    USER-COMMAND DMY,
    CB_POST TYPE CHAR1 RADIOBUTTON GROUP G2.
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Processing Data
  PERFORM F_PROCESS_DATA  USING  CB_TEST
                        CHANGING GT_RESULT
                                 GS_PROC
                                 GS_SUM.
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


*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.
*&---------------------------------------------------------------------*
*& Form F_AUTHORIZE_CHECK
*&---------------------------------------------------------------------*
*& Check Authorization on t-code
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK  USING UF_TCODE  TYPE  SY-TCODE.
  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LIST_IFILE
*&---------------------------------------------------------------------*
*& Popup for Input file selection
*&---------------------------------------------------------------------*\
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
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA
*&---------------------------------------------------------------------*
*& Processing data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA  USING  UF_TEST   TYPE FLAG
                   CHANGING CT_RESULT TYPE TT_RESULT
                            CS_PROC   TYPE TS_PROC_INFO
                            CS_SUM    TYPE TS_SUM.
  DATA:
    LT_XLSX   TYPE  TT_XLSX,
    LT_RESULT TYPE  TT_RESULT,
    LT_DATA   TYPE  TT_DATA.

* Initialize Output
  CLEAR: CT_RESULT,
         CS_PROC.
  CLEAR: CS_SUM.

* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

*-Assign data
  PERFORM F_ASSIGN_SD_STRUC  USING LT_XLSX
                          CHANGING LT_DATA
                                   LT_RESULT.
  IF LT_RESULT IS NOT INITIAL.
*       Collect Final Result
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
    CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
  ENDIF.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
  PERFORM F_POST_DATA USING  LT_DATA
                             UF_TEST
                    CHANGING CT_RESULT
                             CS_SUM .

* Assign Processing Info
  CS_PROC-DATUM = SY-DATUM.
  CS_PROC-UZEIT = SY-UZEIT.
  CS_PROC-UNAME = SY-UNAME.
  CS_PROC-IFILE = P_IFILE.

  IF CB_TEST EQ GC_TRUE.
*   Text-x01: Test Run
    CS_PROC-MODE  =  TEXT-X01 .
  ELSE.
*   Text-x02: Production Run
    CS_PROC-MODE  = TEXT-X02 .
  ENDIF.

  CS_PROC-COUNT-TOTAL = CS_PROC-COUNT-SUCCS + CS_PROC-COUNT-ERROR.

* Show Message Complete
* Text-i07: Processing completed.
  MESSAGE S000(ZSDSPS01) WITH TEXT-I07 SPACE SPACE SPACE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_FILE
*&---------------------------------------------------------------------*
*& Read File into Text Table
*&---------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

  DATA:
    LF_FILENAME   TYPE  STRING.

* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 15 TEXT-P01.

  CLEAR: LF_FILENAME.
  LF_FILENAME = P_IFILE.

  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = LF_FILENAME
      IF_READ_ACTIVE_WORKSHEET    = 'X'
*     IF_XSTRING                  =
*     IT_WORKSHEET                =
    IMPORTING
      ET_XLSX_DATA                = CT_XLSX
    EXCEPTIONS
      MISSING_FILENAME_OR_XSTRING = 1
      ERROR_READ_FILENAME         = 2
      NO_DATA_FOUND               = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ASSIGN_SD_STRUC
*&---------------------------------------------------------------------*
*& Assign data to structure
*&---------------------------------------------------------------------*
FORM F_ASSIGN_SD_STRUC  USING    UT_XLSX  TYPE  TT_XLSX
                        CHANGING
                                 CT_DATA  TYPE TT_DATA
                                 CT_RESULT  TYPE  TT_RESULT.
  DATA:
    LT_RESULT  TYPE  TT_RESULT.


* Initialize Output
  CLEAR:
         CT_DATA,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

* Read Project Def Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>) INDEX 1.

  IF SY-SUBRC EQ 0.
    PERFORM F_ASSIGN_DATA USING  <L_SHEET>
                         CHANGING
                                  CT_DATA
                                  LT_RESULT.
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_data
*&---------------------------------------------------------------------*
*& Assign data
*&---------------------------------------------------------------------*

FORM F_ASSIGN_DATA  USING  US_XLSX    TYPE TS_XLSX
                 CHANGING  CT_DATA    TYPE TT_DATA
                           CT_RESULT  TYPE TT_RESULT.

  DATA:
    LS_DATA  TYPE TS_DATA,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_ROW_START TYPE I,
    LF_COL_START TYPE I,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE TS_FNAME,
    LF_MSGTX     TYPE TS_MSG_TXT.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.
  ##NEEDED
  DATA:
    LS_KEY   TYPE  TS_KEY,
    LS_ITEM  TYPE  TS_ITEM,
    LS_SCHED TYPE  TS_SCHED.
*    LS_ITEM_DATA TYPE  TS_ITEM_DATA.

*  DATA:
*    LF_KUNNR  TYPE  KNA1-KUNNR,
*    LF_LIFNR  TYPE  LFA1-LIFNR,
*    LF_AMOUNT TYPE  F.

* Initialize Output
  CLEAR:
         CT_DATA,
         CT_RESULT.

  LF_ROW_START = P_BGROW.
  LF_COL_START = P_BGCOL.

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE LF_ROW_START.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR:
      LS_DATA,
      LF_MSGTX.

    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>). "#EC CI_NESTED.

      CHECK SY-TABIX GE LF_COL_START.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Check No change value?
      CHECK <L_FIELD> NE GC_NOCHG.



      CASE LF_FNAME.
*---Fill data from Template Header to Header Data--------
        WHEN 'QUOTATION'.
          LS_DATA-HEAD-QUOTATION  = <L_FIELD>.
        WHEN 'SALESORDER'.
          LS_DATA-HEAD-SALESORDER  = <L_FIELD>.
          LS_KEY-DOCNO  = <L_FIELD>.
        WHEN 'DOC_TYPE'.
          LS_DATA-HEAD-DOC_TYPE = GC_ZO01.
*          PERFORM F_VALIDATE_DOCTYPE  USING  <L_FIELD>
*                                    CHANGING LS_DATA-HEAD-DOC_TYPE
*                                             LF_MSGTX.
        WHEN 'SALES_ORG'.
          LS_DATA-HEAD-SALES_ORG  = <L_FIELD>.
        WHEN 'DISTR_CHAN'.
          LS_DATA-HEAD-DISTR_CHAN = <L_FIELD>.
        WHEN 'DIVISION'.
          LS_DATA-HEAD-DIVISION = <L_FIELD>.
        WHEN 'SALES_OFF'.
          LS_DATA-HEAD-SALES_OFF = <L_FIELD>.
        WHEN 'SALES_GRP'.
          LS_DATA-HEAD-SALES_GRP = <L_FIELD>.
        WHEN 'PURCH_NO_C'.
          LS_DATA-HEAD-PURCH_NO_C = <L_FIELD>.
        WHEN 'CT_VALID_F'.
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_DATA-HEAD-CT_VALID_F
                                          LF_MSGTX.
        WHEN 'CT_VALID_T'.
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_DATA-HEAD-CT_VALID_T
                                          LF_MSGTX.
        WHEN 'PARTN_NUMB_AG'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_DATA-HEAD-PARTN_NUMB_AG.
        WHEN 'PARTN_NUMB_WE'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_DATA-HEAD-PARTN_NUMB_WE.
        WHEN 'PARTN_NUMB_RE'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_DATA-HEAD-PARTN_NUMB_RE.
        WHEN 'PARTN_NUMB_RG'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_DATA-HEAD-PARTN_NUMB_RG.
        WHEN 'PARTN_NUMB_VE'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_DATA-HEAD-PARTN_NUMB_VE.
        WHEN 'NAME'.
          LS_DATA-HEAD-NAME = <L_FIELD>.
        WHEN 'NAME_2'.
          LS_DATA-HEAD-NAME_2 = <L_FIELD>.
        WHEN 'NAME_3'.
          LS_DATA-HEAD-NAME_3 = <L_FIELD>.
        WHEN 'NAME_4'.
          LS_DATA-HEAD-NAME_3 = <L_FIELD>.
        WHEN 'STREET'.
          LS_DATA-HEAD-STREET = <L_FIELD>.
        WHEN 'STR_SUPPL3'.
          LS_DATA-HEAD-STR_SUPPL3 = <L_FIELD>.
        WHEN 'LOCATION'.
          LS_DATA-HEAD-LOCATION = <L_FIELD>.
        WHEN 'STR_SUPPL1'.
          LS_DATA-HEAD-STR_SUPPL1 = <L_FIELD>.
        WHEN 'STR_SUPPL2'.
          LS_DATA-HEAD-STR_SUPPL2 = <L_FIELD>.
        WHEN 'DISTRICT'.
          LS_DATA-HEAD-DISTRICT = <L_FIELD>.
        WHEN 'CITY'.
          LS_DATA-HEAD-CITY = <L_FIELD>.
        WHEN 'POSTL_COD1'.
          LS_DATA-HEAD-POSTL_COD1 = <L_FIELD>.
        WHEN 'STCD3'.
          LS_DATA-HEAD-STCD3 = <L_FIELD>.
        WHEN 'PMNTTRMS'.
          LS_DATA-HEAD-PMNTTRMS = <L_FIELD>.
        WHEN 'BNAME'.
          LS_DATA-HEAD-BNAME = <L_FIELD>.
        WHEN 'REF_1'.
          LS_DATA-HEAD-REF_1 = <L_FIELD>.
        WHEN 'USAGE'.
          LS_DATA-HEAD-USAGE = <L_FIELD>.
        WHEN 'CONDTYPE_ZDH1'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-CONDTYPE_ZDH1
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDH2'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-CONDTYPE_ZDH2
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDH3'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-CONDTYPE_ZDH3
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDH4'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-CONDTYPE_ZDH4
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDH5'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-CONDTYPE_ZDH5
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDH6'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_DATA-HEAD-CONDTYPE_ZDH6
                                            LF_MSGTX.
        WHEN 'HDTXT_ZH01'.
          LS_DATA-HEAD-HDTXT_ZH01 = <L_FIELD>.
        WHEN 'HDTXT_ZH03'.
          LS_DATA-HEAD-HDTXT_ZH03 = <L_FIELD>.
        WHEN 'HDTXT_ZH06'.
          LS_DATA-HEAD-HDTXT_ZH06 = <L_FIELD>.
        WHEN 'HDTXT_ZH10'.
          LS_DATA-HEAD-HDTXT_ZH10 = <L_FIELD>.
        WHEN 'HDTXT_ZH13'.
          LS_DATA-HEAD-HDTXT_ZH13 = <L_FIELD>.
        WHEN 'HDTXT_ZH15'.
          LS_DATA-HEAD-HDTXT_ZH15 = <L_FIELD>.
        WHEN 'HDTXT_ZH19'.
          LS_DATA-HEAD-HDTXT_ZH19 = <L_FIELD>.
        WHEN 'HDTXT_ZH09'.
          LS_DATA-HEAD-HDTXT_ZH09 = <L_FIELD>.
*---Fill data from Template Item to Item Data--------
        WHEN 'ITM_NUMBER1'.
          LS_ITEM-ITM_NUMBER1 = <L_FIELD>.
        WHEN 'HG_LV_ITEM'.
          LS_ITEM-HG_LV_ITEM = <L_FIELD>.
        WHEN 'MATERIAL'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_ITEM-MATERIAL.
        WHEN 'TARGET_QTY'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-TARGET_QTY
                                            LF_MSGTX.
        WHEN 'TARGET_QU'.
*          LS_ITEM-TARGET_QU = <L_FIELD>.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              INPUT          = <L_FIELD>
              LANGUAGE       = SY-LANGU
            IMPORTING
              OUTPUT         = LS_ITEM-TARGET_QU
            EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
          IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
          ENDIF.
        WHEN 'PLANT'.
          LS_ITEM-PLANT = <L_FIELD>.
        WHEN 'STORE_LOC'.
          LS_ITEM-STORE_LOC = <L_FIELD>.
        WHEN 'ITEM_CATE'.
          LS_ITEM-ITEM_CATE = <L_FIELD>.
        WHEN 'ORDERID'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_FIELD>
            IMPORTING
              OUTPUT = LS_ITEM-ORDERID.
        WHEN 'WBS_ELEM'.
          LS_ITEM-WBS_ELEM = <L_FIELD>.
        WHEN 'ZLOB'.
          LS_ITEM-ZLOB = <L_FIELD>.
        WHEN 'ITM_NUMBER2'.
*          LS_SCHED-QUOTATION2 = LS_DATA-HEAD-QUOTATION.
*          LS_SCHED-ITM_NUMBER2 = <L_FIELD>.

          LS_ITEM-ITM_NUMBER2 = <L_FIELD>.
        WHEN 'SCHED_LINE_1'.
*          LS_SCHED-SCHED_LINE_1 = <L_FIELD>.

          LS_ITEM-SCHED_LINE_1 = <L_FIELD>.
        WHEN 'REQ_DATE_1'.
          PERFORM F_VALIDATE_DATE  USING  <L_FIELD>
                                 CHANGING LS_ITEM-REQ_DATE_1 "LS_SCHED-REQ_DATE_1
                                          LF_MSGTX.

        WHEN 'REQ_QTY_1'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-REQ_QTY_1  "LS_SCHED-REQ_QTY_1
                                            LF_MSGTX.

        WHEN 'CONDTYPE_ZPR1'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZPR1
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDI1'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZDI1
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDI2'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZDI2
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDI3'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZDI3
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZDI4'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZDI4
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD01'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD01
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD02'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD02
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD03'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD03
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD04'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD04
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD05'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD05
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD06'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD06
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD07'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD07
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD08'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD08
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD09'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD09
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZD10'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZD10
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZF01'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZF01
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZPS1'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZPS1
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZPS2'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZPS2
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZPS3'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZPS3
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZWR1'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZWR1
                                            LF_MSGTX.
        WHEN 'CONDTYPE_ZWR2'.
          PERFORM F_VALIDATE_AMOUNT  USING <L_FIELD>
                                   CHANGING LS_ITEM-CONDTYPE_ZWR2
                                            LF_MSGTX.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
      IF LS_KEY-DOCNO IS INITIAL .
        CONTINUE.
      ENDIF.

*  Collect Data
*      LS_ITEM_DATA-ROWNO = LF_ROWNO .
*      LS_ITEM-DATA =  LS_ITEM_DATA .
      LS_ITEM-ROWNO = LF_ROWNO.

    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-ROWNO = LF_ROWNO.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.

      CONTINUE.
    ENDIF.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND  USING  LF_ROWNO
                                          LS_KEY
                                          LS_DATA-HEAD
                                          LS_ITEM
                                          LS_SCHED
                                          LS_MESSG
                                CHANGING  CT_DATA.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_TARGET_FIELD
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD USING  UF_TYPE   TYPE  CHAR20
                               UF_INDEX  TYPE  I
                      CHANGING CF_FIELD  TYPE  TS_FNAME.

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
*&---------------------------------------------------------------------*
*& Form F_CONVERT_INT_AMOUNT
*&---------------------------------------------------------------------*
*& Convert amount to internal format based on currency key
*&---------------------------------------------------------------------*
FORM F_CONVERT_INT_AMOUNT USING UF_AMOUNT_I  TYPE  ANY ##CALLED
                                UF_WAERS     TYPE  TCURC-WAERS
                       CHANGING CF_AMOUNT_O  TYPE  ANY.

  STATICS:
    LT_TCURX TYPE  SORTED TABLE OF TCURX
                      WITH UNIQUE KEY CURRKEY,
    LS_TCURX TYPE  TCURX.

  DATA:
    LF_DEC TYPE  I,
    LF_MUL TYPE  I,
    LF_DIV TYPE  I.


* Initialize Output
  CLEAR CF_AMOUNT_O.

* Get Currency Decimals
* Check Buffer
  IF UF_WAERS NE LS_TCURX-CURRKEY.
*   Read from Memory
    READ TABLE LT_TCURX INTO LS_TCURX
                        WITH KEY CURRKEY = UF_WAERS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Read from Database
      SELECT SINGLE *
        INTO LS_TCURX
        FROM TCURX
       WHERE CURRKEY EQ UF_WAERS.
      IF SY-SUBRC NE 0.
        IF UF_WAERS EQ '%'.
*         Default is 3
          CLEAR LS_TCURX.
          LS_TCURX-CURRKEY = UF_WAERS.
          LS_TCURX-CURRDEC = 3.
        ELSE.
*         Default is 2
          CLEAR LS_TCURX.
          LS_TCURX-CURRKEY = UF_WAERS.
          LS_TCURX-CURRDEC = 2.
        ENDIF.
      ENDIF.
      INSERT LS_TCURX INTO TABLE LT_TCURX.
    ENDIF.
  ENDIF.

* Get Decimal of output
  DESCRIBE FIELD CF_AMOUNT_O DECIMALS LF_DEC.

* Calculate Multiply factors
  LF_MUL = 1.
  DO LS_TCURX-CURRDEC TIMES.
    LF_MUL = LF_MUL * 10.
  ENDDO.

* Calculate Divide factors
  LF_DIV = 1.
  DO LF_DEC TIMES.
    LF_DIV = LF_DIV * 10.
  ENDDO.

* Assign Output amount
  CF_AMOUNT_O = UF_AMOUNT_I * LF_MUL / LF_DIV.


  DATA LF_AMOUNT TYPE WMTO_S-AMOUNT .
  LF_AMOUNT = CF_AMOUNT_O .
  CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
    EXPORTING
      CURRENCY        = UF_WAERS
      AMOUNT_INTERNAL = LF_AMOUNT
    IMPORTING
      AMOUNT_DISPLAY  = LF_AMOUNT
    EXCEPTIONS
      INTERNAL_ERROR  = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    CLEAR  CF_AMOUNT_O .
  ELSE.
    CF_AMOUNT_O = LF_AMOUNT .
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_AMOUNT
*&---------------------------------------------------------------------*
*& Validate Amount
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AMOUNT   USING UF_STRING TYPE  ANY
                      CHANGING CF_AMOUNT TYPE  ANY
                               CF_MSGTX  TYPE  TS_MSG_TXT.
* Initialize Output
  CLEAR: CF_AMOUNT,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

* Move to amount field
  TRY.
      CF_AMOUNT = UF_STRING.
    CATCH CX_ROOT.                                       "#EC CATCH_ALL
*     Text-e06 : Invalid amount value:
      CONCATENATE TEXT-E06 UF_STRING
             INTO CF_MSGTX
        SEPARATED BY SPACE.
      RETURN.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_COMPCODE
*&---------------------------------------------------------------------*
*& Validate Company Code
*&---------------------------------------------------------------------*

FORM F_VALIDATE_COMPCODE  USING  UF_STRING  TYPE  STRING ##CALLED
                        CHANGING CF_BUKRS   TYPE  T001-BUKRS
                                 CF_WAERS   TYPE  T001-WAERS
                                 CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_T001,
           BUKRS TYPE  T001-BUKRS,
           WAERS TYPE  T001-WAERS,
         END OF LTS_T001.
  TYPES: LTT_T001  TYPE  SORTED TABLE OF LTS_T001
                           WITH UNIQUE KEY BUKRS.

  STATICS:
    LS_T001 TYPE  LTS_T001,
    LT_T001 TYPE  LTT_T001.

  DATA:
    LF_LEN   TYPE  I,
    LF_BUKRS TYPE  LTS_T001-BUKRS.


* Initialize Output
  CLEAR: CF_BUKRS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 4.
*   Text-e04 : Invalid Company code:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_BUKRS = UF_STRING.

* Check Buffer
  IF LS_T001-BUKRS NE LF_BUKRS.
*   Validate with Memory
    READ TABLE LT_T001 INTO LS_T001
                        WITH KEY BUKRS = LF_BUKRS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T001.
*     Validate with Database
      SELECT SINGLE BUKRS WAERS
        INTO LS_T001
        FROM T001
       WHERE BUKRS EQ LF_BUKRS.
      IF SY-SUBRC NE 0.
*       Text-e04 : Invalid Company code:
        CONCATENATE TEXT-E04 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T001 INTO TABLE LT_T001.
    ENDIF.

  ENDIF.

* Assign Output
  CF_BUKRS = LS_T001-BUKRS.
  CF_WAERS = LS_T001-WAERS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_COSTCTR
*&---------------------------------------------------------------------*
*& Validate Cost Center
*&---------------------------------------------------------------------*
FORM F_VALIDATE_COSTCTR  USING  UF_STRING TYPE  STRING ##CALLED
                                UF_BUKRS  TYPE  T001-BUKRS
                       CHANGING CF_KOSTL  TYPE  CSKS-KOSTL
                                CF_MSGTX  TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_CSKS,
           BUKRS TYPE  T001-BUKRS,
           KOSTL TYPE  CSKS-KOSTL,
         END OF LTS_CSKS.
  TYPES: LTT_CSKS  TYPE  SORTED TABLE OF LTS_CSKS
                          WITH UNIQUE KEY BUKRS
                                          KOSTL.

  STATICS:
    LT_CSKS TYPE  LTT_CSKS,
    LS_CSKS TYPE  LTS_CSKS.

  DATA:
    LF_KOSTL  TYPE  CSKS-KOSTL,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_KOSTL,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e12 : Invalid Cost Center:
    CONCATENATE TEXT-E12 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_KOSTL.

* Check Buffer
  IF LS_CSKS-BUKRS NE UF_BUKRS OR
     LS_CSKS-KOSTL NE LF_KOSTL.
*   Validate with Memory
    READ TABLE LT_CSKS INTO LS_CSKS
                       WITH KEY BUKRS = UF_BUKRS
                                KOSTL = LF_KOSTL
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_CSKS.
*     Validate with Database
      SELECT TKA02~BUKRS CSKS~KOSTL                  "#EC CI_SEL_NESTED
          UP TO 1 ROWS
        INTO LS_CSKS
        FROM TKA02
             INNER JOIN CSKS                           "#EC CI_BUFFJOIN
               ON  CSKS~KOKRS = TKA02~KOKRS
       WHERE TKA02~BUKRS  EQ  UF_BUKRS
         AND TKA02~GSBER  EQ  SPACE
         AND CSKS~KOSTL   EQ  LF_KOSTL
         AND CSKS~DATBI   GE  SY-DATUM
         AND CSKS~DATAB   LE  SY-DATUM
       ORDER BY TKA02~BUKRS ASCENDING
                CSKS~KOSTL  ASCENDING.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e12 : Invalid Cost Center:
        CONCATENATE TEXT-E12 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_CSKS INTO TABLE LT_CSKS.
    ENDIF.

  ENDIF.

* Assign Output
  CF_KOSTL = LS_CSKS-KOSTL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_CURENCY
*&---------------------------------------------------------------------*
*& Validate Currency Key
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CURENCY  USING  UF_STRING  TYPE  STRING ##CALLED
                       CHANGING CF_WAERS   TYPE  WAERS
                                CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TCURC,
           WAERS TYPE  TCURC-WAERS,
         END OF LTS_TCURC.
  TYPES: LTT_TCURC  TYPE  SORTED TABLE OF LTS_TCURC
                           WITH UNIQUE KEY WAERS.

  STATICS:
    LT_TCURC TYPE  LTT_TCURC,
    LS_TCURC TYPE  LTS_TCURC.

  DATA:
    LF_LENGTH TYPE  I,
    LF_WAERS  TYPE  LTS_TCURC-WAERS.


* Initialize Output
  CLEAR: CF_WAERS,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 5.
*   Text-e05 : Invalid currency:
    CONCATENATE TEXT-E05 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_WAERS = UF_STRING.

* Check %, no more validation
  IF LF_WAERS EQ '%'.
    CF_WAERS = LF_WAERS.
    RETURN.
  ENDIF.

* Check Buffer
  IF LF_WAERS NE LS_TCURC-WAERS.
*   Read from Memory
    READ TABLE LT_TCURC INTO LS_TCURC
                        WITH KEY WAERS = LF_WAERS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TCURC.
*     Read from Database
      SELECT SINGLE WAERS
        INTO LS_TCURC-WAERS
        FROM TCURC
       WHERE WAERS EQ LF_WAERS.
      IF SY-SUBRC NE 0.
*       Text-e05 : Invalid currency:
        CONCATENATE TEXT-E05 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TCURC INTO TABLE LT_TCURC.
    ENDIF.
  ENDIF.

* Assign Output
  CF_WAERS = LS_TCURC-WAERS.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_CUSTOMER
*&---------------------------------------------------------------------*
*& Validate Customer Number
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CUSTOMER  USING  UF_STRING  TYPE STRING ##CALLED
                        CHANGING CF_KUNNR   TYPE KNA1-KUNNR
                                 CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_KNA1,
           KUNNR TYPE  KNA1-KUNNR,
         END OF LTS_KNA1.
  TYPES: LTT_KNA1  TYPE  SORTED TABLE OF LTS_KNA1
                           WITH UNIQUE KEY KUNNR.

  STATICS:
    LS_KNA1 TYPE  LTS_KNA1,
    LT_KNA1 TYPE  LTT_KNA1.

  DATA:
    LF_LEN   TYPE  I,
    LF_KUNNR TYPE  LTS_KNA1-KUNNR.


* Initialize Output
  CLEAR: CF_KUNNR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 10.
*   Text-e15 : Invalid
    CONCATENATE TEXT-E15 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_KUNNR.

* Check Buffer
  IF LS_KNA1-KUNNR NE LF_KUNNR.

*   Validate with Memory
    READ TABLE LT_KNA1 INTO LS_KNA1
                        WITH KEY KUNNR = LF_KUNNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_KNA1.
*     Validate with Database
      SELECT SINGLE KUNNR
        INTO LS_KNA1
        FROM KNA1
       WHERE KUNNR  EQ LF_KUNNR.                     "#EC CI_SEL_NESTED
      IF SY-SUBRC NE 0.
*       Text-e08 : Invalid Customer no.:
        CONCATENATE TEXT-E08 UF_STRING
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
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DATE
*&---------------------------------------------------------------------*
*& Validate Date
*&---------------------------------------------------------------------*
FORM F_VALIDATE_DATE  USING  UF_STRING  TYPE  ANY
                    CHANGING CF_DATUM   TYPE  SY-DATUM
                             CF_MSGTX   TYPE  TS_MSG_TXT.
  CONSTANTS :
    LC_01(2)   TYPE N VALUE 01,
    LC_31(2)   TYPE N VALUE 31,
    LC_12(2)   TYPE N VALUE 12,
    LC_1900(4) TYPE N VALUE 1900,
    LC_2200(4) TYPE N VALUE 2200.

  DATA:
    LF_LENGTH  TYPE  I.


* Initialize Output
  CLEAR: CF_DATUM,
         CF_MSGTX.

* Not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Only number and .?
  IF NOT UF_STRING CO '0123456789.'.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Length?
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH NE 10.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* First 2 Digit is date
  IF NOT UF_STRING(2) BETWEEN LC_01 AND LC_31.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 4-5th Digit is month
  IF NOT UF_STRING+3(2) BETWEEN LC_01 AND LC_12.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 7-8th digit is year
  IF NOT UF_STRING+6(4) BETWEEN LC_1900 AND LC_2200 AND
     UF_STRING+6(4) NE '9999'.
*   Text-e02 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E02 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  CONCATENATE UF_STRING+6(4) UF_STRING+3(2) UF_STRING(2)
         INTO CF_DATUM.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_doctype
*----------------------------------------------------------------------*
*  Validate Document Type
*----------------------------------------------------------------------*
FORM F_VALIDATE_DOCTYPE  USING  UF_STRING TYPE  STRING ##CALLED
                       CHANGING CF_AUART  TYPE  VBAK-AUART
                                CF_MSGTX  TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVAK,
           AUART TYPE  TVAK-AUART,
         END OF LTS_TVAK.
  TYPES: LTT_TVAK  TYPE  SORTED TABLE OF LTS_TVAK
                           WITH UNIQUE KEY AUART.

  STATICS:
    LS_TVAK TYPE  LTS_TVAK,
    LT_TVAK TYPE  LTT_TVAK.

  DATA:
    LF_AUART TYPE  LTS_TVAK-AUART.


* Initialize Output
  CLEAR: CF_AUART,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  IF STRLEN( UF_STRING ) GT 4.
*   Text-e03 : Invalid Document type:
    CONCATENATE TEXT-E03 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_AUART = UF_STRING.

* Check Buffer
  IF LS_TVAK-AUART NE LF_AUART.
*   Validate with Memory
    READ TABLE LT_TVAK INTO LS_TVAK
                        WITH KEY AUART = LF_AUART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVAK.
*     Validate with Database
      SELECT SINGLE AUART
        INTO LS_TVAK
        FROM TVAK
       WHERE AUART EQ LF_AUART.
      IF SY-SUBRC NE 0.
*       Text-e03 : Invalid Document type:
        CONCATENATE TEXT-E03 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVAK INTO TABLE LT_TVAK.
    ENDIF.

  ENDIF.

* Assign Output
  CF_AUART = LS_TVAK-AUART.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_plant
*----------------------------------------------------------------------*
*       Validate Plant
*----------------------------------------------------------------------*
FORM F_VALIDATE_PLANT  USING  UF_STRING TYPE STRING ##CALLED
                     CHANGING CF_WERKS  TYPE T001W-WERKS
                              CF_MSGTX  TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_WERKS,
           WERKS TYPE  T001W-WERKS,
         END OF LTS_WERKS.
  TYPES: LTT_WERKS  TYPE  SORTED TABLE OF LTS_WERKS
                           WITH UNIQUE KEY WERKS.

  STATICS:
    LS_WERKS TYPE  LTS_WERKS,
    LT_WERKS TYPE  LTT_WERKS.

  DATA:
    LF_LEN   TYPE  I,
    LF_WERKS TYPE  LTS_WERKS-WERKS.


* Initialize Output
  CLEAR: CF_WERKS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  LF_LEN = STRLEN(  UF_STRING ).
  IF LF_LEN NE 4.
*   Text-e17 : Invalid plant:
    CONCATENATE TEXT-E17 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_WERKS = UF_STRING.

* Check Buffer
  IF LS_WERKS-WERKS NE LF_WERKS.
*   Validate with Memory
    READ TABLE LT_WERKS INTO LS_WERKS
                        WITH KEY WERKS = LF_WERKS
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_WERKS.
*     Validate with Database
      SELECT SINGLE T001W~WERKS
        INTO LS_WERKS-WERKS
        FROM T001W
       WHERE T001W~WERKS = LF_WERKS.
      IF SY-SUBRC NE 0.
*       Text-e17 : Invalid plant:
        CONCATENATE TEXT-E17 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_WERKS INTO TABLE LT_WERKS.
    ENDIF.

  ENDIF.

* Assign Output
  CF_WERKS = LS_WERKS-WERKS.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_matnr
*----------------------------------------------------------------------*
*  Validate Material Number
*----------------------------------------------------------------------*
FORM F_VALIDATE_MATNR  USING  UF_STRING  TYPE  STRING ##CALLED
                     CHANGING CF_MATNR   TYPE  MARA-MATNR
                              CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_MARA,
           MATNR TYPE  MARA-MATNR,
         END OF LTS_MARA.
  TYPES: LTT_MARA  TYPE  SORTED TABLE OF LTS_MARA
                          WITH UNIQUE KEY MATNR.

  STATICS:
    LT_MARA TYPE  LTT_MARA,
    LS_MARA TYPE  LTS_MARA.

  DATA:
    LF_MATNR  TYPE  MARA-MATNR.


* Initialize Output
  CLEAR: CF_MATNR,
         CF_MSGTX.

* Only when not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = UF_STRING
    IMPORTING
      OUTPUT       = LF_MATNR
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            INTO CF_MSGTX.
    RETURN.
  ENDIF.

* Check Buffer
  IF LS_MARA-MATNR NE LF_MATNR.
*   Validate with Memory
    READ TABLE LT_MARA INTO LS_MARA
                       WITH KEY MATNR = LF_MATNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_MARA.
*     Validate with Database
      SELECT SINGLE MATNR                            "#EC CI_SEL_NESTED
        INTO LS_MARA
        FROM MARA
       WHERE MATNR  EQ  LF_MATNR.
      IF SY-SUBRC NE 0.
*       Text-e18 : Invalid Material number:
        CONCATENATE TEXT-E18 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_MARA INTO TABLE LT_MARA.
    ENDIF.

  ENDIF.

* Assign Output
  CF_MATNR = LF_MATNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_and_append
*----------------------------------------------------------------------*
*  Validate Whole Line data and collect into internal table for posting
*----------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND  USING  UF_ROWNO   TYPE TS_RESULT-ROWNO
                                   US_KEY     TYPE TS_KEY
                                   US_HEAD    TYPE TS_HEAD
*                                   US_ITEM    TYPE TS_ITEM_DATA
                                   US_ITEM    TYPE TS_ITEM
                                   US_SCHED   TYPE TS_SCHED
                                   US_MESSG   TYPE TS_MESSG
                          CHANGING CT_DATA    TYPE TT_DATA.

  DATA:
    LT_MESSG TYPE  TT_MESSG.

  DATA:
    LS_DATA  TYPE  TS_DATA,
    LS_ITEM  TYPE  TS_ITEM,
*    LS_SCHED TYPE  TS_SCHED,
    LS_MESSG TYPE  TS_MESSG.

  FIELD-SYMBOLS:
    <L_DATA>  TYPE  TS_DATA.
*    <L_ITEM>  TYPE  TS_ITEM,
*    <L_SCHED> TYPE  TS_SCHED.

* Read Key
  READ TABLE CT_DATA ASSIGNING <L_DATA>
                     WITH KEY KEY = US_KEY.
  IF SY-SUBRC NE 0.
*   Insert New Key
    CLEAR LS_DATA.
    LS_DATA-KEY  = US_KEY.
    INSERT LS_DATA INTO TABLE CT_DATA ASSIGNING <L_DATA>.

*   Validate Header
    PERFORM F_VALIDATE_HEAD  USING  US_HEAD
*                                    US_HEADX
                                    UF_ROWNO
                           CHANGING LS_MESSG.
    IF LS_MESSG IS NOT INITIAL.
      INSERT LS_MESSG INTO TABLE LT_MESSG.
    ENDIF.
    <L_DATA>-HEAD  = US_HEAD.
*    <L_DATA>-HEADX = US_HEADX.
  ENDIF.

  IF US_HEAD IS NOT INITIAL AND
     US_HEAD NE <L_DATA>-HEAD.
*   Header data conflict
    CLEAR: LS_MESSG.
    LS_MESSG-ROWNO = UF_ROWNO.
    LS_MESSG-MSGTY = 'E'.
    LS_MESSG-MSGID = 'ZSDSPS01'.
    LS_MESSG-MSGNO = '000'.
*   Text-e27 : Header data conflict within document.
    LS_MESSG-MSGTX = TEXT-E27.
    INSERT LS_MESSG INTO TABLE LT_MESSG.
  ENDIF.

  PERFORM F_VALIDATE_ITEM  USING  <L_DATA>-HEAD
                                  US_ITEM
                                  US_SCHED
                                  UF_ROWNO
                         CHANGING LS_ITEM
                                  LS_MESSG.



  IF LS_MESSG IS NOT INITIAL.
    INSERT LS_MESSG INTO TABLE LT_MESSG.
  ENDIF.

  INSERT LS_ITEM INTO TABLE <L_DATA>-ITEM.

*  LS_SCHED = US_SCHED.
*  READ TABLE LS_ITEM-SCHED ASSIGNING <L_SCHED>
*                            WITH KEY QUOTATION2  = US_HEAD-QUOTATION
*                                     ITM_NUMBER2 = US_ITEM-ITM_NUMBER1.
**  INSERT LS_SCHED INTO TABLE LS_DATA-ITEM-SCHED.
*
*  READ TABLE <L_DATA>-ITEM ASSIGNING <L_ITEM>
*                            WITH KEY ITM_NUMBER1 = US_ITEM-ITM_NUMBER1.
*  IF SY-SUBRC NE 0.
*    INSERT LS_ITEM INTO TABLE <L_DATA>-ITEM.
*  ENDIF.


  IF LT_MESSG IS NOT INITIAL.
    APPEND LINES OF LT_MESSG TO <L_DATA>-MESSG.
  ENDIF.

  IF US_MESSG IS NOT INITIAL.
    APPEND US_MESSG TO <L_DATA>-MESSG.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_head
*----------------------------------------------------------------------*
*  Validate Header data
*----------------------------------------------------------------------*
FORM F_VALIDATE_HEAD  USING  US_HEAD   TYPE  TS_HEAD
*                             US_HEADX  TYPE  TS_HEADX   ##NEEDED
                             UF_ROWNO  TYPE  TS_RESULT-ROWNO
                    CHANGING CS_MESSG  TYPE  TS_MESSG.


* Initialize Output
  CLEAR: CS_MESSG.
  IF US_HEAD-PMNTTRMS IS NOT INITIAL.
    SELECT ZTAG1
      FROM T052
      INTO @DATA(LV_SO_DAYS)
      UP TO 1 ROWS
      WHERE ZTERM EQ @US_HEAD-PMNTTRMS
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      CLEAR: LV_SO_DAYS.
    ENDIF.

*    READ TABLE GT_VTWEG_MAP INTO DATA(LS_VTWEG_MAP) WITH KEY VTWEG = IS_VBAK-VTWEG.
*    IF SY-SUBRC = 0.
*      LF_VTWEG = LS_VTWEG_MAP-VTWEG_SEL.
*    ELSE.
*      LF_VTWEG = IS_VBAK-VTWEG.
*    ENDIF.
*   Get payment term from customer master
    SELECT SINGLE ZTERM
      FROM KNVV
      INTO @DATA(LV_MASTER_ZTERM)
      WHERE KUNNR EQ @US_HEAD-PARTN_NUMB_AG
        AND VKORG EQ @US_HEAD-SALES_ORG
        AND VTWEG EQ @US_HEAD-DISTR_CHAN
        AND SPART EQ @US_HEAD-DIVISION.

* Get days from baseline date from payment term key
    IF LV_MASTER_ZTERM IS NOT INITIAL.
      SELECT ZTAG1
        FROM T052
        INTO @DATA(LV_MASTER_DAYS)
        UP TO 1 ROWS
        WHERE ZTERM EQ @LV_MASTER_ZTERM
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        CLEAR: LV_MASTER_DAYS.
      ENDIF.
    ENDIF.
* -----------------------
* Compare payment term in sales document with master data
* -----------------------
    IF US_HEAD-PMNTTRMS NE LV_MASTER_ZTERM.
      IF LV_SO_DAYS > LV_MASTER_DAYS.
*     The term of payment is more than term of payment in master data
        CS_MESSG-ROWNO = UF_ROWNO.
        CS_MESSG-MSGTY = 'E'.
        CS_MESSG-MSGID = 'ZSDSSD01'.
        CS_MESSG-MSGNO = '001'.
*   Text-e20 : Missing Document Date.
        CS_MESSG-MSGTX = TEXT-E50.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.
*
*  IF US_HEAD-BLDAT IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e20 : Missing Document Date.
*    CS_MESSG-MSGTX = TEXT-E20.
*    RETURN.
*  ENDIF.
*
*  IF US_HEAD-BUDAT IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e21 : Missing Posting Date.
*    CS_MESSG-MSGTX = TEXT-E21.
*    RETURN.
*  ENDIF.
*
*  IF US_HEAD-BLART IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e23 : Missing Document Type.
*    CS_MESSG-MSGTX = TEXT-E23.
*    RETURN.
*  ENDIF.
*
*  IF US_HEAD-BUKRS IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e24 : Missing Company Code.
*    CS_MESSG-MSGTX = TEXT-E24.
*    RETURN.
*
*
*  ENDIF.
*
*  IF US_HEAD-WAERS IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e25 : Missing Currency.
*    CS_MESSG-MSGTX = TEXT-E25.
*    RETURN.
*  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_get_field_desc
*----------------------------------------------------------------------*
*  Get Field Description
*----------------------------------------------------------------------*
FORM F_GET_FIELD_DESC  USING  UF_TABNM  TYPE  TMODU-TABNM ##CALLED
                              UF_FELDN  TYPE  TMODU-FELDN
                     CHANGING CF_TEXT   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_DD03VT,
           TABNAME   TYPE  DD03VT-TABNAME,
           FIELDNAME TYPE  DD03VT-FIELDNAME,
           TEXT      TYPE  DD03VT-SCRTEXT_M,
         END OF LTS_DD03VT.
  TYPES: LTT_DD03VT TYPE SORTED TABLE OF LTS_DD03VT
                          WITH UNIQUE KEY TABNAME
                                          FIELDNAME.

  STATICS:
    LT_DD03VT TYPE  LTT_DD03VT,
    LS_DD03VT TYPE  LTS_DD03VT.

  DATA:
    LF_TABNAME   TYPE  LTS_DD03VT-TABNAME,
    LF_FIELDNAME TYPE  LTS_DD03VT-FIELDNAME.


* Initialize Output
  CLEAR: CF_TEXT.

  LF_TABNAME   = UF_TABNM.
  LF_FIELDNAME = UF_FELDN.

  IF LS_DD03VT-TABNAME NE LF_TABNAME OR
     LS_DD03VT-FIELDNAME NE LF_FIELDNAME.
    READ TABLE LT_DD03VT INTO LS_DD03VT
                         WITH KEY TABNAME = LF_TABNAME
                                  FIELDNAME = LF_FIELDNAME
                         BINARY SEARCH.
    IF SY-SUBRC NE 0.
      SELECT TABNAME FIELDNAME SCRTEXT_M
          UP TO 1 ROWS
        INTO LS_DD03VT
        FROM DD03VT
       WHERE TABNAME    EQ  LF_TABNAME
         AND FIELDNAME  EQ  LF_FIELDNAME
         AND DDLANGUAGE EQ  SY-LANGU
       ORDER BY TABNAME FIELDNAME SCRTEXT_M.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        RETURN.
      ENDIF.
      INSERT LS_DD03VT INTO TABLE LT_DD03VT.
    ENDIF.
  ENDIF.

* Assign Output
  CF_TEXT = LS_DD03VT-TEXT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_glaccount
*----------------------------------------------------------------------*
*  Validate GL Account
*----------------------------------------------------------------------*
FORM F_VALIDATE_GLACCOUNT  USING  UF_STRING  TYPE STRING ##CALLED
                                  UF_BUKRS   TYPE T001-BUKRS
                         CHANGING CF_SAKNR   TYPE SKA1-SAKNR
                                  CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_SKA1,
           BUKRS TYPE  T001-BUKRS,
           SAKNR TYPE  SKA1-SAKNR,
         END OF LTS_SKA1.
  TYPES: LTT_SKA1  TYPE  SORTED TABLE OF LTS_SKA1
                           WITH UNIQUE KEY BUKRS
                                           SAKNR.

  STATICS:
    LS_SKA1 TYPE  LTS_SKA1,
    LT_SKA1 TYPE  LTT_SKA1.

  DATA:
    LF_LEN   TYPE  I,
    LF_SAKNR TYPE  LTS_SKA1-SAKNR.


* Initialize Output
  CLEAR: CF_SAKNR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 10
  LF_LEN = STRLEN( UF_STRING ).
  IF LF_LEN GT 10.
*   Text-e09 : Invalid GL Account:
    CONCATENATE TEXT-E09 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_SAKNR.

* Check Buffer
  IF LS_SKA1-BUKRS NE UF_BUKRS OR
     LS_SKA1-SAKNR NE LF_SAKNR.

*   Validate with Memory
    READ TABLE LT_SKA1 INTO LS_SKA1
                        WITH KEY BUKRS = UF_BUKRS
                                 SAKNR = LF_SAKNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_SKA1.
*     Validate with Database
      SELECT T001~BUKRS SKA1~SAKNR
          UP TO 1 ROWS
        INTO LS_SKA1                   "#EC CI_DB_OPERATION_OK[2431747]
        FROM T001                      "#EC CI_DB_OPERATION_OK[2389136]
               INNER JOIN SKA1                         "#EC CI_BUFFJOIN
                 ON  SKA1~KTOPL = T001~KTOPL
       WHERE T001~BUKRS  EQ UF_BUKRS
         AND SKA1~SAKNR  EQ LF_SAKNR
       ORDER BY T001~BUKRS ASCENDING
                SKA1~SAKNR ASCENDING.                "#EC CI_SEL_NESTED
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e09 : Invalid GL Account:
        CONCATENATE TEXT-E09 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_SKA1 INTO TABLE LT_SKA1.
    ENDIF.

  ENDIF.

* Assign Output
  CF_SAKNR = LS_SKA1-SAKNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_item
*----------------------------------------------------------------------*
*  Validate Item data
*----------------------------------------------------------------------*
FORM F_VALIDATE_ITEM  USING  US_HEAD  TYPE  TS_HEAD  ##NEEDED
*                             US_ITEM  TYPE  TS_ITEM_DATA
                             US_ITEM  TYPE  TS_ITEM  ##NEEDED
                             US_SCHED TYPE  TS_SCHED ##NEEDED
                             UF_ROWNO TYPE  TS_RESULT-ROWNO  ##NEEDED
                    CHANGING CS_ITEM_O  TYPE  TS_ITEM
                             CS_MESSG   TYPE  TS_MESSG.
*
*  DATA:
*    LF_LIFNR TYPE  LFBW-LIFNR,
*    LF_KUNNR TYPE  KNA1-KUNNR.


* Initialize Output
  CLEAR: CS_ITEM_O,
         CS_MESSG.

* Assign Item data
  CS_ITEM_O = US_ITEM.
*  CS_ITEM_O-DATAX = US_ITEMX.

*  IF US_ITEM-NEWBS IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e28 : Missing Posting Key.
*    CS_MESSG-MSGTX = TEXT-E28.
*    RETURN.
*  ENDIF.
*
*  IF US_ITEM-NEWKO IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e29 : Missing GL Account/Vendor/Customer.
*    CS_MESSG-MSGTX = TEXT-E29.
*    RETURN.
*  ENDIF.
*
*  IF US_ITEM-WRBTR IS INITIAL.
*    CS_MESSG-ROWNO = UF_ROWNO.
*    CS_MESSG-MSGTY = 'E'.
*    CS_MESSG-MSGID = 'ZSDSPS01'.
*    CS_MESSG-MSGNO = '000'.
**   Text-e31 : Missing Amount in DC.
*    CS_MESSG-MSGTX = TEXT-E31.
*    RETURN.
*  ENDIF.
*
** -------------------
** For Vendor
** -------------------
*  IF US_ITEM-KOART EQ GC_KOART_VEND.
*    LF_LIFNR = US_ITEM-NEWKO.
**   Get Withholding tax type for Vendor
*    SELECT WITHT
*      INTO TABLE CS_ITEM_O-WITHT
*      FROM LFBW
*     WHERE LIFNR EQ LF_LIFNR
*       AND BUKRS EQ US_HEAD-BUKRS
*     ORDER BY PRIMARY KEY.                           "#EC CI_SEL_NESTED
*      IF SY-SUBRC NE 0.
*        CLEAR: CS_ITEM_O-WITHT.
*      ENDIF.
**   Get Reconcile account
*      SELECT SINGLE AKONT
*        INTO CS_ITEM_O-DATA-AKONT
*        FROM LFB1
*       WHERE LIFNR EQ LF_LIFNR
*         AND BUKRS EQ US_HEAD-BUKRS.                 "#EC CI_SEL_NESTED
*        IF SY-SUBRC NE 0.
*          CS_MESSG-ROWNO = UF_ROWNO.
*          CS_MESSG-MSGTY = 'E'.
*          CS_MESSG-MSGID = 'ZSDSPS01'.
*          CS_MESSG-MSGNO = '000'.
**     Text-e34 : Vendor is not valid for company.
*          CS_MESSG-MSGTX = TEXT-E34.
*          RETURN.
*        ENDIF.
** -------------------
** For Customer
** -------------------
*      ELSEIF US_ITEM-KOART EQ GC_KOART_CUST.
*        LF_KUNNR = US_ITEM-NEWKO.
**   Get Withholding tax type for Customer
*        SELECT WITHT
*          INTO TABLE CS_ITEM_O-WITHT
*          FROM KNBW
*         WHERE KUNNR EQ LF_KUNNR
*           AND BUKRS EQ US_HEAD-BUKRS
*         ORDER BY PRIMARY KEY.                       "#EC CI_SEL_NESTED
*          IF SY-SUBRC NE 0.
*            CLEAR: CS_ITEM_O-WITHT.
*          ENDIF.
**   Get Reconcile account
*          SELECT SINGLE AKONT
*            INTO CS_ITEM_O-DATA-AKONT
*            FROM KNB1
*           WHERE KUNNR EQ LF_KUNNR
*             AND BUKRS EQ US_HEAD-BUKRS.             "#EC CI_SEL_NESTED
*            IF SY-SUBRC NE 0.
*              CS_MESSG-ROWNO = UF_ROWNO.
*              CS_MESSG-MSGTY = 'E'.
*              CS_MESSG-MSGID = 'ZSDSPS01'.
*              CS_MESSG-MSGNO = '000'.
**     Text-e35 : Customer is not valid for company.
*              CS_MESSG-MSGTX = TEXT-E35.
*              RETURN.
*            ENDIF.
*          ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_order
*----------------------------------------------------------------------*
*  Validate Order
*----------------------------------------------------------------------*
FORM F_VALIDATE_ORDER  USING  UF_STRING  TYPE STRING ##CALLED
                     CHANGING CF_AUFNR   TYPE AUFK-AUFNR
                              CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_AUFK,
           AUFNR TYPE  AUFK-AUFNR,
         END OF LTS_AUFK.
  TYPES: LTT_AUFK  TYPE  SORTED TABLE OF LTS_AUFK
                          WITH UNIQUE KEY AUFNR.

  STATICS:
    LT_AUFK TYPE  LTT_AUFK,
    LS_AUFK TYPE  LTS_AUFK.

  DATA:
    LF_AUFNR  TYPE  AUFK-AUFNR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_AUFNR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e13 : Invalid Order:
    CONCATENATE TEXT-E13 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_AUFNR.

* Check Buffer
  IF LS_AUFK-AUFNR NE LF_AUFNR.
*   Validate with Memory
    READ TABLE LT_AUFK INTO LS_AUFK
                       WITH KEY AUFNR = LF_AUFNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_AUFK.
*     Validate with Database
      SELECT SINGLE AUFNR                            "#EC CI_SEL_NESTED
        INTO LS_AUFK
        FROM AUFK
       WHERE AUFNR  EQ  LF_AUFNR.
      IF SY-SUBRC NE 0.
*       Text-e13 : Invalid Order:
        CONCATENATE TEXT-E13 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_AUFK INTO TABLE LT_AUFK.
    ENDIF.

  ENDIF.

* Assign Output
  CF_AUFNR = LS_AUFK-AUFNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_postingkey
*----------------------------------------------------------------------*
*  Validate Posting Key
*----------------------------------------------------------------------*
FORM F_VALIDATE_POSTINGKEY  USING  UF_STRING  TYPE  STRING ##CALLED
                          CHANGING CF_BSCHL   TYPE  TBSL-BSCHL
                                   CF_KOART   TYPE  TBSL-KOART
                                   CF_SHKZG   TYPE  TBSL-SHKZG
                                   CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TBSL,
           BSCHL TYPE  TBSL-BSCHL,
           KOART TYPE  TBSL-KOART,
           SHKZG TYPE  TBSL-SHKZG,
         END OF LTS_TBSL.
  TYPES: LTT_TBSL  TYPE  SORTED TABLE OF LTS_TBSL
                           WITH UNIQUE KEY BSCHL.

  STATICS:
    LT_TBSL TYPE  LTT_TBSL,
    LS_TBSL TYPE  LTS_TBSL.

  DATA:
    LF_LENGTH TYPE  I,
    LF_BSCHL  TYPE  LTS_TBSL-BSCHL.


* Initialize Output
  CLEAR: CF_BSCHL,
         CF_KOART,
         CF_SHKZG,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 5.
*   Text-e07 : Invalid Posting Key:
    CONCATENATE TEXT-E07 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_BSCHL = UF_STRING.

* Check %, no more validation
  IF LF_BSCHL EQ '%'.
    CF_BSCHL = LF_BSCHL.
    RETURN.
  ENDIF.

* Check Buffer
  IF LF_BSCHL NE LS_TBSL-BSCHL.
*   Read from Memory
    READ TABLE LT_TBSL INTO LS_TBSL
                        WITH KEY BSCHL = LF_BSCHL
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TBSL.
*     Read from Database
      SELECT SINGLE BSCHL KOART SHKZG
        INTO LS_TBSL
        FROM TBSL
       WHERE BSCHL EQ LF_BSCHL.
      IF SY-SUBRC NE 0.
*       Text-e07 : Invalid Posting Key:
        CONCATENATE TEXT-E07 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TBSL INTO TABLE LT_TBSL.
    ENDIF.
  ENDIF.

* Assign Output
  CF_BSCHL = LS_TBSL-BSCHL.
  CF_KOART = LS_TBSL-KOART.
  CF_SHKZG = LS_TBSL-SHKZG.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_profitctr
*----------------------------------------------------------------------*
*  Validate Profit Center
*----------------------------------------------------------------------*
FORM F_VALIDATE_PROFITCTR  USING  UF_STRING TYPE  STRING ##CALLED
                                  UF_BUKRS  TYPE  T001-BUKRS
                         CHANGING CF_PRCTR  TYPE  CEPC-PRCTR
                                  CF_MSGTX  TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_CEPC,
           BUKRS TYPE  T001-BUKRS,
           PRCTR TYPE  CEPC-PRCTR,
         END OF LTS_CEPC.
  TYPES: LTT_CEPC  TYPE  SORTED TABLE OF LTS_CEPC
                         WITH UNIQUE KEY BUKRS
                                         PRCTR.

  STATICS:
    LS_CEPC TYPE  LTS_CEPC,
    LT_CEPC TYPE  LTT_CEPC.

  DATA:
    LF_PRCTR  TYPE  CEPC-PRCTR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_PRCTR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e11 : Invalid Profit Center:
    CONCATENATE TEXT-E11 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_PRCTR.

* Check Buffer
  IF LS_CEPC-BUKRS NE UF_BUKRS OR
     LS_CEPC-PRCTR NE LF_PRCTR.
*   Validate with Memory
    READ TABLE LT_CEPC INTO LS_CEPC
                       WITH KEY BUKRS = UF_BUKRS
                                PRCTR = LF_PRCTR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_CEPC.
*     Validate with Database
      SELECT TKA02~BUKRS CEPC~PRCTR                  "#EC CI_SEL_NESTED
          UP TO 1 ROWS
        INTO LS_CEPC
        FROM TKA02
             INNER JOIN CEPC                           "#EC CI_BUFFJOIN
               ON  CEPC~KOKRS = TKA02~KOKRS
       WHERE TKA02~BUKRS  EQ  UF_BUKRS
         AND TKA02~GSBER  EQ  SPACE
         AND CEPC~PRCTR   EQ  LF_PRCTR
         AND CEPC~DATBI   GE  SY-DATUM
         AND CEPC~DATAB   LE  SY-DATUM
       ORDER BY TKA02~BUKRS ASCENDING
                CEPC~PRCTR  ASCENDING.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e11 : Invalid Profit Center:
        CONCATENATE TEXT-E11 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_CEPC INTO TABLE LT_CEPC.
    ENDIF.

  ENDIF.

* Assign Output
  CF_PRCTR = LS_CEPC-PRCTR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_vendor
*----------------------------------------------------------------------*
*  Validate Vendor
*----------------------------------------------------------------------*
FORM F_VALIDATE_VENDOR  USING  UF_STRING  TYPE  STRING ##CALLED
                      CHANGING CF_LIFNR   TYPE  LFA1-LIFNR
                               CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_LFA1,
           LIFNR TYPE  LFA1-LIFNR,
         END OF LTS_LFA1.
  TYPES: LTT_LFA1  TYPE  SORTED TABLE OF LTS_LFA1
                          WITH UNIQUE KEY LIFNR.

  STATICS:
    LT_LFA1 TYPE  LTT_LFA1,
    LS_LFA1 TYPE  LTS_LFA1.

  DATA:
    LF_LIFNR  TYPE  LFA1-LIFNR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_LIFNR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e10 : Invalid Vendor:
    CONCATENATE TEXT-E10 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_LIFNR.

* Check Buffer
  IF LS_LFA1-LIFNR NE LF_LIFNR.
*   Validate with Memory
    READ TABLE LT_LFA1 INTO LS_LFA1
                       WITH KEY LIFNR = LF_LIFNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_LFA1.
*     Validate with Database
      SELECT SINGLE LIFNR                            "#EC CI_SEL_NESTED
        INTO LS_LFA1
        FROM LFA1
       WHERE LIFNR  EQ  LF_LIFNR.
      IF SY-SUBRC NE 0.
*       Text-e10 : Invalid Vendor:
        CONCATENATE TEXT-E10 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_LFA1 INTO TABLE LT_LFA1.
    ENDIF.

  ENDIF.

* Assign Output
  CF_LIFNR = LS_LFA1-LIFNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_collect_result
*&---------------------------------------------------------------------*
*& Collect Result data
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  US_DATA   TYPE TS_DATA
                     CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.

  FIELD-SYMBOLS:
    <L_MESSG>  TYPE  TS_MESSG.


  CLEAR: LS_RESULT.
  MOVE-CORRESPONDING US_DATA-HEAD TO LS_RESULT.

*
  LOOP AT US_DATA-ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>) .
    MOVE-CORRESPONDING <L_ITEM> TO LS_RESULT.
*    LS_RESULT-ROWNO = <L_ITEM>-ROWNO.
*
    LOOP AT US_DATA-MESSG ASSIGNING <L_MESSG> .
*      MOVE-CORRESPONDING <L_MESSG> TO LS_RESULT.
*      LS_RESULT-ROWNO = <L_MESSG>-ROWNO.
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
*
      APPEND LS_RESULT TO CT_RESULT.
    ENDLOOP.
*
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& Display Processing Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT  TYPE TT_RESULT.

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
*  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& ALV Field Catalog for Report
*&---------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT  CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.
*  CONSTANTS LC_220(2) TYPE N VALUE 50.

*  DATA:
*    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SHEET'.
        <L_FIELDCAT>-TECH = GC_TRUE.
      WHEN 'PARTN_NUMB_AG'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C04.
      WHEN 'PARTN_NUMB_WE'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C05.
      WHEN 'PARTN_NUMB_RE'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C06.
      WHEN 'PARTN_NUMB_RG'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C07.
      WHEN 'PARTN_NUMB_VE'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C08.
      WHEN 'NAME'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C09.
      WHEN 'NAME_2'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C10.
      WHEN 'NAME_3'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C11.
      WHEN 'NAME_4'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C12.
      WHEN 'STREET'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C13.
      WHEN 'STR_SUPPL3'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C14.
      WHEN 'LOCATION'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C15.
      WHEN 'STR_SUPPL1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C16.
      WHEN 'STR_SUPPL2'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C17.
      WHEN 'DISTRICT'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C18.
      WHEN 'CITY'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C19.
      WHEN 'POSTL_COD1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C20.
      WHEN 'STCD3'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C21.
      WHEN 'CONDTYPE_ZDH1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C22.
      WHEN 'CONDTYPE_ZDH2'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C23.
      WHEN 'CONDTYPE_ZDH3'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C24.
      WHEN 'CONDTYPE_ZDH4'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C25.
      WHEN 'CONDTYPE_ZDH5'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C26.
      WHEN 'CONDTYPE_ZDH6'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C27.
      WHEN 'HDTXT_ZH01'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C28.
      WHEN 'HDTXT_ZH03'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C29. "***
      WHEN 'HDTXT_ZH06'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C30.
      WHEN 'HDTXT_ZH10'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C31.
      WHEN 'HDTXT_ZH13'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C32.
      WHEN 'HDTXT_ZH15'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C33.
      WHEN 'HDTXT_ZH19'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C34.
      WHEN 'HDTXT_ZH09'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C56.
      WHEN 'CONDTYPE_ZPR1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C35.
      WHEN 'CONDTYPE_ZDI1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C36.
      WHEN 'CONDTYPE_ZDI2'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C37.
      WHEN 'CONDTYPE_ZDI3'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C38.
      WHEN 'CONDTYPE_ZDI4'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C39.
      WHEN 'CONDTYPE_ZD01'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C40.
      WHEN 'CONDTYPE_ZD02'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C41.
      WHEN 'CONDTYPE_ZD03'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C42.
      WHEN 'CONDTYPE_ZD04'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C43.
      WHEN 'CONDTYPE_ZD05'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C44.
      WHEN 'CONDTYPE_ZD06'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C45.
      WHEN 'CONDTYPE_ZD07'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C46.
      WHEN 'CONDTYPE_ZD08'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C47.
      WHEN 'CONDTYPE_ZD09'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C48.
      WHEN 'CONDTYPE_ZD10'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C49.
      WHEN 'CONDTYPE_ZF01'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C50.
      WHEN 'CONDTYPE_ZPS1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C51.
      WHEN 'CONDTYPE_ZPS2'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C52.
      WHEN 'CONDTYPE_ZPS3'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C53.
      WHEN 'CONDTYPE_ZWR1'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C54.
      WHEN 'CONDTYPE_ZWR2'.
        <L_FIELDCAT>-COLTEXT   = TEXT-C55.

    ENDCASE.

  ENDLOOP.

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
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
*FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.
*
**  CONSTANTS:
**    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'TRANS_NO'.
**    lc_sort2 TYPE  lvc_s_sort-fieldname VALUE 'ROWNO'.
*  DATA:
*    LS_SORT  TYPE  LVC_S_SORT.
*
*
** Initialize Output
**  CLEAR: CT_SORT.
**
*** Sort by Document Group Number
**  CLEAR LS_SORT.
**  LS_SORT-FIELDNAME = LC_SORT1.
**  LS_SORT-UP        = GC_TRUE.
**  LS_SORT-SUBTOT    = SPACE.
**  APPEND LS_SORT TO CT_SORT.
*
** Sort by Row Number
**  CLEAR ls_sort.
**  ls_sort-fieldname = lc_sort2.
**  ls_sort-up        = gc_true.
**  ls_sort-subtot    = space.
**  APPEND ls_sort TO pt_sort.
*
*ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD UF_DYNDOC_ID->ADD_TABLE
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
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Processing Date/Time:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : System/Client:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Process By:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total Documents:
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success Records:
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line 7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Failed Records:
  LF_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE.

*-----------------------
* Add value in Line 8
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
  IF CB_TEST IS INITIAL.
*   Text-h09 : Production Run
    LF_TEXT = TEXT-H09.
  ELSE.
*   Text-h08 : Test Run
    LF_TEXT = TEXT-H08.
  ENDIF.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 25,
    LF_COL02 TYPE  I VALUE 35,
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50.


  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h04 : Process By:
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-UNAME NO-GAP.

* Text-h05 : Total Records:
  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP.

* Text-h06 : Success Records:
  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h07 : Failed Records:
  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_NEGATIVE.
  CASE GC_TRUE .
    WHEN CB_TEST.
*   Text-h09 : Production Run
      LF_TEXT = TEXT-H09.
    WHEN CB_POST .
*   Text-h08 : Test Run
      LF_TEXT = TEXT-H08.
  ENDCASE.
  WRITE AT: /1(20) LF_TEXT CENTERED COLOR COL_TOTAL.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_data
*&---------------------------------------------------------------------*
*& Posting data
*&---------------------------------------------------------------------*
FORM F_POST_DATA    USING  UT_DATA   TYPE  TT_DATA
                           UF_TEST   TYPE  FLAG
                  CHANGING CT_RESULT TYPE  TT_RESULT
                           CS_SUM    TYPE  TS_SUM.

  DATA:
    LT_MESSG TYPE  TT_MESSG.

  DATA:
    LS_DATA  TYPE  TS_DATA,
    LS_SDDOC TYPE  TS_DOC.

  DATA:
    LF_ERROR  TYPE  FLAG.


* Initialize Output
  CLEAR: CT_RESULT.
  CLEAR:   CS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

* Processing each record
  LOOP AT UT_DATA INTO LS_DATA.

*   Only non validation error
    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_POST_SD_DOC       USING  LS_DATA
                                         UF_TEST
                                CHANGING LS_SDDOC
                                         LT_MESSG
                                         LF_ERROR.
      IF LS_SDDOC IS NOT INITIAL.
        LS_DATA-RESULT = LS_SDDOC.
      ENDIF.
      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LF_ERROR = GC_TRUE.
    ENDIF.

    CS_SUM-TOTAL = CS_SUM-TOTAL + 1.

    IF LF_ERROR EQ GC_TRUE.
      CS_SUM-ERROR = CS_SUM-ERROR + 1.
    ELSE.
      CS_SUM-SUCCS = CS_SUM-SUCCS + 1.
    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT  USING  LS_DATA
                            CHANGING CT_RESULT.

  ENDLOOP.

* Show Message Complete
* Text-i07: Processing completed.
  MESSAGE S000(ZSDSPS01) WITH TEXT-I07 SPACE SPACE SPACE.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POST_SD_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_POST_SD_DOC USING  US_DATA  TYPE  TS_DATA
                          UF_TEST  TYPE  FLAG
                 CHANGING CS_SDDOC TYPE  TS_DOC
                          CT_MESSG TYPE  TT_MESSG
                          CF_ERROR TYPE  FLAG.


  DATA:
    LT_ITEM             TYPE STANDARD TABLE OF BAPISDITM,
*    LT_ITEMX            TYPE STANDARD TABLE OF BAPISDITMX,
    LT_RETURN           TYPE STANDARD TABLE OF BAPIRET2,
    LT_PARTNERS         TYPE STANDARD TABLE OF BAPIPARNR,
    LT_PARTNERADDRESSES TYPE STANDARD TABLE OF BAPIADDR1,
    LT_ITEMS_COND       TYPE STANDARD TABLE OF BAPICOND,
    LT_ITEMS_CONDX      TYPE STANDARD TABLE OF BAPICONDX,
    LT_SCHEDULE_LINES   TYPE STANDARD TABLE OF BAPISCHDL,
    LT_SCHEDULE_LINESX  TYPE STANDARD TABLE OF BAPISCHDLX ##NEEDED,
    LT_TEXTS            TYPE STANDARD TABLE OF BAPISDTEXT,
    LS_BAPE_VBAP        TYPE BAPE_VBAP,
    LS_BAPE_VBAPX       TYPE BAPE_VBAPX,
*    LS_BAPE_VBAK        TYPE BAPE_VBAK,
*    LS_BAPE_VBAKX       TYPE BAPE_VBAKX,
    LS_VBPA3KOM         TYPE VBPA3KOM,
    LT_VBPA3KOM         TYPE STANDARD TABLE OF VBPA3KOM,
    LS_EXTENSIONIN      TYPE BAPIPAREX,
    LT_EXTENSIONIN      TYPE BAPIPAREX_T,
    LS_HEADER           TYPE BAPISDHD1,
*    LS_HEADERX          TYPE BAPISDHD1X,
    LV_SALES_UNIT_CONV  TYPE VRKME,
*    LS_RESPONSE         TYPE ZSDSCAS006,
*    LV_SALESORG         TYPE VKORG,
*    LV_DIVISION         TYPE SPART,
    LT_CONDITION_ITEM   TYPE ZSDSSDS006_TT.

*  TYPES:
*    BEGIN OF LTY_SERVMAT_ITEMNO,
*      DIGIT1     TYPE NUMC1,
*      MAX_ITEMNO TYPE POSNR,
*    END OF LTY_SERVMAT_ITEMNO.

*  DATA: LS_SERVMAT_ITEMNO TYPE LTY_SERVMAT_ITEMNO,
*        LT_SERVMAT_ITEMNO TYPE TABLE OF LTY_SERVMAT_ITEMNO,
*        LV_CHAR6          TYPE CHAR6,
*        LV_INT            TYPE I,
   DATA: LV_REF_DOC_IT     TYPE POSNR.


* Iniaitlize Output
  CLEAR: CT_MESSG.
  CLEAR: CF_ERROR , CS_SDDOC.
  DATA:
    LS_ERROR_FLAG(1) TYPE C,
    LS_MESSG         TYPE TS_MESSG.

  DATA: LS_DATA_ITEM  TYPE TS_ITEM,
        LT_DATA_ITEM  TYPE TABLE OF TS_ITEM,
        LT_DATA_SCHED TYPE TABLE OF TS_ITEM.



*-------------------Header-----------------------------
  IF US_DATA-HEAD-QUOTATION IS NOT INITIAL.
    LS_HEADER = VALUE #( NAME        = US_DATA-HEAD-REF_1"US_DATA-HEAD-BNAME
                         DOC_DATE    = SY-DATUM
                         DOC_TYPE    = US_DATA-HEAD-DOC_TYPE
                         REF_DOC_L   = US_DATA-HEAD-QUOTATION
                         REF_DOC     = US_DATA-HEAD-QUOTATION
                         REFDOC_CAT  = 'B'
                         SALES_ORG   = US_DATA-HEAD-SALES_ORG "Default ‘1000’ SDS Siam Daikin Sale
                         DISTR_CHAN  = US_DATA-HEAD-DISTR_CHAN
                         DIVISION    = US_DATA-HEAD-DIVISION "Default '00' Common
                         SALES_OFF   = US_DATA-HEAD-SALES_OFF
                         SALES_GRP   = US_DATA-HEAD-SALES_GRP
                         PURCH_NO_C  = US_DATA-HEAD-PURCH_NO_C
                         REF_1       = US_DATA-HEAD-SALESORDER "US_DATA-HEAD-REF_1
                         PMNTTRMS    = US_DATA-HEAD-PMNTTRMS
                         DLVSCHDUSE  = US_DATA-HEAD-USAGE
                       ).
  ELSE.
    LS_HEADER = VALUE #( NAME        = US_DATA-HEAD-REF_1"US_DATA-HEAD-BNAME
                         DOC_DATE    = SY-DATUM
                         DOC_TYPE    = US_DATA-HEAD-DOC_TYPE
*                       REF_DOC_L   = US_DATA-HEAD-QUOTATION
*                       REF_DOC     = US_DATA-HEAD-QUOTATION
                         REFDOC_CAT  = 'B'
                         SALES_ORG   = US_DATA-HEAD-SALES_ORG "Default ‘1000’ SDS Siam Daikin Sale
                         DISTR_CHAN  = US_DATA-HEAD-DISTR_CHAN
                         DIVISION    = US_DATA-HEAD-DIVISION "Default '00' Common
                         SALES_OFF   = US_DATA-HEAD-SALES_OFF
                         SALES_GRP   = US_DATA-HEAD-SALES_GRP
                         PURCH_NO_C  = US_DATA-HEAD-PURCH_NO_C
                         REF_1       = US_DATA-HEAD-SALESORDER "US_DATA-HEAD-REF_1
                         PMNTTRMS    = US_DATA-HEAD-PMNTTRMS
                         DLVSCHDUSE  = US_DATA-HEAD-USAGE
                       ).
  ENDIF.
*  LS_HEADERX = VALUE #( NAME       = ABAP_TRUE
*                       DOC_DATE    = ABAP_TRUE
*                       DOC_TYPE    = ABAP_TRUE
*                       REF_DOC_L   = ABAP_TRUE
*                       REF_DOC     = ABAP_TRUE
*                       REFDOC_CAT  = ABAP_TRUE
*                       SALES_ORG   = ABAP_TRUE
*                       DISTR_CHAN  = ABAP_TRUE
*                       DIVISION    = ABAP_TRUE
*                       SALES_OFF   = ABAP_TRUE
*                       SALES_GRP   = ABAP_TRUE
*                       PURCH_NO_C  = ABAP_TRUE
*                       REF_1       = ABAP_TRUE
*                       PMNTTRMS    = ABAP_TRUE
*                       DLVSCHDUSE  = ABAP_TRUE
*                     ).

* Partner
  "Currency
  SELECT SINGLE KUNNR,VKORG,VTWEG,SPART,WAERS
    FROM KNVV
   WHERE KUNNR = @US_DATA-HEAD-PARTN_NUMB_AG
     AND VKORG = @US_DATA-HEAD-SALES_ORG
     AND VTWEG = '00'  "Common
     AND SPART = '00'  "Common
    INTO @DATA(LS_KNVV).

  SELECT SINGLE KUNNR,ADRNR,KTOKD INTO @DATA(LS_KNA1)
    FROM KNA1
   WHERE KUNNR = @US_DATA-HEAD-PARTN_NUMB_AG.
*  IF LS_KNA1-KTOKD IN GRT_ONETIMECUST.
  IF LS_KNA1-KTOKD = GC_Z060.
    IF US_DATA-HEAD-PARTN_NUMB_AG IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'AG'
                      PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_AG ALPHA = IN }|
                      ADDR_LINK  = LS_KNA1-ADRNR ) TO LT_PARTNERS.
    ENDIF.

    IF US_DATA-HEAD-PARTN_NUMB_RE IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RE'
                      PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_RE ALPHA = IN }|
                      ADDR_LINK  = LS_KNA1-ADRNR ) TO LT_PARTNERS.
    ENDIF.

    IF US_DATA-HEAD-PARTN_NUMB_RG IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RG'
                      PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_RG ALPHA = IN }|
                      ADDR_LINK  = LS_KNA1-ADRNR ) TO LT_PARTNERS.
    ENDIF.

  ELSE.
    IF US_DATA-HEAD-PARTN_NUMB_AG IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'AG'
                      PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_AG ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF US_DATA-HEAD-PARTN_NUMB_RE IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RE'
                      PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_RE ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF US_DATA-HEAD-PARTN_NUMB_RG IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RG'
                      PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_RG ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.


  ENDIF.

  IF US_DATA-HEAD-PARTN_NUMB_WE IS NOT INITIAL.
    APPEND VALUE #( PARTN_ROLE = 'WE'
                    PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_WE ALPHA = IN }| ) TO LT_PARTNERS.
  ENDIF.


  IF US_DATA-HEAD-PARTN_NUMB_VE IS NOT INITIAL.
    APPEND VALUE #( PARTN_ROLE = 'VE'
                    PARTN_NUMB = |{ US_DATA-HEAD-PARTN_NUMB_VE ALPHA = IN }| ) TO LT_PARTNERS.
  ENDIF.

  "Partner Address
  CLEAR: LT_PARTNERADDRESSES.

  SELECT SINGLE KUNNR,ADRNR,KTOKD INTO @LS_KNA1
    FROM KNA1
   WHERE KUNNR = @US_DATA-HEAD-PARTN_NUMB_AG.
*    IF LS_KNA1-KTOKD IN GRT_ONETIMECUST.
  IF LS_KNA1-KTOKD = GC_Z060.
    APPEND VALUE #( ADDR_NO      = LS_KNA1-ADRNR
                    NAME         = US_DATA-HEAD-NAME
                    NAME_2       = US_DATA-HEAD-NAME_2
                    NAME_3       = US_DATA-HEAD-NAME_3
                    NAME_4       = US_DATA-HEAD-NAME_4
                    STREET       = US_DATA-HEAD-STREET
                    STR_SUPPL3   = US_DATA-HEAD-STR_SUPPL3
                    LOCATION     = US_DATA-HEAD-LOCATION
                    STR_SUPPL1   = US_DATA-HEAD-STR_SUPPL1
                    STR_SUPPL2   = US_DATA-HEAD-STR_SUPPL2
                    DISTRICT     = US_DATA-HEAD-DISTRICT
                    CITY         = US_DATA-HEAD-CITY
                    POSTL_COD1   = US_DATA-HEAD-POSTL_COD1
                 ) TO LT_PARTNERADDRESSES.
  ENDIF.

* HD Texts
  CLEAR: LT_TEXTS.

  PERFORM APPEND_TEXT USING 'ZH01' 'E' US_DATA-HEAD-HDTXT_ZH01 CHANGING LT_TEXTS.
  PERFORM APPEND_TEXT USING 'ZH03' 'E' US_DATA-HEAD-HDTXT_ZH03 CHANGING LT_TEXTS.
  PERFORM APPEND_TEXT USING 'ZH06' 'E' US_DATA-HEAD-HDTXT_ZH06 CHANGING LT_TEXTS.
  PERFORM APPEND_TEXT USING 'ZH10' 'E' US_DATA-HEAD-HDTXT_ZH10 CHANGING LT_TEXTS.
  PERFORM APPEND_TEXT USING 'ZH13' 'E' US_DATA-HEAD-HDTXT_ZH09 CHANGING LT_TEXTS. "Swap with ZH09
  PERFORM APPEND_TEXT USING 'ZH15' 'E' US_DATA-HEAD-HDTXT_ZH15 CHANGING LT_TEXTS.
  PERFORM APPEND_TEXT USING 'ZH19' 'E' US_DATA-HEAD-HDTXT_ZH19 CHANGING LT_TEXTS.
  PERFORM APPEND_TEXT USING 'ZH09' 'E' US_DATA-HEAD-HDTXT_ZH13 CHANGING LT_TEXTS. "Swap with ZH13

*  APPEND VALUE #( TEXT_ID   = 'ZH01'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH01 ) TO LT_TEXTS.
*
*  APPEND VALUE #( TEXT_ID   = 'ZH03'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH03 ) TO LT_TEXTS.
*
*  APPEND VALUE #( TEXT_ID   = 'ZH06'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH06 ) TO LT_TEXTS.
*
*  APPEND VALUE #( TEXT_ID   = 'ZH10'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH10 ) TO LT_TEXTS.
*
*  APPEND VALUE #( TEXT_ID   = 'ZH13'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH13 ) TO LT_TEXTS.
*
*  APPEND VALUE #( TEXT_ID   = 'ZH15'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH15 ) TO LT_TEXTS.
*
*  APPEND VALUE #( TEXT_ID   = 'ZH19'
*                  LANGU     = 'EN'
*                  TEXT_LINE = US_DATA-HEAD-HDTXT_ZH19 ) TO LT_TEXTS.

*Tax3 one-time customer
*(PARTN_ROLE) = ‘SP’ Sold-to  AG
*(PARTN_ROLE) = ‘BP’ Bill-to  RE
*(PARTN_ROLE) = ‘PY’ Payer    RG
  IF US_DATA-HEAD-STCD3 IS NOT INITIAL.
    DO 3 TIMES.
      CLEAR LS_VBPA3KOM.

      LS_VBPA3KOM-POSNR = '000000'.

      CASE SY-INDEX.
        WHEN 1.
          LS_VBPA3KOM-PARVW = 'AG'.
        WHEN 2.
          LS_VBPA3KOM-PARVW = 'RE'.
        WHEN 3.
          LS_VBPA3KOM-PARVW = 'RG'.
      ENDCASE.

      LS_VBPA3KOM-STCD3 = US_DATA-HEAD-STCD3.

      APPEND LS_VBPA3KOM TO LT_VBPA3KOM.

    ENDDO.
  ENDIF.

  IF LT_VBPA3KOM IS NOT INITIAL.
    "Export to MV45AFZZ  FORM USEREXIT_SAVE_DOCUMENT_PREPARE.
    EXPORT LT_VBPA3KOM FROM LT_VBPA3KOM TO MEMORY ID 'ONETIMECUST_TAX3'.
  ENDIF.

**  Conditions
**ZDH1
*  APPEND VALUE #( KPOSN = '000000'
*                  KSCHL = 'ZDH1'
*                  KBETR = US_DATA-HEAD-CONDTYPE_ZDH1 * 10
*                  WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**ZDH2
*  APPEND VALUE #( KPOSN = '000000'
*                  KSCHL = 'ZDH2'
*                  KBETR = US_DATA-HEAD-CONDTYPE_ZDH2
*                  WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**ZDH3
*  APPEND VALUE #( KPOSN = '000000'
*                  KSCHL = 'ZDH3'
*                  KBETR = US_DATA-HEAD-CONDTYPE_ZDH3 * 10
*                  WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**ZDH4
*  APPEND VALUE #( KPOSN = '000000'
*                  KSCHL = 'ZDH4'
*                  KBETR = US_DATA-HEAD-CONDTYPE_ZDH4
*                  WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**ZDH5
*  APPEND VALUE #( KPOSN = '000000'
*                  KSCHL = 'ZDH5'
*                  KBETR = US_DATA-HEAD-CONDTYPE_ZDH5 * 10
*                  WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**ZDH6
*  APPEND VALUE #( KPOSN = '000000'
*                  KSCHL = 'ZDH6'
*                  KBETR = US_DATA-HEAD-CONDTYPE_ZDH6
*                  WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.




  IF US_DATA-HEAD-CONDTYPE_ZDH1 = 0.
    APPEND VALUE #( KPOSN = '000000'
                    KSCHL = 'ZDH1'
                    KBETR = US_DATA-HEAD-CONDTYPE_ZDH1 * 10
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
  ENDIF.

  IF US_DATA-HEAD-CONDTYPE_ZDH2 = 0.
    APPEND VALUE #( KPOSN = '000000'
                    KSCHL = 'ZDH2'
                    KBETR = US_DATA-HEAD-CONDTYPE_ZDH2
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
  ENDIF.

  IF US_DATA-HEAD-CONDTYPE_ZDH3 = 0.
    APPEND VALUE #( KPOSN = '000000'
                    KSCHL = 'ZDH3'
                    KBETR = US_DATA-HEAD-CONDTYPE_ZDH3 * 10
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
  ENDIF.

  IF US_DATA-HEAD-CONDTYPE_ZDH4 = 0.
    APPEND VALUE #( KPOSN = '000000'
                    KSCHL = 'ZDH4'
                    KBETR = US_DATA-HEAD-CONDTYPE_ZDH4
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**ZDH5
  ENDIF.

  IF US_DATA-HEAD-CONDTYPE_ZDH5 = 0.
    APPEND VALUE #( KPOSN = '000000'
                    KSCHL = 'ZDH5'
                    KBETR = US_DATA-HEAD-CONDTYPE_ZDH5 * 10
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
  ENDIF.
  IF US_DATA-HEAD-CONDTYPE_ZDH6 = 0.
    APPEND VALUE #( KPOSN = '000000'
                    KSCHL = 'ZDH6'
                    KBETR = US_DATA-HEAD-CONDTYPE_ZDH6
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
  ENDIF.



  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH1'
                  COND_VALUE = US_DATA-HEAD-CONDTYPE_ZDH1 * 10
                  CURRENCY = LS_KNVV-WAERS ) TO  LT_ITEMS_COND.

  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH1'
                  COND_VALUE = GC_TRUE
                  CURRENCY = GC_TRUE
                  UPDATEFLAG = 'I' ) TO LT_ITEMS_CONDX.


  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH2'
                  COND_VALUE = US_DATA-HEAD-CONDTYPE_ZDH2
                  CURRENCY = LS_KNVV-WAERS ) TO  LT_ITEMS_COND.
  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH2'
                  COND_VALUE = GC_TRUE
                  CURRENCY = GC_TRUE
                  UPDATEFLAG = 'I' ) TO LT_ITEMS_CONDX.


  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH3'
                  COND_VALUE = US_DATA-HEAD-CONDTYPE_ZDH3 * 10
                  CURRENCY = LS_KNVV-WAERS ) TO  LT_ITEMS_COND.
  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH3'
                  COND_VALUE = GC_TRUE
                  CURRENCY = GC_TRUE
                  UPDATEFLAG = 'I' ) TO LT_ITEMS_CONDX.

  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH4'
                  COND_VALUE = US_DATA-HEAD-CONDTYPE_ZDH4
                  CURRENCY = LS_KNVV-WAERS ) TO  LT_ITEMS_COND.
  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH4'
                  COND_VALUE = GC_TRUE
                  CURRENCY = GC_TRUE
                 UPDATEFLAG = 'I' ) TO LT_ITEMS_CONDX.

  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH5'
                  COND_VALUE = US_DATA-HEAD-CONDTYPE_ZDH5 * 10
                  CURRENCY = LS_KNVV-WAERS ) TO  LT_ITEMS_COND.
  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH5'
                  COND_VALUE = GC_TRUE
                  CURRENCY = GC_TRUE
                  UPDATEFLAG = 'I' ) TO LT_ITEMS_CONDX.

  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH6'
                  COND_VALUE = US_DATA-HEAD-CONDTYPE_ZDH6
                  CURRENCY = LS_KNVV-WAERS ) TO  LT_ITEMS_COND.
  APPEND VALUE #( ITM_NUMBER = '000000'
                  COND_TYPE = 'ZDH6'
                  COND_VALUE = GC_TRUE
                  CURRENCY = GC_TRUE
                  UPDATEFLAG = 'I' ) TO LT_ITEMS_CONDX.

  LT_DATA_ITEM = US_DATA-ITEM.
  SORT LT_DATA_ITEM BY ITM_NUMBER1.
  DELETE ADJACENT DUPLICATES FROM LT_DATA_ITEM COMPARING ITM_NUMBER1.

  LOOP AT LT_DATA_ITEM INTO LS_DATA_ITEM ##INTO_OK.


*Items
    "Convert Sales Unit to Internal Value
    IF LS_DATA_ITEM-TARGET_QU IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          INPUT          = LS_DATA_ITEM-TARGET_QU
        IMPORTING
          OUTPUT         = LV_SALES_UNIT_CONV
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
        CLEAR: LV_SALES_UNIT_CONV.
      ENDIF.
    ENDIF.


*Check service material
    LV_REF_DOC_IT = LS_DATA_ITEM-ITM_NUMBER1.

    APPEND VALUE #( ITM_NUMBER  = LS_DATA_ITEM-ITM_NUMBER1
                    REF_DOC     = US_DATA-HEAD-QUOTATION
                    REF_DOC_IT  = LV_REF_DOC_IT
                    REF_DOC_CA  = 'B'
                    MATERIAL    = LS_DATA_ITEM-MATERIAL
                    HG_LV_ITEM  = LS_DATA_ITEM-HG_LV_ITEM
                    TARGET_QTY  = LS_DATA_ITEM-TARGET_QTY
                    SALES_UNIT  = LV_SALES_UNIT_CONV
                    PLANT       = LS_DATA_ITEM-PLANT
                    STORE_LOC   = LS_DATA_ITEM-STORE_LOC
                    ITEM_CATEG  = LS_DATA_ITEM-ITEM_CATE
*                    ORDERID     = LS_DATA_ITEM-ORDERID
                    WBS_ELEM    = LS_DATA_ITEM-WBS_ELEM
                    )
                    TO LT_ITEM.

*    APPEND VALUE #( ITM_NUMBER  = LS_DATA_ITEM-ITM_NUMBER1
*                    REF_DOC     = ABAP_TRUE
*                    REF_DOC_IT  = ABAP_TRUE
*                    REF_DOC_CA  = 'B'
*                    MATERIAL    = ABAP_TRUE
*                    HG_LV_ITEM  = ABAP_TRUE
*                    TARGET_QTY  = ABAP_TRUE
*                    SALES_UNIT  = ABAP_TRUE
*                    PLANT       = ABAP_TRUE
*                    STORE_LOC   = ABAP_TRUE
*                    ITEM_CATEG  = ABAP_TRUE
*                    ORDERID     = ABAP_TRUE
*                    WBS_ELEM    = ABAP_TRUE
*                    )
*                    TO LT_ITEMX.
*  Conditions
*ZDH4
*    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
*                    KSCHL = 'ZDH4'
*                    KBETR = LS_DATA_ITEM-ONTOPDISCOUNTAMOUNT
*                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*ZPR1
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZPR1'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZPR1
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI1' ITEM DISCOUNT (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZDI1'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZDI1 * 10
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI2' ITEM DISCOUNT (VAL.)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZDI2'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZDI2
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI3'
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZDI3'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZDI3 * 10
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI4'
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZDI4'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZDI4
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZD01' ITEM DISCOUNT LEVEL 1 (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD01'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD01 * 10
                     ) TO LT_CONDITION_ITEM.

*'ZD02' ITEM DISCOUNT LEVEL 1 (VAL.)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD02'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD02
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZD03' ITEM DISCOUNT LEVEL 2 (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD03'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD03 * 10
                     ) TO LT_CONDITION_ITEM.

*'ZD04' ITEM DISCOUNT LEVEL 2 (VAL.)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD04'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD04
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZD05' ITEM DISCOUNT LEVEL 3 (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD05'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD05 * 10
                    ) TO LT_CONDITION_ITEM.

*'ZD06' ITEM DISCOUNT LEVEL 3 (VAL.)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD06'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD06
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZD07' ITEM DISCOUNT LEVEL 4 (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD07'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD07 * 10
                     ) TO LT_CONDITION_ITEM.

*'ZD08' ITEM DISCOUNT LEVEL 4 (VAL.)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD08'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD08
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZD09' ITEM DISCOUNT LEVEL 5 (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD09'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD09 * 10
                     ) TO LT_CONDITION_ITEM.

*'ZD10' ITEM DISCOUNT LEVEL 5 (VAL.)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZD10'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZD10
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZF01'  (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZF01'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZF01 * 10
                     ) TO LT_CONDITION_ITEM.

*'ZPS1' Installation
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZPS1'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZPS1
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZPS2' Maintenance
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZPS2'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZPS2
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZPS3' SLA
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZPS3'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZPS3
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
**'ZWR1' STDWarranty (%)
*    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
*                    KSCHL = 'ZWR1'
*                    KBETR = LS_DATA_ITEM-CONDTYPE_ZWR1  * 10
*                    ) TO LT_CONDITION_ITEM.
*
**'ZWR2' ExtendWarranty
*    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
*                    KSCHL = 'ZWR2'
*                    KBETR = LS_DATA_ITEM-CONDTYPE_ZWR2
*                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.
*'ZWR1' STDWarranty
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZWR1'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZWR1
                    WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZWR2' ExtendWarranty (%)
    APPEND VALUE #( KPOSN = LS_DATA_ITEM-ITM_NUMBER1
                    KSCHL = 'ZWR2'
                    KBETR = LS_DATA_ITEM-CONDTYPE_ZWR2  * 10
                     ) TO LT_CONDITION_ITEM.
*Schedule line
    LT_DATA_SCHED = US_DATA-ITEM.
    DELETE LT_DATA_SCHED WHERE ITM_NUMBER1 <> LS_DATA_ITEM-ITM_NUMBER1.
    LOOP AT LT_DATA_SCHED ASSIGNING FIELD-SYMBOL(<LFS_SCHEDULELINE>).
      IF <LFS_SCHEDULELINE>-REQ_QTY_1 = 0.
        APPEND VALUE #( ITM_NUMBER = <LFS_SCHEDULELINE>-ITM_NUMBER1
                        SCHED_LINE = <LFS_SCHEDULELINE>-SCHED_LINE_1
                        REQ_QTY    = LS_DATA_ITEM-TARGET_QTY          "******
                        REQ_DATE   = <LFS_SCHEDULELINE>-REQ_DATE_1 )
                        TO LT_SCHEDULE_LINES.
      ELSE.
        APPEND VALUE #( ITM_NUMBER = <LFS_SCHEDULELINE>-ITM_NUMBER1
                        SCHED_LINE = <LFS_SCHEDULELINE>-SCHED_LINE_1
                        REQ_QTY    = <LFS_SCHEDULELINE>-REQ_QTY_1
                        REQ_DATE   = <LFS_SCHEDULELINE>-REQ_DATE_1 )
                        TO LT_SCHEDULE_LINES.
      ENDIF.

      APPEND VALUE #( ITM_NUMBER = <LFS_SCHEDULELINE>-ITM_NUMBER1
                      SCHED_LINE = <LFS_SCHEDULELINE>-SCHED_LINE_1
                      REQ_QTY    = ABAP_TRUE
                      REQ_DATE   = ABAP_TRUE )
                      TO LT_SCHEDULE_LINESX.
    ENDLOOP.

*  Extensions
    CLEAR LS_EXTENSIONIN.
    LS_BAPE_VBAP-POSNR          = LS_DATA_ITEM-ITM_NUMBER1.
    LS_BAPE_VBAP-ZZ1_LOB_SO_SDI = LS_DATA_ITEM-ZLOB.

    LS_EXTENSIONIN-STRUCTURE = 'BAPE_VBAP'.

    PERFORM APPEND_EXTENSION USING LS_BAPE_VBAP CHANGING LS_EXTENSIONIN LT_EXTENSIONIN.

    CLEAR LS_EXTENSIONIN.
    LS_BAPE_VBAPX-POSNR          = LS_DATA_ITEM-ITM_NUMBER1.
    LS_BAPE_VBAPX-ZZ1_LOB_SO_SDI = 'X'.

    LS_EXTENSIONIN-STRUCTURE = 'BAPE_VBAPX'.

    PERFORM APPEND_EXTENSION USING LS_BAPE_VBAPX CHANGING LS_EXTENSIONIN LT_EXTENSIONIN.

    CLEAR:
      LV_SALES_UNIT_CONV.

  ENDLOOP.
  "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
  EXPORT LT_CONDITION_ITEM FROM LT_CONDITION_ITEM TO MEMORY ID 'CONDTYP_SO_FROM_SF'.
*   Post Sales Order

  DATA: LT_MIGRATION_CONDTYP TYPE KSCHL_RAN_TAB.
  APPEND VALUE #( SIGN   = 'I'
                OPTION = 'EQ'
                LOW    = 'ZDH1' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDH2' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDH3' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDH4' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDH5' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDH6' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZPR1' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDI1' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDI2' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDI3' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZDI4' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD01' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD02' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD03' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD04' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD05' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD06' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD07' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD08' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD09' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZD10' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZF01' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZPS1' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZPS2' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZPS3' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZWR1' ) TO LT_MIGRATION_CONDTYP.
  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'ZWR2' ) TO LT_MIGRATION_CONDTYP.

  "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
  EXPORT LT_MIGRATION_CONDTYP FROM LT_MIGRATION_CONDTYP TO MEMORY ID 'MIGRATION_CONDTYP'.


  DATA: LOGIC_SWITCH  TYPE BAPISDLS.
  LOGIC_SWITCH-NOSTRUCTURE = 'X'.

  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
      ORDER_HEADER_IN      = LS_HEADER
      TESTRUN              = UF_TEST
      LOGIC_SWITCH         = LOGIC_SWITCH
    IMPORTING
      SALESDOCUMENT        = CS_SDDOC-VBELN
    TABLES
      RETURN               = LT_RETURN
      ORDER_ITEMS_IN       = LT_ITEM
      ORDER_SCHEDULES_IN   = LT_SCHEDULE_LINES
      ORDER_PARTNERS       = LT_PARTNERS
      ORDER_CONDITIONS_IN  = LT_ITEMS_COND
      ORDER_CONDITIONS_INX = LT_ITEMS_CONDX
      ORDER_TEXT           = LT_TEXTS
      PARTNERADDRESSES     = LT_PARTNERADDRESSES
      EXTENSIONIN          = LT_EXTENSIONIN.
  CLEAR: LS_ERROR_FLAG.
  LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                        WHERE ( TYPE EQ 'E' OR
                                TYPE EQ 'A' ).

    LS_ERROR_FLAG = 'X'.

  ENDLOOP.

  IF LS_ERROR_FLAG   EQ 'X' .

    CF_ERROR = GC_TRUE.

    LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                          WHERE ( TYPE EQ 'E' OR
                                  TYPE EQ 'A' ).

      CLEAR LS_MESSG.
*      LS_MESSG-ROWNO = LS_DATA_ITEM-ROWNO.
      LS_MESSG-MSGTY = <L_RETURN>-TYPE.
      LS_MESSG-MSGID = <L_RETURN>-ID.
      LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
      LS_MESSG-MSGTX = <L_RETURN>-MESSAGE .
      INSERT LS_MESSG INTO TABLE CT_MESSG.
    ENDLOOP.

  ELSE. "Success
    IF UF_TEST = 'X'.
      CLEAR LS_MESSG.
*    LS_MESSG-ROWNO = LS_DATA_ITEM-ROWNO.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSCA01'.
      LS_MESSG-MSGNO = '000'.
*     Text-i01: Upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.

      INSERT LS_MESSG INTO TABLE CT_MESSG.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*       EXPORTING
*         WAIT          =
*       IMPORTING
*         RETURN        =
        .

      CLEAR LS_MESSG.
      READ TABLE LT_RETURN ASSIGNING <L_RETURN> WITH KEY TYPE = 'S'
                                                         ID = 'V1'
                                                         NUMBER = '311'.
      IF SY-SUBRC = 0.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE .

        INSERT LS_MESSG INTO TABLE CT_MESSG.
      ENDIF.
    ENDIF.
  ENDIF.
*
*
*  ELSE. "Actual Run
*    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
*      EXPORTING
*        DOCUMENTHEADER    = GS_DOCUMENTHEADER
*        CUSTOMERCPD       = GS_CUSTOMERCPD
*      IMPORTING
*        OBJ_TYPE          = LF_OBJ_TYPE
*        OBJ_KEY           = LF_OBJ_KEY
*        OBJ_SYS           = LF_OBJ_SYS
*      TABLES
*        ACCOUNTGL         = GT_ACCOUNTGL
*        ACCOUNTRECEIVABLE = GT_ACCOUNTRECEIVABLE
*        ACCOUNTPAYABLE    = GT_ACCOUNTPAYABLE
*        ACCOUNTTAX        = GT_ACCOUNTTAX
*        CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
*        CRITERIA          = GT_CRITERIA
**       VALUEFIELD        =
*        EXTENSION1        = GT_EXTENSION1
*        RETURN            = GT_RETURN
*        EXTENSION2        = GT_EXTENSION2
*        ACCOUNTWT         = GT_ACCOUNTWT.
*
*    CLEAR: LS_ERROR_FLAG.
*    LOOP AT GT_RETURN ASSIGNING <L_RETURN>
*                          WHERE ( TYPE EQ 'E' OR
*                                  TYPE EQ 'A' ).
*      LS_ERROR_FLAG = 'X'.
*    ENDLOOP.
*
**--AA Retirement--
*    IF GS_GEN_INFO IS NOT INITIAL.
*
*      CLEAR: LS_ORIGINDOC, LS_RETURN_AA .
*
*      LS_ORIGINDOC-OBJ_TYPE = 'AMBU'.
*      LS_ORIGINDOC-OBJ_KEY  = LF_OBJ_KEY.
*      LS_ORIGINDOC-OBJ_SYS  = LF_OBJ_SYS.
*
*      CALL FUNCTION 'BAPI_ASSET_RETIREMENT_POST'
*        EXPORTING
*          ORIGINDOCREFERENCE = LS_ORIGINDOC
*          GENERALPOSTINGDATA = GS_GEN_INFO
*          RETIREMENTDATA     = GS_RETIREMENT
*          FURTHERPOSTINGDATA = GS_FURTHER
*        IMPORTING
*          RETURN             = LS_RETURN_AA
*        TABLES
*          RETURN_ALL         = LT_RETURN_AA.
*
*      IF LS_RETURN_AA-TYPE = 'E' OR  LS_RETURN_AA-TYPE = 'A' .
*        LS_ERROR_FLAG = 'X' .
*
*        CLEAR LS_MESSG.
*        LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
*        LS_MESSG-MSGTY = LS_RETURN_AA-TYPE.
*        LS_MESSG-MSGID = LS_RETURN_AA-ID.
*        LS_MESSG-MSGNO = LS_RETURN_AA-NUMBER.
*        LS_MESSG-MSGTX = LS_RETURN_AA-MESSAGE .
*        INSERT LS_MESSG INTO TABLE CT_MESSG.
*      ENDIF.
*    ENDIF.
**-AA-- Retriment
*
*    IF LS_ERROR_FLAG EQ 'X'. "Error
*      LOOP AT GT_RETURN ASSIGNING <L_RETURN> WHERE ( TYPE EQ 'E' OR
*                                                     TYPE EQ 'A' ) .
*        CLEAR LS_MESSG.
*        LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
*        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
*        LS_MESSG-MSGID = <L_RETURN>-ID.
*        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
*        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE .
*        INSERT LS_MESSG INTO TABLE CT_MESSG.
*      ENDLOOP.
*    ELSE. "Success
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          WAIT = 'X'.
*
*      READ TABLE GT_RETURN ASSIGNING <L_RETURN> WITH KEY ID     = 'RW'
*                                                         NUMBER = '605'.
*
*      CLEAR LS_MESSG.
*      LS_MESSG-ROWNO = LS_DATA_ITEM-DATA-ROWNO.
*      LS_MESSG-MSGTY = 'S'.
*      LS_MESSG-MSGID = 'ZSDSCA01'.
*      LS_MESSG-MSGNO = '000'.
**     Text-i02: FI Document created successfully.
*      LS_MESSG-MSGTX = TEXT-I02.
*      INSERT LS_MESSG INTO TABLE CT_MESSG.
*
*      CS_FIDOC-BUKRS = US_DATA-HEAD-BUKRS.
*      CS_FIDOC-BELNR = <L_RETURN>-MESSAGE_V2(10).
*      CS_FIDOC-GJAHR = <L_RETURN>-MESSAGE_V2+14(4).
*    ENDIF.
*
*  ENDIF.
*
** Clear value
*  CLEAR:   GS_CUSTOMERCPD, GS_GEN_INFO, GS_RETIREMENT .
*
*  CLEAR:   GT_ACCOUNTGL, GT_ACCOUNTRECEIVABLE, GT_ACCOUNTPAYABLE,
*           GT_ACCOUNTTAX, GT_CURRENCYAMOUNT, GT_CRITERIA, GT_EXTENSION1, GT_EXTENSION2,
*           GT_RETURN, GT_ACCOUNTWT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_distchan
*----------------------------------------------------------------------*
*  Validate Distribution Channel
*----------------------------------------------------------------------*
FORM F_VALIDATE_DISTCHAN  USING  UF_STRING  TYPE STRING ##CALLED
                        CHANGING CF_VTWEG   TYPE TVTW-VTWEG
                                 CF_MSGTX   TYPE TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVTW,
           VTWEG TYPE  TVTW-VTWEG,
         END OF LTS_TVTW.
  TYPES: LTT_TVTW  TYPE  SORTED TABLE OF LTS_TVTW
                           WITH UNIQUE KEY VTWEG.

  STATICS:
    LS_TVTW TYPE  LTS_TVTW,
    LT_TVTW TYPE  LTT_TVTW.

  DATA:
    LF_VTWEG TYPE  LTS_TVTW-VTWEG.


* Initialize Output
  CLEAR: CF_VTWEG,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 2
  IF STRLEN( UF_STRING ) GT 2.
*   Text-e40 : Invalid Channel :
    CONCATENATE TEXT-E40 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_VTWEG = UF_STRING.

* Check Buffer
  IF LS_TVTW-VTWEG NE LF_VTWEG.
*   Validate with Memory
    READ TABLE LT_TVTW INTO LS_TVTW
                        WITH KEY VTWEG = LF_VTWEG
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVTW.
*     Validate with Database
      SELECT SINGLE VTWEG
        INTO LS_TVTW
        FROM TVTW
       WHERE VTWEG EQ LF_VTWEG.
      IF SY-SUBRC NE 0.
*       Text-e40 : Invalid Channel :
        CONCATENATE TEXT-E40 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVTW INTO TABLE LT_TVTW.
    ENDIF.

  ENDIF.

* Assign Output
  CF_VTWEG = LS_TVTW-VTWEG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_BILLING_TYPE
*&---------------------------------------------------------------------*
FORM F_VALIDATE_BILLING_TYPE  USING UF_STRING  TYPE  STRING ##CALLED
                           CHANGING CF_FKART   TYPE  ANY
                                    CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVFK,
           FKART TYPE  TVFK-FKART,
         END OF LTS_TVFK.
  TYPES: LTT_TVFK  TYPE  SORTED TABLE OF LTS_TVFK
                         WITH UNIQUE KEY FKART.

  STATICS:
    LS_TVFK TYPE  LTS_TVFK,
    LT_TVFK TYPE  LTT_TVFK.

  DATA: LF_FKART TYPE  TVFK-FKART.
* Initialize Output
  CLEAR: CF_FKART,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_FKART = UF_STRING.

  IF LS_TVFK-FKART NE LF_FKART.
*   Validate with Memory
    READ TABLE LT_TVFK INTO LS_TVFK
                        WITH KEY FKART = CF_FKART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVFK.
*     Validate with Database
      SELECT SINGLE FKART
        INTO LS_TVFK
        FROM TVFK
       WHERE FKART EQ LF_FKART.
      IF SY-SUBRC NE 0.
*       Text-e48 : Invalid Billing Type:
        CONCATENATE TEXT-E48 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVFK INTO TABLE LT_TVFK.
    ENDIF.
  ENDIF.

* Assign Output
  CF_FKART = LS_TVFK-FKART.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_sales_order
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SALES_ORDER  USING UF_STRING  TYPE  STRING ##CALLED
                          CHANGING CF_KAUFN   TYPE  ANY
                                   CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_VBAK,
           KAUFN TYPE  VBAK-VBELN,
         END OF LTS_VBAK.
  TYPES: LTT_VBAK  TYPE  SORTED TABLE OF LTS_VBAK
                           WITH UNIQUE KEY KAUFN.

  STATICS:
    LS_VBAK TYPE  LTS_VBAK,
    LT_VBAK TYPE  LTT_VBAK.

  DATA: LF_KAUFN TYPE  VBAK-VBELN.
* Initialize Output
  CLEAR: CF_KAUFN,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_KAUFN = UF_STRING.

  IF LS_VBAK-KAUFN NE LF_KAUFN.
*   Validate with Memory
    READ TABLE LT_VBAK INTO LS_VBAK
                        WITH KEY KAUFN = LF_KAUFN
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_VBAK.
*     Validate with Database
      SELECT SINGLE VBELN
        INTO LS_VBAK
        FROM VBAK
       WHERE VBELN EQ LF_KAUFN.
      IF SY-SUBRC NE 0.
*       Text-e49 : Invalid Sales Order:
        CONCATENATE TEXT-E49 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_VBAK INTO TABLE LT_VBAK.
    ENDIF.
  ENDIF.

* Assign Output
  CF_KAUFN = LS_VBAK-KAUFN.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_assetno
*&---------------------------------------------------------------------*
*& Validate Asset No.
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ASSETNO  USING UF_STRING  TYPE  STRING ##CALLED
                      CHANGING CF_ANLN1   TYPE  ANLH-ANLN1
                               CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_ANLH,
           ANLN1 TYPE  ANLH-ANLN1,
         END OF LTS_ANLH.
  TYPES: LTT_ANLH  TYPE  SORTED TABLE OF LTS_ANLH
                          WITH UNIQUE KEY ANLN1.

  STATICS:
    LT_ANLH TYPE  LTT_ANLH,
    LS_ANLH TYPE  LTS_ANLH.

  DATA:
    LF_ANLN1  TYPE  ANLH-ANLN1.


* Initialize Output
  CLEAR: CF_ANLN1,
         CF_MSGTX.

* Only when not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = UF_STRING
    IMPORTING
      OUTPUT = LF_ANLN1.

* Check Buffer
  IF LS_ANLH-ANLN1 NE LF_ANLN1.
*   Validate with Memory
    READ TABLE LT_ANLH INTO LS_ANLH
                       WITH KEY ANLN1 = LF_ANLN1
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_ANLH.
*     Validate with Database
      SELECT ANLN1 UP TO 1 ROWS                      "#EC CI_SEL_NESTED
        INTO LS_ANLH
        FROM ANLH
       WHERE ANLN1  EQ  LF_ANLN1
        ORDER BY ANLN1 .
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e01 : Invalid Asset no:
        CONCATENATE TEXT-E01 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_ANLH INTO TABLE LT_ANLH.
    ENDIF.

  ENDIF.

* Assign Output
  CF_ANLN1 = LF_ANLN1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SALESOFF
*&---------------------------------------------------------------------*
*& Validate Sales Office
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SALESOFF  USING  UF_STRING  TYPE  STRING ##CALLED
                        CHANGING CF_VKBUR   TYPE  TVBUR-VKBUR
                                 CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVBUR,
           VKBUR TYPE  TVBUR-VKBUR,
         END OF LTS_TVBUR.
  TYPES: LTT_TVBUR  TYPE  SORTED TABLE OF LTS_TVBUR
                           WITH UNIQUE KEY VKBUR.

  STATICS:
    LS_TVBUR TYPE  LTS_TVBUR,
    LT_TVBUR TYPE  LTT_TVBUR.

  DATA:
    LF_VKBUR TYPE  LTS_TVBUR-VKBUR.


* Initialize Output
  CLEAR: CF_VKBUR,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Length = 4
  IF STRLEN( UF_STRING ) GT 4.
*   Text-e39 : Invalid Sales Office :
    CONCATENATE TEXT-E39 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LF_VKBUR = UF_STRING.

* Check Buffer
  IF LS_TVBUR-VKBUR NE LF_VKBUR.
*   Validate with Memory
    READ TABLE LT_TVBUR INTO LS_TVBUR
                        WITH KEY VKBUR = LF_VKBUR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVBUR.
*     Validate with Database
      SELECT SINGLE VKBUR
        INTO LS_TVBUR
        FROM TVBUR
       WHERE VKBUR EQ LF_VKBUR.
      IF SY-SUBRC NE 0.
*       Text-e39 : Invalid Sales Office :
        CONCATENATE TEXT-E39 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVBUR INTO TABLE LT_TVBUR.
    ENDIF.

  ENDIF.

* Assign Output
  CF_VKBUR = LS_TVBUR-VKBUR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DIVISION
*&---------------------------------------------------------------------*
*& Validate Division
*&---------------------------------------------------------------------*
FORM F_VALIDATE_DIVISION  USING UF_STRING  TYPE  STRING ##CALLED
                       CHANGING CF_SPART   TYPE  ANY
                                CF_MSGTX   TYPE  TS_MSG_TXT.


  TYPES: BEGIN OF LTS_TSPA,
           SPART TYPE  TSPA-SPART,
         END OF LTS_TSPA.
  TYPES: LTT_TSPA  TYPE  SORTED TABLE OF LTS_TSPA
                           WITH UNIQUE KEY SPART.

  STATICS:
    LS_TSPA TYPE  LTS_TSPA,
    LT_TSPA TYPE  LTT_TSPA.

  DATA: LF_SPART TYPE  TSPA-SPART.
* Initialize Output
  CLEAR: CF_SPART,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_SPART = UF_STRING.

  IF LS_TSPA-SPART NE LF_SPART.
*   Validate with Memory
    READ TABLE LT_TSPA INTO LS_TSPA
                        WITH KEY SPART = LF_SPART
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TSPA.
*     Validate with Database
      SELECT SINGLE SPART
        INTO LS_TSPA
        FROM TSPA
       WHERE SPART EQ LF_SPART.
      IF SY-SUBRC NE 0.
*       Text-e45 : Invalid Division:
        CONCATENATE TEXT-E45 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TSPA INTO TABLE LT_TSPA.
    ENDIF.
  ENDIF.

* Assign Output
  CF_SPART = LS_TSPA-SPART.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SALESORG
*&---------------------------------------------------------------------*
*& Validate SalesOrg
*&---------------------------------------------------------------------*

FORM F_VALIDATE_SALESORG  USING UF_STRING  TYPE  STRING ##CALLED
                       CHANGING CF_VKORG   TYPE  TVKO-VKORG
                                CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVKO,
           VKORG TYPE  TVKO-VKORG,
         END OF LTS_TVKO.
  TYPES: LTT_TVKO  TYPE  SORTED TABLE OF LTS_TVKO
                           WITH UNIQUE KEY VKORG.

  STATICS:
    LS_TVKO TYPE  LTS_TVKO,
    LT_TVKO TYPE  LTT_TVKO.

  DATA: LF_VKORG TYPE  TVKO-VKORG.
* Initialize Output
  CLEAR: CF_VKORG,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_VKORG = UF_STRING.

  IF LS_TVKO-VKORG NE LF_VKORG.
*   Validate with Memory
    READ TABLE LT_TVKO INTO LS_TVKO
                        WITH KEY VKORG = LF_VKORG
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVKO.
*     Validate with Database
      SELECT SINGLE VKORG
        INTO LS_TVKO
        FROM TVKO
       WHERE VKORG EQ LF_VKORG.
      IF SY-SUBRC NE 0.
*       Text-e44 : Invalid Sales Organization:
        CONCATENATE TEXT-E44 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVKO INTO TABLE LT_TVKO.
    ENDIF.
  ENDIF.

* Assign Output
  CF_VKORG = LS_TVKO-VKORG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_SALES_GROUP
*&---------------------------------------------------------------------*
*& Validate SalesGroup
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SALES_GROUP  USING UF_STRING  TYPE  STRING ##CALLED
                          CHANGING CF_VKGRP   TYPE  ANY
                                   CF_MSGTX   TYPE  TS_MSG_TXT.

  TYPES: BEGIN OF LTS_TVKGR,
           VKGRP TYPE  TVKGR-VKGRP,
         END OF LTS_TVKGR.
  TYPES: LTT_TVKGR  TYPE  SORTED TABLE OF LTS_TVKGR
                           WITH UNIQUE KEY VKGRP.

  STATICS:
    LS_TVKGR TYPE  LTS_TVKGR,
    LT_TVKGR TYPE  LTT_TVKGR.

  DATA: LF_VKGRP TYPE  TVKGR-VKGRP.
* Initialize Output
  CLEAR: CF_VKGRP,
         CF_MSGTX.

* Only when value exist
  CHECK UF_STRING IS NOT INITIAL.

  LF_VKGRP = UF_STRING.

  IF LS_TVKGR-VKGRP NE LF_VKGRP.
*   Validate with Memory
    READ TABLE LT_TVKGR INTO LS_TVKGR
                        WITH KEY VKGRP = LF_VKGRP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_TVKGR.
*     Validate with Database
      SELECT SINGLE VKGRP
        INTO LS_TVKGR
        FROM TVKGR
       WHERE VKGRP EQ LF_VKGRP.
      IF SY-SUBRC NE 0.
*       Text-e47 : Invalid Sales Group:
        CONCATENATE TEXT-E47 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_TVKGR INTO TABLE LT_TVKGR.
    ENDIF.
  ENDIF.

* Assign Output
  CF_VKGRP = LS_TVKGR-VKGRP.


ENDFORM.

*----------------------------------------------------------------------*
*  Form f_display_SDdoc
*----------------------------------------------------------------------*
*  Display FI Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_SDDOC  USING  UF_VBELN  TYPE  VBAK-VBELN.

  SET PARAMETER ID 'AUN' FIELD UF_VBELN.

* Call VB03
  PERFORM F_AUTHORIZE_CHECK USING GC_VA03.
  CALL TRANSACTION GC_VA03 WITHOUT AUTHORITY-CHECK
                           AND SKIP FIRST SCREEN.        "#EC CI_CALLTA

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_hotspot_click_1
*----------------------------------------------------------------------*
*  ALV Event on Hot spot click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID    TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.

  FIELD-SYMBOLS:
    <L_RESULT>  TYPE  TS_RESULT.


* Read Row
  READ TABLE GT_RESULT ASSIGNING <L_RESULT>
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
*   No row found
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'SALESORDER'.
      PERFORM F_DISPLAY_SDDOC  USING  <L_RESULT>-QUOTATION.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form append_extension
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM APPEND_EXTENSION  USING    BAPE_STRUCTURE TYPE ANY
                       CHANGING EXTENSIONIN    TYPE BAPIPAREX
                                EXTENSIONIN_T  TYPE BAPIPAREX_T.

*for extension
  CONSTANTS:
    LC_PART1 TYPE I VALUE 240,
    LC_PART2 TYPE I VALUE 480,
    LC_PART3 TYPE I VALUE 720,
    LC_PART4 TYPE I VALUE 960.

  FIELD-SYMBOLS: <LF_STRUCTURE> TYPE ANY.
  DATA:
    LF_OFF1        TYPE I,
    LF_OFF2        TYPE I,
    LF_OFF3        TYPE I,
    LF_OFF4        TYPE I,
    LF_LEN1        TYPE I,
    LF_LEN2        TYPE I,
    LF_LEN3        TYPE I,
    LF_LEN4        TYPE I,
    LF_LENGTH      TYPE I,
    LF_LENGTH1     TYPE I,
    LF_LENGTH2     TYPE I,
    LF_LENGTH3     TYPE I,
    LF_LENGTH4     TYPE I,
    LF_LENGTH_COMP TYPE I,
    LF_FLG_PART2   TYPE FLAG,
    LF_FLG_PART3   TYPE FLAG,
    LF_FLG_PART4   TYPE FLAG.

  ASSIGN BAPE_STRUCTURE TO <LF_STRUCTURE>.

  IF <LF_STRUCTURE> IS ASSIGNED.
    DO.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LF_STRUCTURE> TO FIELD-SYMBOL(<LF_FIELD>).
      DESCRIBE FIELD <LF_FIELD> LENGTH LF_LENGTH_COMP IN CHARACTER MODE.

      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
      LF_LENGTH = LF_LENGTH + LF_LENGTH_COMP.
      IF LF_LENGTH <= LC_PART1.
        LF_LENGTH1  = LF_LENGTH.
      ENDIF.
      IF LF_LENGTH >= LC_PART1 AND
         LF_LENGTH <= LC_PART2.
        LF_OFF2 = LF_LENGTH1.
        LF_LENGTH2  = LF_LENGTH.
        LF_FLG_PART2 = ABAP_ON.
      ENDIF.
      IF LF_LENGTH >= LC_PART2 AND
         LF_LENGTH <= LC_PART3.
        LF_OFF3 = LF_LENGTH2.
        LF_LENGTH3  = LF_LENGTH.
        LF_FLG_PART3 = ABAP_ON.
      ENDIF.
      IF LF_LENGTH >= LC_PART3 AND
         LF_LENGTH <= LC_PART4.
        LF_OFF4 = LF_LENGTH3.
        LF_LENGTH4  = LF_LENGTH.
        LF_FLG_PART4 = ABAP_ON.
      ENDIF.
    ENDDO.
    CLEAR: LF_LEN1,LF_LEN2,LF_LEN3,LF_LEN4.
    LF_LEN1 = LF_LENGTH1.
    LF_LEN2 = LF_LENGTH2.
    LF_LEN3 = LF_LENGTH3.
    LF_LEN4 = LF_LENGTH4.

    EXTENSIONIN-VALUEPART1 = <LF_STRUCTURE>+LF_OFF1(LF_LEN1).
    IF LF_FLG_PART2 = ABAP_ON.
      EXTENSIONIN-VALUEPART2 = <LF_STRUCTURE>+LF_OFF2(LF_LEN2).
    ENDIF.
    IF LF_FLG_PART3 = ABAP_ON.
      EXTENSIONIN-VALUEPART3 = <LF_STRUCTURE>+LF_OFF3(LF_LEN3).
    ENDIF.
    IF LF_FLG_PART4 = ABAP_ON.
      EXTENSIONIN-VALUEPART4 = <LF_STRUCTURE>+LF_OFF4(LF_LEN4).
    ENDIF.
  ENDIF.

  APPEND  EXTENSIONIN TO EXTENSIONIN_T.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM APPEND_TEXT  USING    PV_TEXT_ID TYPE TDID
                           PV_LANGU TYPE SPRAS
                           PV_HDTXT TYPE TEXT8192
                  CHANGING PT_TEXTS TYPE BAPISDTEXT_T.

  DATA: LT_HDTXT TYPE LDPS_TXT_TAB.

  FIELD-SYMBOLS: <LFS_HDTXT> LIKE LINE OF LT_HDTXT.

  SPLIT PV_HDTXT AT '\N' INTO TABLE LT_HDTXT.

  LOOP AT LT_HDTXT ASSIGNING <LFS_HDTXT>.

    APPEND VALUE #( TEXT_ID   = PV_TEXT_ID
                    LANGU     = PV_LANGU
                    TEXT_LINE = <LFS_HDTXT> ) TO PT_TEXTS.
  ENDLOOP.

ENDFORM.
