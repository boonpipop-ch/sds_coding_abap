*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0700
*  Creation Date      : 17.06.2025
*  Author             : Boonpipop Ch. (SDS)
*  Add-on ID          : N/A
*  Description        : Report for Delivery Order Signed
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSSDR0700
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0700 MESSAGE-ID ZSDSSD01.
CLASS LCL_HANDLE_EVENTS DEFINITION DEFERRED."Ref. Std : SALV_TEST_TABLE_EVENTS
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: LIKP,
        ZSDSSDT024.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF TYP_ALV.
    INCLUDE TYPE ZSDSSDT024.
TYPES:
    VSTEL       TYPE LIKP-VSTEL,
    VKORG       TYPE LIKP-VKORG,
    LFART       TYPE LIKP-LFART,
    KUNNR       TYPE LIKP-KUNNR,
    NAME1       TYPE KNA1-NAME1,
    NAME2       TYPE KNA1-NAME2,
    NAME3       TYPE KNA1-NAME3,
    NAME4       TYPE KNA1-NAME4,
    ERNAM       TYPE LIKP-ERNAM,
    INV_NUM     TYPE VBRK-VBELN,
    INV_DAT     TYPE VBRK-FKDAT,
    ZTERM       TYPE VBRK-ZTERM,
    LSTEL_TXT   TYPE TVLAT-VTEXT,
  END OF TYP_ALV.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA: GT_LOG_HEADER  TYPE BALHDR_T,
      GT_MSGH        TYPE BAL_T_MSGH,
      GS_LOG_H       TYPE BAL_S_LOG,
      GS_LOG_FILTER  TYPE BAL_S_LFIL,
      GS_R_SUBOBJECT TYPE BAL_S_SUB,
      GS_R_OBJECT    TYPE BAL_S_OBJ,
      GS_R_EXTNUMBER TYPE BAL_S_EXTN,
      GS_MSG         TYPE BAL_S_MSG,
      GT_ALV         TYPE STANDARD TABLE OF TYP_ALV,
      GO_EVENTS      TYPE REF TO LCL_HANDLE_EVENTS.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
*CONSTANTS:

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS:

*      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE
*        IMPORTING ROW COLUMN.

      ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN.
ENDCLASS.                    "lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS IMPLEMENTATION.
  METHOD ON_LINK_CLICK.
*    PERFORM show_cell_info USING row column TEXT-i07.
    READ TABLE GT_ALV INDEX ROW INTO DATA(LS_ALV).
    IF SY-SUBRC = 0.
      SELECT SINGLE VBELN INTO @DATA(LV_VBELN) FROM LIKP WHERE VBELN = @LS_ALV-DO_NUM.
      IF SY-SUBRC = 0.
        SET PARAMETER ID 'VL' FIELD LS_ALV-DO_NUM.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE TEXT-E01 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "on_double_click
ENDCLASS.

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
  SELECT-OPTIONS: S_VBELN FOR LIKP-VBELN,
                  S_ERDAT FOR ZSDSSDT024-ERDAT,
                  S_ERZET FOR ZSDSSDT024-ERZET.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS: S_LFART FOR LIKP-LFART.
SELECTION-SCREEN END OF BLOCK S01.

SELECTION-SCREEN BEGIN OF BLOCK S02 WITH FRAME TITLE TEXT-002.
* PARAMETERS:
  PARAMETERS: RB_OPT1 RADIOBUTTON GROUP rd1 DEFAULT 'X',
              RB_OPT2 RADIOBUTTON GROUP rd1,
              RB_OPT3 RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF BLOCK S02.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  S_ERDAT-LOW = |{ SY-DATUM+0(4) }{ SY-DATUM+4(2) }01|.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = S_ERDAT-LOW
    IMPORTING
      LAST_DAY_OF_MONTH = S_ERDAT-HIGH.
  APPEND S_ERDAT.

  S_LFART-LOW   = 'ZD01'.
  S_LFART-HIGH  = 'ZD07'.
  APPEND S_LFART.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
  PERFORM GET_DATA.
  IF GT_ALV[] IS INITIAL.
    MESSAGE 'Data not found.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  PERFORM F_ALV_DISPLAY.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& GET DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  SELECT * FROM ZSDSSDT024
    INNER JOIN LIKP ON ZSDSSDT024~DO_NUM = LIKP~VBELN
    INNER JOIN KNA1 ON LIKP~KUNNR = KNA1~KUNNR
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV
    WHERE ZSDSSDT024~DO_NUM IN S_VBELN
      AND ZSDSSDT024~ERDAT  IN S_ERDAT
      AND ZSDSSDT024~ERZET  IN S_ERZET.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& Display ALV
*&---------------------------------------------------------------------*
FORM F_ALV_DISPLAY .
  DATA:
    ##NEEDED
    LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT,
    LS_ALVTABLE         TYPE REF TO CL_SALV_TABLE,
    LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
    LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
    LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
    LF_TEXT             TYPE STRING,
    LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
    LS_KEY              TYPE SALV_S_LAYOUT_KEY,
    LS_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
    LS_FUNCTIONS_LIST   TYPE REF TO CL_SALV_FUNCTIONS_LIST,
    LO_VBELN            TYPE REF TO CL_SALV_COLUMN_TABLE,
    LO_EVENTS           TYPE REF TO CL_SALV_EVENTS_TABLE.

  FIELD-SYMBOLS <LFS_TABLE> TYPE ANY TABLE.
  ASSIGN GT_ALV TO <LFS_TABLE>.

*Initialize ref to cl_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = LS_ALVTABLE
        CHANGING
          T_TABLE      = <LFS_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
*      MESSAGE TEXT-E01 TYPE 'E'.
  ENDTRY.

  LR_COLUMNS = LS_ALVTABLE->GET_COLUMNS( ).
  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

  TRY.
      LO_VBELN ?= LR_COLUMNS->GET_COLUMN( 'DO_NUM' ).
      LO_VBELN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
    CATCH CX_SALV_NOT_FOUND.                            "#EC NO_HANDLER
  ENDTRY.

  "Event
  LO_EVENTS = LS_ALVTABLE->GET_EVENT( ).
  CREATE OBJECT GO_EVENTS.
  SET HANDLER GO_EVENTS->ON_LINK_CLICK FOR LO_EVENTS.

* Set column
  PERFORM F_DETAIL_COLUMN_SETTINGS_REPT USING LR_COLUMNS.

  LS_DISPLAY_SETTINGS = LS_ALVTABLE->GET_DISPLAY_SETTINGS( ).
  LS_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( 'X' ).
  LS_DISPLAY_SETTINGS->SET_LIST_HEADER( SY-TITLE ).

  LS_FUNCTIONS_LIST = LS_ALVTABLE->GET_FUNCTIONS( ).
  LS_FUNCTIONS_LIST->SET_ALL( ).

  LS_KEY-REPORT = SY-CPROG.
  LS_KEY-HANDLE = '0001'.
  LS_LAYOUT = LS_ALVTABLE->GET_LAYOUT( ).
  LS_LAYOUT->SET_KEY( LS_KEY ).
  LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
  LS_LAYOUT->SET_DEFAULT( 'X' ).


* Top_of_list
  CREATE OBJECT LR_CONTENT.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT   = SY-TITLE ).

  LF_TEXT = TEXT-T01.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 2
    COLUMN = 2
    TEXT   = SY-DATUM ).

  LF_TEXT = TEXT-T02.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 3
    COLUMN = 2
    TEXT   = SY-UNAME ).

  "total records
  DATA(LV_COUNT) = LINES( GT_ALV ).
  LF_TEXT = TEXT-T03.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 4
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 4
    COLUMN = 2
    TEXT   = LV_COUNT ).


  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).
  LS_ALVTABLE->SET_TOP_OF_LIST( LR_CONTENT ).
*
* Display Table
  LS_ALVTABLE->DISPLAY( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DETAIL_COLUMN_SETTINGS_REPT
*&---------------------------------------------------------------------*
FORM F_DETAIL_COLUMN_SETTINGS_REPT  USING UR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE,
        LF_S_TEXT TYPE SCRTEXT_S,
        LF_M_TEXT TYPE SCRTEXT_M,
        LF_L_TEXT TYPE SCRTEXT_L.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'OBJNR' ).
      LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
  ENDTRY.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'MANDT' ).
      LR_COLUMN->SET_VISIBLE( VALUE = SPACE ).
    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
  ENDTRY.

  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C01.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'ERDAT' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.


  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C02.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'ERZET' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.
ENDFORM.
