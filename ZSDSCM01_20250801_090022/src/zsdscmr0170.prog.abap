*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0170
*  Creation Date      : 27.05.2025
*  Author             : Boonpipop Ch. (SDS)
*  Add-on ID          : N/A
*  Description        : Report for Warranty Letter
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
*& Report ZSDSCMR0170
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSCMR0170.
CLASS LCL_HANDLE_EVENTS DEFINITION DEFERRED.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: ZSDSCMT005,
        VBRK.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF TYP_ALV.
         INCLUDE TYPE ZSDSCMS015.
TYPES:   MATNR   TYPE VBRP-MATNR,     "material
         ARKTX   TYPE VBRP-ARKTX,     "Description
         POSNR   TYPE VBRP-POSNR,     "Item
         NAME_SP TYPE CHAR120,        "Sold to name
         DO_NUM  TYPE LIPS-VBELN,
         DO_ITM  TYPE LIPS-POSNR,
         SERNR   TYPE OBJK-SERNR.
TYPES: END OF TYP_ALV.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA:
*-> internal tables
  GT_ALV TYPE STANDARD TABLE OF TYP_ALV,
*-> range
*-> work areas
  GS_ALV TYPE TYP_ALV.
*-> variables
*-> reference
DATA: GT_LOG_HEADER  TYPE BALHDR_T,
      GT_MSGH        TYPE BAL_T_MSGH,
      GS_LOG_H       TYPE BAL_S_LOG,
      GS_LOG_FILTER  TYPE BAL_S_LFIL,
      GS_R_SUBOBJECT TYPE BAL_S_SUB,
      GS_R_OBJECT    TYPE BAL_S_OBJ,
      GS_R_EXTNUMBER TYPE BAL_S_EXTN,
      GS_MSG         TYPE BAL_S_MSG,
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

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
* SELECT-OPTIONS:
*SELECTION-SCREEN END OF BLOCK s01.
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS:

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
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
    SELECT-OPTIONS: S_POSID  FOR ZSDSCMT005-PROJWBS,
                    S_WL_ID  FOR ZSDSCMT005-WL_ID,
                    S_VBELN  FOR VBRK-VBELN,
                    S_DOCDAT FOR ZSDSCMT005-DOCDATE.

  SELECTION-SCREEN END OF BLOCK B01.
*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
  PERFORM F_GET_DATA.
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
*& Form f_get_data
*&---------------------------------------------------------------------*
*& Get Warranty Data
*&---------------------------------------------------------------------*
FORM F_GET_DATA .

  DATA: LV_WBS_ELEMENT TYPE PS_POSID.

  SELECT
    ZSDSCMT005~WL_ID,           "Warranty Letter ID
    ZSDSCMT005~PROJWBS,         "Work Breakdown Structure Element (WBS Element)
    ZSDSCMT005~DOCDATE,         "Document Date (Date Received/Sent)
    ZSDSCMT005~WLSTARTDATE,     "Warranty Start Date
    ZSDSCMT005~STATUS,          "Status
    ZSDSCMT005~PRINTED,         "Printed
    ZSDSCMT005~REQUESTER,       "Requester
    ZSDSCMT005~REQEMAIL,        "Requester Email
    ZSDSCMT005~EMPID,           "Personnel Number
    ZSDSCMT005~R_WARR1,         "Single-Character Flag
    ZSDSCMT005~R_WARR2,         "Single-Character Flag
    ZSDSCMT005~R_WARR2_DAYS,    "Count Parameters
    ZSDSCMT005~R_WARR3,         "Single-Character Flag
    ZSDSCMT005~R_WARR3_DATE,    "Warranty Start Date
    ZSDSCMT005~ERDAT,           "Create Date
    ZSDSCMT005~ERTIM,           "Create Time
    ZSDSCMT005~ERNAM,           "Create User
    ZSDSCMT005~AEDAT,           "Update Date
    ZSDSCMT005~AETIM,           "Update Time
    ZSDSCMT005~AENAM,           "Update User
    ZSDSCMT007~INV_NO,          "Billing Document
    VBRP~POSNR,                 "Item No.
    VBRP~MATNR,                 "Material
    VBRP~FKIMG,                 "Actual billed quantity
    VBRP~ARKTX,                 "Short Material Description
    VBRK~FKDAT,                 "Billing Date
    VBRK~KUNAG,                 "Sold-to Party
    VBRP~PS_PSP_PNR,            "WBS Element
    VBRP~VKGRP,                 "Sales Group
    TVGRT~BEZEI,                "Sales Group (desc)
    VBPA~PERNR,                 "Personnel Number
    PA0001~ENAME                "Formatted Name of Employee or Applicant
    INTO TABLE @DATA(LT_WARRANTY)
    FROM ZSDSCMT005                                   "Warranty Header
    INNER JOIN ZSDSCMT007                             "Mapping Invoice number
            ON ZSDSCMT007~WL_ID = ZSDSCMT005~WL_ID
    INNER JOIN VBRP                                   "Invoice Master
            ON VBRP~VBELN = ZSDSCMT007~INV_NO
    LEFT OUTER JOIN VBRK
            ON VBRP~VBELN EQ VBRK~VBELN
    LEFT OUTER JOIN TVGRT
            ON VBRP~VKGRP = TVGRT~VKGRP AND TVGRT~SPRAS = 'E'
    LEFT OUTER JOIN VBPA
            ON VBRP~VBELN = VBPA~VBELN AND PARVW = 'VE'
    LEFT OUTER JOIN PA0001
            ON VBPA~PERNR = PA0001~PERNR
   WHERE ZSDSCMT005~WL_ID     IN @S_WL_ID
     AND PROJWBS              IN @S_POSID
     AND ZSDSCMT007~INV_NO    IN @S_VBELN
     AND ZSDSCMT005~DOCDATE   IN @S_DOCDAT
     AND VBRK~VF_STATUS <> 'C' "Canceled
     AND VBRK~VBTYP <> 'N'.    "Invoice Cancellation
  IF SY-SUBRC = 0.

    "Get Sold-to
    SELECT
      VBPA~VBELN,
      VBPA~KUNNR,
      VBPA~ADRNR,
      ADRC~NAME1,
      ADRC~NAME2,
      ADRC~NAME3,
      ADRC~NAME4
    FROM
      VBPA
    LEFT OUTER JOIN ADRC ON ADRC~ADDRNUMBER = VBPA~ADRNR AND NATION = @SPACE
    INTO TABLE @DATA(LT_VBPA)
    FOR ALL ENTRIES IN @LT_WARRANTY
    WHERE VBPA~VBELN = @LT_WARRANTY-INV_NO
      AND VBPA~PARVW = 'AG'.
    IF SY-SUBRC = 0.
      SORT LT_VBPA BY VBELN.
    ENDIF.

    SELECT POSID, STUFE, POST1 INTO TABLE @DATA(LT_POSID) "#EC CI_NO_TRANSFORM
      FROM PRPS
       FOR ALL ENTRIES IN @LT_WARRANTY
     WHERE POSID = @LT_WARRANTY-PROJWBS.
    IF SY-SUBRC = 0.
      SORT LT_POSID BY POSID.
    ENDIF.

    "Get Do
    SELECT VBFA~VBELV,    "Do Number
           VBFA~POSNV,    "Do Item
           VBFA~VBELN,
           VBFA~POSNN,
           OBJK~SERNR
      FROM VBFA
      INNER JOIN SER01 ON ( SER01~LIEF_NR = VBFA~VBELV AND
                            SER01~POSNR   = VBFA~POSNV )
      INNER JOIN OBJK  ON OBJK~OBKNR = SER01~OBKNR
      INTO TABLE @DATA(LT_SERIAL)
      FOR ALL ENTRIES IN @LT_WARRANTY[]
      WHERE VBFA~VBTYP_N = 'M' "Invoice
        AND VBFA~VBTYP_V = 'J'
        AND ( VBFA~VBELN = @LT_WARRANTY-INV_NO AND
              VBFA~POSNN = @LT_WARRANTY-POSNR ).
    IF SY-SUBRC = 0.
      SORT LT_SERIAL BY VBELN POSNN SERNR.
    ENDIF.

    "Mapping to Output
    LOOP AT LT_WARRANTY INTO DATA(LS_WARRANTY).

      CLEAR GS_ALV.
      ASSIGN GS_ALV TO  FIELD-SYMBOL(<LFS_ALV>).

      <LFS_ALV>-WL_ID       =  LS_WARRANTY-WL_ID.
      <LFS_ALV>-VBELN       =  LS_WARRANTY-INV_NO.
      <LFS_ALV>-FKDAT       =  LS_WARRANTY-FKDAT.
      <LFS_ALV>-WLSTARTDATE =  LS_WARRANTY-WLSTARTDATE.
      <LFS_ALV>-STATUS      =  LS_WARRANTY-STATUS.
      PERFORM GET_STATUS_TEXT USING LS_WARRANTY-STATUS CHANGING <LFS_ALV>-STATUS_TEXT.

      <LFS_ALV>-VKGRP         = LS_WARRANTY-VKGRP.
      <LFS_ALV>-VKGRP_NAME    = LS_WARRANTY-BEZEI.
      <LFS_ALV>-SALES_PERSON  = LS_WARRANTY-PERNR.
      <LFS_ALV>-SALES_NAME    = LS_WARRANTY-ENAME.
      <LFS_ALV>-PROJWBS       = LS_WARRANTY-PROJWBS.
      <LFS_ALV>-POSNR         = LS_WARRANTY-POSNR.
      <LFS_ALV>-MATNR         = LS_WARRANTY-MATNR.
      <LFS_ALV>-ARKTX         = LS_WARRANTY-ARKTX.

      READ TABLE LT_POSID ASSIGNING FIELD-SYMBOL(<LFS_WBSPROJ>) WITH KEY POSID = LS_WARRANTY-PROJWBS.
      IF SY-SUBRC = 0.
        <LFS_ALV>-PROJWBS_NAME = <LFS_WBSPROJ>-POST1.
      ENDIF.

      CLEAR: LV_WBS_ELEMENT.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          INPUT  = LS_WARRANTY-PS_PSP_PNR
        IMPORTING
          OUTPUT = LV_WBS_ELEMENT.
      <LFS_ALV>-WBS          = LV_WBS_ELEMENT.

      "Get sold-to data
      READ TABLE LT_VBPA INTO DATA(LS_VBPA)
                         WITH KEY VBELN = LS_WARRANTY-INV_NO
                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LFS_ALV>-KUNAG =  LS_VBPA-KUNNR.
        <LFS_ALV>-NAME1 =  LS_VBPA-NAME1.
        CONCATENATE LS_VBPA-NAME1
                    LS_VBPA-NAME2
                    LS_VBPA-NAME3
                    LS_VBPA-NAME4
                    INTO <LFS_ALV>-NAME_SP.
      ENDIF.

      READ TABLE LT_SERIAL TRANSPORTING NO FIELDS
                           WITH KEY
                              VBELN = LS_WARRANTY-INV_NO
                              POSNN = LS_WARRANTY-POSNR
                           BINARY SEARCH.
      IF SY-SUBRC = 0.
        DATA(LV_TABIX) = SY-TABIX.
        LOOP AT LT_SERIAL INTO DATA(LS_SERIAL) FROM LV_TABIX.
          IF LS_SERIAL-VBELN <> LS_WARRANTY-INV_NO AND
             LS_SERIAL-POSNN <> LS_WARRANTY-POSNR.
            EXIT.
          ENDIF.
          <LFS_ALV>-SERNR     = LS_SERIAL-SERNR.
          <LFS_ALV>-DO_NUM    = LS_SERIAL-VBELV.
          <LFS_ALV>-DO_ITM    = LS_SERIAL-POSNV.

          APPEND GS_ALV TO GT_ALV.
        ENDLOOP.
      ELSE.
        APPEND GS_ALV TO GT_ALV.
      ENDIF.

      UNASSIGN <LFS_ALV>.
      CLEAR: LS_VBPA,
             LS_WARRANTY.
    ENDLOOP.

  ENDIF.
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

*  LS_ALVTABLE->GET_COLUMNS( )->SET_COLUMN_POSITION


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
*& Form get_status_text
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_STATUS_TEXT  USING    PV_STATUS TYPE CHAR1
                      CHANGING PV_STATUS_TXT TYPE TEXT15.
  DATA: LT_FIXED_VALUE TYPE STANDARD TABLE OF DD07V.
  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      NAME          = 'ZSDSDM_WL_STATUS'
      LANGU         = 'E'
    TABLES
      DD07V_TAB     = LT_FIXED_VALUE
    EXCEPTIONS
      ILLEGAL_INPUT = 1
      OTHERS        = 2.
  IF SY-SUBRC = 0.
    READ TABLE LT_FIXED_VALUE ASSIGNING FIELD-SYMBOL(<LFS_VALUE>) WITH KEY DOMVALUE_L = PV_STATUS.
    IF SY-SUBRC = 0.
      PV_STATUS_TXT = <LFS_VALUE>-DDTEXT.
    ENDIF.
  ENDIF.
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
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'NAME1' ).
      LR_COLUMN->SET_VISIBLE( VALUE = SPACE ).
    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
  ENDTRY.


*  TRY.
*      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'WL_ID' ).
*
*      LR_COLUMN->SET_CO
*    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
*  ENDTRY.



  "Customer Name
  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C01.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'NAME_SP' ).

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

TYPES: BEGIN OF LTYP_ALV.
         INCLUDE TYPE ZSDSCMS015.
TYPES:   MATNR   TYPE VBRP-MATNR,     "material
         ARKTX   TYPE VBRP-ARKTX,     "Description
         POSNR   TYPE VBRP-POSNR,     "Item
         NAME_SP TYPE CHAR120.        "Sold to name
TYPES: END OF LTYP_ALV.

  "Position Set
  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'WL_ID'
                                    POSITION   =  1 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'VBELN'
                                    POSITION   =  2 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'POSNR'
                                    POSITION   =  3 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'FKDAT'
                                    POSITION   =  4 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'WLSTARTDATE'
                                    POSITION   =  5 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'STATUS'
                                    POSITION   =  6 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'STATUS_TEXT'
                                    POSITION   =  7 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'KUNAG'
                                    POSITION   =  8 ).

  UR_COLUMNS->SET_COLUMN_POSITION(  COLUMNNAME =  'NAME_SP'
                                    POSITION   =  9 ).




ENDFORM.
