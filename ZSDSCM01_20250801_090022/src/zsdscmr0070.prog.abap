*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0070
*  Creation Date      : 18.02.2025
*  Author             : Boonpipop Ch.
*  Add-on ID          : N/A
*  Description        : Service Order/Confrim Order - Error Log
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
*& Report ZSDSCMR0070
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSCMR0070 MESSAGE-ID ZSDSCM01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: CRMS4D_SERV_H.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF TYP_ALV,
    OBJTYPE_H      TYPE CRMS4D_SERV_H-OBJTYPE_H,
    OBJTYPE_DESC   TYPE CRMC_SUBOB_CAT_T-S_DESCRIPTION,
    OBJECT_ID      TYPE CRMS4D_SERV_H-OBJECT_ID,
    PROCESS_TYPE   TYPE CRMS4D_SERV_H-PROCESS_TYPE,
    PO_NUMBER_SOLD TYPE CRMS4D_SERV_H-PO_NUMBER_SOLD,
*    STAT_ERROR   TYPE CRMS4D_SERV_H-STAT_ERROR,
    STAT_ERROR     TYPE BAPIRET2-TYPE,
    MESSAGE        TYPE BAPIRET2-MESSAGE,
    TIME_STMP      TYPE BALTIMSTMP,
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
      GT_ALV         TYPE STANDARD TABLE OF TYP_ALV.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:
  BEGIN OF GC_TRAN_CAT,
    ORDER TYPE CRMS4D_SERV_H-OBJTYPE_H VALUE 'BUS2000116',
    CONFM TYPE CRMS4D_SERV_H-OBJTYPE_H VALUE 'BUS2000117',
  END OF GC_TRAN_CAT,

  GC_MSG_OBJECT TYPE BALOBJ_D       VALUE 'CRM_DOCUMENT',
  GC_MSG_CLASS  TYPE CRMT_MSG_CLASS VALUE 'SINGLE',
  GC_MSG_TYPE   TYPE BAPIRET2-TYPE  VALUE 'E'.

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
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS : S_DOCID   FOR CRMS4D_SERV_H-OBJECT_ID,
                   S_OJTYP   FOR CRMS4D_SERV_H-OBJTYPE_H NO-EXTENSION MODIF ID M01.   "Object Type
SELECTION-SCREEN END OF BLOCK B01.

SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-B02.
  PARAMETERS: R_SERV RADIOBUTTON GROUP RAD1 MODIF ID 001 DEFAULT 'X' USER-COMMAND U01,
              R_CONF RADIOBUTTON GROUP RAD1 MODIF ID 001.
SELECTION-SCREEN END OF BLOCK B02.


*OBJTYPE_H

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    "Hide High value
    IF SCREEN-NAME EQ 'S_OJTYP-HIGH'.
      SCREEN-ACTIVE     = 0.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'M01'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 = '001'.
      IF R_SERV = ABAP_TRUE.
        REFRESH S_OJTYP.
        S_OJTYP = VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = GC_TRAN_CAT-ORDER ).
        APPEND S_OJTYP.
      ELSE.
        REFRESH S_OJTYP.
        S_OJTYP = VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = GC_TRAN_CAT-CONFM ).
        APPEND S_OJTYP.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
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
    MESSAGE E046.
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

  DATA: LT_LOG_HEADER  TYPE BALHDR_T,
        LT_MSGH        TYPE BAL_T_MSGH,
        LS_LOG_H       TYPE BAL_S_LOG,
        LS_LOG_FILTER  TYPE BAL_S_LFIL,
        LS_R_SUBOBJECT TYPE BAL_S_SUB,
        LS_R_OBJECT    TYPE BAL_S_OBJ,
        LS_R_EXTNUMBER TYPE BAL_S_EXTN,
        LS_MSG         TYPE BAL_S_MSG.

  SELECT
    CRMS4D_SERV_H~OBJECT_ID,
    CRMS4D_SERV_H~HEADER_GUID,
    CRMS4D_SERV_H~OBJTYPE_H,
    CRMS4D_SERV_H~STAT_ERROR,
    CRMS4D_SERV_H~PROCESS_TYPE,
    CRMS4D_SERV_H~PO_NUMBER_SOLD,
    CRMC_SUBOB_CAT_T~S_DESCRIPTION
    INTO TABLE @DATA(LT_SERV_H)
    FROM CRMS4D_SERV_H
    INNER JOIN CRMC_SUBOB_CAT_T
    ON CRMS4D_SERV_H~OBJTYPE_H = CRMC_SUBOB_CAT_T~SUBOBJ_CATEGORY
    WHERE CRMS4D_SERV_H~OBJECT_ID   IN @S_DOCID
      AND CRMS4D_SERV_H~OBJTYPE_H   IN @S_OJTYP
      AND CRMS4D_SERV_H~STAT_ERROR  = @ABAP_TRUE
      AND CRMC_SUBOB_CAT_T~LANGU = 'E'.
  IF SY-SUBRC = 0.
    SORT LT_SERV_H BY OBJECT_ID.
  ENDIF.

  LOOP AT LT_SERV_H INTO DATA(LS_SERV_H).

    CLEAR: LS_LOG_FILTER,
           LT_LOG_HEADER[],
           LT_MSGH[].

* Application log object: CRM document
    LS_R_OBJECT-SIGN = 'I'.
    LS_R_OBJECT-OPTION = 'EQ'.
    LS_R_OBJECT-LOW = GC_MSG_OBJECT.
    APPEND LS_R_OBJECT TO LS_LOG_FILTER-OBJECT.

* Application log subobject: As specified
    LS_R_SUBOBJECT-SIGN = 'I'.
    LS_R_SUBOBJECT-OPTION = 'EQ'.
    LS_R_SUBOBJECT-LOW = GC_MSG_CLASS.
    APPEND LS_R_SUBOBJECT TO LS_LOG_FILTER-SUBOBJECT.

* Fill the extnumber
    LS_R_EXTNUMBER-SIGN = 'I'.
    LS_R_EXTNUMBER-OPTION = 'EQ'.
    LS_R_EXTNUMBER-LOW = LS_SERV_H-HEADER_GUID.
    APPEND LS_R_EXTNUMBER TO LS_LOG_FILTER-EXTNUMBER.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        I_S_LOG_FILTER     = LS_LOG_FILTER
      IMPORTING
        E_T_LOG_HEADER     = LT_LOG_HEADER
      EXCEPTIONS
        LOG_NOT_FOUND      = 1
        NO_FILTER_CRITERIA = 2.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        I_T_LOG_HEADER = LT_LOG_HEADER
      IMPORTING
        E_T_MSG_HANDLE = LT_MSGH
      EXCEPTIONS
        OTHERS         = 1.
    IF SY-SUBRC <> 0.
      CONTINUE. "message
    ENDIF.

    LOOP AT LT_MSGH INTO DATA(LS_MSGH).
      CLEAR LS_MSG.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          I_S_MSG_HANDLE = LS_MSGH
        IMPORTING
          E_S_MSG        = LS_MSG
        EXCEPTIONS
          LOG_NOT_FOUND  = 1
          MSG_NOT_FOUND  = 2
          OTHERS         = 3.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ELSE.
        "Append data to ALV
        APPEND INITIAL LINE TO GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
        <LFS_ALV>-OBJECT_ID       = LS_SERV_H-OBJECT_ID.
        <LFS_ALV>-OBJTYPE_H       = LS_SERV_H-OBJTYPE_H.
        <LFS_ALV>-PROCESS_TYPE    = LS_SERV_H-PROCESS_TYPE.
        <LFS_ALV>-PO_NUMBER_SOLD  = LS_SERV_H-PO_NUMBER_SOLD.
*        <LFS_ALV>-STAT_ERROR  = LS_SERV_H-STAT_ERROR.
        <LFS_ALV>-OBJTYPE_DESC  = LS_SERV_H-S_DESCRIPTION.
        <LFS_ALV>-STAT_ERROR  = GC_MSG_TYPE.
        <LFS_ALV>-TIME_STMP   = LS_MSG-TIME_STMP.
        MESSAGE ID LS_MSG-MSGID TYPE  LS_MSG-MSGTY NUMBER LS_MSG-MSGNO
        INTO <LFS_ALV>-MESSAGE WITH LS_MSG-MSGV1
                                    LS_MSG-MSGV2
                                    LS_MSG-MSGV3
                                    LS_MSG-MSGV4.

      ENDIF.

    ENDLOOP.
  ENDLOOP.


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
    LS_FUNCTIONS_LIST   TYPE REF TO CL_SALV_FUNCTIONS_LIST.

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

  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C01.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'VBELN' ).
*      LR_COLUMN->SET_CURRENCY_COLUMN( VALUE = 'WAERS' ).

      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.
ENDFORM.
