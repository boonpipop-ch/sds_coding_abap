*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0420_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_HIERSEQ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DISPLAY_HIERSEQ .

  CHECK RB_M1 EQ ABAP_TRUE OR RB_M2 EQ ABAP_TRUE.

  DATA:
    LT_BINDING          TYPE SALV_T_HIERSEQ_BINDING,
    LS_BINDING          TYPE SALV_S_HIERSEQ_BINDING,
    LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_HIERSEQ,
    LR_LEVEL            TYPE REF TO CL_SALV_HIERSEQ_LEVEL,
    LR_EVENTS           TYPE REF TO CL_SALV_EVENTS_HIERSEQ,
*... set list title
    LR_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
    L_TITLE             TYPE LVC_TITLE.

*... create the binding information between master and slave
  LS_BINDING-MASTER = 'VBELN_KEY'.
  LS_BINDING-SLAVE  = 'VBELN_KEY'.
  APPEND LS_BINDING TO LT_BINDING.
*  LS_BINDING-MASTER = 'MATNR_KEY'.
*  LS_BINDING-SLAVE  = 'MATNR_KEY'.
*  APPEND LS_BINDING TO LT_BINDING.

*... create an ALV hierseq table
  TRY.
      CL_SALV_HIERSEQ_TABLE=>FACTORY(
        EXPORTING
          T_BINDING_LEVEL1_LEVEL2 = LT_BINDING
        IMPORTING
          R_HIERSEQ               = GR_HIERSEQ
        CHANGING
          T_TABLE_LEVEL1          = GT_MASTER
          T_TABLE_LEVEL2          = GT_SLAVE ).
      ##NO_HANDLER
    CATCH CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND.
  ENDTRY.

*... *** MASTER Settings ***
  TRY.
      LR_COLUMNS = GR_HIERSEQ->GET_COLUMNS( 1 ).
    ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... optimize the columns
  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

*... set the columns technical
*  TRY.
*      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MANDT' ).
*      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
*    ##NO_HANDLER   CATCH CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND.
*  ENDTRY.

*... set expand column
  TRY.
      LR_COLUMNS->SET_EXPAND_COLUMN( 'EXPAND' ).
    ##NO_HANDLER    CATCH CX_SALV_DATA_ERROR CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... set items expanded
  TRY.
      LR_LEVEL = GR_HIERSEQ->GET_LEVEL( 1 ).
    ##NO_HANDLER    CATCH CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND.
  ENDTRY.

  LR_LEVEL->SET_ITEMS_EXPANDED( ).
*.Set structure1
  PERFORM F_SET_DETAIL_STRUC USING LR_COLUMNS  'ZSDSFIS125'.

*... §4.1 set exception column
  TRY.
      LR_COLUMNS->SET_EXCEPTION_COLUMN( 'EXCEPTION' ).
    CATCH CX_SALV_DATA_ERROR.                           "#EC NO_HANDLER
  ENDTRY.

*... *** SLAVE Settings ***
  TRY.
      LR_COLUMNS = GR_HIERSEQ->GET_COLUMNS( 2 ).
    ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
  ENDTRY.

*... optimize the columns
  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

*..Set structure1
  PERFORM F_SET_DETAIL_STRUC USING LR_COLUMNS  'ZSDSFIS125'.

*... §6 register to the events of cl_salv_hierseq_table

  LR_EVENTS = GR_HIERSEQ->GET_EVENT( ).

  CREATE OBJECT GR_EVENTS.

*... §6.1 register to the event
  SET HANDLER GR_EVENTS->ON_TOP_OF_PAGE FOR LR_EVENTS.

  LR_DISPLAY_SETTINGS = GR_HIERSEQ->GET_DISPLAY_SETTINGS( ).
  LR_DISPLAY_SETTINGS->SET_LIST_HEADER( L_TITLE ).
  LR_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( VALUE = ABAP_TRUE ).

*... §7 display the table
  GR_HIERSEQ->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DISPLAY_ALV .

  DATA:
    LR_FUNCTIONS        TYPE REF TO CL_SALV_FUNCTIONS_LIST,
    LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
    LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
    LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
    LF_TEXT             TYPE CHAR80,
    LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
    LS_KEY              TYPE SALV_S_LAYOUT_KEY,
    LS_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
    LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT,
    LR_ICON             TYPE REF TO CL_SALV_FORM_ICON.

*Initialize ref to cl_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = GR_TABLE
        CHANGING
          T_TABLE      = GT_OUTTAB ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
      MESSAGE TEXT-E01 TYPE 'E'.
  ENDTRY.

*... §3 Functions
*... §3.1 activate ALV generic Functions
  LR_FUNCTIONS = GR_TABLE->GET_FUNCTIONS( ).
  LR_FUNCTIONS->SET_ALL( ABAP_TRUE ).

*... set column
  LR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  PERFORM F_SET_COLUMNS_TECHNICAL USING LR_COLUMNS.
  PERFORM F_SET_COLUMNS USING LR_COLUMNS.

  LS_DISPLAY_SETTINGS = GR_TABLE->GET_DISPLAY_SETTINGS( ).
  LS_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( 'X' ).
  LS_DISPLAY_SETTINGS->SET_LIST_HEADER( SY-TITLE ).

*
  LS_KEY-REPORT = SY-CPROG.
  LS_KEY-HANDLE = '0001'.
  LS_LAYOUT = GR_TABLE->GET_LAYOUT( ).
  LS_LAYOUT->SET_KEY( LS_KEY ).
  LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
  LS_LAYOUT->SET_DEFAULT( 'X' ).
  CREATE OBJECT LR_CONTENT.
* Top_of_list
*  IF RB_M3 EQ ABAP_TRUE.
*
*    LR_LABEL = LR_CONTENT->CREATE_LABEL(
*      ROW    = 1
*      COLUMN = 1
*      TEXT   = TEXT-H07 ).
*  ELSE.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT   = TEXT-H08 ).
*  ENDIF.
*///////////////////////
  LF_TEXT = |{ TEXT-005 }:|.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW     = 2
    COLUMN  = 1
    TEXT    = LF_TEXT
    TOOLTIP = SPACE ).

  LF_TEXT = P_BUKRS.
  LR_TEXT = LR_CONTENT->CREATE_TEXT(
    ROW     = 2
    COLUMN  = 2
    TEXT    = LF_TEXT
    TOOLTIP = SPACE ).

*  IF RB_M3 EQ ABAP_TRUE.
*    LF_TEXT = |{ TEXT-011 }:|.
*    LR_LABEL = LR_CONTENT->CREATE_LABEL(
*      ROW     = 3
*      COLUMN  = 1
*      TEXT    = LF_TEXT
*      TOOLTIP = SPACE ).
*
*    WRITE P_RPDAT TO LF_TEXT MM/DD/YYYY .
*    LR_TEXT = LR_CONTENT->CREATE_TEXT(
*      ROW     = 3
*      COLUMN  = 2
*      TEXT    = LF_TEXT
*      TOOLTIP = SPACE ).
*
*    LF_TEXT = |{ TEXT-H03 }:|.
*    LR_LABEL = LR_CONTENT->CREATE_LABEL(
*      ROW     = 4
*      COLUMN  = 1
*      TEXT    = LF_TEXT
*      TOOLTIP = SPACE ).
*
*    LF_TEXT = |[{ CB_M1 }] { TEXT-H04 } [{ CB_M2 }] { TEXT-H05 } [{ CB_M3 }] { TEXT-H06 } |.
*    LR_TEXT = LR_CONTENT->CREATE_TEXT(
*      ROW     = 4
*      COLUMN  = 2
*      TEXT    = LF_TEXT
*      TOOLTIP = SPACE ).
*  ELSE.
  LF_TEXT = |{ TEXT-009 }:|.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW     = 3
    COLUMN  = 1
    TEXT    = LF_TEXT
    TOOLTIP = SPACE ).

  WRITE P_RUNDT TO LF_TEXT MM/DD/YYYY .
  LR_TEXT = LR_CONTENT->CREATE_TEXT(
    ROW     = 3
    COLUMN  = 2
    TEXT    = LF_TEXT
    TOOLTIP = SPACE ).

*<<< Begin of insertion CH03
  LF_TEXT = |{ TEXT-018 }:|.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW     = 4
    COLUMN  = 1
    TEXT    = LF_TEXT
    TOOLTIP = SPACE ).

  WRITE P_BUDAT TO LF_TEXT MM/DD/YYYY .
  LR_TEXT = LR_CONTENT->CREATE_TEXT(
    ROW     = 4
    COLUMN  = 2
    TEXT    = LF_TEXT
    TOOLTIP = SPACE ).
*>>> End of insertion CH03

*  ENDIF.
*//////////////////////
  LF_TEXT = 'Product Warranty'(013).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 5
    COLUMN = 1
    TEXT   = LF_TEXT ).

  CREATE OBJECT LR_ICON
    EXPORTING
      ICON    = ICON_LED_YELLOW "ICON_OO_CLASS    "#EC NOTEXT
      TOOLTIP = 'Product Warranty'(013).     "#EC NOTEXT

  CALL METHOD LR_CONTENT->SET_ELEMENT
    EXPORTING
      ROW       = 5
      COLUMN    = 2
      R_ELEMENT = LR_ICON.
*..............
  LF_TEXT = 'Actual Cost'(014).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 6
    COLUMN = 1
    TEXT   = LF_TEXT ).

  CREATE OBJECT LR_ICON
    EXPORTING
      ICON    = ICON_BW_APD_TARGET     "#EC NOTEXT
      TOOLTIP = 'Actual Cost'(014).     "#EC NOTEXT

  CALL METHOD LR_CONTENT->SET_ELEMENT
    EXPORTING
      ROW       = 6
      COLUMN    = 2
      R_ELEMENT = LR_ICON.


  LF_TEXT = 'Warranty Provision'(015).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 7
    COLUMN = 1
    TEXT   = LF_TEXT ).

  CREATE OBJECT LR_ICON
    EXPORTING
      ICON    = ICON_LED_GREEN  "ICON_OO_OBJECT     "#EC NOTEXT
      TOOLTIP = 'Warranty Provision'(015).     "#EC NOTEXT

  CALL METHOD LR_CONTENT->SET_ELEMENT
    EXPORTING
      ROW       = 7
      COLUMN    = 2
      R_ELEMENT = LR_ICON.


  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  GR_TABLE->SET_TOP_OF_LIST( LR_CONTENT ).

*... §4 display the table
  GR_TABLE->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&---------------------------------------------------------------------*
FORM F_SET_COLUMNS_TECHNICAL  USING IR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.

  DATA: LR_COLUMN TYPE REF TO CL_SALV_COLUMN.

  TRY.
      LR_COLUMN = IR_COLUMNS->GET_COLUMN( 'MANDT' ).
      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
    CATCH CX_SALV_NOT_FOUND.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_COLUMNS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&---------------------------------------------------------------------*
FORM F_SET_COLUMNS  USING IR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.

  DATA: LS_COLOR1 TYPE LVC_S_COLO,
        LS_COLOR2 TYPE LVC_S_COLO,
        LS_COLOR3 TYPE LVC_S_COLO.

  LS_COLOR1-COL = 7.
  LS_COLOR2-COL = 1.
  LS_COLOR3-COL = 5.

  TRY.
      IR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      TRY.
          IR_COLUMNS->SET_EXCEPTION_COLUMN( 'STATUS' ).
          ##NO_HANDLER
        CATCH CX_SALV_DATA_ERROR.
      ENDTRY.

*..Set structure1
      PERFORM F_SET_DETAIL_STRUC1 USING IR_COLUMNS LS_COLOR1 'ZSDSFIS125'.
      PERFORM F_SET_DETAIL_STRUC1 USING IR_COLUMNS LS_COLOR2 'ZSDSFIS126'.
      PERFORM F_SET_DETAIL_STRUC1 USING IR_COLUMNS LS_COLOR3 'ZSDSFIS127'.

      ##NO_HANDLER
    CATCH CX_SALV_METHOD_NOT_SUPPORTED.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_DETAIL_STRUC1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IR_COLUMNS
*&      --> LF_COLOR1
*&---------------------------------------------------------------------*
FORM F_SET_DETAIL_STRUC1   USING IR_COLUMNS   TYPE REF TO CL_SALV_COLUMNS_TABLE
                                 UF_COLOR     TYPE LVC_S_COLO
                                 UF_STRUCNAME TYPE STRING.

  DATA: LF_TABNAME   TYPE DDOBJNAME,
        LT_DFIES_TAB TYPE STANDARD TABLE OF DFIES,
        LR_COLUMN    TYPE REF TO CL_SALV_COLUMN_TABLE.

  LF_TABNAME = UF_STRUCNAME.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = LF_TABNAME
    TABLES
      DFIES_TAB      = LT_DFIES_TAB
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_DFIES_TAB ASSIGNING FIELD-SYMBOL(<L_DFIES_TAB>).
    TRY.
        LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( <L_DFIES_TAB>-FIELDNAME ).
        LR_COLUMN->SET_OPTIMIZED( ABAP_TRUE ).
        LR_COLUMN->SET_COLOR( UF_COLOR ).
        ##NO_HANDLER
      CATCH CX_SALV_DATA_ERROR.
        ##NO_HANDLER
      CATCH CX_SALV_NOT_FOUND.
        ##NO_HANDLER
      CATCH CX_SALV_METHOD_NOT_SUPPORTED.
    ENDTRY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SCR_DEFAULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SCR_DEFAULT .

  DATA: LV_ENDDA TYPE SY-DATUM.

  CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = SY-DATUM
    IMPORTING
      LAST_DAY_OF_MONTH = LV_ENDDA
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    P_RUNDT = LV_ENDDA.
*    P_RPDAT = LV_ENDDA.
    P_BUDAT = LV_ENDDA.     "CH03+
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SCREEN_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM F_SCREEN_CHECK .
*
*  CASE ABAP_TRUE.
*    WHEN RB_M1 OR RB_M2.
*      IF P_RUNDT IS INITIAL.
*        MESSAGE TEXT-E02 TYPE  'E'.
*        SET CURSOR FIELD 'P_RUNDT'.
*      ELSE.
*        DATA: LV_DATE TYPE SY-DATUM.
*        LV_DATE = P_RUNDT.
*        CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
*          EXPORTING
*            DAY_IN            = LV_DATE
*          IMPORTING
*            LAST_DAY_OF_MONTH = LV_DATE
*          EXCEPTIONS
*            DAY_IN_NO_DATE    = 1
*            OTHERS            = 2.
*        IF SY-SUBRC NE 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ELSE.
*          IF LV_DATE NE P_RUNDT.
*            MESSAGE TEXT-E02 TYPE 'E'.
*            SET CURSOR FIELD 'P_RUNDT'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    WHEN OTHERS.
*
*      IF P_RPDAT IS INITIAL.
*        MESSAGE TEXT-E02 TYPE  'E'.
*        SET CURSOR FIELD 'P_RPDAT'.
*      ELSE.
*
*        LV_DATE = P_RPDAT.
*        CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
*          EXPORTING
*            DAY_IN            = LV_DATE
*          IMPORTING
*            LAST_DAY_OF_MONTH = LV_DATE
*          EXCEPTIONS
*            DAY_IN_NO_DATE    = 1
*            OTHERS            = 2.
*        IF SY-SUBRC NE 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ELSE.
*          IF LV_DATE NE P_RPDAT.
*            MESSAGE TEXT-E02 TYPE 'E'.  SET CURSOR FIELD 'P_RPDAT'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*  ENDCASE.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_WAR_CLAIM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PREPARE_WAR_CLAIM .

  CHECK RB_M1 EQ ABAP_TRUE.

  PERFORM F_DBGET USING P_RUNDT.

  PERFORM F_PREPARE_POSTING_DATA USING RB_M1 RB_M2.

  PERFORM F_DOCUMENT_POSTING.

  PERFORM F_SET_STATUS.

  PERFORM F_DELETE_OUTTAB.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_WAR_END
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PREPARE_WAR_END .

  CHECK RB_M2 EQ ABAP_TRUE.

  PERFORM F_DBGET USING P_RUNDT.

  PERFORM F_PREPARE_POSTING_DATA USING RB_M1 RB_M2.

  PERFORM F_DOCUMENT_POSTING.

  PERFORM F_SET_STATUS.

  PERFORM F_DELETE_OUTTAB.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_REPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM F_PREPARE_REPORT .
*
**  CHECK RB_M3 EQ ABAP_TRUE.
**
**  PERFORM F_DBGET USING P_RPDAT.
**  PERFORM F_SET_STATUS.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DATA_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DATA_INIT .

  FREE: GT_OUTTAB, GT_MASTER, GT_SLAVE,
        GT_BLNTAB, GT_FTPOST, GT_FTTAX.

  CLEAR: GF_COUNT, GS_FTPOST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_POSTING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PREPARE_POSTING_DATA USING UF_M1 TYPE CHAR1
                                  UF_M2 TYPE CHAR1.

  DATA: LT_OUTTAB TYPE STANDARD TABLE OF ZSDSFIS128,
        LS_DATA   TYPE ZSDSFIS128,
        LS_HEADER TYPE ZSDSFIS125,
        LF_BUZEI  TYPE ACDOCA-BUZEI,
        LF_WRBTR  TYPE BSEG-WRBTR,
        LF_RACCT  TYPE RACCT.

  FREE: GT_MASTER, GT_SLAVE.

  APPEND LINES OF GT_OUTTAB TO LT_OUTTAB.

  LOOP AT GT_OUTTAB INTO DATA(LS_OUTTAB).
    LS_DATA = LS_OUTTAB.
    MOVE-CORRESPONDING LS_OUTTAB TO LS_HEADER.

    AT NEW VBELN_KEY.
      CLEAR LF_BUZEI.
      APPEND INITIAL LINE TO GT_MASTER ASSIGNING FIELD-SYMBOL(<L_MASTER>).
      <L_MASTER>-VBELN_KEY = LS_HEADER-VBELN_KEY.
*      <L_MASTER>-BUDAT = P_RUNDT.  "CH04-
      <L_MASTER>-BUDAT = P_BUDAT.   "CH04+
      <L_MASTER>-BLART = P_BLART.
      <L_MASTER>-BUKRS = P_BUKRS.
      <L_MASTER>-WAERS = LS_DATA-RWCUR_AC.
      <L_MASTER>-XBLNR = LS_DATA-AWKEY_ALLW.
      IF UF_M1 EQ ABAP_TRUE.
        <L_MASTER>-BKTXT = |{ TEXT-003 } { P_RUNDT }|. "CH01-{ P_RUNDT+4(2) }/{ P_RUNDT(4) }|.
      ELSE.
        <L_MASTER>-BKTXT = |{ TEXT-006 } { P_RUNDT }|. "CH01-{ P_RUNDT+4(2) }/{ P_RUNDT(4) }|.
      ENDIF.
      <L_MASTER>-EXCEPTION = 2.
    ENDAT.

    AT NEW MATNR_KEY.

      SUM.
*..Debit
      CLEAR: LF_WRBTR, LF_RACCT.
      LF_BUZEI = LF_BUZEI + 1.
      APPEND INITIAL LINE TO GT_SLAVE ASSIGNING FIELD-SYMBOL(<L_SLAVE>).
      MOVE-CORRESPONDING LS_HEADER TO <L_SLAVE>.

      PERFORM F_CALCULATE_POSTING_AMOUNT TABLES LT_OUTTAB USING UF_M1 UF_M2 LS_OUTTAB
                                         CHANGING LF_WRBTR LF_RACCT.


      <L_SLAVE>-RACCT = LF_RACCT.

      IF <L_SLAVE>-RACCT EQ GC_CREDIT_ACC.
        ##ENH_OK
        MOVE-CORRESPONDING LS_DATA TO <L_SLAVE>. "copa to credit acc
        IF LS_HEADER-MIGFLG_MAS EQ ABAP_TRUE.
          <L_SLAVE>-KOSTL = GC_KOSTL_DUMMY.
        ENDIF.
      ELSE.
        <L_SLAVE>-BUPLA = GC_BUPLA_0000.
        <L_SLAVE>-XREF3 = |{ LS_DATA-EQUNR_MAS ALPHA = OUT }|.
      ENDIF.

      <L_SLAVE>-VBELN_KEY = LS_HEADER-VBELN_KEY.
      <L_SLAVE>-BUZEI = LF_BUZEI.
      <L_SLAVE>-BSCHL = GC_DEBIT.

*      <L_SLAVE>-BUPLA = GC_BUPLA_0000.
      <L_SLAVE>-MATNR = LS_DATA-MATNR_KEY.
      <L_SLAVE>-SGTXT = CONDENSE( |{ LS_DATA-EQUNR_MAS ALPHA = OUT }| ) && |/{ LS_DATA-SERNR_MAS ALPHA = OUT }|.
      <L_SLAVE>-SGTXT = |{ <L_SLAVE>-SGTXT }/{ LS_DATA-STD_WRT_BEG_MAS }/{ LS_DATA-STD_WRT_END_MAS }|.

      <L_SLAVE>-WRBTR = ABS( LF_WRBTR ).


*      IF UF_M2 EQ ABAP_TRUE AND LS_DATA-STD_WRT_END_MAS(6) GT P_RUNDT(6).  "CH01-
      IF UF_M2 EQ ABAP_TRUE AND LS_DATA-STD_WRT_END_MAS GT P_RUNDT.  "CH01+
        <L_SLAVE>-WRBTR = 0.
        <L_SLAVE>-SGTXT = |{ TEXT-012 } { LS_DATA-STD_WRT_END_MAS } > { P_RUNDT }|.
      ENDIF.

*..Credit
      LF_BUZEI = LF_BUZEI + 1.
      UNASSIGN <L_SLAVE>.
      APPEND INITIAL LINE TO GT_SLAVE ASSIGNING <L_SLAVE>.
      MOVE-CORRESPONDING LS_HEADER TO <L_SLAVE>.
      ##ENH_OK

      IF LF_RACCT EQ GC_DEBIT_ACC.
        <L_SLAVE>-RACCT = GC_CREDIT_ACC.
      ELSE.
        <L_SLAVE>-RACCT = GC_DEBIT_ACC.
      ENDIF.

      IF <L_SLAVE>-RACCT EQ GC_CREDIT_ACC.
        ##ENH_OK
        MOVE-CORRESPONDING LS_DATA TO <L_SLAVE>. "copa to credit acc
        IF LS_HEADER-MIGFLG_MAS EQ ABAP_TRUE.
          <L_SLAVE>-KOSTL = GC_KOSTL_DUMMY.
        ENDIF.
      ELSE.
        <L_SLAVE>-BUPLA = GC_BUPLA_0000.
        <L_SLAVE>-XREF3 = |{ LS_DATA-EQUNR_MAS ALPHA = OUT }|.
      ENDIF.

      <L_SLAVE>-VBELN_KEY = LS_HEADER-VBELN_KEY.
      <L_SLAVE>-BUZEI = LF_BUZEI.
      <L_SLAVE>-BSCHL = GC_CREDIT.

      <L_SLAVE>-MATNR = LS_DATA-MATNR_KEY.
      <L_SLAVE>-SGTXT = CONDENSE( |{ LS_DATA-EQUNR_MAS ALPHA = OUT }| ) && |/{ LS_DATA-SERNR_MAS ALPHA = OUT }|.
      <L_SLAVE>-SGTXT = |{ <L_SLAVE>-SGTXT }/{ LS_DATA-STD_WRT_BEG_MAS }/{ LS_DATA-STD_WRT_END_MAS }|.
      <L_SLAVE>-WRBTR = ABS( LF_WRBTR ).

*      IF UF_M2 EQ ABAP_TRUE AND LS_DATA-STD_WRT_END_MAS(6) GT P_RUNDT(6). "CH01-
      IF UF_M2 EQ ABAP_TRUE AND LS_DATA-STD_WRT_END_MAS GT P_RUNDT. "CH01+
        <L_SLAVE>-WRBTR = 0.
        <L_SLAVE>-SGTXT = |{ TEXT-012 } { LS_DATA-STD_WRT_END_MAS } > { P_RUNDT }|.
      ENDIF.
    ENDAT.

  ENDLOOP.

  CASE ABAP_TRUE.
    WHEN RB_M1 OR RB_M2.
      DELETE GT_SLAVE WHERE WRBTR IS INITIAL.
      ##INTO_OK
      LOOP AT GT_MASTER INTO DATA(LS_MASTER).
        DATA(LV_TABIX) = SY-TABIX.
        READ TABLE GT_SLAVE WITH KEY VBELN_KEY = LS_MASTER-VBELN_KEY TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
          DELETE GT_MASTER INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_DETAIL_STRUC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&      --> P_
*&---------------------------------------------------------------------*
FORM F_SET_DETAIL_STRUC  USING   UR_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_HIERSEQ
                                  UF_STRUCNAME TYPE STRING.
  DATA: LF_TABNAME   TYPE DDOBJNAME,
        LT_DFIES_TAB TYPE STANDARD TABLE OF DFIES.

  DATA:
    LR_COLUMN  TYPE REF TO CL_SALV_COLUMN_HIERSEQ.

  LF_TABNAME = UF_STRUCNAME.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = LF_TABNAME
    TABLES
      DFIES_TAB      = LT_DFIES_TAB
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_DFIES_TAB ASSIGNING FIELD-SYMBOL(<L_DFIES_TAB>).
    TRY.
*      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'EXCEPTION' ).
*      LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
        LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( <L_DFIES_TAB>-FIELDNAME ).
        LR_COLUMN->SET_TECHNICAL( IF_SALV_C_BOOL_SAP=>TRUE ).
      ##NO_HANDLER    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND.
    ENDTRY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DOCUMENT_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DOCUMENT_POSTING .
  DATA: LF_BUZEI  TYPE ACDOCA-BUZEI.

  LOOP AT GT_MASTER ASSIGNING FIELD-SYMBOL(<L_MASTER>).
    FREE: GT_BLNTAB, GT_FTPOST.
    CLEAR: LF_BUZEI.

*    PERFORM F_POSTING_INTERFACE_START.
    PERFORM F_FILL_FTPOST_HEADER USING <L_MASTER>.

    LOOP AT GT_SLAVE INTO DATA(LS_SLAVE) WHERE VBELN_KEY EQ <L_MASTER>-VBELN_KEY.
      LF_BUZEI = LF_BUZEI + 1.
      PERFORM F_FILL_FTPOST_ITEM USING LS_SLAVE LF_BUZEI.
    ENDLOOP.

*>pst interface document
    PERFORM F_POSTING_INTERFACE_DOCUMENT CHANGING <L_MASTER>.
    PERFORM F_POSTING_INTERFACE_END.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POSTING_INTERFACE_START
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_POSTING_INTERFACE_START .

  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      I_FUNCTION         = 'C'
*     I_MODE             = 'N'
    EXCEPTIONS
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      OTHERS             = 6.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FTPOST_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CF_MASTER
*&---------------------------------------------------------------------*
FORM F_FILL_FTPOST_HEADER  USING US_MASTER  TYPE G_TYPE_S_MASTER.
* Post Document: Header Data

  DATA: LF_FVALUE TYPE FTPOST-FVAL.

  GF_COUNT = GF_COUNT + 1.

  GS_FTPOST-STYPE = 'K'.
  GS_FTPOST-COUNT =  GF_COUNT.
*
  WRITE US_MASTER-BUKRS TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BUKRS' LF_FVALUE.       "Company Code
*
  WRITE US_MASTER-BLART TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BLART' LF_FVALUE.       "Doc Type
*
  WRITE US_MASTER-BUDAT TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BLDAT' LF_FVALUE.       "Document Date

  WRITE US_MASTER-BUDAT TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BUDAT' LF_FVALUE.     "postin date

*..
  WRITE US_MASTER-BKTXT TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-BKTXT' LF_FVALUE.       "Header Text

  WRITE US_MASTER-XBLNR TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'BKPF-XBLNR' LF_FVALUE.       "Header Text

*  WRITE US_HEADER-WAERS TO LF_FVALUE.
  LF_FVALUE = 'THB'.
  PERFORM F_FTPOST_FIELD USING 'BKPF-WAERS' LF_FVALUE.       "curr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FTPOST_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0434   text
*      -->P_FVALUE  text
*----------------------------------------------------------------------*
FORM F_FTPOST_FIELD USING UF_FNAM TYPE FTPOST-FNAM
                          UF_FVAL TYPE FTPOST-FVAL.

  APPEND INITIAL LINE TO GT_FTPOST ASSIGNING FIELD-SYMBOL(<L_FTPOST>).
  <L_FTPOST>-STYPE = GS_FTPOST-STYPE.
  <L_FTPOST>-COUNT = GS_FTPOST-COUNT.
  <L_FTPOST>-FNAM = UF_FNAM.
  <L_FTPOST>-FVAL = UF_FVAL.

ENDFORM.                    " F_FTPOST_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FTPOST_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_FTPOST_ITEM USING US_SLAVE  TYPE G_TYPE_S_SLAVE
                              UF_BUZEI  TYPE ACDOCA-BUZEI.
  DATA: LF_FVALUE TYPE FTPOST-FVAL.

** Post Document: Fields of first line item

  GS_FTPOST-STYPE = 'P'.                  " Line Item
*  GS_FTPOST-COUNT =  US_SLAVE-BUZEI.      " Nr Line Item
  GS_FTPOST-COUNT =  UF_BUZEI.      " Nr Line Item

  WRITE US_SLAVE-BSCHL TO LF_FVALUE.
  PERFORM F_FTPOST_FIELD USING 'RF05A-NEWBS' LF_FVALUE.        " Post Key
*
  WRITE US_SLAVE-RACCT TO LF_FVALUE LEFT-JUSTIFIED.
  PERFORM F_FTPOST_FIELD USING 'RF05A-NEWKO' LF_FVALUE.        " Account

*    LF_FVALUE = 'VX'.
*    PERFORM F_FTPOST_FIELD USING 'BSEG-MWSKZ' LF_FVALUE.    "Vat code

*
  WRITE ABS( US_SLAVE-WRBTR ) TO LF_FVALUE NO-GROUPING LEFT-JUSTIFIED.
  PERFORM F_FTPOST_FIELD USING 'BSEG-WRBTR' LF_FVALUE.         " Amount

  WRITE US_SLAVE-SGTXT TO LF_FVALUE LEFT-JUSTIFIED.
  PERFORM F_FTPOST_FIELD USING 'BSEG-SGTXT' LF_FVALUE.         " Item text

  IF US_SLAVE-XREF3 IS NOT INITIAL.
    WRITE US_SLAVE-XREF3 TO LF_FVALUE LEFT-JUSTIFIED.
    PERFORM F_FTPOST_FIELD USING 'BSEG-XREF3' LF_FVALUE.         " Item text
  ENDIF.

  IF US_SLAVE-KOSTL IS NOT INITIAL.
    WRITE US_SLAVE-KOSTL TO LF_FVALUE LEFT-JUSTIFIED.
    PERFORM F_FTPOST_FIELD USING 'COBL-KOSTL' LF_FVALUE.
  ENDIF.

  IF US_SLAVE-MATNR IS NOT INITIAL.
    WRITE US_SLAVE-MATNR TO LF_FVALUE LEFT-JUSTIFIED.
    PERFORM F_FTPOST_FIELD USING 'COBL-MATNR' LF_FVALUE.
  ENDIF.

  IF US_SLAVE-AUFNR IS NOT INITIAL.
    WRITE US_SLAVE-AUFNR TO LF_FVALUE  LEFT-JUSTIFIED.
    PERFORM F_FTPOST_FIELD USING 'COBL-AUFNR' LF_FVALUE.
  ENDIF.

  IF US_SLAVE-MATNR IS NOT INITIAL.
    WRITE US_SLAVE-PRCTR TO LF_FVALUE LEFT-JUSTIFIED.
    PERFORM F_FTPOST_FIELD USING 'COBL-PRCTR' LF_FVALUE.
  ENDIF.

  IF US_SLAVE-PROJK IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = US_SLAVE-PROJK
      IMPORTING
        OUTPUT = LF_FVALUE.
    PERFORM F_FTPOST_FIELD USING 'COBL-PS_POSID' LF_FVALUE.
  ENDIF.

  IF US_SLAVE-PAOBJNR IS NOT INITIAL.
    PERFORM F_APPEND_RKE_FIELDS USING US_SLAVE UF_BUZEI.
  ENDIF.

ENDFORM.                    " F_FILL_FTPOST_ITEM
*&---------------------------------------------------------------------*
*& Form F_POSTING_INTERFACE_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CF_MASTER
*&---------------------------------------------------------------------*
FORM F_POSTING_INTERFACE_DOCUMENT  CHANGING CS_MASTER TYPE G_TYPE_S_MASTER.

  DATA: LF_SUBRC      TYPE SY-SUBRC ##NEEDED,
        LF_MSGID      TYPE SY-MSGID,
        LF_MSGNO      TYPE SY-MSGNO,
        LF_MSGTY      TYPE SY-MSGTY,
        LF_MSGV1      TYPE SY-MSGV1,
        LF_MSGV2      TYPE SY-MSGV2,
        LF_MSGV3      TYPE SY-MSGV3,
        LF_MSGV4      TYPE SY-MSGV4,
        LF_LINEP(110).

  ##NEEDED
  LOOP AT GT_FTPOST TRANSPORTING NO FIELDS WHERE FNAM EQ 'BSEG-WRBTR'
                                             AND FVAL NE '0.00'.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    CS_MASTER-EXCEPTION = 2.
    CS_MASTER-MSGTX     = TEXT-017.
    FREE: GT_BLNTAB, GT_FTPOST, GT_FTTAX.
    RETURN.
  ENDIF.

  PERFORM F_POSTING_INTERFACE_START.

  CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
    EXPORTING
      I_TCODE                  = 'FB01'
      I_XSIMU                  = CB_TEST
*     I_SGFUNCT                = ' '
*     I_NO_AUTH                = ' '
    IMPORTING
      E_SUBRC                  = LF_SUBRC
      E_MSGID                  = LF_MSGID
      E_MSGTY                  = LF_MSGTY
      E_MSGNO                  = LF_MSGNO
      E_MSGV1                  = LF_MSGV1
      E_MSGV2                  = LF_MSGV2
      E_MSGV3                  = LF_MSGV3
      E_MSGV4                  = LF_MSGV4
    TABLES
      T_BLNTAB                 = GT_BLNTAB
      T_FTPOST                 = GT_FTPOST
      T_FTTAX                  = GT_FTTAX
    EXCEPTIONS
      ACCOUNT_MISSING          = 1
      COMPANY_CODE_MISSING     = 2
      POSTING_KEY_INVALID      = 3
      POSTING_KEY_MISSING      = 4
      RECORD_TYPE_INVALID      = 5
      TRANSACTION_CODE_INVALID = 6
      AMOUNT_FORMAT_ERROR      = 7
      TOO_MANY_LINE_ITEMS      = 8
      COMPANY_CODE_INVALID     = 9
      SCREEN_NOT_FOUND         = 10
      NO_AUTHORIZATION         = 11
      OTHERS                   = 12.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO CS_MASTER-MSGTX.
    CS_MASTER-EXCEPTION = SWITCH CHAR01( SY-MSGTY
                      WHEN 'I' OR 'S' THEN '3'
                      WHEN 'W' THEN '2'
                      ELSE '1' ).
  ELSE.
    READ TABLE GT_BLNTAB INTO DATA(LS_BLNTAB) INDEX 1.
    IF SY-SUBRC EQ 0.
      CS_MASTER-EXCEPTION = '3'.
      CS_MASTER-BUKRS = LS_BLNTAB-BUKRS.
      CS_MASTER-GJAHR = LS_BLNTAB-GJAHR.
      CS_MASTER-BELNR = LS_BLNTAB-BELNR.
    ELSE.
      IF LF_MSGID IS NOT INITIAL.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            ID        = LF_MSGID
            LANG      = SY-LANGU
            NO        = LF_MSGNO
            V1        = LF_MSGV1
            V2        = LF_MSGV2
            V3        = LF_MSGV3
            V4        = LF_MSGV4
          IMPORTING
            MSG       = LF_LINEP
          EXCEPTIONS ##FM_SUBRC_OK
            NOT_FOUND = 1
            OTHERS    = 2.
        CS_MASTER-MSGTX = LF_LINEP.
        CS_MASTER-EXCEPTION = SWITCH CHAR01( LF_MSGTY
                              WHEN 'I' OR 'S' THEN '3'
                              WHEN 'W' THEN '2'
                              ELSE '1' ).
        IF LF_MSGID EQ '00' AND LF_MSGNO EQ '344'
                            AND LF_MSGTY EQ 'S'.
          CLEAR CS_MASTER-MSGTX.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POSTING_INTERFACE_END
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_POSTING_INTERFACE_END .

  CALL FUNCTION 'POSTING_INTERFACE_END'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_APPEND_RKE_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_SLAVE
*&---------------------------------------------------------------------*
FORM F_APPEND_RKE_FIELDS  USING  US_SLAVE TYPE G_TYPE_S_SLAVE
                                 UF_BUZEI TYPE ACDOCA-BUZEI.

  DATA: LT_COPADATA TYPE COPADATA_TAB,
        LF_FNAM     TYPE FTPOST-FNAM,
        LF_PAOBJNR  TYPE CEST1-PAOBJNR.

  LF_PAOBJNR = US_SLAVE-PAOBJNR.

  CALL FUNCTION 'RKE_CONVERT_PAOBJNR_COPADATA'
    EXPORTING
      BUKRS          = P_BUKRS
      KOKRS          = '1000'
      VORGN          = 'RFBU'
      PAOBJNR        = LF_PAOBJNR
      SET_PREFIX     = ABAP_TRUE
    TABLES
      I_COPADATA     = LT_COPADATA
    EXCEPTIONS
      NO_ERKRS_FOUND = 1
      PAOBJNR_WRONG  = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ELSE.

    GS_FTPOST-STYPE = 'P'.                  " Line Item
*    GS_FTPOST-COUNT =  US_SLAVE-BUZEI.     " Nr Line Item
    GS_FTPOST-COUNT =  UF_BUZEI.            " Nr Line Item

    LOOP AT LT_COPADATA INTO DATA(LS_COPADATA).
      CHECK LS_COPADATA-FVAL IS NOT INITIAL.
      LF_FNAM = |BSEG-{ LS_COPADATA-FNAM }|.
      IF LF_FNAM = 'BSEG-RKE_PRCTR'.
        LS_COPADATA-FVAL = US_SLAVE-PRCTR.
      ENDIF.
      PERFORM F_FTPOST_FIELD USING LF_FNAM  LS_COPADATA-FVAL.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  create_alv_form_content_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_ALV_FORM_CONTENT_TOP
  CHANGING CR_CONTENT     TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        L_TEXT    TYPE CHAR80.

*... create a grid
  CREATE OBJECT LR_GRID.

  L_TEXT = SY-TITLE.
  LR_GRID->CREATE_HEADER_INFORMATION(
    ROW     = 1
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = L_TEXT ).

*... add a row to the grid -> row 2
  LR_GRID->ADD_ROW( ).

*... in the cell [3,1] create a grid
  LR_GRID_1 = LR_GRID->CREATE_GRID(
    ROW    = 3
    COLUMN = 1 ).

  L_TEXT = |{ TEXT-004 }:|.
  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = '0L'.
  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 2
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = |{ TEXT-005 }:|.
  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 2
    COLUMN  = 1
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  L_TEXT = P_BUKRS.
  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 2
    COLUMN  = 2
    TEXT    = L_TEXT
    TOOLTIP = SPACE ).

  IF RB_M1 EQ ABAP_TRUE OR RB_M2 EQ ABAP_TRUE.

    L_TEXT = |{ TEXT-H01 }:|.
    LR_LABEL = LR_GRID_1->CREATE_LABEL(
      ROW     = 3
      COLUMN  = 1
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    L_TEXT = SWITCH CHAR80( RB_M1 WHEN 'X' THEN 'Warranty Claim'(003)
                                  ELSE 'Warranty End'(006) ).
    LR_TEXT = LR_GRID_1->CREATE_TEXT(
      ROW     = 3
      COLUMN  = 2
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    L_TEXT = |{ TEXT-009 }:|.
    LR_LABEL = LR_GRID_1->CREATE_LABEL(
      ROW     = 4
      COLUMN  = 1
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    WRITE P_RUNDT TO L_TEXT MM/DD/YYYY.
    LR_TEXT = LR_GRID_1->CREATE_TEXT(
      ROW     = 4
      COLUMN  = 2
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    L_TEXT = |{ TEXT-018 }:|.
    LR_LABEL = LR_GRID_1->CREATE_LABEL(
      ROW     = 5
      COLUMN  = 1
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    WRITE P_BUDAT TO L_TEXT MM/DD/YYYY.
    LR_TEXT = LR_GRID_1->CREATE_TEXT(
      ROW     = 5
      COLUMN  = 2
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).


    L_TEXT = |{ TEXT-010 }:|.
    LR_LABEL = LR_GRID_1->CREATE_LABEL(
      ROW     = 6
      COLUMN  = 1
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    L_TEXT = P_BLART.
    LR_TEXT = LR_GRID_1->CREATE_TEXT(
      ROW     = 6
      COLUMN  = 2
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    L_TEXT = |{ TEXT-007 }:|.
    LR_LABEL = LR_GRID_1->CREATE_LABEL(
      ROW     = 7
      COLUMN  = 1
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).

    L_TEXT = CB_TEST.
    LR_TEXT = LR_GRID_1->CREATE_TEXT(
      ROW     = 7
      COLUMN  = 2
      TEXT    = L_TEXT
      TOOLTIP = SPACE ).
  ENDIF.

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

*... content is the top grid
  CR_CONTENT = LR_GRID.

ENDFORM.                    " create_alv_form_content_top
*&---------------------------------------------------------------------*
*& Form F_SET_QUERY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LF_SELECT_QUERY
*&---------------------------------------------------------------------*
FORM F_SET_QUERY  CHANGING UF_SELECT_QUERY TYPE STRING.

  CLEAR UF_SELECT_QUERY.

  UF_SELECT_QUERY = 'BSEG~BUKRS, BSEG~BELNR, BSEG~GJAHR, BSEG~BUZEI, BSEG~AWKEY, BSEG~MATNR'.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, BSEG~HKONT, BSEG~XREF3, BSEG~PAOBJNR, BSEG~AUFNR, BSEG~PROJK, BSEG~KOSTL|.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, BKPF~XBLNR, BKPF~BUDAT, BKPF~BLART, BKPF~BKTXT, SKAT~TXT50|.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, ACDOCA~RACCT, ACDOCA~WSL, ACDOCA~RWCUR, ACDOCA~HSL, ACDOCA~RHCUR|.

*...Account Assignment Fields
  PERFORM F_SET_QUERY_FIELDS USING 'ACDOC_SI_GL_ACCAS'
                             CHANGING UF_SELECT_QUERY.
*...COPA Fields
  PERFORM F_SET_QUERY_FIELDS USING 'ACDOC_SI_COPA'
                             CHANGING UF_SELECT_QUERY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_QUERY_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- UF_SELECT_QUERY
*&---------------------------------------------------------------------*
FORM F_SET_QUERY_FIELDS  USING    VALUE(UF_STRUCTURE) TYPE DDOBJNAME
                         CHANGING UF_SELECT_QUERY TYPE STRING.

  DATA: LT_DFIES TYPE DFIES_TAB.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = UF_STRUCTURE
      LANGU          = SY-LANGU
    TABLES
      DFIES_TAB      = LT_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_DFIES ASSIGNING FIELD-SYMBOL(<L_DFIES>).
    UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, ACDOCA~{ <L_DFIES>-FIELDNAME }|.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CALCULATE_POSTING_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OUTTAB
*&      --> UF_M1
*&      --> UF_M2
*&      --> LS_DATA
*&      <-- LF_WRBTR
*&---------------------------------------------------------------------*
FORM F_CALCULATE_POSTING_AMOUNT  TABLES  UT_OUTTAB TYPE GTY_DATA
                                 USING   UF_M1 TYPE CHAR01
                                         UF_M2 TYPE CHAR01
                                         US_DATA TYPE ZSDSFIS128
                                 CHANGING US_WRBTR TYPE BSEG-WRBTR
                                          US_RACCT TYPE RACCT.

  DATA: LF_TOTAL_SG            TYPE BSEG-WRBTR,
        LF_TOTAL_WAR_ALLOWANCE TYPE BSEG-WRBTR.

  CLEAR: US_WRBTR, US_RACCT.

  IF UF_M2 EQ ABAP_TRUE.
    US_WRBTR = ABS( US_DATA-WSL_ALLW ).
    IF US_DATA-WSL_ALLW LT 0.
      US_RACCT = GC_DEBIT_ACC.
    ELSE.
      US_RACCT = GC_CREDIT_ACC.
    ENDIF.
  ELSEIF UF_M1 EQ ABAP_TRUE.

*..Total SG doc.
    ##INTO_OK
    LOOP AT UT_OUTTAB INTO DATA(LS_OUTTAB) WHERE VBELN_KEY EQ US_DATA-VBELN_KEY
                                             AND MATNR_KEY EQ US_DATA-MATNR_KEY
                                             AND ( BLART_ALLW EQ 'SG'
                                              OR   BLART_ALLW EQ 'SX' ). "CH01+
      IF RB_M1 EQ ABAP_TRUE.
        CHECK LS_OUTTAB-BUDAT_ALLW(6) EQ P_RUNDT(6).
      ENDIF.
      LF_TOTAL_SG = LF_TOTAL_SG + LS_OUTTAB-WSL_ALLW.
    ENDLOOP.

*..Total Warranty Allowance [ total actual - total allowance ]
    LF_TOTAL_WAR_ALLOWANCE  = US_DATA-WSL_AC - LF_TOTAL_SG.

    IF ABS( LF_TOTAL_WAR_ALLOWANCE ) LT ABS( US_DATA-WSL_ALLW ).
      US_WRBTR = ABS( LF_TOTAL_WAR_ALLOWANCE ).
      IF LF_TOTAL_WAR_ALLOWANCE LT 0.
        US_RACCT = GC_CREDIT_ACC.
      ELSE.
        US_RACCT = GC_DEBIT_ACC.
      ENDIF.
    ELSE.
      US_WRBTR = ABS( US_DATA-WSL_ALLW ).
      IF US_DATA-WSL_ALLW LT 0.
        US_RACCT = GC_DEBIT_ACC.
      ELSE.
        US_RACCT = GC_CREDIT_ACC.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_F4_GET_PRDHA_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- S_PRDHA1_LOW
*&---------------------------------------------------------------------*
*FORM F_F4_GET_PRDHA_LIST  USING    VALUE(UF_LEVEL) TYPE T179-STUFE
*                          CHANGING CF_VALUE TYPE MARA-PRDHA.
*
*  DATA: LV_SELECTED_VALUE TYPE STRING.
*
*  CALL FUNCTION 'RKE_PRODH_F4'
*    EXPORTING
**     I_DISPLAY        = ' '
*      I_LEVEL          = UF_LEVEL
*    IMPORTING
*      E_SELECTED_VALUE = LV_SELECTED_VALUE.
*
*  CF_VALUE = LV_SELECTED_VALUE.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DBGET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_RUNDT
*&---------------------------------------------------------------------*
FORM F_DBGET USING UF_KEYDATE TYPE SY-DATUM.

*...non migration data
  PERFORM F_OUTTAB_GET USING UF_KEYDATE.

*...migration data
  PERFORM F_OUTTAB_DM_FINETUNE_GET USING UF_KEYDATE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_REF_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BSEG_ALLW
*&      --> LS_ALLW
*&      <-- LS_REF_INVOICE
*&---------------------------------------------------------------------*
FORM F_GET_REF_INVOICE  TABLES   UT_BSEG_ALLW TYPE TTY_ACDOCA
                        CHANGING CS_DATA TYPE TY_ACDOCA.

  SELECT VBELV, VBELN, VBTYP_V INTO TABLE @DATA(LT_VBFA)
    FROM VBFA
    WHERE VBELN EQ @CS_DATA-AWKEY
      AND VBTYP_V EQ 'M' ORDER BY VBELV DESCENDING.
  READ TABLE LT_VBFA INTO DATA(LS_VBFA) INDEX 1.
  IF SY-SUBRC EQ 0.
    READ TABLE UT_BSEG_ALLW INTO DATA(LS_BSEG_ALLW) WITH KEY AWKEY = LS_VBFA-VBELV
                                                             MATNR = CS_DATA-MATNR.
    IF SY-SUBRC EQ 0.
      CS_DATA-XBLNR = LS_BSEG_ALLW-AWKEY.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_authorize_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_TCODE
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LAST_DAY_OF_MONTH_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LF_ENDDA
*&---------------------------------------------------------------------*
FORM F_LAST_DAY_OF_MONTH_GET  CHANGING UF_ENDDA TYPE SYDATUM.

  CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = UF_ENDDA
    IMPORTING
      LAST_DAY_OF_MONTH = UF_ENDDA
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_STATUS .

  DATA: LT_OUTTAB TYPE STANDARD TABLE OF ZSDSFIS128,
        LS_OUTTAB TYPE ZSDSFIS128.


  ##INTO_OK
  LOOP AT GT_OUTTAB INTO LS_OUTTAB.

    AT NEW MATNR_KEY.
      SUM.
      IF LS_OUTTAB-WSL_ALLW LT 0.
        DATA(LF_STATUS) = 2.
      ELSEIF LS_OUTTAB-WSL_ALLW = 0.
        LF_STATUS = 3.
      ELSE.
        LF_STATUS = 1.
      ENDIF.
    ENDAT.

*    IF RB_M3 EQ ABAP_TRUE.
*      IF LF_STATUS EQ 1.
*        CHECK CB_M3 EQ ABAP_TRUE.
*      ENDIF.
*      IF LF_STATUS EQ 2.
*        CHECK CB_M2 EQ ABAP_TRUE.
*      ENDIF.
*      IF LF_STATUS EQ 3.
*        CHECK CB_M1 EQ ABAP_TRUE.
*      ENDIF.
*    ENDIF.

    APPEND INITIAL LINE TO LT_OUTTAB ASSIGNING FIELD-SYMBOL(<L_OUTTAB>).
    MOVE-CORRESPONDING LS_OUTTAB TO <L_OUTTAB>.
    <L_OUTTAB>-STATUS = LF_STATUS.

  ENDLOOP.

  FREE GT_OUTTAB.

  APPEND LINES OF LT_OUTTAB TO GT_OUTTAB.

  FREE LT_OUTTAB.

  IF LINES( GT_OUTTAB ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DELETE_OUTTAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DELETE_OUTTAB.
  ##INTO_OK
  LOOP AT GT_OUTTAB INTO DATA(LS_OUTTAB).
    DATA(LV_TABIX) = SY-TABIX.
    READ TABLE GT_MASTER WITH KEY VBELN_KEY = LS_OUTTAB-VBELN_KEY
                         TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      DELETE GT_OUTTAB INDEX LV_TABIX.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_OUTTAB_DM_FINETUNE_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_KEYDATE
*&---------------------------------------------------------------------*
FORM F_OUTTAB_DM_FINETUNE_GET  USING UF_KEYDATE TYPE SY-DATUM.

  DATA:
    LT_BSEG_ACT     TYPE STANDARD TABLE OF TY_ACDOCA,
    LT_BSEG_ALLW    TYPE STANDARD TABLE OF TY_ACDOCA,
    LT_BSEG_PROV    TYPE STANDARD TABLE OF TY_ACDOCA,
    LS_OUTTAB       TYPE ZSDSFIS128,
    LF_SELECT_QUERY TYPE STRING,
    LF_BEGDA        TYPE SYDATUM,
    LF_ENDDA        TYPE SYDATUM,
    LR_WARR         TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LR_ALL          TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LR_ALLW         TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LR_PROV         TYPE STANDARD TABLE OF FAGL_R_S_SELOPT.

  CHECK CB_MGT EQ ABAP_TRUE.

  PERFORM F_SET_VAR USING UF_KEYDATE CHANGING LF_BEGDA LF_ENDDA
                                               LR_WARR LR_ALLW LR_PROV.

  IF LINES( LR_WARR ) EQ 0 OR LINES( LR_ALLW ) EQ 0 OR LINES( LR_PROV ) EQ 0.
    MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  APPEND LINES OF LR_ALLW TO LR_ALL.
  APPEND LINES OF LR_PROV TO LR_ALL.

* Actual Cost
*Posting Date (BKPF-BUDAT) = in the period of run date selected.
*1) Get Actual Cost data from table “ACDOCA”, “BKPF”, “BSEG”
*            - GL Account (ACDOCA-RACCT) = value in GEN_C table
*•  5421000010 Warranty Cost_FG
*•  5422000010 Warranty Cost_SP
*•  5423000010 Warranty Cost_Service
*            - Equipment no. (BSEG-XREF3) = Equipment no. in (7)
*  - Reversed doc (BKPF-STBLG) = Blank
  PERFORM F_SET_FINETUNE_QUERY CHANGING LF_SELECT_QUERY.

  PERFORM F_GET_ACTUAL_DATA TABLES LT_BSEG_ACT
                                   LR_WARR
                                   USING LF_SELECT_QUERY
                                         LF_BEGDA
                                         LF_ENDDA.
*
  DELETE LT_BSEG_ACT WHERE XREF3 IS INITIAL.
  SORT LT_BSEG_ACT.

  IF RB_M1 EQ ABAP_TRUE.
    IF LINES( LT_BSEG_ACT ) EQ 0.
      RETURN.
    ENDIF.
  ENDIF.

* Warranty Provision
* - Posting Date (BKPF-BUDAT) = As of run date selected.
*1) Get Warranty Allowance data from table “ACDOCA”, “BKPF”, “BSEG”
*- GL Account (ACDOCA-RACCT) = value in GEN_C table
*•  2211000010 Allowance for Product Warranties_General
*            - Marial (ACDOCA-MATNR) = Marial in (3)
*            - Billing (BKPF-AWKEY) = Billing in (4)
*  - Reversed doc (BKPF-STBLG) = Blank
  PERFORM F_SET_QUERY CHANGING LF_SELECT_QUERY.

  PERFORM F_GET_WAR_PROV_DATA TABLES LT_BSEG_ALLW
                                  LR_ALL
                                  USING LF_SELECT_QUERY
                                        LF_BEGDA
                                        LF_ENDDA.

  DELETE LT_BSEG_ALLW WHERE MATNR IS NOT INITIAL.

  APPEND LINES OF LT_BSEG_ALLW TO LT_BSEG_PROV.
  DELETE LT_BSEG_ALLW WHERE HKONT IN LR_PROV.
  DELETE LT_BSEG_PROV WHERE HKONT IN LR_ALLW.

*====================================================================================
*...Warranty Master / Product Warranty
  ##NUMBER_OK
  SELECT  "LTRIM( EQUNR, '0' )  AS EQUNR_MAS,
      EQUNR AS EQUNR_MAS,
      ZSDSCMT003~MATNR AS MATNR_MAS,
      SERNR AS SERNR_MAS,
      WRTPK AS WRTPK_MAS,
      WRTLT_FLAG AS WRTLT_FLAG_MAS,
      WRTLT AS WRTLT_MAS,
      VBELN_VL AS VBELN_VL_MAS,
      FKDAT AS FKDAT_MAS,
      WADAT_IST AS WADAT_IST_MAS,
      VBELN_VA AS VBELN_VA_MAS,
      ZSDSCMT003~KUNNR AS KUNNR_MAS,
      VKBUR AS VKBUR_MAS,
      VKGRP AS VKGRP_MAS,
      STD_WRT_BEG AS STD_WRT_BEG_MAS,
      STD_WRT_END AS STD_WRT_END_MAS,
      MIGFLG AS MIGFLG_MAS,
      PRDHA
    FROM ZSDSCMT003 INNER JOIN MARA
     ON ZSDSCMT003~MATNR EQ MARA~MATNR
    FOR ALL ENTRIES IN @LT_BSEG_ACT
    WHERE EQUNR EQ @LT_BSEG_ACT-XREF3
          AND ZSDSCMT003~MATNR IN @S_MATNR
          AND EQUNR IN @S_EQUNR
          AND VKBUR IN @S_VKBUR
          AND VKGRP IN @S_VKGRP
          AND STD_WRT_END IN @S_ENDDT
          AND MIGFLG      EQ @CB_MGT
          INTO TABLE @DATA(LT_WARMAST).

  SORT LT_WARMAST BY EQUNR_MAS ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_WARMAST COMPARING ALL FIELDS.

  IF LINES( LT_WARMAST ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*...Filter Product Hierarchy
  LOOP AT LT_WARMAST ASSIGNING FIELD-SYMBOL(<L_WARMAST>).
    UNPACK <L_WARMAST>-EQUNR_MAS TO <L_WARMAST>-EQUNR_MAS.

*..default std. war enddate
    IF <L_WARMAST>-STD_WRT_END_MAS IS INITIAL.
      <L_WARMAST>-STD_WRT_END_MAS = '99991231'.
    ENDIF.

*    SPLIT <L_WARMAST>-PRDHA AT '  '  INTO DATA(LV_PRDHA1) DATA(LV_PRDHA2) DATA(LV_PRDHA3) IN CHARACTER MODE.
*    LV_PRDHA2 = |{ LV_PRDHA1 }  { LV_PRDHA2 }|.
*    LV_PRDHA3 = |{ LV_PRDHA2 }  { LV_PRDHA3 }|.
*    IF NOT ( LV_PRDHA1 IN S_PRDHA1 AND LV_PRDHA2 IN S_PRDHA2 AND LV_PRDHA3 IN S_PRDHA3 ).
*      DELETE LT_WARMAST INDEX SY-TABIX.
*    ENDIF.
  ENDLOOP.

  IF LINES( LT_WARMAST ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  SORT LT_WARMAST BY VBELN_VL_MAS MATNR_MAS ASCENDING                 "CH01+
                            STD_WRT_END_MAS DESCENDING                "CH01+
                            EQUNR_MAS ASCENDING.                      "CH01+


  IF LINES( LT_WARMAST ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*====================================================================================
  SORT LT_BSEG_ACT  BY BUDAT BUKRS BELNR GJAHR HKONT.
  SORT LT_BSEG_ALLW BY BUDAT BUKRS BELNR GJAHR HKONT.
  SORT LT_BSEG_PROV BY BUDAT BUKRS BELNR GJAHR HKONT.

  SORT LT_WARMAST BY VBELN_VL_MAS MATNR_MAS ASCENDING                 "CH01+
                            STD_WRT_END_MAS DESCENDING                "CH01+
                            EQUNR_MAS ASCENDING.                      "CH01+

  LOOP AT LT_WARMAST INTO DATA(LS_WARMAST).  ##INTO_OK
    ##ENH_OK    MOVE-CORRESPONDING LS_WARMAST TO LS_OUTTAB.
    CLEAR: LS_OUTTAB-VBELN_KEY, LS_OUTTAB-MATNR_KEY.
    IF LINES( GT_OUTTAB ) EQ 0.
      PERFORM F_SET_PROV_MIG TABLES LT_BSEG_ALLW LT_BSEG_PROV USING LS_OUTTAB.
    ENDIF.
*..Actual
    ##INTO_OK  LOOP AT LT_BSEG_ACT INTO DATA(LS_ACT) WHERE XREF3 EQ LS_WARMAST-EQUNR_MAS.
      DATA(LV_INDEX) = SY-TABIX.

      CLEAR LS_OUTTAB.
      ##ENH_OK      MOVE-CORRESPONDING LS_WARMAST TO LS_OUTTAB.
      CLEAR: LS_OUTTAB-VBELN_KEY, LS_OUTTAB-MATNR_KEY.

      LS_OUTTAB-BUKRS_AC = LS_ACT-BUKRS.
      LS_OUTTAB-BELNR_AC = LS_ACT-BELNR.
      LS_OUTTAB-GJAHR_AC = LS_ACT-GJAHR.
      LS_OUTTAB-BUDAT_AC = LS_ACT-BUDAT.
      LS_OUTTAB-BLART_AC = LS_ACT-BLART.
      LS_OUTTAB-HKONT_AC = LS_ACT-RACCT.
      LS_OUTTAB-TXT50_AC = LS_ACT-TXT50.
      LS_OUTTAB-XREF3_AC = LS_ACT-XREF3.

      LS_OUTTAB-WSL_AC   = LS_ACT-WSL.
      LS_OUTTAB-RWCUR_AC = LS_ACT-RWCUR.
      LS_OUTTAB-HSL_AC   = LS_ACT-HSL.
      LS_OUTTAB-RHCUR_AC = LS_ACT-RHCUR.
      SHIFT LS_OUTTAB-XREF3_AC LEFT DELETING LEADING '0'.
      SHIFT LS_OUTTAB-EQUNR_MAS LEFT DELETING LEADING '0'.
      APPEND LS_OUTTAB TO GT_OUTTAB.
      DELETE LT_BSEG_ACT INDEX LV_INDEX.
    ENDLOOP.
  ENDLOOP.

  IF LINES( GT_OUTTAB ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_QUERY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LF_SELECT_QUERY
*&---------------------------------------------------------------------*
FORM F_SET_FINETUNE_QUERY  CHANGING UF_SELECT_QUERY TYPE STRING.

  CLEAR UF_SELECT_QUERY.

  UF_SELECT_QUERY = 'BSIS~BUKRS, BSIS~BELNR, BSIS~GJAHR, BSIS~BUZEI, BKPF~AWKEY, ACDOCA~MATNR'.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, BSIS~HKONT, LPAD( BSIS~XREF3, 18, '0' ) AS XREF3, ACDOCA~PAOBJNR, BSIS~AUFNR, BSIS~PROJK, BSIS~KOSTL|.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, BKPF~XBLNR, BKPF~BUDAT, BKPF~BLART, BKPF~BKTXT, SKAT~TXT50|.
  UF_SELECT_QUERY = |{ UF_SELECT_QUERY }, ACDOCA~RACCT, ACDOCA~WSL, ACDOCA~RWCUR, ACDOCA~HSL, ACDOCA~RHCUR|.

*...Account Assignment Fields
  PERFORM F_SET_QUERY_FIELDS USING 'ACDOC_SI_GL_ACCAS'
                             CHANGING UF_SELECT_QUERY.
*...COPA Fields
  PERFORM F_SET_QUERY_FIELDS USING 'ACDOC_SI_COPA'
                             CHANGING UF_SELECT_QUERY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_VAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_KEYDATE
*&      <-- LF_BEGDA
*&      <-- LF_ENDDA
*&      <-- LR_WARR
*&      <-- LR_ALLW
*&      <-- LR_PROV
*&---------------------------------------------------------------------*
FORM F_SET_VAR  USING    UF_KEYDATE TYPE SYDATUM
                CHANGING CF_BEGDA TYPE SYDATUM
                         CF_ENDDA TYPE SYDATUM
                         CR_WARR TYPE STANDARD TABLE
                         CR_ALLW TYPE STANDARD TABLE
                         CR_PROV TYPE STANDARD TABLE.
  CF_BEGDA = CF_ENDDA = UF_KEYDATE.
  CF_BEGDA+6(2) = '01'.

  PERFORM F_LAST_DAY_OF_MONTH_GET CHANGING CF_ENDDA.

*  IF RB_M3 EQ ABAP_TRUE.
*    CF_BEGDA = '19990101'.
*  ENDIF.

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSFIR0420'
                                                  IF_PARAM = 'HKONT_WAR'
                                        IMPORTING ET_RANGE = CR_WARR ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSFIR0420'
                                                  IF_PARAM = 'HKONT_WAR_ALLOWANCE'
                                        IMPORTING ET_RANGE = CR_ALLW ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSFIR0420'
                                                  IF_PARAM = 'HKONT_WAR_PROVISION'
                                        IMPORTING ET_RANGE = CR_PROV ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_OUTTAB_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_RPDAT
*&---------------------------------------------------------------------*
FORM F_OUTTAB_GET  USING UF_KEYDATE TYPE SY-DATUM.

  CHECK CB_MGT EQ ABAP_FALSE.

  DATA:
    LF_SELECT_QUERY TYPE STRING,
    LF_BEGDA        TYPE SYDATUM,
    LF_ENDDA        TYPE SYDATUM,
    LR_WARR         TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LR_ALL          TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LR_ALLW         TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LR_PROV         TYPE STANDARD TABLE OF FAGL_R_S_SELOPT,
    LT_OUTTAB       TYPE STANDARD TABLE OF ZSDSFIS128,
    LS_OUTTAB       TYPE ZSDSFIS128,

    LT_BSEG_ACT     TYPE STANDARD TABLE OF TY_ACDOCA,
    LT_BSEG_ALLW    TYPE STANDARD TABLE OF TY_ACDOCA,
    LT_BSEG_PROV    TYPE STANDARD TABLE OF TY_ACDOCA.

  PERFORM F_SET_VAR USING UF_KEYDATE CHANGING LF_BEGDA LF_ENDDA
                                              LR_WARR LR_ALLW LR_PROV.
  IF LINES( LR_WARR ) EQ 0 OR LINES( LR_ALLW ) EQ 0 OR LINES( LR_PROV ) EQ 0.
    MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  APPEND LINES OF LR_ALLW TO LR_ALL.
  APPEND LINES OF LR_PROV TO LR_ALL.

*<<< Begin of insertion CH03
  IF RB_M2 EQ ABAP_TRUE.
    SELECT  EQUNR AS EQUNR_MAS,
            ZSDSCMT003~MATNR AS MATNR_MAS,
            SERNR AS SERNR_MAS,
            WRTPK AS WRTPK_MAS,
            WRTLT_FLAG AS WRTLT_FLAG_MAS,
            WRTLT AS WRTLT_MAS,
            VBELN_VL AS VBELN_VL_MAS,
            FKDAT AS FKDAT_MAS,
            WADAT_IST AS WADAT_IST_MAS,
            VBELN_VA AS VBELN_VA_MAS,
            ZSDSCMT003~KUNNR AS KUNNR_MAS,
            VKBUR AS VKBUR_MAS,
            VKGRP AS VKGRP_MAS,
            STD_WRT_BEG AS STD_WRT_BEG_MAS,
            STD_WRT_END AS STD_WRT_END_MAS,
            MIGFLG AS MIGFLG_MAS,
            PRDHA
          FROM ZSDSCMT003 INNER JOIN MARA
           ON ZSDSCMT003~MATNR EQ MARA~MATNR
          WHERE EQUNR IN @S_EQUNR
                AND ZSDSCMT003~MATNR IN @S_MATNR
                AND VBELN_VL IS NOT INITIAL
                AND EQUNR IN @S_EQUNR
                AND VKBUR IN @S_VKBUR
                AND VKGRP IN @S_VKGRP
                AND STD_WRT_END IN @S_ENDDT
                AND MIGFLG      EQ @CB_MGT
          INTO TABLE @DATA(LT_WARMAST).
    IF LINES( LT_WARMAST ) EQ 0.
      MESSAGE TEXT-016 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
*>>> End of insertion CH03

*<<< Get Actual data
  PERFORM F_SET_FINETUNE_QUERY CHANGING LF_SELECT_QUERY.

  PERFORM F_GET_ACTUAL_DATA TABLES LT_BSEG_ACT
                                   LR_WARR
                                   USING LF_SELECT_QUERY
                                         LF_BEGDA
                                         LF_ENDDA.
  SORT LT_BSEG_ACT.

  IF RB_M1 EQ ABAP_TRUE.
    IF LINES( LT_BSEG_ACT ) EQ 0.
      RETURN.
    ENDIF.
  ENDIF.

* Warranty Provision
  PERFORM F_SET_QUERY CHANGING LF_SELECT_QUERY.
  PERFORM F_GET_WAR_PROV_DATA TABLES LT_BSEG_ALLW
                                  LR_ALL
                                  USING LF_SELECT_QUERY
                                        LF_BEGDA
                                        LF_ENDDA.
  DELETE LT_BSEG_ALLW WHERE MATNR IS INITIAL.

  IF LINES( LT_BSEG_ALLW ) GT 0
           AND RB_M2 EQ ABAP_FALSE. "CH03+
    SELECT  EQUNR AS EQUNR_MAS,
            ZSDSCMT003~MATNR AS MATNR_MAS,
            SERNR AS SERNR_MAS,
            WRTPK AS WRTPK_MAS,
            WRTLT_FLAG AS WRTLT_FLAG_MAS,
            WRTLT AS WRTLT_MAS,
            VBELN_VL AS VBELN_VL_MAS,
            FKDAT AS FKDAT_MAS,
            WADAT_IST AS WADAT_IST_MAS,
            VBELN_VA AS VBELN_VA_MAS,
            ZSDSCMT003~KUNNR AS KUNNR_MAS,
            VKBUR AS VKBUR_MAS,
            VKGRP AS VKGRP_MAS,
            STD_WRT_BEG AS STD_WRT_BEG_MAS,
            STD_WRT_END AS STD_WRT_END_MAS,
            MIGFLG AS MIGFLG_MAS,
            PRDHA
          FROM ZSDSCMT003 INNER JOIN MARA
           ON ZSDSCMT003~MATNR EQ MARA~MATNR
          FOR ALL ENTRIES IN @LT_BSEG_ALLW
          WHERE ZSDSCMT003~MATNR EQ @LT_BSEG_ALLW-MATNR
            AND EQUNR IN @S_EQUNR
                AND ZSDSCMT003~MATNR IN @S_MATNR
                AND VBELN_VL IS NOT INITIAL "Delivery no.
                AND EQUNR IN @S_EQUNR
                AND VKBUR IN @S_VKBUR
                AND VKGRP IN @S_VKGRP
                AND STD_WRT_END IN @S_ENDDT
      AND MIGFLG      EQ @CB_MGT
          INTO TABLE @LT_WARMAST.
  ENDIF.

  APPEND LINES OF LT_BSEG_ALLW TO LT_BSEG_PROV.
  DELETE LT_BSEG_ALLW WHERE HKONT IN LR_PROV.
  DELETE LT_BSEG_PROV WHERE HKONT IN LR_ALLW.

*====================================================================================
*...Warranty Master / Product Warranty
  IF LINES( LT_BSEG_ACT ) IS NOT INITIAL AND RB_M1 EQ ABAP_TRUE.

    SELECT
         EQUNR AS EQUNR_MAS,
         ZSDSCMT003~MATNR AS MATNR_MAS,
         SERNR AS SERNR_MAS,
         WRTPK AS WRTPK_MAS,
         WRTLT_FLAG AS WRTLT_FLAG_MAS,
         WRTLT AS WRTLT_MAS,
         VBELN_VL AS VBELN_VL_MAS,
         FKDAT AS FKDAT_MAS,
         WADAT_IST AS WADAT_IST_MAS,
         VBELN_VA AS VBELN_VA_MAS,
         ZSDSCMT003~KUNNR AS KUNNR_MAS,
         VKBUR AS VKBUR_MAS,
         VKGRP AS VKGRP_MAS,
         STD_WRT_BEG AS STD_WRT_BEG_MAS,
         STD_WRT_END AS STD_WRT_END_MAS,
         MIGFLG AS MIGFLG_MAS,
         PRDHA
       FROM ZSDSCMT003 INNER JOIN MARA
        ON ZSDSCMT003~MATNR EQ MARA~MATNR
       FOR ALL ENTRIES IN @LT_BSEG_ACT
       WHERE EQUNR EQ @LT_BSEG_ACT-XREF3
             AND ZSDSCMT003~MATNR IN @S_MATNR
             AND EQUNR IN @S_EQUNR
             AND VKBUR IN @S_VKBUR
             AND VKGRP IN @S_VKGRP
             AND STD_WRT_END IN @S_ENDDT
             AND MIGFLG      EQ @CB_MGT
             APPENDING TABLE  @LT_WARMAST.

  ENDIF.
  SORT LT_WARMAST BY EQUNR_MAS ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_WARMAST COMPARING ALL FIELDS.

  SORT LT_WARMAST BY VBELN_VL_MAS MATNR_MAS ASCENDING                 "CH01+
                           STD_WRT_END_MAS DESCENDING                "CH01+
                           EQUNR_MAS ASCENDING.                      "CH01+

  IF LINES( LT_WARMAST ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*...Filter Product Hierarchy
  LOOP AT LT_WARMAST ASSIGNING FIELD-SYMBOL(<L_WARMAST>).

*..default std. war enddate
    IF <L_WARMAST>-STD_WRT_END_MAS IS INITIAL.
      <L_WARMAST>-STD_WRT_END_MAS = '99991231'.
    ENDIF.

  ENDLOOP.

  IF LINES( LT_WARMAST ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT LT_WARMAST BY VBELN_VL_MAS MATNR_MAS ASCENDING                 "CH01+
                            STD_WRT_END_MAS DESCENDING                "CH01+
                            EQUNR_MAS ASCENDING.                      "CH01+

  SELECT VBFA~VBELV, VBFA~VBELN, VBRK~FKDAT, VBRK~VKORG, VBRK~VTWEG, VBRK~SPART
     INTO TABLE @DATA(LT_VBFA)
    FROM VBFA INNER JOIN VBRK
    ON VBFA~VBELN EQ VBRK~VBELN
    FOR ALL ENTRIES IN @LT_WARMAST
    WHERE VBELV EQ @LT_WARMAST-VBELN_VL_MAS
      AND VBTYP_N EQ 'M'
      AND VBRK~VBELN IN @S_VBELN
      AND VBRK~FKDAT IN @S_FKDAT
      AND VBRK~VKORG IN @S_VKORG
      AND VBRK~VTWEG IN @S_VTWEG
      AND VBRK~SPART IN @S_SPART.

  SORT LT_VBFA BY VBELV DESCENDING VBELN DESCENDING.

*...Filter Billing No.&Billing Date based on selection screen
  LOOP AT LT_WARMAST ASSIGNING <L_WARMAST>.
    DATA(LV_TABIX) = SY-TABIX.
    READ TABLE LT_VBFA WITH KEY VBELV = <L_WARMAST>-VBELN_VL_MAS TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      DELETE LT_WARMAST INDEX LV_TABIX.
    ENDIF.
  ENDLOOP.

  IF LINES( LT_WARMAST ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*..Update DN/CN (XBLNR) with Billing no.
  DATA(LT_BILLING) =  LT_BSEG_ALLW.
  DELETE LT_BILLING WHERE BLART NE 'R1'.
  LOOP AT LT_BSEG_ALLW ASSIGNING FIELD-SYMBOL(<L_BSEG_ALLW>) WHERE BLART EQ 'R2' OR
                                                                   BLART EQ 'R3'.
    PERFORM F_GET_REF_INVOICE TABLES LT_BSEG_ALLW  CHANGING <L_BSEG_ALLW>.
  ENDLOOP.

  FREE LT_BILLING.
*====================================================================================
  SORT LT_BSEG_ACT  BY BUDAT BUKRS BELNR GJAHR HKONT.
  SORT LT_BSEG_ALLW BY BUDAT BUKRS BELNR GJAHR HKONT.
  SORT LT_BSEG_PROV BY BUDAT BUKRS BELNR GJAHR HKONT.

  LOOP AT LT_WARMAST INTO DATA(LS_WARMAST).  ##INTO_OK
    READ TABLE LT_VBFA INTO DATA(LS_FLOW) WITH KEY VBELV = LS_WARMAST-VBELN_VL_MAS.
    CHECK SY-SUBRC EQ 0.
    CLEAR LS_OUTTAB.
    ##ENH_OK    MOVE-CORRESPONDING LS_WARMAST TO LS_OUTTAB.
    LS_OUTTAB-VBELN_KEY = LS_FLOW-VBELN.
    LS_OUTTAB-MATNR_KEY = LS_WARMAST-MATNR_MAS.
    LS_OUTTAB-FKDAT_MAS = LS_FLOW-FKDAT.
    LS_OUTTAB-VKORG_MAS = LS_FLOW-VKORG.
    LS_OUTTAB-VTWEG_MAS = LS_FLOW-VTWEG.
    LS_OUTTAB-SPART_MAS = LS_FLOW-SPART.

    LOOP AT LT_BSEG_ALLW INTO DATA(LS_ALLW)
      WHERE ( AWKEY(10) EQ LS_OUTTAB-VBELN_KEY OR
              XBLNR(10) EQ LS_OUTTAB-VBELN_KEY )
        AND   MATNR    EQ LS_OUTTAB-MATNR_KEY.
      DATA(LV_ALLW_TABIX) = SY-TABIX.
      LOOP AT LT_VBFA TRANSPORTING NO FIELDS WHERE VBELV = LS_WARMAST-VBELN_VL_MAS
                                     AND ( VBELN = LS_ALLW-AWKEY(10) OR VBELN EQ LS_ALLW-XBLNR(10) ).
        EXIT.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      DELETE LT_BSEG_ALLW INDEX LV_ALLW_TABIX.
      READ TABLE LT_BSEG_PROV INTO DATA(LS_PROV) WITH KEY  BUKRS = LS_ALLW-BUKRS
                                                           BELNR = LS_ALLW-BELNR
                                                           GJAHR = LS_ALLW-GJAHR
                                                           MATNR = LS_OUTTAB-MATNR_KEY.
      IF SY-SUBRC NE 0.
        CLEAR LS_PROV.
      ELSE.
        DATA(LV_PROV_TABIX) = SY-TABIX.
        DELETE LT_BSEG_PROV INDEX LV_PROV_TABIX.
      ENDIF.
      PERFORM F_SET_PROV_ITEM TABLES LT_OUTTAB USING LS_OUTTAB LS_ALLW LS_PROV.
    ENDLOOP.

*..Actual
    ##INTO_OK    LOOP AT LT_BSEG_ACT INTO DATA(LS_ACT) WHERE XREF3 EQ LS_WARMAST-EQUNR_MAS.
      DATA(LV_INDEX) = SY-TABIX.

      CLEAR LS_OUTTAB.
      ##ENH_OK      MOVE-CORRESPONDING LS_WARMAST  TO LS_OUTTAB.
      LS_OUTTAB-VBELN_KEY = LS_FLOW-VBELN.
      LS_OUTTAB-MATNR_KEY = LS_WARMAST-MATNR_MAS.
      LS_OUTTAB-FKDAT_MAS = LS_FLOW-FKDAT.
      LS_OUTTAB-VKORG_MAS = LS_FLOW-VKORG.
      LS_OUTTAB-VTWEG_MAS = LS_FLOW-VTWEG.
      LS_OUTTAB-SPART_MAS = LS_FLOW-SPART.
      LS_OUTTAB-BUKRS_AC = LS_ACT-BUKRS.
      LS_OUTTAB-BELNR_AC = LS_ACT-BELNR.
      LS_OUTTAB-GJAHR_AC = LS_ACT-GJAHR.
      LS_OUTTAB-BUDAT_AC = LS_ACT-BUDAT.
      LS_OUTTAB-BLART_AC = LS_ACT-BLART.
      LS_OUTTAB-HKONT_AC = LS_ACT-RACCT.
      LS_OUTTAB-TXT50_AC = LS_ACT-TXT50.
      LS_OUTTAB-XREF3_AC = LS_ACT-XREF3.
      LS_OUTTAB-WSL_AC   = LS_ACT-WSL.
      LS_OUTTAB-RWCUR_AC = LS_ACT-RWCUR.
      LS_OUTTAB-HSL_AC   = LS_ACT-HSL.
      LS_OUTTAB-RHCUR_AC = LS_ACT-RHCUR.
      SHIFT LS_OUTTAB-XREF3_AC LEFT DELETING LEADING '0'.
      SHIFT LS_OUTTAB-EQUNR_MAS LEFT DELETING LEADING '0'.
      APPEND LS_OUTTAB TO LT_OUTTAB.
      DELETE LT_BSEG_ACT INDEX LV_INDEX.
    ENDLOOP.

    AT END OF MATNR_MAS.
      IF LINES( LT_OUTTAB ) GT 0.
        APPEND LINES OF LT_OUTTAB TO GT_OUTTAB.
      ENDIF.
      FREE LT_OUTTAB.
    ENDAT.
  ENDLOOP.

  IF LINES( GT_OUTTAB ) EQ 0.
    MESSAGE 'No data found'(016) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PSEGMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_LINE
*&      <-- <L_SLAVE>
*&---------------------------------------------------------------------*
*FORM F_SET_PSEGMENT  USING    US_LINE TYPE ZSDSFIS128
*                     CHANGING CS_SLAVE TYPE G_TYPE_S_SLAVE.
*
*  DATA: LS_GL_ACCAS TYPE ACDOC_SI_GL_ACCAS,
*        LS_COPA     TYPE ACDOC_SI_COPA.
*
*  MOVE-CORRESPONDING US_LINE TO: LS_GL_ACCAS, LS_COPA.
*
*  MOVE-CORRESPONDING LS_GL_ACCAS TO CS_SLAVE.
*  MOVE-CORRESPONDING LS_COPA     TO CS_SLAVE.
*  MOVE: US_LINE-PAOBJNR TO CS_SLAVE-PAOBJNR,
*        US_LINE-AUFNR   TO CS_SLAVE-AUFNR,
*        US_LINE-PROJK   TO CS_SLAVE-PROJK.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SCREEN_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SCREEN_CHECK.

  IF RB_M1 EQ ABAP_TRUE OR RB_M2 EQ ABAP_TRUE.
    IF P_BUDAT IS INITIAL.
      MESSAGE TEXT-E02 TYPE 'E'.
    ENDIF.
  ENDIF.

  IF RB_M2 EQ ABAP_TRUE AND CB_MGT EQ ABAP_FALSE.
    IF S_ENDDT-LOW IS INITIAL OR S_ENDDT-HIGH IS INITIAL.
      MESSAGE TEXT-E05 TYPE 'E'.
    ELSEIF S_ENDDT-LOW GT P_RUNDT.
      MESSAGE TEXT-E06 TYPE 'E'.
    ELSEIF S_ENDDT-HIGH GT P_RUNDT.
      MESSAGE TEXT-E07 TYPE 'E'.
    ENDIF.
  ENDIF.

  IF CB_MGT EQ ABAP_TRUE.
    IF S_MATNR IS NOT INITIAL OR
      S_EQUNR IS NOT INITIAL OR
      S_VBELN IS NOT INITIAL OR
      S_FKDAT IS NOT INITIAL OR
      S_VKORG IS NOT INITIAL OR
      S_VTWEG IS NOT INITIAL OR
      S_SPART IS NOT INITIAL OR
      S_VKBUR IS NOT INITIAL OR
      S_VKGRP IS NOT INITIAL OR
      S_ENDDT IS NOT INITIAL.
      MESSAGE TEXT-E04 TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_ACTUAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BSEG_ACT
*&      --> LR_WARR
*&      --> LF_SELECT_QUERY
*&      --> LF_BEGDA
*&      --> LF_ENDDA
*&---------------------------------------------------------------------*
FORM F_GET_ACTUAL_DATA  TABLES   LT_BSEG_ACT TYPE STANDARD TABLE
                                 LR_WARR TYPE STANDARD TABLE
                        USING    LF_SELECT_QUERY
                                 LF_BEGDA
                                 LF_ENDDA.
  SELECT (LF_SELECT_QUERY)                              "#EC CI_NOORDER
    FROM BSIS_VIEW AS BSIS INNER JOIN BKPF
    ON BSIS~BUKRS EQ BKPF~BUKRS
      AND BSIS~BELNR EQ BKPF~BELNR
      AND BSIS~GJAHR EQ BKPF~GJAHR
    INNER JOIN ACDOCA
    ON BSIS~BUKRS EQ ACDOCA~RBUKRS
      AND BSIS~BELNR EQ ACDOCA~BELNR
      AND BSIS~GJAHR EQ ACDOCA~GJAHR
      AND BSIS~BUZEI EQ ACDOCA~BUZEI
    INNER JOIN SKAT                                    "#EC CI_BUFFJOIN
    ON ACDOCA~RACCT EQ SKAT~SAKNR
    WHERE BKPF~BUKRS EQ @P_BUKRS
      AND BKPF~BUDAT BETWEEN @LF_BEGDA AND @LF_ENDDA
      AND BSIS~HKONT IN @LR_WARR
      AND SKAT~SPRAS EQ 'E'
      AND SKAT~KTOPL EQ 'RCOA'
*      AND BSEG~XREF3 EQ @LT_WARMAST-EQUNR_MAS      "--> to filtered later
        AND ACDOCA~RLDNR EQ '0L' INTO TABLE @LT_BSEG_ACT .
  SORT LT_BSEG_ACT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_WAR_PROV_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BSEG_ALLW
*&      --> LR_ALL
*&      --> LF_SELECT_QUERY
*&      --> LF_BEGDA
*&      --> LF_ENDDA
*&---------------------------------------------------------------------*
FORM F_GET_WAR_PROV_DATA  TABLES   LT_BSEG_ALLW TYPE STANDARD TABLE
                                   LR_ALL TYPE STANDARD TABLE
                          USING    LF_SELECT_QUERY
                                   LF_BEGDA                  ##NEEDED
                                   LF_ENDDA.
  SELECT (LF_SELECT_QUERY)                              "#EC CI_NOORDER
      FROM BSEG  INNER JOIN BKPF
      ON BSEG~BUKRS EQ BKPF~BUKRS
        AND BSEG~BELNR EQ BKPF~BELNR
        AND BSEG~GJAHR EQ BKPF~GJAHR
      INNER JOIN ACDOCA
      ON BSEG~BUKRS EQ ACDOCA~RBUKRS
        AND BSEG~BELNR EQ ACDOCA~BELNR
        AND BSEG~GJAHR EQ ACDOCA~GJAHR
        AND BSEG~BUZEI EQ ACDOCA~BUZEI
    INNER JOIN SKAT                                    "#EC CI_BUFFJOIN
    ON ACDOCA~RACCT EQ SKAT~SAKNR
    WHERE BSEG~HKONT IN @LR_ALL
        AND BKPF~BUKRS EQ @P_BUKRS
        AND BKPF~BUDAT LE @LF_ENDDA
    AND SKAT~SPRAS EQ 'E'
      AND SKAT~KTOPL EQ 'RCOA'
          AND ACDOCA~RLDNR EQ '0L' INTO TABLE @LT_BSEG_ALLW.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PROV_MIG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_OUTTAB
*&---------------------------------------------------------------------*
FORM F_SET_PROV_MIG  TABLES LT_BSEG_ALLW TYPE STANDARD TABLE
                            LT_BSEG_PROV TYPE STANDARD TABLE
                      USING FS_OUTTAB TYPE ZSDSFIS128.

  DATA: LS_BSEG_ALLW TYPE TY_ACDOCA,
        LS_BSEG_PROV TYPE TY_ACDOCA,
        LS_OUTTAB    TYPE ZSDSFIS128.

  MOVE-CORRESPONDING FS_OUTTAB TO LS_OUTTAB.

  ##INTO_OK  LOOP AT LT_BSEG_ALLW INTO LS_BSEG_ALLW.
    DATA(LV_INDEX) = SY-TABIX.
    READ TABLE LT_BSEG_PROV INTO LS_BSEG_PROV INDEX LV_INDEX.
    IF SY-SUBRC NE 0.
      CLEAR LS_BSEG_PROV.
    ENDIF.

    LS_OUTTAB-BUKRS_ALLW = LS_BSEG_ALLW-BUKRS.
    LS_OUTTAB-BELNR_ALLW = LS_BSEG_ALLW-BELNR.
    LS_OUTTAB-GJAHR_ALLW = LS_BSEG_ALLW-GJAHR.
    LS_OUTTAB-BUDAT_ALLW = LS_BSEG_ALLW-BUDAT.
    LS_OUTTAB-MATNR_ALLW = LS_BSEG_ALLW-MATNR.
    LS_OUTTAB-AWKEY_ALLW = LS_BSEG_ALLW-AWKEY.
    LS_OUTTAB-BKTXT_ALLW = LS_BSEG_ALLW-BKTXT.
    LS_OUTTAB-BLART_ALLW = LS_BSEG_ALLW-BLART.
    LS_OUTTAB-HKONT_ALLW = LS_BSEG_ALLW-RACCT.
    LS_OUTTAB-TXT50_ALLW = LS_BSEG_ALLW-TXT50.
    LS_OUTTAB-WSL_ALLW   = LS_BSEG_ALLW-WSL.
    LS_OUTTAB-RWCUR_ALLW = LS_BSEG_ALLW-RWCUR.
    LS_OUTTAB-HSL_ALLW   = LS_BSEG_ALLW-HSL.
    LS_OUTTAB-RHCUR_ALLW = LS_BSEG_ALLW-RHCUR.

**..Update prov data
    ##ENH_OK
    MOVE-CORRESPONDING LS_BSEG_PROV TO LS_OUTTAB.
    LS_OUTTAB-HKONT_PROV = LS_BSEG_PROV-RACCT.
    LS_OUTTAB-XREF3_PROV = LS_BSEG_PROV-XREF3.
    LS_OUTTAB-TXT50_PROV = LS_BSEG_PROV-TXT50.
    SHIFT LS_OUTTAB-XREF3_PROV LEFT DELETING LEADING '0'.
    SHIFT LS_OUTTAB-EQUNR_MAS LEFT DELETING LEADING '0'.
    APPEND LS_OUTTAB TO GT_OUTTAB.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PROV_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OUTTAB
*&      --> LS_ALLW
*&      --> LS_PROV
*&---------------------------------------------------------------------*
FORM F_SET_PROV_ITEM  TABLES   LT_OUTTAB TYPE STANDARD TABLE
                      USING    FS_OUTTAB TYPE ZSDSFIS128
                               FS_BSEG_ALLW TYPE TY_ACDOCA
                               FS_BSEG_PROV TYPE TY_ACDOCA.

  DATA: LS_OUTTAB    TYPE ZSDSFIS128.

  MOVE-CORRESPONDING FS_OUTTAB TO LS_OUTTAB.
  LS_OUTTAB-BUKRS_ALLW = FS_BSEG_ALLW-BUKRS.
  LS_OUTTAB-BELNR_ALLW = FS_BSEG_ALLW-BELNR.
  LS_OUTTAB-GJAHR_ALLW = FS_BSEG_ALLW-GJAHR.
  LS_OUTTAB-BUDAT_ALLW = FS_BSEG_ALLW-BUDAT.
  LS_OUTTAB-MATNR_ALLW = FS_BSEG_ALLW-MATNR.
  LS_OUTTAB-AWKEY_ALLW = FS_BSEG_ALLW-AWKEY.
  LS_OUTTAB-BKTXT_ALLW = FS_BSEG_ALLW-BKTXT.
  LS_OUTTAB-BLART_ALLW = FS_BSEG_ALLW-BLART.
  LS_OUTTAB-HKONT_ALLW = FS_BSEG_ALLW-RACCT.
  LS_OUTTAB-TXT50_ALLW = FS_BSEG_ALLW-TXT50.
  LS_OUTTAB-WSL_ALLW   = FS_BSEG_ALLW-WSL.
  LS_OUTTAB-RWCUR_ALLW = FS_BSEG_ALLW-RWCUR.
  LS_OUTTAB-HSL_ALLW   = FS_BSEG_ALLW-HSL.
  LS_OUTTAB-RHCUR_ALLW = FS_BSEG_ALLW-RHCUR.

**..Update prov data
  ##ENH_OK
  MOVE-CORRESPONDING FS_BSEG_PROV TO LS_OUTTAB.
  LS_OUTTAB-HKONT_PROV = FS_BSEG_PROV-RACCT.
  LS_OUTTAB-XREF3_PROV = FS_BSEG_PROV-XREF3.
  LS_OUTTAB-TXT50_PROV = FS_BSEG_PROV-TXT50.
  SHIFT LS_OUTTAB-XREF3_PROV LEFT DELETING LEADING '0'.
  SHIFT LS_OUTTAB-EQUNR_MAS LEFT DELETING LEADING '0'.
  APPEND LS_OUTTAB TO LT_OUTTAB.

ENDFORM.
