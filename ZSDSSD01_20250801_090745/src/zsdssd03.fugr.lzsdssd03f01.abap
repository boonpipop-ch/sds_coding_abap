*----------------------------------------------------------------------*
***INCLUDE LZSDSSD03F01.
*----------------------------------------------------------------------*
************************************************************************
*-----------------------------------------------------------------------
* CLASS DEFINITION
*-----------------------------------------------------------------------
*Event Handler
CLASS LCL_EVENT_HANDLER_1 DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      HANDLE_TOOLBAR_1       FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

      ON_DATA_CHANGED_1 FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED
                  E_ONF4
                  E_ONF4_BEFORE
                  E_ONF4_AFTER
                  E_UCOMM,

      ON_DATA_CHANGED_FINISHED_1 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED
                  ET_GOOD_CELLS.

ENDCLASS.                    "lcl_event_handler DEFINITION

*-----------------------------------------------------------------------
* CLASS IMPLEMENTATION
*-----------------------------------------------------------------------
* Event Handler
CLASS LCL_EVENT_HANDLER_1 IMPLEMENTATION.

  METHOD HANDLE_TOOLBAR_1.
    PERFORM F_HANDLE_TOOLBAR_1
      USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  METHOD ON_DATA_CHANGED_1.
    PERFORM F_ON_DATA_CHANGED_1
      USING ER_DATA_CHANGED
            E_ONF4
            E_ONF4_BEFORE
            E_ONF4_AFTER
            E_UCOMM.
  ENDMETHOD.

  METHOD ON_DATA_CHANGED_FINISHED_1.
    PERFORM F_ON_DATA_CHANGED_FINISHED_1
      USING E_MODIFIED
            ET_GOOD_CELLS.
  ENDMETHOD.

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*  Form F_COLLECT_ITEM
*----------------------------------------------------------------------*
*  Collect Item
*----------------------------------------------------------------------*
FORM F_COLLECT_ITEM  USING  US_ADVREC  TYPE  TS_ADVREC_DATA
                            UF_ASSIGN  TYPE  FLAG
                   CHANGING CT_ITEM TYPE  TT_ITEM_SORT.

  DATA:
    LS_ITEM  TYPE  TS_ITEM.


  READ TABLE CT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>)
                     WITH KEY BUKRS = US_ADVREC-BUKRS
                              BELNR = US_ADVREC-BELNR
                              GJAHR = US_ADVREC-GJAHR
                     BINARY SEARCH.
  IF SY-SUBRC NE 0.
    CLEAR LS_ITEM.
    LS_ITEM-BUKRS = US_ADVREC-BUKRS.
    LS_ITEM-UMSKZ = US_ADVREC-UMSKZ.
    LS_ITEM-BELNR = US_ADVREC-BELNR.
    LS_ITEM-GJAHR = US_ADVREC-GJAHR.
    LS_ITEM-BUDAT = US_ADVREC-BUDAT.
    LS_ITEM-WAERS = US_ADVREC-WAERS.
    INSERT LS_ITEM INTO TABLE CT_ITEM
                   ASSIGNING <L_ITEM>.
  ENDIF.
  <L_ITEM>-REMAM = <L_ITEM>-REMAM + US_ADVREC-DMBTR.
  IF UF_ASSIGN EQ GC_TRUE.
    <L_ITEM>-ASNAM = <L_ITEM>-ASNAM + US_ADVREC-DMBTR.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_DISPLAY_1
*----------------------------------------------------------------------*
*  Display ALV
*----------------------------------------------------------------------*
FORM F_ALV_DISPLAY_1 .

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF GREF_GRID_1 IS INITIAL.

    IF GREF_CUSTOM_CONTAINER_1 IS INITIAL.
      CREATE OBJECT GREF_CUSTOM_CONTAINER_1
        EXPORTING
          CONTAINER_NAME = GF_CONTAINER_1.
    ENDIF.

    GREF_CONTAINER_GRID_1 = GREF_CUSTOM_CONTAINER_1.

*   Create First ALV Object
    CREATE OBJECT GREF_GRID_1
      EXPORTING
        I_PARENT = GREF_CONTAINER_GRID_1.

*   Set Event Handler for First ALV
    CREATE OBJECT GREF_EVENT_RECEIVER_1.

    SET HANDLER:
      GREF_EVENT_RECEIVER_1->HANDLE_TOOLBAR_1           FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DATA_CHANGED_1          FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DATA_CHANGED_FINISHED_1 FOR GREF_GRID_1.

*   Show First ALV
    CALL METHOD GREF_GRID_1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_BUFFER_ACTIVE               = 'X'
        IS_VARIANT                    = GS_VARIANT_1
        IS_LAYOUT                     = GS_LAYOUT_1
        IS_PRINT                      = GS_PRINT_1
        I_SAVE                        = GC_SAVE_ALL
        IT_TOOLBAR_EXCLUDING          = GT_TOOL_EXC_1
      CHANGING
        IT_OUTTAB                     = <G_LIST_1>
        IT_FIELDCATALOG               = GT_FIELDCAT_1
        IT_SORT                       = GT_SORT_1
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.
*  ELSE.
*    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        I_SOFT_REFRESH = 'X'.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CALL_MAINTENANCE_SCREEN
*----------------------------------------------------------------------*
*  Call Maintenance Screen
*----------------------------------------------------------------------*
FORM F_CALL_MAINTENANCE_SCREEN  USING  UT_RESULT TYPE TT_ITEM.

* Disable Save option
  IF GF_DISPLAY_ONLY EQ 'X'.
    INSERT GC_FUNC_SAVE   INTO TABLE GT_TOOL_EXC_1.
    INSERT GC_FUNC_DELETE INTO TABLE GT_TOOL_EXC_1.
    INSERT GC_FUNC_CANC   INTO TABLE GT_TOOL_EXC_1.
  ELSE.
    IF GF_DELETE_ALLOW IS INITIAL.
      INSERT GC_FUNC_DELETE INTO TABLE GT_TOOL_EXC_1.
    ENDIF.
    INSERT GC_FUNC_CLOSE  INTO TABLE GT_TOOL_EXC_1.
  ENDIF.

* ALV Layout
  PERFORM F_ALV_LAYOUT1 CHANGING GS_LAYOUT_1
                                 GS_VARIANT_1
                                 GS_PRINT_1.

* Assign Output Data
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT1 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT1 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000 STARTING AT 20 1.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT1
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT1 CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                            CS_VARIANT TYPE  DISVARIANT
                            CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
*  CS_LAYOUT-SEL_MODE   = 'C'.
  CS_LAYOUT-SEL_MODE   = 'A'.
*  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.
*  CS_LAYOUT-BOX_FNAME  = 'CHKBOX'.
*  CS_LAYOUT-CTAB_FNAME = 'COLTAB'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT1
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT1 CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT TYPE  TEXT50,
    LF_EDIT TYPE  FLAG.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


  IF GF_DISPLAY_ONLY IS INITIAL.
    LF_EDIT = 'X'.
  ENDIF.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
*      WHEN 'CHKBOX'.
*        <L_FIELDCAT>-EDIT      = LF_EDIT.
*        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
      WHEN 'BUKRS'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'UMSKZ'.
      WHEN 'BELNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'GJAHR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'BUDAT'.
      WHEN 'REMAM'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM    = GC_TRUE.
*       Text-c01 : Remain ADV.Receive
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ASNAM'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
        <L_FIELDCAT>-DO_SUM    = GC_TRUE.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
        <L_FIELDCAT>-EDIT      = LF_EDIT.
*       Text-c02 : Assigned Amount
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WAERS'.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT1
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT1  CHANGING PT_SORT TYPE LVC_T_SORT ##NEEDED.

*  CONSTANTS:
*    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'EDATU'.
*
*  DATA:
*    LS_SORT  TYPE  LVC_S_SORT.
*
*
** Initialize Output
*  CLEAR: PT_SORT.
*
** Sort by Delivery Date
*  CLEAR LS_SORT.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO PT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_prepare_fieldcat_o
*----------------------------------------------------------------------*
*       Prepare fieldcat method ALV used for online
*----------------------------------------------------------------------*
*  -->  PFD_STRUCTURE  Structure name of the list table
*  <--  PIT_FIELDCAT   Field catalog
*----------------------------------------------------------------------*
FORM F_PREPARE_FIELDCAT_O  USING  UF_STRUCTURE TYPE DD02L-TABNAME
                         CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT ##CALLED.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = UF_STRUCTURE
    CHANGING
      CT_FIELDCAT            = CT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " f_prepare_fieldcat_o

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UREF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

* Handle Toolbar as needed
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&INFO'.

  APPEND VALUE #( FUNCTION      = '&ALL'
                ICON         = '@4B@'
                QUICKINFO       = 'Select All'
             ) TO UREF_OBJECT->MT_TOOLBAR.

  APPEND VALUE #( FUNCTION      = '&SAL'
                ICON         = '@4D@'
                QUICKINFO       = 'Deselect All'
             ) TO UREF_OBJECT->MT_TOOLBAR.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMANDS_1
*----------------------------------------------------------------------*
*  USER Commands
*----------------------------------------------------------------------*
FORM F_USER_COMMANDS_1 .
  DATA: LT_ROWS    TYPE LVC_T_ROW,
        LS_ROW     TYPE LVC_S_ROW,
        LT_ROW_IDS TYPE LVC_T_ROID,
        LS_ROW_ID  TYPE LVC_S_ROID.
  FIELD-SYMBOLS: <LFS_FIELD> TYPE ANY.

  CASE GF_FCODE.
    WHEN GC_FUNC_DELETE.
      CLEAR GF_FCODE.
      PERFORM F_DELETE_DATA  USING  GT_ITEM
                           CHANGING GT_DATA
                                    GT_ADVREC.
      CLEAR GF_FCODE.
    WHEN GC_FUNC_SAVE.
      CLEAR GF_FCODE.
      PERFORM F_SAVE_DATA  USING  GS_HEAD
                                  GT_ITEM
                         CHANGING GT_DATA
                                  GT_ADVREC.
    WHEN GC_FUNC_CANC OR GC_FUNC_CLOSE.
      CLEAR GF_FCODE.
      GF_CANCEL = GC_TRUE.
      LEAVE TO SCREEN 0.
    WHEN GC_FUNC_RESET.
      CLEAR GF_FCODE.
      CLEAR LT_ROWS.
      CALL METHOD GREF_GRID_1->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = LT_ROWS
          ET_ROW_NO     = LT_ROW_IDS.
      IF LT_ROWS IS NOT INITIAL.
        LOOP AT LT_ROWS INTO LS_ROW.
          READ TABLE <G_LIST_1> ASSIGNING FIELD-SYMBOL(<LFS_ADVREC>) INDEX LS_ROW-INDEX.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT 'ASNAM' OF STRUCTURE <LFS_ADVREC> TO <LFS_FIELD>.
            IF SY-SUBRC = 0.
              CLEAR <LFS_FIELD>.
            ENDIF.
          ENDIF.
        ENDLOOP.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_SAVE_DATA
*----------------------------------------------------------------------*
*  Save Data
*----------------------------------------------------------------------*
FORM F_SAVE_DATA   USING  US_HEAD  TYPE  TS_HEAD
                          UT_ITEM  TYPE  TT_ITEM
                 CHANGING CT_DATA  TYPE  TT_CONF_DATA
                          CT_ADVREC TYPE  TT_ADVREC_DATA.

  DATA:
    LT_ITEM          TYPE TT_ITEM_SORT,
    LT_ADVREC_ASSIGN TYPE TT_ADVREC_ASSIGN.

  DATA:
    LS_ADVREC_ASSIGN TYPE  TS_ADVREC_ASSIGN,
    LS_RETURN        TYPE  BAPIRET1.

  DATA:
    LF_VALID        TYPE  CHAR01,
    LF_REFRESH      TYPE  CHAR01,
    LF_AMOUNT       TYPE  TS_ITEM-ASNAM,
    LF_SCHED_AMOUNT TYPE  TS_CONF_DATA-AMOUNT,
    LF_DMBTR        TYPE  TS_CONF_DATA-AMOUNT.


* Initialize Output
  CLEAR: CT_ADVREC.

* ---------------------
* Validate ALV Data (GT_ITEM & GS_HEAD must be updated)
* ---------------------
  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID   = LF_VALID
                                   CHANGING  C_REFRESH = LF_REFRESH ).
* Continue processing only when valid
  IF LF_VALID IS INITIAL.
    RETURN.
  ENDIF.

* ---------------------
* Validate Data
* ---------------------
  IF US_HEAD-STATUS NE GC_OK.
*   Error: Advance receive amount is not yet completely assigned to sched.line item.
    MESSAGE I018(ZSDSSD01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* For sorting
  INSERT LINES OF UT_ITEM INTO TABLE LT_ITEM.

* ---------------------
* Collect Data for saving
* ---------------------
  LOOP AT LT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>).

    LF_AMOUNT = <L_ITEM>-ASNAM.

*   Assign to Sched item(s)
    IF LF_AMOUNT GT 0.
      LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

        IF <L_DATA>-AMOUNT EQ <L_DATA>-AMOUNT_ADV.
          CONTINUE.
        ENDIF.

*       Amount in Sched line
        LF_SCHED_AMOUNT = <L_DATA>-AMOUNT -  <L_DATA>-AMOUNT_ADV.

*       Assigned into sched amount
        IF LF_AMOUNT GE LF_SCHED_AMOUNT.
          LF_DMBTR = LF_SCHED_AMOUNT.
        ELSE.
          LF_DMBTR = LF_AMOUNT.
        ENDIF.
*       Calc remain
        LF_AMOUNT = LF_AMOUNT - LF_DMBTR.

        CLEAR  LS_ADVREC_ASSIGN.
        LS_ADVREC_ASSIGN-VBELN = <L_DATA>-VBELN.
        LS_ADVREC_ASSIGN-POSNR = <L_DATA>-POSNR.
        LS_ADVREC_ASSIGN-ETENR = <L_DATA>-ETENR.
        LS_ADVREC_ASSIGN-BUKRS = <L_ITEM>-BUKRS.
        LS_ADVREC_ASSIGN-BELNR = <L_ITEM>-BELNR.
        LS_ADVREC_ASSIGN-GJAHR = <L_ITEM>-GJAHR.
        LS_ADVREC_ASSIGN-DMBTR = LF_DMBTR.
        LS_ADVREC_ASSIGN-WAERS = <L_ITEM>-WAERS.
        INSERT LS_ADVREC_ASSIGN INTO TABLE LT_ADVREC_ASSIGN.

*       Add Assigned
        <L_DATA>-AMOUNT_ADV = <L_DATA>-AMOUNT_ADV + LF_DMBTR.
        INSERT VALUE #( BUKRS = <L_ITEM>-BUKRS
                        BELNR = <L_ITEM>-BELNR
                        GJAHR = <L_ITEM>-GJAHR
                        BUDAT = <L_ITEM>-BUDAT
                        DMBTR = LF_DMBTR
                        WAERS = <L_ITEM>-WAERS )
               INTO TABLE <L_DATA>-ADVREC.

*       Until no amount
        IF LF_AMOUNT LE 0.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

*   Assign Remainging
    LF_AMOUNT = <L_ITEM>-REMAM - <L_ITEM>-ASNAM.
    IF LF_AMOUNT GT 0.

      INSERT VALUE #( BUKRS = <L_ITEM>-BUKRS
                      BELNR = <L_ITEM>-BELNR
                      GJAHR = <L_ITEM>-GJAHR
                      BUDAT = <L_ITEM>-BUDAT
                      DMBTR = LF_AMOUNT
                      WAERS = <L_ITEM>-WAERS )
             INTO TABLE CT_ADVREC.
    ENDIF.
  ENDLOOP.

* ---------------------
* Save Data
* ---------------------
  CALL METHOD ZCL_SDSSD_ORDER_CONFIRMATION=>SAVE_ADVREC_ASSIGNMENT
    EXPORTING
      IT_ADVREC_ASSIGN = LT_ADVREC_ASSIGN
      IF_COMMIT        = GC_TRUE
    IMPORTING
      ES_RETURN        = LS_RETURN.
  IF LS_RETURN IS NOT INITIAL.
*   Error
    MESSAGE ID LS_RETURN-ID
            TYPE   LS_RETURN-TYPE
            NUMBER LS_RETURN-NUMBER DISPLAY LIKE 'E'
            WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    RETURN.
  ENDIF.

* Save Successfully
* Message: Advance receive assignement has been saved successfully.
  MESSAGE S021(ZSDSSD01).

  LEAVE TO SCREEN 0.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_1 USING UREF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                               UF_ONF4 TYPE  CHAR01          ##NEEDED
                               UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                               UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                               UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.

  DATA:
    LF_TEXT  TYPE  TEXT50.


* For all valid changing cells
  LOOP AT UREF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>)
                      INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.

      WHEN 'ASNAM'.
*       Check Value not exceed AdvRec Amount
        IF <L_GOOD_CELL>-VALUE > <L_ITEM>-REMAM.
*         Error: Amount entered must not greater than advance receive amount &1.
          WRITE <L_ITEM>-REMAM TO LF_TEXT CURRENCY <L_ITEM>-WAERS LEFT-JUSTIFIED.
          CONDENSE LF_TEXT NO-GAPS.
          CALL METHOD UREF_DATA_CHANGED->ADD_PROTOCOL_ENTRY
            EXPORTING
              I_MSGID     = 'ZSDSSD01'
              I_MSGTY     = 'E'
              I_MSGNO     = '017'
              I_MSGV1     = LF_TEXT
              I_FIELDNAME = <L_GOOD_CELL>-FIELDNAME
              I_ROW_ID    = <L_GOOD_CELL>-ROW_ID
              I_TABIX     = <L_GOOD_CELL>-TABIX.

        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UPDATE_HEADER
*----------------------------------------------------------------------*
*  Recalculate Amount in Header
*----------------------------------------------------------------------*
FORM F_UPDATE_HEADER  USING  UT_ITEM  TYPE  TT_ITEM
                    CHANGING CS_HEAD  TYPE  TS_HEAD.

  DATA: LV_ADVREC_DIFF TYPE WERT1.

  CLEAR: CS_HEAD-ASSIGN_AMOUNT,
         CS_HEAD-UNASSIGN_AMOUNT.

  PERFORM GET_GENC CHANGING LV_ADVREC_DIFF.

  LOOP AT UT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>)
                  WHERE ASNAM IS NOT INITIAL.
    CS_HEAD-ASSIGN_AMOUNT = CS_HEAD-ASSIGN_AMOUNT + <L_ITEM>-ASNAM.
  ENDLOOP.
  CS_HEAD-UNASSIGN_AMOUNT = CS_HEAD-ITEMS_AMOUNT - CS_HEAD-ASSIGN_AMOUNT.

*  IF CS_HEAD-UNASSIGN_AMOUNT EQ 0 OR                            "DEL- 18/02/2025  IMS420000395
  IF ( CS_HEAD-UNASSIGN_AMOUNT BETWEEN 0 AND LV_ADVREC_DIFF ) OR "Add+ 18/02/2025  IMS420000395
     ( CS_HEAD-REMAIN_ADVREC EQ 0 AND
       CS_HEAD-UNASSIGN_AMOUNT GT 0 ).
    CS_HEAD-STATUS = GC_OK.
  ELSE.
    CS_HEAD-STATUS = GC_NOT_OK.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_INITIALIZE_DATA
*----------------------------------------------------------------------*
*  Initialize Data
*----------------------------------------------------------------------*
FORM F_INITIALIZE_DATA .

  IF GREF_CONTAINER_GRID_1 IS NOT INITIAL.
    GREF_CONTAINER_GRID_1->FREE( ).
  ENDIF.

  CLEAR: GS_HEAD,
         GT_ITEM,
         GT_DATA,
         GT_ADVREC,
         GF_CANCEL,
         GF_DISPLAY_ONLY.

  FREE:  GREF_GRID_1,
         GREF_CUSTOM_CONTAINER_1,
         GREF_CONTAINER_GRID_1,
         GREF_EVENT_RECEIVER_1.

  CLEAR: GT_TOOL_EXC_1,
         GT_FIELDCAT_1,
         GT_SORT_1,
         GS_VARIANT_1,
         GS_LAYOUT_1,
         GS_FIELDCAT_1,
         GS_SORT_1,
         GS_PRINT_1.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_FINISHED_1
*----------------------------------------------------------------------*
*  On Data Changed Finished
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_FINISHED_1  USING UF_MODIFIED TYPE	CHAR01 ##NEEDED
                                         UT_GOOD_CELLS TYPE	LVC_T_MODI ##NEEDED.

  DATA:
    LS_STBL  TYPE LVC_S_STBL.


* ---------------------
* Update Header Data
* ---------------------
  PERFORM F_UPDATE_HEADER  USING  GT_ITEM
                         CHANGING GS_HEAD.

  LS_STBL-ROW = 'X'.
  LS_STBL-COL = 'X'.
  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STBL
      I_SOFT_REFRESH = ' '.

* Force PBO to update Scren
  SUPPRESS DIALOG.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DELETE_DATA
*----------------------------------------------------------------------*
*  Delete assignment data
*----------------------------------------------------------------------*
FORM F_DELETE_DATA  USING  UT_ITEM  TYPE  TT_ITEM
                  CHANGING CT_DATA  TYPE  TT_CONF_DATA
                           CT_ADVREC TYPE  TT_ADVREC_DATA.

  DATA:
    LT_ITEM          TYPE TT_ITEM_SORT,
    LT_ADVREC_ASSIGN TYPE TT_ADVREC_ASSIGN.

  DATA:
    LS_ADVREC_ASSIGN TYPE  TS_ADVREC_ASSIGN,
    LS_RETURN        TYPE  BAPIRET1.

  DATA:
    LF_AMOUNT       TYPE  TS_ITEM-ASNAM.


* Initialize Output
  CLEAR: CT_ADVREC.

* For sorting
  INSERT LINES OF UT_ITEM INTO TABLE LT_ITEM.

* ---------------------
* Collect Data for saving
* ---------------------
  LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CLEAR  LS_ADVREC_ASSIGN.
    LS_ADVREC_ASSIGN-VBELN = <L_DATA>-VBELN.
    LS_ADVREC_ASSIGN-POSNR = <L_DATA>-POSNR.
    LS_ADVREC_ASSIGN-ETENR = <L_DATA>-ETENR.
    INSERT LS_ADVREC_ASSIGN INTO TABLE LT_ADVREC_ASSIGN.
  ENDLOOP.

  LOOP AT LT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>).
*   Assign Remainging
    LF_AMOUNT = <L_ITEM>-REMAM.
    IF LF_AMOUNT GT 0.

      INSERT VALUE #( BUKRS = <L_ITEM>-BUKRS
                      BELNR = <L_ITEM>-BELNR
                      GJAHR = <L_ITEM>-GJAHR
                      BUDAT = <L_ITEM>-BUDAT
                      DMBTR = LF_AMOUNT
                      WAERS = <L_ITEM>-WAERS )
             INTO TABLE CT_ADVREC.
    ENDIF.
  ENDLOOP.

* ---------------------
* Save Data
* ---------------------
  CALL METHOD ZCL_SDSSD_ORDER_CONFIRMATION=>SAVE_ADVREC_ASSIGNMENT
    EXPORTING
      IT_ADVREC_ASSIGN = LT_ADVREC_ASSIGN
      IF_COMMIT        = GC_TRUE
    IMPORTING
      ES_RETURN        = LS_RETURN.
  IF LS_RETURN IS NOT INITIAL.
*   Error
    MESSAGE ID LS_RETURN-ID
            TYPE   LS_RETURN-TYPE
            NUMBER LS_RETURN-NUMBER DISPLAY LIKE 'E'
            WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    RETURN.
  ENDIF.

* Save Successfully
* Message: Advance receive assignement has been deleted.
  MESSAGE S022(ZSDSSD01).

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_GENC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_GENC  CHANGING P_ADVREC_DIFF TYPE WERT1.

  CONSTANTS:
    LC_ADVREC_DIFF TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_DIFF'.

*  STATICS:
*    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC        TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C,
    LT_RANGE_PARAM TYPE RANGE OF ZSDSCAC001-PARAM.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSSD_ORDER_CONFIRMATION'.


** Check Already Read?
*  IF LF_READ EQ 'X'.
*    RETURN.
*  ENDIF.

* Initialize Output
  CLEAR: P_ADVREC_DIFF.

  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LC_ADVREC_DIFF ) TO LT_RANGE_PARAM.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID  = LF_REPID
      IRT_PARAM = LT_RANGE_PARAM
    IMPORTING
      ET_GEN_C  = LT_GENC.

** Mark Read
*  LF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Allowance
*     ------------------------------------
      WHEN LC_ADVREC_DIFF.
        READ TABLE GT_DATA ASSIGNING FIELD-SYMBOL(<LFS_DATA>) INDEX 1.
        IF SY-SUBRC = 0.
          IF <LFS_DATA>-ADVGP = <L_GENC>-PARAM_EXT.
            P_ADVREC_DIFF = <L_GENC>-VALUE_LOW.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDFORM.
