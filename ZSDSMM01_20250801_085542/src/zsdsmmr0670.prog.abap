*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0670
*  Creation Date      : 12.12.2024
*  Author             : B CHIEWSARIKIJ (SDS)
*  Add-on ID          : N/A
*  Description        : Update Storage bin report
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
REPORT ZSDSMMR0670 MESSAGE-ID ZSDSMM01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: MARA,
        MARD.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF TYP_OUTPUT,
    FLAG    TYPE CHECKBOX,
    MATNR   TYPE MARD-MATNR,  "Material
    MAKTX   TYPE MAKT-MAKTX,  "Material Description
    WERKS   TYPE MARD-WERKS,  "Plant
    LGORT   TYPE MARD-LGORT,  "Storage Location
    LGPBE_O TYPE MARD-LGPBE,  "Storage Bin (Old)
    LGPBE_N TYPE MARD-LGPBE,  "Storage Bin (New)
  END OF TYP_OUTPUT.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
*DATA:
*-> internal tables
DATA: GT_OUTPUT TYPE STANDARD TABLE OF ZSDSMMS062.
*-> range
*-> work areas
DATA: GS_OUTPUT TYPE ZSDSMMS062.
*-> variables
*-> reference
*-> ALV
DATA:
  GT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV,                   "#EC NEEDED
  GS_LAYOUT     TYPE SLIS_LAYOUT_ALV,                       "#EC NEEDED
  GT_SORT       TYPE SLIS_T_SORTINFO_ALV,                   "#EC NEEDED
  GT_FILTER     TYPE SLIS_T_FILTER_ALV,                     "#EC NEEDED
  GT_EXCLUDING  TYPE SLIS_T_EXTAB,                          "#EC NEEDED
  GT_LISTHEADER TYPE SLIS_T_LISTHEADER,                     "#EC NEEDED
  GS_PRINT      TYPE SLIS_PRINT_ALV,                        "#EC NEEDED
  GS_KEYINFO    TYPE SLIS_KEYINFO_ALV,                      "#EC NEEDED
  GV_PF_STATUS  LIKE SY-PFKEY,                              "#EC NEEDED
  GS_VARIANT    LIKE DISVARIANT,                            "#EC NEEDED
  GS_ALV        TYPE ZSDSSDS057
  .
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS062'.

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
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
SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
  SELECT-OPTIONS: S_MATNR   FOR MARD-MATNR,
                  S_WERKS   FOR MARD-WERKS DEFAULT '1000' OBLIGATORY MODIF ID M01 NO-EXTENSION,
                  S_LGORT   FOR MARD-LGORT NO-EXTENSION,
                  S_LGPBE   FOR MARD-LGPBE NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK S01.

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
    IF SCREEN-NAME EQ 'S_LGORT-HIGH' OR
       SCREEN-NAME EQ 'S_WERKS-HIGH' OR
       SCREEN-NAME EQ 'S_LGPBE-HIGH'.
      SCREEN-ACTIVE     = 0.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'M01'.
      SCREEN-INPUT = 0.
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

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  PERFORM F_DISPLAY_RESULT.
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
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .

  "Get data from MARD
  SELECT
    MATNR,
    WERKS,
    LGORT,
    LGPBE
   FROM MARD
   INTO TABLE @DATA(LT_MARD)
   WHERE MATNR IN @S_MATNR
     AND WERKS IN @S_WERKS
     AND LGORT IN @S_LGORT
     AND LGPBE IN @S_LGPBE.
  IF SY-SUBRC = 0.
    SORT LT_MARD BY MATNR.

    "Material Description
    SELECT
      MATNR,
      MAKTX
    FROM MAKT
    INTO TABLE @DATA(LT_MAKT)
    FOR ALL ENTRIES IN @LT_MARD
    WHERE MATNR = @LT_MARD-MATNR
      AND SPRAS = 'E'.
    IF SY-SUBRC = 0.
      SORT LT_MAKT BY MATNR.
    ENDIF.
  ENDIF.

  "Append data to output
  LOOP AT LT_MARD INTO DATA(LS_MARD).

    APPEND INITIAL LINE TO GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LFS_OUTPUT>).
    MOVE-CORRESPONDING LS_MARD TO <LFS_OUTPUT>.
    <LFS_OUTPUT>-LGPBE_O = LS_MARD-LGPBE.

    READ TABLE LT_MAKT INTO DATA(LS_MAKT)
                       WITH KEY MATNR = LS_MARD-MATNR
                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      <LFS_OUTPUT>-MAKTX = LS_MAKT-MAKTX.
    ENDIF.

    <LFS_OUTPUT>-STATUS   =  ICON_LIGHT_OUT.
    UNASSIGN <LFS_OUTPUT>.

  ENDLOOP.

  SORT GT_OUTPUT BY MATNR WERKS LGORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& Display Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT .

  MC_SHOW_PROGRESS 99 TEXT-P99.

* set layout attribute
  PERFORM LAYOUT_BUILD.
* build field catalog
  PERFORM FIELDCAT_BUILD.
* Show alv report
  PERFORM ALV_GRID_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LAYOUT_BUILD
*&---------------------------------------------------------------------*
*& Layout Building
*&---------------------------------------------------------------------*
FORM LAYOUT_BUILD .
* Initialize Output
  CLEAR:  GS_LAYOUT, GS_VARIANT, GS_PRINT.

* determine layout
  GS_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.
  GS_LAYOUT-ZEBRA             = ABAP_TRUE.
  GS_LAYOUT-BOX_FIELDNAME     = 'FLAG'.

* For Variant Saving
  GS_VARIANT-REPORT   = SY-REPID.
*  gs_print-no_colwopt = abap_true.

  GS_LAYOUT-DETAIL_POPUP           = 'X'.   " display detailed pop-up
  GS_LAYOUT-DETAIL_INITIAL_LINES   = 'X'.   " display initial value
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*& Field category Building
*&---------------------------------------------------------------------*
FORM FIELDCAT_BUILD .
  FIELD-SYMBOLS :
    <LS_FCAT>   TYPE SLIS_FIELDCAT_ALV.

* build field catalog
  PERFORM ALV_FIELDCAT_BUILD  USING GC_STRUCTURE_1.

  DELETE GT_FIELDCAT WHERE FIELDNAME = 'FLAG'.
* Edit Field catalog
  LOOP AT GT_FIELDCAT ASSIGNING <LS_FCAT>.

* --- non-display
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'ZZZZZZZZZ'
      OR   'XZZZZZZZZ'
      .
        <LS_FCAT>-NO_OUT  = 'X'.
      WHEN OTHERS.
        <LS_FCAT>-NO_OUT  = ' '.
    ENDCASE.

*    CASE <LS_FCAT>-FIELDNAME.
*      WHEN 'LGPBE_O'.
*        <LS_FCAT>-OUTPUTLEN = '200'.
*      WHEN 'LGPBE_N'.
*        <LS_FCAT>-OUTPUTLEN = '200'.
*    ENDCASE.

* --- column text
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'FLAG'.
        <LS_FCAT>-SELTEXT_L    = TEXT-C01.
        <LS_FCAT>-SELTEXT_M    = TEXT-C01.
        <LS_FCAT>-SELTEXT_S    = TEXT-C01.
        <LS_FCAT>-REPTEXT_DDIC = TEXT-C01.
      WHEN 'LGPBE_O'.
        <LS_FCAT>-SELTEXT_L    = TEXT-C02.
        <LS_FCAT>-SELTEXT_M    = TEXT-C02.
        <LS_FCAT>-SELTEXT_S    = TEXT-C02.
        <LS_FCAT>-REPTEXT_DDIC = TEXT-C02.
      WHEN 'LGPBE_N'.
        <LS_FCAT>-SELTEXT_L    = TEXT-C03.
        <LS_FCAT>-SELTEXT_M    = TEXT-C03.
        <LS_FCAT>-SELTEXT_S    = TEXT-C03.
        <LS_FCAT>-REPTEXT_DDIC = TEXT-C03.
      WHEN 'STATUS'.
        <LS_FCAT>-SELTEXT_L    = TEXT-C04.
        <LS_FCAT>-SELTEXT_M    = TEXT-C04.
        <LS_FCAT>-SELTEXT_S    = TEXT-C04.
        <LS_FCAT>-REPTEXT_DDIC = TEXT-C04.
    ENDCASE.

    "Editable
    CASE <LS_FCAT>-FIELDNAME.
*      WHEN 'FLAG'.
*        <LS_FCAT>-CHECKBOX    = 'X'.
*        <LS_FCAT>-INPUT       = 'X'.
*        <LS_FCAT>-EDIT        = 'X'.
      WHEN 'LGPBE_N'.
        <LS_FCAT>-INPUT       = 'X'.
        <LS_FCAT>-EDIT        = 'X'.
      WHEN 'STATUS'.
        <LS_FCAT>-ICON        = ABAP_TRUE.
        <LS_FCAT>-COL_POS     = 1.
      WHEN OTHERS.
    ENDCASE.


  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*& ALV Display
*&---------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
      IT_SORT                  = GT_SORT
*     IT_FILTER                =
*     IS_SEL_HIDE              =
      I_DEFAULT                = 'X'
      I_SAVE                   = 'X'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_OUTPUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*& ALV Build Field Catagory
*&---------------------------------------------------------------------*
FORM ALV_FIELDCAT_BUILD USING IV_TABNAME TYPE SLIS_TABNAME.
  DATA : LT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV.
  REFRESH : LT_FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
*     I_INTERNAL_TABNAME     = IV_TABNAME
      I_STRUCTURE_NAME       = IV_TABNAME
      I_CLIENT_NEVER_DISPLAY = 'X'
      I_INCLNAME             = SY-REPID
      I_BYPASSING_BUFFER     = 'X'
      I_BUFFER_ACTIVE        = ' '
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    APPEND LINES OF LT_FIELDCAT TO GT_FIELDCAT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       Status
*----------------------------------------------------------------------*
FORM PF_STATUS_1 USING US_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDARD_FULLSCREEN'. "EXCLUDING us_extab.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       User Command
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.

  ENDIF.
*&---------------------------------------------------------------------*

  CASE P_UCOMM.
    WHEN '&IC1'.
*      PERFORM f_call_transection.
    WHEN '&DATA_SAVE'.
      PERFORM F_SAVE_DATA.
    WHEN '&ZALL'.
      PERFORM F_SELECT USING 'X'.
    WHEN '&ZSAL'.
      PERFORM F_SELECT USING SPACE.
*    WHEN '&ZPRINT'.
*      PERFORM PRINT_PROCESS.
*      PERFORM UPDATE_LOG.
  ENDCASE.

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM F_SELECT USING LV_CHECK.

  GS_OUTPUT-FLAG = LV_CHECK.
  MODIFY GT_OUTPUT FROM GS_OUTPUT  TRANSPORTING FLAG
                                   WHERE FLAG NE GS_OUTPUT-FLAG.

ENDFORM.                    " F_SELECT
*&---------------------------------------------------------------------*
*& Form F_SAVE_DATA
*&---------------------------------------------------------------------*
*& Save
*&---------------------------------------------------------------------*
FORM F_SAVE_DATA .

  DATA: LS_RETURN       TYPE BAPIRET2,
        LS_HEADER       TYPE BAPIMATHEAD,
        LS_STORAGE_LOC  TYPE BAPI_MARD,
        LS_STORAGE_LOCX TYPE BAPI_MARDX,
        LS_DATA         LIKE LINE OF GT_OUTPUT.

  DATA(LT_DATA) = GT_OUTPUT[].
  DELETE LT_DATA WHERE FLAG IS INITIAL.

  LOOP AT LT_DATA INTO LS_DATA.

    LS_HEADER-MATERIAL      = LS_DATA-MATNR.
    LS_STORAGE_LOC-PLANT    = LS_DATA-WERKS.
    LS_STORAGE_LOC-STGE_LOC = LS_DATA-LGORT.
    LS_STORAGE_LOC-STGE_BIN = LS_DATA-LGPBE_N.

    "Mark X
    MOVE: LS_DATA-WERKS TO LS_STORAGE_LOCX-PLANT,
          LS_DATA-LGORT TO LS_STORAGE_LOCX-STGE_LOC,
          ABAP_TRUE TO LS_STORAGE_LOCX-STGE_BIN.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA             = LS_HEADER
        STORAGELOCATIONDATA  = LS_STORAGE_LOC
        STORAGELOCATIONDATAX = LS_STORAGE_LOCX
      IMPORTING
        RETURN               = LS_RETURN.
    IF LS_RETURN-TYPE = 'E' OR LS_RETURN-TYPE = 'A'.
      READ TABLE GT_OUTPUT ASSIGNING FIELD-SYMBOL(<LFS_OUTPUT>)
                            WITH KEY MATNR = LS_DATA-MATNR
                                     WERKS = LS_DATA-WERKS
                                     LGORT = LS_DATA-LGORT
                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        CLEAR <LFS_OUTPUT>-LGPBE_N.
        <LFS_OUTPUT>-STATUS = ICON_RED_LIGHT.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.

      READ TABLE GT_OUTPUT ASSIGNING <LFS_OUTPUT>
                            WITH KEY MATNR = LS_DATA-MATNR
                                     WERKS = LS_DATA-WERKS
                                     LGORT = LS_DATA-LGORT
                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LFS_OUTPUT>-LGPBE_O = <LFS_OUTPUT>-LGPBE_N.
        CLEAR <LFS_OUTPUT>-LGPBE_N.
        <LFS_OUTPUT>-STATUS = ICON_GREEN_LIGHT.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.
      .

    ENDIF.
  ENDLOOP.

  IF LT_DATA[] IS INITIAL.
    MESSAGE I001 WITH TEXT-I02.
  ELSE.
    MESSAGE I001 WITH TEXT-I01.
  ENDIF.


ENDFORM.
