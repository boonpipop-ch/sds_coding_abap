*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0450
*  Creation Date      : 27.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : Blocked SD Document
*  Copied from        : Function spec is referred to VKM1
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  10.01.2025   F36K910932  Boontip R. IMS# 420000195 (CH01)
*  approve at the same time , add checking before call standard function
*-----------------------------------------------------------------------
*  03.02.2025   F36K911951  Boontip R. IMS# 420000363 (CH02)
*  add field VBAK-CMNGV, VBAK-CM_LAST_CHECK, VBAK-VDATU
*-----------------------------------------------------------------------
*  03.02.2025   F36K912194  Boontip R. IMS# 420000363 (CH03)
*  report: change VBAK-CMNGV to be "delivery date" , remove VBAK-VDATU
*  selection screen: change text of S_CMNGV from "Next shipping date" to
*  "Delivery date" , remove S_VDATU ( Delivery date )
*-----------------------------------------------------------------------

REPORT ZSDSFIR0450.
INCLUDE ZSDSCAI9990 ##INCL_OK.
INCLUDE <ICON>      ##INCL_OK.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBKRED,
*        VBEP,
*        VBAK, "CH02+ "CH03-
        SYST. "CH02+
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
"-- REFER DECLARATION IN POSTAB IN RVKREDTO
TYPES:
  BEGIN OF TS_POSTAB .
    INCLUDE STRUCTURE VBKRED.
TYPES:
    STATUS_BEL TYPE ICON-ID,
    ABGRU_NEW  TYPE VBAP-ABGRU,
    SBGRP_NEW  TYPE VBAK-SBGRP,
    COL(3)     TYPE C,           " Farbfeld für ALV
  END OF TS_POSTAB,
  TT_POSTAB TYPE STANDARD TABLE OF TS_POSTAB,
  BEGIN OF TS_VBAK,
    VBELN         TYPE VBAK-VBELN,
    AUART         TYPE VBAK-AUART,
    VDATU         TYPE VBAK-VDATU,         "CH02+
    CM_LAST_CHECK TYPE VBAK-CM_LAST_CHECK, "CH02+
  END OF TS_VBAK,
  TT_VBAK TYPE SORTED TABLE OF TS_VBAK WITH NON-UNIQUE KEY VBELN,
  BEGIN OF TS_DATA,
    LIGHT         TYPE ICON-ID,
    MARK          TYPE FLAG,
    MSGTX         TYPE TEXT80,
    ACTION        TYPE ICON-ID,
    RJRSN         TYPE ZSDSFIT053-RJRSN,
*    ABGRU       TYPE TVAGT-ABGRU,
*    ABGRU_TX    TYPE TVAGT-BEZEI,
    VBELN         TYPE VBKRED-VBELN,
    VBTYP         TYPE VBKRED-VBTYP,
    AUART         TYPE VBAK-AUART,
    AUART_TX      TYPE TVAKT-BEZEI,
    VKORG         TYPE VBKRED-VKORG,
    VKORG_TX      TYPE TVKOT-VTEXT,
    VTWEG         TYPE VBKRED-VTWEG,
    VTWEG_TX      TYPE TVTWT-VTEXT,
    SPART         TYPE VBKRED-SPART,
    SPART_TX      TYPE TSPAT-VTEXT,
    KNKLI         TYPE VBKRED-KNKLI,
    NAME1         TYPE VBKRED-NAME1,
    CTLPC         TYPE VBKRED-CTLPC,
    CTLPC_TX      TYPE UKM_RISK_CL0T-RISK_CLASS_TXT,
    VKBUR         TYPE VBKRED-VKBUR,
    VKBUR_TX      TYPE TVKBT-BEZEI,
    VKGRP         TYPE VBKRED-VKGRP,
    VKGRP_TX      TYPE TVGRT-BEZEI,
    NETWR         TYPE VBKRED-NETWR,
    AMTBL         TYPE VBAK-AMTBL,
    WAERK         TYPE VBKRED-WAERK,
    CMGST         TYPE VBKRED-CMGST,
    CMGST_TX      TYPE DD07T-DDTEXT,
    SALESMAN      TYPE VBPA-PERNR,
    SALESMAN_NM   TYPE TEXT40, "pa0002-vorna + pa0002-nachn,
    BSTNK         TYPE VBKRED-BSTNK,
    ZTERM         TYPE VBKRED-ZTERM,
    CMNGV         TYPE VBKRED-CMNGV,             "CH02+
*    VDATU         TYPE VBAK-VDATU,               "CH02+ "CH03-
    CM_LAST_CHECK TYPE SY-DATUM,                 "CH02+
    FIELD_STYLE   TYPE LVC_T_STYL,
    MSGTY         TYPE MSGTY,
    ROW_COLOR     TYPE CHAR4,
    COL_COLOR     TYPE LVC_T_SCOL,
  END OF TS_DATA,
  TT_DATA TYPE STANDARD TABLE OF TS_DATA.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA:
  GT_POSTAB TYPE TT_POSTAB ##NEEDED,
  GT_DATA   TYPE TT_DATA ##NEEDED.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
DATA:
  GR_GBSTK TYPE RANGE OF VBUK-GBSTK ##NEEDED.
*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
DATA:
  GF_ABGRU_DEFAULT TYPE VBAP-ABGRU ##NEEDED.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF GC_VBTYP,
    SO TYPE VBAK-VBTYP VALUE 'C',
  END OF GC_VBTYP,
  GC_COLOR_GREEN TYPE CHAR4 VALUE 'C500',
  GC_COLOR_RED   TYPE CHAR4 VALUE 'C600',
  BEGIN OF GC_INPUT_COLOR,
    COL TYPE LVC_COL VALUE 3,
    INT TYPE LVC_INT VALUE 0,
    INV TYPE LVC_INV VALUE 0,
  END OF GC_INPUT_COLOR,
  GC_CMGST_NOT_APPROVED TYPE VBAK-CMGST VALUE 'B'. "CH01+
*----------------------------------------------------------------------*
* ALV
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
DEFINE %FCAT.
  <l_fcat>-scrtext_s = <l_fcat>-scrtext_m =
  <l_fcat>-scrtext_l = <l_fcat>-coltext =
  <l_fcat>-seltext   = <l_fcat>-tooltip =
  <l_fcat>-reptext   = &1.
END-OF-DEFINITION.

DATA:   GF_EDIT    TYPE  CHAR1 ##NEEDED.
CONSTANTS:
  GC_TRUE            TYPE FLAG VALUE 'X',
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK CREDIT WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS: S_KKBER    FOR VBKRED-KKBER MEMORY ID KKB,
                  S_SBGRP    FOR VBKRED-SBGRP MEMORY ID KBG.

  SELECTION-SCREEN SKIP.
  SELECT-OPTIONS: S_CMLST    FOR SYST-DATUM. "CH02+
*                  S_VDATU    FOR VBAK-VDATU. "CH02+ CH03-
  SELECT-OPTIONS: S_CMNGV    FOR VBKRED-CMNGV,
                  S_KNKLI    FOR VBKRED-KNKLI MEMORY ID KUN
                           MATCHCODE OBJECT DEBI,
                  S_CTLPC    FOR VBKRED-CTLPC,
                  S_GRUPP    FOR VBKRED-GRUPP.

  SELECTION-SCREEN SKIP.
  SELECT-OPTIONS: S_VTWEG FOR VBKRED-VTWEG,
                  S_SPART FOR VBKRED-SPART,
                  S_VKBUR FOR VBKRED-VKBUR.
*                  S_EDATU FOR VBEP-EDATU, "CH02- >> it will use S_CMNGV as delivery date
SELECTION-SCREEN END OF BLOCK CREDIT.

SELECTION-SCREEN BEGIN OF BLOCK ALV WITH FRAME TITLE TEXT-S02.
  PARAMETERS: P_VARI TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK ALV.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
* Process on value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM  F_F4_FOR_VARIANT CHANGING P_VARI.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_VALUE.
  PERFORM F_SUBMIT_RVKRED01 CHANGING GT_POSTAB .
  PERFORM F_SELECT_AND_PREPARE_DATA USING GT_POSTAB
                                    CHANGING GT_DATA.
  IF GT_DATA IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_DISPLAY_RESULT USING GT_DATA.
*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS_1 INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE EXIT_COMMANDS_2 INPUT.
  LOOP AT GT_DATA TRANSPORTING NO FIELDS WHERE ACTION IS NOT INITIAL
                                         AND   MSGTY IS INITIAL      ##NEEDED.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC = 0.
    PERFORM F_POPUP_TO_CONFIRM .
  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " EXIT_COMMANDS_1  INPUT
*&---------------------------------------------------------------------*
*& Form f_f4_for_variant
*&---------------------------------------------------------------------*
FORM F_F4_FOR_VARIANT CHANGING CF_VARI TYPE DISVARIANT-VARIANT.
  DATA: LS_VARIANT      TYPE DISVARIANT.

  DATA: LF_SAVE(1) TYPE C VALUE 'A', " To save the variant
        LF_EXIT(1) TYPE C. " To get Dialog cancelled by user

  CLEAR LS_VARIANT.
  LS_VARIANT-REPORT = SY-REPID.

*-- Get variant
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = LS_VARIANT
      I_SAVE        = LF_SAVE
    IMPORTING
      E_EXIT        = LF_EXIT
      ES_VARIANT    = LS_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF SY-SUBRC = 0 AND LF_EXIT = SPACE.
    CF_VARI = LS_VARIANT-VARIANT.
  ENDIF.
ENDFORM.                    " f_f4_for_variant
*&---------------------------------------------------------------------*
*& Form f_submit_RVKRED01
*&---------------------------------------------------------------------*
FORM F_SUBMIT_RVKRED01 CHANGING CT_POSTAB TYPE TT_POSTAB.
  DATA: LO_DATA TYPE REF TO DATA.
  FIELD-SYMBOLS: <L_DATA> TYPE ANY TABLE.

  CL_SALV_BS_RUNTIME_INFO=>SET( EXPORTING DISPLAY  = ABAP_FALSE
                                          METADATA = ABAP_FALSE
                                          DATA     = ABAP_TRUE ).
  SUBMIT RVKRED01 WITH KKBER IN S_KKBER                  "#EC CI_SUBMIT
                  WITH SBGRP IN S_SBGRP
                  WITH CMNGV IN S_CMNGV
                  WITH KNKLI IN S_KNKLI
                  WITH CTLPC IN S_CTLPC
                  WITH GRUPP IN S_GRUPP
                  WITH CMGST EQ 'B'
                  WITH GBSTK IN GR_GBSTK
                  WITH VKBUR IN S_VKBUR
                  WITH ORDER EQ 'X'
                  WITH DELIV EQ ' '
                  WITH CALLMODE EQ 'X'
                  WITH P_VARI   EQ ' '
                  WITH CODE EQ 'VKM1'
                  AND RETURN.
  TRY.
      CL_SALV_BS_RUNTIME_INFO=>GET_DATA_REF(
        IMPORTING R_DATA = LO_DATA  ).
      IF LO_DATA IS BOUND.
        ASSIGN LO_DATA->* TO  <L_DATA>.
        MOVE-CORRESPONDING <L_DATA> TO CT_POSTAB[].
      ENDIF.
    CATCH CX_SALV_BS_SC_RUNTIME_INFO.
      MESSAGE E000(ZSDSCA01) WITH 'Unable to retrieve ALV data'(m02).
  ENDTRY.
  CL_SALV_BS_RUNTIME_INFO=>CLEAR_ALL( ) .

  DELETE CT_POSTAB WHERE VBTYP <> GC_VBTYP-SO
                   OR    VTWEG NOT IN S_VTWEG
                   OR    SPART NOT IN S_SPART.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INIT_VALUE
*&---------------------------------------------------------------------*
FORM F_INIT_VALUE .
  INSERT VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'A'
                  HIGH   = '' )
  INTO TABLE GR_GBSTK.
  INSERT VALUE #( SIGN   = 'I'
                  OPTION = 'EQ'
                  LOW    = 'B'
                  HIGH   = '' )
  INTO TABLE GR_GBSTK.

  SELECT VALUE_LOW                                      "#EC CI_NOORDER
  INTO @GF_ABGRU_DEFAULT
  UP TO 1 ROWS
  FROM ZSDSCAC001
  WHERE REPID = @SY-REPID
  AND   PARAM = 'REASON_FOR_REJECT_DEFAULT'.
  ENDSELECT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_AND_PREPARE_DATA
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_DATA  USING    UT_POSTAB TYPE TT_POSTAB
                                CHANGING CT_DATA TYPE TT_DATA.
  DATA: LS_DATA TYPE TS_DATA,
        LT_VBAK TYPE TT_VBAK.
  IF UT_POSTAB IS INITIAL.
    RETURN.
  ENDIF.
*BOD CH02
*  SELECT A~VBELN,
*         A~POSNR,
*         A~VRKME,
*         A~EDATU,
*         A~BMENG
*  FROM VBEP AS A
*  FOR ALL ENTRIES IN @UT_POSTAB
*  WHERE A~VBELN =  @UT_POSTAB-VBELN
*  AND   A~EDATU IN @S_EDATU
*  INTO TABLE @DATA(LT_VBEP).
*  SORT LT_VBEP BY VBELN POSNR EDATU DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM LT_VBEP COMPARING VBELN POSNR.
*EOD CH02

  SELECT VBELN,
         AUART,
         VDATU,
         CM_LAST_CHECK "CH02+
  FROM VBAK
  FOR ALL ENTRIES IN @UT_POSTAB
  WHERE VBELN = @UT_POSTAB-VBELN
*  AND   VDATU IN @S_VDATU
  INTO TABLE @LT_VBAK.

  SELECT *
  INTO TABLE @DATA(LT_053)
  FROM ZSDSFIT053
  FOR ALL ENTRIES IN @UT_POSTAB
  WHERE VBELN = @UT_POSTAB-VBELN.
  SORT LT_053 BY VBELN   .


* -----------------  loop to prepare CT_DATA -----------------
  LOOP AT UT_POSTAB ASSIGNING FIELD-SYMBOL(<L_POSTAB>).
    CLEAR: LS_DATA.
    MOVE-CORRESPONDING <L_POSTAB> TO LS_DATA ##ENH_OK.
*BOD CH02
*    READ TABLE LT_VBEP TRANSPORTING NO FIELDS WITH KEY VBELN = <L_POSTAB>-VBELN
*                                                     BINARY SEARCH.
*    IF SY-SUBRC <> 0
*    AND S_EDATU[] IS NOT INITIAL.
*      CONTINUE.
*    ENDIF.
*EOD CH02
    READ TABLE LT_053 INTO DATA(LS_053) WITH KEY VBELN = <L_POSTAB>-VBELN
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_DATA-RJRSN = LS_053-RJRSN.
    ENDIF.

    READ TABLE LT_VBAK INTO DATA(LS_VBAK) WITH KEY VBELN =  <L_POSTAB>-VBELN
                                                   BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_DATA-AUART = LS_VBAK-AUART.
*      LS_DATA-VDATU = LS_VBAK-VDATU.  "CH02+CH03-
*BOI CH02

      CONVERT TIME STAMP  LS_VBAK-CM_LAST_CHECK TIME ZONE SYST-ZONLO
      INTO DATE LS_DATA-CM_LAST_CHECK  .
      IF S_CMLST[] IS NOT INITIAL
      AND LS_DATA-CM_LAST_CHECK NOT IN  S_CMLST[].
        CONTINUE.
      ENDIF.
*EOI CH02
*BOD CH03
**BOI CH02
*    ELSE.
*      IF S_VDATU[] IS NOT INITIAL.
*        CLEAR LS_VBAK.
*        CONTINUE.
*      ENDIF.
**EOI CH02
*EOD CH03
    ENDIF.
    LS_DATA-AMTBL = <L_POSTAB>-KWKKC.

    PERFORM F_SELECT_AUART_TX  USING LT_VBAK
                                    LS_DATA-AUART
                               CHANGING LS_DATA-AUART_TX.
    PERFORM F_SELECT_VKORG_TX USING UT_POSTAB
                                    LS_DATA-VKORG
                              CHANGING LS_DATA-VKORG_TX.
    PERFORM F_SELECT_VTWEG_TX USING UT_POSTAB
                                    LS_DATA-VTWEG
                              CHANGING LS_DATA-VTWEG_TX.
    PERFORM F_SELECT_SPART_TX USING UT_POSTAB
                                    LS_DATA-SPART
                              CHANGING LS_DATA-SPART_TX.
    PERFORM F_SELECT_CTLPC_TX USING UT_POSTAB
                                    LS_DATA-CTLPC
                              CHANGING LS_DATA-CTLPC_TX.
    PERFORM F_SELECT_VKBUR_TX USING UT_POSTAB
                                    LS_DATA-VKBUR
                              CHANGING LS_DATA-VKBUR_TX.
    PERFORM F_SELECT_VKGRP_TX USING UT_POSTAB
                                    LS_DATA-VKGRP
                              CHANGING LS_DATA-VKGRP_TX.
    PERFORM F_SELECT_CMGST_TX USING LS_DATA-CMGST
                              CHANGING LS_DATA-CMGST_TX.

    PERFORM F_SELECT_SALESMANS USING UT_POSTAB
                                     LS_DATA-VBELN
                               CHANGING LS_DATA-SALESMAN
                                        LS_DATA-SALESMAN_NM.
    APPEND LS_DATA TO CT_DATA.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_DATA TYPE TT_DATA.
* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
  IF GF_EDIT EQ SPACE.
*   Enable Soft Refresh only in display mode
    GF_SOFT_REFRESH_1 = GC_TRUE.
  ELSE.
*   No auto refresh in edit mode
    GF_NO_AUTO_REFRESH_1 = GC_TRUE.
  ENDIF.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_DATA TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat, sort
  PERFORM F_ALV_BUILD_FIELDCAT    USING UT_DATA
                                  CHANGING GT_FIELDCAT_1.

  PERFORM F_ALV_MODIFY_FC   CHANGING GT_FIELDCAT_1.
  GS_VARIANT_1-REPORT = SY-REPID.
  GS_VARIANT_1-VARIANT = P_VARI.

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
  CS_LAYOUT-SEL_MODE   = 'B'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.

  CS_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.
  CS_LAYOUT-INFO_FNAME = 'ROW_COLOR'.
  CS_LAYOUT-CTAB_FNAME = 'COL_COLOR'.
* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT USING UT_DATA TYPE ANY TABLE
                       CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA: LF_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
        LF_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
        LF_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
        LF_TABLE        TYPE REF TO DATA.

  FIELD-SYMBOLS: <TABLE> TYPE ANY TABLE.

* create unprotected table from import data
  CREATE DATA LF_TABLE LIKE  UT_DATA.
  ASSIGN LF_TABLE->* TO <TABLE>.

*...New ALV Instance ...............................................
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = LF_SALV_TABLE
        CHANGING
          T_TABLE      = <TABLE> ).
    CATCH CX_SALV_MSG ##NO_HANDLER.
  ENDTRY.
  LF_COLUMNS  = LF_SALV_TABLE->GET_COLUMNS( ).
  LF_AGGREGATIONS = LF_SALV_TABLE->GET_AGGREGATIONS( ).

  CALL METHOD CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG
    EXPORTING
      R_COLUMNS      = LF_COLUMNS
      R_AGGREGATIONS = LF_AGGREGATIONS
    RECEIVING
      T_FIELDCATALOG = CT_FIELDCAT[].

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<L_FCAT>) .
    CASE <L_FCAT>-FIELDNAME.
      WHEN 'HEAD'
      OR   'LIGHT'
      OR   'MSGTY'
      OR   'MSGTX'.
        <L_FCAT>-TECH = GC_TRUE.
      WHEN 'MARK'.
        %FCAT 'Select'(c01).
        <L_FCAT>-CHECKBOX = GC_TRUE.
        <L_FCAT>-EDIT     = GC_TRUE.
      WHEN 'ACTION'.
        %FCAT 'Action'(c03).
        <L_FCAT>-ICON = GC_TRUE.
      WHEN 'VBELN'.
        %FCAT 'SO No'(C06).
      WHEN 'NETWR'.
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'AMTBL'.
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'KWMENG'.
        <L_FCAT>-QFIELDNAME = 'VRKME'.
      WHEN 'BMENG'.
        <L_FCAT>-QFIELDNAME = 'VRKME'.
      WHEN 'BSTNK'.
        %FCAT 'PO No.'(c02).
      WHEN 'AUART'.
        %FCAT 'SO Type'(c07).
      WHEN 'AUART_TX'.
        %FCAT 'SO Type desc'(c08).
      WHEN 'VKORG'.
        %FCAT 'SOrg.'(c09).
      WHEN 'VKORG_TX'.
        %FCAT 'SOrg.desc'(c10).
      WHEN 'VTWEG'.
        %FCAT 'DChl.'(c11).
      WHEN 'VTWEG_TX'.
        %FCAT 'DChl.desc'(c12).
      WHEN 'SPART'.
        %FCAT 'Dv.'(c13).
      WHEN 'SPART_TX'.
        %FCAT 'Dv. desc'(c14).
      WHEN 'CTLPC'.
        %FCAT 'Risk Cat.'(c15).
      WHEN 'CTLPC_TX'.
        %FCAT 'Risk Cat. desc'(c16).
      WHEN 'VKGRP'.
        %FCAT 'SGrp'(c17).
      WHEN 'VKGRP_TX'.
        %FCAT 'SGrp desc'(c18).
      WHEN 'VKBUR'.
        %FCAT 'SOff.'(c19).
      WHEN 'VKBUR_TX'.
        %FCAT 'SOff. desc'(c20).
      WHEN 'CMGST'.
        %FCAT 'OvCS'(c21).
      WHEN 'CMGST_TX'.
        %FCAT 'OvCS desc'(c22).
      WHEN 'SALESMAN'.
        %FCAT 'Salesman'(c23).
      WHEN 'SALESMAN_NM'.
        %FCAT 'Salesman Name'(c24).
*BOI CH02
      WHEN 'CMNGV'.
*        %FCAT 'Next shipping date'(c27).  "CH03-
         %FCAT 'Delivery date '(C25).      "CH03+
*BOD CH03
*      WHEN 'VDATU'.
*        %FCAT 'Delivery date '(C25).
*EOD CH03
      WHEN 'CM_LAST_CHECK'.
        %FCAT 'Submit to credit date'(C26).
*EOI CH02
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_1 USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                               UF_ONF4 TYPE  CHAR01          ##NEEDED
                               UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                               UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                               UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.

  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.
      WHEN 'MARK'.
        LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA2>) WHERE VBELN = <L_DATA>-VBELN.
          <L_DATA2>-MARK = <L_GOOD_CELL>-VALUE.
        ENDLOOP.
        LF_REFRESH = GC_TRUE.

    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    CALL METHOD GREF_GRID_1->GET_FRONTEND_LAYOUT
      IMPORTING
        ES_LAYOUT = DATA(LS_LAYOUT).

    LS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
    CALL METHOD GREF_GRID_1->SET_FRONTEND_LAYOUT
      EXPORTING
        IS_LAYOUT = LS_LAYOUT.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.
    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = ''.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM TYPE SY-UCOMM ##CALLED.
  DATA:
    LF_VALID   TYPE CHAR01  ##NEEDED,
    LF_REFRESH TYPE CHAR01  ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
  CASE UF_UCOMM.
    WHEN 'SAVE_1'.
      CALL SCREEN '910' . "refered to screen 203 in RVKRED01
    WHEN 'ZAPV'.
      PERFORM F_MARK_APPROVE.
    WHEN 'ZREJ'.
      PERFORM F_MARK_REJECT.
    WHEN 'ZUNDO'.
      PERFORM F_MARK_UNDO.
    WHEN 'ZRFS'.
      PERFORM F_REFRESH_DATA.
  ENDCASE.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.
  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& MODULE SUPPRESS_DIALOG OUTPUT
*&---------------------------------------------------------------------*
MODULE SUPPRESS_DIALOG OUTPUT.
  SUPPRESS DIALOG.
ENDMODULE.
*&---------------------------------------------------------------------*
*& MODULE SICHERN
*&---------------------------------------------------------------------*
MODULE SICHERN.
  PERFORM F_SAVE_ORDER.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_SAVE_ORDER
*&---------------------------------------------------------------------*
FORM F_SAVE_ORDER .
  DATA:
    LS_STABLE TYPE LVC_S_STBL,
    LF_SUBRC  TYPE SY-SUBRC,
    LS_053    TYPE ZSDSFIT053.

  LOOP AT GT_DATA TRANSPORTING NO FIELDS WHERE ACTION <> ''.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    MESSAGE S000(ZSDSCA01) WITH 'Please select line to approve or reject'(m04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE GT_DATA TRANSPORTING NO FIELDS WITH KEY ACTION = ICON_REJECT
                                                     RJRSN = ''.
  IF SY-SUBRC = 0.
    MESSAGE S000(ZSDSCA01) WITH 'Please input reject reason'(m06) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SD_CREDIT_REFRESH'
    EXPORTING
      CHECK_NO_REFRESH = ' '.
  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) WHERE ACTION <> ''
                                                   AND MSGTY <> 'S'.
*BOI CH01
    SELECT SINGLE CMGST, GBSTK
    INTO   @DATA(LS_VBAK)
    FROM   VBAK
    WHERE  VBELN = @<L_DATA>-VBELN.


    IF SY-SUBRC = 0 .
      IF LS_VBAK-CMGST <> GC_CMGST_NOT_APPROVED
      OR LS_VBAK-GBSTK NOT IN GR_GBSTK.
        <L_DATA>-LIGHT = ICON_RED_LIGHT.
        <L_DATA>-MSGTY = 'E'.
        <L_DATA>-MSGTX = 'Aleardy processed by other person'(M08).
        CONTINUE.
      ENDIF.
    ELSE.
      CLEAR LS_VBAK.
    ENDIF.
*EOI CH01
    IF <L_DATA>-ACTION = ICON_RELEASE.
      CALL FUNCTION 'SD_ORDER_CREDIT_RELEASE'
        EXPORTING
          VBELN         = <L_DATA>-VBELN
        EXCEPTIONS
          ERROR_MESSAGE = 4.
      LF_SUBRC = SY-SUBRC.
    ELSEIF <L_DATA>-ACTION = ICON_REJECT.
      CALL FUNCTION 'SD_ORDER_CREDIT_CANCEL'
        EXPORTING
          VBELN         = <L_DATA>-VBELN
          ABGRU         = GF_ABGRU_DEFAULT "<L_DATA>-ABGRU
        EXCEPTIONS
          ERROR_MESSAGE = 4.
      LF_SUBRC = SY-SUBRC.
    ELSE.
      CONTINUE.
    ENDIF.
    IF LF_SUBRC IS NOT INITIAL.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = SY-MSGID
          MSGNR               = SY-MSGNO
          MSGV1               = SY-MSGV1
          MSGV2               = SY-MSGV2
          MSGV3               = SY-MSGV3
          MSGV4               = SY-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = <L_DATA>-MSGTX.
      <L_DATA>-LIGHT = ICON_RED_LIGHT.
      <L_DATA>-MSGTY = 'E'.
    ELSE.
      IF <L_DATA>-ACTION = ICON_REJECT.
        LS_053-VBELN = <L_DATA>-VBELN.
        LS_053-RJRSN = <L_DATA>-RJRSN.
        LS_053-UPDDT = SY-DATUM.
        LS_053-UPDTM = SY-UZEIT.
        LS_053-USNAM = SY-UNAME.
        MODIFY ZSDSFIT053 FROM LS_053.
      ENDIF.
      "remove successfully data from display
      DELETE GT_DATA.

*      <L_DATA>-LIGHT = ICON_GREEN_LIGHT.
*      <L_DATA>-MSGTY = 'S'.
*      IF <L_DATA>-ACTION = ICON_RELEASE.
*        <L_DATA>-MSGTX = 'Released sucessfully'(m03).
*      ELSEIF <L_DATA>-ACTION = ICON_REJECT.
*        <L_DATA>-MSGTX = 'Rejected sucessfully'(m09).
*      ENDIF.
*      DELETE <L_DATA>-FIELD_STYLE WHERE FIELDNAME = 'MARK'.
*      LS_STYLEROW-FIELDNAME = 'MARK' .
*      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*      INSERT LS_STYLEROW  INTO TABLE <L_DATA>-FIELD_STYLE.
*
*      DELETE <L_DATA>-COL_COLOR WHERE FNAME = 'RJRSN'.
    ENDIF.
  ENDLOOP.
  CALL METHOD GREF_GRID_1->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = DATA(LT_FCAT).

  CALL METHOD GREF_GRID_1->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = DATA(LS_LAYOUT).

  LS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<L_FCAT>) .
    CASE <L_FCAT>-FIELDNAME.
      WHEN 'LIGHT'.
        %FCAT 'Status'(c04).
        <L_FCAT>-TECH = ABAP_FALSE.
        <L_FCAT>-NO_OUT = ABAP_FALSE.
        <L_FCAT>-ICON = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FCAT>-TECH = ABAP_FALSE.
        <L_FCAT>-NO_OUT = ABAP_FALSE.
        %FCAT 'Message'(c05).
    ENDCASE.
  ENDLOOP.
  CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = LT_FCAT.

  CALL METHOD GREF_GRID_1->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = LS_LAYOUT.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MARK_APPROVE
*&---------------------------------------------------------------------*
FORM F_MARK_APPROVE .
  DATA: LS_STYLEROW TYPE LVC_S_STYL.
  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) WHERE MARK = ABAP_TRUE.
    <L_DATA>-ACTION = ICON_RELEASE .
    CLEAR <L_DATA>-MARK .

    DELETE <L_DATA>-FIELD_STYLE WHERE FIELDNAME = 'RJRSN'.
    LS_STYLEROW-FIELDNAME = 'RJRSN' .
    LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT LS_STYLEROW  INTO TABLE <L_DATA>-FIELD_STYLE.

    <L_DATA>-ROW_COLOR = GC_COLOR_GREEN.
    DELETE <L_DATA>-COL_COLOR WHERE FNAME = 'RJRSN'.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    MESSAGE S000(ZSDSCA01) WITH 'Please select line to approve'(m01).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MARK_REJECT
*&---------------------------------------------------------------------*
FORM F_MARK_REJECT .
  DATA: LS_STYLEROW  TYPE LVC_S_STYL,
        LS_CELLCOLOR TYPE LVC_S_SCOL.

  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) WHERE MARK = ABAP_TRUE.
    <L_DATA>-ACTION = ICON_REJECT .
    CLEAR <L_DATA>-MARK .

    DELETE <L_DATA>-FIELD_STYLE WHERE FIELDNAME = 'RJRSN'.
    LS_STYLEROW-FIELDNAME = 'RJRSN' .
    LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    INSERT LS_STYLEROW  INTO TABLE <L_DATA>-FIELD_STYLE.

    <L_DATA>-ROW_COLOR = GC_COLOR_RED.

    DELETE <L_DATA>-COL_COLOR WHERE FNAME = 'RJRSN'.
    LS_CELLCOLOR-FNAME = 'RJRSN'.
    LS_CELLCOLOR-COLOR = GC_INPUT_COLOR.
    INSERT LS_CELLCOLOR INTO TABLE <L_DATA>-COL_COLOR.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    MESSAGE S000(ZSDSCA01) WITH 'Please select line to reject'(m05).
    RETURN.
  ENDIF.
  MESSAGE S000(ZSDSCA01) WITH 'Please input reject reason'(m06).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MARK_undo
*&---------------------------------------------------------------------*
FORM F_MARK_UNDO .
  DATA: LS_STYLEROW TYPE LVC_S_STYL.

  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) WHERE MARK = ABAP_TRUE
                                                   AND   ACTION IS NOT INITIAL.
    CLEAR: <L_DATA>-ACTION,
           <L_DATA>-MARK.

    DELETE <L_DATA>-FIELD_STYLE WHERE FIELDNAME = 'RJRSN'.
    LS_STYLEROW-FIELDNAME = 'RJRSN' .
    LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT LS_STYLEROW  INTO TABLE <L_DATA>-FIELD_STYLE.

    CLEAR <L_DATA>-ROW_COLOR.

    DELETE <L_DATA>-COL_COLOR WHERE FNAME = 'RJRSN'.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    MESSAGE S000(ZSDSCA01) WITH 'No line to undo'(m07).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_popup_to_confirm
*&---------------------------------------------------------------------*
FORM F_POPUP_TO_CONFIRM .
  DATA: LF_ANS     TYPE CHAR01.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Please confirm'(p01)
      TEXT_QUESTION         = 'Data was changed. Do you want to exit without saving?'(p02)
      TEXT_BUTTON_1         = 'Yes'(p03)
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = 'No'(p04)
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DISPLAY_CANCEL_BUTTON = ' '
      POPUP_TYPE            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      ANSWER                = LF_ANS
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    LEAVE TO SCREEN 0.
  ELSE.
    IF LF_ANS = 1.
      LEAVE TO SCREEN 0 .
    ELSE.
      CLEAR GF_SAVE_OK.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REFRESH_DATA
*&---------------------------------------------------------------------*
FORM F_REFRESH_DATA .
  CLEAR: GT_POSTAB,
         GT_DATA.
  PERFORM F_SUBMIT_RVKRED01 CHANGING GT_POSTAB .
  PERFORM F_SELECT_AND_PREPARE_DATA USING GT_POSTAB
                                    CHANGING GT_DATA.
  IF GT_DATA IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_auart_tx
*&---------------------------------------------------------------------*
FORM F_SELECT_AUART_TX  USING    UT_VBAK TYPE TT_VBAK
                                 UF_AUART TYPE VBAK-AUART
                        CHANGING CF_AUART_TX TYPE TVAKT-BEZEI.
  TYPES:
    BEGIN OF LTS_TVAKT,
      AUART TYPE TVAKT-AUART,
      BEZEI TYPE TVAKT-BEZEI,
    END OF LTS_TVAKT,
    LTT_TVAKT TYPE SORTED TABLE OF LTS_TVAKT WITH NON-UNIQUE KEY AUART.
  STATICS: LT_TVAKT TYPE LTT_TVAKT.

  IF LT_TVAKT IS INITIAL
  AND UT_VBAK IS NOT INITIAL.
    SELECT  DISTINCT                                   "#EC CI_BUFFJOIN
            A~AUART,
            A~BEZEI
    FROM TVAKT AS A INNER JOIN @UT_VBAK AS B
    ON A~AUART = B~AUART
    WHERE SPRAS = @SY-LANGU
    INTO TABLE @LT_TVAKT.

  ENDIF.
  READ TABLE LT_TVAKT INTO DATA(LS_TVAKT) WITH KEY AUART = UF_AUART.
  IF SY-SUBRC = 0.
    CF_AUART_TX = LS_TVAKT-BEZEI.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_vkorg_tx
*&---------------------------------------------------------------------*
FORM F_SELECT_VKORG_TX  USING    UT_POSTAB TYPE TT_POSTAB
                                 UF_VKORG TYPE TS_DATA-VKORG
                        CHANGING CF_VKORG_TX TYPE TS_DATA-VKORG_TX.
  TYPES:
    BEGIN OF LTS_TVKOT,
      VKORG TYPE TVKOT-VKORG,
      VTEXT TYPE TVKOT-VTEXT,
    END OF LTS_TVKOT,
    LTT_TVKOT TYPE SORTED TABLE OF LTS_TVKOT WITH NON-UNIQUE KEY VKORG.
  STATICS: LT_TVKOT TYPE LTT_TVKOT.

  IF LT_TVKOT IS INITIAL
  AND UT_POSTAB IS NOT INITIAL.
    SELECT  DISTINCT                    "#EC CI_BUFFJOIN "#EC CI_BYPASS
            VKORG,
            VTEXT
    FROM TVKOT
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE VKORG = @UT_POSTAB-VKORG
    AND   SPRAS = @SY-LANGU
    INTO TABLE @LT_TVKOT.

  ENDIF.
  READ TABLE LT_TVKOT INTO DATA(LS_TVKOT) WITH KEY VKORG = UF_VKORG.
  IF SY-SUBRC = 0.
    CF_VKORG_TX = LS_TVKOT-VTEXT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_vtweg_tx
*&---------------------------------------------------------------------*
FORM F_SELECT_VTWEG_TX  USING    UT_POSTAB TYPE TT_POSTAB
                                 UF_VTWEG TYPE TS_DATA-VTWEG
                        CHANGING CF_VTWEG_TX TYPE TS_DATA-VTWEG_TX.
  TYPES:
    BEGIN OF LTS_TVTWT,
      VTWEG TYPE TVTWT-VTWEG,
      VTEXT TYPE TVTWT-VTEXT,
    END OF LTS_TVTWT,
    LTT_TVTWT TYPE SORTED TABLE OF LTS_TVTWT WITH NON-UNIQUE KEY VTWEG.
  STATICS: LT_TVTWT TYPE LTT_TVTWT.

  IF LT_TVTWT IS INITIAL
  AND UT_POSTAB IS NOT INITIAL.
    SELECT  DISTINCT                    "#EC CI_BUFFJOIN "#EC CI_BYPASS
            VTWEG,
            VTEXT
    FROM TVTWT
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE VTWEG = @UT_POSTAB-VTWEG
    AND   SPRAS = @SY-LANGU
    INTO TABLE @LT_TVTWT.

  ENDIF.
  READ TABLE LT_TVTWT INTO DATA(LS_TVTWT) WITH KEY VTWEG = UF_VTWEG.
  IF SY-SUBRC = 0.
    CF_VTWEG_TX = LS_TVTWT-VTEXT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_SPART_TX
*&---------------------------------------------------------------------*
FORM F_SELECT_SPART_TX  USING    UT_POSTAB TYPE TT_POSTAB
                                 UF_SPART TYPE TS_DATA-SPART
                        CHANGING CF_SPART_TX TYPE TS_DATA-SPART_TX.
  TYPES:
    BEGIN OF LTS_TSPAT,
      SPART TYPE TSPAT-SPART,
      VTEXT TYPE TSPAT-VTEXT,
    END OF LTS_TSPAT,
    LTT_TSPAT TYPE SORTED TABLE OF LTS_TSPAT WITH NON-UNIQUE KEY SPART.
  STATICS: LT_TSPAT TYPE LTT_TSPAT.

  IF LT_TSPAT IS INITIAL
  AND UT_POSTAB IS NOT INITIAL.
    SELECT  DISTINCT                     "#EC CI_BUFFJOIN #EC CI_BYPASS
            SPART,
            VTEXT
    FROM TSPAT
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE SPART = @UT_POSTAB-SPART
    AND   SPRAS = @SY-LANGU
    INTO TABLE @LT_TSPAT.

  ENDIF.
  READ TABLE LT_TSPAT INTO DATA(LS_TSPAT) WITH KEY SPART = UF_SPART.
  IF SY-SUBRC = 0.
    CF_SPART_TX = LS_TSPAT-VTEXT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_CTLPC_TX
*&---------------------------------------------------------------------*
FORM F_SELECT_CTLPC_TX  USING    UT_POSTAB TYPE TT_POSTAB
                                 UF_CTLPC TYPE TS_DATA-CTLPC
                        CHANGING CF_CTLPC_TX TYPE TS_DATA-CTLPC_TX.
  TYPES:
    BEGIN OF LTS_UKM_RISK_CL0T,
      RISK_CLASS     TYPE UKM_RISK_CL0T-RISK_CLASS,
      RISK_CLASS_TXT TYPE UKM_RISK_CL0T-RISK_CLASS_TXT,
    END OF LTS_UKM_RISK_CL0T,
    LTT_UKM_RISK_CL0T TYPE SORTED TABLE OF LTS_UKM_RISK_CL0T WITH NON-UNIQUE KEY RISK_CLASS.
  STATICS: LT_UKM_RISK_CL0T TYPE LTT_UKM_RISK_CL0T.

  IF LT_UKM_RISK_CL0T IS INITIAL
  AND UT_POSTAB IS NOT INITIAL.
    SELECT  DISTINCT                    "#EC CI_BUFFJOIN "#EC CI_BYPASS
            RISK_CLASS,
            RISK_CLASS_TXT
    FROM UKM_RISK_CL0T
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE RISK_CLASS = @UT_POSTAB-CTLPC
    AND   LANGU = @SY-LANGU
    INTO TABLE @LT_UKM_RISK_CL0T.

  ENDIF.
  READ TABLE LT_UKM_RISK_CL0T INTO DATA(LS_UKM_RISK_CL0T) WITH KEY RISK_CLASS = UF_CTLPC.
  IF SY-SUBRC = 0.
    CF_CTLPC_TX = LS_UKM_RISK_CL0T-RISK_CLASS_TXT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_VKBUR_TX
*&---------------------------------------------------------------------*
FORM F_SELECT_VKBUR_TX  USING    UT_POSTAB TYPE TT_POSTAB
                                 UF_VKBUR TYPE TS_DATA-VKBUR
                        CHANGING CF_VKBUR_TX TYPE TS_DATA-VKBUR_TX.
  TYPES:
    BEGIN OF LTS_TVKBT,
      VKBUR TYPE TVKBT-VKBUR,
      BEZEI TYPE TVKBT-BEZEI,
    END OF LTS_TVKBT,
    LTT_TVKBT TYPE SORTED TABLE OF LTS_TVKBT WITH NON-UNIQUE KEY VKBUR.
  STATICS: LT_TVKBT TYPE LTT_TVKBT.

  IF LT_TVKBT IS INITIAL
  AND UT_POSTAB IS NOT INITIAL.
    SELECT  DISTINCT                     "#EC CI_BUFFJOIN #EC CI_BYPASS
            VKBUR,
            BEZEI
    FROM TVKBT
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE VKBUR = @UT_POSTAB-VKBUR
    AND   SPRAS = @SY-LANGU
    INTO TABLE @LT_TVKBT.

  ENDIF.
  READ TABLE LT_TVKBT INTO DATA(LS_TVKBT) WITH KEY VKBUR = UF_VKBUR.
  IF SY-SUBRC = 0.
    CF_VKBUR_TX = LS_TVKBT-BEZEI.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_VKGRP_TX
*&---------------------------------------------------------------------*
FORM F_SELECT_VKGRP_TX  USING    UT_POSTAB TYPE TT_POSTAB
                                 UF_VKGRP TYPE TS_DATA-VKGRP
                        CHANGING CF_VKGRP_TX TYPE TS_DATA-VKGRP_TX.
  TYPES:
    BEGIN OF LTS_TVGRT,
      VKGRP TYPE TVGRT-VKGRP,
      BEZEI TYPE TVGRT-BEZEI,
    END OF LTS_TVGRT,
    LTT_TVGRT TYPE SORTED TABLE OF LTS_TVGRT WITH NON-UNIQUE KEY VKGRP.
  STATICS: LT_TVGRT TYPE LTT_TVGRT.

  IF LT_TVGRT IS INITIAL
  AND UT_POSTAB IS NOT INITIAL.
    SELECT  DISTINCT                    "#EC CI_BUFFJOIN "#EC CI_BYPASS
            VKGRP,
            BEZEI
    FROM TVGRT
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE VKGRP = @UT_POSTAB-VKGRP
    AND   SPRAS = @SY-LANGU
    INTO TABLE @LT_TVGRT.

  ENDIF.
  READ TABLE LT_TVGRT INTO DATA(LS_TVGRT) WITH KEY VKGRP = UF_VKGRP.
  IF SY-SUBRC = 0.
    CF_VKGRP_TX = LS_TVGRT-BEZEI.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_cmgst_tx
*&---------------------------------------------------------------------*
FORM F_SELECT_CMGST_TX  USING    UF_CMGST TYPE TS_DATA-CMGST
                        CHANGING UF_CMGST_TX TYPE TS_DATA-CMGST_TX.
  STATICS: LT_TVBST TYPE SORTED TABLE OF TVBST WITH NON-UNIQUE KEY STATU.
  IF LT_TVBST IS INITIAL.
    SELECT *                                           "#EC CI_BUFFJOIN
    INTO TABLE LT_TVBST
    FROM TVBST
    WHERE SPRAS = SY-LANGU
    AND   TBNAM = 'VBAK'
    AND   FDNAM = 'CMGST'.
  ENDIF.
  READ TABLE LT_TVBST INTO DATA(LS_TVBST) WITH KEY STATU = UF_CMGST.
  IF SY-SUBRC = 0.
    UF_CMGST_TX  = LS_TVBST-BEZEI.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_salesmans
*&---------------------------------------------------------------------*
FORM F_SELECT_SALESMANS  USING    UT_POSTAB TYPE TT_POSTAB
                                  UF_VBELN TYPE VBAK-VBELN
                         CHANGING CS_SALESMAN TYPE TS_DATA-SALESMAN
                                  CS_SALESMAN_NM TYPE TS_DATA-SALESMAN_NM.
  CONSTANTS: LC_PARVW_SALES TYPE VBPA-PARVW VALUE 'VE'.
  TYPES:
    BEGIN OF LTS_VBPA,
      VBELN TYPE VBPA-VBELN,
      PERNR TYPE VBPA-PERNR,
      VORNA TYPE PA0002-VORNA,
      NACHN TYPE PA0002-NACHN,
    END OF LTS_VBPA,
    TT_VBPA TYPE SORTED TABLE OF LTS_VBPA WITH NON-UNIQUE KEY VBELN.
  STATICS: LT_VBPA TYPE TT_VBPA.
  IF UT_POSTAB[] IS NOT INITIAL
  AND LT_VBPA IS INITIAL.
    SELECT VBELN,
           A~PERNR,
           VORNA,
           NACHN
    FROM VBPA AS A INNER JOIN PA0002 AS B
    ON   A~PERNR = B~PERNR
    FOR ALL ENTRIES IN @UT_POSTAB
    WHERE A~VBELN = @UT_POSTAB-VBELN
    AND  POSNR = '000000'
    AND  PARVW = @LC_PARVW_SALES
    INTO TABLE @LT_VBPA.
  ENDIF.
  READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = UF_VBELN.
  IF SY-SUBRC = 0.
    CS_SALESMAN = LS_VBPA-PERNR.
    CS_SALESMAN_NM = | { LS_VBPA-VORNA } { LS_VBPA-NACHN }|.
  ENDIF.
ENDFORM.
