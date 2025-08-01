*&---------------------------------------------------------------------*
*& REPORT RFIDTRBOE1                                                   *
*&---------------------------------------------------------------------*
*& Financal Accounting: Standard business transactions with checks/BoE *
*&---------------------------------------------------------------------*
REPORT ZSDSFIR0740 LINE-SIZE 80 LINE-COUNT 60 NO STANDARD PAGE HEADING
             MESSAGE-ID ICC_TR.

DATA : GV_KURZT TYPE T053-KURZT.

* Includes
* --------
INCLUDE RFIDTRBOE1_GLOBAL_DATA.
INCLUDE RSDBCOM4.

* Definition of first tabstrip subscreen
* --------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-003 FOR FIELD X1.
  PARAMETERS: X1 RADIOBUTTON GROUP ULI DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-004 FOR FIELD X2.
  PARAMETERS: X2 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-005 FOR FIELD X3.
  PARAMETERS: X3 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-006 FOR FIELD X4.
  PARAMETERS: X4 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-007 FOR FIELD X5.
  PARAMETERS: X5 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-008 FOR FIELD X6.
  PARAMETERS: X6 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-012 FOR FIELD X7.
  PARAMETERS: X7 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-009 FOR FIELD X8.
  PARAMETERS: X8 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE,
  COMMENT 1(32) TEXT-010 FOR FIELD X9.
  PARAMETERS: X9 RADIOBUTTON GROUP ULI.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1001.

* Definition of second tabstrip subscreen
* ---------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1002 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK TWO WITH FRAME TITLE CHAR60.
    SELECT-OPTIONS S_LIFNR FOR LFB1-LIFNR.
    PARAMETERS P_BANKN LIKE SKB1-SAKNR.
    PARAMETERS P_PORT1 LIKE T045P-PORTFO DEFAULT '?'.
    PARAMETERS P_PORT4 LIKE T045P-PORTFO DEFAULT '?'.
    PARAMETERS P_PORT2 LIKE T045P-PORTFO DEFAULT '?'.
    PARAMETERS P_PORT3 LIKE T045P-PORTFO DEFAULT '?'.
    SELECT-OPTIONS S_PORT1 FOR T045P-PORTFO.
    PARAMETERS P_WAERS LIKE BKPF-WAERS DEFAULT '?'.
    PARAMETERS P_UMSKZ LIKE BSEG-UMSKZ.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS S_BOENO FOR BSED-BOENO.
    SELECT-OPTIONS S_BANKK FOR BNKA-BANKL.
    SELECT-OPTIONS S_ACCOU FOR BSED-ACCOU.
    SELECT-OPTIONS S_BORDR FOR GF_BORDR.
    SELECT-OPTIONS S_BELNR FOR BKPF-BELNR.
    SELECT-OPTIONS S_ZFBDT FOR BSEG-ZFBDT.
    SELECT-OPTIONS S_WRBTR FOR BSEG-WRBTR.
    SELECT-OPTIONS S_GSBER FOR BSEG-GSBER.
    SELECT-OPTIONS S_KUNNR FOR KNB1-KUNNR.
    SELECT-OPTIONS S_FILKD FOR BSEG-FILKD.                  "1324719
  SELECTION-SCREEN END OF BLOCK TWO.
SELECTION-SCREEN END OF SCREEN 1002.

* Definition of third tabstrip subscreen
* --------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1003 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK THREE WITH FRAME TITLE TEXT-024.
    SELECT-OPTIONS S_XREF1 FOR BSEG-XREF1.
    SELECT-OPTIONS S_XREF2 FOR BSEG-XREF2.
    SELECT-OPTIONS S_XREF3 FOR BSEG-XREF3.
  SELECTION-SCREEN END OF BLOCK THREE.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF BLOCK FOUR WITH FRAME TITLE TEXT-025.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-022.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-019 FOR FIELD Y1.
    PARAMETERS: Y1 RADIOBUTTON GROUP AKTZ DEFAULT 'X'.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-020 FOR FIELD Y2.
    PARAMETERS: Y2 RADIOBUTTON GROUP AKTZ.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-021 FOR FIELD Y3.
    PARAMETERS: Y3 RADIOBUTTON GROUP AKTZ.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-023.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-019 FOR FIELD Z1.
    PARAMETERS: Z1 RADIOBUTTON GROUP SIWE DEFAULT 'X'.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-020 FOR FIELD Z2.
    PARAMETERS: Z2 RADIOBUTTON GROUP SIWE.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: BEGIN OF LINE,
    COMMENT 1(30) TEXT-021 FOR FIELD Z3.
    PARAMETERS: Z3 RADIOBUTTON GROUP SIWE.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK FOUR.
SELECTION-SCREEN END OF SCREEN 1003.

* Begin of selectionscreen
* -------------------------
* selection-screen skip 1.
SELECTION-SCREEN BEGIN OF BLOCK FIVE WITH FRAME TITLE TEXT-027.
  PARAMETERS P_BLDAT LIKE RFPDO-WOBLBBLDAT OBLIGATORY DEFAULT SY-DATUM.
  PARAMETERS P_BUDAT LIKE RFPDO-WOBLBBUDAT OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK FIVE.
SELECTION-SCREEN SKIP 1.

* Definition of tabstrip
* ----------------------
SELECTION-SCREEN BEGIN OF TABBED BLOCK TABS1 FOR 15 LINES.
  SELECTION-SCREEN TAB (30) TAB1 USER-COMMAND UCO1          "#EC NEEDED
    DEFAULT SCREEN 1001.
  SELECTION-SCREEN TAB (30) TAB2 USER-COMMAND UCO2          "#EC NEEDED
    DEFAULT SCREEN 1002.
  SELECTION-SCREEN TAB (30) TAB3 USER-COMMAND UCO3          "#EC NEEDED
    DEFAULT SCREEN 1003.
SELECTION-SCREEN END OF BLOCK TABS1.

* Selection of ALV variant
* ------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK SIX WITH FRAME TITLE TEXT-041.
  PARAMETERS P_VARI LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK SIX.

***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

*  PRINTER PROPERTIES
DATA: GS_FP_OUTPUTPARAMS TYPE SFPOUTPUTPARAMS,  "Output parameters
      GV_W_CX_ROOT       TYPE REF TO CX_ROOT,
      GV_INTERFACE_TYPE  TYPE FPINTERFACETYPE,
      GV_MESG            TYPE STRING,
      GS_FP_DOCPARAMS    TYPE SFPDOCPARAMS.

DATA: BEGIN OF OUTPUT_HEADER_TABLE OCCURS 0,
        STATUS(4) TYPE C,                " print status
        EXPAND    TYPE C,                " FLAG: expand/contract
        CHECK     TYPE C,                " Checkbox
        XBLNR     LIKE BKPF-XBLNR,
        XGJAHR    LIKE BKPF-GJAHR,
        BUKRS     LIKE BKPF-BUKRS,
        TRANS(60) TYPE C,
      END OF OUTPUT_HEADER_TABLE.

DATA: BEGIN OF WA_ITEM,
        XGJAHR LIKE BKPF-GJAHR.
        INCLUDE STRUCTURE BOE_TR_LIST.
DATA   END OF WA_ITEM.

DATA : IT_ITAB LIKE TABLE OF WA_ITEM.

DATA:  WA_BOE_TR_LIST LIKE BOE_TR_LIST.

DATA:   OUTPUT_ITEM_TABLE LIKE WA_ITEM OCCURS 10 WITH HEADER LINE.

DATA: WA_HEADER_TABLE LIKE OUTPUT_HEADER_TABLE,
      WA_ITEM_TABLE   LIKE LINE OF IT_ITAB,
      WA_PRINT        LIKE BOE_TR_LIST.

DATA: BOE_INTERN LIKE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
      HELP_TABIX LIKE SY-TABIX,
      FORM       LIKE T001F-FORNR,
      SETTINGS   LIKE  ITCPO,
      RESULT     LIKE  ITCPP.

DATA ONCE TYPE C.
DATA:   FOUND         TYPE    I.
DATA:   GV_FOUND TYPE C .
CLEAR:  ONCE, FOUND.

TABLES : BOE_TR_SUMS,
         BOE_TR_LIST,
**    tcurx,
         BOE_GLOBAL_TURKEY.

*  DATA TYPE
DATA : GS_DATA   TYPE RFTR_BOE_REPRINT_ST1_PDF,                        "work area for item data
       GT_DATA   TYPE STANDARD TABLE OF RFTR_BOE_REPRINT_ST1_PDF,      "structure for item data
       GS_FOOTER TYPE RFTR_BOE_REPRINT_ST2_PDF,                      "Work area for output table
       GT_FOOTER TYPE STANDARD TABLE OF RFTR_BOE_REPRINT_ST2_PDF.    "Structure for output table

DATA BOE_GLOBAL TYPE BOE_GLOBAL_TURKEY.

DATA: WAERS    LIKE BKPF-WAERS,
      LF_WRBTR LIKE BOE_TR_LIST-WRBTR,
      CURR     LIKE TCURX-CURRDEC,
      BUDAT    LIKE BKPF-BUDAT,
      AMOUNT   TYPE AFLEDF34_DEC,               "AFLE Change
      DAYS     TYPE I,
      PRODUCT  TYPE AFLEDF34_DEC,               "AFLE Change "f,
      SUM      TYPE AFLEDF34_DEC,               "AFLE Change "f,
      TOTAL    TYPE AFLEDF34_DEC,               "AFLE Change "f,
      PRELIM   TYPE F,
      RESDAYS  TYPE I,
      AVERAGE  LIKE BKPF-BUDAT,
      IN_WORDS TYPE  SPELL.

*  FORM PROPERTIES

DATA: GV_FM_NAME TYPE RS38L_FNAM,  "function module
      GV_FPNAME  TYPE FPNAME.      "form name

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

************Start  of PDF conversion ,Date: 29/07/2008 ,C5112660
DATA: G_ACTIVE   TYPE C,
      G_GROUPID  TYPE ERP_EHP_SWF_GROUPID VALUE 'ALPDF',
      G_SUBGROUP TYPE ERP_EHP_SWF_SUBGROUPID VALUE 'ACC_FI_FORM'.
***********End  of PDF conversion ,Date: 29/07/2008 ,C5112660


************Start  of PDF conversion ,Date: 9/06/2006 ,C5112660

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE TEXT-053.
  PARAMETERS: PSCRIPT RADIOBUTTON GROUP GR1,
              PDF     RADIOBUTTON GROUP GR1,
              DUMMY   RADIOBUTTON GROUP GR1  DEFAULT 'X'.
  PARAMETERS:  S_SINGLE  TYPE FLAG DEFAULT ' ' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(18) text-060 FOR FIELD s_single.
*SELECTION-SCREEN END OF LINE.


************End  of PDF conversion ,Date: 9/06/2006 ,C5112660



* Begin of event: INITIALIZATION
* ------------------------------
INITIALIZATION.
************Start  of PDF conversion ,Date: 29/07/2008 ,C5112660
  CALL METHOD CL_ERP_SWITCH_CHECK=>GET_SOFT_SWITCH
    EXPORTING
      I_GROUP_ID    = G_GROUPID
      I_SUBGROUP_ID = G_SUBGROUP
    RECEIVING
      R_IS_ACTIVE   = G_ACTIVE.

*  loop at screen.                "2226143
*    if screen-name = '%B053078_BLOCK_1000' OR
*       screen-name = 'PSCRIPT' OR
*       screen-name = 'PDF'.
*      if g_active <> 'Y'.
*        screen-active = 0.
*        modify screen.
*      endif.
*    endif.
*  endloop.
************End  of PDF conversion ,Date: 29/07/2008 ,C5112660

******* SAP note 2719318
* Check Cloud System
* Checks if this is a S/4 HANA On-Premise system
  CALL METHOD CL_COS_UTILITIES=>IS_S4H_ON_PREMISE
    RECEIVING
      RV_IS_S4H_ON_PREMISE = GV_IS_S4H_PREMISE.
******* End of SAP note 2719318

* set default value of company code
* ---------------------------------
  REFRESH SD_BUKRS. CLEAR SD_BUKRS.
  GET PARAMETER ID 'BUK' FIELD SD_BUKRS-LOW.
  SD_BUKRS-SIGN = 'I'.
  SD_BUKRS-OPTION = 'EQ'.
  APPEND SD_BUKRS.

* set default value of portfolio range
* ------------------------------------
  IF S_PORT1[] IS INITIAL.
    S_PORT1-LOW = '?'.
    S_PORT1-SIGN = 'I'.
    S_PORT1-OPTION = 'EQ'.
    APPEND S_PORT1.
  ENDIF.

* set status and title
* --------------------
  SET PF-STATUS 'SELECT'.
* set tabstrip properties
* -----------------------
  TABS1-DYNNR = '1001'.
  TABS1-ACTIVETAB = 'UCO1'.           "first tabstrip screen active
  TAB1 = TEXT-002.                    "set names of tabstrip screens
  TAB2 = TEXT-011.
  TAB3 = TEXT-001.

* get date format setting of user
* -------------------------------
  SELECT SINGLE DATFM FROM USR01 INTO GF_DATFM
                           WHERE BNAME = SY-UNAME.

* inizialize ALV parameters
* -------------------------
  G_REPID = SY-REPID.
  PERFORM GS_LAYOUT_INIT USING GS_LAYOUT.
  PERFORM GT_EVENTS_INIT USING GT_EVENTS[].
  PERFORM GT_SP_GROUP_BUILD USING GT_SP_GROUP[].

* get ALV standard variant
* ------------------------
  G_SAVE = 'A'.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.
  GX_VARIANT = G_VARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = G_SAVE
    CHANGING
      CS_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 0.
    P_VARI = GX_VARIANT-VARIANT.
  ENDIF.

* set parameters for call transactions
* ------------------------------------
  MODE = 'N'.
  UPDATE = 'S'.
  GF_BLART = 'ZP'.



* Begin of event: AT SELECTION-SCREEN OUTPUT.
* -------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*==> Start of Indonesia PDC/BOE Note 2629428
  IF P_BUKRS IS INITIAL.
    GET PARAMETER ID 'BUK' FIELD P_BUKRS.
  ENDIF.

  IF P_BUKRS IS NOT INITIAL.
    CALL FUNCTION 'J_1BSA_COMPONENT_ACTIVE'
      EXPORTING
        BUKRS                = P_BUKRS
        COMPONENT            = '**'
      IMPORTING
        ACTIVE_COMPONENT     = GF_COUNTRY
      EXCEPTIONS
        COMPONENT_NOT_ACTIVE = 1.

    IF GF_COUNTRY = 'ID'
    OR GF_COUNTRY = 'IN'    "Note 2772124 India PDC
    OR GF_COUNTRY = 'TH'    "Note 2784273 India PDC
    OR GF_COUNTRY = 'PH'.   "Note 2668160 Philippines PDC
      LOOP AT SCREEN.
        IF SCREEN-NAME = 'X2'
        OR SCREEN-NAME = 'X4'
        OR SCREEN-NAME = 'X6'  .
          SCREEN-INPUT = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF GV_OLD_COUNTRY IS INITIAL.
      GV_OLD_COUNTRY = GF_COUNTRY.
    ENDIF.
  ENDIF.
*==> End of Indonesia PDC/BOE Note 2629428
* set title of screen 1002 in dependence of chosen radiobutton
* ------------------------------------------------------------
  IF TABS1-DYNNR = '1002'.
    PERFORM GET_TITLE_SCREEN_1002 USING CHAR60.

    IF P_WAERS NE '?' AND NOT ( S_WRBTR[] IS INITIAL ).
      BKPF-WAERS = P_WAERS.
      LOOP AT S_WRBTR.
        BSEG-WRBTR = S_WRBTR-LOW.
        S_WRBTR-LOW = BSEG-WRBTR.
        BSEG-WRBTR = S_WRBTR-HIGH.
        S_WRBTR-HIGH = BSEG-WRBTR.
        MODIFY S_WRBTR.
      ENDLOOP.
    ENDIF.
  ENDIF.



  LOOP AT SCREEN.
* Make fields of logical database invisible.
* ------------------------------------------
    IF SCREEN-NAME EQ '%_SD_KTOPL_%_APP_%-TEXT' OR
       SCREEN-NAME EQ '%_SD_KTOPL_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ 'SD_KTOPL-LOW' OR
       SCREEN-NAME EQ 'SD_KTOPL-HIGH' OR
       SCREEN-NAME EQ '%_SD_KTOPL_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_SAKNR_%_APP_%-TEXT' OR
       SCREEN-NAME EQ '%_SD_SAKNR_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ 'SD_SAKNR-LOW' OR
       SCREEN-NAME EQ 'SD_SAKNR-HIGH' OR
       SCREEN-NAME EQ '%_SD_SAKNR_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ 'SSCRTEXTS-MCID_TEXT' OR
       SCREEN-NAME EQ 'SD_INDEX-HOTKEY' OR
       SCREEN-NAME EQ 'SSCRTEXTS-TEXT_MCID' OR
       SCREEN-NAME EQ 'SSCRTEXTS-STRNG_TEXT' OR
       SCREEN-NAME EQ 'SD_INDEX-STRING' OR
       SCREEN-NAME EQ 'SSCRTEXTS-SEARCH_TXT' OR
       SCREEN-NAME EQ 'SSCRFIELDS-SEARCH_BTN' OR
       SCREEN-NAME EQ 'SD_BUKRS-HIGH' OR
       SCREEN-NAME EQ '%_SD_BUKRS_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_BUKRS_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ '%B000003_BLOCK_1000' OR
       SCREEN-NAME EQ '%B027052_BLOCK_1000' OR
       SCREEN-NAME EQ '%_SD_GJAHR_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'SD_GJAHR-LOW' OR
       SCREEN-NAME EQ 'SD_GJAHR-HIGH' OR
       SCREEN-NAME EQ '%_SD_GJAHR_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_GSB_S_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'SD_GSB_S-LOW' OR
       SCREEN-NAME EQ 'SD_GJAHR-HIGH' OR
       SCREEN-NAME EQ '%_SD_GSB_S_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_GSB_B_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ '%_SD_GSB_B_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'SD_GSB_S-LOW' OR
       SCREEN-NAME EQ 'SD_GSB_S-HIGH' OR
       SCREEN-NAME EQ '%_SD_GSB_B_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_STIDA_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'SD_GSB_B-LOW' OR
       SCREEN-NAME EQ 'SD_GSB_B-HIGH' OR
       SCREEN-NAME EQ '%_SD_GSB_B_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_STIDA_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'SD_STIDA' OR
       SCREEN-NAME EQ '%_SD_AUGDT_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'SD_AUGDT-LOW' OR
       SCREEN-NAME EQ 'SD_AUGDT-HIGH' OR
       SCREEN-NAME EQ '%_SD_AUGDT_%_APP_%-VALU_PUSH' OR
       SCREEN-NAME EQ '%_SD_AUGDT_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ '%_SUBSCREEN_%_SUB%_CONTAINER'.
      SCREEN-ACTIVE = '0'.
      MODIFY SCREEN.
    ENDIF.

* Make fields invisible/visible in dependency of the chosen transaction.
* ----------------------------------------------------------------------
    IF SCREEN-NAME EQ '%_P_PORT1_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'P_PORT1'.
      IF X3 EQ 'X' OR X7 EQ 'X' OR X8 EQ 'X' OR X9 EQ 'X'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_S_PORT1_%_APP_%-TEXT' OR
       SCREEN-NAME EQ '%_S_PORT1_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ 'S_PORT1-LOW' OR
       SCREEN-NAME EQ '%_S_PORT1_%_APP_%-TO_TEXT' OR
       SCREEN-NAME EQ 'S_PORT1-HIGH' OR
       SCREEN-NAME EQ '%_S_PORT1_%_APP_%-VALU_PUSH'.
      IF X3 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_P_PORT2_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'P_PORT2'.
      IF X9 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_P_PORT3_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'P_PORT3'.
      IF X7 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_P_PORT4_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'P_PORT4'.
      IF X8 EQ 'X' OR X9 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_P_UMSKZ_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'P_UMSKZ'.
      IF X7 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_P_BANKN_%_APP_%-TEXT' OR
       SCREEN-NAME EQ 'P_BANKN'.
      IF X3 EQ 'X' OR X5 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ '%_S_LIFNR_%_APP_%-TEXT' OR
       SCREEN-NAME EQ '%_S_LIFNR_%_APP_%-OPTI_PUSH' OR
       SCREEN-NAME EQ 'S_LIFNR-LOW' OR
       SCREEN-NAME EQ '%_S_LIFNR_%_APP_%-TO_TEXT' OR
       SCREEN-NAME EQ 'S_LIFNR-HIGH' OR
       SCREEN-NAME EQ '%_S_LIFNR_%_APP_%-VALU_PUSH'.
      IF X6 EQ 'X' OR X4 EQ 'X'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.


* hide printoutput buttons for cloud edition
*-----------------------------------
***** SAP note 2719318
    IF GV_IS_S4H_PREMISE = ABAP_FALSE.
      IF SCREEN-NAME EQ '%B053094_BLOCK_1000' OR SCREEN-NAME EQ '%_PSCRIPT_%_APP_%-TEXT' OR
        SCREEN-NAME EQ '%_PDF_%_APP_%-TEXT' OR SCREEN-NAME EQ '%_DUMMY_%_APP_%-TEXT' OR
        SCREEN-NAME EQ 'PSCRIPT' OR SCREEN-NAME EQ 'PDF' OR SCREEN-NAME EQ 'DUMMY' .
        SCREEN-ACTIVE = 0.
        SCREEN-REQUIRED = 0.
        SCREEN-INPUT = 0.
        SCREEN-OUTPUT = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF GV_IS_S4H_PREMISE = ABAP_TRUE.
      IF
        SCREEN-NAME EQ '%_DUMMY_%_APP_%-TEXT' OR
        SCREEN-NAME EQ  'DUMMY' .
        SCREEN-ACTIVE = 0.
        SCREEN-REQUIRED = 0.
        SCREEN-INPUT = 0.
        SCREEN-OUTPUT = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF. " SAP note 2719318


  ENDLOOP.


* Help requests for radiobuttons on first tabstrip page
* -----------------------------------------------------
AT SELECTION-SCREEN ON HELP-REQUEST FOR X1.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1A'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X2.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1B'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X3.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1C'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X4.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1D'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X5.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1E'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X6.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1F'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X7.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1G'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X8.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1H'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR X9.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      POPUP_TITLE = TEXT-043
      TEXT_OBJECT = 'RFIDTRRB1I'.


* Process on value request for ALV variant
* ----------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_ALV_VARIANT.

* Begin of event: AT SELECTION-SCREEN ON ...
* ------------------------------------------
AT SELECTION-SCREEN ON SD_BUKRS.
  P_BUKRS = SD_BUKRS-LOW.
  SELECT SINGLE * FROM T001 INTO GS_T001 WHERE BUKRS = P_BUKRS.
  IF SY-SUBRC NE 0.
    MESSAGE E102.
  ELSE.
* check for turkish company code
* ------------------------------
    CALL FUNCTION 'J_1BSA_COMPONENT_ACTIVE'
      EXPORTING
        BUKRS                = GS_T001-BUKRS
        COMPONENT            = '**'
      IMPORTING
        ACTIVE_COMPONENT     = GF_COUNTRY
      EXCEPTIONS
        COMPONENT_NOT_ACTIVE = 1.

    IF GF_COUNTRY NE 'TR' AND GF_COUNTRY NE 'TH' AND GF_COUNTRY NE 'GR'
*// Begin of Note 1391059 (Bill of Exchange functionality for UAE and SA)
*      and gf_country ne 'SA' and gf_country ne 'AE'. "note 1498051
      AND GF_COUNTRY NE 'SA' AND GF_COUNTRY NE 'AE'   "note 1498051
      AND GF_COUNTRY NE 'RO'                          "note 1498051
      AND GF_COUNTRY NE 'KW'                          "note 1875128
      AND GF_COUNTRY NE 'IN'                          "note 2772124
      AND GF_COUNTRY NE 'OM'                          "note 2027650
      AND GF_COUNTRY NE 'QA'                          "note 1683781
      AND GF_COUNTRY NE 'EG'                         "note 2254658
*// End of Note 1391059
      AND GF_COUNTRY NE 'PH'                         "Note 2668160 Philippines PDC
*==> Start of Indonesia PDC/BOE Note 2629428
      AND GF_COUNTRY NE 'ID'.
*==> End of Indonesia PDC/BOE Note 2629428
      MESSAGE E135 WITH GS_T001-BUKRS.
    ENDIF.

*==> Start of Indonesia PDC/BOE Note 2629428
*  Checking the company code changed of ID.
    GV_NEXT_SCREEN = 'X'.
    IF GV_OLD_COUNTRY <> GF_COUNTRY
   AND ( GV_OLD_COUNTRY = 'ID'
    OR   GV_OLD_COUNTRY = 'PH'    "Note 2668160 Philippines PDC
    OR   GF_COUNTRY = 'PH'        "Note 2668160 Philippines PDC
    OR   GV_OLD_COUNTRY = 'TH'    "Note 2784273 Thailand PDC
    OR   GF_COUNTRY     = 'TH'    "Note 2784273 Thailand PDC
    OR   GV_OLD_COUNTRY = 'IN'    "Note 2772124 India PDC
    OR   GF_COUNTRY     = 'IN'    "Note 2772124 India PDC
    OR   GF_COUNTRY = 'ID' ).
      GV_NEXT_SCREEN = 'Y'.
    ENDIF.
    GV_OLD_COUNTRY = GF_COUNTRY.
*==> End of Indonesia PDC/BOE Note 2629428

* store default value of company code
* -----------------------------------
    SET PARAMETER ID 'BUK' FIELD P_BUKRS.

* allocate chart of account to range table of logical data base:
* --------------------------------------------------------------
    REFRESH SD_KTOPL. CLEAR SD_KTOPL.
    SD_KTOPL-LOW = GS_T001-KTOPL.
    SD_KTOPL-SIGN = 'I'.
    SD_KTOPL-OPTION = 'EQ'.
    APPEND SD_KTOPL.

    SET PARAMETER ID 'KPL' FIELD GS_T001-KTOPL.
  ENDIF.
** check existence of tolerance group        "0001313596
** ----------------------------------
*  PERFORM check_t043x USING p_bukrs.        "0001313596

AT SELECTION-SCREEN ON P_BLDAT.
  IF TABS1-ACTIVETAB = 'UCO2'.
    IF P_BLDAT LT SY-DATUM.
      MESSAGE W104.
    ENDIF.
    IF P_BLDAT GT SY-DATUM.
      MESSAGE W105.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_BUDAT.
  IF TABS1-ACTIVETAB = 'UCO2'.
    IF P_BUDAT LT SY-DATUM.
      MESSAGE W104.
    ENDIF.
    IF P_BUDAT GT SY-DATUM.
      MESSAGE W105.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_PORT1.
  IF SY-UCOMM NE 'EXAN'.
    IF ( NOT ( X3 EQ 'X' OR X7 EQ 'X' OR X8 EQ 'X' OR X9 EQ 'X' ) ).
* check existence of portfolio
* ----------------------------
      IF P_PORT1 EQ '?'.
        MESSAGE E143.
      ENDIF.
      SELECT SINGLE WEKON FLAGCB HKONT VKONT FROM T045P
                                 INTO (GF_WEKON1, GF_FLAGCB1,
                                       GF_HKONT1, GF_VKONT1)
                                                WHERE  PORTFO = P_PORT1
                                                   AND  BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0.
        MESSAGE E106.
      ELSEIF GF_WEKON1 IS INITIAL OR GF_FLAGCB1 IS INITIAL OR
             GF_HKONT1 IS INITIAL OR GF_VKONT1 IS INITIAL.
        MESSAGE E171 WITH P_PORT1.
      ENDIF.

      IF X2 EQ 'X'.
* check existence of BoE-to-vendor account
* -------------------------------------------------------
        SELECT SINGLE * FROM T045W WHERE KTOPL = GS_T001-KTOPL
                                     AND BKONT = GF_VKONT1
                                     AND WVERW = 'F'.
        IF SY-SUBRC NE 0.
          MESSAGE E112.
        ENDIF.
        SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS
                                    AND SAKNR = GF_VKONT1.
        IF SY-SUBRC NE 0.
* check whether account is only open for internal postings
* ---------------------------------------------------
          IF SKB1-XINTB = ' '.
            MESSAGE E131 WITH GF_VKONT1.
          ELSE.
            MESSAGE E132 WITH GF_VKONT1.
          ENDIF.
        ENDIF.
        GF_BANKM = GF_VKONT1.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_PORT4.
  IF SY-UCOMM NE 'EXAN'.
    IF X8 EQ 'X' OR X9 EQ 'X'.
* check existence of portfolio
* ----------------------------
      IF P_PORT4 EQ '?'.
        MESSAGE E143.
      ENDIF.
      SELECT SINGLE WEKON FLAGCB HKONT VKONT FROM T045P
                                 INTO (GF_WEKON4, GF_FLAGCB4,
                                       GF_HKONT4, GF_VKONT4)
                                      WHERE  PORTFO = P_PORT4
                                         AND  BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0.
        MESSAGE E106.
      ELSEIF GF_WEKON4 IS INITIAL OR GF_FLAGCB4 IS INITIAL OR
             GF_HKONT4 IS INITIAL OR GF_VKONT4 IS INITIAL.
        MESSAGE E171 WITH P_PORT4.
      ENDIF.
      IF X8 EQ 'X'.
* check properties of rec. account at portfolio movements
* -------------------------------------------------------
        SELECT SINGLE * FROM T045W WHERE KTOPL = GS_T001-KTOPL
                                     AND BKONT = GF_HKONT4
                                     AND WVERW = 'D'.
        IF SY-SUBRC NE 0.
          MESSAGE E112.
        ENDIF.
        SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS
                                    AND SAKNR = GF_HKONT4.
        IF SY-SUBRC NE 0.
* check whether account is only open for internal postings
* ---------------------------------------------------
          IF SKB1-XINTB = ' '.
            MESSAGE E131 WITH GF_HKONT4.
          ELSE.
            MESSAGE E132 WITH GF_HKONT4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_PORT2.
  IF SY-UCOMM NE 'EXAN'.
    IF X9 EQ 'X'.
      IF P_PORT2 EQ '?'.
        MESSAGE E149.
      ENDIF.
* check existence of target portfolio; different to portfolio 4?
* --------------------------------------------------------------
      SELECT SINGLE WEKON FLAGCB FROM T045P
                                      INTO (GF_WEKON2, GF_FLAGCB2)
                                          WHERE  PORTFO = P_PORT2
                                             AND  BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0.
        MESSAGE E108.
      ELSEIF GF_WEKON2 IS INITIAL OR GF_FLAGCB2 IS INITIAL.
        MESSAGE E171 WITH GF_WEKON2.
      ENDIF.

      IF P_PORT4 EQ P_PORT2.
        MESSAGE E109.
      ENDIF.
      IF GF_FLAGCB4 NE GF_FLAGCB2.
        MESSAGE E133.
      ENDIF.
* check properties of BoE receivable account
* ------------------------------------------
      SELECT SINGLE * FROM SKA1 WHERE KTOPL = GS_T001-KTOPL
                                  AND SAKNR = GF_WEKON2.
      IF SY-SUBRC NE 0.
        MESSAGE E136 WITH GF_WEKON2.
      ELSEIF SKA1-XBILK NE 'X'.
        MESSAGE E128 WITH GF_WEKON2.
      ENDIF.

      SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS
                                  AND SAKNR = GF_WEKON2.
      IF SY-SUBRC NE 0.
        MESSAGE E132 WITH GF_WEKON2.
      ELSEIF SKB1-MITKZ NE 'D'.
        MESSAGE E138 WITH GF_WEKON2.
      ELSEIF SKB1-XINTB NE ' '.
        MESSAGE E131 WITH GF_WEKON2.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_PORT3.
  IF SY-UCOMM NE 'EXAN'.
    IF X7 EQ 'X'.
      IF P_PORT3 EQ '?'.
        MESSAGE E143.
      ENDIF.
* check existence of portfolio for bounced BoE
* --------------------------------------------
      SELECT SINGLE WEKON FLAGCB HKONT FROM T045P
                          INTO (GF_WEKON3, GF_FLAGCB3, GF_HKONT3)
                                         WHERE  PORTFO = P_PORT3
                                            AND  BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0.
        MESSAGE E106.
      ELSEIF GF_WEKON3 IS INITIAL OR GF_FLAGCB3 IS INITIAL OR
             GF_HKONT3 IS INITIAL.
        MESSAGE E171 WITH P_PORT3.
      ENDIF.
* check whether reconsi. account is only open for internal postings
* -----------------------------------------------------------------
      SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS
                                  AND SAKNR = GF_HKONT3.
      IF SY-SUBRC NE 0.
        MESSAGE E132 WITH GF_HKONT3.
      ENDIF.
      IF SKB1-XINTB NE ' '.
        MESSAGE E131 WITH GF_HKONT3.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON P_WAERS.
  IF SY-UCOMM NE 'EXAN'.
    IF P_WAERS EQ '?'.
      MESSAGE E145.
    ENDIF.
    SELECT SINGLE * FROM TCURC WHERE WAERS = P_WAERS.
    IF SY-SUBRC NE 0.
      MESSAGE E110.
    ENDIF.
    BKPF-WAERS = P_WAERS.
  ENDIF.

AT SELECTION-SCREEN ON S_LIFNR.
  IF SY-UCOMM NE 'EXAN'.
    IF X4 EQ 'X' OR X6 EQ 'X'.
* check existence of acc. receivables
* -----------------------------------
      DESCRIBE TABLE S_LIFNR LINES GF_LINES2.
      IF GF_LINES2 NE 0.
        LOOP AT S_LIFNR.
          IF NOT ( S_LIFNR-LOW IS INITIAL ).
            IF S_LIFNR-LOW EQ '?'.
              MESSAGE E148.
            ENDIF.
            SELECT SINGLE * FROM LFB1 WHERE LIFNR = S_LIFNR-LOW
                                        AND BUKRS = P_BUKRS.
            IF SY-SUBRC NE 0.
              MESSAGE E123.
            ENDIF.
          ENDIF.
          IF NOT ( S_LIFNR-HIGH IS INITIAL ).
            SELECT SINGLE * FROM LFB1 WHERE LIFNR = S_LIFNR-HIGH
                                        AND BUKRS = P_BUKRS.
            IF SY-SUBRC NE 0.
              MESSAGE E123.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON S_KUNNR.
* check existence of acc. payables
* --------------------------------
  DESCRIBE TABLE S_KUNNR LINES GF_LINES2.
  IF GF_LINES2 NE 0.
    LOOP AT S_KUNNR.
      IF NOT ( S_KUNNR-LOW IS INITIAL ).
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = S_KUNNR-LOW
                             AND BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0.
          MESSAGE E125 WITH S_KUNNR-LOW.
        ENDIF.
      ENDIF.
      IF NOT ( S_KUNNR-HIGH IS INITIAL ).
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = S_KUNNR-HIGH
                              AND BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0.
          MESSAGE E125 WITH S_KUNNR-HIGH.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
* Begin of Note 1324719
AT SELECTION-SCREEN ON P_BANKN.
  IF SY-UCOMM NE 'EXAN'.
* check existence and properties of BoE-to-bank account
* -----------------------------------------------------
    IF X3 EQ 'X' OR X5 EQ 'X'.
      IF P_BANKN NE SPACE.
        SELECT SINGLE * FROM T045W WHERE KTOPL = GS_T001-KTOPL
                                     AND BKONT = P_BANKN
                                     AND WVERW = 'I'.
        IF SY-SUBRC NE 0.
          MESSAGE E112.
        ENDIF.
        SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS
                                    AND SAKNR = P_BANKN.
        IF SY-SUBRC NE 0.
* check whether account is only open for internal postings
* ---------------------------------------------------
          IF SKB1-XINTB = ' '.
            MESSAGE E131 WITH P_BANKN.
          ELSE.
            MESSAGE E132 WITH P_BANKN.
          ENDIF.
        ENDIF.
        CLEAR: GF_COUNT, GS_T012K.
        "Begin of T012K Replacement, CM80672
*        SELECT * FROM t012k INTO gs_t012k
*                            WHERE bukrs = p_bukrs.
*          IF gs_t012k-wikon EQ p_bankn.
*            gf_count = gf_count + 1.
*            gf_banko = gs_t012k-hkont.
*          ENDIF.
*        ENDSELECT.
        TRY.
            DATA(LT_HBA_INSTANCE) = CL_FCLM_OBJECT_FACTORY=>GET_HBA_INSTANCES(
                                    IT_BUKRS = VALUE #( ( P_BUKRS ) ) ).
          CATCH CX_FCLM_BAM_HOUSE_BANK_ACCOUNT ##NO_HANDLER .
        ENDTRY.
        LOOP AT LT_HBA_INSTANCE ASSIGNING FIELD-SYMBOL(<FS_HBA_INSTANCE>).
          <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_VALIDITY_PERIOD(
            IMPORTING
              EV_VALID_FROM = DATA(LV_HBA_VALID_FROM)
              EV_VALID_TO   = DATA(LV_HBA_VALID_TO)
          ).
          IF LV_HBA_VALID_FROM > SY-DATUM OR LV_HBA_VALID_TO < SY-DATUM.
            CONTINUE.
          ENDIF.
          DATA(LT_ACLINK2) = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_BANK_ACCOUNT~GET_ACLINK2_FIELDS(
                               IT_REQ_FIELDS = VALUE #( ( 'WIKON' ) )
                             ).
          DATA(LS_ACLINK2) = LT_ACLINK2[ ACC_ID   = <FS_HBA_INSTANCE>-ACC_ID
                                         GUID     = <FS_HBA_INSTANCE>-GUID
                                         REVISION = IF_FCLM_BANK_ACCOUNT=>GC_ACTIVE_REVISION ].
          IF LS_ACLINK2-WIKON EQ P_BANKN.
            GF_COUNT = GF_COUNT + 1.
            GF_BANKO = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_GL_ACCOUNT( ).
          ENDIF.
        ENDLOOP.
        "End of T012K Replacement, CM80672
        IF GF_COUNT NE 1.
          MESSAGE E193.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON S_BANKK.
* check whether bank, which pays for BoE, exists
* ----------------------------------------------
  DESCRIBE TABLE S_BANKK LINES GF_LINES2.
  IF GF_LINES2 NE 0.
    LOOP AT S_BANKK.
      IF NOT ( S_BANKK-LOW IS INITIAL ).
        SELECT SINGLE * FROM BNKA WHERE BANKL = S_BANKK-LOW
                              AND BANKS = GS_T001-LAND1.
        IF SY-SUBRC NE 0.
          MESSAGE E124 WITH S_BANKK-LOW.
        ENDIF.
      ENDIF.
      IF NOT ( S_BANKK-HIGH IS INITIAL ).
        SELECT SINGLE * FROM BNKA WHERE BANKL = S_BANKK-HIGH
                             AND BANKS = GS_T001-LAND1.
        IF SY-SUBRC NE 0.
          MESSAGE E124 WITH S_BANKK-HIGH.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON S_WRBTR.
* transfer amount entries to local currency
* ----------------------------------
  IF SY-UCOMM NE 'EXAN'.
    REFRESH R_WRBTR.
    SELECT SINGLE * FROM TCURX WHERE CURRKEY = P_WAERS.
    IF SY-SUBRC NE 0.
      TCURX-CURRDEC = 2.
    ENDIF.
    LOOP AT S_WRBTR.
      R_WRBTR-SIGN   = S_WRBTR-SIGN.
      R_WRBTR-OPTION = S_WRBTR-OPTION.
      BSEG-WRBTR = S_WRBTR-LOW.
      S_WRBTR-LOW = BSEG-WRBTR.
      R_WRBTR-LOW    = S_WRBTR-LOW.
      BSEG-WRBTR = S_WRBTR-HIGH.
      S_WRBTR-HIGH = BSEG-WRBTR.
      R_WRBTR-HIGH   = S_WRBTR-HIGH.
      APPEND R_WRBTR.
      MODIFY S_WRBTR.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON P_UMSKZ.
  IF SY-UCOMM NE 'EXAN'.
    IF ( X7 EQ 'X' ) AND
       ( NOT P_UMSKZ IS INITIAL ).
* check whether SHKZG exists
* --------------------------
      SELECT SINGLE * FROM T074U WHERE KOART = 'D'
                                   AND UMSKZ = P_UMSKZ.
      IF SY-SUBRC NE 0.
        MESSAGE E113.
      ENDIF.
    ENDIF.
  ENDIF.

* Start of event: AT SELECTION-SCREEN
* -----------------------------------
AT SELECTION-SCREEN.

*==> Start of Indonesia PDC/BOE Note 2629428
  IF GF_COUNTRY = 'ID'
  OR GF_COUNTRY = 'TH'  " Note 2784273 Thailand PDC
  OR GF_COUNTRY = 'IN'  " Note 2772124 India PDC
  OR GF_COUNTRY = 'PH'. " Note 2668160 Philippines PDC

    IF X2 = 'X'
    OR X4 = 'X'
    OR X6 = 'X'.
      MESSAGE E801.
    ELSE.
      LOOP AT SCREEN.
        IF SCREEN-NAME = 'X2'
        OR SCREEN-NAME = 'X4'.
          SCREEN-ACTIVE = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*==> End of Indonesia PDC/BOE Note 2629428

* check existence of ALV variant
* ------------------------------
  PERFORM ALV_VARIANT_EXISTENCE.

  IF SY-UCOMM = 'EXAN'.
* start with selection screen again, delete lock entries
* ------------------------------------------------------
    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _SYNCHRON = 'X'.
    SUBMIT RFIDTRBOE1 VIA SELECTION-SCREEN.
  ENDIF.

  IF SY-UCOMM = 'E' OR SY-UCOMM = 'ENDE'.
* delete lock entries
* -------------------
    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _SYNCHRON = 'X'.
  ENDIF.

*==> Start of Indonesia PDC/BOE Note 2629428
*  IF sscrfields-ucomm = '   ' OR  sscrfields-ucomm = 'EXEX'.
  IF ( SSCRFIELDS-UCOMM = '   ' OR  SSCRFIELDS-UCOMM = 'EXEX' )
 AND GV_NEXT_SCREEN = 'X'.
*==> End of Indonesia PDC/BOE Note 2629428
* check whether all must entires are set
* --------------------------------------
    IF ( P_WAERS EQ '?' ) OR
       ( ( S_PORT1[] IS INITIAL ) AND ( X3 EQ 'X' ) ) OR
       ( ( P_PORT1 EQ '?' ) AND ( NOT ( X3 EQ 'X' OR
                                        X7 EQ 'X' OR
                                        X8 EQ 'X' OR
                                       X9 EQ 'X' ) ) ) OR
       ( ( P_PORT2 EQ '?' ) AND ( X9 EQ 'X' ) ) OR
       ( ( P_PORT3 EQ '?' ) AND ( X7 EQ 'X' ) ) OR
       ( ( P_PORT4 EQ '?' ) AND ( X8 EQ 'X' OR X9 EQ 'X' ) ).
      TABS1-DYNNR = '1002'.
      TABS1-ACTIVETAB = 'UCO2'.
    ELSE.
* execute only if second page on tabstrip is active
* -------------------------------------------------
      IF SSCRFIELDS-UCOMM = 'EXEX'.
        IF TABS1-DYNNR = '1002' AND TABS1-ACTIVETAB = 'UCO2'.
          SSCRFIELDS-UCOMM = '    '.
          SSCRFIELDS-UCOMM = 'ONLI'.
        ELSE.
          TABS1-DYNNR = '1002'.
          SSCRFIELDS-UCOMM = 'UCO2'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Begin of event: START-OF-SELECTION
* ----------------------------------
START-OF-SELECTION.

* BADI initialization - change dynamical selections
* -------------------------------------------------
  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE         "#EC CI_BADI_GETINST
    CHANGING
      INSTANCE = EXIT.

  TBUKRS[] = SD_BUKRS[].
  TBELNR[] = S_BELNR[].
  TBOENO[] = S_BOENO[].

  PERFORM FILL_GS_SELECT.

* Call methode.
  CALL METHOD EXIT->CHANGE_SELECTION
    EXPORTING
      I_SELECT  = GS_SELECT
      T_BUKRS   = TBUKRS
      T_BELNR   = TBELNR
      T_BOENO   = TBOENO
    CHANGING
      C_DYN_SEL = DYN_SEL.


  SD_STIDA = P_BUDAT.
  SD_APOPT = SPACE.
  B0SG-XNOPL = 'X'.

* allocate G/L account to range table of logical data base:
* --------------------------------------------------------
  REFRESH SD_SAKNR.
  IF X7 EQ 'X'.
    SD_SAKNR-LOW = GF_WEKON3.
    SD_SAKNR-SIGN = 'I'.
    SD_SAKNR-OPTION = 'EQ'.
    APPEND SD_SAKNR.
  ELSEIF ( X8 EQ 'X' OR X9 EQ 'X' ).
    SD_SAKNR-LOW = GF_WEKON4.
    SD_SAKNR-SIGN = 'I'.
    SD_SAKNR-OPTION = 'EQ'.
    APPEND SD_SAKNR.
  ELSEIF ( X3 EQ 'X' ).
    SELECT * FROM T045P WHERE  PORTFO IN S_PORT1
                          AND  BUKRS =  P_BUKRS.
      SD_SAKNR-LOW = T045P-WEKON.
      SD_SAKNR-SIGN = 'I'.
      SD_SAKNR-OPTION = 'EQ'.
      COLLECT SD_SAKNR.
    ENDSELECT.
  ELSE.
    SD_SAKNR-LOW = GF_WEKON1.
    SD_SAKNR-SIGN = 'I'.
    SD_SAKNR-OPTION = 'EQ'.
    APPEND SD_SAKNR.
  ENDIF.

* inizialize ALV parameters
* -------------------------
  PERFORM FIELDCAT_INIT USING GT_FIELDCAT[].

* save business place in Thailand
* --------------------------------
  IF GF_COUNTRY = 'TH'.
    GET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    GET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    GET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* refresh of table with BoE data shown by ALV:
* --------------------------------------------
  REFRESH GT_WTAB.
  CLEAR GF_XBLNR.

* refresh table with lok entries
* ------------------------------
  REFRESH: GT_LOCKDOC, GT_LOCK.

* determine lines of range table for collection number selection:
* ---------------------------------------------------------------
  DESCRIBE TABLE S_BORDR LINES GF_LINES.

* Logical data base selections
* ----------------------------
GET SKA1.

GET SKB1.
  CHECK SKB1-MITKZ = 'D'.

GET BSIS.
  CHECK BSIS-AUGBL = ' '.
  CHECK BSIS-AUGDT = '00000000'.
  CHECK BSIS-SHKZG = 'S'.
  CHECK BSIS-WAERS = P_WAERS.
  CHECK BSIS-WRBTR IN R_WRBTR.
  CHECK BSIS-BELNR IN S_BELNR.
  CHECK BSIS-XREF3 IN S_XREF3.
  CHECK BSIS-SHKZG = 'S'.
  CHECK BSIS-GSBER IN S_GSBER.
* check due date
* --------------
  CHECK BSIS-ZFBDT IN S_ZFBDT.

GET BKPF.
* check collection number selection:
* ----------------------------------
  IF X1 EQ 'X' OR X2 EQ 'X' OR X7 EQ 'X' OR X8 EQ 'X' OR X9 EQ 'X'.
    IF GF_LINES NE 0.
      GF_BORDR = BKPF-XBLNR.
      CHECK GF_BORDR IN S_BORDR.
    ENDIF.
    GF_XBLNR = BKPF-XBLNR.
  ENDIF.
* check: normal BoE or bounced BoE?
* -------------------------------
  IF X7 EQ 'X'.
    GS_NRIV-SUBOBJECT = P_BUKRS.
    GS_NRIV-OBJECT = 'TRBORDRO'.
    GS_NRIV-NRRANGENR = '06'.
    GS_NRIV-TOYEAR = BKPF-GJAHR.

    CALL FUNCTION 'CHECK_BORDRONUMBER'
      EXPORTING
        I_NRIV         = GS_NRIV
        I_BORDRO       = BKPF-XBLNR
      EXCEPTIONS
        NOT_INCLUDED   = 1
        INVALID_BORDRO = 2.

    IF SY-SUBRC NE 0.
      GS_NRIV-NRRANGENR = '07'.

      CALL FUNCTION 'CHECK_BORDRONUMBER'
        EXPORTING
          I_NRIV         = GS_NRIV
          I_BORDRO       = BKPF-XBLNR
        EXCEPTIONS
          NOT_INCLUDED   = 1
          INVALID_BORDRO = 2.
    ENDIF.
    CHECK SY-SUBRC EQ 0.
  ENDIF.

GET BSEG.
  CHECK BSEG-UMSKS = 'W'.
  CHECK BSEG-XREF1 IN S_XREF1.
  CHECK BSEG-XREF2 IN S_XREF2.
  CHECK BSED-BANK IN S_BANKK.
  CHECK BSED-BOENO IN S_BOENO.
  CHECK BSED-ACCOU IN S_ACCOU.

* check whether BoE portfolio or Bounced-BoE portfolio is necessary
* -----------------------------------------------------------------
  IF X7 EQ 'X'.
    CHECK BSED-PORTF = P_PORT3.
  ELSEIF ( X8 EQ 'X' OR X9 EQ 'X' ).
    CHECK BSED-PORTF = P_PORT4.
  ELSEIF ( X3 EQ 'X' ).
    CHECK BSED-PORTF IN S_PORT1.
  ELSE.
    CHECK BSED-PORTF = P_PORT1.
  ENDIF.

  CHECK BSEG-KUNNR IN S_KUNNR.
  CHECK BSEG-FILKD IN S_FILKD.                              "1324719
  CHECK BSEG-UMSKZ NE SPACE.
* check BoE usage
* ---------------
  IF X3 EQ 'X' OR X4 EQ 'X' OR X5 EQ 'X' OR X6 EQ 'X' OR X9 EQ 'X'.
    CHECK BSED-WVERD NE '00000000'.
    IF X3 EQ 'X' OR X5 EQ 'X'.
      CHECK BSEG-WVERW CO 'I'.
    ELSEIF X4 EQ 'X' OR X6 EQ 'X'.
      CHECK BSEG-WVERW CO 'F'.
    ELSE.
      CHECK BSEG-WVERW CO 'D'.
    ENDIF.
    CHECK BSEG-DISBN NE SPACE.
    CHECK BSEG-DISBJ NE SPACE.
  ENDIF.
* additional check of BoE usage
* -----------------------------
  IF X1 EQ 'X' OR X2 EQ 'X' OR X7 EQ 'X' OR X8 EQ 'X'.
    CHECK BSEG-WVERW EQ ' '.
    CHECK: BSEG-DISBN EQ SPACE, BSEG-DISBJ EQ SPACE,
           BSEG-DISBZ EQ '000'.
  ENDIF.
  CHECK BSEG-AUGBL = SPACE.
  CHECK BSEG-AUGDT = '00000000'.
* additional check of collection number selection
* -----------------------------------------------
  IF X3 EQ 'X' OR X4 EQ 'X' OR X5 EQ 'X' OR X6 EQ 'X'.
    SELECT SINGLE * FROM *BKPF WHERE BELNR = BSEG-DISBN
                                 AND GJAHR = BSEG-DISBJ
                                 AND BUKRS = P_BUKRS.
    IF GF_LINES NE 0.
      GF_BORDR = *BKPF-XBLNR.
      CHECK GF_BORDR IN S_BORDR.
*        check *bkpf-xblnr in S_XBLNR.
    ENDIF.
    GF_XBLNR = *BKPF-XBLNR.
  ENDIF.
* check of extended selections / tabstrip screen 1002
* ---------------------------------------------------
  IF Y2 EQ 'X'.
    CHECK BSED-XAKTZ = 'X'.
  ENDIF.
  IF Y3 EQ 'x'.
    CHECK BSED-XAKTZ = ' '.
  ENDIF.
  IF Z2 EQ 'X'.
    CHECK BSED-XSIWE = 'X'.
  ENDIF.
  IF Z3 EQ 'X'.
    CHECK BSED-XSIWE = ' '.
  ENDIF.
  IF X4 EQ 'X' OR X6 EQ 'X'.
    IF BSED-VENDR CO '0123456789'.
      CHECK BSED-VENDR+6(10) IN S_LIFNR.
    ELSE.
      CHECK BSED-VENDR IN S_LIFNR.
    ENDIF.
  ENDIF.
* check of sign: BoE 'on the way' between different portfolios
* ------------------------------------------------------------
  IF X9 EQ 'X'.
    CHECK BSED-WGBKZ EQ '1'.
    IF BSED-VENDR CO '0123456789'.
      MOVE P_PORT2 TO GF_VENDR.
      CHECK BSED-VENDR = GF_VENDR.
    ELSE.
      CHECK BSED-VENDR = P_PORT2.
    ENDIF.
  ELSE.
    CHECK BSED-WGBKZ NE '1'.
  ENDIF.
  IF X7 EQ 'X'.
    IF NOT P_UMSKZ IS INITIAL.
      CHECK BSEG-UMSKZ EQ P_UMSKZ.
    ENDIF.
  ENDIF.

* check of BoE-to-bank account
* ----------------------------
  IF X3 EQ 'X' OR X5 EQ 'X'.
    IF P_BANKN NE SPACE.
*Begin of note  2373695
*      SELECT SINGLE * FROM *bseg INTO gs_bseg_a
*                                 WHERE belnr = bseg-disbn
*                                   AND gjahr = bseg-disbj
*                                   AND bukrs = p_bukrs
*                                   AND shkzg = 'S'.

      SELECT SINGLE * FROM *BSEG INTO GS_BSEG_A
                                 WHERE BELNR = BSEG-DISBN
                                   AND GJAHR = BSEG-DISBJ
                                   AND BUKRS = P_BUKRS
                                   AND SHKZG = 'S'
                                   AND HKONT = P_BANKN.
      CHECK SY-SUBRC = 0.
*End of note 2373695.
    ENDIF.
  ENDIF.

* set lock entry for selected BoE document only once
* --------------------------------------------------
  CLEAR GF_LOCKFLAG.

  CALL FUNCTION 'ENQUEUE_EFBKPF'
    EXPORTING
      MODE_BKPF      = 'X'
      BELNR          = BSIS-BELNR
      BUKRS          = BSIS-BUKRS
      GJAHR          = BSIS-GJAHR
      _SCOPE         = '2'
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING BSIS TO GT_LOCKDOC.
    APPEND GT_LOCKDOC.
    GF_LOCKFLAG = 'X'.
  ELSEIF SY-SUBRC EQ 1.
    LOOP AT GT_LOCKDOC.
      IF GT_LOCKDOC-BELNR = BSIS-BELNR AND
         GT_LOCKDOC-BUKRS = BSIS-BUKRS AND
         GT_LOCKDOC-GJAHR = BSIS-GJAHR.
        GF_LOCKFLAG = 'X'.
      ENDIF.
    ENDLOOP.
  ELSEIF SY-SUBRC EQ 2.
    GF_LOCKFLAG = ' '.
  ENDIF.

* fill internal table gt_wtab.
* ---------------------------
  CLEAR GT_WTAB. " Note 1128407
  IF GF_LOCKFLAG NE 'X'.
    GT_WTAB-BOX = '2'.
  ENDIF.
  GT_WTAB-BELNR = BSIS-BELNR.
  GT_WTAB-BUZEI = BSIS-BUZEI.
  GT_WTAB-GJAHR = BSIS-GJAHR.
  GT_WTAB-WRBTR = BSIS-WRBTR.
  GT_WTAB-WAERS = BSIS-WAERS.
  GT_WTAB-ZFBDT = BSEG-ZFBDT.
  GT_WTAB-WELGF = BSED-WELGF.
  GT_WTAB-BOENO = BSED-BOENO.
  GT_WTAB-BANK = BSED-BANK.
  GT_WTAB-ACCOU = BSED-ACCOU.
  GT_WTAB-KUNNR = BSEG-KUNNR.
  GT_WTAB-FILKD = BSEG-FILKD.                               " 1324719
  GT_WTAB-BUKRS = P_BUKRS.
  GT_WTAB-SAKNR = BSIS-HKONT.
  GT_WTAB-GSBER = BSEG-GSBER.
  GT_WTAB-SHKZG = BSEG-SHKZG.
  GT_WTAB-UMSKZ = BSEG-UMSKZ.
  GT_WTAB-WVERW = BSEG-WVERW.
  GT_WTAB-DISBN = BSEG-DISBN.
  GT_WTAB-DISBJ = BSEG-DISBJ.
  GT_WTAB-HKONT = GS_BSEG_A-HKONT.
  GT_WTAB-DMBTR = BSEG-DMBTR.
  GT_WTAB-HWAER = BKPF-HWAER.
  GT_WTAB-DMBE2 = BSEG-DMBE2.
  GT_WTAB-HWAE2 = BKPF-HWAE2.
  GT_WTAB-DMBE3 = BSEG-DMBE3.
  GT_WTAB-HWAE3 = BKPF-HWAE3.
  GT_WTAB-BUDAT = P_BUDAT.
  GT_WTAB-WDATE = BSED-WDATE.
  GT_WTAB-SGTXT = BSEG-SGTXT.
  GT_WTAB-ZUONR = BSEG-ZUONR.
  GT_WTAB-WBANK = BSED-WBANK.
  GT_WTAB-WNAME = BSED-WNAME.
  GT_WTAB-WORT1 = BSED-WORT1.
  GT_WTAB-WBZOG = BSED-WBZOG.
  GT_WTAB-WORT2 = BSED-WORT2.
  GT_WTAB-WVERD = BSED-WVERD.
  GT_WTAB-AUGBL = BSEG-AUGBL.
  GT_WTAB-AUGDT = BSEG-AUGDT.
  GT_WTAB-XBLNR = GF_XBLNR.
  GT_WTAB-BKTXT = BKPF-BKTXT.
  GT_WTAB-XREF1 = BSEG-XREF1.
  GT_WTAB-XREF2 = BSEG-XREF2.
  GT_WTAB-XREF3 = BSEG-XREF3.
  GT_WTAB-XAKTZ = BSED-XAKTZ.
  GT_WTAB-XSIWE = BSED-XSIWE.
  GT_WTAB-DMBE2 = BSEG-DMBE2.
  GT_WTAB-HWAE2 = BKPF-HWAE2.
  GT_WTAB-DMBE3 = BSEG-DMBE3.
  GT_WTAB-HWAE3 = BKPF-HWAE3.
  IF NOT X9 EQ 'X'.
    GT_WTAB-VENDR = BSED-VENDR.
  ELSE.
    GT_WTAB-VENDR = ' '.
  ENDIF.
  IF X4 EQ 'X' OR X6 EQ 'X'.
    GT_WTAB-LIFNA = BSED-VENDR.
  ELSE.
    GT_WTAB-LIFNA = ' '.
  ENDIF.
  IF X3 EQ 'X'.
    GT_WTAB-PORTF1 = BSED-PORTF.
  ENDIF.
  GT_WTAB-KKBER = BSEG-KKBER.                               "1292178
  GF_ACCOU = GT_WTAB-ACCOU.                                 "NR1421096
  CALL FUNCTION 'READ_IBAN_FROM_DB'                        "NR1421096
    EXPORTING
      I_BANKS = GS_T001-LAND1
      I_BANKL = GT_WTAB-BANK
      I_BANKN = GF_ACCOU
      I_BKONT = ''
    IMPORTING
      E_IBAN  = GT_WTAB-IBAN.

  APPEND GT_WTAB.

  PERFORM SELECTED_ITEMS.

* Begin of event: END-OF-SELECTION
* --------------------------------
END-OF-SELECTION.

* sort BoE table and number entries
* ---------------------------------
  SORT GT_WTAB BY GJAHR BELNR BUZEI.

  ZAEHLER = 0.

  LOOP AT GT_WTAB.
    ZAEHLER = ZAEHLER + 1.
    GT_WTAB-COUN = ZAEHLER.
    MODIFY GT_WTAB.
  ENDLOOP.

  IF ZAEHLER > 0.
    MESSAGE S024(MSITEM) WITH ZAEHLER.
  ENDIF.


* create/open application log
* ----------------------
  PERFORM G_S_LOG_INIT USING G_S_LOG.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG = G_S_LOG
    EXCEPTIONS
      OTHERS  = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
               SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* call ALV tool
* -------------
*==> Start of CM 282235 1UX Adoption
*=>  Replace the ALV List with ALV Grid For UI Harmonization
*  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*    EXPORTING
*      i_buffer_active          = ' '
*      i_callback_program       = g_repid
*      it_fieldcat              = gt_fieldcat[]
*      it_special_groups        = gt_sp_group[]
*      is_layout                = gs_layout
*      i_callback_pf_status_set = g_status
*      i_save                   = g_save
*      is_variant               = g_variant
*      it_events                = gt_events[]
*    TABLES
*      t_outtab                 = gt_wtab.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_BUFFER_ACTIVE          = ' '
      I_CALLBACK_PROGRAM       = G_REPID
      IT_FIELDCAT              = GT_FIELDCAT[]
      IT_SPECIAL_GROUPS        = GT_SP_GROUP[]
      IS_LAYOUT                = GS_LAYOUT
      I_CALLBACK_PF_STATUS_SET = G_STATUS
      I_SAVE                   = G_SAVE
      IS_VARIANT               = G_VARIANT
      IT_EVENTS                = GT_EVENTS[]
    TABLES
      T_OUTTAB                 = GT_WTAB.
*==> End of CM 282235 1UX Adoption

* Includes
*---------
  INCLUDE RFIDTRBOE1_ALV_SUBROUTINES.
*  INCLUDE RFIDTRBOE1_BATCH_INPUT.
  INCLUDE ZSDSFIR0740_BATCH_INPUT.
  INCLUDE RFIDTRBOE1_GENERAL_SUBROUTINES.
  INCLUDE RFIDTRBOE1_PBO.
  INCLUDE RFIDTRBOE1_PAI.

  INCLUDE RFIDTRBOE1_UT IF FOUND.  "AFLE Change C5201065


AT SELECTION-SCREEN ON S_PORT1.
  IF SY-UCOMM NE 'EXAN'.
    IF X3 EQ 'X'.
* check existence of portfolio
* ----------------------------
      IF S_PORT1[] IS INITIAL.
        MESSAGE E143.
      ENDIF.
      SELECT * FROM T045P WHERE  PORTFO IN S_PORT1
                            AND  BUKRS =  P_BUKRS.
        IF T045P-WEKON IS INITIAL OR T045P-FLAGCB IS INITIAL OR
               T045P-HKONT IS INITIAL OR T045P-VKONT IS INITIAL.
          MESSAGE E171 WITH T045P-PORTFO.
        ENDIF.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        MESSAGE E106.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON S_FILKD.
* check existence of customer branch
* --------------------------------
  DESCRIBE TABLE S_FILKD LINES GF_LINES2.
  IF GF_LINES2 NE 0.
    LOOP AT S_FILKD.
      IF NOT ( S_FILKD-LOW IS INITIAL ).
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = S_FILKD-LOW
                             AND BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0.
          MESSAGE E125 WITH S_FILKD-LOW.
        ENDIF.
      ENDIF.
      IF NOT ( S_FILKD-HIGH IS INITIAL ).
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = S_FILKD-HIGH
                              AND BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0.
          MESSAGE E125 WITH S_FILKD-HIGH.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
* End of Note  1324719

  INCLUDE RFIDTRBOE1_QATAR.
