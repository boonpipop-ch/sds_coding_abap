*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0180
*  Creation Date      : 24.06.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : SDE011
*  Description        : This is program to approve/unlock sales order
*                       schedule lines with feature to assign advance
*                       payment amount.
*  Purpose            : To approve/unlock sales order schedule lines
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*  25.02.2025  420000395  Zulkiff B.   Add allowance condition in
*                                      advance payment step
*-----------------------------------------------------------------------
*  19.06.2025  420000631  Zulkiff B.  Deposit Amount incorrect
*-----------------------------------------------------------------------
REPORT ZSDSSDR0180.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  VBAK,
  VBAP,
  VBEP,
  BKPF.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT1.
         INCLUDE  TYPE  ZSDSSDS039.
TYPES:   CELLTAB   TYPE  LVC_T_STYL,
         LINECOLOR TYPE  CHAR4,
         DISABLE   TYPE  FLAG,
       END OF TS_RESULT1.
TYPES: TT_RESULT1  TYPE  STANDARD TABLE OF TS_RESULT1 WITH EMPTY KEY.

TYPES: BEGIN OF TS_COUNT,
         TOTAL TYPE  I,
         PEND  TYPE  I,
       END OF TS_COUNT.

TYPES: BEGIN OF TS_HEAD,
         MODE  TYPE  TEXT50,
         COUNT TYPE  TS_COUNT,
       END OF TS_HEAD.

TYPES: BEGIN OF TS_RESULT2.
         INCLUDE TYPE ZSDSSDS046.
TYPES: END OF TS_RESULT2.
TYPES: TT_RESULT2  TYPE  STANDARD TABLE OF TS_RESULT2.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE       TYPE  CHAR1     VALUE 'X',
  GC_TCODE_CO   TYPE  SY-TCODE  VALUE 'ZSDSSD011',
  GC_TCODE_MGR  TYPE  SY-TCODE  VALUE 'ZSDSSD012',

  GC_LED_GREEN  TYPE  ICON-ID VALUE '@5B@',
  GC_LED_RED    TYPE  ICON-ID VALUE '@5C@',
  GC_ICON_APPRV TYPE  ICON-ID VALUE '@0V@',
  GC_ICON_REJCT TYPE  ICON-ID VALUE '@0W@',
  GC_ICON_PENDG TYPE  ICON-ID VALUE '@BZ@',

  GC_STAT_APPRV TYPE  ZSDSDE_SD_APPV_STAT VALUE 'Y',
  GC_STAT_REJCT TYPE  ZSDSDE_SD_APPV_STAT VALUE 'N'.



*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT1     TYPE  TT_RESULT1                          ##NEEDED,
  GT_RESULT1_HD  TYPE  TT_RESULT1                          ##NEEDED,
  GT_RESULT1_DET TYPE  TT_RESULT1                          ##NEEDED,
  GT_RESULT1_ORI TYPE  TT_RESULT1                          ##NEEDED,
  GT_RESULT2     TYPE  TT_RESULT2                          ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_HEAD            TYPE TS_HEAD                             ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  GF_TITLE    TYPE  TEXT100                                  ##NEEDED,
  GF_APPROVER TYPE  CHAR1                                    ##NEEDED.

DATA : GF_CONTROL TYPE C LENGTH 1.
*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GT_AUART       TYPE  RANGE OF VBAK-AUART               ##NEEDED,
  GT_UMSKZ       TYPE  RANGE OF BSID_VIEW-UMSKZ          ##NEEDED,
  GT_ADVREC_DIFF TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.    ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS039'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 10,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 90.

CONSTANTS:
  GC_STRUCTURE_2     TYPE  TABNAME  VALUE 'ZSDSSDS046'.

CONSTANTS:
  GC_HEADER_HEIGHT_2 TYPE  I                VALUE 10,
  GC_ALV_HEIGHT_2    TYPE  I                VALUE 90.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF SY-BATCH IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = &1 ##NUMBER_OK
        TEXT       = &2.
  ELSE.
*   Show message step in background
    MESSAGE I000(38) WITH &2 SPACE SPACE SPACE.
  ENDIF.
END-OF-DEFINITION.

DEFINE MC_ASSIGN_ICON_STAT.
  CASE &1.
    WHEN GC_STAT_APPRV.
      &2 = GC_ICON_APPRV.
    WHEN GC_STAT_REJCT.
      &2 = GC_ICON_REJCT.
    WHEN OTHERS.
      &2 = GC_ICON_PENDG.
  ENDCASE.
END-OF-DEFINITION.

DEFINE MC_ASSIGN_TXT_STAT.
  CASE &1.
    WHEN GC_STAT_APPRV.
*     Text-x01: Approved
      &2 = TEXT-X01.
    WHEN GC_STAT_REJCT.
*     Text-x02: Rejected
      &2 = TEXT-X02.
    WHEN OTHERS.
      CLEAR &2.
  ENDCASE.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s00: Mode
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S00.
  PARAMETERS:
    RB_CONF TYPE  CHAR1 RADIOBUTTON GROUP G1 DEFAULT 'X'
                        USER-COMMAND DMY,
    RB_REP  TYPE  CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B0.
* Text-s01: Order Confirmation
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_VKORG  FOR  VBAK-VKORG MODIF ID CNF NO INTERVALS NO-EXTENSION
                             DEFAULT '1000',
    S_EDATU  FOR  VBEP-EDATU MODIF ID CNF,
    S_VBELN  FOR  VBAK-VBELN MODIF ID CNF,
    S_VTWEG  FOR  VBAK-VTWEG MODIF ID CNF,
    S_KUNNR  FOR  VBAK-KUNNR MODIF ID CNF,
    S_VKBUR  FOR  VBAK-VKBUR MODIF ID CNF,
    S_VKGRP  FOR  VBAK-VKGRP MODIF ID CNF,
    S_AUART  FOR  VBAK-AUART MODIF ID CNF,
    S_SPART  FOR  VBAK-SPART MODIF ID CNF,
    S_MATNR  FOR  VBAP-MATNR MODIF ID CNF.
* Text-s03: Schedule line status
  SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03 NO INTERVALS.
    PARAMETERS:
      RB_PEND TYPE  CHAR1 RADIOBUTTON GROUP G2 DEFAULT 'X'
                          MODIF ID CNF,
      RB_APPV TYPE  CHAR1 RADIOBUTTON GROUP G2 MODIF ID CNF,
      RB_ALL  TYPE  CHAR1 RADIOBUTTON GROUP G2 MODIF ID CNF.
  SELECTION-SCREEN END OF BLOCK B3.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS:
    P_VARI TYPE DISVARIANT-VARIANT MODIF ID CNF.
SELECTION-SCREEN END OF BLOCK B1.
* Text-s02: Advance Receive Data
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  SELECT-OPTIONS:
    S_VKORG2  FOR VBAK-VKORG MODIF ID REP NO INTERVALS NO-EXTENSION
                             DEFAULT '1000',
    S_EDATU2  FOR VBEP-EDATU MODIF ID REP,
    S_VBELN2  FOR VBAK-VBELN MODIF ID REP,
    S_KUNNR2  FOR VBAK-KUNNR MODIF ID REP,
    S_BUDAT   FOR BKPF-BUDAT MODIF ID REP.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 1(22) BUT_UPS USER-COMMAND UPS.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING SY-TCODE.
  PERFORM F_GET_CONSTANTS.
  PERFORM F_BOTTON_NAME.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F_LIST_VARIANT CHANGING P_VARI.

AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SET_SELECTION_SCREEN.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ELSEIF SSCRFIELDS-UCOMM EQ 'UPS'.
    PERFORM F_CAL_PRODUCT_CONTROL.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CASE GC_TRUE.
    WHEN RB_CONF.
*     Get Data
      PERFORM F_GET_CONF_DATA CHANGING GT_RESULT1
                                       GS_HEAD-COUNT.
      IF GT_RESULT1 IS INITIAL.
*       Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
    WHEN RB_REP.
*     Get Data
      PERFORM F_GET_ADVREC_REPORT CHANGING GT_RESULT2.
      IF GT_RESULT2 IS INITIAL.
*       Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
  ENDCASE.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  CASE GC_TRUE.
    WHEN RB_CONF.
*     Display Confirmation Result

      "Display header S/O
      GT_RESULT1_HD = GT_RESULT1.
      SORT GT_RESULT1_HD BY VBELN.
      DELETE ADJACENT DUPLICATES FROM GT_RESULT1_HD COMPARING VBELN.
      PERFORM F_DISPLAY_RESULT1_HD USING GT_RESULT1_HD.
*      PERFORM F_DISPLAY_RESULT1 USING GT_RESULT1.
    WHEN RB_REP.
*     Display Advance Receive Result
      PERFORM F_DISPLAY_RESULT2 USING GT_RESULT2.
  ENDCASE.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
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

* Assign Approver Mode
  CASE UF_TCODE.
    WHEN GC_TCODE_CO.
      GF_APPROVER = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_COOPERATOR.
    WHEN GC_TCODE_MGR.
      GF_APPROVER = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MANAGER.
    WHEN OTHERS.
      AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
        ID 'TCD' FIELD GC_TCODE_CO.
      IF SY-SUBRC <> 0.
*       Error You are not authorized to use transaction &
        MESSAGE S172(00) WITH GC_TCODE_CO.
        LEAVE PROGRAM.
      ENDIF.
      GF_APPROVER = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_COOPERATOR.
  ENDCASE.

  CASE GF_APPROVER.
    WHEN ZCL_SDSSD_ORDER_CONFIRMATION=>GC_COOPERATOR.
*     text-d01: Order Confirmation for Sales Co-operation
      GF_TITLE = TEXT-D01.
    WHEN ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MANAGER.
*     text-d02: Order Confirmation for Manager
      GF_TITLE = TEXT-D02.
  ENDCASE.

* Set Program title
  SY-TITLE = GF_TITLE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_AUART       TYPE  ZSDSDE_PARAM_NAME VALUE 'SO_TYPE',
    LC_UMSKZ       TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_SPGL_IND',
    LC_ADVREC_DIFF TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_DIFF'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC        TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C,
    LT_RANGE_PARAM TYPE RANGE OF ZSDSCAC001-PARAM.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_AUART.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Example Parameter
*     ------------------------------------
      WHEN LC_AUART.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_AUART.
    ENDCASE.

  ENDLOOP.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = 'ZCL_SDSSD_ORDER_CONFIRMATION'
    IMPORTING
      ET_GEN_C = LT_GENC.
  LOOP AT LT_GENC ASSIGNING <L_GENC>.

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Example Parameter
*     ------------------------------------
      WHEN LC_UMSKZ.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_UMSKZ.
    ENDCASE.

  ENDLOOP.

  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LC_ADVREC_DIFF ) TO LT_RANGE_PARAM.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID  = 'ZCL_SDSSD_ORDER_CONFIRMATION'
      IRT_PARAM = LT_RANGE_PARAM
    IMPORTING
      ET_GEN_C  = GT_ADVREC_DIFF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_list_variant
*----------------------------------------------------------------------*
*  List Variant
*----------------------------------------------------------------------*
FORM F_LIST_VARIANT  CHANGING CF_VARI TYPE DISVARIANT-VARIANT.

  DATA:
    LS_VARIANT  TYPE DISVARIANT.

  CLEAR LS_VARIANT.
  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      I_SAVE        = GC_SAVE_ALL
      IS_VARIANT    = LS_VARIANT
    IMPORTING
      ES_VARIANT    = LS_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  IF SY-SUBRC NE 0.
*   Message: No layout found.
    MESSAGE S030(ZSDSCA01).
    RETURN.
  ENDIF.

* Assign Output
  CF_VARI = LS_VARIANT-VARIANT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  CASE GC_TRUE.

*   Confirmation
    WHEN RB_CONF.
      IF S_VKORG[] IS INITIAL.
        SET CURSOR FIELD 'S_VKORG-LOW'.
*       Error: Fill out all required entry fields
        MESSAGE E055(00).
        RETURN.
      ENDIF.
      IF S_EDATU[] IS INITIAL.
        SET CURSOR FIELD 'S_EDATU-LOW'.
*       Error: Fill out all required entry fields
        MESSAGE E055(00).
        RETURN.
      ENDIF.
      AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
       ID 'VKORG' FIELD S_VKORG-LOW
       ID 'VTWEG' DUMMY
       ID 'SPART' DUMMY
       ID 'ACTVT' FIELD '02'.
      IF SY-SUBRC <> 0.
*       Error: No authorization for maintaining sales documents in &1 &2 &3
        MESSAGE E515(V1) WITH S_VKORG-LOW SPACE SPACE.
        RETURN.
      ENDIF.

*     Assign Mode
      CASE GC_TRUE.
        WHEN RB_PEND.
*         Text-b01: Pending for Approval Items
          GS_HEAD-MODE = TEXT-B01.
        WHEN RB_APPV.
*         Text-b02: Already approved Items
          GS_HEAD-MODE = TEXT-B02.
        WHEN RB_ALL.
*         Text-b03: All Items
          GS_HEAD-MODE = TEXT-B03.
      ENDCASE.

*   Report
    WHEN RB_REP.
      IF S_VKORG2[] IS INITIAL.
        SET CURSOR FIELD 'S_VKORG2-LOW'.
*       Error: Fill out all required entry fields
        MESSAGE E055(00).
        RETURN.
      ENDIF.
      IF S_EDATU2[] IS INITIAL.
        SET CURSOR FIELD 'S_EDATU2-LOW'.
*       Error: Fill out all required entry fields
        MESSAGE E055(00).
        RETURN.
      ENDIF.
      AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
       ID 'VKORG' FIELD S_VKORG2-LOW
       ID 'VTWEG' DUMMY
       ID 'SPART' DUMMY
       ID 'ACTVT' FIELD '03'.
      IF SY-SUBRC <> 0.
*       Error: No authorization for displaying sales documents in &1 &2 &3
        MESSAGE E514(V1) WITH S_VKORG2-LOW SPACE SPACE.
        RETURN.
      ENDIF.

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_SET_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Set Selection Screen
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN .

  LOOP AT SCREEN.

    IF SCREEN-GROUP1 EQ 'CNF'.
      IF RB_CONF EQ GC_TRUE.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'REP'.
      IF RB_REP EQ GC_TRUE.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONF_DATA
*----------------------------------------------------------------------*
*  Get Order Confirmation Data
*----------------------------------------------------------------------*
FORM F_GET_CONF_DATA  CHANGING CT_RESULT  TYPE  TT_RESULT1
                               CS_COUNT   TYPE  TS_COUNT.

  DATA:
    LT_CONF          TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA,
    LT_REMAIN_ADVREC TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_REMAIN_ADVREC,
    LT_AUART         TYPE  RANGE OF VBAK-AUART.

  DATA:
    LF_MODE         TYPE  CHAR1.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Assign Mode
  CASE GC_TRUE.
    WHEN RB_PEND.
      LF_MODE = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MODE_PENDING.
    WHEN RB_APPV.
      LF_MODE = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MODE_APPROVED.
    WHEN RB_ALL.
      LF_MODE = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MODE_ALL.
  ENDCASE.

* Get List of Valid Sales Order Type
  IF S_AUART IS NOT INITIAL.
    SELECT 'I' AS SIGN,
           'EQ' AS OPTION,
           A~AUART AS LOW,
           ' '  AS HIGH
      FROM TVAK AS A
     WHERE A~AUART IN @S_AUART
       AND A~AUART IN @GT_AUART
      INTO TABLE @LT_AUART.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ELSE.
    LT_AUART = GT_AUART.
  ENDIF.

* Get Order confirmation data
  ZCL_SDSSD_ORDER_CONFIRMATION=>GET_DATA(
    EXPORTING
      IT_EDATU         = S_EDATU[]
      IT_VKORG         = S_VKORG[]
      IT_VTWEG         = S_VTWEG[]
      IT_KUNNR         = S_KUNNR[]
      IT_VKBUR         = S_VKBUR[]
      IT_VKGRP         = S_VKGRP[]
      IT_AUART         = LT_AUART[]
      IT_VBELN         = S_VBELN[]
      IT_SPART         = S_SPART[]
      IT_MATNR         = S_MATNR[]
      IF_APPROVER      = GF_APPROVER
      IF_MODE          = LF_MODE
      IF_REMAIN_ADVREC = GC_TRUE
    IMPORTING
      ET_DATA          = LT_CONF
      ET_REMAIN_ADVREC = LT_REMAIN_ADVREC ).

* Assign Result
  PERFORM F_COLLECT_CONF_RESULT  USING  LT_CONF
                                        LT_REMAIN_ADVREC
                               CHANGING CT_RESULT.

* Count
  CS_COUNT-TOTAL = LINES( CT_RESULT ).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT1_HD
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT1_HD  USING  UT_RESULT TYPE TT_RESULT1.

  DATA:
    LT_FIELDCAT   TYPE LVC_T_FCAT,                          "#EC NEEDED
    LT_SORT       TYPE LVC_T_SORT,                          "#EC NEEDED
    LT_TOOL_EXC_1 TYPE UI_FUNCTIONS,                        "#EC NEEDED
    LT_EXCL       TYPE STANDARD TABLE OF SY-UCOMM,          "#EC NEEDED
    LS_VARIANT    TYPE DISVARIANT,                          "#EC NEEDED
    LS_LAYOUT     TYPE LVC_S_LAYO,                          "#EC NEEDED
    LS_FIELDCAT   TYPE LVC_S_FCAT,                          "#EC NEEDED
    LS_SORT       TYPE LVC_S_SORT ##NEEDED,
    LS_PRINT      TYPE LVC_S_PRNT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Enable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Not soft refresh but not all refresh
  CLEAR: GF_SOFT_REFRESH_1,
         GF_REFRESH_ALL_1.

* ALV Layout
  PERFORM F_ALV_LAYOUT1_HD CHANGING LS_LAYOUT
                                    LS_VARIANT
                                    LS_PRINT.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
*  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT1_HD CHANGING LT_FIELDCAT.
* Sort data
  PERFORM F_ALV_SORT_RESULT1_HD CHANGING LT_SORT.
* Call ALV Screen
*  CALL SCREEN 9000.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_BUFFER_ACTIVE          = 'X'
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS_HD'
      I_CALLBACK_USER_COMMAND  = 'F_USER_COMMAND_1_HD '
      IS_LAYOUT_LVC            = LS_LAYOUT
      IT_FIELDCAT_LVC          = LT_FIELDCAT
*     I_SAVE                   = G_SAVE
      IS_VARIANT               = LS_VARIANT
*     IT_EVENTS                = GT_EVENTS[]
    TABLES
      T_OUTTAB                 = UT_RESULT
    EXCEPTIONS ##FM_SUBRC_OK
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS_HD USING R_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'HEADER' EXCLUDING GT_EXCL ##STAT_UNDEF.
  SET TITLEBAR  '9000' WITH GF_TITLE.
ENDFORM.                    "PF_STATUS_SET
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT1
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT1  USING  UT_RESULT TYPE TT_RESULT1.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Enable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Not soft refresh but not all refresh
  CLEAR: GF_SOFT_REFRESH_1,
         GF_REFRESH_ALL_1.

* ALV Layout
  PERFORM F_ALV_LAYOUT1 CHANGING GS_LAYOUT_1
                                 GS_VARIANT_1
                                 GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT1 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT1 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT1_HD
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT1_HD CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                               CS_VARIANT TYPE  DISVARIANT
                               CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'C'.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.
*  CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.
*  CS_LAYOUT-BOX_FNAME  = 'SELCT'.
*  CS_LAYOUT-CTAB_FNAME = 'COLTAB'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*-----------
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
  CS_LAYOUT-SEL_MODE   = 'C'.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.
*  CS_LAYOUT-BOX_FNAME  = 'SELCT'.
*  CS_LAYOUT-CTAB_FNAME = 'COLTAB'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT1_HD
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT1_HD CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT   TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SELCT'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-EDIT      = GC_TRUE.
        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
*       Text-c01 : Select
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'VBELN'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
      WHEN 'KUNNR'.
      WHEN 'KUNNR_NAME'.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'VTWEG'.
      WHEN 'VTWEG_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'AUART'.
      WHEN 'AUART_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'VKBUR'.
      WHEN 'VKBUR_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'VKGRP'.
      WHEN 'VKGRP_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'SPART'.
      WHEN 'SPART_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT1
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT1 CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT   TYPE  TEXT50.
*    LF_COLOR1 TYPE  LVC_S_FCAT-EMPHASIZE VALUE 'C700',
*    LF_COLOR2 TYPE  LVC_S_FCAT-EMPHASIZE VALUE 'C410'.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SELCT'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-EDIT      = GC_TRUE.
        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
*       Text-c01 : Select
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EDATU'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'VBELN'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
      WHEN 'POSNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'ETENR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'UEPOS'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'LIFSP'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'BMENG'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'VRKME'.
      WHEN 'AMOUNT0'.
*       Text-c05 : Total amount
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'AMOUNT'.
*       Text-c06 : Total Inc. VAT.
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WAERS'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'KUNNR'.
      WHEN 'KUNNR_NAME'.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'VTWEG'.
      WHEN 'VTWEG_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'AUART'.
      WHEN 'AUART_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'VKBUR'.
      WHEN 'VKBUR_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'VKGRP'.
      WHEN 'VKGRP_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'SPART'.
      WHEN 'SPART_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'ZTERM'.
      WHEN 'ADVGP'.
      WHEN 'MATNR'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'ARKTX'.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'ICON_CO_APPV'.
        <L_FIELDCAT>-ICON  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 8.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR1.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
*       Text-c03 : CO-Oper. Appv.
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CO_APPV_STAT_TXT'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR1.
        <L_FIELDCAT>-JUST      = 'C'.
*       Text-c03 : CO-Oper. Appv.
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CO_APPV_STAT'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR1.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
*       Text-c03 : CO-Oper. Appv.
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CO_APPV_BY'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR1.
      WHEN 'CO_APPV_DATE'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR1.
      WHEN 'CO_APPV_TIME'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR1.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'ICON_MGR_APPV'.
        <L_FIELDCAT>-ICON  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 8.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR2.
*       Text-c04 : Manager Appv.
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MGR_APPV_STAT_TXT'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR2.
        <L_FIELDCAT>-JUST      = 'C'.
*       Text-c04 : Manager Appv.
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MGR_APPV_STAT'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR2.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
*       Text-c04 : Manager Appv.
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MGR_APPV_BY'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR2.
      WHEN 'MGR_APPV_DATE'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR2.
      WHEN 'MGR_APPV_TIME'.
*        <L_FIELDCAT>-EMPHASIZE = LF_COLOR2.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'BT_ADVREC'.
        <L_FIELDCAT>-ICON  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 8.
*       Text-c02 : AdvRec. Status
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 30  ##NUMBER_OK.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT1_HD
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT1_HD  CHANGING PT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBELN'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

* Sort by Sales Order number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT1
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT1  CHANGING PT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'EDATU',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBELN',
    LC_SORT3 TYPE  LVC_S_SORT-FIELDNAME VALUE 'POSNR',
    LC_SORT4 TYPE  LVC_S_SORT-FIELDNAME VALUE 'ETENR'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

* Sort by Delivery Date
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

* Sort by Sales Order number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

* Sort by Sales Order Item number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

* Sort by Sales Order Schedule line number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT4.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CASE GC_TRUE.
    WHEN RB_CONF.
      PERFORM F_TOP_OF_PAGE_CONF USING UREF_DYNDOC_ID.
    WHEN RB_REP.
      PERFORM F_TOP_OF_PAGE_REP USING UREF_DYNDOC_ID.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  CASE GC_TRUE.
    WHEN RB_CONF.
      PERFORM F_PRINT_TOP_OF_PAGE_CONF.
    WHEN RB_REP.
      PERFORM F_PRINT_TOP_OF_PAGE_REP.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UREF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  DATA:
    LS_BUTTON TYPE STB_BUTTON.


  CASE GC_TRUE.
*   ------------------
*   Order Confirmation
*   ------------------
    WHEN RB_CONF.
*     Handle Toolbar as needed
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

*     Add Select All Button
      CLEAR LS_BUTTON.
      LS_BUTTON-FUNCTION = 'SALL'.
      LS_BUTTON-ICON     = '@4B@'.
*     Text-a02: Select All
      LS_BUTTON-QUICKINFO = TEXT-A02.
      INSERT LS_BUTTON INTO UREF_OBJECT->MT_TOOLBAR
                       INDEX 3.

*     Add De Select All Button
      CLEAR LS_BUTTON.
      LS_BUTTON-FUNCTION = 'DSALL'.
      LS_BUTTON-ICON     = '@4D@'.
*     Text-a03: De Select All
      LS_BUTTON-QUICKINFO = TEXT-A03.
      INSERT LS_BUTTON INTO UREF_OBJECT->MT_TOOLBAR
                       INDEX 4.

*     Add Refresh Button
      CLEAR LS_BUTTON.
      LS_BUTTON-FUNCTION = '&REF'.
      LS_BUTTON-ICON     = '@42@'.
*     Text-a01: Refresh
      LS_BUTTON-QUICKINFO = TEXT-A01.
      INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

*     Add Approve Button
      CLEAR LS_BUTTON.
      LS_BUTTON-FUNCTION = 'APPV'.
      LS_BUTTON-ICON     = '@8X@'.
*     Text-a04: Approve
      LS_BUTTON-QUICKINFO = TEXT-A04.
      LS_BUTTON-TEXT      = TEXT-A04.
      INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

      IF GF_CONTROL EQ ABAP_TRUE." AND
        "SY-TCODE   EQ GC_TCODE_MGR.
        CLEAR LS_BUTTON.
        LS_BUTTON-FUNCTION = 'CTRL'.
        LS_BUTTON-ICON     = 'ICON_FOREIGN_KEY'.
*     Text-a04: Approve
        LS_BUTTON-QUICKINFO = TEXT-A07.
        LS_BUTTON-TEXT      = TEXT-A07.
        INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.
      ENDIF.

      IF RB_PEND IS INITIAL.
*       Add Undo Button
        CLEAR LS_BUTTON.
        LS_BUTTON-FUNCTION = 'UNDO'.
        LS_BUTTON-ICON     = '@2W@'.
*       Text-a06: Cancel Approval
        LS_BUTTON-QUICKINFO = TEXT-A06.
        LS_BUTTON-TEXT      = TEXT-A06.
        INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.
      ENDIF.

      IF GF_APPROVER EQ ZCL_SDSSD_ORDER_CONFIRMATION=>GC_COOPERATOR.

*       Add Assign AdvRec Button
        CLEAR LS_BUTTON.
        LS_BUTTON-FUNCTION = 'ADVR'.
        LS_BUTTON-ICON     = '@93@'.
*       Text-a05: Assign ADV.Receive
        LS_BUTTON-QUICKINFO = TEXT-A05.
        LS_BUTTON-TEXT      = TEXT-A05.
        INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

      ENDIF.

*   ------------------
*   Advance Receive Report
*   ------------------
    WHEN RB_REP.

  ENDCASE.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1_HD USING R_UCOMM     TYPE SY-UCOMM  ##CALLED
                               RS_SELFIELD TYPE SLIS_SELFIELD.

  CLEAR: GT_RESULT1_DET.

  CASE R_UCOMM.
    WHEN '&IC1' .
      READ TABLE GT_RESULT1_HD ASSIGNING FIELD-SYMBOL(<LFS_RESULT_HD>) INDEX RS_SELFIELD-TABINDEX .
      IF SY-SUBRC = 0.
        CASE RS_SELFIELD-FIELDNAME.
          WHEN 'VBELN'.
            GT_RESULT1_ORI = GT_RESULT1.

            DELETE  GT_RESULT1 WHERE VBELN <> <LFS_RESULT_HD>-VBELN.
            IF <LFS_RESULT_HD>-SELCT = GC_TRUE.
              PERFORM F_GET_CONTROL_FLAG.
              LOOP AT GT_RESULT1 ASSIGNING FIELD-SYMBOL(<LFS_RESULT1>).
                <LFS_RESULT1>-SELCT = GC_TRUE.
              ENDLOOP.
            ENDIF.

            PERFORM F_CALL_CHECK_CONTROL USING <LFS_RESULT_HD>-VBELN.
            PERFORM F_DISPLAY_RESULT1 USING GT_RESULT1.


            LOOP AT GT_RESULT1 ASSIGNING <LFS_RESULT1>.
              READ TABLE GT_RESULT1_ORI ASSIGNING FIELD-SYMBOL(<LFS_RESULT1_ORI>) WITH KEY VBELN = <LFS_RESULT1>-VBELN
                                                                                           POSNR = <LFS_RESULT1>-POSNR
                                                                                           ETENR = <LFS_RESULT1>-ETENR.
              IF SY-SUBRC = 0.
                MOVE-CORRESPONDING <LFS_RESULT1> TO <LFS_RESULT1_ORI>.
              ENDIF.
            ENDLOOP.
            GT_RESULT1 = GT_RESULT1_ORI.
        ENDCASE.
      ENDIF.
    WHEN '&REF'.
*         Get data
      PERFORM F_GET_CONF_DATA CHANGING GT_RESULT1
                                       GS_HEAD-COUNT.
      GT_RESULT1_HD = GT_RESULT1.
      SORT GT_RESULT1_HD BY VBELN.
      DELETE ADJACENT DUPLICATES FROM GT_RESULT1_HD COMPARING VBELN.

      DATA : E_GRID TYPE REF TO CL_GUI_ALV_GRID.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          E_GRID = E_GRID.

      CALL METHOD E_GRID->CHECK_CHANGED_DATA.

      "update_alv_tab

      CALL METHOD E_GRID->REFRESH_TABLE_DISPLAY.
*         Force PBO Process
      SUPPRESS DIALOG.
  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  DATA:
    LS_STABLE TYPE LVC_S_STBL.


  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CASE GC_TRUE.
    WHEN RB_CONF.
      CASE UF_UCOMM.
*       ----------------
*       Refresh Data
*       ----------------
        WHEN '&REF'.
*         Get data
          PERFORM F_GET_CONF_DATA CHANGING GT_RESULT1
                                           GS_HEAD-COUNT.
*         Force PBO Process
          SUPPRESS DIALOG.

*       ----------------
*       Select All
*       ----------------
        WHEN 'SALL'.
          MODIFY GT_RESULT1 FROM VALUE #( SELCT = GC_TRUE )
                            TRANSPORTING SELCT
                            WHERE SELCT IS INITIAL
                              AND DISABLE IS INITIAL.
*         Refresh Display
          CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE      = LS_STABLE
              I_SOFT_REFRESH = 'X'.

*       ----------------
*       De-Select All
*       ----------------
        WHEN 'DSALL'.
          MODIFY GT_RESULT1 FROM VALUE #( SELCT = SPACE )
                            TRANSPORTING SELCT
                            WHERE SELCT IS NOT INITIAL.
*         Refresh Display
          CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE      = LS_STABLE
              I_SOFT_REFRESH = 'X'.

*       ----------------
*       Approve
*       ----------------
        WHEN 'APPV'.
          PERFORM F_APPROVAL_SCHEDLINE  USING  ZCL_SDSSD_ORDER_CONFIRMATION=>GC_STAT_APPROVE
                                      CHANGING GT_RESULT1.
*         Refresh Display
          CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE      = LS_STABLE
              I_SOFT_REFRESH = 'X'.
*       ----------------
*       Set Qry Confirmation
*       ----------------
        WHEN 'CTRL'.
          PERFORM F_SET_QTY_CONFIRMATION.
*       ----------------
*       Cancel Approval
*       ----------------
        WHEN 'UNDO'.
          PERFORM F_CANCEL_APPROVAL CHANGING GT_RESULT1.
*         Refresh Display
          CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE      = LS_STABLE
              I_SOFT_REFRESH = 'X'.

*       ----------------
*       Assign Advance Receive
*       ----------------
        WHEN 'ADVR'.
          PERFORM F_ASSIGN_ADVREC_MULTI CHANGING GT_RESULT1.
*         Refresh Display
          CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE      = LS_STABLE
              I_SOFT_REFRESH = 'X'.

      ENDCASE.

    WHEN RB_REP.

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_TOP_OF_PAGE_CONF
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_CONF  USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT.

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
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
* Text-h01 : List Mode:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_HEAD-MODE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Total Count Entries:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_HEAD-COUNT-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
*      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_1
*----------------------------------------------------------------------*
*  ALV Hotspot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                             US_COLUMN_ID TYPE LVC_S_COL.

  CASE GC_TRUE.
    WHEN RB_CONF.
      PERFORM F_HOTSPOT_CLICK_CONF  USING US_ROW_ID
                                          US_COLUMN_ID.

    WHEN RB_REP.
      PERFORM F_HOTSPOT_CLICK_REP  USING US_ROW_ID
                                         US_COLUMN_ID.

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PRINT_TOP_OF_PAGE_CONF
*----------------------------------------------------------------------*
*  Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_CONF .

  DATA:
    LF_COL01 TYPE  I VALUE 25,
    LF_COL02 TYPE  I VALUE 35,
    LF_TEXT  TYPE  TEXT50.


* Text-h01 : List Mode:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    GS_HEAD-MODE NO-GAP.

* Text-h02 : Total Count Entries:
  WRITE GS_HEAD-COUNT-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_CONF
*----------------------------------------------------------------------*
*  Hot Spot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_CONF  USING  US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                                  US_COLUMN_ID TYPE LVC_S_COL.

* Read Line
  READ TABLE GT_RESULT1 ASSIGNING FIELD-SYMBOL(<L_RESULT1>)
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'VBELN'.
      PERFORM F_DISPLAY_SO USING  <L_RESULT1>-VBELN.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_SO
*----------------------------------------------------------------------*
*  Display Sales Order
*----------------------------------------------------------------------*
FORM F_DISPLAY_SO  USING UF_VBELN TYPE  VBAK-VBELN.

  IF UF_VBELN IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'AUN' FIELD UF_VBELN.
  CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.         "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_APPROVAL_SCHEDLINE
*----------------------------------------------------------------------*
*  Approval Schedule line
*----------------------------------------------------------------------*
FORM F_APPROVAL_SCHEDLINE  USING  UF_STAT   TYPE ZSDSDE_SD_APPV_STAT
                         CHANGING CT_RESULT TYPE TT_RESULT1.

  DATA:
    LT_SCHEDLINE   TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE,
    LT_APPV_RESULT TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_APPV_RESULT.

  DATA:
    LS_RETURN  TYPE  BAPIRET1.

  DATA: LS_ZSDSSDT009 TYPE ZSDSSDT009.

  DATA: LV_CHECK TYPE C.

* Clear Message Column
  MODIFY CT_RESULT FROM VALUE #( MSGTX = SPACE )
                   TRANSPORTING MSGTX
                   WHERE MSGTX IS NOT INITIAL.

* Collect Selected Schedline
  PERFORM F_COLLECT_SELECTION  USING  CT_RESULT
                             CHANGING LT_SCHEDLINE.
  IF LT_SCHEDLINE IS INITIAL.
*   Error: Please select item(s) to be processed.
    MESSAGE S004(ZSDSSD01).
    RETURN.
  ENDIF.

  CONSTANTS : BEGIN OF LC_CON,
                I_REPID     TYPE CHAR20 VALUE 'ZSDSSDR0180',
                I_PARAM     TYPE CHAR20 VALUE 'ORDER_TYPE',
                I_PARAM_EXT TYPE CHAR20 VALUE 'NO_APP',
              END OF LC_CON.
  DATA : LR_AUART TYPE RANGE OF VBAK-AUART.

  CONSTANTS : BEGIN OF LC_CON1,
                I_REPID     TYPE CHAR20 VALUE 'ZSDSSDR0180',
                I_PARAM     TYPE CHAR20 VALUE 'TERM_PAYMENT',
                I_PARAM_EXT TYPE CHAR20 VALUE 'NO_APP',
              END OF LC_CON1.
  DATA : LR_ZTERM TYPE RANGE OF VBKD-ZTERM.

  CONSTANTS : BEGIN OF LC_CON2,
                I_REPID     TYPE CHAR20 VALUE 'ZSDSSDR0180',
                I_PARAM     TYPE CHAR20 VALUE 'NAME_CHECK',
                I_PARAM_EXT TYPE CHAR20 VALUE 'NO_APP',
              END OF LC_CON2.
  DATA : LR_NAME1 TYPE RANGE OF KNA1-NAME1.

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID     = LC_CON-I_REPID
                                                I_PARAM     = LC_CON-I_PARAM
                                                I_PARAM_EXT = LC_CON-I_PARAM_EXT
                                       CHANGING CR_RETURN   = LR_AUART
                                     ).

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID     = LC_CON1-I_REPID
                                                I_PARAM     = LC_CON1-I_PARAM
                                                I_PARAM_EXT = LC_CON1-I_PARAM_EXT
                                       CHANGING CR_RETURN   = LR_ZTERM
                                     ).

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID     = LC_CON2-I_REPID
                                                I_PARAM     = LC_CON2-I_PARAM
                                                I_PARAM_EXT = LC_CON2-I_PARAM_EXT
                                       CHANGING CR_RETURN   = LR_NAME1
                                     ).

  IF LR_AUART IS INITIAL.
    LR_AUART =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = 'NODATA' ) ).
  ENDIF.

  IF LR_ZTERM IS INITIAL.
    LR_AUART =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = 'NODATA' ) ).
  ENDIF.

  SELECT SINGLE KUNNR_NAME,
                KUNNR
      FROM @CT_RESULT AS A
      WHERE SELCT EQ @GC_TRUE
        AND AUART IN @LR_AUART[]
        AND ZTERM IN @LR_ZTERM[]
      INTO @DATA(LV_KUNNR).
  IF SY-SUBRC EQ 0.
* Start Approval FOR Items
    CLEAR : LV_CHECK.
    IF LV_KUNNR-KUNNR EQ 'OT01'.
      LOOP AT LR_NAME1 INTO DATA(LS_NAME_CHECK).
        FIND LS_NAME_CHECK-LOW IN LV_KUNNR-KUNNR_NAME.
        IF SY-SUBRC EQ 0.
          LV_CHECK = ABAP_TRUE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF LV_CHECK EQ ABAP_TRUE.
      MESSAGE S000(ZSDSSD01) WITH TEXT-E01 DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      ZCL_SDSSD_ORDER_CONFIRMATION=>PROCESS_APPROVAL(
        EXPORTING
          IT_SCHEDLINE = LT_SCHEDLINE
          IF_APPROVER  = GF_APPROVER
          IF_APPV_STAT = UF_STAT
        IMPORTING
          ES_RETURN    = LS_RETURN
          ET_RESULT    = LT_APPV_RESULT ).
      IF LS_RETURN IS NOT INITIAL.
*   Show Error
        MESSAGE ID LS_RETURN-ID
                TYPE 'S'
                NUMBER LS_RETURN-NUMBER DISPLAY LIKE LS_RETURN-TYPE
                WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                     LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
        RETURN.
      ELSE.
        ZCL_SDSSD_ORDER_CONFIRMATION=>PROCESS_APPROVAL(
         EXPORTING
           IT_SCHEDLINE = LT_SCHEDLINE
           IF_APPROVER  = ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MANAGER
           IF_APPV_STAT = UF_STAT
         IMPORTING
           ES_RETURN    = LS_RETURN
           ET_RESULT    = LT_APPV_RESULT ).
        IF LS_RETURN IS NOT INITIAL.
*   Show Error
          LOOP AT LT_SCHEDLINE INTO DATA(LS_TMP).
            UPDATE ZSDSSDT009 SET CO_APPV_STAT = SPACE
                            WHERE VBELN EQ LS_TMP-VBELN
                              AND POSNR EQ LS_TMP-POSNR
                              AND ETENR EQ LS_TMP-ETENR.
            COMMIT WORK AND WAIT.
          ENDLOOP.
          MESSAGE ID LS_RETURN-ID
                  TYPE 'S'
                  NUMBER LS_RETURN-NUMBER DISPLAY LIKE LS_RETURN-TYPE
                  WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                       LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
* Start Approval for Items
    ZCL_SDSSD_ORDER_CONFIRMATION=>PROCESS_APPROVAL(
      EXPORTING
        IT_SCHEDLINE = LT_SCHEDLINE
        IF_APPROVER  = GF_APPROVER
        IF_APPV_STAT = UF_STAT
      IMPORTING
        ES_RETURN    = LS_RETURN
        ET_RESULT    = LT_APPV_RESULT ).
    IF LS_RETURN IS NOT INITIAL.
*   Show Error
      MESSAGE ID LS_RETURN-ID
              TYPE 'S'
              NUMBER LS_RETURN-NUMBER DISPLAY LIKE LS_RETURN-TYPE
              WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                   LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
      RETURN.
    ENDIF.
  ENDIF.

* Update result
  IF LT_APPV_RESULT IS NOT INITIAL.
    PERFORM F_UPDATE_RESULT  USING  LT_SCHEDLINE
                                    LT_APPV_RESULT
                          CHANGING  CT_RESULT.
  ENDIF.

* Message: Operation completed. Please check result in column "Message".
  MESSAGE S014(ZSDSSD01).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_CONF_RESULT
*----------------------------------------------------------------------*
*  Collect Confirmation result
*----------------------------------------------------------------------*
FORM F_COLLECT_CONF_RESULT  USING  UT_CONF TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA
                                   UT_REMAIN_ADVREC TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_REMAIN_ADVREC
                          CHANGING CT_RESULT TYPE  TT_RESULT1.

  DATA:
    LS_RESULT  TYPE  TS_RESULT1,
    LS_CELLTAB TYPE  LVC_S_STYL.

  DATA:
    LF_REMAIN_FOUND TYPE  FLAG,
    LF_ADVREC_DIFF  TYPE  WERT1,
    LF_AMOUNT_DIFF  TYPE  WERT1.


* Initialize Output
  CLEAR: CT_RESULT.

  LOOP AT UT_CONF ASSIGNING FIELD-SYMBOL(<L_CONF>).

    CLEAR LS_RESULT.

    LS_RESULT-EDATU          =  <L_CONF>-EDATU.
    LS_RESULT-VBELN          =  <L_CONF>-VBELN.
    LS_RESULT-POSNR          =  <L_CONF>-POSNR.
    LS_RESULT-ETENR          =  <L_CONF>-ETENR.
    LS_RESULT-UEPOS          =  <L_CONF>-UEPOS.
    LS_RESULT-LIFSP          =  <L_CONF>-LIFSP.
    LS_RESULT-BMENG          =  <L_CONF>-BMENG.
    LS_RESULT-VRKME          =  <L_CONF>-VRKME.
    LS_RESULT-AMOUNT0        =  <L_CONF>-AMOUNT0.
    LS_RESULT-AMOUNT         =  <L_CONF>-AMOUNT.
    LS_RESULT-WAERS          =  <L_CONF>-WAERS.
    LS_RESULT-KUNNR          =  <L_CONF>-KUNNR.
    LS_RESULT-KUNNR_NAME     =  <L_CONF>-KUNNR_NAME.
    LS_RESULT-VTWEG          =  <L_CONF>-VTWEG.
    LS_RESULT-VTWEG_TXT      =  <L_CONF>-VTWEG_TXT.
    LS_RESULT-AUART          =  <L_CONF>-AUART.
    LS_RESULT-AUART_TXT      =  <L_CONF>-AUART_TXT.
    LS_RESULT-VKBUR          =  <L_CONF>-VKBUR.
    LS_RESULT-VKBUR_TXT      =  <L_CONF>-VKBUR_TXT.
    LS_RESULT-VKGRP          =  <L_CONF>-VKGRP.
    LS_RESULT-VKGRP_TXT      =  <L_CONF>-VKGRP_TXT.
    LS_RESULT-SPART          =  <L_CONF>-SPART.
    LS_RESULT-SPART_TXT      =  <L_CONF>-SPART_TXT.
    LS_RESULT-ZTERM          =  <L_CONF>-ZTERM.
    LS_RESULT-ADVGP          =  <L_CONF>-ADVGP.
    LS_RESULT-MATNR          =  <L_CONF>-MATNR.
    LS_RESULT-ARKTX          =  <L_CONF>-ARKTX.
    LS_RESULT-CO_APPV_STAT   =  <L_CONF>-CO_APPV_STAT.
    LS_RESULT-CO_APPV_BY     =  <L_CONF>-CO_APPV_BY.
    LS_RESULT-CO_APPV_DATE   =  <L_CONF>-CO_APPV_DATE.
    LS_RESULT-CO_APPV_TIME   =  <L_CONF>-CO_APPV_TIME.
    LS_RESULT-MGR_APPV_STAT  =  <L_CONF>-MGR_APPV_STAT.
    LS_RESULT-MGR_APPV_BY    =  <L_CONF>-MGR_APPV_BY.
    LS_RESULT-MGR_APPV_DATE  =  <L_CONF>-MGR_APPV_DATE.
    LS_RESULT-MGR_APPV_TIME  =  <L_CONF>-MGR_APPV_TIME.

*   Check Remaining Advrec exist for customer?
    CLEAR LF_REMAIN_FOUND.
    READ TABLE UT_REMAIN_ADVREC ASSIGNING FIELD-SYMBOL(<L_REMAIN_ADVREC>)
                                WITH KEY KUNNR = LS_RESULT-KUNNR
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      READ TABLE <L_REMAIN_ADVREC>-ADVGP_ADVREC TRANSPORTING NO FIELDS
                                                WITH KEY ADVGP = LS_RESULT-ADVGP
                                                BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LF_REMAIN_FOUND = GC_TRUE.
      ENDIF.
    ENDIF.

*   When Remain ADVREC Found or ADVREC Assigned
    IF <L_CONF>-AMOUNT GT 0 AND
       NOT ( LF_REMAIN_FOUND IS INITIAL AND
             <L_CONF>-ADVREC IS INITIAL ).
*     Assign ADVREC Button
      CLEAR LS_CELLTAB.
      LS_CELLTAB-FIELDNAME = 'BT_ADVREC' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON .
      INSERT LS_CELLTAB INTO TABLE LS_RESULT-CELLTAB.

*Add+ 420000395 >>>
      CLEAR: LF_ADVREC_DIFF,LF_AMOUNT_DIFF.
      READ TABLE GT_ADVREC_DIFF ASSIGNING FIELD-SYMBOL(<L_GENC>)
                                  WITH KEY PARAM_EXT = <L_CONF>-ADVGP
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
        LF_ADVREC_DIFF = <L_GENC>-VALUE_LOW.
        LF_AMOUNT_DIFF = <L_CONF>-AMOUNT - <L_CONF>-AMOUNT_ADV.
      ENDIF.
*<<< 420000395
*     Assign ADVREC Icon
      IF <L_CONF>-ADVREC IS NOT INITIAL AND
*         <L_CONF>-AMOUNT = <L_CONF>-AMOUNT_ADV.  "Del- 420000395
        ( ( <L_CONF>-AMOUNT = <L_CONF>-AMOUNT_ADV ) OR ( LF_AMOUNT_DIFF BETWEEN 0 AND LF_ADVREC_DIFF ) ). "Add+ 420000395
        LS_RESULT-BT_ADVREC  =  GC_LED_GREEN.
      ELSE.
        LS_RESULT-BT_ADVREC  =  GC_LED_RED.
      ENDIF.
    ENDIF.

*   Approve Status Icon
    MC_ASSIGN_ICON_STAT LS_RESULT-CO_APPV_STAT  LS_RESULT-ICON_CO_APPV.
    MC_ASSIGN_ICON_STAT LS_RESULT-MGR_APPV_STAT LS_RESULT-ICON_MGR_APPV.

*   Approve Status Text
    MC_ASSIGN_TXT_STAT LS_RESULT-CO_APPV_STAT  LS_RESULT-CO_APPV_STAT_TXT.
    MC_ASSIGN_TXT_STAT LS_RESULT-MGR_APPV_STAT LS_RESULT-MGR_APPV_STAT_TXT.

*   Assign Line Color
    PERFORM F_ASSIGN_LINE_COLOR1  USING  LS_RESULT
                                CHANGING LS_RESULT-LINECOLOR.

*   Disable Selection when
*      - In CoOper list and Manager approved
    IF ( LS_RESULT-MGR_APPV_STAT EQ ZCL_SDSSD_ORDER_CONFIRMATION=>GC_STAT_APPROVE AND
         GF_APPROVER EQ ZCL_SDSSD_ORDER_CONFIRMATION=>GC_COOPERATOR ) OR
*       - Delivery not blocked without manager approved
       ( LS_RESULT-MGR_APPV_STAT NE ZCL_SDSSD_ORDER_CONFIRMATION=>GC_STAT_APPROVE AND
         LS_RESULT-LIFSP IS INITIAL ).
      LS_RESULT-DISABLE = GC_TRUE.
      CLEAR LS_CELLTAB.
      LS_CELLTAB-FIELDNAME = 'SELCT' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED .
      INSERT LS_CELLTAB INTO TABLE LS_RESULT-CELLTAB.
    ENDIF.

    INSERT LS_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_RERRESH_DATA
*----------------------------------------------------------------------*
*  Get Refreshed data of specfied schedule lines
*----------------------------------------------------------------------*
FORM F_GET_RERRESH_DATA  USING  UT_SCHEDLINE  TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE
                                UF_APPROVER   TYPE  CHAR1
                       CHANGING CT_RESULT     TYPE  TT_RESULT1.

  DATA:
    LT_CONF          TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA,
    LT_REMAIN_ADVREC TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_REMAIN_ADVREC.


* Initialize Output
  CLEAR: CT_RESULT.

* Get Refresh data for Specified Key
  ZCL_SDSSD_ORDER_CONFIRMATION=>GET_DATA(
    EXPORTING
      IT_SCHEDLINE     = UT_SCHEDLINE
      IF_APPROVER      = UF_APPROVER
      IF_REMAIN_ADVREC = GC_TRUE
    IMPORTING
      ET_DATA          = LT_CONF
      ET_REMAIN_ADVREC = LT_REMAIN_ADVREC ).

* Assign Result
  PERFORM F_COLLECT_CONF_RESULT  USING  LT_CONF
                                        LT_REMAIN_ADVREC
                               CHANGING CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CANCEL_APPROVAL
*----------------------------------------------------------------------*
*  Cancel approval
*----------------------------------------------------------------------*
FORM F_CANCEL_APPROVAL  CHANGING CT_RESULT TYPE TT_RESULT1.

  DATA:
    LT_SCHEDLINE   TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE,
    LT_APPV_RESULT TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_APPV_RESULT.

  DATA:
    LS_RETURN  TYPE  BAPIRET1.


* Clear Message Column
  MODIFY CT_RESULT FROM VALUE #( MSGTX = SPACE )
                   TRANSPORTING MSGTX
                   WHERE MSGTX IS NOT INITIAL.

* Collect Selected Schedline
  PERFORM F_COLLECT_SELECTION  USING  CT_RESULT
                             CHANGING LT_SCHEDLINE.
  IF LT_SCHEDLINE IS INITIAL.
*   Error: Please select item(s) to be processed.
    MESSAGE S004(ZSDSSD01).
    RETURN.
  ENDIF.

* Start Approval for Items
  ZCL_SDSSD_ORDER_CONFIRMATION=>CANCEL_APPROVAL(
    EXPORTING
      IT_SCHEDLINE = LT_SCHEDLINE
      IF_APPROVER  = GF_APPROVER
    IMPORTING
      ES_RETURN    = LS_RETURN
      ET_RESULT    = LT_APPV_RESULT ).
  IF LS_RETURN IS NOT INITIAL.
*   Show Error
    MESSAGE ID LS_RETURN-ID
            TYPE 'S'
            NUMBER LS_RETURN-NUMBER DISPLAY LIKE LS_RETURN-TYPE
            WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    RETURN.
  ENDIF.

* Update result
  IF LT_APPV_RESULT IS NOT INITIAL.
    PERFORM F_UPDATE_RESULT  USING  LT_SCHEDLINE
                                    LT_APPV_RESULT
                          CHANGING  CT_RESULT.
  ENDIF.

* Message: Operation completed. Please check result in column "Message".
  MESSAGE S014(ZSDSSD01).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_SELECTION
*----------------------------------------------------------------------*
*  Collect SchedLine selected
*----------------------------------------------------------------------*
FORM F_COLLECT_SELECTION  USING  UT_RESULT  TYPE  TT_RESULT1
                        CHANGING CT_SCHEDLINE TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE.

* Initialize Output
  CLEAR CT_SCHEDLINE.


  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE SELCT EQ GC_TRUE.
*   Collect Sched line
    INSERT CORRESPONDING ZCL_SDSSD_ORDER_CONFIRMATION=>TS_SCHEDLINE( <L_RESULT> )
           INTO TABLE CT_SCHEDLINE.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UPDATE_RESULT
*----------------------------------------------------------------------*
*  Update Result
*----------------------------------------------------------------------*
FORM F_UPDATE_RESULT  USING  UT_SCHEDLINE  TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE
                             UT_APPV_RESULT TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_APPV_RESULT
                    CHANGING CT_RESULT  TYPE  TT_RESULT1.

  DATA:
    LT_DATA        TYPE  TT_RESULT1.


* Get Refresh data for selected items
  PERFORM F_GET_RERRESH_DATA  USING  UT_SCHEDLINE
                                     GF_APPROVER
                            CHANGING LT_DATA.

* Update Result
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    READ TABLE CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                         WITH KEY VBELN = <L_DATA>-VBELN
                                  POSNR = <L_DATA>-POSNR
                                  ETENR = <L_DATA>-ETENR.
    IF SY-SUBRC EQ 0.
*     Update Data
      <L_RESULT> = <L_DATA>.
      <L_RESULT>-SELCT = GC_TRUE.
    ENDIF.
  ENDLOOP.

* Update Approve Result
  LOOP AT UT_APPV_RESULT ASSIGNING FIELD-SYMBOL(<L_APPV_RESULT>).
    READ TABLE CT_RESULT ASSIGNING <L_RESULT>
                         WITH KEY VBELN = <L_APPV_RESULT>-VBELN
                                  POSNR = <L_APPV_RESULT>-POSNR
                                  ETENR = <L_APPV_RESULT>-ETENR.
    IF SY-SUBRC EQ 0.
      <L_RESULT>-MSGTX = <L_APPV_RESULT>-MSGTX.
    ENDIF.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_LINE_COLOR1
*----------------------------------------------------------------------*
*  Assign Line Color
*----------------------------------------------------------------------*
FORM F_ASSIGN_LINE_COLOR1  USING  US_RESULT  TYPE  TS_RESULT1
                         CHANGING CF_LINECOLOR TYPE  TS_RESULT1-LINECOLOR.

  CONSTANTS:
*    LC_YELLOW  TYPE  TS_RESULT1-LINECOLOR VALUE 'C300',
*    LC_YELLOW2 TYPE  TS_RESULT1-LINECOLOR VALUE 'C310',
*    LC_BLUE    TYPE  TS_RESULT1-LINECOLOR VALUE 'C410',
    LC_RED    TYPE  TS_RESULT1-LINECOLOR VALUE 'C600',
    LC_GREEN  TYPE  TS_RESULT1-LINECOLOR VALUE 'C500',
    LC_ORANGE TYPE  TS_RESULT1-LINECOLOR VALUE 'C700'.


* Initialize Output
  CLEAR: CF_LINECOLOR.

* Line unblocked without manager approval
  IF US_RESULT-LIFSP IS INITIAL AND
     US_RESULT-MGR_APPV_STAT NE ZCL_SDSSD_ORDER_CONFIRMATION=>GC_STAT_APPROVE.
    CF_LINECOLOR = LC_RED.
* Line which approved by manager
  ELSEIF US_RESULT-MGR_APPV_STAT EQ ZCL_SDSSD_ORDER_CONFIRMATION=>GC_STAT_APPROVE.
    CF_LINECOLOR = LC_GREEN.
* Line related with Adv Receive
  ELSEIF US_RESULT-BT_ADVREC IS NOT INITIAL.
    CF_LINECOLOR = LC_ORANGE.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_ADVREC_MULTI
*----------------------------------------------------------------------*
*  Assign Advance Receive for multiple items
*----------------------------------------------------------------------*
FORM F_ASSIGN_ADVREC_MULTI  CHANGING CT_RESULT TYPE TT_RESULT1.

  DATA:
    LT_SCHEDLINE TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE.


* Clear Message Column
  MODIFY CT_RESULT FROM VALUE #( MSGTX = SPACE )
                   TRANSPORTING MSGTX
                   WHERE MSGTX IS NOT INITIAL.

* Collect Selected Schedline
  PERFORM F_COLLECT_SELECTION  USING  CT_RESULT
                             CHANGING LT_SCHEDLINE.
  IF LT_SCHEDLINE IS INITIAL.
*   Error: Please select item(s) to be processed.
    MESSAGE S004(ZSDSSD01).
    RETURN.
  ENDIF.

* Assign ADV Receive
  PERFORM F_ASSIGN_ADVREC  USING  LT_SCHEDLINE
                                  GC_TRUE
                         CHANGING CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_button_click_1
*----------------------------------------------------------------------*
*  ALV Event on Button click
*----------------------------------------------------------------------*
FORM F_BUTTON_CLICK_1 USING US_ROW_ID    TYPE LVC_S_ROID
                            US_COLUMN_ID TYPE LVC_S_COL ##CALLED.

  DATA:
    LT_SCHEDLINE  TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE.

  DATA:
    LS_STABLE TYPE LVC_S_STBL.


  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'BT_ADVREC'.
*     Read Row
      READ TABLE GT_RESULT1 ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                            INDEX US_ROW_ID-ROW_ID.
      IF SY-SUBRC NE 0.
*       No row found
        RETURN.
      ENDIF.
      IF GF_APPROVER EQ ZCL_SDSSD_ORDER_CONFIRMATION=>GC_MANAGER AND
         <L_RESULT>-BT_ADVREC EQ GC_LED_RED.
*       Error: Advance Receive amount is not yet assigned.
        MESSAGE I023(ZSDSSD01).
        RETURN.
      ENDIF.

      INSERT VALUE #( VBELN = <L_RESULT>-VBELN
                      POSNR = <L_RESULT>-POSNR
                      ETENR = <L_RESULT>-ETENR )
             INTO TABLE LT_SCHEDLINE.
*     Assign ADV Receive
      PERFORM F_ASSIGN_ADVREC  USING  LT_SCHEDLINE
                                      <L_RESULT>-SELCT
                             CHANGING GT_RESULT1.
*     Refresh Display
      CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = LS_STABLE
          I_SOFT_REFRESH = 'X'.

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_ADVREC
*----------------------------------------------------------------------*
*  Assign Advance Receive data
*----------------------------------------------------------------------*
FORM F_ASSIGN_ADVREC  USING  UT_SCHEDLINE TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TT_SCHEDLINE
                             UF_MULTI  TYPE  FLAG
                    CHANGING CT_RESULT TYPE  TT_RESULT1.

  DATA:
    LT_CONF          TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA,
    LT_ADVREC        TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_ADVREC_DATA,
    LT_ADVGP_ADVREC  TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_ADVGP_ADVREC,
    LT_REMAIN_ADVREC TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_REMAIN_ADVREC,
    LT_DATA          TYPE  TT_RESULT1.


* Call ADVRec Assignment
  ZCL_SDSSD_ORDER_CONFIRMATION=>MAINTAIN_ADVREC_ASSIGN(
    EXPORTING
      IT_SCHEDLINE = UT_SCHEDLINE
      IF_AUTO      = GC_TRUE
    IMPORTING
      ET_CONF      = LT_CONF
      ET_ADVREC    = LT_ADVREC ).

  READ TABLE LT_CONF ASSIGNING FIELD-SYMBOL(<L_CONF>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Update Result
  INSERT VALUE #( ADVGP = <L_CONF>-ADVGP
                  ADVREC = LT_ADVREC )
         INTO TABLE LT_ADVGP_ADVREC.
  INSERT VALUE #( KUNNR = <L_CONF>-KUNNR
                  ADVGP_ADVREC = LT_ADVGP_ADVREC )
         INTO TABLE LT_REMAIN_ADVREC.

* Assign Result
  PERFORM F_COLLECT_CONF_RESULT  USING  LT_CONF
                                        LT_REMAIN_ADVREC
                               CHANGING LT_DATA.

* Update Result
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    READ TABLE CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                         WITH KEY VBELN = <L_DATA>-VBELN
                                  POSNR = <L_DATA>-POSNR
                                  ETENR = <L_DATA>-ETENR.
    IF SY-SUBRC EQ 0.
*     Update Data
      <L_RESULT> = <L_DATA>.
      <L_RESULT>-SELCT = UF_MULTI.
    ENDIF.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_ADVREC_REPORT
*----------------------------------------------------------------------*
*  Get Advance Receive Data
*----------------------------------------------------------------------*
FORM F_GET_ADVREC_REPORT  CHANGING CT_RESULT TYPE TT_RESULT2.

  TYPES: BEGIN OF TS_SOITEM,
           VBELN TYPE  VBAP-VBELN,
           POSNR TYPE  VBAP-POSNR,
         END OF TS_SOITEM.
  TYPES: TT_SOITEM TYPE SORTED TABLE OF TS_SOITEM
                          WITH UNIQUE KEY VBELN
                                          POSNR.

  CONSTANTS:
    LC_ETENR  TYPE  VBEP-ETENR VALUE '0000'.

  DATA:
    LS_RESULT      TYPE  TS_RESULT2,
    LS_RESULT_PREV TYPE  TS_RESULT2.

  DATA:
    LF_NATION TYPE  ADRC-NATION VALUE ' ',
    LF_FLAG   TYPE  C.


  SELECT A~VBELN,
         A~POSNR,
         A~ETENR,
         A~BUKRS,
         A~BELNR,
         A~GJAHR,
         D~EDATU,
         B~KUNNR,
         CONCAT_WITH_SPACE( F~NAME1,F~NAME2,1 ) AS KUNNR_NAME,
         C~BUDAT,
         A~DMBTR,
         A~WAERS
    FROM ZSDSSDT010 AS A
           INNER JOIN VBAK AS B
             ON  B~VBELN = A~VBELN
           INNER JOIN BKPF AS C
             ON  C~BUKRS = A~BUKRS
             AND C~BELNR = A~BELNR
             AND C~GJAHR = A~GJAHR
           INNER JOIN VBEP AS D
             ON  D~VBELN = A~VBELN
             AND D~POSNR = A~POSNR
             AND D~ETENR = A~ETENR
*            INNER JOIN KNA1 AS E
*              ON  E~KUNNR = B~KUNNR
              INNER JOIN VBPA AS J
              ON  A~VBELN = J~VBELN
              AND J~PARVW = 'AG'
            INNER JOIN ADRC AS F
              ON  F~ADDRNUMBER = J~ADRNR
              AND F~NATION     = @LF_NATION
   WHERE A~VBELN     IN @S_VBELN2
     AND D~EDATU     IN @S_EDATU2
     AND B~VKORG     IN @S_VKORG2
     AND B~KUNNR     IN @S_KUNNR2
     AND C~BUDAT     IN @S_BUDAT
     AND F~DATE_FROM LE @SY-DATUM
     AND F~DATE_TO   GE @SY-DATUM
      ORDER BY A~VBELN ASCENDING,
               A~POSNR ASCENDING,
               A~ETENR ASCENDING,
               A~BUKRS ASCENDING,
               A~BELNR ASCENDING,
               A~GJAHR ASCENDING
    INTO TABLE @DATA(LT_ADVREC).
  IF SY-SUBRC NE 0.
    RETURN.
  ELSE.
*Add+ 420000631 >>>
    SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,KOART,DMBTR "#EC CI_NO_TRANSFORM
      FROM BSEG
       FOR ALL ENTRIES IN @LT_ADVREC
     WHERE BUKRS = @LT_ADVREC-BUKRS
       AND BELNR = @LT_ADVREC-BELNR
       AND GJAHR = @LT_ADVREC-GJAHR
       AND KOART = 'D'
       AND AUGBL <> @SPACE
      INTO TABLE @DATA(LT_BSEG_KUNNR).
    IF SY-SUBRC = 0.
      SORT LT_BSEG_KUNNR BY BUKRS BELNR GJAHR.
      SELECT BUKRS,XREF2,GJAHR,BELNR,BUZEI,UMSKZ,KUNNR,UMSKS,DMBTR "#EC CI_NO_TRANSFORM
        FROM BSEG
         FOR ALL ENTRIES IN @LT_BSEG_KUNNR
       WHERE BUKRS = @LT_BSEG_KUNNR-BUKRS
         AND BELNR = @LT_BSEG_KUNNR-AUGBL
         AND GJAHR = @LT_BSEG_KUNNR-GJAHR
         AND UMSKZ = 'S'
         AND XREF2 <> @SPACE
        INTO TABLE @DATA(LT_BSEG_S).
      IF SY-SUBRC = 0.
        SORT LT_BSEG_S BY BUKRS XREF2 GJAHR.

        SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,ZUONR,WRBTR,WMWST"DMBTR,MWSTS "#EC CI_NO_TRANSFORM
          FROM BSID_VIEW
           FOR ALL ENTRIES IN @LT_BSEG_S
         WHERE BUKRS = @LT_BSEG_S-BUKRS
           AND BELNR = @LT_BSEG_S-BELNR
           AND GJAHR = @LT_BSEG_S-GJAHR
     APPENDING TABLE @DATA(LT_BSID).

        SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,ZUONR,WRBTR,WMWST"DMBTR,MWSTS "#EC CI_NO_TRANSFORM
          FROM BSAD_VIEW
           FOR ALL ENTRIES IN @LT_BSEG_S
         WHERE BUKRS = @LT_BSEG_S-BUKRS
           AND BELNR = @LT_BSEG_S-BELNR
           AND GJAHR = @LT_BSEG_S-GJAHR
     APPENDING TABLE @LT_BSID.
        IF LT_BSID IS NOT INITIAL.
          SORT LT_BSID BY BUKRS BELNR GJAHR.
        ENDIF.
      ENDIF.
    ENDIF.
*<<<Add+ 420000631
*    GT_UMSKZ
    SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,ZUONR,WRBTR,WMWST"DMBTR,MWSTS "#EC CI_NO_TRANSFORM
      FROM BSID_VIEW
       FOR ALL ENTRIES IN @LT_ADVREC
     WHERE BUKRS = @LT_ADVREC-BUKRS
       AND BELNR = @LT_ADVREC-BELNR
       AND GJAHR = @LT_ADVREC-GJAHR
       AND UMSKZ IN @GT_UMSKZ
      INTO TABLE @DATA(LT_BSID_UMSKZ).
    SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,ZUONR,WRBTR,WMWST"DMBTR,MWSTS "#EC CI_NO_TRANSFORM
      FROM BSAD_VIEW
       FOR ALL ENTRIES IN @LT_ADVREC
     WHERE BUKRS = @LT_ADVREC-BUKRS
       AND BELNR = @LT_ADVREC-BELNR
       AND GJAHR = @LT_ADVREC-GJAHR
       AND UMSKZ IN @GT_UMSKZ
    APPENDING TABLE @LT_BSID_UMSKZ.
    IF LT_BSID_UMSKZ IS NOT INITIAL.
      SORT LT_BSID_UMSKZ BY BUKRS BELNR GJAHR.
      LOOP AT LT_ADVREC ASSIGNING FIELD-SYMBOL(<L_ADVREC>).
        DATA(LV_TABIX) = SY-TABIX.
        READ TABLE LT_BSID_UMSKZ WITH KEY BUKRS = <L_ADVREC>-BUKRS
                                          BELNR = <L_ADVREC>-BELNR
                                          GJAHR = <L_ADVREC>-GJAHR
                                          BINARY SEARCH TRANSPORTING NO FIELDS.
        IF SY-SUBRC <> 0.
          DELETE LT_ADVREC INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF LT_ADVREC IS NOT INITIAL.
      SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,ZUONR,WRBTR,WMWST"DMBTR,MWSTS "#EC CI_NO_TRANSFORM
        FROM BSID_VIEW
         FOR ALL ENTRIES IN @LT_ADVREC
       WHERE BUKRS = @LT_ADVREC-BUKRS
         AND BELNR = @LT_ADVREC-BELNR
         AND GJAHR = @LT_ADVREC-GJAHR
    APPENDING TABLE @LT_BSID.
      SELECT BUKRS,BELNR,GJAHR,BUZEI,UMSKZ,KUNNR,UMSKS,AUGDT,AUGBL,ZUONR,WRBTR,WMWST"DMBTR,MWSTS "#EC CI_NO_TRANSFORM
        FROM BSAD_VIEW
         FOR ALL ENTRIES IN @LT_ADVREC
       WHERE BUKRS = @LT_ADVREC-BUKRS
         AND BELNR = @LT_ADVREC-BELNR
         AND GJAHR = @LT_ADVREC-GJAHR
      APPENDING TABLE @LT_BSID.
      IF LT_BSID IS NOT INITIAL.
        SORT LT_BSID BY BUKRS BELNR GJAHR.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.
  ENDIF.

* Collect Sales Order Item
  DATA(LT_SOITEM) = CORRESPONDING TT_SOITEM( LT_ADVREC DISCARDING DUPLICATES
                                               MAPPING VBELN = VBELN
                                                       POSNR = POSNR ).

* Get Related Billing
  SELECT X~VBELN,
         X~POSNR,
         @LC_ETENR AS ETENR,
         A~VBELN AS VBELN_VF,
         A~POSNR AS POSNR_VF,
         B~FKDAT,
         B~WAERK AS WAERS,
         A~NETWR,
         A~MWSBP,
         A~CMPRE
    FROM @LT_SOITEM AS X
           INNER JOIN VBRP AS A
             ON  A~AUBEL = X~VBELN
             AND A~AUPOS = X~POSNR
           INNER JOIN VBRK AS B
             ON  B~VBELN = A~VBELN
   ORDER BY X~VBELN ASCENDING,
            X~POSNR ASCENDING,
            B~FKDAT ASCENDING,
            A~VBELN ASCENDING,
            A~POSNR ASCENDING
    INTO TABLE @DATA(LT_VBRP).
  IF SY-SUBRC NE 0.
    CLEAR LT_VBRP.
  ENDIF.

* Collect Result
  SORT LT_ADVREC BY BELNR EDATU VBELN POSNR.
  LOOP AT LT_ADVREC ASSIGNING <L_ADVREC>.

    CLEAR LS_RESULT.
    LS_RESULT-EDATU      = <L_ADVREC>-EDATU.
    LS_RESULT-KUNNR      = <L_ADVREC>-KUNNR.
    LS_RESULT-KUNNR_NAME = <L_ADVREC>-KUNNR_NAME.
    LS_RESULT-VBELN      = <L_ADVREC>-VBELN.
    LS_RESULT-POSNR      = <L_ADVREC>-POSNR.
    LS_RESULT-ETENR      = <L_ADVREC>-ETENR.
    LS_RESULT-BUKRS      = <L_ADVREC>-BUKRS.
*   Link with Billing Item
    READ TABLE LT_VBRP ASSIGNING FIELD-SYMBOL(<L_VBRP>)
                       WITH KEY VBELN = LS_RESULT-VBELN
                                POSNR = LS_RESULT-POSNR
                                ETENR = LS_RESULT-ETENR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      READ TABLE LT_VBRP ASSIGNING <L_VBRP>
                         WITH KEY VBELN = LS_RESULT-VBELN
                                  POSNR = LS_RESULT-POSNR
                                  ETENR = LC_ETENR
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_VBRP>-ETENR = LS_RESULT-ETENR.
      ELSE.
        UNASSIGN <L_VBRP>.
      ENDIF.
    ENDIF.
    IF <L_VBRP> IS ASSIGNED.
      LS_RESULT-VBELN_VF   = <L_VBRP>-VBELN_VF.
      LS_RESULT-POSNR_VF   = <L_VBRP>-POSNR_VF.
      LS_RESULT-FKDAT      = <L_VBRP>-FKDAT.
      LS_RESULT-NETWR      = <L_VBRP>-NETWR + <L_VBRP>-MWSBP.
    ENDIF.
*    LS_RESULT-BELNR      = <L_ADVREC>-BELNR. "Del- 420000631
*Add+ 420000631 >>>
    "Replace LS_RESULT-BELNR
    IF LS_RESULT-VBELN_VF IS INITIAL.
      READ TABLE LT_BSEG_KUNNR INTO DATA(LS_BSEG_KUNNR) WITH KEY BUKRS = <L_ADVREC>-BUKRS
                                                                 BELNR = <L_ADVREC>-BELNR
                                                                 GJAHR = <L_ADVREC>-GJAHR
                                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        READ TABLE LT_BSEG_S INTO DATA(LS_BSEG_S) WITH KEY BUKRS = LS_BSEG_KUNNR-BUKRS
                                                           XREF2 = LS_BSEG_KUNNR-BELNR
                                                           GJAHR = LS_BSEG_KUNNR-GJAHR
                                                           BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_RESULT-BELNR      = LS_BSEG_S-BELNR.
          LS_RESULT-UMSKZ      = LS_BSEG_S-UMSKZ.
        ELSE.
          LF_FLAG = 'X'.
        ENDIF.
      ELSE.
        LF_FLAG = 'X'.
      ENDIF.
    ELSE.
      LF_FLAG = 'X'.
    ENDIF.
    IF LF_FLAG = 'X'.
      LS_RESULT-BELNR      = <L_ADVREC>-BELNR.
    ENDIF.
    CLEAR LF_FLAG.
*<<<Add+ 420000631
    LS_RESULT-GJAHR      = <L_ADVREC>-GJAHR.
    LS_RESULT-DMBTR      = <L_ADVREC>-DMBTR.
    LS_RESULT-WAERS      = <L_ADVREC>-WAERS.
*Del- 420000631 >>>
*    READ TABLE LT_BSID ASSIGNING FIELD-SYMBOL(<L_BSID>)
*                       WITH KEY BUKRS = <L_ADVREC>-BUKRS
*                                BELNR = <L_ADVREC>-BELNR
*                                GJAHR = <L_ADVREC>-GJAHR
*                       BINARY SEARCH.
*<<<Del- 420000631
    READ TABLE LT_BSID ASSIGNING FIELD-SYMBOL(<L_BSID>)
                       WITH KEY BUKRS = LS_RESULT-BUKRS
                                BELNR = LS_RESULT-BELNR
                                GJAHR = LS_RESULT-GJAHR
                                BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_RESULT-UMSKZ     = <L_BSID>-UMSKZ.
      LS_RESULT-FI_DMBTR  = <L_BSID>-WRBTR + <L_BSID>-WMWST.

      IF LS_RESULT_PREV-BELNR = LS_RESULT-BELNR.
        LS_RESULT-BEGIN = LS_RESULT_PREV-REMAIN.
      ELSE.
        LS_RESULT-BEGIN = LS_RESULT-FI_DMBTR.
      ENDIF.

      LS_RESULT-REMAIN    = LS_RESULT-BEGIN - LS_RESULT-DMBTR.
    ENDIF.

    LS_RESULT_PREV =  LS_RESULT.

**   Link with Billing Item
*    READ TABLE LT_VBRP ASSIGNING FIELD-SYMBOL(<L_VBRP>)
*                       WITH KEY VBELN = LS_RESULT-VBELN
*                                POSNR = LS_RESULT-POSNR
*                                ETENR = LS_RESULT-ETENR
*                       BINARY SEARCH.
*    IF SY-SUBRC NE 0.
*      READ TABLE LT_VBRP ASSIGNING <L_VBRP>
*                         WITH KEY VBELN = LS_RESULT-VBELN
*                                  POSNR = LS_RESULT-POSNR
*                                  ETENR = LC_ETENR
*                         BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        <L_VBRP>-ETENR = LS_RESULT-ETENR.
*      ELSE.
*        UNASSIGN <L_VBRP>.
*      ENDIF.
*    ENDIF.
*    IF <L_VBRP> IS ASSIGNED.
*      LS_RESULT-VBELN_VF   = <L_VBRP>-VBELN_VF.
*      LS_RESULT-POSNR_VF   = <L_VBRP>-POSNR_VF.
*      LS_RESULT-FKDAT      = <L_VBRP>-FKDAT.
*      LS_RESULT-NETWR      = <L_VBRP>-NETWR + <L_VBRP>-MWSBP.
*    ENDIF.

    INSERT LS_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

*  SORT CT_RESULT BY BELNR.
*  LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>).
*
*    IF LS_RESULT_PREV-BELNR = <LFS_RESULT>-BELNR.
*      <LFS_RESULT>-BEGIN = LS_RESULT_PREV-REMAIN.
*    ELSE.
*      <LFS_RESULT>-BEGIN = <LFS_RESULT>-FI_DMBTR.
*    ENDIF.
*
*    LS_RESULT_PREV =  <LFS_RESULT>.
*  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT2
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT2  USING  UT_RESULT TYPE TT_RESULT2.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Enable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Not soft refresh but not all refresh
  CLEAR: GF_SOFT_REFRESH_1,
         GF_REFRESH_ALL_1.

* ALV Layout
  PERFORM F_ALV_LAYOUT2 CHANGING GS_LAYOUT_1
                                 GS_VARIANT_1
                                 GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_2.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_2.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT2 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT2 CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT2
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT2 CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                            CS_VARIANT TYPE  DISVARIANT
                            CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'C'.
  CS_LAYOUT-NO_ROWMARK = SPACE.
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = 'ZSDSSDR0180_1'. "Separated saved Layout

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT2
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT2 CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT   TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_2
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'EDATU'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'KUNNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'KUNNR_NAME'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'VBELN'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
      WHEN 'POSNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'ETENR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'BUKRS'.
      WHEN 'BELNR'.
*       Text-c08 : Pay-In No.
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'GJAHR'.
      WHEN 'UMSKZ'.
      WHEN 'FI_DMBTR'. "Pay-In Amount
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-COL_OPT   = ABAP_TRUE.
      WHEN 'BEGIN'.	"Beginning
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-COL_OPT   = ABAP_TRUE.
      WHEN 'REMAIN'. "emaining Pay-In
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-COL_OPT   = ABAP_TRUE.
      WHEN 'DMBTR'.
*       Text-c07 : SO Adv.Amt.
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'WAERS'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'VBELN_VF'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
      WHEN 'POSNR_VF'.
      WHEN 'FKDAT'.
      WHEN 'NETWR'.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
*       Text-c09 : Item Amount Inc. VAT.
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.

      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT2
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT2  CHANGING PT_SORT TYPE LVC_T_SORT ##NEEDED.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'BELNR',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'EDATU',
    LC_SORT3 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBELN',
    LC_SORT4 TYPE  LVC_S_SORT-FIELDNAME VALUE 'POSNR'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

* Sort by Pay-in no (FI doc)
*Del- 420000631 >>>
*  CLEAR LS_SORT.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO PT_SORT.
*<<<Del- 420000631

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT4.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_REP
*----------------------------------------------------------------------*
*  Hot Spot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_REP  USING  US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                                 US_COLUMN_ID TYPE LVC_S_COL.

* Read Line
  READ TABLE GT_RESULT2 ASSIGNING FIELD-SYMBOL(<L_RESULT2>)
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'VBELN'.
      PERFORM F_DISPLAY_SO USING  <L_RESULT2>-VBELN.
    WHEN 'VBELN_VF'.
      PERFORM F_DISPLAY_BILL USING  <L_RESULT2>-VBELN_VF.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_TOP_OF_PAGE_REP
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_REP  USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT.

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LF_TEXT1     TYPE  TEXT50,
    LF_TEXT2     TYPE  TEXT50,
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
* Text-h11 : Report:
  LF_TEXT = TEXT-H11.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
* Text-h13 : Advance Receive Report
  LF_TEXT = TEXT-H13.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h12 : Date/Time:
  LF_TEXT = TEXT-H12.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE SY-DATUM TO LF_TEXT1.
  WRITE SY-UZEIT TO LF_TEXT2.
  LF_TEXT = |{ LF_TEXT1 }/{ LF_TEXT2 }|.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PRINT_TOP_OF_PAGE_REP
*----------------------------------------------------------------------*
*  Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_REP .

  DATA:
    LF_COL01 TYPE  I VALUE 25,
    LF_COL02 TYPE  I VALUE 35,
    LF_TEXT1 TYPE  TEXT50,
    LF_TEXT2 TYPE  TEXT50,
    LF_TEXT  TYPE  TEXT50.


* Text-h11 : Report:
* Text-h13 : Advance Receive Report
  WRITE AT: /1(LF_COL01)  TEXT-H11 INTENSIFIED ON NO-GAP,
            (LF_COL02)    TEXT-H13 NO-GAP.

* Text-h12 : Date/Time:
  WRITE SY-DATUM TO LF_TEXT1.
  WRITE SY-UZEIT TO LF_TEXT2.
  LF_TEXT = |{ LF_TEXT1 }/{ LF_TEXT2 }|.
  WRITE AT: /1(LF_COL01)  TEXT-H12 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP.

ENDFORM.
*----------------------------------------------------------------------*
*       Module  STATUS_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE STATUS_ALV OUTPUT.

* Check ALV 1 in Edit Mode?
  IF GS_LAYOUT_1-EDIT EQ SPACE.
    READ TABLE GT_FIELDCAT_1 TRANSPORTING NO FIELDS
                             WITH KEY EDIT = 'X'.
    IF SY-SUBRC NE 0.
      APPEND GC_SAVE_1 TO GT_EXCL.
    ENDIF.
  ENDIF.

  SET PF-STATUS '9000' EXCLUDING GT_EXCL ##STAT_UNDEF.
  SET TITLEBAR  '9000' WITH GF_TITLE.

ENDMODULE.                 " STATUS_0100_1  OUTPUTENDMODULE.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_BILL
*----------------------------------------------------------------------*
*  Display Billing Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_BILL  USING  UF_VBELN  TYPE  VBRK-VBELN.

  IF UF_VBELN IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'VF' FIELD UF_VBELN.
  CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.         "#EC CI_CALLTA

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_QTY_CONFIRMATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_QTY_CONFIRMATION.

  DATA :  S_VBELN TYPE RANGE OF VBAK-VBELN.

  READ TABLE GT_RESULT1 INTO DATA(LS_RESULT) INDEX 1.
  IF SY-SUBRC EQ 0.
    S_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = LS_RESULT-VBELN ) ).
    SUBMIT ZSDSSDR0560 USING SELECTION-SCREEN  1000
                            WITH S_VBELN IN S_VBELN
                            AND RETURN."VIA SELECTION-SCREEN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONTROL_FLAG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_CONTROL_FLAG.

  DATA : BEGIN OF LS_VBEP,
           VBELN TYPE VBEP-VBELN,
           POSNR TYPE VBEP-POSNR,
*           ETENR TYPE VBEP-ETENR,
         END OF LS_VBEP.
  DATA : LT_VBEP LIKE HASHED TABLE OF LS_VBEP WITH UNIQUE KEY VBELN
                                                              POSNR.
*                                                              ETENR.

  LT_VBEP =  CORRESPONDING #( GT_RESULT1  DISCARDING DUPLICATES ).


  SELECT COUNT( * )
    FROM @LT_VBEP AS A
*    INNER JOIN vbep on A~VBELN eq vbep~VBELN AND
*                       A~POSNR eq vbep~POSNR AND
*                       A~ETENR eq vbep~ETENR
    INNER JOIN VBAP ON A~VBELN EQ VBAP~VBELN AND
                       A~POSNR EQ VBAP~POSNR
    INNER JOIN ZSDSSDC030 ON  VBAP~MATNR           EQ ZSDSSDC030~MATNR AND
                              ZSDSSDC030~DAY_FIRST LE @SY-DATUM AND
                              ZSDSSDC030~DAY_LAST  GE @SY-DATUM.
  IF SY-SUBRC EQ 0.
    GF_CONTROL = ABAP_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BOTTON_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_BOTTON_NAME .
  BUT_UPS = 'Manage Product Control'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CAL_PRODUCT_CONTROL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CAL_PRODUCT_CONTROL .
  SUBMIT ZSDSSDR0560 USING SELECTION-SCREEN  1000
                          AND RETURN VIA SELECTION-SCREEN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CALL_CHECK_CONTROL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CALL_CHECK_CONTROL USING LV_VBLEN.
  DATA : S_VBELN TYPE RANGE OF VBAK-VBELN.

  S_VBELN[] =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = LV_VBLEN ) ).

  SUBMIT ZSDSSDR0560 USING SELECTION-SCREEN  1000
                          WITH S_VBELN IN S_VBELN[]
                          WITH P_EXT   EQ ABAP_TRUE
                          AND RETURN.
ENDFORM.
