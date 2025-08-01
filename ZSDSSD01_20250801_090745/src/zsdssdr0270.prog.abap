*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0270
*  Creation Date      : 23.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : SDE033
*  Description        : Register Warranty Master
*  Purpose            : To insert warranty master table ZSDSCMT003 on
*                       goods issue date from delivery order
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0270.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  EQUI.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSSDS058.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT  TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_KEY1,
         MATNR TYPE  MARA-MATNR,
       END OF TS_KEY1.
TYPES: TT_KEY1  TYPE  SORTED TABLE OF TS_KEY1
                        WITH UNIQUE KEY MATNR.

TYPES: BEGIN OF TS_JOB,
         MBLNR TYPE  MKPF-MBLNR,
         MJAHR TYPE  MKPF-MJAHR,
         EQUNR TYPE  RANGE OF EQUI-EQUNR,
       END OF TS_JOB.
TYPES: TT_JOB  TYPE  SORTED TABLE OF TS_JOB
                     WITH UNIQUE KEY MBLNR
                                     MJAHR.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE      TYPE  CHAR1       VALUE 'X',
  GC_TCODE     TYPE  SY-TCODE    VALUE 'ZSDSSD022',

  GC_LED_GREEN TYPE  ICON-ID VALUE '@5B@',
  GC_LED_RED   TYPE  ICON-ID VALUE '@5C@'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE  TT_RESULT                                   ##NEEDED,
  GT_JOB    TYPE  TT_JOB                                      ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

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
DATA:
  GF_DESCP_STDWRT TYPE  ZSDSPSC002-DESCP                      ##NEEDED,
  GF_DESCP_EXTWRT TYPE  ZSDSPSC002-DESCP                      ##NEEDED,
  GR_VKORG        TYPE  RANGE OF VBAK-VKORG                   ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS058'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 24,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 76.

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

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01 NO INTERVALS.
  SELECT-OPTIONS:
    S_EQUNR  FOR EQUI-EQUNR OBLIGATORY.
  PARAMETERS:
    CB_TEST  TYPE  CHAR1 AS CHECKBOX DEFAULT 'X'.
  PARAMETERS:
    P_MBLNR TYPE  MKPF-MBLNR NO-DISPLAY,
    P_MJAHR TYPE  MKPF-MJAHR NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT.
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

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_DESCP_STDWRT TYPE  ZSDSDE_PARAM_NAME VALUE 'STD_WRT_DESCP',
    LC_DESCP_EXTWRT TYPE  ZSDSDE_PARAM_NAME VALUE 'EXT_WRT_DESCP',
    LC_ACTIVE_VKORG TYPE  ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_SALESORG'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GF_DESCP_STDWRT,
         GF_DESCP_EXTWRT,
         GR_VKORG.

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
*     Description of Standard Warranty WBS
*     ------------------------------------
      WHEN LC_DESCP_STDWRT.
        GF_DESCP_STDWRT = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     Description of Extended Warranty WBS
*     ------------------------------------
      WHEN LC_DESCP_EXTWRT.
        GF_DESCP_EXTWRT = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     Activated Sales Org
*     ------------------------------------
      WHEN LC_ACTIVE_VKORG.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
                     INTO TABLE GR_VKORG.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection Screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN  ##NEEDED.
* Validate Selection Screen here....
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PROCESS_DATA
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  TYPES: BEGIN OF TS_REVS,
           EQUNR TYPE  ZSDSCMT003-EQUNR,
           MATNR TYPE  ZSDSCMT003-MATNR,
           SERNR TYPE  ZSDSCMT003-SERNR,
         END OF TS_REVS.
  TYPES: TT_REVS TYPE STANDARD TABLE OF TS_REVS.

  CONSTANTS:
    LC_SER03  TYPE  OBJK-TASER VALUE 'SER03'.

  DATA:
    LT_REVS   TYPE  TT_REVS.

  DATA:
    LS_REVS   TYPE  TS_REVS,
    LS_INPUT  TYPE  ZCL_SDSCM_WARRANTY_UTIL=>TS_REGISTER_INFO,
    LS_RETURN TYPE BAPIRET1,
    LS_RESULT TYPE TS_RESULT.

  DATA:
    LF_DATUM TYPE  SY-DATUM,
    LF_MONTH TYPE  I.


* Initialize Output
  CLEAR: CT_RESULT.

* Wait Until Matdoc found
  IF P_MBLNR IS NOT INITIAL AND
     P_MJAHR IS NOT INITIAL.
*   Wait up to 1 minutes
    DO 60 TIMES ##NUMBER_OK.
      SELECT SINGLE MBLNR
        FROM MKPF
       WHERE MBLNR EQ @P_MBLNR
         AND MJAHR EQ @P_MJAHR
        INTO @DATA(LF_MBLNR).
      IF SY-SUBRC EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.
    IF LF_MBLNR IS INITIAL.
*     Text-e01: Cannot find material document
      MESSAGE E000(ZSDSSD01) WITH TEXT-E01 P_MBLNR P_MJAHR SPACE.
      RETURN.
    ENDIF.
  ENDIF.

* Get Equipment
  SELECT A~EQUNR,
         A~MATNR,
         A~SERNR,
         E~ZZWARRANTY_L AS WRTLT_FLAG,
         D~VBELN        AS VBELN_VL,
         F~FKDAT,
         F~WADAT_IST,
         E~VBELN        AS VBELN_VA,
         E~KUNNR,
         E~VKBUR,
         E~VKGRP,
         G~POSID        AS MAT_POSID,
*        Additional Info
         C~MBLNR,
         C~MJAHR,
         C~ZEILE,
         C~BWART,
         C~SHKZG,
         C~BUDAT_MKPF   AS BUDAT,
         C~CPUDT_MKPF   AS CPUDT,
         C~CPUTM_MKPF   AS CPUTM,
         C~SMBLN,
         C~SJAHR,
         C~SMBLP,
         G~POST1,
         G~PSPHI
    FROM OBJK AS A
           INNER JOIN SER03 AS B
             ON  B~OBKNR = A~OBKNR
           INNER JOIN MSEG AS C
             ON  C~MBLNR = B~MBLNR
             AND C~MJAHR = B~MJAHR
             AND C~ZEILE = B~ZEILE
           INNER JOIN LIPS AS D
             ON  D~VBELN = C~VBELN_IM
             AND D~POSNR = C~VBELP_IM
           INNER JOIN VBAK AS E
             ON  E~VBELN = D~VGBEL
           INNER JOIN LIKP AS F
             ON  F~VBELN = D~VBELN
           LEFT OUTER JOIN PRPS AS G
             ON  G~PSPNR = D~PS_PSP_PNR
   WHERE A~EQUNR IN @S_EQUNR
     AND A~TASER EQ @LC_SER03
   ORDER BY A~EQUNR ASCENDING,
            C~CPUDT_MKPF DESCENDING,
            C~CPUTM_MKPF DESCENDING
    INTO TABLE @DATA(LT_EQUI).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Remove Cancelled
  LOOP AT LT_EQUI ASSIGNING FIELD-SYMBOL(<L_EQUI>)
                  WHERE SMBLN IS NOT INITIAL.
*   Remove Reversed item
    DELETE LT_EQUI WHERE MBLNR EQ <L_EQUI>-SMBLN
                     AND MJAHR EQ <L_EQUI>-SJAHR
                     AND ZEILE EQ <L_EQUI>-SMBLP.

*   Collecting Delete Equipment
    CLEAR LS_REVS.
    LS_REVS-EQUNR = <L_EQUI>-EQUNR.
    LS_REVS-MATNR = <L_EQUI>-MATNR.
    LS_REVS-SERNR = <L_EQUI>-SERNR.
    COLLECT LS_REVS INTO LT_REVS.

*   Remove Reversing item
    DELETE LT_EQUI.
  ENDLOOP.

* Re-Insert Reversed with Blank data (To remove already registered)
  LOOP AT LT_REVS ASSIGNING FIELD-SYMBOL(<L_REVS>).
    READ TABLE LT_EQUI TRANSPORTING NO FIELDS
                       WITH KEY EQUNR = <L_REVS>-EQUNR
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      CONTINUE.
    ENDIF.
*   Append Blank data
    INSERT VALUE #( EQUNR = <L_REVS>-EQUNR
                    MATNR = <L_REVS>-MATNR
                    SERNR = <L_REVS>-SERNR )
           INTO TABLE LT_EQUI.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    SORT LT_EQUI BY EQUNR ASCENDING
                    CPUDT DESCENDING
                    CPUTM DESCENDING.
  ENDIF.

* Only Latest movement of each equipment
  DELETE ADJACENT DUPLICATES FROM LT_EQUI COMPARING EQUNR.

* Get Warranty Package
  DATA(LT_TMP) = CORRESPONDING TT_KEY1( LT_EQUI DISCARDING DUPLICATES
                                          MAPPING MATNR = MATNR ).
  SELECT A~MATNR,
         A~BEGDA,
         A~ENDDA,
         A~WRTPK,
         A~GIWRT,
         A~RGWRT
    FROM @LT_TMP AS X
           INNER JOIN ZSDSCMC003 AS A
             ON  A~MATNR = X~MATNR
    ORDER BY A~MATNR ASCENDING,
             A~BEGDA ASCENDING,
             A~ENDDA ASCENDING
    INTO TABLE @DATA(LT_WRTPK).
  IF SY-SUBRC NE 0.
    CLEAR LT_WRTPK.
  ENDIF.

  SELECT A~WRTPK,
         MAX( A~STWRT ) AS STWRT
    FROM @LT_WRTPK AS X
           INNER JOIN ZSDSCMC002 AS A
             ON  A~WRTPK = X~WRTPK
   GROUP BY A~WRTPK
    INTO TABLE @DATA(LT_WRTPK_YRS).
  IF SY-SUBRC NE 0.
    CLEAR LT_WRTPK_YRS.
  ENDIF.
  SORT LT_WRTPK_YRS BY WRTPK ASCENDING.

* Update Warranty Master
  LOOP AT LT_EQUI ASSIGNING <L_EQUI>.

    CLEAR LS_INPUT.
    LS_INPUT-EQUNR       = <L_EQUI>-EQUNR.
    LS_INPUT-MATNR       = <L_EQUI>-MATNR.
    LS_INPUT-SERNR       = <L_EQUI>-SERNR.

*   Only Already GI
    IF <L_EQUI>-SHKZG EQ 'H'.

      LS_INPUT-WRTLT_FLAG  = <L_EQUI>-WRTLT_FLAG.
      LS_INPUT-VBELN_VL    = <L_EQUI>-VBELN_VL.
      LS_INPUT-FKDAT       = <L_EQUI>-FKDAT.
      LS_INPUT-WADAT_IST   = <L_EQUI>-WADAT_IST.
      LS_INPUT-VBELN_VA    = <L_EQUI>-VBELN_VA.
      LS_INPUT-KUNNR       = <L_EQUI>-KUNNR.
      LS_INPUT-VKBUR       = <L_EQUI>-VKBUR.
      LS_INPUT-VKGRP       = <L_EQUI>-VKGRP.
      LS_INPUT-MAT_POSID   = <L_EQUI>-MAT_POSID.

      LF_DATUM = LS_INPUT-WADAT_IST.
      LOOP AT LT_WRTPK ASSIGNING FIELD-SYMBOL(<L_WRTPK>)
                       WHERE MATNR EQ LS_INPUT-MATNR
                         AND BEGDA LE LF_DATUM
                         AND ENDDA GE LF_DATUM.
        LS_INPUT-WRTPK = <L_WRTPK>-WRTPK.
        PERFORM F_CALC_PERIOD  USING  LS_INPUT-WADAT_IST
                                      <L_WRTPK>-GIWRT
                             CHANGING LS_INPUT-GI_WRT_BEG
                                      LS_INPUT-GI_WRT_END.

*       Calculate Standard warranty
        CLEAR LF_MONTH.
        READ TABLE LT_WRTPK_YRS ASSIGNING FIELD-SYMBOL(<L_WRTPK_YRS>)
                                WITH KEY WRTPK = <L_WRTPK>-WRTPK
                                BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LF_MONTH = <L_WRTPK_YRS>-STWRT * 12 ##NUMBER_OK.
        ENDIF.
        PERFORM F_CALC_PERIOD  USING  LS_INPUT-GI_WRT_BEG
                                      LF_MONTH
                             CHANGING LS_INPUT-STD_WRT_BEG
                                      LS_INPUT-STD_WRT_END.
        EXIT.
      ENDLOOP.

      IF LS_INPUT-MAT_POSID IS NOT INITIAL.
        PERFORM F_ASSIGN_STD_POSID  USING  <L_EQUI>-PSPHI
                                           <L_EQUI>-POST1
                                  CHANGING LS_INPUT-STD_POSID.
        PERFORM F_ASSIGN_EXT_POSID  USING  <L_EQUI>-PSPHI
                                           <L_EQUI>-POST1
                                  CHANGING LS_INPUT-EXT_POSID.
      ENDIF.

    ENDIF.

*   Register Warranty Master
    ZCL_SDSCM_WARRANTY_UTIL=>REGISTER_WARRANTY_PRODUCT(
      EXPORTING
        IS_INPUT  = LS_INPUT
        IF_TEST   = CB_TEST
        IF_COMMIT = GC_TRUE
      IMPORTING
        ES_RETURN = LS_RETURN ).

*   Collect result
    CLEAR LS_RESULT.

    LS_RESULT-EQUNR       = LS_INPUT-EQUNR.
    LS_RESULT-MATNR       = LS_INPUT-MATNR.
    LS_RESULT-SERNR       = LS_INPUT-SERNR.
    LS_RESULT-WRTPK       = LS_INPUT-WRTPK.
    LS_RESULT-WRTLT_FLAG  = LS_INPUT-WRTLT_FLAG.
    LS_RESULT-VBELN_VL    = LS_INPUT-VBELN_VL.
    LS_RESULT-FKDAT       = LS_INPUT-FKDAT.
    LS_RESULT-WADAT_IST   = LS_INPUT-WADAT_IST.
    LS_RESULT-VBELN_VA    = LS_INPUT-VBELN_VA.
    LS_RESULT-KUNNR       = LS_INPUT-KUNNR.
    LS_RESULT-VKBUR       = LS_INPUT-VKBUR.
    LS_RESULT-VKGRP       = LS_INPUT-VKGRP.
    LS_RESULT-GI_WRT_BEG  = LS_INPUT-GI_WRT_BEG.
    LS_RESULT-GI_WRT_END  = LS_INPUT-GI_WRT_END.
    LS_RESULT-STD_WRT_BEG = LS_INPUT-STD_WRT_BEG.
    LS_RESULT-STD_WRT_END = LS_INPUT-STD_WRT_END.
    LS_RESULT-MAT_POSID   = LS_INPUT-MAT_POSID.
    LS_RESULT-STD_POSID   = LS_INPUT-STD_POSID.
    LS_RESULT-EXT_POSID   = LS_INPUT-EXT_POSID.

    IF LS_RETURN-TYPE EQ 'S'.
      LS_RESULT-STATU    = GC_LED_GREEN.
    ELSE.
      LS_RESULT-STATU    = GC_LED_RED.
    ENDIF.
    LS_RESULT-MSGTY      = LS_RETURN-TYPE.
    LS_RESULT-MSGTX      = LS_RETURN-MESSAGE.

    INSERT LS_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.

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
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
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
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'EQUNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'MATNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'SERNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'WRTPK'.
      WHEN 'WRTLT_FLAG'.
*        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
*        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'VBELN_VL'.
      WHEN 'FKDAT'.
      WHEN 'WADAT_IST'.
      WHEN 'VBELN_VA'.
      WHEN 'KUNNR'.
      WHEN 'VKBUR'.
      WHEN 'VKGRP'.
      WHEN 'GI_WRT_BEG'.
      WHEN 'GI_WRT_END'.
      WHEN 'STD_WRT_BEG'.
      WHEN 'STD_WRT_END'.
      WHEN 'MAT_POSID'.
      WHEN 'STD_POSID'.
      WHEN 'EXT_POSID'.
      WHEN 'STATU'.
*       Text-c01 : Status
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 50 ##NUMBER_OK.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'EQUNR'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by Equipment Number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CALC_PERIOD
*----------------------------------------------------------------------*
*  Calculate Period
*----------------------------------------------------------------------*
FORM F_CALC_PERIOD  USING  UF_DATUM  TYPE  SY-DATUM
                           UF_MONTH  TYPE  I
                  CHANGING CF_BEGDA  TYPE  SY-DATUM
                           CF_ENDDA  TYPE  SY-DATUM.

  DATA:
    LF_BEGDA TYPE  SY-DATUM,
    LF_ENDDA TYPE  SY-DATUM,
    LF_MONTH TYPE  DLYMO,
    LF_YEAR  TYPE  DLYYR.


* Initialize Output
  CLEAR: CF_BEGDA,
         CF_ENDDA.

  LF_YEAR  = UF_MONTH DIV 12 ##NUMBER_OK.
  LF_MONTH = UF_MONTH MOD 12 ##NUMBER_OK.

  LF_BEGDA = UF_DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = LF_BEGDA
      DAYS      = 0
      MONTHS    = LF_MONTH
      SIGNUM    = '+'
      YEARS     = LF_YEAR
    IMPORTING
      CALC_DATE = LF_ENDDA.

* Assign Output
  CF_BEGDA = LF_BEGDA.
  IF LF_ENDDA+6(2) EQ LF_BEGDA+6(2).
    CF_ENDDA = LF_ENDDA - 1.
  ELSE.
    CF_ENDDA = LF_ENDDA.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_STD_POSID
*----------------------------------------------------------------------*
*  Get Standard Warranty WBS
*----------------------------------------------------------------------*
FORM F_ASSIGN_STD_POSID  USING  UF_PSPNR TYPE PROJ-PSPNR
                                UF_POST1 TYPE PRPS-POST1
                       CHANGING CF_STD_POSID TYPE PRPS-POSID.

  TYPES: BEGIN OF TS_STDWRT,
           PRJTY TYPE  ZSDSPSC002-PRJTY,
           ITMTY TYPE  ZSDSPSC002-ITMTY,
           ACTTY TYPE  ZSDSPSC002-ACTTY,
           DESCP TYPE  ZSDSPSC002-DESCP,
         END OF TS_STDWRT.

  STATICS:
    LS_STDWRT TYPE  TS_STDWRT.

  DATA:
    LF_PSPID  TYPE  PROJ-PSPID,
    LF_SEARCH TYPE  PRPS-POSID.


* Initialize Output
  CLEAR: CF_STD_POSID.

* Project ID
  WRITE UF_PSPNR TO LF_PSPID.

* Get Standard Warranty ItemType/ActType
  IF LS_STDWRT IS INITIAL.
    SELECT PRJTY,
           ITMTY,
           ACTTY,
           DESCP
      FROM ZSDSPSC002
     WHERE PRJTY EQ @LF_PSPID+2(1)
       AND DESCP EQ @GF_DESCP_STDWRT
     ORDER BY PRIMARY KEY
      INTO @LS_STDWRT
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

* Find Standard Warrant of FG using WBS Description
  LF_SEARCH = |{ LF_PSPID }-{ LS_STDWRT-ITMTY }-{ LS_STDWRT-ACTTY }-%|.

  SELECT POSID
    FROM PRPS
   WHERE PSPHI  EQ  @UF_PSPNR
     AND POST1  EQ  @UF_POST1
     AND POSID LIKE @LF_SEARCH
   ORDER BY POSID ASCENDING
    INTO @CF_STD_POSID
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_REGISTER_WARRANTY
*----------------------------------------------------------------------*
*  Register Warranty Master
*----------------------------------------------------------------------*
FORM F_REGISTER_WARRANTY  USING  UT_XMKPF TYPE TY_T_MKPF
                                 UT_XMSEG TYPE TY_T_MSEG.

  DATA:
    LT_SERNO TYPE STANDARD TABLE OF RSEROB,
    LT_EQUNR TYPE RANGE OF EQUI-EQUNR.

  DATA:
    LS_KEY  TYPE  RSEROB.

  DATA:
    LF_FNAME TYPE  CHAR40 VALUE '(SAPMV50A)LIKP-VKORG'.


* Initialize Data
  CLEAR: GT_JOB.

* Get Constants
  PERFORM F_GET_CONSTANTS.

  IF GR_VKORG IS INITIAL.
    RETURN.
  ENDIF.

* Collect Equipment By Material Document
  LOOP AT UT_XMKPF ASSIGNING FIELD-SYMBOL(<L_XMKPF>).

    CLEAR: LS_KEY,
           LT_SERNO.

*   Check Activation
    SELECT SINGLE VKORG
      FROM LIKP
     WHERE VBELN EQ @<L_XMKPF>-LE_VBELN
      INTO @DATA(LF_VKORG).
    IF SY-SUBRC NE 0.
*     Read from Memory
      ASSIGN (LF_FNAME) TO FIELD-SYMBOL(<LF_VKORG>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      LF_VKORG = <LF_VKORG>.
    ENDIF.

    IF NOT LF_VKORG IN GR_VKORG.
      CONTINUE.
    ENDIF.

    LS_KEY-TASER = 'SER03'.

*   Use Reversed doc if found
    LOOP AT UT_XMSEG ASSIGNING FIELD-SYMBOL(<L_XMSEG>)
                     WHERE SMBLN IS NOT INITIAL.
      LS_KEY-MBLNR = <L_XMSEG>-SMBLN.
      LS_KEY-MJAHR = <L_XMSEG>-SJAHR.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      LS_KEY-MBLNR = <L_XMKPF>-MBLNR.
      LS_KEY-MJAHR = <L_XMKPF>-MJAHR.
    ENDIF.

    CALL FUNCTION 'GET_SERNOS_OF_DOCUMENT'
      EXPORTING
        KEY_DATA            = LS_KEY
      TABLES
        SERNOS              = LT_SERNO
      EXCEPTIONS
        KEY_PARAMETER_ERROR = 1
        NO_SUPPORTED_ACCESS = 2
        NO_DATA_FOUND       = 3
        OTHERS              = 4.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

*   Collect Equipment No
    CLEAR LT_EQUNR.
    LOOP AT LT_SERNO ASSIGNING FIELD-SYMBOL(<L_SERNO>).
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW    = <L_SERNO>-EQUNR )
                    INTO TABLE LT_EQUNR.
    ENDLOOP.

*   Collect Job
    INSERT VALUE #( MBLNR = <L_XMKPF>-MBLNR
                    MJAHR = <L_XMKPF>-MJAHR
                    EQUNR = LT_EQUNR )
                 INTO TABLE GT_JOB.

  ENDLOOP.

* Create Job to Register warranty for equipment
  PERFORM F_CREATE_BG_JOB ON COMMIT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_BG_JOB
*----------------------------------------------------------------------*
*  Create Background job to execute the program
*----------------------------------------------------------------------*
FORM F_CREATE_BG_JOB.

  DATA:
    LF_JOBNAME  TYPE  TBTCJOB-JOBNAME,
    LF_JOBCOUNT TYPE  TBTCJOB-JOBCOUNT.


* Only when Criteria exist
  IF GT_JOB IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT GT_JOB ASSIGNING FIELD-SYMBOL(<L_JOB>).

*   Assign Job Name
    LF_JOBNAME = |ZSDSSD_REGWRT_{ <L_JOB>-MBLNR }|.

*   Create Job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LF_JOBNAME
      IMPORTING
        JOBCOUNT         = LF_JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

*   Submit Program
    SUBMIT ZSDSSDR0270 AND RETURN                        "#EC CI_SUBMIT
                       WITH S_EQUNR IN <L_JOB>-EQUNR
                       WITH CB_TEST EQ SPACE
                       WITH P_MBLNR EQ <L_JOB>-MBLNR
                       WITH P_MJAHR EQ <L_JOB>-MJAHR
                       VIA JOB LF_JOBNAME
                       NUMBER LF_JOBCOUNT.

*   Close Job
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LF_JOBCOUNT
        JOBNAME              = LF_JOBNAME
        STRTIMMED            = 'X'
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        INVALID_TARGET       = 8
        INVALID_TIME_ZONE    = 9
        OTHERS               = 10.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_EXT_POSID
*----------------------------------------------------------------------*
*  Get Extended Warranty WBS
*----------------------------------------------------------------------*
FORM F_ASSIGN_EXT_POSID  USING  UF_PSPNR TYPE PROJ-PSPNR
                                UF_POST1 TYPE PRPS-POST1
                       CHANGING CF_EXT_POSID TYPE PRPS-POSID.

  TYPES: BEGIN OF TS_EXTWRT,
           PRJTY TYPE  ZSDSPSC002-PRJTY,
           ITMTY TYPE  ZSDSPSC002-ITMTY,
           ACTTY TYPE  ZSDSPSC002-ACTTY,
           DESCP TYPE  ZSDSPSC002-DESCP,
         END OF TS_EXTWRT.

  STATICS:
    LS_EXTWRT TYPE  TS_EXTWRT.

  DATA:
    LF_PSPID  TYPE  PROJ-PSPID,
    LF_SEARCH TYPE  PRPS-POSID.


* Initialize Output
  CLEAR: CF_EXT_POSID.

* Project ID
  WRITE UF_PSPNR TO LF_PSPID.

* Get Standard Warranty ItemType/ActType
  IF LS_EXTWRT IS INITIAL.
    SELECT PRJTY,
           ITMTY,
           ACTTY,
           DESCP
      FROM ZSDSPSC002
     WHERE PRJTY EQ @LF_PSPID+2(1)
       AND DESCP EQ @GF_DESCP_EXTWRT
     ORDER BY PRIMARY KEY
      INTO @LS_EXTWRT
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

* Find Standard Warrant of FG using WBS Description
  LF_SEARCH = |{ LF_PSPID }-{ LS_EXTWRT-ITMTY }-{ LS_EXTWRT-ACTTY }-%|.

  SELECT POSID
    FROM PRPS
   WHERE PSPHI  EQ  @UF_PSPNR
     AND POST1  EQ  @UF_POST1
     AND POSID LIKE @LF_SEARCH
   ORDER BY POSID ASCENDING
    INTO @CF_EXT_POSID
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.
