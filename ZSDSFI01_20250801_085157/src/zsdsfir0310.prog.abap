*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0310
*  Creation Date      : 14.06.2024
*  Author             : Suthichai P.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program download AR invoice to bank agent
*  Purpose            : Program download AR invoice to bank agent
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0310.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES :
  P0001,
  SSCRFIELDS,
  VBAK,
  VBKD,
  ZSDSFIT029.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES :
  BEGIN OF TS_RESULT.
    INCLUDE STRUCTURE ZSDSFIT029.
TYPES :
    PERNM            TYPE TEXT100,
    CNAMT            TYPE TEXT180,
    CNAMT2           TYPE TEXT180,
    CNAME            TYPE TEXT180,
    VTWEG            TYPE VTWEG,
    KUNNE            TYPE VBPA-KUNNR,
    VKBUR            TYPE VBAK-VKBUR,
    VKGRP            TYPE VBAK-VKGRP,
    POST1            TYPE PS_POST1,
    BSTKD            TYPE VBKD-BSTKD,
    ZTERM            TYPE VBKD-ZTERM,
    KTEXT            TYPE CEPCT-KTEXT,
    BANKK2           TYPE BANKK,
    ZBLLCYL          TYPE ZSDSFIC024-ZBLLCYL,
    ZBLLCOL          TYPE ZSDSFIC024-ZBLLCOL,
    ZFILE            TYPE FILENAME_AL11,
    ZSTAT            TYPE ZSDSDE_STATUS_EXP,
    VTWET            TYPE TVTWT-VTEXT,
    VKGRT            TYPE TVGRT-BEZEI,
    VKBUT            TYPE TVKBT-BEZEI,
    SPTXT            TYPE TEXT100,
    WORK_DATE_TX     TYPE CHAR10,
    BLDAT_TX         TYPE CHAR10,
    FAEDT_TX         TYPE CHAR10,
    PAYMENT_DATE_TX  TYPE CHAR10,
    FOLLOW_DATE_TX   TYPE CHAR10,
    RECEIVED_DATE_TX TYPE CHAR10,
    BANK_DATE_TX     TYPE CHAR10,
    ZSEQ             TYPE ZSDSDE_FI_SEQ,
    ZSEL             TYPE CHAR1,
  END OF TS_RESULT.

TYPES: TT_RESULT  TYPE  STANDARD TABLE OF TS_RESULT.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE    TYPE CHAR1     VALUE 'X',
  GC_EXP_XLS TYPE SY-UCOMM  VALUE 'EXP_XLS',                "#EC NEEDED
  GC_SALL    TYPE SY-UCOMM  VALUE 'SALL',                   "#EC NEEDED
  GC_UALL    TYPE SY-UCOMM  VALUE 'UALL',                   "#EC NEEDED
  GC_TCODE   TYPE SY-TCODE  VALUE 'ZSDSFI021'.

CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSFIT029'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 10,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 90.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  GF_DOWNLOAD_XLS  TYPE  FLAG                               ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA :
  GRT_STATUS        TYPE RANGE OF ZSDSFIT029-STATUS         ##NEEDED,
  GRT_ACTIVITY_TYPE TYPE RANGE OF ZSDSFIT029-ACTION_TYPE    ##NEEDED.
*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

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

DEFINE MC_DATE_TX.
  IF &1 <> '00000000'.
    WRITE &1 TO &2.
  ENDIF.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s00: Mode
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S00.
  PARAMETERS :
    RB_REP RADIOBUTTON GROUP GR1 DEFAULT 'X' USER-COMMAND CM
                       MODIF ID M0,
    RB_UNB RADIOBUTTON GROUP GR1 MODIF ID M0.
SELECTION-SCREEN END OF BLOCK B0.
* Text-s04: Type of Report
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-S04.
  PARAMETERS :
    RB_AR RADIOBUTTON GROUP GR2 DEFAULT 'X',
    RB_ME RADIOBUTTON GROUP GR2.

  PARAMETERS :
    CB_SEND AS CHECKBOX MODIF ID M1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B4.
* Text-s01: Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    P_BUKRS TYPE T001-BUKRS DEFAULT '1000' MODIF ID M1,
    P_PERNR TYPE P0001-PERNR MODIF ID M1 NO-DISPLAY ##NEEDED
                             MATCHCODE OBJECT ZSDSH_PERNR.
  SELECT-OPTIONS:
    S_PERNR FOR  P0001-PERNR MODIF ID M1
                 MATCHCODE OBJECT ZSDSH_PERNR,
    S_WRKD  FOR  ZSDSFIT029-WORK_DATE MODIF ID M1,
    S_ACTY  FOR  ZSDSFIT029-ACTION_TYPE MODIF ID M1,
    S_STAT  FOR  ZSDSFIT029-STATUS MODIF ID M1.
SELECTION-SCREEN END OF BLOCK B1.
* Text-s02: Select Option
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  SELECT-OPTIONS:
    S_KUNNR FOR  ZSDSFIT029-KUNNR MODIF ID M1,
    S_GJAHR FOR  ZSDSFIT029-GJAHR MODIF ID M1,
    S_BELNR FOR  ZSDSFIT029-BELNR MODIF ID M1,
    S_BLDAT FOR  ZSDSFIT029-BLDAT MODIF ID M1,
    S_XBLNR FOR  ZSDSFIT029-XBLNR MODIF ID M1,
    S_BPLNO FOR  ZSDSFIT029-BILLPL_NO MODIF ID M1,
    S_BPLDA FOR  ZSDSFIT029-BILLPL_DATE MODIF ID M1,
    S_PMETD FOR  ZSDSFIT029-PYMT_METHOD MODIF ID M1,
    S_ZTERM FOR  VBKD-ZTERM MODIF ID M1,
    S_PAYNO FOR  ZSDSFIT029-PAYNO MODIF ID M1,
    S_FOLDA FOR  ZSDSFIT029-FOLLOW_DATE MODIF ID M1,
    S_BRNCH FOR  ZSDSFIT029-BRNCH MODIF ID M1,
    S_TRANS FOR  ZSDSFIT029-TRANF_NO MODIF ID M1,
    S_CHEQ  FOR  ZSDSFIT029-CHEQUE_NO MODIF ID M1,
    S_CHEQD FOR  ZSDSFIT029-CHEQUE_DATE MODIF ID M1.
SELECTION-SCREEN END OF BLOCK B2.
* Text-s03: Addition Selection
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
  SELECT-OPTIONS:
    S_VKBUR FOR  VBAK-VKBUR MODIF ID M1,
    S_VKGRP FOR  VBAK-VKGRP MODIF ID M1,
    S_VBELN FOR  VBAK-VBELN MODIF ID M1.
SELECTION-SCREEN END OF BLOCK B3.
* Text-s01: Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    P_BUKR2 TYPE T001-BUKRS DEFAULT '1000' MODIF ID M2.
  SELECT-OPTIONS:
    S_BPLN2 FOR  ZSDSFIT029-BILLPL_NO MODIF ID M2,
    S_BPLD2 FOR  ZSDSFIT029-BILLPL_DATE MODIF ID M2,
    S_GJAH2 FOR  ZSDSFIT029-GJAHR MODIF ID M2,
    S_BELN2 FOR  ZSDSFIT029-BELNR MODIF ID M2.
SELECTION-SCREEN END OF BLOCK B5.

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
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_HIDE_SCREEN.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CASE GC_TRUE.
    WHEN RB_REP.
      PERFORM F_GET_DATA.
    WHEN RB_UNB.
      PERFORM F_UNBLOCK_DOWNLOAD.
  ENDCASE.

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
    LC_PARAM_STATUS   TYPE  ZSDSDE_PARAM_NAME VALUE 'STATUS',
    LC_PARAM_ACT_TYPE TYPE  ZSDSDE_PARAM_NAME VALUE 'ACTIVITY_TYPE'.

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
  CLEAR:
    GRT_STATUS,
    GRT_ACTIVITY_TYPE.

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
      WHEN LC_PARAM_ACT_TYPE.
*       <Fill logic to assign value to GenC Constant>
        INSERT VALUE #(
          SIGN    = <L_GENC>-PARAM_SIGN
          OPTION  = <L_GENC>-PARAM_OPTION
          LOW     = <L_GENC>-VALUE_LOW
          HIGH    = <L_GENC>-VALUE_HIGH ) INTO TABLE GRT_ACTIVITY_TYPE.

      WHEN LC_PARAM_STATUS.
*       <Fill logic to assign value to GenC Constant>
        INSERT VALUE #(
          SIGN    = <L_GENC>-PARAM_SIGN
          OPTION  = <L_GENC>-PARAM_OPTION
          LOW     = <L_GENC>-VALUE_LOW
          HIGH    = <L_GENC>-VALUE_HIGH ) INTO TABLE GRT_STATUS.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& Get Data
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA .

  DATA :
    LRT_STATUS        TYPE RANGE OF ZSDSFIT029-STATUS,
    LRT_ACTIVITY_TYPE TYPE RANGE OF ZSDSFIT029-ACTION_TYPE,
    LT_RETURN         TYPE STANDARD TABLE OF BAPIRET2,
    LS_ADDRESS        TYPE BAPIADDR3,
    LF_DATA_TYPE      TYPE ZSDSFIT029-DATA_TYPE,
    LF_SEQNO          TYPE ZSDSDE_FI_SEQ.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      USERNAME = SY-UNAME
    IMPORTING
      ADDRESS  = LS_ADDRESS
    TABLES
      RETURN   = LT_RETURN.

  CASE ABAP_TRUE.
    WHEN RB_AR.
      LF_DATA_TYPE = ''.
    WHEN RB_ME.
      LF_DATA_TYPE = 'M'.
  ENDCASE.

  PERFORM F_SET_FILTER
    CHANGING LRT_STATUS LRT_ACTIVITY_TYPE.

  SELECT FROM ZSDSFIT029 AS Z
    LEFT OUTER JOIN CEPCT AS PC                        "#EC CI_BUFFJOIN
    ON Z~PRCTR EQ PC~PRCTR
    AND PC~SPRAS EQ 'E'
    LEFT OUTER JOIN ZSDSFIC024 AS Z24
    ON Z~KUNNR EQ Z24~KUNNR
    LEFT OUTER JOIN CVI_CUST_LINK AS CL
    ON Z~KUNNR EQ CL~CUSTOMER
    LEFT OUTER JOIN BUT000 AS BP
    ON CL~PARTNER_GUID EQ BP~PARTNER_GUID
    LEFT OUTER JOIN TSAD3T AS TT
    ON BP~TITLE EQ TT~TITLE
    AND TT~LANGU EQ 'E'
    LEFT OUTER JOIN ZSDSFIT039 AS L                    "#EC CI_BUFFJOIN
    ON Z~BILLPL_NO EQ L~BILLPL_NO
    AND Z~BILLPL_DATE EQ L~BILLPL_DATE
    AND Z~BUKRS EQ L~BUKRS
    AND Z~BELNR EQ L~BELNR
    AND Z~GJAHR EQ L~GJAHR
    FIELDS Z~* , PC~KTEXT AS KTEXT,
      CONCAT_WITH_SPACE( BP~NAME_ORG1 ,
        CONCAT_WITH_SPACE( BP~NAME_ORG2 ,
          CONCAT_WITH_SPACE( BP~NAME_ORG3 , BP~NAME_ORG4 , 1 ) , 1 ) , 1 )
      AS CNAMT,
      CONCAT_WITH_SPACE( TT~TITLE_MEDI ,
        CONCAT_WITH_SPACE( BP~NAME_FIRST , BP~NAME_LAST , 1 ) , 1 )
      AS CNAMT2,
      Z24~ZBLLCYL , Z24~ZBLLCOL ,
      L~FILENAME AS ZFILE,
      L~STATU AS ZSTAT
    WHERE Z~BUKRS       EQ @P_BUKRS
    AND   Z~DATA_TYPE   EQ @LF_DATA_TYPE
    AND   Z~PERNR       IN @S_PERNR
    AND   Z~WORK_DATE   IN @S_WRKD
    AND   Z~STATUS      IN @LRT_STATUS
    AND   Z~ACTION_TYPE IN @LRT_ACTIVITY_TYPE
    AND   Z~KUNNR       IN @S_KUNNR
    AND   Z~GJAHR       IN @S_GJAHR
    AND   Z~BELNR       IN @S_BELNR
    AND   Z~BLDAT       IN @S_BLDAT
    AND   Z~XBLNR       IN @S_XBLNR
    AND   Z~BILLPL_NO   IN @S_BPLNO
    AND   Z~BILLPL_DATE IN @S_BPLDA
    AND   Z~PYMT_METHOD IN @S_PMETD
    AND   Z~PAYNO       IN @S_PAYNO
    AND   Z~FOLLOW_DATE IN @S_FOLDA
    AND   Z~BRNCH       IN @S_BRNCH
    AND   Z~TRANF_NO    IN @S_TRANS
    AND   Z~CHEQUE_NO   IN @S_CHEQ
    AND   Z~CHEQUE_DATE IN @S_CHEQD
    AND   Z~VBELN       IN @S_VBELN
    AND   Z~DELETE_FLAG EQ @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_RESULT ##TOO_MANY_ITAB_FIELDS.

*  IF CB_SEND EQ 'X'.
*    DELETE GT_RESULT WHERE ZSTAT EQ 'X'.
*  ENDIF.

  IF GT_RESULT[] IS INITIAL.
    RETURN.
  ENDIF.

  SELECT FROM VBRP AS IV                           "#EC CI_NO_TRANSFORM
    LEFT OUTER JOIN VBAK AS SO
    ON IV~AUBEL EQ SO~VBELN
    LEFT OUTER JOIN VBAP AS SI
    ON SO~VBELN EQ SI~VBELN
    AND IV~AUPOS EQ SI~POSNR
    LEFT OUTER JOIN VBKD AS SB
    ON SO~VBELN EQ SB~VBELN
    AND SB~POSNR EQ '000000'
    LEFT OUTER JOIN VBPA AS SP
    ON SO~VBELN EQ SP~VBELN
    AND SP~PARVW EQ 'VE'
    LEFT OUTER JOIN BUT000 AS BP
    ON SP~ASSIGNED_BP EQ BP~PARTNER
    LEFT OUTER JOIN PRPS AS WBS
    ON SI~PS_PSP_PNR = WBS~PSPNR
    LEFT OUTER JOIN PROJ AS PJ
    ON WBS~PSPHI EQ PJ~PSPNR
    LEFT OUTER JOIN CEPCT AS PC                        "#EC CI_BUFFJOIN
    ON SI~PRCTR EQ PC~PRCTR
    AND PC~SPRAS EQ 'E'
    LEFT OUTER JOIN TVTWT AS SS
    ON SO~VTWEG EQ SS~VTWEG
    AND SS~SPRAS EQ 'E'
    LEFT OUTER JOIN TVGRT AS SG
    ON SO~VKGRP EQ SG~VKGRP
    AND SG~SPRAS EQ 'E'
    LEFT OUTER JOIN TVKBT AS SF
    ON SO~VKBUR EQ SF~VKBUR
    AND SF~SPRAS EQ 'E'
    FIELDS  IV~VBELN AS VBELN_VF,
            SO~VTWEG AS VTWEG,
            SP~PERNR AS KUNNE,
            SO~VKBUR AS VKBUR,
            SO~VKGRP AS VKGRP,
            PJ~POST1 AS POST1,
            SB~BSTKD AS BSTKD,
            SB~ZTERM AS ZTERM,
            PC~KTEXT AS KTEXT,
            IV~AUBEL AS VBELN,
            SI~PRCTR AS PRCTR,
            SS~VTEXT AS VTWET,
            SG~BEZEI AS VKGRT,
            SF~BEZEI AS VKBUT,
            BP~NAME_FIRST AS SP_FIRST,
            BP~NAME_LAST  AS SP_LAST
    FOR ALL ENTRIES IN @GT_RESULT
    WHERE IV~VBELN EQ @GT_RESULT-VBELN_VF
    AND   IV~UEPOS  EQ '000000'
    INTO TABLE @DATA(LT_INV).
  IF SY-SUBRC EQ 0.
    SORT LT_INV BY VBELN_VF.
  ENDIF.

  DATA(LT_KUNNR) = GT_RESULT[].
  SORT LT_KUNNR BY KUNNR.
  DELETE ADJACENT DUPLICATES FROM LT_KUNNR COMPARING KUNNR.
  IF LT_KUNNR IS NOT INITIAL.
    SELECT FROM BUT020 AS BP
      INNER JOIN ADRC AS BE
      ON BP~ADDRNUMBER EQ BE~ADDRNUMBER
      AND BE~NATION = 'I'
      FIELDS BP~PARTNER , BE~NAME1 , BE~NAME2 , BE~NAME3 , BE~NAME4
      FOR ALL ENTRIES IN @LT_KUNNR
      WHERE BP~PARTNER EQ @LT_KUNNR-KUNNR
      INTO TABLE @DATA(LT_NAME_ENG).
    IF SY-SUBRC EQ 0.
      SORT LT_NAME_ENG BY PARTNER.
    ENDIF.
  ENDIF.

  DATA(LT_PERNR) = GT_RESULT[].
  SORT LT_PERNR BY PERNR.
  DELETE ADJACENT DUPLICATES FROM LT_PERNR COMPARING PERNR.
  IF LT_PERNR IS NOT INITIAL.
    SELECT FROM PA0002
      FIELDS PERNR , VORNA , NACHN
      FOR ALL ENTRIES IN @LT_PERNR
      WHERE PERNR EQ @LT_PERNR-PERNR
      AND   ENDDA >= @SY-DATLO
      AND   BEGDA <= @SY-DATLO
      INTO TABLE @DATA(LT_PA0002).
    IF SY-SUBRC EQ 0.
      SORT LT_PA0002 BY PERNR.
    ENDIF.
  ENDIF.

  CLEAR LF_SEQNO.
  LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>).

    READ TABLE LT_INV ASSIGNING FIELD-SYMBOL(<LFS_INV>)
      WITH KEY VBELN_VF = <LFS_RESULT>-VBELN_VF
      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING <LFS_INV> TO <LFS_RESULT>.

      IF <LFS_INV>-POST1 IS INITIAL.
        SELECT FROM VBRP AS VP
          INNER JOIN PRPS AS WBS
          ON VP~PS_PSP_PNR = WBS~PSPNR
          INNER JOIN PROJ AS PJ
          ON WBS~PSPHI EQ PJ~PSPNR
          FIELDS PJ~POST1
          WHERE VP~VBELN EQ @<LFS_RESULT>-VBELN_VF
          INTO TABLE @DATA(LT_POST1).
        READ TABLE LT_POST1                             "#EC CI_NOORDER
          ASSIGNING FIELD-SYMBOL(<LFS_POST1>)
          INDEX 1.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-POST1 = <LFS_POST1>-POST1.
        ENDIF.
      ENDIF.

      CONCATENATE <LFS_INV>-SP_FIRST <LFS_INV>-SP_LAST
        INTO <LFS_RESULT>-SPTXT SEPARATED BY SPACE.
    ENDIF.

    IF <LFS_RESULT>-SPTXT IS INITIAL.
      SELECT FROM VBPA AS SP
        INNER JOIN BUT000 AS BP
        ON SP~ASSIGNED_BP EQ BP~PARTNER
      FIELDS
        BP~NAME_FIRST AS SP_FIRST,
        BP~NAME_LAST  AS SP_LAST
      WHERE SP~VBELN EQ @<LFS_RESULT>-VBELN
        AND SP~PARVW EQ 'ZM'
      INTO TABLE @DATA(LT_BP_SERVICE).

      READ TABLE LT_BP_SERVICE                          "#EC CI_NOORDER
        ASSIGNING FIELD-SYMBOL(<LFS_BP_SERVICE>)
        INDEX 1.
      IF SY-SUBRC EQ 0.
        CONCATENATE <LFS_BP_SERVICE>-SP_FIRST <LFS_BP_SERVICE>-SP_LAST
          INTO <LFS_RESULT>-SPTXT SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

    IF <LFS_RESULT>-ZTERM IS INITIAL OR
       <LFS_RESULT>-PRCTR IS INITIAL OR
       <LFS_RESULT>-VKGRT IS INITIAL OR
       <LFS_RESULT>-VKBUT IS INITIAL.
      SELECT FROM VBRP AS IV                       "#EC CI_NO_TRANSFORM
        INNER JOIN VBRK AS IH
        ON IV~VBELN EQ IH~VBELN
        INNER JOIN CEPCT AS PC                         "#EC CI_BUFFJOIN
        ON IV~PRCTR EQ PC~PRCTR
        AND PC~SPRAS EQ 'E'
        INNER JOIN TVGRT AS SG
        ON IV~VKGRP EQ SG~VKGRP
        AND SG~SPRAS EQ 'E'
        INNER JOIN TVKBT AS SF
        ON IV~VKBUR EQ SF~VKBUR
        AND SF~SPRAS EQ 'E'
        FIELDS  IV~POSNR,
                IH~ZTERM,
                IV~PRCTR AS PRCTR,
                PC~KTEXT AS KTEXT,
                SG~BEZEI AS VKGRT,
                SF~BEZEI AS VKBUT
        WHERE IV~VBELN EQ @<LFS_RESULT>-VBELN "Sales Order No.
        INTO TABLE @DATA(LT_INV_SERVICE).
      READ TABLE LT_INV_SERVICE                         "#EC CI_NOORDER
        ASSIGNING FIELD-SYMBOL(<LFS_INV_SERVICE>)
        INDEX 1.
      IF SY-SUBRC EQ 0.
        IF <LFS_RESULT>-ZTERM IS INITIAL.
          <LFS_RESULT>-ZTERM = <LFS_INV_SERVICE>-ZTERM.
        ENDIF.

        IF <LFS_RESULT>-PRCTR IS INITIAL.
          <LFS_RESULT>-PRCTR = <LFS_INV_SERVICE>-PRCTR.
          <LFS_RESULT>-KTEXT = <LFS_INV_SERVICE>-KTEXT.
        ENDIF.

        IF <LFS_RESULT>-VKGRT IS INITIAL.
          <LFS_RESULT>-VKGRT = <LFS_INV_SERVICE>-VKGRT.
        ENDIF.

        IF <LFS_RESULT>-VKBUT IS INITIAL.
          <LFS_RESULT>-VKBUT = <LFS_INV_SERVICE>-VKBUT.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE FROM VBFA                                 "#EC WARNOK
      FIELDS VBELV
      WHERE VBELN EQ @<LFS_RESULT>-VBELN
      AND   VBTYP_N EQ 'EBDR'
      AND ( VBTYP_V EQ 'CSVO' OR VBTYP_V EQ 'CSCT' )
      INTO @DATA(LF_VBELN).
    IF SY-SUBRC EQ 0.
      <LFS_RESULT>-VBELN = LF_VBELN.

      SELECT SINGLE FROM CRMS4D_SERV_H                      "#EC WARNOK
        FIELDS ZZ1_CUS_PO
        WHERE ( OBJTYPE_H EQ 'BUS2000116' OR OBJTYPE_H EQ 'BUS2000112' )
        AND   OBJECT_ID EQ @LF_VBELN
        INTO @DATA(LF_CUS_PO).
      IF SY-SUBRC EQ 0.
        <LFS_RESULT>-BSTKD = LF_CUS_PO.
      ENDIF.

      SELECT FROM VBRK AS VK                                "#EC WARNOK
        INNER JOIN VBRP AS VP
        ON VK~VBELN EQ VP~VBELN
        LEFT OUTER JOIN PRPS AS WBS
        ON VP~PS_PSP_PNR = WBS~PSPNR
        LEFT OUTER JOIN PROJ AS PJ
        ON WBS~PSPHI EQ PJ~PSPNR
        FIELDS VK~VBELN , VK~FKART , VP~AUFNR , PJ~POST1
        WHERE VK~VBELN EQ @<LFS_RESULT>-VBELN_VF
        INTO TABLE @DATA(LT_PROJ).
      LOOP AT LT_PROJ ASSIGNING FIELD-SYMBOL(<LFS_PROJ>).
        CASE <LFS_PROJ>-FKART+0(2).
          WHEN 'ZV'.
            IF <LFS_PROJ>-AUFNR IS NOT INITIAL.
              <LFS_RESULT>-POST1 = <LFS_PROJ>-AUFNR.
              EXIT.                                     "#EC CI_NOORDER
            ENDIF.
          WHEN OTHERS.
            IF <LFS_PROJ>-POST1 IS NOT INITIAL.
              <LFS_RESULT>-POST1 = <LFS_PROJ>-POST1.
              EXIT.                                     "#EC CI_NOORDER
            ENDIF.
        ENDCASE.
      ENDLOOP.
      CLEAR LT_PROJ.
    ENDIF.

    IF <LFS_RESULT>-PERNR IS NOT INITIAL.
      READ TABLE LT_PA0002 ASSIGNING FIELD-SYMBOL(<LFS_PA0002>)
        WITH KEY PERNR = <LFS_RESULT>-PERNR
        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CONCATENATE
          <LFS_PA0002>-VORNA
          <LFS_PA0002>-NACHN
          INTO <LFS_RESULT>-PERNM SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

    READ TABLE LT_NAME_ENG ASSIGNING FIELD-SYMBOL(<LFS_NAME_ENG>)
      WITH KEY PARTNER = <LFS_RESULT>-KUNNR
      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      CONCATENATE
        <LFS_NAME_ENG>-NAME1
        <LFS_NAME_ENG>-NAME2
        <LFS_NAME_ENG>-NAME3
        <LFS_NAME_ENG>-NAME4
        INTO <LFS_RESULT>-CNAME SEPARATED BY SPACE.
    ENDIF.

    IF <LFS_RESULT>-PERNR IS INITIAL.
      <LFS_RESULT>-PERNM = LS_ADDRESS-FULLNAME.
    ENDIF.

    IF <LFS_RESULT>-KUNNR+0(1) EQ 'E'.
      <LFS_RESULT>-CNAMT = <LFS_RESULT>-CNAMT2.
    ENDIF.

    LF_SEQNO = LF_SEQNO + 1.
    <LFS_RESULT>-ZSEQ = LF_SEQNO.

    MC_DATE_TX :
      <LFS_RESULT>-WORK_DATE      <LFS_RESULT>-WORK_DATE_TX,
      <LFS_RESULT>-BLDAT          <LFS_RESULT>-BLDAT_TX,
      <LFS_RESULT>-FAEDT          <LFS_RESULT>-FAEDT_TX,
      <LFS_RESULT>-PAYMENT_DATE   <LFS_RESULT>-PAYMENT_DATE_TX,
      <LFS_RESULT>-FOLLOW_DATE    <LFS_RESULT>-FOLLOW_DATE_TX,
      <LFS_RESULT>-RECEIVED_DATE  <LFS_RESULT>-RECEIVED_DATE_TX,
      <LFS_RESULT>-BANK_DATE      <LFS_RESULT>-BANK_DATE_TX.

*    CASE CB_SEND.
*      WHEN ABAP_TRUE.
*        CLEAR :
*        <LFS_RESULT>-PAYMENT_DATE,
*        <LFS_RESULT>-FOLLOW_DATE,
*        <LFS_RESULT>-RECEIVED_AMT,
*        <LFS_RESULT>-RECEIVED_DATE,
*        <LFS_RESULT>-DEDUCT_AMT,
*        <LFS_RESULT>-PYMT_METHOD,
*        <LFS_RESULT>-CHEQUE_DATE,
*        <LFS_RESULT>-CHEQUE_NO,
*        <LFS_RESULT>-BANKL,
*        <LFS_RESULT>-HBKID,
*        <LFS_RESULT>-HKTID,
*        <LFS_RESULT>-BANKK,
*        <LFS_RESULT>-BANKN,
*        <LFS_RESULT>-EXPS_AMT,
*        <LFS_RESULT>-INCOME_AMT,
*        <LFS_RESULT>-WHT_AMT,
*        <LFS_RESULT>-FEE,
*        <LFS_RESULT>-RETENTION,
*        <LFS_RESULT>-CASH_CON,
*        <LFS_RESULT>-PAYNO,
*        <LFS_RESULT>-BRNCH,
*        <LFS_RESULT>-BILLPL_NO,
*        <LFS_RESULT>-ZBANK_ITEM,
*        <LFS_RESULT>-BILLPL_AMT,
*        <LFS_RESULT>-DOCUMENT_STATUS,
*        <LFS_RESULT>-INV_STATUS,
*        <LFS_RESULT>-REMARK_VENDOR,
*        <LFS_RESULT>-REASON_VENDOR.
*    ENDCASE.

  ENDLOOP.

  IF S_ZTERM[] IS NOT INITIAL.
    DELETE GT_RESULT WHERE ZTERM NOT IN S_ZTERM.
  ENDIF.

  IF S_VKBUR[] IS NOT INITIAL.
    DELETE GT_RESULT WHERE VKBUR NOT IN S_VKBUR.
  ENDIF.

  IF S_VKGRP[] IS NOT INITIAL.
    DELETE GT_RESULT WHERE VKGRP NOT IN S_VKGRP.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

  IF RB_REP <> GC_TRUE.
    RETURN.
  ENDIF.

  IF GT_RESULT[] IS INITIAL.
    MESSAGE S001(00) WITH TEXT-E02 DISPLAY LIKE 'E' ##MG_MISSING.
    RETURN.
  ENDIF.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.

* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

  PERFORM F_EXCLUDE_TOOL.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]

  GF_DOWNLOAD_XLS = ABAP_FALSE.
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.

  IF CB_SEND IS INITIAL.
    APPEND GC_EXP_XLS TO GT_EXCL.
    APPEND GC_SALL TO GT_EXCL.
    APPEND GC_UALL TO GT_EXCL.
  ENDIF.

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
  "CS_LAYOUT-SEL_MODE   = ''.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
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
    LT_FIELDCAT TYPE  LVC_T_FCAT,
    LS_FIELDCAT TYPE  LVC_S_FCAT,
    LF_COLPOS   TYPE  LVC_S_FCAT-COL_POS.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  DEFINE MC_FIELDCAT.

    READ TABLE LT_FIELDCAT ASSIGNING <L_FIELDCAT>
      WITH KEY FIELDNAME = &1.
    IF SY-SUBRC EQ 0.
      LS_FIELDCAT =  <L_FIELDCAT>.
    ELSE.
      LS_FIELDCAT-FIELDNAME = &1.
      LS_FIELDCAT-DATATYPE = &3.
    ENDIF.

    CASE LS_FIELDCAT-DATATYPE.
      WHEN 'CURR'.
        LS_FIELDCAT-DO_SUM = GC_TRUE.
        LS_FIELDCAT-NO_ZERO = GC_TRUE.
    ENDCASE.

    IF &1 EQ 'ZSEL'.
      LS_FIELDCAT-CHECKBOX  = GC_TRUE.
      LS_FIELDCAT-EDIT   = GC_TRUE.
    ENDIF.

    IF &2 IS NOT INITIAL.
      LS_FIELDCAT-SCRTEXT_S = &2.
      LS_FIELDCAT-SCRTEXT_M = &2.
      LS_FIELDCAT-SCRTEXT_L = &2.
      LS_FIELDCAT-REPTEXT = &2.
      LS_FIELDCAT-COLTEXT = &2.
      LS_FIELDCAT-COLDDICTXT = 'L'.
    ELSE.
      LS_FIELDCAT-COLDDICTXT = 'R'.
    ENDIF.

    IF &4 IS NOT INITIAL.
      LS_FIELDCAT-OUTPUTLEN = &4.
    ENDIF.

    LF_COLPOS = LF_COLPOS + 1.
    LS_FIELDCAT-COL_POS = LF_COLPOS.
    APPEND LS_FIELDCAT TO CT_FIELDCAT.
    CLEAR LS_FIELDCAT.

  END-OF-DEFINITION.

* Build Field cat from Structure.
  CLEAR :
    CT_FIELDCAT[],
    LT_FIELDCAT[].
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING LT_FIELDCAT.

  MC_FIELDCAT :
    "FIELDNAME      "REPTEXT  "TYPE   "LEN
                    "Select
    'ZSEL'          TEXT-F01  ''      '5',
                    "NO
    'ZSEQ'          TEXT-F02  ''      '5',
                    "Company
    'BUKRS'         TEXT-F34  ''      '4',
                    "Personnel No.
    'PERNR'         TEXT-F35  ''      '8',
                    "Collector Name
    'PERNM'         TEXT-F03  'CHAR'  '20',

    'WORK_DATE'     ''        ''      '',

    'ACTION_TYPE'   ''        ''      '8',
                    "STATUS
    'STATUS'        TEXT-F49  ''      '5',
                    "Customer Code
    'KUNNR'         TEXT-F04  ''      '10',
                    "Customer Name [TH]
    'CNAMT'         TEXT-F05  ''      '20',
                    "Customer Name [EN]
    'CNAME'         TEXT-F06  ''      '20',
                    "DocumentNo
    'BELNR'         TEXT-F07  ''      '',
                    "Fiscal Year
    'GJAHR'         TEXT-F36  ''      '',
                    "Sales Order No.
    'VBELN'         TEXT-F08  ''      '',
                    "Tax Invoice No.
    'XBLNR'         TEXT-F56  ''      '',
                    "Doc. Date
    'BLDAT'         TEXT-F41  ''      '',
                    "Net Due Date
    'FAEDT'         TEXT-F09  ''      '',
                    "Amount
    'WRBTR'         TEXT-F10  ''      '15',
                    "Currency
    'WAERS'         TEXT-F47  ''      '',
                    "Payment Date
    'PAYMENT_DATE'  TEXT-F42  ''      '',

    'FOLLOW_DATE'   ''        ''      '',

    'RECEIVED_DATE' ''        ''      '',

    'RECEIVED_AMT'  ''        ''      '15',
                    "Deduct Amount
    'DEDUCT_AMT'    TEXT-F43  ''      '15',
                    "Payment Method
    'PYMT_METHOD'   TEXT-F11  ''      '12',
                    "TF/CHQ. Date
    'BANK_DATE'     TEXT-F12  ''      '15',
                    "Cheque Number
    'CHEQUE_NO'     TEXT-F13   ''      '',
                    "Bank Name
    'BANKL'         TEXT-F44   ''      '',
                    "House Bank
    'HBKID'         TEXT-F50   ''      '',
                    "Account ID
    'HKTID'         TEXT-F48   ''      '',

    'BANKK'         TEXT-F37   'CHAR'  '15',

    'BANKN'         ''         ''      '',
                    "Receipt No.
    'RECEIPT_NO'    TEXT-F45    ''      '',

    'REMARK'        ''         ''      '20',

    'EXPS_AMT'      ''         ''      '',
                    "Income Amout
    'INCOME_AMT'    TEXT-F17   ''      '15',

    'WHT_AMT'       ''         ''      '15',

    'FEE'           ''         ''      '15',

    'RETENTION'     ''         ''      '15',
                    "PDC Contra Amount
    'CASH_CON'      TEXT-F53   ''      '15',

    'BAL_AMT'       ''         ''      '15',
                    "Pay-In No
    'PAYNO'         TEXT-F46   ''      '',

    'BRNCH'         ''         ''      '',
                    "Sale Person
    'SPTXT'         TEXT-F20   'CHAR'  '20',
*    'VTWEG'         TEXT-F20   'CHAR'  '2',
                    "Sale office
    'VKBUT'         TEXT-F21   'CHAR'  '20',
*    'VKBUR'         TEXT-F21   'CHAR'  '',
                    "Sale group
    'VKGRT'         TEXT-F22   'CHAR'  '20',
*    'VKGRP'         TEXT-F22   'CHAR'  '',
                    "Project Name
    'POST1'         TEXT-F23   'CHAR'  '',
                    "PO. No.
    'BSTKD'         TEXT-F24   ''      '',
                    "Payment Term
    'ZTERM'         TEXT-F25   'CHAR'  '',
                    "Profit Center
    'PRCTR'         TEXT-F51   ''      '',
                    "Profit Center Description
    'KTEXT'         TEXT-F26   'CHAR'  '',
                    "Transfer No.
    'TRANF_NO'      TEXT-F27   ''      '10',

    'BILLPL_NO'     ''         ''      '',
                    "Mapping Bank
    'BANKK2'        TEXT-F32   ''      '',
                    "Mapping Bank item
    'ZBANK_ITEM'    TEXT-F33   ''      '',

    'BILLPL_AMT'    ''         ''      '15',
                      "Document Status
    'DOCUMENT_STATUS' TEXT-F52 ''      '10',

    'INV_STATUS'    ''         ''      '10',

    'REMARK_VENDOR' ''         ''      '20',

    'REASON_VENDOR' ''         ''      '20',
                    "Billing Cycle
    'ZBLLCYL'       TEXT-F54  ''      '20',
    "'BILLING_CYCLE' ''         ''      '20',
                    "Collection Cycle
    'ZBLLCOL'       TEXT-F55   ''      '20',
    "'COLLECTION_CYCLE' ''      ''      '20',

    'AWB_NO'        TEXT-F38   'CHAR'  '10'.

  IF CB_SEND IS INITIAL.
    DELETE CT_FIELDCAT WHERE FIELDNAME EQ 'ZSEL'.
  ENDIF.

  IF GF_DOWNLOAD_XLS EQ ABAP_TRUE.
    LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<LFS_FIELDCAT>).
      CASE <LFS_FIELDCAT>-FIELDNAME.
        WHEN 'WORK_DATE'
          OR 'BLDAT'
          OR 'FAEDT'
          OR 'PAYMENT_DATE'
          OR 'FOLLOW_DATE'
          OR 'RECEIVED_DATE'
          OR 'BANK_DATE'.

          CONCATENATE <LFS_FIELDCAT>-FIELDNAME '_TX'
            INTO <LFS_FIELDCAT>-FIELDNAME.
          <LFS_FIELDCAT>-DATATYPE   = 'CHAR'.
          <LFS_FIELDCAT>-INTTYPE    = 'C'.
          <LFS_FIELDCAT>-INTLEN     = '10'.
          <LFS_FIELDCAT>-OUTPUTLEN  = '10'.
          <LFS_FIELDCAT>-DD_OUTLEN  = '10'.
          <LFS_FIELDCAT>-REF_TABLE  = ''.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

* Initialize Output
  CLEAR: CT_SORT.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '23%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '77%'.

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
* Text-h01 : Report:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-TITLE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Company Code:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = P_BUKRS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
*  CALL METHOD LREF_TABLE->NEW_ROW.
** Text-h03 : Personnel Number:
*  LF_TEXT = TEXT-H03.
*  CALL METHOD LREF_COL_KEY->ADD_TEXT
*    EXPORTING
*      TEXT = LF_TEXT.
*  LF_TEXT = P_PERNR.
*  CALL METHOD LREF_COL_VAL->ADD_TEXT
*    EXPORTING
*      TEXT = LF_TEXT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##NEEDED ##CALLED.

  DATA:
    LV_VALID   TYPE  CHAR01,
    LV_REFRESH TYPE CHAR01.


  CASE SY-UCOMM.
    WHEN 'EXP_XLS'.
*     Validate Data
      GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID   = LV_VALID
                                       CHANGING  C_REFRESH = LV_REFRESH ).
*     Continue processing only when valid
      IF LV_VALID IS INITIAL.
        RETURN.
      ENDIF.

*     Add processing logic for User-Command here. . .
      PERFORM F_DOWNLOAD_XLS.

    WHEN 'SALL'.

      PERFORM F_SET_ZSEL USING GC_TRUE.

    WHEN 'UALL'.

      PERFORM F_SET_ZSEL USING SPACE.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DOWNLOAD_XLS
*&---------------------------------------------------------------------*
*& Download Excel File.
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DOWNLOAD_XLS .

  DATA:
    LT_BIN  TYPE SOLIX_TAB.

  DATA:
    LF_FILE    TYPE STRING,
    LF_PATHS   TYPE STRING,
    LF_XSTRING TYPE XSTRING,
    LF_SIZE    TYPE I.

* Show progress
* Text-p98 : Generating Excel File . . .
  MC_SHOW_PROGRESS 99 TEXT-P98.

  DATA(LT_RESULT) = GT_RESULT[].
  DELETE GT_RESULT WHERE ZSEL <> 'X'.
  IF GT_RESULT[] IS INITIAL.
    MESSAGE E001(00) WITH TEXT-E01 ##MG_MISSING.
    RETURN.
  ENDIF.

  GREF_GRID_1->REFRESH_TABLE_DISPLAY( ).

  LF_PATHS = 'C:\temp' ##NO_TEXT.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      INITIAL_FOLDER       = LF_PATHS
    CHANGING
      SELECTED_FOLDER      = LF_PATHS
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    MESSAGE S001(00) WITH TEXT-E09 DISPLAY LIKE 'E' ##MG_MISSING.
    RETURN.
  ENDIF.

  CONCATENATE
    LF_PATHS
    '\'    ##NO_TEXT
    "P_PERNR
    TEXT-T01
    SY-DATLO
    SY-TIMLO
    '.XLSX'
  INTO LF_FILE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

  GF_DOWNLOAD_XLS = ABAP_TRUE.
* Build Field catalog
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
  DELETE GT_FIELDCAT_1 WHERE FIELDNAME EQ 'ZSEL'.
  DELETE GT_FIELDCAT_1 WHERE FIELDNAME EQ 'ZSEQ'.

* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.

* Call Method Generate XLSX Xstring
  CLEAR LF_XSTRING.
  LF_XSTRING = ZCL_SDSCA_UTILITIES=>CREATE_XLSX_FROM_ITAB(
    EXPORTING
      IS_LAYOUT   = GS_LAYOUT_1
      IT_FIELDCAT = GT_FIELDCAT_1
      IT_SORT     = GT_SORT_1
      IT_DATA     = GT_RESULT ).
  IF LF_XSTRING IS INITIAL.
*   Error occurred during generate Excel file.
    MESSAGE S004(ZSDSCA01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Convert XString to Binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      BUFFER        = LF_XSTRING
    IMPORTING
      OUTPUT_LENGTH = LF_SIZE
    TABLES
      BINARY_TAB    = LT_BIN.

* Download to local file
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
      BIN_FILESIZE            = LF_SIZE
      FILENAME                = LF_FILE
      FILETYPE                = 'BIN'
    CHANGING
      DATA_TAB                = LT_BIN
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      NOT_SUPPORTED_BY_GUI    = 22
      ERROR_NO_GUI            = 23
      OTHERS                  = 24.
  IF SY-SUBRC <> 0.
*   Error occurred during generate Excel file.
    MESSAGE S004(ZSDSCA01) DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
*   Show Success Message
*   Export file generated successfully.
    MESSAGE S005(ZSDSCA01).

    PERFORM F_UPDATE_ZLOG USING LF_FILE.
  ENDIF.

  GT_RESULT[] = LT_RESULT[].
  DELETE GT_RESULT WHERE ZSEL EQ GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_ZSEL
*&---------------------------------------------------------------------*
*& Set ZSEL All Value
*&---------------------------------------------------------------------*
*&      --> GC_TRUE
*&---------------------------------------------------------------------*
FORM F_SET_ZSEL  USING    UF_FLAG ##PERF_NO_TYPE.

  LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>).
    <LFS_RESULT>-ZSEL = UF_FLAG.
  ENDLOOP.

  GREF_GRID_1->REFRESH_TABLE_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXCLUDE_TOOL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_EXCLUDE_TOOL .

  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_PRINT TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_INFO TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW TO GT_TOOL_EXC_1.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW TO GT_TOOL_EXC_1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UPDATE_ZLOG
*&---------------------------------------------------------------------*
*& Update Log Table ZSDSFIT039
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_UPDATE_ZLOG USING UF_FILE  TYPE STRING.

  DATA :
    GT_ZLOG TYPE STANDARD TABLE OF ZSDSFIT039.

  LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>).
    APPEND INITIAL LINE TO GT_ZLOG ASSIGNING FIELD-SYMBOL(<LFS_ZLOG>).
    MOVE-CORRESPONDING <LFS_RESULT> TO <LFS_ZLOG>.

    <LFS_ZLOG>-FILENAME   = UF_FILE.
    "<LFS_ZLOG>-STATU      = GC_TRUE.

    <LFS_ZLOG>-ZCRT_DATE  = SY-DATLO.
    <LFS_ZLOG>-ZCRT_TIME  = SY-TIMLO.
    <LFS_ZLOG>-ZCRT_USER  = SY-UNAME.
    <LFS_ZLOG>-ZCRT_PGM   = SY-REPID.

    IF <LFS_RESULT>-ZFILE IS NOT INITIAL.
      <LFS_ZLOG>-ZUPD_DATE  = SY-DATLO.
      <LFS_ZLOG>-ZUPD_TIME  = SY-TIMLO.
      <LFS_ZLOG>-ZUPD_USER  = SY-UNAME.
      <LFS_ZLOG>-ZUPD_PGM   = SY-REPID.
    ENDIF.
  ENDLOOP.

  IF GT_ZLOG[] IS NOT INITIAL.
    MODIFY ZSDSFIT039 FROM TABLE GT_ZLOG.
    IF SY-SUBRC EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_HIDE_SCREEN
*&---------------------------------------------------------------------*
*& Hide Screen
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_HIDE_SCREEN .

  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'M0'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      WHEN 'M1'.
        IF RB_REP EQ GC_TRUE.
          SCREEN-ACTIVE = '1'.
        ELSE.
          SCREEN-ACTIVE = '0'.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'M2'.
        IF RB_UNB EQ GC_TRUE.
          SCREEN-ACTIVE = '1'.
        ELSE.
          SCREEN-ACTIVE = '0'.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  CASE GC_TRUE.
    WHEN RB_REP.
      IF P_BUKRS IS INITIAL.
        SET CURSOR FIELD 'P_BUKRS'.
*       Error: Please enter Company Code.
        MESSAGE E001(00) WITH TEXT-E07 ##MG_MISSING.
        RETURN.
      ENDIF.

*      IF P_PERNR IS INITIAL.
*        SET CURSOR FIELD 'P_BUKRS'.
**       Error: Please enter Personnel Number.
*        MESSAGE E001(00) WITH TEXT-E08 ##MG_MISSING.
*        RETURN.
*      ENDIF.

    WHEN RB_UNB.
      IF S_BPLN2 IS INITIAL AND
         S_GJAH2 IS INITIAL AND
         S_BELN2 IS INITIAL.
        SET CURSOR FIELD 'S_BPLN2-LOW'.
*       Error: Please enter Bill Placement No or Invoice Number Collector.
        MESSAGE E001(00) WITH TEXT-E03 TEXT-E04 ##MG_MISSING.
        RETURN.
      ENDIF.

      IF S_BELN2 IS NOT INITIAL.

        IF P_BUKR2 IS INITIAL.
          SET CURSOR FIELD 'P_BUKR2'.
*         Error: Please enter Company Code.
          MESSAGE E001(00) WITH TEXT-E07 ##MG_MISSING.
          RETURN.
        ENDIF.

        IF S_GJAH2 IS INITIAL.
          SET CURSOR FIELD 'S_GJAH2-LOW'.
*         Error: Please enter Fiscal Year.
          MESSAGE E001(00) WITH TEXT-E05 ##MG_MISSING.
          RETURN.
        ENDIF.
      ENDIF.

      IF S_GJAH2 IS NOT INITIAL.
        IF P_BUKR2 IS INITIAL.
          SET CURSOR FIELD 'P_BUKR2'.
*         Error: Please enter Company Code.
          MESSAGE E001(00) WITH TEXT-E07 ##MG_MISSING.
          RETURN.
        ENDIF.

        IF S_BELN2 IS INITIAL.
          SET CURSOR FIELD 'S_BELN2-LOW'.
*         Error: Please enter Invoice Number Collector.
          MESSAGE E001(00) WITH TEXT-E06  ##MG_MISSING.
          RETURN.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UNBLOCK_DOWNLOAD
*&---------------------------------------------------------------------*
*& Unblock Download
*&---------------------------------------------------------------------*
FORM F_UNBLOCK_DOWNLOAD .

  DATA :
    LF_EXIT TYPE C.

  SELECT FROM ZSDSFIT039
    FIELDS *
    WHERE BILLPL_NO   IN @S_BPLN2
    AND   BILLPL_DATE IN @S_BPLD2
    AND   BUKRS       EQ @P_BUKR2
    AND   BELNR       IN @S_BELN2
    AND   GJAHR       IN @S_GJAH2
    AND   STATU       EQ 'X'
    INTO TABLE @DATA(LT_UPD).
  IF SY-SUBRC <> 0.
    MESSAGE S001(00) WITH TEXT-E02 DISPLAY LIKE 'E' ##MG_MISSING.
    RETURN.
  ENDIF.

  LOOP AT LT_UPD ASSIGNING FIELD-SYMBOL(<LFS_UPD>).
    <LFS_UPD>-STATU     = ''.
    <LFS_UPD>-ZUPD_DATE = SY-DATLO.
    <LFS_UPD>-ZUPD_TIME = SY-TIMLO.
    <LFS_UPD>-ZUPD_USER = SY-UNAME.
    <LFS_UPD>-ZUPD_PGM  = SY-REPID.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      I_TITLE          = 'Unblock List' ##NO_TEXT
      I_TABNAME        = 'ZSDSFIT039'
      I_STRUCTURE_NAME = 'ZSDSFIT039'
    IMPORTING
      E_EXIT           = LF_EXIT
    TABLES
      T_OUTTAB         = LT_UPD.

  IF LF_EXIT IS NOT INITIAL.
    RETURN.
  ENDIF.

  IF LT_UPD[] IS NOT INITIAL.
    MODIFY ZSDSFIT039 FROM TABLE LT_UPD.
    IF SY-SUBRC EQ 0.
      COMMIT WORK AND WAIT.
      "Unblock Download Excel Success.
      MESSAGE S001(00) WITH TEXT-I01 ##MG_MISSING.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_FILTER
*&---------------------------------------------------------------------*
*& Set Filter
*&---------------------------------------------------------------------*
*&      <-- LRT_STATUS2
*&      <-- LRT_ACTIVITY_TYP2
*&---------------------------------------------------------------------*
FORM F_SET_FILTER  CHANGING CT_STATUS         ##PERF_NO_TYPE
                            CT_ACTIVITY_TYPE  ##PERF_NO_TYPE.

  DATA :
    LRT_STATUS        TYPE RANGE OF ZSDSFIT029-STATUS,
    LRT_ACTIVITY_TYPE TYPE RANGE OF ZSDSFIT029-ACTION_TYPE.

  CASE CB_SEND.
    WHEN ABAP_TRUE.

      IF GRT_STATUS[] IS NOT INITIAL.
        IF S_STAT[] IS NOT INITIAL.
          SELECT FROM DD07L
            FIELDS
              'I'         AS SIGN   ,
              'EQ'        AS OPTION ,
              DOMVALUE_L  AS LOW    ,
              ' '         AS HIGH
            WHERE DOMNAME EQ 'ZSDSDM_COL_STATUS'
            AND   DOMVALUE_L IN @S_STAT
            INTO TABLE @DATA(LT_STAT).
          LOOP AT LT_STAT ASSIGNING FIELD-SYMBOL(<LFS_STAT>).
            IF <LFS_STAT>-LOW IN GRT_STATUS.
              APPEND <LFS_STAT> TO LRT_STATUS.
            ENDIF.
          ENDLOOP.
        ELSE.
          APPEND LINES OF GRT_STATUS TO LRT_STATUS.
        ENDIF.
      ELSE.
        APPEND LINES OF S_STAT TO LRT_STATUS.
      ENDIF.

      IF GRT_ACTIVITY_TYPE[] IS NOT INITIAL.
        IF S_ACTY[] IS NOT INITIAL.
          SELECT FROM DD07L
            FIELDS
              'I'         AS SIGN   ,
              'EQ'        AS OPTION ,
              DOMVALUE_L  AS LOW    ,
              ' '         AS HIGH
            WHERE DOMNAME EQ 'ZSDSDM_ACTION_TYPE'
            AND   DOMVALUE_L IN @S_ACTY
            INTO TABLE @DATA(LT_ACTY).
          LOOP AT LT_ACTY ASSIGNING FIELD-SYMBOL(<LFS_ACTY>).
            IF <LFS_ACTY>-LOW IN GRT_ACTIVITY_TYPE.
              APPEND <LFS_ACTY> TO LRT_ACTIVITY_TYPE.
            ENDIF.
          ENDLOOP.
        ELSE.
          APPEND LINES OF GRT_ACTIVITY_TYPE TO LRT_ACTIVITY_TYPE.
        ENDIF.
      ELSE.
        APPEND LINES OF S_ACTY TO LRT_ACTIVITY_TYPE.
      ENDIF.

    WHEN OTHERS.
      APPEND LINES OF S_STAT TO LRT_STATUS.
      APPEND LINES OF S_ACTY TO LRT_ACTIVITY_TYPE.
  ENDCASE.

  CT_STATUS = LRT_STATUS[].
  CT_ACTIVITY_TYPE = LRT_ACTIVITY_TYPE[].

ENDFORM.
