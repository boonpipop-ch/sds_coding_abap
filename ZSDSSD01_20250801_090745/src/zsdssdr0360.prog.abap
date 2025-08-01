*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0360
*  Creation Date      : 22.10.2024
*  Author             : Jutamas Y.(Eviden)
*  Add-on ID          : N/A
*  Description        : Billing Plan Report
*  Purpose            : The report will display transaction in
*                       sales transactions and document relate such as
*                       quotation, sales order, billing plan in SO,
*                       billing document
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0360.


*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBAK,  FPLT .


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSSDS111.
TYPES:
         LINECOLOR TYPE  CHAR4,
       END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_SO ,
         VBELN       TYPE VBAP-VBELN,
         POSNR       TYPE VBAP-POSNR,
         VKORG       TYPE VBAK-VKORG,
         VTWEG       TYPE VBAK-VTWEG,
         VKBUR       TYPE VBAK-VKBUR,
         VKGRP       TYPE VBAK-VKGRP,
         KUNNR       TYPE VBAK-KUNNR,
         AUART       TYPE VBAK-AUART,
         ERDAT       TYPE VBAK-ERDAT,
         ERNAM       TYPE VBAK-ERNAM,
         NAME1       TYPE KNA1-NAME1,
         NAME2       TYPE KNA1-NAME2,
         ZZPOB       TYPE VBAK-ZZPOB,
         DESCRIPTION TYPE FARR_C_EVNT_TY_T-DESCRIPTION,
       END OF   TS_SO .
TYPES TT_SO TYPE STANDARD TABLE OF TS_SO .

TYPES: BEGIN OF TS_INVOICE ,
         VBELV   TYPE VBFA-VBELV,
         POSNV   TYPE VBFA-POSNV,
         VBTYP_V TYPE VBFA-VBTYP_V,
         VBELN   TYPE VBFA-VBELN,
         POSNN   TYPE VBFA-POSNN,
         VBTYP_N TYPE VBFA-VBTYP_N,
         FKART   TYPE VBRK-FKART,
         FKDAT   TYPE VBRK-FKDAT,
       END OF   TS_INVOICE .
TYPES TT_INVOICE TYPE STANDARD TABLE OF TS_INVOICE .

TYPES: BEGIN OF TS_BILLINGPLAN ,
         VBELN TYPE FPLA-VBELN,
         FPLNR TYPE FPLT-FPLNR,
         FPLTR TYPE FPLT-FPLTR,
         FKDAT TYPE FPLT-FKDAT,
         FPROZ TYPE FPLT-FPROZ,
         FAKWR TYPE FPLT-FAKWR,
         WAERS TYPE FPLT-WAERS,
         FAKSP TYPE FPLT-FAKSP,
       END OF   TS_BILLINGPLAN .
TYPES TT_BILLINGPLAN TYPE STANDARD TABLE OF TS_BILLINGPLAN .


TYPES: BEGIN OF TS_QUOTATION ,
         VBELV   TYPE VBFA-VBELV,
         POSNV   TYPE VBFA-POSNV,
         VBTYP_V TYPE VBFA-VBTYP_V,
         VBELN   TYPE VBFA-VBELN,
         POSNN   TYPE VBFA-POSNN,
         VBTYP_N TYPE VBFA-VBTYP_N,
         AUART   TYPE VBAK-AUART,
       END OF   TS_QUOTATION .
TYPES TT_QUOTATION TYPE STANDARD TABLE OF TS_QUOTATION .

TYPES: BEGIN OF TS_SHIPTO ,
         VBELN TYPE VBPA-VBELN,
         KUNNR TYPE VBPA-KUNNR,
         NAME1 TYPE KNA1-NAME1,
       END OF   TS_SHIPTO .
TYPES TT_SHIPTO TYPE STANDARD TABLE OF TS_SHIPTO .

TYPES: BEGIN OF TS_SALESEMP ,
         VBELN TYPE VBPA-VBELN,
         PERNR TYPE VBPA-PERNR,
         ENAME TYPE PA0001-ENAME,
       END OF   TS_SALESEMP .
TYPES TT_SALESEMP TYPE STANDARD TABLE OF TS_SALESEMP .



*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
##NEEDED
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSSD031'.


*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GT_RESULT     TYPE TT_RESULT.

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
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS111'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100.

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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_VKORG FOR VBAK-VKORG OBLIGATORY DEFAULT '1000' ,
    S_VTWEG	FOR VBAK-VTWEG ,
    S_VKBUR	FOR VBAK-VKBUR ,
    S_VKGRP	FOR VBAK-VKGRP ,
    S_KUNNR	FOR VBAK-KUNNR ,
    S_AUART	FOR VBAK-AUART DEFAULT 'ZO02',
    S_VBELN	FOR VBAK-VBELN ,
    S_ERDAT	FOR VBAK-ERDAT ,
    S_FKDAT	FOR FPLT-FKDAT ,
    S_FAKSP	FOR FPLT-FAKSP ,
    S_ZZPOB FOR VBAK-ZZPOB .
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN.



*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT.

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
  GF_ALV_HEADER_1 = GC_TRUE.
* Soft refresh data
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
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
  CS_LAYOUT-SEL_MODE   = 'B'.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.
  CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  CS_LAYOUT-CTAB_FNAME = 'COLTAB'.

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
  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  DEFINE MC_FIELDCAT.
    <L_FIELDCAT>-scrtext_s = <L_FIELDCAT>-scrtext_m =
    <L_FIELDCAT>-scrtext_l = <L_FIELDCAT>-coltext =
    <L_FIELDCAT>-seltext   = <L_FIELDCAT>-tooltip =
    <L_FIELDCAT>-reptext   = &1.
  END-OF-DEFINITION.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.
    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'VKORG'.
* text-C01: Sales Organization
        MC_FIELDCAT TEXT-C01.
      WHEN 'VTWEG'.
* text-C02: Distribution Channel
        MC_FIELDCAT  TEXT-C02.
      WHEN 'VKBUR'.
* text-C03: Sales Office
        MC_FIELDCAT  TEXT-C03.
      WHEN 'VKGRP'.
* text-C04: Sales Group
        MC_FIELDCAT  TEXT-C04.
      WHEN 'CUST_NO'.
* text-C05: Customer Code
        MC_FIELDCAT  TEXT-C05.
      WHEN 'CUST_NAME1'.
* text-C06: Customer Name 1
        MC_FIELDCAT  TEXT-C06.
      WHEN 'CUST_NAME2'.
* text-C07: Customer Name 2
        MC_FIELDCAT  TEXT-C07.
      WHEN 'SHIPTO_NO'.
* text-C08: ShipTo Code
        MC_FIELDCAT  TEXT-C08.
      WHEN 'SHIPTO_NAME'.
* text-C09: ShipTo Name
        MC_FIELDCAT  TEXT-C09.
      WHEN 'PERNR'.
* text-C10: Sales Employee Code
        MC_FIELDCAT  TEXT-C10.
      WHEN 'ENAME'.
* text-C11: Sales Employee Name
        MC_FIELDCAT  TEXT-C11.
      WHEN 'QT_TYPE'.
* text-C12: Q/T Type
        MC_FIELDCAT  TEXT-C12.
      WHEN 'QT_NO'.
* text-C13: Q/T No.
        MC_FIELDCAT  TEXT-C13.
      WHEN 'SO_TYPE'.
* text-C14: S/O Type
        MC_FIELDCAT  TEXT-C14.
      WHEN 'SO_NO'.
* text-C15: S/O No.
        MC_FIELDCAT  TEXT-C15.
        <L_FIELDCAT>-HOTSPOT = 'X' .
      WHEN 'ERDAT'.
* text-C16: S/O Create Date
        MC_FIELDCAT  TEXT-C16.
      WHEN 'ERNAM'.
* text-C17: S/O Create name
        MC_FIELDCAT  TEXT-C17.
      WHEN 'BILL_TYPE'.
* text-C18: Billing Type
        MC_FIELDCAT  TEXT-C18.
      WHEN 'BILL_NO'.
* text-C19: Billing Number
        MC_FIELDCAT  TEXT-C19.
      WHEN 'FKDAT'.
* text-C20: Billing Date
        MC_FIELDCAT  TEXT-C20.
      WHEN 'FPROZ'.
* text-C21: Percentage
        MC_FIELDCAT  TEXT-C21.
      WHEN 'FAKWR'.
* text-C22: Amount
        MC_FIELDCAT  TEXT-C22.
      WHEN 'WAERS'.
* text-C23: Currency
        MC_FIELDCAT  TEXT-C23.
      WHEN 'FAKSP'.
* text-C24: Billing Block
        MC_FIELDCAT  TEXT-C24.
      WHEN 'ZZPOB'.
* text-C25: POB
        MC_FIELDCAT  TEXT-C25.
      WHEN 'DESCRIPTION'.
* text-C26: POB Description
        MC_FIELDCAT  TEXT-C26.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING PT_SORT TYPE LVC_T_SORT ##NEEDED.
*  CONSTANTS:
*  LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VKORG'.
*
*  DATA:
*  LS_SORT  TYPE  LVC_S_SORT.
*
** INITIALIZE OUTPUT
*  CLEAR: PT_SORT.
*
** Sort by Company Code
*  CLEAR LS_SORT.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO PT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA   CHANGING CT_RESULT TYPE  TT_RESULT .

  DATA:
    LT_SO          TYPE TT_SO,
    LT_QUOTATION   TYPE TT_QUOTATION,
    LT_BILLINGPLAN TYPE TT_BILLINGPLAN,
    LT_SHIPTO      TYPE TT_SHIPTO,
    LT_SALESEMP    TYPE TT_SALESEMP,
    LT_INVOICE     TYPE TT_INVOICE.

* Show progress
* Text-p01 : Reading data . . .
  MC_SHOW_PROGRESS 40 TEXT-P01.

* Initialize Output
  CLEAR: CT_RESULT.

  PERFORM F_GET_DATA CHANGING LT_SO
                              LT_QUOTATION
                              LT_BILLINGPLAN
                              LT_SHIPTO
                              LT_SALESEMP
                              LT_INVOICE.
  PERFORM F_SET_DATA USING LT_SO
                           LT_QUOTATION
                           LT_BILLINGPLAN
                           LT_SHIPTO
                           LT_SALESEMP
                           LT_INVOICE
                  CHANGING CT_RESULT.

  SORT CT_RESULT BY SO_NO FKDAT .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
*& Get data
*&---------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_SO          TYPE TT_SO
                          CT_QUOTATION   TYPE TT_QUOTATION
                          CT_BILLINGPLAN TYPE TT_BILLINGPLAN
                          CT_SHIPTO      TYPE TT_SHIPTO
                          CT_SALESEMP    TYPE TT_SALESEMP
                          CT_INVOICE     TYPE TT_INVOICE .

  CONSTANTS: LC_WE TYPE VBPA-PARVW VALUE 'WE',
             LC_VE TYPE VBPA-PARVW VALUE 'VE'.

*-Initialize value
  CLEAR: CT_SO ,
         CT_QUOTATION  ,
         CT_BILLINGPLAN,
         CT_SHIPTO ,
         CT_SALESEMP .

  SELECT  VBAP~VBELN,
          VBAP~POSNR,
          VBAK~VKORG,
          VBAK~VTWEG,
          VBAK~VKBUR,
          VBAK~VKGRP,
          VBAK~KUNNR,
          VBAK~AUART,
          VBAK~ERDAT,
          VBAK~ERNAM,
          KNA1~NAME1,
          KNA1~NAME2,
          VBAK~ZZPOB,
          F~DESCRIPTION
  INTO TABLE @CT_SO
  FROM  VBAP  INNER JOIN VBAK ON VBAK~VBELN =  VBAP~VBELN
              INNER JOIN KNA1 ON VBAK~KUNNR =  KNA1~KUNNR
              INNER JOIN FARR_C_EVNT_TY_T AS F  "#EC CI_BUFFJOIN
                         ON  EVENT_TYPE = VBAK~ZZPOB
                         AND LANGUAGE = 'E'
  WHERE VBAK~VKORG IN @S_VKORG
    AND VBAK~VTWEG IN @S_VTWEG
    AND VBAK~VKBUR IN @S_VKBUR
    AND VBAK~VKGRP IN @S_VKGRP
    AND VBAK~KUNNR IN @S_KUNNR
    AND VBAK~AUART IN @S_AUART
    AND VBAK~VBELN IN @S_VBELN
    AND VBAK~ERDAT IN @S_ERDAT
    and vbak~zzpob in @s_zzpob.

  IF CT_SO IS NOT INITIAL.
    SORT CT_SO BY VBELN VKORG VTWEG VKBUR VKGRP .

    "Get ship to
    SELECT VBPA~VBELN,
           VBPA~KUNNR,
           KNA1~NAME1
      FROM VBPA LEFT OUTER JOIN KNA1 ON VBPA~KUNNR = KNA1~KUNNR
       FOR ALL ENTRIES IN @CT_SO
     WHERE VBELN = @CT_SO-VBELN
       AND PARVW = @LC_WE
      INTO TABLE @CT_SHIPTO .
    IF SY-SUBRC = 0 .
      SORT CT_SHIPTO BY VBELN KUNNR .
    ENDIF.

    "Get sales employee
    SELECT VBPA~VBELN,
           VBPA~PERNR ,
           PA0001~ENAME
      FROM VBPA LEFT OUTER JOIN PA0001 ON VBPA~PERNR = PA0001~PERNR
      FOR ALL ENTRIES IN @CT_SO
    WHERE VBELN = @CT_SO-VBELN
      AND PARVW = @LC_VE
     INTO TABLE @CT_SALESEMP .
    IF SY-SUBRC = 0 .
      SORT CT_SALESEMP BY VBELN PERNR .
    ENDIF.

    "Get Invoice
    SELECT
      VBFA~VBELV,
      VBFA~POSNV,
      VBFA~VBTYP_V,
      VBFA~VBELN,
      VBFA~POSNN,
      VBFA~VBTYP_N,
      VBRK~FKART,
      VBRK~FKDAT
       FROM VBFA  INNER JOIN VBRK ON VBRK~VBELN = VBFA~VBELN
       FOR ALL ENTRIES IN @CT_SO
     WHERE VBFA~VBELV   =  @CT_SO-VBELN
       AND VBFA~VBTYP_N = 'M'  "Invoice
       AND VBRK~FKSTO IS INITIAL
      INTO TABLE @CT_INVOICE .
    IF SY-SUBRC = 0 .
      SORT CT_INVOICE BY VBELV POSNV .
    ENDIF.

    "Get Quatation data
    SELECT  VBFA~VBELV,
            VBFA~POSNV,
            VBFA~VBTYP_V,
            VBFA~VBELN,
            VBFA~POSNN,
            VBFA~VBTYP_N,
            VBAK~AUART
    FROM VBFA INNER JOIN VBAK ON VBAK~VBELN = VBFA~VBELV
    FOR ALL ENTRIES IN @CT_SO
    WHERE VBFA~VBELN = @CT_SO-VBELN
      AND VBFA~POSNN = @CT_SO-POSNR
      AND VBFA~VBTYP_N = 'C' "Order
      AND VBFA~VBTYP_V = 'B' "Quatation
    INTO TABLE @CT_QUOTATION.
    IF SY-SUBRC = 0 .
      SORT CT_QUOTATION BY VBELN VBTYP_N .
    ENDIF.

    "Get Bill Plan data
    SELECT
      FPLA~VBELN,
      FPLT~FPLNR,
      FPLT~FPLTR,
      FPLT~FKDAT,
      FPLT~FPROZ,
      FPLT~FAKWR,
      FPLT~WAERS,
      FPLT~FAKSP
      FROM FPLT INNER JOIN FPLA ON FPLA~FPLNR = FPLT~FPLNR
       FOR ALL ENTRIES IN @CT_SO
     WHERE FPLT~FKDAT IN @S_FKDAT
       AND FPLT~FAKSP IN @S_FAKSP
       AND FPLA~VBELN = @CT_SO-VBELN
       AND FPLA~RFPLN = ''
      INTO TABLE @CT_BILLINGPLAN.
    IF SY-SUBRC = 0 .
      SORT CT_BILLINGPLAN BY FPLNR FPLTR.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_data
*&---------------------------------------------------------------------*
*& Set data to display
*&---------------------------------------------------------------------*
FORM F_SET_DATA  USING  UT_SO          TYPE TT_SO
                        UT_QUOTATION   TYPE TT_QUOTATION
                        UT_BILLINGPLAN TYPE TT_BILLINGPLAN
                        UT_SHIPTO      TYPE TT_SHIPTO
                        UT_SALESEMP    TYPE TT_SALESEMP
                        UT_INVOICE     TYPE TT_INVOICE
              CHANGING  CT_RESULT      TYPE TT_RESULT .

  DATA LS_RESULT TYPE TS_RESULT .

  LOOP AT UT_BILLINGPLAN ASSIGNING FIELD-SYMBOL(<FS_PLAN>).
    MOVE-CORRESPONDING <FS_PLAN> TO LS_RESULT .



    READ TABLE UT_SO ASSIGNING FIELD-SYMBOL(<FS_SO>)
                     WITH KEY VBELN = <FS_PLAN>-VBELN
                     BINARY SEARCH.
    IF SY-SUBRC = 0 .
      MOVE-CORRESPONDING <FS_SO> TO LS_RESULT .
      LS_RESULT-CUST_NO    = <FS_SO>-KUNNR.
      LS_RESULT-CUST_NAME1 = <FS_SO>-NAME1.
      LS_RESULT-CUST_NAME2 = <FS_SO>-NAME2.
      LS_RESULT-SO_TYPE    = <FS_SO>-AUART .
      LS_RESULT-SO_NO      = <FS_SO>-VBELN .

      READ TABLE UT_SHIPTO ASSIGNING FIELD-SYMBOL(<FS_SHIPTO>)
                           WITH KEY VBELN = <FS_SO>-VBELN
                           BINARY SEARCH.
      IF <FS_SHIPTO> IS ASSIGNED .
        LS_RESULT-SHIPTO_NO = <FS_SHIPTO>-KUNNR .

        LS_RESULT-SHIPTO_NAME = <FS_SHIPTO>-NAME1 .

        UNASSIGN <FS_SHIPTO> .
      ENDIF.

      READ TABLE UT_SALESEMP ASSIGNING FIELD-SYMBOL(<FS_SALESEMP>)
                             WITH KEY VBELN = <FS_SO>-VBELN
                             BINARY SEARCH.
      IF <FS_SALESEMP> IS ASSIGNED.
        LS_RESULT-PERNR = <FS_SALESEMP>-PERNR .
        LS_RESULT-ENAME = <FS_SALESEMP>-ENAME .
        UNASSIGN <FS_SALESEMP> .
      ENDIF.

      READ TABLE UT_QUOTATION ASSIGNING FIELD-SYMBOL(<LFS_QT>)
                      WITH KEY VBELN = <FS_SO>-VBELN
                      BINARY SEARCH .
      IF SY-SUBRC = 0 .
        LS_RESULT-QT_TYPE = <LFS_QT>-AUART.
        LS_RESULT-QT_NO   = <LFS_QT>-VBELV .
      ENDIF.

    ENDIF.

    LOOP AT UT_INVOICE ASSIGNING FIELD-SYMBOL(<FS_INVOICE>) WHERE VBELV = <FS_PLAN>-VBELN
                                                             AND FKDAT = <FS_PLAN>-FKDAT.
      LS_RESULT-BILL_TYPE = <FS_INVOICE>-FKART.
      LS_RESULT-BILL_NO = <FS_INVOICE>-VBELN .
      EXIT.
    ENDLOOP .


    APPEND LS_RESULT TO CT_RESULT .
    CLEAR LS_RESULT.
  ENDLOOP.


ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_1
*----------------------------------------------------------------------*
*  ALV Hotspot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                             US_COLUMN_ID TYPE LVC_S_COL.


  PERFORM F_HOTSPOT_CLICK_CONF  USING US_ROW_ID
                                      US_COLUMN_ID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_HOTSPOT_CLICK_CONF
*&---------------------------------------------------------------------*
*& Hot Spot Click
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_CONF  USING  US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                                  US_COLUMN_ID TYPE LVC_S_COL.

* Read Line
  READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'SO_NO'.
      PERFORM F_DISPLAY_SO USING  <L_RESULT>-SO_NO.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_SO
*&---------------------------------------------------------------------*
*& Display Sales Order
*&---------------------------------------------------------------------*
FORM F_DISPLAY_SO USING UF_VBELN TYPE  VBAK-VBELN.

  IF UF_VBELN IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'AUN' FIELD UF_VBELN.
  CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.         "#EC CI_CALLTA


ENDFORM.
