*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0300
*  Creation Date      : 06.05.2024
*  Author             : Kittirat C.(Eviden)
*  Add-on ID          : ZMMR010
*  Description        : Report Supply Availability Overview
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*  09.01.2025  v1.00      Zulkiff B.   Bug fixing: Sum Schedule line to
*            (420000235)               one line
*-----------------------------------------------------------------------
*  21.01.2025  v2.00      Zulkiff B.   Bug fixing: Sum D/O split to
*            (420000092)               one line
*-----------------------------------------------------------------------
*  05.03.2025 420000476   Kittirat C   CH01: Chnage getting project name
*  31.03.2025  F36K915008 Suteera P.   420000543 Add logic to remove
*                                      reject item.
*-----------------------------------------------------------------------
REPORT ZSDSMMR0300.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:  BEGIN OF TS_RESULT.
          INCLUDE TYPE ZSDSMMS025.
TYPES:    LINECOLOR(4) TYPE C,          " Color
          SORT_DATE    TYPE SY-DATUM,   " For Sort
          SORTIND_00   TYPE TAG00,
          SORTIND_01   TYPE SORT1,
          SORTIND_02   TYPE SORT2,
          SLOCCODE_PLN TYPE LIPS-LGORT, " SLoc Code(Plan)
          SLOCCODE_ACT TYPE LIPS-LGORT. " SLoc Code(Actual)
TYPES:  END OF TS_RESULT.

TYPES: BEGIN OF TS_MATERIAL,
         MATNR TYPE MARD-MATNR,
         WERKS TYPE MARD-WERKS,
         LGORT TYPE MARD-LGORT,
         LABST TYPE MARD-LABST,
         UMLME TYPE MARD-UMLME,
         INSME TYPE MARD-INSME,
         SPEME TYPE MARD-SPEME,
       END OF TS_MATERIAL.

TYPES: BEGIN OF TS_SALES_ORDER,
         VBELN            TYPE VBEP-VBELN,
         POSNR            TYPE VBEP-POSNR,
         ETENR            TYPE VBEP-ETENR,
         WMENG            TYPE VBEP-WMENG,
         BMENG            TYPE VBEP-BMENG,
         EDATU            TYPE VBEP-EDATU,
         MBDAT            TYPE VBEP-MBDAT,
         LIFSP            TYPE VBEP-LIFSP,
         MATNR            TYPE VBAP-MATNR,
         WERKS            TYPE VBAP-WERKS,
         LGORT            TYPE VBAP-LGORT,
         ERDAT            TYPE VBAP-ERDAT,
         VBAP_OBJNR       TYPE VBAP-OBJNR,
         LIFSK            TYPE VBAK-LIFSK,
         BSTNK            TYPE VBAK-BSTNK,
         VBAK_OBJNR       TYPE VBAK-OBJNR,
         CUST_NAME        TYPE KNA1-NAME1,
         DISTR_CH_NAME    TYPE TVTWT-VTEXT,
         SALESOFFICE_DESC TYPE TVKBT-BEZEI,
         SALESGRP_DESC    TYPE TVGRT-BEZEI,
         STATUS_DESC      TYPE TVBST-BEZEI,
       END OF TS_SALES_ORDER.

TYPES: BEGIN OF TS_SO_QTY,
         MATNR TYPE VBAP-MATNR,
         WERKS TYPE VBAP-WERKS,
         WMENG TYPE VBEP-WMENG,
         BMENG TYPE VBEP-BMENG,
       END OF TS_SO_QTY.

TYPES: BEGIN OF TS_SO_CALC,
         VBELN TYPE VBEP-VBELN,
         POSNR TYPE VBEP-POSNR,
         ETENR TYPE VBEP-ETENR,
         WMENG TYPE VBEP-WMENG,
         BMENG TYPE VBEP-BMENG,
       END OF TS_SO_CALC.

TYPES: BEGIN OF TS_DELIVERY,
         VBELN            TYPE LIPS-VBELN,
         POSNR            TYPE LIPS-POSNR,
         SO_VBELN         TYPE VBEP-VBELN,
         SO_POSNR         TYPE VBEP-POSNR,
         SO_ETENR         TYPE VBEP-ETENR,
         ERDAT            TYPE LIPS-ERDAT,
         LFIMG            TYPE LIPS-LFIMG,
         VGBEL            TYPE LIPS-VGBEL,
         VGPOS            TYPE LIPS-VGPOS,
         LFDAT            TYPE LIKP-LFDAT,
         LIFSK            TYPE LIKP-LIFSK,
         SO_EDATU         TYPE VBEP-EDATU,
         VBAP_OBJNR       TYPE VBAP-OBJNR,
         SO_BSTNK         TYPE VBAK-BSTNK,
         VBAK_OBJNR       TYPE VBAK-OBJNR,
         CUST_NAME        TYPE KNA1-NAME1,
         DISTR_CH_NAME    TYPE TVTWT-VTEXT,
         SALESOFFICE_DESC TYPE TVKBT-BEZEI,
         SALESGRP_DESC    TYPE TVGRT-BEZEI,
         STATUS_DESC      TYPE TVBST-BEZEI,
       END OF TS_DELIVERY.

TYPES: BEGIN OF TS_DO_QTY,
         VGBEL TYPE LIPS-VGBEL,
         VGPOS TYPE LIPS-VGPOS,
         LFIMG TYPE LIPS-LFIMG,
       END OF TS_DO_QTY.

TYPES: BEGIN OF TS_DO_QTY2,
         VBELN TYPE LIPS-VBELN,
         POSNR TYPE LIPS-POSNR,
         VGBEL TYPE LIPS-VGBEL,
         VGPOS TYPE LIPS-VGPOS,
       END OF TS_DO_QTY2.

TYPES: BEGIN OF TS_PURCH_ORDER,
         EBELN TYPE EKPO-EBELN,
         EBELP TYPE EKPO-EBELP,
       END OF TS_PURCH_ORDER.

TYPES: BEGIN OF TS_STORAGE_LOC,
         LGORT TYPE T001L-LGORT,
         LGOBE TYPE T001L-LGOBE,
       END OF TS_STORAGE_LOC.

TYPES: BEGIN OF TS_PARTNER,
         VBELN        TYPE VBPA-VBELN,
         PARVW        TYPE VBPA-PARVW,
         POSNR        TYPE VBPA-POSNR,
         PARTNER_NAME TYPE ADRC-NAME1,
       END OF TS_PARTNER.

TYPES: BEGIN OF TS_INBOUND_DO,
         VBELN TYPE LIPS-VBELN,
         VGBEL TYPE LIPS-VGBEL,
         POSNR TYPE LIPS-POSNR,
         VGPOS TYPE LIPS-VGPOS,
       END OF TS_INBOUND_DO.

TYPES:  TT_RESULT       TYPE STANDARD TABLE OF TS_RESULT.

TYPES:  TT_MATERIAL     TYPE SORTED TABLE OF TS_MATERIAL
                        WITH UNIQUE KEY MATNR
                                        WERKS
                                        LGORT.

TYPES:  TT_SALES_ORDER  TYPE SORTED TABLE OF TS_SALES_ORDER
                        WITH UNIQUE KEY VBELN
                                        POSNR
                                        ETENR.
TYPES:  TT_SALES_ORDER2  TYPE STANDARD TABLE OF TS_SALES_ORDER.

TYPES:  TT_SO_QTY       TYPE SORTED TABLE OF TS_SO_QTY
                        WITH UNIQUE KEY MATNR
                                        WERKS.

TYPES:  TT_SO_CALC      TYPE SORTED TABLE OF TS_SO_CALC
                        WITH UNIQUE KEY VBELN
                                        POSNR
                                        ETENR.

TYPES:  TT_DELIVERY     TYPE SORTED TABLE OF TS_DELIVERY
                        WITH UNIQUE KEY VBELN
                                        POSNR
                                        SO_VBELN
                                        SO_POSNR
                                        SO_ETENR.

TYPES:  TT_DO_QTY       TYPE SORTED TABLE OF TS_DO_QTY
                        WITH UNIQUE KEY VGBEL
                                        VGPOS.

TYPES:  TT_DO_QTY2       TYPE SORTED TABLE OF TS_DO_QTY2
                         WITH UNIQUE KEY VBELN
                                         POSNR
                                         VGBEL
                                         VGPOS.

TYPES:  TT_PURCH_ORDER  TYPE SORTED TABLE OF TS_PURCH_ORDER
                        WITH UNIQUE KEY EBELN
                                        EBELP.

TYPES:  TT_STORAGE_LOC  TYPE SORTED TABLE OF TS_STORAGE_LOC
                        WITH UNIQUE KEY LGORT.

TYPES:  TT_INBOUND_DO   TYPE SORTED TABLE OF TS_INBOUND_DO
                        WITH UNIQUE KEY VBELN
                                        VGBEL
                                        POSNR.
TYPES:  TT_PARTNER      TYPE SORTED TABLE OF TS_PARTNER
                        WITH UNIQUE KEY VBELN
                                        PARVW
                                        POSNR.

*TYPES:  TT_PARTNER      TYPE STANDARD TABLE OF TS_PARTNER.
TYPES:  TT_MRP_IND      TYPE STANDARD TABLE OF BAPI_MRP_IND_LINES.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE CHAR1       VALUE 'X',
  GC_TCODE TYPE SY-TCODE    VALUE 'ZSDSMM008',
  GC_ZFG   TYPE MARA-MTART  VALUE 'ZFG'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_RESULT      TYPE TT_RESULT                            ##NEEDED.
DATA: GT_SALES_ORDER TYPE TT_SALES_ORDER                       ##NEEDED.
DATA: GT_SO_QTY      TYPE TT_SO_QTY                            ##NEEDED.
DATA: GT_SO_CALC     TYPE TT_SO_CALC                           ##NEEDED.
DATA: GT_DELIVERY    TYPE TT_DELIVERY                          ##NEEDED.
DATA: GT_DO_QTY      TYPE TT_DO_QTY                            ##NEEDED.
DATA: GT_PURCH_ORDER TYPE TT_PURCH_ORDER                       ##NEEDED.
DATA: GT_INBOUND_DO  TYPE TT_INBOUND_DO                        ##NEEDED.
DATA: GT_MATERIAL    TYPE TT_MATERIAL                          ##NEEDED.
DATA: GT_STORAGE_LOC TYPE TT_STORAGE_LOC                       ##NEEDED.
DATA: GT_PARTNER     TYPE TT_PARTNER                           ##NEEDED.
DATA: GT_MRP_IND     TYPE TT_MRP_IND                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: GR_AVAIL_SLOC TYPE RANGE OF MARD-LGORT                   ##NEEDED.

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

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1  WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_MATNR TYPE MARD-MATNR OBLIGATORY.
  PARAMETERS: P_WERKS TYPE MARD-WERKS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_GLOBAL_VAR.
  PERFORM F_GET_AVAIL_SLOC.
  PERFORM F_GET_DATA.
  PERFORM F_PREPARE_RESULT.
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
* ALV VARIABLES
*----------------------------------------------------------------------*
  CONSTANTS:
    GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS025',
    GC_HEADER_HEIGHT_1 TYPE  I        VALUE 10,
    GC_ALV_HEIGHT_1    TYPE  I        VALUE 100.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& Get Data
*&---------------------------------------------------------------------*
FORM F_GET_DATA .
  PERFORM F_GET_MAT_STOCK_REQ.
  PERFORM F_GET_MATERIAL.
  PERFORM F_GET_SALES_ORDER.
  PERFORM F_GET_PO_IBD.
  PERFORM F_GET_STORAGE_LOC.
  PERFORM F_GET_PARTNER.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MAT_STOCK_REQ
*&---------------------------------------------------------------------*
*& Get Material Stock Requirment List
*&---------------------------------------------------------------------*
FORM F_GET_MAT_STOCK_REQ .
  DATA: LS_RETURN     TYPE BAPIRET2.
  DATA: LF_MATERIAL      TYPE BAPI_MRP_MAT_PARAM-MATERIAL,
        LF_MATERIAL_LONG TYPE BAPI_MRP_MAT_PARAM-MATERIAL_LONG.
  DATA: LF_PLANT      TYPE BAPI_MRP_MAT_PARAM-PLANT.

*  LF_MATERIAL = P_MATNR.
  LF_MATERIAL_LONG = P_MATNR.
  LF_PLANT         = P_WERKS.

  CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
    EXPORTING
      MATERIAL      = LF_MATERIAL
      MATERIAL_LONG = LF_MATERIAL_LONG
      PLANT         = LF_PLANT
    IMPORTING
      RETURN        = LS_RETURN
    TABLES
      MRP_IND_LINES = GT_MRP_IND.

  IF LS_RETURN-TYPE NE 'S'.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01)  DISPLAY LIKE 'E'.
  ELSE.
    DELETE GT_MRP_IND WHERE PLNGSEGMT NE '02' AND
                            PLNGSEGMT NE '20'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SALES_ORDER
*&---------------------------------------------------------------------*
*& Get Sales Order
*&---------------------------------------------------------------------*
FORM F_GET_SALES_ORDER .
  DATA: BEGIN OF LS_COLLECT_VBEP,
          VBELN TYPE VBEP-VBELN,
          POSNR TYPE VBEP-POSNR,
          WMENG TYPE VBEP-WMENG,
          BMENG TYPE VBEP-BMENG,
        END OF LS_COLLECT_VBEP,
        LT_COLLECT_VBEP    LIKE TABLE OF LS_COLLECT_VBEP,
        LT_SALES_ORDER     TYPE TT_SALES_ORDER2,
        LT_SALES_ORDER_TMP TYPE TT_SALES_ORDER2,
        LS_SALES_ORDER     TYPE TS_SALES_ORDER.
  DATA: LF_DO_QTY TYPE LIPS-LFIMG.
  DATA: LF_WMENG  TYPE VBEP-WMENG.
  DATA: LF_BMENG  TYPE VBEP-BMENG.

  CHECK NOT GT_MATERIAL IS INITIAL.

  SELECT FROM VBEP
  INNER JOIN VBAP ON  VBAP~VBELN  = VBEP~VBELN
                  AND VBAP~POSNR  = VBEP~POSNR
  INNER JOIN VBAK ON  VBAK~VBELN  = VBEP~VBELN
                  AND VBAK~VBTYP  = 'C' "Order
  LEFT OUTER JOIN VBUK  ON  VBUK~VBELN  = VBAK~VBELN
  LEFT OUTER JOIN KNA1  ON  KNA1~KUNNR  = VBAK~KUNNR
  LEFT OUTER JOIN TVTWT ON  TVTWT~SPRAS = @SY-LANGU
                        AND TVTWT~VTWEG = VBAK~VTWEG
  LEFT OUTER JOIN TVKBT ON  TVKBT~SPRAS = @SY-LANGU
                        AND TVKBT~VKBUR = VBAK~VKBUR
  LEFT OUTER JOIN TVGRT ON  TVGRT~SPRAS = @SY-LANGU
                        AND TVGRT~VKGRP = VBAK~VKGRP
  LEFT OUTER JOIN TVBST ON  TVBST~SPRAS = @SY-LANGU
                        AND TVBST~TBNAM = 'VBUK'
                        AND TVBST~FDNAM = 'CMGST'
                        AND TVBST~STATU = VBUK~CMGST   "#EC CI_BUFFJOIN
  FIELDS VBEP~VBELN,
         VBEP~POSNR,
         VBEP~ETENR,
         VBEP~WMENG,
         VBEP~BMENG,
         VBEP~EDATU,
         VBEP~MBDAT,
         VBEP~LIFSP,
         VBAP~MATNR,
         VBAP~WERKS,
         VBAP~LGORT,
         VBAP~ERDAT,
         VBAP~OBJNR  AS VBAP_OBJNR,
         VBAK~LIFSK,
         VBAK~BSTNK,
         VBAK~OBJNR  AS VBAK_OBJNR,
         KNA1~NAME1  AS CUST_NAME,
         TVTWT~VTEXT AS DISTR_CH_NAME,
         TVKBT~BEZEI AS SALESOFFICE_DESC,
         TVGRT~BEZEI AS SALESGROUP_DESC,
         TVBST~BEZEI AS STATUS_DESC
  WHERE VBAP~MATNR  EQ @P_MATNR
    AND VBAP~WERKS  EQ @P_WERKS
    AND VBAP~ABGRU  EQ @SPACE "420000543++
  INTO TABLE @GT_SALES_ORDER.
* >>>>> v1.00 INSERT START >>>>> *
  IF SY-SUBRC = 0.
    "collect schedule line
    LT_SALES_ORDER_TMP = GT_SALES_ORDER.
    LOOP AT LT_SALES_ORDER_TMP ASSIGNING FIELD-SYMBOL(<LFS_SALES_ORDER>).
      MOVE-CORRESPONDING <LFS_SALES_ORDER> TO LS_COLLECT_VBEP.
      COLLECT LS_COLLECT_VBEP INTO LT_COLLECT_VBEP.
    ENDLOOP.

    SORT LT_SALES_ORDER_TMP BY VBELN POSNR EDATU DESCENDING.
    LOOP AT LT_COLLECT_VBEP INTO LS_COLLECT_VBEP.
      CLEAR: LS_SALES_ORDER.
      READ TABLE LT_SALES_ORDER_TMP INTO LS_SALES_ORDER WITH KEY VBELN = LS_COLLECT_VBEP-VBELN
                                                                 POSNR = LS_COLLECT_VBEP-POSNR
                                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LS_COLLECT_VBEP TO LS_SALES_ORDER.
        APPEND LS_SALES_ORDER TO LT_SALES_ORDER.
      ENDIF.
    ENDLOOP.
    GT_SALES_ORDER = LT_SALES_ORDER.
  ENDIF.
* <<<<< v1.00 INSERT END   <<<<< *

  PERFORM F_GET_DELIVERY.

  LOOP AT GT_SALES_ORDER ASSIGNING FIELD-SYMBOL(<L_SALES_ORDER>).
    CLEAR: LF_DO_QTY,
           LF_WMENG,
           LF_BMENG.

    "Get DO Qty
    READ TABLE GT_DO_QTY ASSIGNING FIELD-SYMBOL(<L_DO_QTY>)
    WITH KEY  VGBEL = <L_SALES_ORDER>-VBELN
              VGPOS = <L_SALES_ORDER>-POSNR
    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LF_DO_QTY = <L_DO_QTY>-LFIMG.
    ENDIF.
    LF_WMENG  = <L_SALES_ORDER>-WMENG - LF_DO_QTY.
    LF_BMENG  = <L_SALES_ORDER>-BMENG - LF_DO_QTY.

    IF LF_WMENG IS INITIAL AND
       LF_BMENG IS INITIAL.
      CONTINUE.
    ENDIF.

    "Append SO Stock at Mat/Plant level
    READ TABLE GT_SO_QTY ASSIGNING FIELD-SYMBOL(<L_SO_QTY>)
    WITH KEY  MATNR = <L_SALES_ORDER>-MATNR
              WERKS = <L_SALES_ORDER>-WERKS
    BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_SO_QTY( MATNR  = <L_SALES_ORDER>-MATNR
                              WERKS  = <L_SALES_ORDER>-WERKS
                              WMENG  = <L_SALES_ORDER>-WMENG - LF_DO_QTY
                              BMENG  = <L_SALES_ORDER>-BMENG - LF_DO_QTY )
      INTO TABLE GT_SO_QTY
      ASSIGNING <L_SO_QTY>.
    ELSE.
*     Sum Qty at Mat/Plant level
      <L_SO_QTY>-WMENG = <L_SO_QTY>-WMENG + ( <L_SALES_ORDER>-WMENG - LF_DO_QTY ).
      <L_SO_QTY>-BMENG = <L_SO_QTY>-BMENG + ( <L_SALES_ORDER>-BMENG - LF_DO_QTY ).
    ENDIF.

    "Append SO Stock at VBEP level
    READ TABLE GT_SO_CALC ASSIGNING FIELD-SYMBOL(<L_SO_CALC>)
    WITH KEY  VBELN = <L_SALES_ORDER>-VBELN
              POSNR = <L_SALES_ORDER>-POSNR
              ETENR = <L_SALES_ORDER>-ETENR
    BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_SO_CALC( VBELN  = <L_SALES_ORDER>-VBELN
                               POSNR  = <L_SALES_ORDER>-POSNR
                               ETENR  = <L_SALES_ORDER>-ETENR
                               WMENG  = <L_SALES_ORDER>-WMENG - LF_DO_QTY
                               BMENG  = <L_SALES_ORDER>-BMENG - LF_DO_QTY )
      INTO TABLE GT_SO_CALC
      ASSIGNING <L_SO_CALC>.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DELIVERY
*&---------------------------------------------------------------------*
*& Get Delivery
*&---------------------------------------------------------------------*
FORM F_GET_DELIVERY .
  DATA: LT_DO_QTY2 TYPE TT_DO_QTY2.

  CHECK NOT GT_SALES_ORDER IS INITIAL.

  SELECT FROM LIPS                                 "#EC CI_NO_TRANSFORM
  INNER JOIN LIKP ON  LIKP~VBELN  = LIPS~VBELN
  INNER JOIN VBEP ON  VBEP~VBELN  = LIPS~VGBEL
                  AND VBEP~POSNR  = LIPS~VGPOS
  LEFT OUTER JOIN VBAP  ON  VBAP~VBELN  = VBEP~VBELN
                        AND VBAP~POSNR  = VBEP~POSNR
  LEFT OUTER JOIN VBAK  ON  VBAK~VBELN  = VBAP~VBELN
  LEFT OUTER JOIN VBUK  ON  VBUK~VBELN  = VBAK~VBELN
  LEFT OUTER JOIN KNA1  ON  KNA1~KUNNR  = LIKP~KUNAG
  LEFT OUTER JOIN TVTWT ON  TVTWT~SPRAS = @SY-LANGU
                        AND TVTWT~VTWEG = VBAK~VTWEG
  LEFT OUTER JOIN TVKBT ON  TVKBT~SPRAS = @SY-LANGU
                        AND TVKBT~VKBUR = VBAK~VKBUR
  LEFT OUTER JOIN TVGRT ON  TVGRT~SPRAS = @SY-LANGU
                        AND TVGRT~VKGRP = VBAK~VKGRP
  LEFT OUTER JOIN TVBST ON  TVBST~SPRAS = @SY-LANGU
                        AND TVBST~TBNAM = 'VBUK'
                        AND TVBST~FDNAM = 'CMGST'
                        AND TVBST~STATU = VBUK~CMGST   "#EC CI_BUFFJOIN
  FIELDS LIPS~VBELN,
         LIPS~POSNR,
         VBEP~VBELN AS SO_VBELN,
         VBEP~POSNR AS SO_POSNR,
         VBEP~ETENR AS SO_ETENR,
         LIPS~ERDAT,
         LIPS~LFIMG,
         LIPS~VGBEL,
         LIPS~VGPOS,
         LIKP~LFDAT,
         LIKP~LIFSK,
         VBEP~EDATU AS SO_EDATU,
         VBAP~OBJNR AS VBAP_OBJNR,
         VBAK~BSTNK AS SO_BSTNK,
         VBAK~OBJNR AS VBAK_OBJNR,
         KNA1~NAME1 AS CUST_NAME,
         TVTWT~VTEXT AS DISTR_CH_NAME,
         TVKBT~BEZEI AS SALESOFFICE_DESC,
         TVGRT~BEZEI AS SALESGROUP_DESC,
         TVBST~BEZEI AS STATUS_DESC
  FOR ALL ENTRIES IN @GT_SALES_ORDER
  WHERE LIPS~VGBEL EQ @GT_SALES_ORDER-VBELN
    AND LIPS~VGPOS EQ @GT_SALES_ORDER-POSNR
  INTO TABLE @GT_DELIVERY.

  LOOP AT GT_DELIVERY ASSIGNING FIELD-SYMBOL(<L_DELIVERY>).
    "Append DO Stock at ref SO level
    READ TABLE LT_DO_QTY2 ASSIGNING FIELD-SYMBOL(<L_DO_QTY2>)
    WITH KEY  VBELN = <L_DELIVERY>-VBELN
              POSNR = <L_DELIVERY>-POSNR
              VGBEL = <L_DELIVERY>-VGBEL
              VGPOS = <L_DELIVERY>-VGPOS
    BINARY SEARCH.
    IF SY-SUBRC NE 0.
* >>>>> v2.00 DELETE START >>>>> *
*      INSERT VALUE TS_DO_QTY( VGBEL = <L_DELIVERY>-VGBEL
*                              VGPOS = <L_DELIVERY>-VGPOS
*                              LFIMG = <L_DELIVERY>-LFIMG )
*      INTO TABLE GT_DO_QTY
*      ASSIGNING <L_DO_QTY>.
* <<<<< v2.00 DELETE END   <<<<< *
* >>>>> v2.00 INSERT START >>>>> *
      INSERT VALUE TS_DO_QTY2( VBELN = <L_DELIVERY>-VBELN
                               POSNR = <L_DELIVERY>-POSNR
                               VGBEL = <L_DELIVERY>-VGBEL
                               VGPOS = <L_DELIVERY>-VGPOS
                                )
      INTO TABLE LT_DO_QTY2
      ASSIGNING <L_DO_QTY2>.

      COLLECT VALUE TS_DO_QTY( VGBEL = <L_DELIVERY>-VGBEL
                               VGPOS = <L_DELIVERY>-VGPOS
                               LFIMG = <L_DELIVERY>-LFIMG )
      INTO GT_DO_QTY.
* <<<<< v2.00 INSERT END   <<<<< *
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PO_IBD
*&---------------------------------------------------------------------*
*& Get Purchase Order and Inbound Delivery
*&---------------------------------------------------------------------*
FORM F_GET_PO_IBD .
  DATA: LRT_EBELN  TYPE RANGE OF EKES-EBELN.
  DATA: LRT_VBELN  TYPE RANGE OF LIPS-VBELN.
  DATA: LS_EBELN  LIKE LINE OF LRT_EBELN.
  DATA: LS_VBELN  LIKE LINE OF LRT_VBELN.
  DATA: LF_EBELN  TYPE EKPO-EBELN ##NEEDED.
  DATA: LF_EBELP  TYPE EKPO-EBELP ##NEEDED.
  DATA: LF_VBELN  TYPE LIPS-VBELN ##NEEDED.

  CHECK NOT GT_MRP_IND IS INITIAL.

  LOOP AT GT_MRP_IND ASSIGNING FIELD-SYMBOL(<L_MRP_IND>)
    WHERE MRP_ELEMENT_IND EQ 'LA' OR
          MRP_ELEMENT_IND EQ 'BE'.
    CLEAR: LF_EBELN, LF_EBELP, LF_VBELN.

    "Collect PO
    SPLIT <L_MRP_IND>-ELEMNT_DATA AT '/' INTO LF_EBELN
                                              LF_EBELP.
    CLEAR LS_EBELN.
    LS_EBELN      = 'IEQ'.
    LS_EBELN-LOW  = LF_EBELN.
    APPEND LS_EBELN TO LRT_EBELN.

    "Collect
    SPLIT <L_MRP_IND>-ELEMNT_DATA AT '/' INTO LF_VBELN
                                              LF_EBELN.
    CLEAR LS_VBELN.
    LS_VBELN      = 'IEQ'.
    LS_VBELN-LOW  = LF_VBELN.
    APPEND LS_VBELN TO LRT_VBELN.
  ENDLOOP.

  IF NOT LRT_EBELN IS INITIAL.
    SELECT FROM EKPO
    FIELDS EBELN,
           EBELP
    WHERE EBELN IN @LRT_EBELN
    INTO TABLE @GT_PURCH_ORDER.
  ENDIF.

  IF NOT LRT_VBELN IS INITIAL.
    SELECT FROM LIPS
    FIELDS VBELN,
           VGBEL,
           POSNR,
           VGPOS
    WHERE VBELN IN @LRT_VBELN
    INTO TABLE @GT_INBOUND_DO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MATERIAL
*&---------------------------------------------------------------------*
*& Get Material
*&---------------------------------------------------------------------*
FORM F_GET_MATERIAL .
  SELECT FROM MARD
  FIELDS MATNR,
         WERKS,
         LGORT,
         LABST,
         UMLME,
         INSME,
         SPEME
  WHERE MATNR EQ @P_MATNR
    AND WERKS EQ @P_WERKS
  INTO TABLE @GT_MATERIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INIT_GLOBAL_VAR
*&---------------------------------------------------------------------*
*& Initialize Global Variables
*&---------------------------------------------------------------------*
FORM F_INIT_GLOBAL_VAR .
  CLEAR: GT_RESULT,
         GT_SALES_ORDER,
         GT_SO_QTY,
         GT_SO_CALC,
         GT_DELIVERY,
         GT_DO_QTY,
         GT_PURCH_ORDER,
         GT_MATERIAL,
         GT_STORAGE_LOC,
         GT_PARTNER,
         GT_MRP_IND,
         GR_AVAIL_SLOC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_RECALC_STOCK_QTY
*&---------------------------------------------------------------------*
*& Re-calculate Stock Qty
*&---------------------------------------------------------------------*
FORM F_RECALC_STOCK_QTY
  USING UF_UNRES_STCK TYPE BAPI_MRP_STOCK_DETAIL-UNRESTRICTED_STCK.

  DATA: LF_UNRES_STCK TYPE BAPI_MRP_STOCK_DETAIL-UNRESTRICTED_STCK.

  LOOP AT GT_MATERIAL ASSIGNING FIELD-SYMBOL(<L_MATERIAL>)
    WHERE LGORT IN GR_AVAIL_SLOC.                       "#EC CI_SORTSEQ
    LF_UNRES_STCK = LF_UNRES_STCK + <L_MATERIAL>-LABST.
  ENDLOOP.

  UF_UNRES_STCK = LF_UNRES_STCK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CALC_FREE_STOCK
*&---------------------------------------------------------------------*
*& Calculate Free Stock
*&---------------------------------------------------------------------*
FORM F_CALC_FREE_STOCK
  USING UF_UNRES_STCK TYPE BAPI_MRP_STOCK_DETAIL-UNRESTRICTED_STCK
        UF_FREEQTY    TYPE ZSDSMMS025-FREEQTY.

  DATA: LF_CONFIRM_SO       TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.
  DATA: LF_DELIVERY_STCK    TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.
  DATA: LF_RESERVED_STCK    TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.
  DATA: LF_SALES_ORDER_STCK TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.
  DATA: LF_REQD_QTY         TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.

  LOOP AT GT_MRP_IND ASSIGNING FIELD-SYMBOL(<L__MRP_IND>).
    LF_REQD_QTY = <L__MRP_IND>-REC_REQD_QTY.
    IF <L__MRP_IND>-PLUS_MINUS EQ '-'.
      LF_REQD_QTY = LF_REQD_QTY * -1.
    ENDIF.

    CASE <L__MRP_IND>-MRP_ELEMENT_IND.
      WHEN 'VJ'.
        LF_DELIVERY_STCK  = LF_DELIVERY_STCK + LF_REQD_QTY.
      WHEN 'MR'.
        LF_RESERVED_STCK  = LF_RESERVED_STCK + LF_REQD_QTY.
      WHEN 'KB'.
        IF NOT <L__MRP_IND>-REC_REQD_QTY IS INITIAL.
          LF_SALES_ORDER_STCK = LF_SALES_ORDER_STCK + LF_REQD_QTY.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  LOOP AT GT_SO_QTY ASSIGNING FIELD-SYMBOL(<L_SO_QTY>).
    LF_CONFIRM_SO = LF_CONFIRM_SO + <L_SO_QTY>-BMENG.
  ENDLOOP.

  UF_FREEQTY  = UF_UNRES_STCK
                  - LF_RESERVED_STCK
                  - LF_CONFIRM_SO
                  - LF_DELIVERY_STCK
                  + LF_SALES_ORDER_STCK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_RESULT
*&---------------------------------------------------------------------*
*& Prepare Result
*&---------------------------------------------------------------------*
FORM F_PREPARE_RESULT .
  DATA: LS_RESULT       LIKE LINE OF GT_RESULT.
  DATA: LS_DELIVERY     LIKE LINE OF GT_DELIVERY.
  DATA: LS_PURCH_ORDER  LIKE LINE OF GT_PURCH_ORDER.
  DATA: LS_INBOUND_DO   LIKE LINE OF GT_INBOUND_DO.
  DATA: LF_UNRES_STCK   TYPE BAPI_MRP_STOCK_DETAIL-UNRESTRICTED_STCK.
  DATA: LF_FREEQTY      TYPE ZSDSMMS025-FREEQTY.
  DATA: LF_REQD_QTY     TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.

  DELETE GT_MRP_IND WHERE NOT STORAGE_LOC IS INITIAL
                      AND NOT STORAGE_LOC IN GR_AVAIL_SLOC.

  PERFORM F_RECALC_STOCK_QTY USING LF_UNRES_STCK.
  PERFORM F_CALC_FREE_STOCK  USING LF_UNRES_STCK
                                   LF_FREEQTY.

  LOOP AT GT_MRP_IND ASSIGNING FIELD-SYMBOL(<L_MRP_IND>).
    CLEAR: LS_RESULT,
           LS_DELIVERY,
           LS_PURCH_ORDER,
           LS_INBOUND_DO,
           LF_REQD_QTY.

    LS_RESULT-SORT_DATE   = <L_MRP_IND>-SORT_DATE.
    LS_RESULT-SORTIND_00  = <L_MRP_IND>-SORTIND_00.
    LS_RESULT-SORTIND_01  = <L_MRP_IND>-SORTIND_01.
    LS_RESULT-SORTIND_02  = <L_MRP_IND>-SORTIND_02.

    LF_REQD_QTY = <L_MRP_IND>-REC_REQD_QTY.
    IF <L_MRP_IND>-PLUS_MINUS EQ '-'.
      LF_REQD_QTY = LF_REQD_QTY * -1.
    ENDIF.

    READ TABLE GT_STORAGE_LOC ASSIGNING FIELD-SYMBOL(<L_STORAGE_LOC>)
    WITH KEY LGORT = <L_MRP_IND>-STORAGE_LOC
    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LS_RESULT-SLOCCODE_ACT  = <L_STORAGE_LOC>-LGORT.
      LS_RESULT-SLOC_ACT      = <L_STORAGE_LOC>-LGOBE.
    ENDIF.

    CASE <L_MRP_IND>-MRP_ELEMENT_IND.
      WHEN 'WB'.
        LS_RESULT-CATEGORY  = 'Stock(SDS)'(A01).
        LS_RESULT-MBDAT     = SY-DATUM.
        LS_RESULT-QTY       = LF_UNRES_STCK.
        LS_RESULT-LINECOLOR = 'C100'.
        LS_RESULT-FREEQTY   = LF_FREEQTY.

      WHEN 'VJ'.
        LS_RESULT-CATEGORY      = 'DeliveryOrder'(A03).
        LS_RESULT-SLOCCODE_ACT  = <L_MRP_IND>-STORAGE_LOC.
        LS_RESULT-SQDAT         = <L_MRP_IND>-AVAIL_DATE.
        LS_RESULT-SORT_DATE     = <L_MRP_IND>-AVAIL_DATE.
        LS_RESULT-DOCNO         = <L_MRP_IND>-ELEMNT_DATA.
        LS_RESULT-REQQTY        = LF_REQD_QTY.
        LS_RESULT-CONQTY        = LF_REQD_QTY.

        PERFORM F_READ_DELIVERY USING <L_MRP_IND>-ELEMNT_DATA
                                      LS_DELIVERY.
        IF NOT LS_DELIVERY IS INITIAL.
          PERFORM F_READ_PARTNER_DO USING LS_DELIVERY
                                          LS_RESULT.
          LS_RESULT-INPUTDAT  = LS_DELIVERY-ERDAT.
          LS_RESULT-NAME1     = LS_DELIVERY-CUST_NAME.
          LS_RESULT-BSTNK     = LS_DELIVERY-SO_BSTNK.
          LS_RESULT-LIFSK     = LS_DELIVERY-LIFSK.
          LS_RESULT-REQETD    = LS_DELIVERY-SO_EDATU.
          LS_RESULT-ACTETD    = LS_DELIVERY-LFDAT.
        ENDIF.

      WHEN 'BE'.
        LS_RESULT-CATEGORY      = 'PurchaseOrder'(A04).
        LS_RESULT-SLOCCODE_ACT  = <L_MRP_IND>-STORAGE_LOC.
        LS_RESULT-MBDAT         = <L_MRP_IND>-AVAIL_DATE.
        LS_RESULT-SORT_DATE     = <L_MRP_IND>-AVAIL_DATE.
        LS_RESULT-DOCNO         = <L_MRP_IND>-ELEMNT_DATA.
        LS_RESULT-QTY           = LF_REQD_QTY.

      WHEN 'LA'.
        PERFORM F_READ_PO_IBD USING <L_MRP_IND>-ELEMNT_DATA
                                    LS_PURCH_ORDER
                                    LS_INBOUND_DO.
        IF NOT LS_PURCH_ORDER IS INITIAL.
          LS_RESULT-CATEGORY  = 'PurchaseOrder'(A04).
        ELSE.
          LS_RESULT-CATEGORY  = 'DeliveryNote'(A05).
        ENDIF.

        IF NOT LS_PURCH_ORDER IS INITIAL.
          LS_RESULT-DOCNO = <L_MRP_IND>-ELEMNT_DATA.
        ELSEIF NOT LS_INBOUND_DO IS INITIAL.
          LS_RESULT-DOCNO =
            |{ LS_INBOUND_DO-VBELN }/{ LS_INBOUND_DO-VGBEL }/{ LS_INBOUND_DO-VGPOS }|.
        ELSE.
          LS_RESULT-DOCNO = <L_MRP_IND>-ELEMNT_DATA.
        ENDIF.

        LS_RESULT-SLOCCODE_ACT  = <L_MRP_IND>-STORAGE_LOC.
        LS_RESULT-MBDAT         = <L_MRP_IND>-AVAIL_DATE.
        LS_RESULT-SORT_DATE     = <L_MRP_IND>-AVAIL_DATE.
        LS_RESULT-QTY           = LF_REQD_QTY.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    APPEND LS_RESULT TO GT_RESULT.
  ENDLOOP.

  "Append Sales Order Stock
  LOOP AT GT_SALES_ORDER ASSIGNING FIELD-SYMBOL(<L_SALES_ORDER>).
    READ TABLE GT_SO_CALC ASSIGNING FIELD-SYMBOL(<L_SO_CALC>)
    WITH KEY VBELN  = <L_SALES_ORDER>-VBELN
             POSNR  = <L_SALES_ORDER>-POSNR
             ETENR  = <L_SALES_ORDER>-ETENR
    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      CLEAR LS_RESULT.
      LS_RESULT-CATEGORY    = 'SalesOrder'(A02).
      LS_RESULT-SQDAT       = <L_SALES_ORDER>-MBDAT.
      LS_RESULT-SORT_DATE   = <L_SALES_ORDER>-MBDAT.
      LS_RESULT-SORTIND_00  = '00'.
      LS_RESULT-SORTIND_01  = '2'.
      LS_RESULT-SORTIND_02  = '03'.
      LS_RESULT-REQQTY      = <L_SO_CALC>-WMENG.
      LS_RESULT-CONQTY      = <L_SO_CALC>-BMENG.
      LS_RESULT-NAME1       = <L_SALES_ORDER>-CUST_NAME.
      LS_RESULT-BSTNK       = <L_SALES_ORDER>-BSTNK.
      LS_RESULT-REQETD      = <L_SALES_ORDER>-EDATU.
      LS_RESULT-INPUTDAT    = <L_SALES_ORDER>-ERDAT.
      LS_RESULT-ACTETD      = <L_SALES_ORDER>-EDATU.
      LS_RESULT-VTWEGT      = <L_SALES_ORDER>-DISTR_CH_NAME.
      LS_RESULT-VKBURT      = <L_SALES_ORDER>-SALESOFFICE_DESC.
      LS_RESULT-VKGRPT      = <L_SALES_ORDER>-SALESGRP_DESC.

      CONCATENATE <L_SALES_ORDER>-VBELN
                  <L_SALES_ORDER>-POSNR
                  <L_SALES_ORDER>-ETENR
             INTO LS_RESULT-DOCNO
             SEPARATED BY '/'.

      IF NOT <L_SALES_ORDER>-LIFSK IS INITIAL.
        LS_RESULT-LIFSK = <L_SALES_ORDER>-LIFSK.
      ELSE.
        LS_RESULT-LIFSK = <L_SALES_ORDER>-LIFSP.
      ENDIF.
      PERFORM F_READ_PARTNER_SO USING <L_SALES_ORDER>
                                      LS_RESULT.
      APPEND LS_RESULT TO GT_RESULT.
    ENDIF.
  ENDLOOP.

  SORT GT_RESULT BY SORT_DATE SORTIND_00 SORTIND_01 SORTIND_02 ASCENDING.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_DELIVERY
*&---------------------------------------------------------------------*
*& Read Delivery
*&---------------------------------------------------------------------*
FORM F_READ_DELIVERY
  USING UF_MRP_ELEMNT_DATA  TYPE BAPI_MRP_IND_LINES-ELEMNT_DATA
        US_DELIVERY  TYPE TS_DELIVERY.

  DATA: LS_DELIVERY LIKE LINE OF GT_DELIVERY.
  DATA: LF_VBELN    TYPE LIPS-VBELN.
  DATA: LF_POSNR    TYPE LIPS-POSNR.
  DATA: LF_ETENR    TYPE VBEP-ETENR ##NEEDED.

  CHECK NOT UF_MRP_ELEMNT_DATA IS INITIAL.

  SPLIT UF_MRP_ELEMNT_DATA AT '/' INTO LF_VBELN
                                       LF_POSNR
                                       LF_ETENR.

  READ TABLE GT_DELIVERY INTO LS_DELIVERY
  WITH KEY VBELN = LF_VBELN
           POSNR = LF_POSNR
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    US_DELIVERY  = LS_DELIVERY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_PO_IBD
*&---------------------------------------------------------------------*
*& Read Purchase Order or Inbound Delivery
*&---------------------------------------------------------------------*
FORM F_READ_PO_IBD
  USING UF_MRP_ELEMNT_DATA  TYPE BAPI_MRP_IND_LINES-ELEMNT_DATA
        US_PURCH_ORDER      TYPE TS_PURCH_ORDER
        US_INBOUND_DO       TYPE TS_INBOUND_DO.

  DATA: LS_PURCH_ORDER  LIKE LINE OF GT_PURCH_ORDER.
  DATA: LS_INBOUND_DO   LIKE LINE OF GT_INBOUND_DO.
  DATA: LF_EBELN        TYPE EKES-EBELN.
  DATA: LF_EBELP        TYPE EKES-EBELP.
  DATA: LF_VBELN        TYPE LIPS-VBELN.

  CHECK NOT UF_MRP_ELEMNT_DATA IS INITIAL.

  SPLIT UF_MRP_ELEMNT_DATA AT '/' INTO LF_EBELN
                                       LF_EBELP.

  READ TABLE GT_PURCH_ORDER INTO LS_PURCH_ORDER
  WITH KEY EBELN = LF_EBELN
           EBELP = LF_EBELP
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    US_PURCH_ORDER  = LS_PURCH_ORDER.
  ELSE.
    SPLIT UF_MRP_ELEMNT_DATA AT '/' INTO LF_VBELN
                                         LF_EBELN.
    READ TABLE GT_INBOUND_DO INTO LS_INBOUND_DO
    WITH KEY VBELN  = LF_VBELN
             VGBEL  = LF_EBELN
    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      US_INBOUND_DO = LS_INBOUND_DO.
    ENDIF.
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
  GF_ALV_HEADER_1 = ABAP_TRUE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

  GF_SECOND_ALV_1 = SPACE.

* ALV Layout
  PERFORM F_ALV_LAYOUT_1 CHANGING GS_LAYOUT_1
                                  GS_VARIANT_1
                                  GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT_1 CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT_1 CHANGING GT_SORT_1.
** Call ALV Screen
  CALL SCREEN 9000.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT_1
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report 1
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT_1 CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT  TYPE  STRING ##NEEDED.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.
    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'ZINDEX'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'REQQTY'.
        <L_FIELDCAT>-DO_SUM = GC_TRUE.
      WHEN 'QTY'.
        <L_FIELDCAT>-DO_SUM = GC_TRUE.
      WHEN 'CONQTY'.
        <L_FIELDCAT>-DO_SUM = GC_TRUE.
      WHEN 'FREEQTY'.
        <L_FIELDCAT>-DO_SUM = GC_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT_1
*----------------------------------------------------------------------*
*  ALV Layout for Report 1
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT_1 CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                             CS_VARIANT TYPE  DISVARIANT
                             CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-CWIDTH_OPT  = GC_TRUE.
  CS_LAYOUT-ZEBRA       = GC_TRUE.
  CS_LAYOUT-INFO_FNAME  = 'LINECOLOR'.   "Line color info

* For Variant Saving
  CS_VARIANT-REPORT   = SY-REPID.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT_1
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV 1
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT_1  CHANGING CT_SORT TYPE LVC_T_SORT.

* Initialize Output
  CLEAR: CT_SORT.

** Sort by CARRID
*  CLEAR LS_SORT.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_1
*----------------------------------------------------------------------*
*  ALV Hotspot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID    TYPE LVC_S_ROW  ##CALLED
                             US_COLUMN_ID TYPE LVC_S_COL  ##NEEDED.

  DATA: LS_RESULT   LIKE LINE OF GT_RESULT.
  DATA: LREF_EXCEPT TYPE REF TO CX_SY_AUTHORIZATION_ERROR.
  DATA: LF_VBELN    TYPE VBELN  ##NEEDED.
  DATA: LF_POSNR    TYPE POSNR  ##NEEDED.
  DATA: LF_ETENR    TYPE ETENR  ##NEEDED.
  DATA: LF_EBELN    TYPE EBELN  ##NEEDED.
  DATA: LF_EBELP    TYPE EBELP  ##NEEDED.
  DATA: LF_MESSAGE  TYPE STRING ##NEEDED.

* Read Line
  READ TABLE GT_RESULT INTO LS_RESULT INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC EQ 0.
    SPLIT LS_RESULT-DOCNO AT '/' INTO LF_VBELN LF_POSNR LF_ETENR.

    CASE LS_RESULT-CATEGORY.
      WHEN 'SalesOrder'.
        TRY.
            SET PARAMETER ID 'AUN' FIELD LF_VBELN.
            CALL TRANSACTION 'VA03' WITH AUTHORITY-CHECK
                                    AND SKIP FIRST SCREEN .
          CATCH CX_SY_AUTHORIZATION_ERROR INTO LREF_EXCEPT.
            CALL METHOD LREF_EXCEPT->IF_MESSAGE~GET_TEXT
              RECEIVING
                RESULT = LF_MESSAGE.
            MESSAGE LF_MESSAGE  TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

      WHEN 'DeliveryOrder'.
        TRY.
            SET PARAMETER ID 'VL' FIELD LF_VBELN.
            CALL TRANSACTION 'VL03N'  WITH AUTHORITY-CHECK
                                      AND SKIP FIRST SCREEN.
          CATCH CX_SY_AUTHORIZATION_ERROR INTO LREF_EXCEPT.
            CALL METHOD LREF_EXCEPT->IF_MESSAGE~GET_TEXT
              RECEIVING
                RESULT = LF_MESSAGE.
            MESSAGE LF_MESSAGE  TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

      WHEN 'PurchaseOrder' OR 'DeliveryNote'.
        TRY.
            SPLIT LS_RESULT-DOCNO AT '/' INTO LF_EBELN
                                              LF_EBELP.

            READ TABLE GT_PURCH_ORDER
            WITH KEY EBELN = LF_EBELN
                     EBELP = LF_EBELP
            BINARY SEARCH
            TRANSPORTING NO FIELDS.
            IF SY-SUBRC EQ 0.
              SET PARAMETER ID 'BES' FIELD LF_EBELN.
              SET PARAMETER ID 'BSP' FIELD LF_EBELP.
              CALL TRANSACTION 'ME23N'  WITH AUTHORITY-CHECK
                                        AND SKIP FIRST SCREEN.
            ELSE.
              SPLIT LS_RESULT-DOCNO AT '/' INTO LF_VBELN
                                                LF_EBELN
                                                LF_EBELP.

              READ TABLE GT_INBOUND_DO
              WITH KEY VBELN  = LF_VBELN
                       VGBEL  = LF_EBELN
              BINARY SEARCH
              TRANSPORTING NO FIELDS.
              IF SY-SUBRC EQ 0.
                SET PARAMETER ID 'BES' FIELD LF_EBELN.
                SET PARAMETER ID 'BSP' FIELD LF_EBELP.
                CALL TRANSACTION 'ME23N'  WITH AUTHORITY-CHECK
                                          AND SKIP FIRST SCREEN.
              ENDIF.
            ENDIF.

          CATCH CX_SY_AUTHORIZATION_ERROR INTO LREF_EXCEPT.
            CALL METHOD LREF_EXCEPT->IF_MESSAGE~GET_TEXT
              RECEIVING
                RESULT = LF_MESSAGE.
            MESSAGE LF_MESSAGE  TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

      WHEN 'Stock(SDS)'.
        TRY.
            SET PARAMETER ID 'MAT' FIELD P_MATNR.
            SET PARAMETER ID 'WRK' FIELD P_WERKS.
            CALL TRANSACTION 'MMBE' WITH AUTHORITY-CHECK
                                    AND SKIP FIRST SCREEN.
          CATCH CX_SY_AUTHORIZATION_ERROR INTO LREF_EXCEPT.
            CALL METHOD LREF_EXCEPT->IF_MESSAGE~GET_TEXT
              RECEIVING
                RESULT = LF_MESSAGE.
            MESSAGE LF_MESSAGE  TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_PARTNER_SO
*&---------------------------------------------------------------------*
*& Read Partner for SO
*&---------------------------------------------------------------------*
FORM F_READ_PARTNER_SO  USING  US_SALES_ORDER  TYPE TS_SALES_ORDER
                               US_RESULT       TYPE TS_RESULT.
  DATA: LS_PARTNER  LIKE LINE OF GT_PARTNER.
  DATA: LT_XVBADR   TYPE STANDARD TABLE OF SADRVB.
  DATA: LT_XVBPA    TYPE STANDARD TABLE OF VBPAVB.
  DATA: LT_STATUS   TYPE STANDARD TABLE OF JSTAT.
  DATA: LT_LINES    TYPE STANDARD TABLE OF TLINE.
  DATA: LS_XVBPA    LIKE LINE OF LT_XVBPA.
  DATA: LS_STATUS   LIKE LINE OF LT_STATUS.
  DATA: LS_LINE     LIKE LINE OF LT_LINES.
  DATA: LF_NAME     TYPE TDOBNAME.
  DATA: LF_OBJECT   TYPE TDOBJECT.

  CALL FUNCTION 'SD_PARTNER_READ'
    EXPORTING
      F_VBELN          = US_SALES_ORDER-VBELN
      OBJECT           = 'VBPA'
      NO_MASTER_ADRESS = ' '
    TABLES
      I_XVBADR         = LT_XVBADR
      I_XVBPA          = LT_XVBPA.

  READ TABLE LT_XVBPA INTO LS_XVBPA WITH KEY PARVW = 'VE'.
  IF SY-SUBRC = 0.
    US_RESULT-PERNRT = LS_XVBPA-NAME1.
  ENDIF.

  READ TABLE GT_PARTNER INTO LS_PARTNER
  WITH KEY VBELN  = US_SALES_ORDER-VBELN
           PARVW  = 'AG'
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    US_RESULT-KUNNRT  = LS_PARTNER-PARTNER_NAME.
  ENDIF.

  US_RESULT-CMGSTT  = US_SALES_ORDER-STATUS_DESC.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      OBJNR            = US_SALES_ORDER-VBAK_OBJNR
    TABLES
      STATUS           = LT_STATUS
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0  ##NEEDED.
*   Implement suitable error handling here
  ENDIF.


  IF NOT LT_STATUS IS INITIAL.
    US_RESULT-E0005T = 'Approved'(T01).
    READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'E0005'.
    IF SY-SUBRC = 0.
      IF LS_STATUS-INACT = 'X'.
      ELSE.
        US_RESULT-E0005T = 'Not Approved'(T02).
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR LT_STATUS.
  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      OBJNR            = US_SALES_ORDER-VBAP_OBJNR
    TABLES
      STATUS           = LT_STATUS
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0  ##NEEDED.
*   Implement suitable error handling here
  ENDIF.

  IF NOT LT_STATUS IS INITIAL.
    US_RESULT-E0005IT = 'Not Block'(T03).
    READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'E0005'.
    IF SY-SUBRC = 0.
      IF LS_STATUS-INACT = 'X'.
      ELSE.
        US_RESULT-E0005IT = 'Block'(T05).
      ENDIF.
    ENDIF.
  ENDIF.
*
  LF_NAME   = US_SALES_ORDER-VBELN.
  LF_OBJECT = 'VBBK'.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     ID                      = 'Z002'  "CH01 DEL
      ID                      = 'ZH06'  "CH01 ADD
      LANGUAGE                = SY-LANGU
      NAME                    = LF_NAME
      OBJECT                  = LF_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    READ TABLE LT_LINES INTO LS_LINE INDEX 1.
    IF SY-SUBRC EQ 0.
      US_RESULT-PJNAME = LS_LINE-TDLINE.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_STORAGE_LOC
*&---------------------------------------------------------------------*
*& Get Storage Location
*&---------------------------------------------------------------------*
FORM F_GET_STORAGE_LOC .
  CHECK NOT GT_MATERIAL IS INITIAL.

  SELECT FROM T001L                                "#EC CI_NO_TRANSFORM
  FIELDS LGORT,
         LGOBE
  FOR ALL ENTRIES IN @GT_MATERIAL
  WHERE WERKS EQ @GT_MATERIAL-WERKS
    AND LGORT EQ @GT_MATERIAL-LGORT
  INTO TABLE @GT_STORAGE_LOC.                          "#EC CI_GENBUFF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PARTNER
*&---------------------------------------------------------------------*
*& Get Partner
*&---------------------------------------------------------------------*
FORM F_GET_PARTNER .
  DATA: LRT_VBELN        TYPE RANGE OF VBPA-VBELN.
  DATA: LS_VBELN        LIKE LINE OF LRT_VBELN.

  LOOP AT GT_SALES_ORDER ASSIGNING FIELD-SYMBOL(<L_SALES_ORDER>).
    CLEAR LS_VBELN.
    LS_VBELN      = 'IEQ'.
    LS_VBELN-LOW  = <L_SALES_ORDER>-VBELN.
    COLLECT LS_VBELN INTO LRT_VBELN.
  ENDLOOP.
  LOOP AT GT_DELIVERY ASSIGNING FIELD-SYMBOL(<L_DELIVERY>).
    CLEAR LS_VBELN.
    LS_VBELN      = 'IEQ'.
    LS_VBELN-LOW  = <L_DELIVERY>-SO_VBELN.
    COLLECT LS_VBELN INTO LRT_VBELN.
  ENDLOOP.

  IF NOT LRT_VBELN IS INITIAL.
    SELECT FROM VBPA
    LEFT OUTER JOIN ADRC ON  ADRC~ADDRNUMBER  = VBPA~ADRNR
                         AND ADRC~DATE_FROM   = '00010101'
                         AND ADRC~NATION      = 'I'
    FIELDS VBPA~VBELN,
           VBPA~PARVW,
           VBPA~POSNR,
           ADRC~NAME1 AS PARTNER_NAME
    WHERE VBPA~VBELN  IN @LRT_VBELN
    INTO TABLE @GT_PARTNER.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_PARTNER_DO
*&---------------------------------------------------------------------*
*& Read Partner for DO
*&---------------------------------------------------------------------*
FORM F_READ_PARTNER_DO  USING  US_DELIVERY  TYPE TS_DELIVERY
                               US_RESULT    TYPE TS_RESULT.
  DATA: LS_PARTNER  LIKE LINE OF GT_PARTNER.
  DATA: LT_XVBADR   TYPE STANDARD TABLE OF SADRVB.
  DATA: LT_XVBPA    TYPE STANDARD TABLE OF VBPAVB.
  DATA: LT_STATUS   TYPE STANDARD TABLE OF JSTAT.
  DATA: LT_LINES    TYPE STANDARD TABLE OF TLINE.
  DATA: LS_XVBPA    LIKE LINE OF LT_XVBPA.
  DATA: LS_STATUS   LIKE LINE OF LT_STATUS.
  DATA: LS_LINE     LIKE LINE OF LT_LINES.
  DATA: LF_NAME     TYPE TDOBNAME.
  DATA: LF_OBJECT   TYPE TDOBJECT.

  CALL FUNCTION 'SD_PARTNER_READ'
    EXPORTING
      F_VBELN          = US_DELIVERY-SO_VBELN
      OBJECT           = 'VBPA'
      NO_MASTER_ADRESS = ' '
    TABLES
      I_XVBADR         = LT_XVBADR
      I_XVBPA          = LT_XVBPA.

  READ TABLE LT_XVBPA INTO LS_XVBPA WITH KEY PARVW = 'VE'.
  IF SY-SUBRC = 0.
    US_RESULT-PERNRT = LS_XVBPA-NAME1.
  ENDIF.

  READ TABLE GT_PARTNER INTO LS_PARTNER
  WITH KEY VBELN  = US_DELIVERY-SO_VBELN
           PARVW  = 'AG'
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    US_RESULT-KUNNRT  = LS_PARTNER-PARTNER_NAME.
  ENDIF.

  US_RESULT-CMGSTT  = US_DELIVERY-STATUS_DESC.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      OBJNR            = US_DELIVERY-VBAK_OBJNR
    TABLES
      STATUS           = LT_STATUS
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0  ##NEEDED.
*   Implement suitable error handling here
  ENDIF.

  IF NOT LT_STATUS IS INITIAL.
    US_RESULT-E0005T = 'Approved'(T01).
    READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'E0005'.
    IF SY-SUBRC = 0.
      IF LS_STATUS-INACT = 'X'.
      ELSE.
        US_RESULT-E0005T = 'Not Approved'(T02).
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR LT_STATUS.
  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      OBJNR            = US_DELIVERY-VBAP_OBJNR
    TABLES
      STATUS           = LT_STATUS
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.

  US_RESULT-E0005IT = 'Not Block'(T03).
  IF SY-SUBRC = 0.
    READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'E0005'.
    IF SY-SUBRC = 0.
      IF LS_STATUS-INACT = 'X'.
      ELSE.
        US_RESULT-E0005IT = 'Block'(T05).
      ENDIF.
    ENDIF.
  ENDIF.
*
  LF_NAME   = US_DELIVERY-SO_VBELN.
  LF_OBJECT = 'VBBK'.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     ID                      = 'Z002'  "CH01 DEL
      ID                      = 'ZH06'  "CH01 ADD
      LANGUAGE                = SY-LANGU
      NAME                    = LF_NAME
      OBJECT                  = LF_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.

    READ TABLE LT_LINES INTO LS_LINE INDEX 1.
    IF SY-SUBRC EQ 0.
      US_RESULT-PJNAME = LS_LINE-TDLINE.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PRINT_TOP_OF_PAGE_1
*&---------------------------------------------------------------------*
*& Prnt Top of Page 1
*&---------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1  USING US_DOC  TYPE REF TO CL_DD_DOCUMENT  ##CALLED.
  DATA: LF_TEXT TYPE SDYDO_TEXT_ELEMENT.

  LF_TEXT = |MODEL : { P_MATNR }|.
  CALL METHOD US_DOC->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  =
*     STYLE_CLASS   =
    .

  US_DOC->NEW_LINE( ).

  LF_TEXT = 'Last Refreshed :'(T04).
  LF_TEXT = |{ LF_TEXT } { SY-DATUM+6(2) }.{ SY-DATUM+4(2) }.{ SY-DATUM+0(4) }|.
  LF_TEXT = |{ LF_TEXT } { SY-UZEIT+0(2) }:{ SY-UZEIT+2(2) }:{ SY-UZEIT+4(2) }|.
  CALL METHOD US_DOC->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  =
*     STYLE_CLASS   =
    .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_AVAIL_SLOC
*&---------------------------------------------------------------------*
*& Get Available Storage Location
*&---------------------------------------------------------------------*
FORM F_GET_AVAIL_SLOC .
  TYPES: BEGIN OF LTS_AVAIL_SLOC,
           WERKS TYPE ZSDSMMC002-WERKS,
           MTART TYPE ZSDSMMC002-MTART,
           LGORT TYPE ZSDSMMC002-LGORT,
         END OF LTS_AVAIL_SLOC.
  DATA: LT_AVAIL_SLOC   TYPE STANDARD TABLE OF LTS_AVAIL_SLOC.
  DATA: LS_R_AVAIL_SLOC LIKE LINE OF GR_AVAIL_SLOC.
  DATA: LF_MTART        TYPE MARA-MTART.

  LF_MTART  = GC_ZFG.

  SELECT FROM ZSDSMMC002
  FIELDS WERKS,
         MTART,
         LGORT
  WHERE WERKS EQ @P_WERKS
    AND MTART EQ @LF_MTART
  INTO TABLE @LT_AVAIL_SLOC.

  LOOP AT LT_AVAIL_SLOC ASSIGNING FIELD-SYMBOL(<L_AVAIL_SLOC>).
    CLEAR LS_R_AVAIL_SLOC.
    LS_R_AVAIL_SLOC     = 'IEQ'.
    LS_R_AVAIL_SLOC-LOW = <L_AVAIL_SLOC>-LGORT.
    APPEND LS_R_AVAIL_SLOC TO GR_AVAIL_SLOC.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on T-code
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
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  CASE UF_UCOMM.
    WHEN 'ZAFF'.
*      PERFORM SELECT_CHECK_BOX USING 'X'.
    WHEN 'ZSTK'.
  ENDCASE.
  SUPPRESS DIALOG.
ENDFORM.
