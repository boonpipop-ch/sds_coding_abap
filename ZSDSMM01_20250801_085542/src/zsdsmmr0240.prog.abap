*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0240
*  Creation Date      : 06.05.2024
*  Author             : Kittirat C.(Eviden)
*  Add-on ID          : ZMMR010
*  Description        : New available FG&SP stock
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
*  17.03.2025  v1.00      Zulkiff B.   Bug fixing: Open Qty, Confirm Qty
*  31.03.2025  F36K914973 Suteera P.   420000541 Correct Qty after
*                                      refresh report.
*                                      420000543 Add logic to remove
*                                      reject item.
*-----------------------------------------------------------------------
REPORT ZSDSMMR0240.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  MARA,
  MARD.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:  BEGIN OF TS_RESULT.
          INCLUDE TYPE ZSDSMMS021.
TYPES:  END OF TS_RESULT.

TYPES: BEGIN OF TS_MATERIAL,
         MATNR        TYPE MARD-MATNR,
         WERKS        TYPE MARD-WERKS,
         LGORT        TYPE MARD-LGORT,
         LABST        TYPE MARD-LABST,
         UMLME        TYPE MARD-UMLME,
         INSME        TYPE MARD-INSME,
         SPEME        TYPE MARD-SPEME,
         LGPBE        TYPE MARD-LGPBE,
         DEL_FLAG_MAT TYPE MARA-LVORM,
         MTART        TYPE MARA-MTART,
         MATKL        TYPE MARA-MATKL,
         PRDHA        TYPE MARA-PRDHA,
         MFRNR        TYPE MARA-MFRNR,
         MAKTX        TYPE MAKT-MAKTX,
         LGOBE        TYPE T001L-LGOBE,
         MANUF_NAME   TYPE LFA1-NAME1,
         PLANT_NAME   TYPE T001W-NAME1,
       END OF TS_MATERIAL.

TYPES: BEGIN OF TS_HEAD_KEY,
         MATNR TYPE MARD-MATNR,
         WERKS TYPE MARD-WERKS,
       END OF TS_HEAD_KEY.

TYPES: BEGIN OF TS_SALES_ORDER,
         VBELN TYPE VBEP-VBELN,
         POSNR TYPE VBEP-POSNR,
         ETENR TYPE VBEP-ETENR,
         MATNR TYPE VBAP-MATNR,
         WERKS TYPE VBAP-WERKS,
         LGORT TYPE VBAP-LGORT,
         WMENG TYPE VBEP-WMENG,
         BMENG TYPE VBEP-BMENG,
         EDATU TYPE VBEP-EDATU,
       END OF TS_SALES_ORDER.

TYPES: BEGIN OF TS_SO_QTY,
         MATNR TYPE VBAP-MATNR,
         WERKS TYPE VBAP-WERKS,
         WMENG TYPE VBEP-WMENG,
         BMENG TYPE VBEP-BMENG,
       END OF TS_SO_QTY.

TYPES: BEGIN OF TS_DO_QTY,
         VGBEL TYPE LIPS-VGBEL,
         VGPOS TYPE LIPS-VGPOS,
         LFIMG TYPE LIPS-LFIMG,
       END OF TS_DO_QTY.

TYPES: BEGIN OF TS_PRICE,
         MATNR TYPE A304-MATNR,
         KNUMH TYPE KONP-KNUMH,
         KOPOS TYPE KONP-KOPOS,
         KBETR TYPE KONP-KBETR,
       END OF TS_PRICE.

TYPES: BEGIN OF TS_MAT_VAL,
         MATNR TYPE MBEW-MATNR,
         BWKEY TYPE MBEW-BWKEY,
         BWTAR TYPE MBEW-BWTAR,
         VERPR TYPE MBEW-VERPR,
       END OF TS_MAT_VAL.

TYPES: BEGIN OF TS_SEARCH,
         PRODH TYPE CHAR18,
         LEVEL TYPE CHAR1,
       END OF TS_SEARCH.

TYPES: BEGIN OF TS_PART_GROUP,
         MATNR      TYPE ZRTCAC002-MATNR,
         PART_GROUP TYPE ZRTCAC002-PART_GROUP,
         ACTIVE     TYPE ZRTCAC002-ACTIVE,
       END OF TS_PART_GROUP.

TYPES:  TT_RESULT       TYPE STANDARD TABLE OF TS_RESULT.

TYPES:  TT_MATERIAL     TYPE SORTED TABLE OF TS_MATERIAL
                        WITH UNIQUE KEY MATNR
                                        WERKS
                                        LGORT.

TYPES:  TT_HEAD_KEY     TYPE SORTED TABLE OF TS_HEAD_KEY
                        WITH UNIQUE KEY MATNR
                                        WERKS.

TYPES:  TT_SALES_ORDER  TYPE SORTED TABLE OF TS_SALES_ORDER
                        WITH UNIQUE KEY VBELN
                                        POSNR
                                        ETENR.
TYPES:  TT_SALES_ORDER2  TYPE STANDARD TABLE OF TS_SALES_ORDER.

TYPES:  TT_SO_QTY       TYPE SORTED TABLE OF TS_SO_QTY
                        WITH UNIQUE KEY MATNR
                                        WERKS.

TYPES:  TT_DO_QTY       TYPE SORTED TABLE OF TS_DO_QTY
                        WITH UNIQUE KEY VGBEL
                                        VGPOS.

TYPES:  TT_PRICE        TYPE SORTED TABLE OF TS_PRICE
                        WITH UNIQUE KEY MATNR.

TYPES:  TT_MAT_VAL      TYPE SORTED TABLE OF TS_MAT_VAL
                        WITH UNIQUE KEY MATNR.

TYPES:  TT_PART_GROUP   TYPE SORTED TABLE OF TS_PART_GROUP
                        WITH UNIQUE KEY MATNR
                                        PART_GROUP.

TYPES:  TT_DFIES        TYPE STANDARD TABLE OF DFIES.
TYPES:  TT_MRP_IND      TYPE STANDARD TABLE OF BAPI_MRP_IND_LINES.
TYPES:  TT_SEARCH       TYPE STANDARD TABLE OF TS_SEARCH.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE              TYPE CHAR1                VALUE 'X',
  GC_TCODE             TYPE SY-TCODE             VALUE 'ZSDSMM008',
  GC_REPID             TYPE SY-REPID             VALUE SY-REPID,
  GC_DEL_MAT_SLOC      TYPE ZSDSMMS021-SLOC      VALUE 'Tota'  ##NO_TEXT,
  GC_DEL_MAT_SLOC_DESC TYPE ZSDSMMS021-SLOC_DESC VALUE 'lX'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_RESULT       TYPE TT_RESULT                           ##NEEDED.
DATA: GT_MATERIAL     TYPE TT_MATERIAL                         ##NEEDED.
DATA: GT_HEAD_KEY     TYPE TT_HEAD_KEY                         ##NEEDED.
DATA: GT_SALES_ORDER  TYPE TT_SALES_ORDER                      ##NEEDED.
DATA: GT_SO_QTY       TYPE TT_SO_QTY                           ##NEEDED.
DATA: GT_DO_QTY       TYPE TT_DO_QTY                           ##NEEDED.
DATA: GT_PRICE        TYPE TT_PRICE                            ##NEEDED.
DATA: GT_MAT_VAL      TYPE TT_MAT_VAL                          ##NEEDED.
DATA: GT_PART_GROUP   TYPE TT_PART_GROUP                       ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: GRT_AVAIL_SLOC_FG  TYPE RANGE OF MARD-LGORT              ##NEEDED.
DATA: GRT_FG_MAT_TYPE    TYPE RANGE OF MARA-MTART              ##NEEDED.
DATA: GRT_SP_MAT_TYPE    TYPE RANGE OF MARA-MTART              ##NEEDED.
DATA: GRT_PLANT          TYPE RANGE OF MARD-WERKS              ##NEEDED.

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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: RB_FG RADIOBUTTON GROUP RG1 USER-COMMAND UC1  DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(14) TEXT-B04 FOR FIELD RB_SP.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: RB_SP RADIOBUTTON GROUP RG1.
    SELECTION-SCREEN COMMENT 3(11) TEXT-B03 FOR FIELD RB_SP.
    SELECTION-SCREEN PUSHBUTTON 22(20) P_CHKCOM USER-COMMAND UC2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
  SELECT-OPTIONS: S_WERKS FOR MARD-WERKS.
  SELECT-OPTIONS: S_LGORT FOR MARD-LGORT.
  SELECT-OPTIONS: S_MATNR FOR MARD-MATNR.
  SELECT-OPTIONS: S_MATKL FOR MARA-MATKL.
  SELECT-OPTIONS: S_PH1   FOR MARA-PRDHA+0(5) NO INTERVALS.
  SELECT-OPTIONS: S_PH2   FOR MARA-PRDHA+5(5) NO INTERVALS.
  SELECT-OPTIONS: S_PH3   FOR MARA-PRDHA+10   NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANT.
  PERFORM F_DEFAULT_SELSCREEN.
  P_CHKCOM  = TEXT-T03.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'S_WERKS-LOW'.
      SCREEN-REQUIRED = 1.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-NAME EQ 'S_LGORT-LOW' OR
       SCREEN-NAME EQ 'S_LGORT-HIGH'.
      IF RB_SP EQ GC_TRUE.
        SCREEN-INPUT  = 1.
        MODIFY SCREEN.
      ELSEIF RB_FG EQ GC_TRUE.
        CLEAR: S_LGORT[],
               S_LGORT.
        SCREEN-INPUT  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ 'P_CHKCOM'.
      IF RB_FG EQ GC_TRUE.
        SCREEN-INVISIBLE  = 1.
      ELSE.
        SCREEN-INVISIBLE  = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PH1-LOW.
  PERFORM F_F4IF_INT_TABLE_VALUE_REQUEST USING 'PH1'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PH2-LOW.
  PERFORM F_F4IF_INT_TABLE_VALUE_REQUEST USING 'PH2'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PH3-LOW.
  PERFORM F_F4IF_INT_TABLE_VALUE_REQUEST USING 'PH3'.

AT SELECTION-SCREEN.
  PERFORM F_CHECK_COMMON_PART.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_GLOB_VAR.
  PERFORM F_CHECK_PART_GROUP.
  PERFORM F_MODIFY_SELOPT.
  PERFORM F_GET_AVAIL_SLOC.
  PERFORM F_GET_DATA.
  PERFORM F_PREPARE_RESULT_FG.
  PERFORM F_PREPARE_RESULT_SP.
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
    GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS021',
    GC_HEADER_HEIGHT_1 TYPE  I        VALUE 0,
    GC_ALV_HEIGHT_1    TYPE  I        VALUE 100.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& Get Data
*&---------------------------------------------------------------------*
FORM F_GET_DATA .
  PERFORM F_GET_MATERIAL.
  PERFORM F_GET_SALES_ORDER.
  PERFORM F_GET_PRICE.
  PERFORM F_GET_MAT_VALUATION.
  PERFORM F_GET_PART_GROUP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MATERIAL
*&---------------------------------------------------------------------*
*& Get Material
*&---------------------------------------------------------------------*
FORM F_GET_MATERIAL .
  DATA: LRT_MTART         TYPE RANGE OF MARA-MTART.
  DATA: LRT_DEL_FLAG_MARD TYPE RANGE OF MARD-LVORM.

  IF RB_FG EQ GC_TRUE.
    LRT_MTART  = GRT_FG_MAT_TYPE.
    APPEND INITIAL LINE TO LRT_DEL_FLAG_MARD ASSIGNING FIELD-SYMBOL(<L_LVORM>).
    <L_LVORM>     = 'IEQ'.
    <L_LVORM>-LOW = SPACE.
  ELSEIF RB_SP EQ GC_TRUE.
    LRT_MTART  = GRT_SP_MAT_TYPE.
  ENDIF.

  SELECT FROM MARD
  INNER JOIN MARA ON  MARA~MATNR  = MARD~MATNR
  INNER JOIN MARC ON  MARC~MATNR  = MARD~MATNR
                  AND MARC~WERKS  = MARD~WERKS
  LEFT OUTER JOIN MAKT  ON  MAKT~MATNR  = MARD~MATNR
                        AND MAKT~SPRAS  = 'E'
  LEFT OUTER JOIN T001L ON  T001L~WERKS = MARD~WERKS
                        AND T001L~LGORT = MARD~LGORT
  LEFT OUTER JOIN LFA1 ON LFA1~LIFNR = MARA~MFRNR
  LEFT OUTER JOIN T001W ON T001W~WERKS = MARC~WERKS    "#EC CI_BUFFJOIN
  FIELDS  MARD~MATNR,
          MARD~WERKS,
          MARD~LGORT,
          MARD~LABST,
          MARD~UMLME,
          MARD~INSME,
          MARD~SPEME,
          MARD~LGPBE,
          MARA~LVORM  AS DEL_FLAG_MAT,
          MARA~MTART,
          MARA~MATKL,
          MARA~PRDHA,
          MARA~MFRNR,
          MAKT~MAKTX,
          T001L~LGOBE,
          LFA1~NAME1  AS MANUF_NAME,
          T001W~NAME1 AS PLANT_NAME
  WHERE MARD~MATNR IN @S_MATNR
    AND MARD~WERKS IN @S_WERKS
    AND MARD~LGORT IN @S_LGORT
    AND MARD~LVORM IN @LRT_DEL_FLAG_MARD
    AND MARA~MTART IN @LRT_MTART
    AND MARA~MATKL IN @S_MATKL
    AND MARA~PRDHA IN @S_PH1
    AND MARA~PRDHA IN @S_PH2
    AND MARA~PRDHA IN @S_PH3
  INTO TABLE @GT_MATERIAL.

  IF RB_FG EQ GC_TRUE.
    DELETE GT_MATERIAL WHERE NOT LGORT IN GRT_AVAIL_SLOC_FG. "#EC CI_SORTSEQ
  ELSE.
    DELETE GT_MATERIAL WHERE LGORT IN GRT_AVAIL_SLOC_FG. "#EC CI_SORTSEQ
  ENDIF.

  LOOP AT GT_MATERIAL ASSIGNING FIELD-SYMBOL(<L_MATERIAL>).
    "Append header data: Mat, Plant
    READ TABLE GT_HEAD_KEY ASSIGNING FIELD-SYMBOL(<L_HEAD_KEY>)
    WITH KEY  MATNR = <L_MATERIAL>-MATNR
              WERKS = <L_MATERIAL>-WERKS
    BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_HEAD_KEY( MATNR  = <L_MATERIAL>-MATNR
                                WERKS  = <L_MATERIAL>-WERKS )
      INTO TABLE GT_HEAD_KEY
      ASSIGNING <L_HEAD_KEY>.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MODIFY_SELOPT
*&---------------------------------------------------------------------*
*& Modify Select Options
*&---------------------------------------------------------------------*
FORM F_MODIFY_SELOPT .
  LOOP AT S_PH1 ASSIGNING FIELD-SYMBOL(<L_PH1>).
    <L_PH1>-OPTION  = 'CP'.
    <L_PH1>-LOW     = |{ <L_PH1>-LOW }*|.
  ENDLOOP.

  LOOP AT S_PH2 ASSIGNING FIELD-SYMBOL(<L_PH2>).
    <L_PH2>-OPTION  = 'CP'.
    <L_PH2>-LOW     = |*{ <L_PH2>-LOW }*|.
  ENDLOOP.

  LOOP AT S_PH3 ASSIGNING FIELD-SYMBOL(<L_PH3>).
    <L_PH3>-OPTION  = 'CP'.
    <L_PH3>-LOW     = |*{ <L_PH3>-LOW }*|.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_RESULT_FG
*&---------------------------------------------------------------------*
*& Prepare Result for FG
*&---------------------------------------------------------------------*
FORM F_PREPARE_RESULT_FG .
  DATA: LT_MRP_IND        TYPE STANDARD TABLE OF BAPI_MRP_IND_LINES.
  DATA: LS_MRP_STOCK_DET  TYPE BAPI_MRP_STOCK_DETAIL.
  DATA: LS_RESULT         LIKE LINE OF GT_RESULT.
  DATA: LF_REQD_QTY       TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.

  CHECK RB_FG EQ ABAP_TRUE.

  LOOP AT GT_HEAD_KEY ASSIGNING FIELD-SYMBOL(<L_HEAD_KEY>).
    CLEAR: LT_MRP_IND,
           LS_RESULT,
           LS_MRP_STOCK_DET.

    PERFORM F_GET_MAT_STOCK_REQ USING <L_HEAD_KEY>-MATNR
                                      <L_HEAD_KEY>-WERKS
                                      LS_MRP_STOCK_DET
                                      LT_MRP_IND.
    IF LT_MRP_IND IS INITIAL.
      CONTINUE.
    ENDIF.

    LS_RESULT-UR_VENDOR_STCK  = LS_MRP_STOCK_DET-VAL_STOCK.
    LS_RESULT-CONSIG_ORD      = LS_MRP_STOCK_DET-CONSIG_ORD.

    LOOP AT GT_MATERIAL ASSIGNING FIELD-SYMBOL(<L_MATERIAL>) "#EC CI_NESTED.
      WHERE MATNR EQ <L_HEAD_KEY>-MATNR
        AND WERKS EQ <L_HEAD_KEY>-WERKS.

      LS_RESULT-PH1               = <L_MATERIAL>-PRDHA+0(5).
      LS_RESULT-PH2               = <L_MATERIAL>-PRDHA+5(5).
      LS_RESULT-PH3               = <L_MATERIAL>-PRDHA+10.
      LS_RESULT-MAT_GROUP         = <L_MATERIAL>-MATKL.
      LS_RESULT-MATERIAL          = <L_MATERIAL>-MATNR.
      LS_RESULT-MATERIAL_DESC     = <L_MATERIAL>-MAKTX.
      LS_RESULT-PLANT             = <L_MATERIAL>-WERKS.
      LS_RESULT-PLANT_NAME        = <L_MATERIAL>-PLANT_NAME.

      "recalculate stock qty
      LS_RESULT-UNRESTRICTED_STCK = LS_RESULT-UNRESTRICTED_STCK + <L_MATERIAL>-LABST.
      LS_RESULT-QUAL_INSPECTION   = LS_RESULT-QUAL_INSPECTION + <L_MATERIAL>-INSME.
      LS_RESULT-BLOCKED_STCK      = LS_RESULT-BLOCKED_STCK + <L_MATERIAL>-SPEME.
      LS_RESULT-STOCK_IN_TFR      = LS_RESULT-STOCK_IN_TFR + <L_MATERIAL>-UMLME.
    ENDLOOP.

    "add stock qty
    LOOP AT LT_MRP_IND ASSIGNING FIELD-SYMBOL(<L_MRP_IND>) "#EC CI_NESTED.
      WHERE STORAGE_LOC IS INITIAL
         OR STORAGE_LOC IN GRT_AVAIL_SLOC_FG.

      LF_REQD_QTY = <L_MRP_IND>-REC_REQD_QTY.
      IF <L_MRP_IND>-PLUS_MINUS EQ '-'.
        LF_REQD_QTY = <L_MRP_IND>-REC_REQD_QTY * -1.
      ENDIF.

      CASE <L_MRP_IND>-MRP_ELEMENT_IND.
        WHEN 'VJ'.
          LS_RESULT-DELIVERY_STCK     = LS_RESULT-DELIVERY_STCK + LF_REQD_QTY.
        WHEN 'BE' OR 'LA'.
          LS_RESULT-PUR_ORDER_STCK    = LS_RESULT-PUR_ORDER_STCK + LF_REQD_QTY.
        WHEN 'MR'.
          LS_RESULT-RESERVED_STCK     = LS_RESULT-RESERVED_STCK + LF_REQD_QTY.
        WHEN 'KB'.
          LS_RESULT-SALES_ORDER_STCK  = LS_RESULT-SALES_ORDER_STCK + LF_REQD_QTY.
      ENDCASE.
    ENDLOOP.

    "add SO qty
    LOOP AT GT_SO_QTY ASSIGNING FIELD-SYMBOL(<L_SO_QTY>) "#EC CI_NESTED.
      WHERE MATNR EQ <L_HEAD_KEY>-MATNR
        AND WERKS EQ <L_HEAD_KEY>-WERKS.

      LS_RESULT-CONFIRM_SO  = LS_RESULT-CONFIRM_SO + <L_SO_QTY>-BMENG.
      LS_RESULT-ORDER_STCK  = LS_RESULT-ORDER_STCK + <L_SO_QTY>-WMENG.
    ENDLOOP.

    "calculate free stock
    LS_RESULT-FREE_STOCK  = LS_RESULT-UNRESTRICTED_STCK
                              - LS_RESULT-RESERVED_STCK
                              - LS_RESULT-CONFIRM_SO
                              - LS_RESULT-DELIVERY_STCK
                              + LS_RESULT-SALES_ORDER_STCK.

    APPEND LS_RESULT TO GT_RESULT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONFIRM_SO
*&---------------------------------------------------------------------*
*& Get Confirm SO
*&---------------------------------------------------------------------*
FORM F_GET_SALES_ORDER .
  DATA: LF_DO_QTY TYPE LIPS-LFIMG.
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

  DATA: LF_WMENG  TYPE VBEP-WMENG.
  DATA: LF_BMENG  TYPE VBEP-BMENG.

  CHECK NOT GT_MATERIAL IS INITIAL.
  CHECK RB_FG EQ GC_TRUE.

  SELECT FROM VBEP
  INNER JOIN VBAP ON  VBEP~VBELN  = VBAP~VBELN
                  AND VBEP~POSNR  = VBAP~POSNR
  INNER JOIN VBAK ON  VBAK~VBELN  = VBAP~VBELN
                  AND VBAK~VBTYP  = 'C' "Order
  FIELDS VBEP~VBELN,
         VBEP~POSNR,
         VBEP~ETENR,
         VBAP~MATNR,
         VBAP~WERKS,
         VBAP~LGORT,
         VBEP~WMENG,
         VBEP~BMENG,
         VBEP~EDATU
  FOR ALL ENTRIES IN @GT_MATERIAL
  WHERE VBAP~MATNR EQ @GT_MATERIAL-MATNR
    AND VBAP~WERKS EQ @GT_MATERIAL-WERKS
    AND VBAP~ABGRU EQ @SPACE "420000543++
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
    CLEAR LF_DO_QTY.

    "Get DO Qty
    READ TABLE GT_DO_QTY ASSIGNING FIELD-SYMBOL(<L_DO_QTY>)
    WITH KEY  VGBEL = <L_SALES_ORDER>-VBELN
              VGPOS = <L_SALES_ORDER>-POSNR
    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LF_DO_QTY = <L_DO_QTY>-LFIMG.
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
*     Sum Qty from Mat/Plant level
      <L_SO_QTY>-WMENG = <L_SO_QTY>-WMENG + ( <L_SALES_ORDER>-WMENG - LF_DO_QTY ).
      <L_SO_QTY>-BMENG = <L_SO_QTY>-BMENG + ( <L_SALES_ORDER>-BMENG - LF_DO_QTY ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MAT_STOCK_REQ
*&---------------------------------------------------------------------*
*& Get Material Stock Requirment List
*&---------------------------------------------------------------------*
FORM F_GET_MAT_STOCK_REQ  USING  UF_MATNR TYPE MARD-MATNR
                                 UF_WERKS TYPE MARD-WERKS
                                 US_MRP_STOCK_DET TYPE BAPI_MRP_STOCK_DETAIL
                                 UT_MRP_IND TYPE TT_MRP_IND.
  DATA: LT_MRP_IND        TYPE STANDARD TABLE OF BAPI_MRP_IND_LINES.
  DATA: LS_MRP_STOCK_DET  TYPE BAPI_MRP_STOCK_DETAIL.
  DATA: LS_RETURN         TYPE BAPIRET2.
  DATA: LF_MATERIAL      TYPE BAPI_MRP_MAT_PARAM-MATERIAL,
        LF_MATERIAL_LONG TYPE BAPI_MRP_MAT_PARAM-MATERIAL_LONG.
  DATA: LF_PLANT          TYPE BAPI_MRP_MAT_PARAM-PLANT.

*  LF_MATERIAL       = UF_MATNR.
  LF_MATERIAL_LONG  = UF_MATNR.
  LF_PLANT          = UF_WERKS.

  CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
    EXPORTING
*     MATERIAL         = LF_MATERIAL
      MATERIAL_LONG    = LF_MATERIAL_LONG
      PLANT            = LF_PLANT
    IMPORTING
      MRP_STOCK_DETAIL = LS_MRP_STOCK_DET
      RETURN           = LS_RETURN
    TABLES
      MRP_IND_LINES    = LT_MRP_IND.

  IF LS_RETURN-TYPE EQ 'S'.
    US_MRP_STOCK_DET  = LS_MRP_STOCK_DET.
    UT_MRP_IND        = LT_MRP_IND.
    DELETE UT_MRP_IND WHERE PLNGSEGMT NE '02' AND
                            PLNGSEGMT NE '20'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PRICE
*&---------------------------------------------------------------------*
*& Get Price
*&---------------------------------------------------------------------*
FORM F_GET_PRICE .
  DATA: LF_KAPPL  TYPE A004-KAPPL VALUE 'V'.
  DATA: LF_KSCHL  TYPE A004-KSCHL VALUE 'ZPR0'.
  DATA: LF_VKORG  TYPE A004-VKORG VALUE '1000'.
  DATA: LF_VTWEG  TYPE A004-VTWEG VALUE '00'.

  CHECK NOT GT_MATERIAL IS INITIAL.

  SELECT FROM A004                                      "#EC CI_NOORDER
  INNER JOIN KONP ON KONP~KNUMH = A004~KNUMH           "#EC CI_BUFFJOIN
  FIELDS A004~MATNR,
         KONP~KNUMH,
         KONP~KOPOS,
         KONP~KBETR
  FOR ALL ENTRIES IN @GT_MATERIAL
  WHERE A004~KAPPL EQ @LF_KAPPL
    AND A004~KSCHL EQ @LF_KSCHL
    AND A004~VKORG EQ @LF_VKORG
    AND A004~VTWEG EQ @LF_VTWEG
    AND A004~MATNR EQ @GT_MATERIAL-MATNR
    AND A004~DATBI GE @SY-DATUM
    AND A004~DATAB LE @SY-DATUM
  INTO TABLE @GT_PRICE.
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
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

  GF_SECOND_ALV_1 = SPACE.

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
** Call ALV Screen
  CALL SCREEN 9000.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.
    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'PH1'.
*       Text-c01 : PH1
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PH2'.
*       Text-c02 : PH2
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PH3'.
*       Text-c03 : PH3
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MAT_GROUP'.
*       Text-c28 : Material Group
        LF_TEXT                = TEXT-C28.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MATERIAL'.
*       Text-c04 : Material
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MATERIAL_DESC'.
*       Text-c05 : Material Description
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PLANT'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'PLANT_NAME'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'SLOC'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*         Text-c06 : Storage Location
          LF_TEXT                = TEXT-C06.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'SLOC_DESC'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*         Text-c07 : Storage Description
          LF_TEXT                = TEXT-C07.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'STORAGE_BIN'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*         Text-c08 : Storage Bin
          LF_TEXT                = TEXT-C08.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'FREE_STOCK'.
*       Text-c09 : Free Stock
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'UNRESTRICTED_STCK'.
*       Text-c10 : Unrestricted
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-DECIMALS_O = 0.
        <L_FIELDCAT>-NO_ZERO   = ABAP_TRUE.
      WHEN 'ORDER_STCK'.
*       Text-c11 : Open SO
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CONFIRM_SO'.
*       Text-c12 : Confirm SO
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'DELIVERY_STCK'.
*       Text-c13 : Schd. for Delivery
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PUR_ORDER_STCK'.
*       Text-c14 : Purchasing
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BLOCKED_STCK'.
*       Text-c15 : Blocked Stock
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'QUAL_INSPECTION'.
*       Text-c16 : Qual. Inspection
        LF_TEXT                = TEXT-C16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CONSIG_ORD'.
*       Text-c17 : Cust. Consignment
        LF_TEXT                = TEXT-C17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'RESERVED_STCK'.
*       Text-c18 : Reserved
        LF_TEXT                = TEXT-C18.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'UR_VENDOR_STCK'.
*       Text-c19 : Stock Provided to Vendor
        LF_TEXT                = TEXT-C19.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'STOCK_IN_TFR'.
*       Text-c20 : Transfer(Sloc)
        LF_TEXT                = TEXT-C20.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SALES_ORDER_STCK'.
*       Text-c27 : Sales Order Stock
        LF_TEXT                = TEXT-C27.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PRICE'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*       Text-c21 : Price
          LF_TEXT                = TEXT-C21.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'AVG_COST'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*       Text-c23 : AVG.Cost
          LF_TEXT                = TEXT-C22.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'PART_GROUP'.
*       Text-c24 : Part Group
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
          LF_TEXT                = TEXT-C23.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'ACTIVE'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*       Text-c25 : Active
          LF_TEXT                = TEXT-C24.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'MANUF_NUM'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*       Text-c26 : Manufacturer
          LF_TEXT                = TEXT-C25.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
      WHEN 'MANUF_NAME'.
        IF RB_FG EQ GC_TRUE.
          <L_FIELDCAT>-NO_OUT    = GC_TRUE.
        ELSEIF RB_SP EQ GC_TRUE.
*       Text-c27 : Manufacturer Name
          LF_TEXT                = TEXT-C26.
          <L_FIELDCAT>-REPTEXT   = LF_TEXT.
          <L_FIELDCAT>-COLTEXT   = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
          <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        ENDIF.
    ENDCASE.

  ENDLOOP.

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
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

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
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID    TYPE LVC_S_ROW       ##CALLED
                             US_COLUMN_ID TYPE LVC_S_COL       ##NEEDED.

  DATA: LS_RESULT   LIKE LINE OF GT_RESULT.
  DATA: LRS_EXCEPT  TYPE REF TO CX_SY_AUTHORIZATION_ERROR.
  DATA: LF_MESSAGE  TYPE STRING.

* Read Line
  READ TABLE GT_RESULT INTO LS_RESULT INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  IF RB_FG EQ ABAP_TRUE.
    SUBMIT ZSDSMMR0300 WITH P_MATNR = LS_RESULT-MATERIAL
                       WITH P_WERKS = LS_RESULT-PLANT    "#EC CI_SUBMIT
    AND RETURN.

  ELSEIF RB_SP EQ ABAP_TRUE.
    SELECT SINGLE FROM MDLG                                 "#EC WARNOK
    FIELDS BERID
    WHERE WERKS = @LS_RESULT-PLANT
      AND LGORT = @LS_RESULT-SLOC
    INTO @DATA(LF_BERID).

    IF SY-SUBRC EQ 0.
      SET PARAMETER ID 'MAT'    FIELD LS_RESULT-MATERIAL.
      SET PARAMETER ID 'BERID'  FIELD LF_BERID.
      SET PARAMETER ID 'WRK'    FIELD LS_RESULT-PLANT.
      TRY.
          CALL TRANSACTION 'MD04' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
        CATCH CX_SY_AUTHORIZATION_ERROR INTO LRS_EXCEPT.
          CALL METHOD LRS_EXCEPT->IF_MESSAGE~GET_TEXT
            RECEIVING
              RESULT = LF_MESSAGE.
          MESSAGE LF_MESSAGE  TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_RESULT_SP
*&---------------------------------------------------------------------*
*& Prepare Result for Spare Parts
*&---------------------------------------------------------------------*
FORM F_PREPARE_RESULT_SP .

  DATA: LT_MRP_IND        TYPE STANDARD TABLE OF BAPI_MRP_IND_LINES.
  DATA: LS_MRP_STOCK_DET  TYPE BAPI_MRP_STOCK_DETAIL.
  DATA: LS_RESULT         LIKE LINE OF GT_RESULT.
  DATA: LF_REQD_QTY       TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.

  CHECK RB_SP EQ ABAP_TRUE.

  LOOP AT GT_HEAD_KEY ASSIGNING FIELD-SYMBOL(<L_HEAD_KEY>).
    CLEAR: LT_MRP_IND,
           LS_RESULT,
           LS_MRP_STOCK_DET.

    PERFORM F_GET_MAT_STOCK_REQ USING <L_HEAD_KEY>-MATNR
                                      <L_HEAD_KEY>-WERKS
                                      LS_MRP_STOCK_DET
                                      LT_MRP_IND.
    IF LT_MRP_IND IS INITIAL.
      CONTINUE.
    ENDIF.

    LOOP AT GT_MATERIAL ASSIGNING FIELD-SYMBOL(<L_MATERIAL>) "#EC CI_NESTED.
      WHERE MATNR EQ <L_HEAD_KEY>-MATNR
        AND WERKS EQ <L_HEAD_KEY>-WERKS.

      CLEAR LS_RESULT.
      LS_RESULT-PH1               = <L_MATERIAL>-PRDHA+0(5).
      LS_RESULT-PH2               = <L_MATERIAL>-PRDHA+5(5).
      LS_RESULT-PH3               = <L_MATERIAL>-PRDHA+10.
      LS_RESULT-MAT_GROUP         = <L_MATERIAL>-MATKL.
      LS_RESULT-MATERIAL          = <L_MATERIAL>-MATNR.
      LS_RESULT-MATERIAL_DESC     = <L_MATERIAL>-MAKTX.
      LS_RESULT-PLANT             = <L_MATERIAL>-WERKS.
      LS_RESULT-PLANT_NAME        = <L_MATERIAL>-PLANT_NAME.
      LS_RESULT-STORAGE_BIN       = <L_MATERIAL>-LGPBE.
      LS_RESULT-MANUF_NUM         = <L_MATERIAL>-MFRNR.
      LS_RESULT-MANUF_NAME        = <L_MATERIAL>-MANUF_NAME.
      LS_RESULT-UNRESTRICTED_STCK = <L_MATERIAL>-LABST.
      LS_RESULT-QUAL_INSPECTION   = <L_MATERIAL>-INSME.
      LS_RESULT-BLOCKED_STCK      = <L_MATERIAL>-SPEME.
      LS_RESULT-STOCK_IN_TFR      = <L_MATERIAL>-UMLME.

      IF <L_MATERIAL>-DEL_FLAG_MAT IS INITIAL.
        LS_RESULT-SLOC      = <L_MATERIAL>-LGORT.
        LS_RESULT-SLOC_DESC = <L_MATERIAL>-LGOBE.
      ELSE.
        LS_RESULT-SLOC      = GC_DEL_MAT_SLOC.
        LS_RESULT-SLOC_DESC = GC_DEL_MAT_SLOC_DESC.
      ENDIF.

      LOOP AT LT_MRP_IND ASSIGNING FIELD-SYMBOL(<L_MRP_IND>) "#EC CI_NESTED.
        WHERE STORAGE_LOC EQ <L_MATERIAL>-LGORT.

        LF_REQD_QTY = <L_MRP_IND>-REC_REQD_QTY.
        IF <L_MRP_IND>-PLUS_MINUS EQ '-'.
          LF_REQD_QTY = LF_REQD_QTY * -1.
        ENDIF.

        CASE <L_MRP_IND>-MRP_ELEMENT_IND.
          WHEN 'VC'.
            LS_RESULT-ORDER_STCK  = LS_RESULT-ORDER_STCK + LF_REQD_QTY.
            LS_RESULT-CONFIRM_SO  = LS_RESULT-CONFIRM_SO + LF_REQD_QTY.
          WHEN 'VJ'.
            LS_RESULT-DELIVERY_STCK = LS_RESULT-DELIVERY_STCK + LF_REQD_QTY.
          WHEN 'BE' OR 'LA'.
            LS_RESULT-PUR_ORDER_STCK  = LS_RESULT-PUR_ORDER_STCK + LF_REQD_QTY.
          WHEN 'MR'.
            LS_RESULT-RESERVED_STCK = LS_RESULT-RESERVED_STCK + LF_REQD_QTY.
        ENDCASE.
      ENDLOOP.

      LS_RESULT-FREE_STOCK  = LS_RESULT-UNRESTRICTED_STCK
                                - LS_RESULT-RESERVED_STCK
                                - LS_RESULT-CONFIRM_SO
                                - LS_RESULT-DELIVERY_STCK.

      READ TABLE GT_PRICE ASSIGNING FIELD-SYMBOL(<L_PRICE>)
      WITH KEY MATNR = <L_MATERIAl>-MATNR
      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_RESULT-PRICE = <L_PRICE>-KBETR.
      ENDIF.

      READ TABLE GT_MAT_VAL ASSIGNING FIELD-SYMBOL(<L_MAT_VAL>)
      WITH KEY MATNR = <L_MATERIAl>-MATNR
      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_RESULT-AVG_COST  = <L_MAT_VAL>-VERPR.
      ENDIF.

      READ TABLE GT_PART_GROUP ASSIGNING FIELD-SYMBOL(<L_PART_GROUP>)
      WITH KEY MATNR = <L_MATERIAl>-MATNR
      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_RESULT-PART_GROUP  = <L_PART_GROUP>-PART_GROUP.
        LS_RESULT-ACTIVE      = <L_PART_GROUP>-ACTIVE.
      ENDIF.

      APPEND LS_RESULT TO GT_RESULT.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_F4IF_INT_TABLE_VALUE_REQUEST USING UF_FIELD  TYPE CHAR5.
  TYPES: BEGIN OF LTS_T179,
           PRODH TYPE T179-PRODH,
         END OF LTS_T179.

  DATA : LF_RETFIELD    TYPE DFIES-FIELDNAME,
         LF_DYNPROFIELD TYPE  HELP_INFO-DYNPROFLD.

  DATA : LT_RETURN    TYPE STANDARD TABLE OF DDSHRETVAL,
         LT_FIELD_TAB TYPE STANDARD TABLE OF DFIES,
         LT_SEARCH    TYPE STANDARD TABLE OF TS_SEARCH,
         LS_SEARCH    LIKE LINE OF LT_SEARCH,
         LT_T179      TYPE STANDARD TABLE OF LTS_T179,
         LS_T179      LIKE LINE OF LT_T179.


  SELECT PRODH
  FROM T179
  INTO TABLE LT_T179
  WHERE STUFE EQ '3'.

  CLEAR : LT_SEARCH[], LS_SEARCH.
  LOOP AT LT_T179 INTO LS_T179.
    IF UF_FIELD EQ 'PH1'.
      LS_SEARCH-PRODH = LS_T179-PRODH+0(5).
      LS_SEARCH-LEVEL = '1'.
    ELSEIF UF_FIELD EQ 'PH2'.
      LS_SEARCH-PRODH = LS_T179-PRODH+5(5).
      LS_SEARCH-LEVEL = '2'.
    ELSEIF UF_FIELD EQ 'PH3'.
      LS_SEARCH-PRODH = LS_T179-PRODH+10(8).
      LS_SEARCH-LEVEL = '3'.
    ENDIF.
    APPEND LS_SEARCH TO LT_SEARCH.
    CLEAR : LS_T179,LS_SEARCH.
  ENDLOOP.

  SORT LT_SEARCH.
  DELETE ADJACENT DUPLICATES FROM LT_SEARCH COMPARING ALL FIELDS.

  LF_RETFIELD    = 'PRODH'.
  LF_DYNPROFIELD = UF_FIELD.

  PERFORM GET_FIELDS_OF_VALUE_TAB TABLES LT_SEARCH
                                         LT_FIELD_TAB
                                CHANGING LF_RETFIELD.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = LF_RETFIELD
*     PVALKEY         = ' '
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = LF_DYNPROFIELD
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      VALUE_ORG       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     MARK_TAB        =
*     IMPORTING
*     USER_RESET      =
    TABLES
      VALUE_TAB       = LT_SEARCH
      FIELD_TAB       = LT_FIELD_TAB
      RETURN_TAB      = LT_RETURN
      "dynpfld_mapping = lt_dynpfld_mapping
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0  ##NEEDED.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " F_F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*& Form GET_FIELDS_OF_VALUE_TAB
*&---------------------------------------------------------------------*
*& Get Fields of Value Tab
*&---------------------------------------------------------------------*
FORM GET_FIELDS_OF_VALUE_TAB  TABLES T_VALUE  TYPE TT_SEARCH  ##NEEDED
                                     T_FIELD  TYPE TT_DFIES
                              CHANGING CF_RETFIELD TYPE DFIES-FIELDNAME.
  DATA: LS_FIELD        LIKE LINE OF T_FIELD.
  DATA: LF_HLP(61).
  DATA: LF_OFFSET       TYPE DFIES-OFFSET.
  DATA: LF_DFIES_ZWI    TYPE DFIES.
  DATA: LF_DTELINFO_WA  TYPE DTELINFO.
  DATA: LF_TABNAME      TYPE DD03P-TABNAME.
  DATA: LF_FIELDNAME    TYPE DFIES-LFIELDNAME.
  FIELD-SYMBOLS: <L_VALUE>  TYPE ANY.
  DATA: LF_INDEX LIKE SY-INDEX.
  DATA: LF_N(4) TYPE N.

  DESCRIBE FIELD T_VALUE HELP-ID LF_HLP.
  DO.
    LF_INDEX = SY-INDEX.
    ASSIGN COMPONENT LF_INDEX OF STRUCTURE T_VALUE TO <L_VALUE>.
    IF SY-SUBRC NE 0 .
      EXIT.
    ENDIF.
    DESCRIBE FIELD <L_VALUE> HELP-ID LF_HLP.
    SPLIT LF_HLP AT '-' INTO LF_TABNAME
                             LF_FIELDNAME.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        TABNAME        = LF_TABNAME
        LFIELDNAME     = LF_FIELDNAME
        ALL_TYPES      = 'X'
      IMPORTING
*       X030L_WA       =
*       DDOBJTYPE      =
        DFIES_WA       = LF_DFIES_ZWI
*        TABLES
*       DFIES_TAB      = DFIES_ZWI
      EXCEPTIONS
        NOT_FOUND      = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.
    CHECK SY-SUBRC = 0.
    DESCRIBE DISTANCE BETWEEN T_VALUE AND <L_VALUE>
             INTO LF_DFIES_ZWI-OFFSET IN BYTE MODE.
    CLEAR LF_DFIES_ZWI-TABNAME.
    LF_DFIES_ZWI-POSITION = LF_INDEX.
    LF_N = LF_INDEX.
    CONCATENATE 'F' LF_N INTO LF_DFIES_ZWI-FIELDNAME.
    LF_DFIES_ZWI-MASK+2(1) = 'X'.         "Rollname für F1-Hilfe verantw.
*   Das Flag F4-Available muß jetzt aber aus dem DTEL kommen.
    CLEAR: LF_DFIES_ZWI-F4AVAILABL,
           LF_DTELINFO_WA.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        TABNAME     = LF_DFIES_ZWI-ROLLNAME
        ALL_TYPES   = 'X'
      IMPORTING
        DTELINFO_WA = LF_DTELINFO_WA
      EXCEPTIONS
        OTHERS      = 0.
    LF_DFIES_ZWI-F4AVAILABL = LF_DTELINFO_WA-F4AVAILABL.

    IF LF_INDEX = 1.
      LF_DFIES_ZWI-REPTEXT     =  'Product Hierarchy'(T01).
      LF_DFIES_ZWI-SCRTEXT_S   =  'Product Hierarchy'(T01).
      LF_DFIES_ZWI-SCRTEXT_M   =  'Product Hierarchy'(T01).
      LF_DFIES_ZWI-SCRTEXT_L   =  'Product Hierarchy'(T01).
    ELSE.
      LF_DFIES_ZWI-REPTEXT     =  'Level'(T02).
      LF_DFIES_ZWI-SCRTEXT_S   =  'Level'(T02).
      LF_DFIES_ZWI-SCRTEXT_M   =  'Level'(T02).
      LF_DFIES_ZWI-SCRTEXT_L   =  'Level'(T02).
    ENDIF.

    APPEND LF_DFIES_ZWI TO T_FIELD.
  ENDDO.
  ASSIGN COMPONENT CF_RETFIELD OF STRUCTURE T_VALUE TO <L_VALUE>.
  DESCRIBE DISTANCE BETWEEN T_VALUE AND <L_VALUE>
           INTO LF_OFFSET IN BYTE MODE.
  READ TABLE T_FIELD INTO LS_FIELD
  WITH KEY OFFSET = LF_OFFSET.
  IF SY-SUBRC = 0.
    CF_RETFIELD = LS_FIELD-FIELDNAME.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DELIVERY
*&---------------------------------------------------------------------*
*& Get Delivery
*&---------------------------------------------------------------------*
FORM F_GET_DELIVERY .
  TYPES: BEGIN OF TS_DO_QTY2,
           VBELN TYPE LIPS-VBELN,
           POSNR TYPE LIPS-POSNR,
           VGBEL TYPE LIPS-VGBEL,
           VGPOS TYPE LIPS-VGPOS,
         END OF TS_DO_QTY2.
  TYPES:  TT_DO_QTY2       TYPE SORTED TABLE OF TS_DO_QTY2
                           WITH UNIQUE KEY VBELN
                                           POSNR
                                           VGBEL
                                           VGPOS.
  DATA: LT_DO_QTY2 TYPE TT_DO_QTY2.

  CHECK NOT GT_SALES_ORDER IS INITIAL.

  SELECT FROM LIPS                                 "#EC CI_NO_TRANSFORM
  INNER JOIN LIKP ON LIKP~VBELN = LIPS~VBELN
  FIELDS LIPS~VBELN,
         LIPS~POSNR,
         LIPS~LFIMG,
         LIPS~VGBEL,
         LIPS~VGPOS
  FOR ALL ENTRIES IN @GT_SALES_ORDER
  WHERE VGBEL EQ @GT_SALES_ORDER-VBELN
    AND VGPOS EQ @GT_SALES_ORDER-POSNR
  INTO TABLE @DATA(LT_DELIVERY).
  SORT LT_DELIVERY BY VBELN POSNR ASCENDING.

  LOOP AT LT_DELIVERY ASSIGNING FIELD-SYMBOL(<L_DELIVERY>).
* >>>>> v1.00 DELETE START >>>>> *
*    "Append DO Stock at ref SO level
*    READ TABLE GT_DO_QTY ASSIGNING FIELD-SYMBOL(<L_DO_QTY>)
*    WITH KEY  VGBEL = <L_DELIVERY>-VGBEL
*              VGPOS = <L_DELIVERY>-VGPOS
*    BINARY SEARCH.
*    IF SY-SUBRC NE 0.
*      INSERT VALUE TS_DO_QTY( VGBEL = <L_DELIVERY>-VGBEL
*                              VGPOS = <L_DELIVERY>-VGPOS
*                              LFIMG = <L_DELIVERY>-LFIMG )
*      INTO TABLE GT_DO_QTY
*      ASSIGNING <L_DO_QTY>.
*    ENDIF.
* <<<<< v1.00 DELETE END   <<<<< *
* >>>>> v1.00 INSERT START >>>>> *
    "Append DO Stock at ref SO level
    READ TABLE LT_DO_QTY2 ASSIGNING FIELD-SYMBOL(<L_DO_QTY2>)
    WITH KEY  VBELN = <L_DELIVERY>-VBELN
              POSNR = <L_DELIVERY>-POSNR
              VGBEL = <L_DELIVERY>-VGBEL
              VGPOS = <L_DELIVERY>-VGPOS
    BINARY SEARCH.
    IF SY-SUBRC NE 0.
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
    ENDIF.
* <<<<< v1.00 INSERT END   <<<<< *
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INIT_GLOB_VAR
*&---------------------------------------------------------------------*
*& Initialize Global Variable
*&---------------------------------------------------------------------*
FORM F_INIT_GLOB_VAR .
  CLEAR:  GT_RESULT,
          GT_MATERIAL,
          GT_HEAD_KEY,
          GT_SALES_ORDER,
          GT_SO_QTY,
          GT_DO_QTY, "420000541++
          GT_PRICE,
          GRT_AVAIL_SLOC_FG.
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
  DATA: LT_AVAIL_SLOC TYPE STANDARD TABLE OF LTS_AVAIL_SLOC.
  DATA: LS_AVAIL_SLOC LIKE LINE OF GRT_AVAIL_SLOC_FG.

  SELECT FROM ZSDSMMC002
  FIELDS WERKS,
         MTART,
         LGORT
  WHERE WERKS IN @S_WERKS
  INTO TABLE @LT_AVAIL_SLOC.

  LOOP AT LT_AVAIL_SLOC ASSIGNING FIELD-SYMBOL(<L_AVAIL_SLOC>).
    CLEAR LS_AVAIL_SLOC.
    LS_AVAIL_SLOC     = 'IEQ'.
    LS_AVAIL_SLOC-LOW = <L_AVAIL_SLOC>-LGORT.
    APPEND LS_AVAIL_SLOC TO GRT_AVAIL_SLOC_FG.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MAT_VALUATION
*&---------------------------------------------------------------------*
*& Get Material Valuation
*&---------------------------------------------------------------------*
FORM F_GET_MAT_VALUATION .
  DATA: LF_BWKEY  TYPE MBEW-BWKEY VALUE '1000'.
  DATA: LF_BWTAR  TYPE MBEW-BWTAR VALUE SPACE.

  CHECK NOT GT_MATERIAL IS INITIAL.

  SELECT FROM MBEW
  FIELDS MATNR,
         BWKEY,
         BWTAR,
         VERPR
  FOR ALL ENTRIES IN @GT_MATERIAL
  WHERE MATNR EQ @GT_MATERIAL-MATNR
    AND BWKEY EQ @LF_BWKEY
    AND BWTAR EQ @LF_BWTAR
  INTO TABLE @GT_MAT_VAL.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_PART_GROUP
*&---------------------------------------------------------------------*
*& Check Part Group
*&---------------------------------------------------------------------*
FORM F_CHECK_PART_GROUP .
  DATA: LS_MATNR  LIKE LINE OF S_MATNR.

  IF NOT S_MATNR[] IS INITIAL.
    "Get Part Group
    SELECT FROM ZRTCAC002  "#EC CI_FAE_LINES_ENSURED  "#EC CI_SGLSELECT
    FIELDS PART_GROUP
    WHERE MATNR IN @S_MATNR
    INTO TABLE @DATA(LT_PG).
    SORT LT_PG BY PART_GROUP ASCENDING.
    IF SY-SUBRC EQ 0.
      "Get all materials in part group
      SELECT FROM ZRTCAC002 "#EC CI_FAE_LINES_ENSURED  "#EC CI_SGLSELECT
      FIELDS PART_GROUP,
             MATNR
      FOR ALL ENTRIES IN @LT_PG
      WHERE PART_GROUP EQ @LT_PG-PART_GROUP
      INTO TABLE @DATA(LT_PG_MAT).
      SORT LT_PG_MAT BY PART_GROUP MATNR ASCENDING.
    ENDIF.
  ENDIF.

  LOOP AT LT_PG_MAT ASSIGNING FIELD-SYMBOL(<L_PG_MAT>).
    LS_MATNR-SIGN    = 'I'.
    LS_MATNR-OPTION  = 'EQ'.
    LS_MATNR-LOW     = <L_PG_MAT>-MATNR.
    APPEND LS_MATNR TO S_MATNR.
  ENDLOOP.
  SORT S_MATNR[].
  DELETE ADJACENT DUPLICATES FROM S_MATNR COMPARING ALL FIELDS.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANT
*&---------------------------------------------------------------------*
*& Get Constant
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANT .
  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'FG_MAT_TYPE'
                                        IMPORTING ET_RANGE = GRT_FG_MAT_TYPE ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'SP_MAT_TYPE'
                                        IMPORTING ET_RANGE = GRT_SP_MAT_TYPE ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'PLANT'
                                        IMPORTING ET_RANGE = GRT_PLANT ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DEFAULT_SELSCREEN
*&---------------------------------------------------------------------*
*& Set Default to Selection Screen
*&---------------------------------------------------------------------*
FORM F_DEFAULT_SELSCREEN .
  DATA: LS_WERKS  LIKE LINE OF S_WERKS.

  LOOP AT GRT_PLANT ASSIGNING FIELD-SYMBOL(<L_PLANT>).
    MOVE-CORRESPONDING <L_PLANT> TO LS_WERKS.
    APPEND LS_WERKS TO S_WERKS.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_COMMON_PART
*&---------------------------------------------------------------------*
*& Check Commmon Part
*&---------------------------------------------------------------------*
FORM F_CHECK_COMMON_PART .
  IF SY-UCOMM EQ 'UC2'.
    SUBMIT ZSDSMMR0520 WITH S_MATNR IN S_MATNR           "#EC CI_SUBMIT
    VIA SELECTION-SCREEN
    AND RETURN.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  CASE UF_UCOMM.
    WHEN 'REFRESH'.
      PERFORM F_INIT_GLOB_VAR.
      PERFORM F_CHECK_PART_GROUP.
      PERFORM F_MODIFY_SELOPT.
      PERFORM F_GET_AVAIL_SLOC.
      PERFORM F_GET_DATA.
      PERFORM F_PREPARE_RESULT_FG.
      PERFORM F_PREPARE_RESULT_SP.
      CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
        EXPORTING
          I_SOFT_REFRESH = ' '.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PART_GROUP
*&---------------------------------------------------------------------*
*& Get Part Group
*&---------------------------------------------------------------------*
FORM F_GET_PART_GROUP .
  CHECK NOT GT_MATERIAL IS INITIAL.
  CHECK RB_SP EQ GC_TRUE.

  SELECT FROM ZRTCAC002                                 "#EC CI_GENBUFF
  FIELDS MATNR,
         PART_GROUP,
         ACTIVE
  FOR ALL ENTRIES IN @GT_MATERIAL
  WHERE MATNR EQ @GT_MATERIAL-MATNR
  INTO TABLE @GT_PART_GROUP.

ENDFORM.
