*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0230
*  Creation Date      : 30.04.2024
*  Author             : Jutamas Y.(Eviden)
*  Add-on ID          : ZMMI013
*  Description        : Spare Part usage by CM Order
*  Purpose            : Interface file after Post MIGO
*  Copied from        : ZR_MM_SAP_TO_WMS_SVO
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0230.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
*  SSCRFIELDS,
  MKPF.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSMMS017.
TYPES:
         LINECOLOR TYPE  CHAR4,
       END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.



TYPES: BEGIN OF TS_INTF_TXT,
         ZFLAG(1)          TYPE  C,
         CUST_ID(12)       TYPE  C,
         MBLNR(10)         TYPE  C,
         BUDAT(10)         TYPE  C,
         CPUDT(10)         TYPE  C,
         ZEILE(5)          TYPE  C,
         MATNR(40)         TYPE  C,
         MAKTX(40)         TYPE  C,
         MENGE(13)         TYPE  C,
         SUPPLIER_CODE(10) TYPE  C,
         SUPPLIER_NAME(35) TYPE  C,
         DELIVERY_TO(10)   TYPE  C,
         REMARK(16)        TYPE  C,
         PICHON(16)        TYPE  C,
         REMARK_PICHON(16) TYPE  C,
         LGORT(10)         TYPE  C,
       END OF TS_INTF_TXT.

*TYPES: TT_INTF_TXT TYPE SORTED TABLE OF TS_INTF_TXT
*                  WITH UNIQUE KEY CUST_ID
*                                  MBLNR.



*TYPES: TT_INTF_CSV TYPE SORTED TABLE OF TS_INTF_CSV
*                   WITH UNIQUE KEY CUST_ID
*                                   MBLNR.

TYPES: BEGIN OF TS_SVO_GI_RE,
         MBLNR    TYPE MKPF-MBLNR,
         MJAHR    TYPE MKPF-MJAHR,
         ZEILE    TYPE MSEG-ZEILE,
         BUDAT    TYPE MKPF-BUDAT,
         CPUDT    TYPE MKPF-CPUDT,
         BLART    TYPE MKPF-BLART,
         XBLNR    TYPE MKPF-XBLNR,
         MATNR    TYPE MSEG-MATNR,
         MENGE    TYPE MSEG-MENGE,
         MEINS    TYPE MSEG-MEINS,
         LGORT    TYPE MSEG-LGORT,
         WERKS    TYPE MSEG-WERKS,
         BWART    TYPE MSEG-BWART,
         AUFNR    TYPE MSEG-AUFNR,
         RSNUM    TYPE MSEG-RSNUM,
         RSPOS    TYPE MSEG-RSPOS,
         SHKZG    TYPE MSEG-SHKZG,
         MAKTX    TYPE MAKT-MAKTX,
         EBELN    TYPE MSEG-EBELN,
         EBELP    TYPE MSEG-EBELP,
         UMLGO    TYPE MSEG-UMLGO,
         LIFNR    TYPE MSEG-LIFNR,
         KUNNR    TYPE MSEG-KUNNR,
         VBELN_IM TYPE MSEG-VBELN_IM,
         VBELP_IM TYPE MSEG-VBELP_IM,
         PO_BSART TYPE EKKO-BSART,
         LGOBE    TYPE T001L-LGOBE,
       END OF TS_SVO_GI_RE.
TYPES: TT_SVO_GI_RE TYPE SORTED TABLE OF TS_SVO_GI_RE
                    WITH UNIQUE KEY MBLNR
                                    MJAHR
                                    ZEILE.

TYPES: BEGIN OF TS_CUSTOMER ,
         VBELN TYPE LIKP-VBELN,
         KUNAG TYPE LIKP-KUNAG,
         NAME1 TYPE KNA1-NAME1,
         VGBEL TYPE LIPS-VGBEL,
       END OF  TS_CUSTOMER .

TYPES: BEGIN OF TS_VENDOR ,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END   OF TS_VENDOR .

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE        TYPE  CHAR1     VALUE 'X',
*  GC_TCODE  TYPE  SY-TCODE  VALUE 'ZSDSMM006',

  GC_INTFNO_SFDC TYPE  ZSDSCAC004-INTFNO VALUE 'MMI013', "SFDC
  GC_INTFNO_SONY TYPE  ZSDSCAC004-INTFNO VALUE 'MMI014'. "Sony



*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
##NEEDED
DATA:
  GT_RESULT     TYPE TT_RESULT,
  GT_SVO_GI_RE  TYPE TT_SVO_GI_RE,
  GT_MBLNR      TYPE STANDARD TABLE OF MKPF-MBLNR,
  GT_MJAHR      TYPE STANDARD TABLE OF MKPF-MJAHR,
  GT_ZSDSMMT016 TYPE STANDARD TABLE OF ZSDSMMT016,
  GT_ZSDSMMT017 TYPE STANDARD TABLE OF ZSDSMMT017,
  GT_CUSTOMER   TYPE STANDARD TABLE OF TS_CUSTOMER,
  GT_VENDOR     TYPE STANDARD TABLE OF TS_VENDOR.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA GF_ERROR TYPE CHAR40 ##NEEDED.
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
  GRT_VGART TYPE  RANGE OF MKPF-VGART                      ##NEEDED,
  GRT_BWART TYPE  RANGE OF MSEG-BWART                      ##NEEDED.
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS017'.

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



*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_INITAIL_DATA .
  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.
    SELECT-OPTIONS:
      S_MBLNR  FOR MKPF-MBLNR ,
      S_MJAHR  FOR MKPF-MJAHR .

    PARAMETERS:
      P_TEST TYPE  FLAG,
      P_WAIT TYPE  FLAG NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK B1.


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_DATA CHANGING GF_ERROR .
  IF GF_ERROR IS NOT INITIAL .
    MESSAGE S000(ZSDSCA01) DISPLAY LIKE 'E' WITH GF_ERROR '' '' ''.
    RETURN.
  ENDIF.

  PERFORM F_SET_DATA CHANGING GT_RESULT.
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01) DISPLAY LIKE 'E'.
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

*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANTS
*&---------------------------------------------------------------------*
*& Get GenC Constants
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANTS .
  CONSTANTS:
    LC_VGART TYPE  ZSDSDE_PARAM_NAME VALUE 'TRANSACTION_TYPE',
    LC_BWART TYPE  ZSDSDE_PARAM_NAME VALUE 'MOVEMENT_TYPE'.
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
  CLEAR: GRT_VGART.

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
*     Transaction Type
*     ------------------------------------
      WHEN LC_VGART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GRT_VGART.

*     ------------------------------------
*     Movement Type
*     ------------------------------------

      WHEN LC_BWART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GRT_BWART.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
FORM F_GET_DATA CHANGING CF_ERROR TYPE CHAR40.

  DATA: LF_SUBRC TYPE SY-SUBRC.

  IF P_WAIT IS NOT INITIAL .
    DO 300 TIMES.
      PERFORM F_GET_MATDOC CHANGING LF_SUBRC
                                    CF_ERROR .
      IF LF_SUBRC EQ 0. "found data
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS .
    ENDDO.

  ELSE.
    PERFORM F_GET_MATDOC CHANGING LF_SUBRC
                                  CF_ERROR.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MATDOC
*&---------------------------------------------------------------------*
FORM F_GET_MATDOC CHANGING CF_SUBRC TYPE SY-SUBRC
                           CF_ERROR TYPE CHAR40.

  CLEAR: CF_SUBRC , CF_ERROR .
  SELECT A~MBLNR, A~MJAHR, B~ZEILE, A~BUDAT, A~CPUDT, A~BLART, A~XBLNR,
         B~MATNR, B~MENGE, B~MEINS, B~LGORT, B~WERKS, B~BWART,
         B~AUFNR, B~RSNUM, B~RSPOS, B~SHKZG, D~MAKTX,
         B~EBELN, B~EBELP, B~UMLGO, B~LIFNR, B~KUNNR,
         B~VBELN_IM, B~VBELP_IM,
         E~BSART AS PO_BSART,
         F~LGOBE
  FROM MKPF AS A INNER JOIN MSEG AS B
                 ON ( A~MBLNR EQ B~MBLNR )
                 INNER JOIN MAKT AS D
                 ON ( B~MATNR EQ D~MATNR
                 AND  D~SPRAS EQ 'E' )
                 LEFT OUTER JOIN EKKO AS E
                 ON ( B~EBELN EQ E~EBELN )
                 LEFT OUTER JOIN T001L AS F
                 ON ( B~WERKS EQ F~WERKS AND
                      B~UMLGO EQ F~LGORT )             "#EC CI_BUFFJOIN
  WHERE A~MBLNR IN @S_MBLNR
    AND A~MJAHR IN @S_MJAHR
    AND A~VGART IN @GRT_VGART
   INTO TABLE @GT_SVO_GI_RE .

  CF_SUBRC = SY-SUBRC .

  IF GT_SVO_GI_RE[] IS NOT INITIAL .
    SELECT *
      FROM ZSDSMMT016
      INTO TABLE GT_ZSDSMMT016
       FOR ALL ENTRIES IN GT_SVO_GI_RE
     WHERE WERKS = GT_SVO_GI_RE-WERKS .            "#EC CI_NO_TRANSFORM
    IF SY-SUBRC = 0.
      SORT GT_ZSDSMMT016 BY WERKS LGORT .
    ELSE.
      "Text-E01 : Please maintain data in table ZSDSMMT016
      CF_ERROR = TEXT-E01 .
      RETURN.
    ENDIF.

    SELECT *
      FROM ZSDSMMT017
      INTO TABLE GT_ZSDSMMT017
       FOR ALL ENTRIES IN GT_SVO_GI_RE
     WHERE BWART = GT_SVO_GI_RE-BWART .            "#EC CI_NO_TRANSFORM
    IF SY-SUBRC = 0 .
      SORT GT_ZSDSMMT017 BY BWART ISSUE_SLOC .
    ELSE.
      "Text-E02 : Please maintain data in table ZSDSMMT017
      CF_ERROR = TEXT-E02 .
      RETURN .
    ENDIF.

    SELECT LIKP~VBELN LIKP~KUNAG NAME1 LIPS~VGBEL
      INTO TABLE GT_CUSTOMER
      FROM LIKP INNER JOIN KNA1 ON LIKP~KUNAG = KNA1~KUNNR
                INNER JOIN LIPS ON LIKP~VBELN = LIPS~VBELN
       FOR ALL ENTRIES IN GT_SVO_GI_RE
     WHERE LIPS~VBELN = GT_SVO_GI_RE-VBELN_IM
       AND LIPS~POSNR = GT_SVO_GI_RE-VBELP_IM .    "#EC CI_NO_TRANSFORM
    IF SY-SUBRC = 0 .
      SORT GT_CUSTOMER BY VBELN KUNAG .
    ENDIF.

    SELECT LIFNR NAME1 INTO TABLE GT_VENDOR
      FROM LFA1
       FOR ALL ENTRIES IN GT_SVO_GI_RE
     WHERE LIFNR = GT_SVO_GI_RE-LIFNR.             "#EC CI_NO_TRANSFORM
    IF SY-SUBRC = 0 .
      SORT GT_VENDOR BY LIFNR .
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SET_DATA
*&---------------------------------------------------------------------*
FORM F_SET_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  CONSTANTS:
    LC_ZOUB   TYPE EKKO-BSART VALUE 'ZOUB',

    LC_SL_DO  TYPE ZSDSMMS017-DELIVERY_TO VALUE 'SL_DO',
    LC_RETURN TYPE ZSDSMMS017-DELIVERY_TO VALUE 'RETURN',
    LC_REC    TYPE ZSDSMMS017-DELIVERY_TO VALUE 'REC',

    LC_601    TYPE MSEG-BWART VALUE '601',
    LC_602    TYPE MSEG-BWART VALUE '602'.
  DATA:
    LS_RESULT     TYPE TS_RESULT,
    LF_ERROR      TYPE FLAG VALUE 'X',
    LF_ISSUE_TYPE TYPE ZSDSMMT016-SLOC_TYPE,
    LF_RECV_TYPE  TYPE ZSDSMMT016-SLOC_TYPE.

  LOOP AT GT_SVO_GI_RE ASSIGNING FIELD-SYMBOL(<L_SVO_GI_RE>).
    CLEAR: LS_RESULT, LF_ISSUE_TYPE, LF_RECV_TYPE .

    READ TABLE GT_ZSDSMMT016 ASSIGNING FIELD-SYMBOL(<L_MMT016>)
    WITH KEY WERKS = <L_SVO_GI_RE>-WERKS
             LGORT = <L_SVO_GI_RE>-LGORT .
    IF SY-SUBRC = 0 .
      LF_ISSUE_TYPE = <L_MMT016>-SLOC_TYPE .
    ENDIF.

    READ TABLE GT_ZSDSMMT016 ASSIGNING <L_MMT016>
    WITH KEY WERKS = <L_SVO_GI_RE>-WERKS
             LGORT = <L_SVO_GI_RE>-UMLGO .
    IF SY-SUBRC = 0 .
      LF_RECV_TYPE = <L_MMT016>-SLOC_TYPE .
    ENDIF.

    LOOP AT GT_ZSDSMMT017 ASSIGNING FIELD-SYMBOL(<L_MMT017>)
                          WHERE ISSUE_SLOC = LF_ISSUE_TYPE
                            AND RECV_SLOC = LF_RECV_TYPE . "#EC CI_NESTED.
      CASE <L_MMT017>-COND_DOC_TYPE .
        WHEN '1' .
          IF <L_SVO_GI_RE>-EBELN IS NOT INITIAL AND
             <L_SVO_GI_RE>-PO_BSART EQ LC_ZOUB .

            CLEAR LF_ERROR .
          ELSE.
            LF_ERROR = GC_TRUE .
          ENDIF.

        WHEN '2' .
          IF <L_SVO_GI_RE>-EBELN IS NOT INITIAL AND
             <L_SVO_GI_RE>-PO_BSART NE LC_ZOUB .

            CLEAR LF_ERROR .
          ELSE.
            LF_ERROR = GC_TRUE .
          ENDIF.

        WHEN OTHERS.
          CLEAR LF_ERROR .
      ENDCASE.

      IF LF_ERROR IS INITIAL .
        LS_RESULT-DELIVERY_TO     = <L_MMT017>-DELIVERY_TO  .
        LS_RESULT-SENT_SALESFORCE = <L_MMT017>-SENT_SALESFORCE .
        LS_RESULT-SENT_SONY       = <L_MMT017>-SENT_SONY .
        LS_RESULT-PREFIX_FILENAME = <L_MMT017>-PREFIX_FILENAME .
      ENDIF.

    ENDLOOP.

    CHECK LS_RESULT-DELIVERY_TO IS NOT INITIAL.

    IF <L_SVO_GI_RE>-SHKZG EQ 'S'.
      LS_RESULT-TYPE_FLAG = '0'.
      "Text-A01: RE-GI
      LS_RESULT-TYPE_FLAG_DESC = TEXT-A01 .
      LS_RESULT-MENGE_SHOW = <L_SVO_GI_RE>-MENGE * -1.
    ELSE.
      LS_RESULT-TYPE_FLAG = '1'.
      "Text-A02: GI
      LS_RESULT-TYPE_FLAG_DESC = TEXT-A02 .
      LS_RESULT-MENGE_SHOW  = <L_SVO_GI_RE>-MENGE.
    ENDIF.

    LS_RESULT-MBLNR  = <L_SVO_GI_RE>-MBLNR.
    LS_RESULT-MJAHR  = <L_SVO_GI_RE>-MJAHR.
    LS_RESULT-BUDAT  = <L_SVO_GI_RE>-BUDAT.
    LS_RESULT-CPUDT  = <L_SVO_GI_RE>-CPUDT.
    LS_RESULT-ZEILE  = <L_SVO_GI_RE>-ZEILE.
    LS_RESULT-MATNR  = <L_SVO_GI_RE>-MATNR.
    LS_RESULT-MAKTX  = <L_SVO_GI_RE>-MAKTX.
    LS_RESULT-MENGE  = <L_SVO_GI_RE>-MENGE.
    LS_RESULT-MEINS  = <L_SVO_GI_RE>-MEINS.

    CASE LS_RESULT-DELIVERY_TO  .
      WHEN LC_SL_DO OR  LC_RETURN .
        READ TABLE GT_CUSTOMER INTO DATA(LS_CUSTOMER)
          WITH KEY VBELN =  <L_SVO_GI_RE>-VBELN_IM
          BINARY SEARCH.
        IF SY-SUBRC = 0 .
          LS_RESULT-SUPPLIER_CODE = LS_CUSTOMER-KUNAG .
          LS_RESULT-SUPPLIER_NAME = LS_CUSTOMER-NAME1.
          LS_RESULT-PICHON = LS_CUSTOMER-VGBEL .
        ENDIF.
      WHEN LC_REC .
        LS_RESULT-SUPPLIER_CODE = <L_SVO_GI_RE>-LIFNR.
        READ TABLE GT_VENDOR INTO DATA(LS_VENDOR)
          WITH KEY LIFNR = <L_SVO_GI_RE>-LIFNR.
        IF SY-SUBRC = 0 .
          LS_RESULT-SUPPLIER_NAME = LS_VENDOR-NAME1 .
        ENDIF.
      WHEN OTHERS .
        LS_RESULT-SUPPLIER_CODE = <L_SVO_GI_RE>-UMLGO .
        LS_RESULT-SUPPLIER_NAME = <L_SVO_GI_RE>-LGOBE.
    ENDCASE.

    CONCATENATE <L_SVO_GI_RE>-RSNUM  <L_SVO_GI_RE>-RSPOS
           INTO  LS_RESULT-REMARK SEPARATED BY '/'.

    IF  ( <L_SVO_GI_RE>-BWART = LC_601 ) OR
        ( <L_SVO_GI_RE>-BWART = LC_602 ) OR
        ( <L_SVO_GI_RE>-BWART+0(1) = 'Z' ) .
      LS_RESULT-PICHON = LS_CUSTOMER-VGBEL .
    ELSE.
      LS_RESULT-PICHON =  <L_SVO_GI_RE>-XBLNR .
    ENDIF.

    LS_RESULT-LGORT  = <L_SVO_GI_RE>-LGORT.
    APPEND LS_RESULT TO CT_RESULT.

  ENDLOOP.

* Create Interface File
  IF P_TEST IS INITIAL.
    PERFORM F_CREATE_INTF_FILE  USING  CT_RESULT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_INTF_FILE
*&---------------------------------------------------------------------*
*& Create Interface File
*&---------------------------------------------------------------------*
FORM F_CREATE_INTF_FILE  USING  UT_RESULT TYPE TT_RESULT.

  DATA:
*    LT_KEY      TYPE  TT_XBLNR,
*    LT_INV      TYPE  STANDARD TABLE OF ZSDSMMT007,
    LT_INTF_TXT TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT,
    LT_INTF_CSV TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA:
    LS_INTF_TXT TYPE  TS_INTF_TXT,
    LS_RETURN   TYPE  BAPIRET2.

  DATA:
    LF_DATUM      TYPE  SY-DATUM,
    LF_UZEIT      TYPE  SY-UZEIT,
    LF_FILENAME_I TYPE  STRING,
    LF_FILEPATH_O TYPE  STRING   ##NEEDED,
    LF_FILENAME_O TYPE  STRING   ##NEEDED.

  DATA: LF_PREFIX_FILE TYPE CHAR10,
        LF_SALEFORCE   TYPE FLAG,
        LF_SONY        TYPE FLAG,
        LF_MBLNR       TYPE MBLNR.

* Processing Log
  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.

  CLEAR: LT_INTF_TXT, LT_INTF_CSV,
         LF_PREFIX_FILE, LF_SALEFORCE , LF_SONY .

  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>) .
*   Interface File data
    CLEAR LS_INTF_TXT.
    LS_INTF_TXT-ZFLAG     = <L_RESULT>-TYPE_FLAG.

    "Text-A03 : 'SDS'
    LS_INTF_TXT-CUST_ID   = TEXT-A03.

    LS_INTF_TXT-MBLNR     = <L_RESULT>-MBLNR.
    CONCATENATE <L_RESULT>-BUDAT+0(4)
                <L_RESULT>-BUDAT+4(2)
                <L_RESULT>-BUDAT+6(2)
           INTO LS_INTF_TXT-BUDAT SEPARATED BY '-'.

    CONCATENATE <L_RESULT>-CPUDT+0(4)
                <L_RESULT>-CPUDT+4(2)
                <L_RESULT>-CPUDT+6(2)
       INTO LS_INTF_TXT-CPUDT SEPARATED BY '-' .

    LS_INTF_TXT-ZEILE   = <L_RESULT>-ZEILE.
    LS_INTF_TXT-MATNR   = <L_RESULT>-MATNR.
    LS_INTF_TXT-MAKTX   = <L_RESULT>-MAKTX.
    LS_INTF_TXT-MENGE   = <L_RESULT>-MENGE.
    LS_INTF_TXT-SUPPLIER_CODE = <L_RESULT>-SUPPLIER_CODE .
    LS_INTF_TXT-SUPPLIER_NAME = <L_RESULT>-SUPPLIER_NAME .
    LS_INTF_TXT-REMARK  = <L_RESULT>-REMARK.
    LS_INTF_TXT-DELIVERY_TO  = <L_RESULT>-DELIVERY_TO.
    LS_INTF_TXT-PICHON  = <L_RESULT>-PICHON.
    LS_INTF_TXT-REMARK_PICHON = ' ' .
    LS_INTF_TXT-LGORT   = <L_RESULT>-LGORT.

    IF <L_RESULT>-SENT_SALESFORCE IS NOT INITIAL .
      LF_SALEFORCE = ABAP_TRUE .
    ENDIF.

    IF <L_RESULT>-SENT_SONY IS NOT INITIAL .
      LF_SONY = ABAP_TRUE .
    ENDIF.

    IF LF_SONY IS NOT INITIAL .  "Text file
      APPEND LS_INTF_TXT TO LT_INTF_TXT.
    ENDIF.

    IF LF_SALEFORCE IS NOT INITIAL . "CSV file
      IF  LT_INTF_CSV IS INITIAL .
        "Add Header
        PERFORM F_ADD_HEADER_CSV CHANGING LT_INTF_CSV.
        "Add Item
        PERFORM F_ADD_ITEM_CSV USING LS_INTF_TXT
                            CHANGING LT_INTF_CSV.
      ELSE.
        "Add Item
        PERFORM F_ADD_ITEM_CSV USING LS_INTF_TXT
                            CHANGING LT_INTF_CSV.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "Set prefix filename and sent file to saleforce , sony
  CLEAR: LF_PREFIX_FILE,  LF_MBLNR .
  READ TABLE UT_RESULT ASSIGNING <L_RESULT> INDEX 1 .
  IF SY-SUBRC = 0 .
    LF_PREFIX_FILE = <L_RESULT>-PREFIX_FILENAME .
    LF_MBLNR       = <L_RESULT>-MBLNR .
  ENDIF.
  IF LF_SONY IS NOT INITIAL .  "Text file

* Create Interface file
    IF LT_INTF_TXT IS NOT INITIAL.
*   Assign Filename
      LF_FILENAME_I = LF_PREFIX_FILE && '_' && LF_MBLNR && '_' &&
                      LF_DATUM && LF_UZEIT && '.txt' ##NO_TEXT.

      CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
        EXPORTING
          IF_INTFNO   = GC_INTFNO_SONY
          IF_FILENAME = LF_FILENAME_I
          IT_DATATXT  = LT_INTF_TXT
        IMPORTING
          ES_RETURN   = LS_RETURN
          EF_FILEPATH = LF_FILEPATH_O
          EF_FILENAME = LF_FILENAME_O.

      "Write path file , path name
      "Text-T02: Path Name:
      MESSAGE I000(38) WITH TEXT-T02 LF_FILENAME_O SPACE SPACE .
      IF LS_RETURN-TYPE NE 'S'.
*     Error
        MESSAGE ID LS_RETURN-ID TYPE 'I'
                NUMBER LS_RETURN-NUMBER
                DISPLAY LIKE LS_RETURN-TYPE
                WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                     LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
        RETURN.
      ENDIF.

    ENDIF.
  ENDIF.

  IF LT_INTF_CSV IS NOT INITIAL .
    IF LF_SALEFORCE IS NOT INITIAL .  "CSV file

*   Assign Filename
      LF_FILENAME_I = LF_PREFIX_FILE && '_' && LF_MBLNR && '_' &&
                      LF_DATUM  && LF_UZEIT && '.csv' ##NO_TEXT.

      CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
        EXPORTING
          IF_INTFNO   = GC_INTFNO_SFDC
          IF_FILENAME = LF_FILENAME_I
          IT_DATATXT  = LT_INTF_CSV
        IMPORTING
          ES_RETURN   = LS_RETURN
          EF_FILEPATH = LF_FILEPATH_O
          EF_FILENAME = LF_FILENAME_O.

      "Write path file , path name
      "Text-T02: Path Name:
      MESSAGE I000(38) WITH TEXT-T02 LF_FILENAME_O SPACE SPACE .

      IF LS_RETURN-TYPE NE 'S'.
*     Error
        MESSAGE ID LS_RETURN-ID TYPE 'I'
                NUMBER LS_RETURN-NUMBER
                DISPLAY LIKE LS_RETURN-TYPE
                WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                     LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& Display Processing Result
*&---------------------------------------------------------------------*
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
      WHEN 'MBLNR'.
*       text-C01 : Material Document
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MJAHR'.
*       text-C02 : Year
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BUDAT'.
*       text-C03 : Order Date
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CPUDT'.
*        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       text-C04 : Create Date
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ZEILE'.
*       text-C05 : Line Item
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MATNR'.
*       text-C06 : Product Code
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MAKTX'.
*       text-C07 : Product Description
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MENGE'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MENGE_SHOW'.
*       text-C08 : Quantity
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MEINS'.
*       text-C08 : Unit
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SUPPLIER_CODE'.
*       text-C11 : Supplier Code
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SUPPLIER_NAME'.
*       text-C12 : Supplier Name
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CUSTOMER_CODE'.
*       text-C13 : Customer Code
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CUSTOMER_NAME'.
*       text-C14 : Customer Name
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'REMARK'.
*       text-C15 : Remark
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PICHON'.
*       text-C16 : Pichon
        LF_TEXT                = TEXT-C16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'LGORT'.
*       text-C17 : Storate Location
        LF_TEXT                = TEXT-C17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.

      WHEN 'DELIVERY_TO'.
*       text-C18 : Delivery To
        LF_TEXT                = TEXT-C18.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.

      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_LAYOUT
*&---------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.

* For Variant Saving
*  CS_VARIANT-REPORT  = SY-REPID.
*  CS_VARIANT-VARIANT = P_VARI.
*  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_SORT_RESULT
*&---------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*& Form f_generate_bgjob
*&---------------------------------------------------------------------*
*& Generate BG Job
*&---------------------------------------------------------------------*
FORM F_GENERATE_BGJOB .
  DATA:
    LT_MBLNR TYPE RANGE OF MKPF-MBLNR,
    LT_MJAHR TYPE RANGE OF MKPF-MJAHR.

  DATA:
    LF_JOBNAME TYPE TBTCJOB-JOBNAME,
    LF_JOBNUM  TYPE TBTCJOB-JOBCOUNT.


  IF GT_MBLNR IS INITIAL.
    RETURN.
  ENDIF.

* Remove Duplicated
  SORT GT_MBLNR ASCENDING.
  DELETE ADJACENT DUPLICATES FROM GT_MBLNR COMPARING ALL FIELDS.

  READ TABLE GT_MBLNR ASSIGNING FIELD-SYMBOL(<L_MBLNR>)
                      INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  READ TABLE GT_MJAHR ASSIGNING FIELD-SYMBOL(<L_MJAHR>)
                      INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Generate JOB to call Interface
  CONCATENATE 'ZSDSMMR0230'
              <L_MBLNR>
              <L_MJAHR>
         INTO LF_JOBNAME
         SEPARATED BY '_'.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      JOBNAME          = LF_JOBNAME
    IMPORTING
      JOBCOUNT         = LF_JOBNUM
    EXCEPTIONS
      CANT_CREATE_JOB  = 1
      INVALID_JOB_DATA = 2
      JOBNAME_MISSING  = 3
      OTHERS           = 4.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Material doc and year
  LOOP AT GT_MBLNR ASSIGNING <L_MBLNR>.
    APPEND VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = <L_MBLNR> )
           TO LT_MBLNR.
  ENDLOOP.

  LOOP AT GT_MJAHR ASSIGNING <L_MJAHR>.
    APPEND VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = <L_MJAHR> )
           TO LT_MJAHR.
  ENDLOOP.

* Call GR Interface Program
  SUBMIT ZSDSMMR0230
    WITH S_MBLNR  IN LT_MBLNR
    WITH S_MJAHR  IN LT_MJAHR
    WITH P_TEST   EQ SPACE
    WITH P_WAIT   EQ GC_TRUE
    VIA JOB LF_JOBNAME NUMBER LF_JOBNUM
    AND RETURN.                                          "#EC CI_SUBMIT

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      JOBCOUNT             = LF_JOBNUM
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
      OTHERS               = 8.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Clear List
  CLEAR: GT_MBLNR, GT_MJAHR .
ENDFORM.

*----------------------------------------------------------------------*
*  Form f_call_b2p_interface
*----------------------------------------------------------------------*
*  Call B2P Interface
*----------------------------------------------------------------------*
FORM F_CALL_B2P_INTERFACE  USING US_MKPF TYPE MKPF .

* Collect Material document and fisical year for BG Job
  INSERT US_MKPF-MBLNR INTO TABLE GT_MBLNR.
  INSERT US_MKPF-MJAHR INTO TABLE GT_MJAHR.

* Generate BG Job when data COMMMIT
  PERFORM F_GENERATE_BGJOB ON COMMIT.
*PERFORM F_GENERATE_BGJOB .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_initail_data
*&---------------------------------------------------------------------*
*& Clear variable
*&---------------------------------------------------------------------*
FORM F_INITAIL_DATA .
  CLEAR:
    GT_RESULT,
    GT_SVO_GI_RE,
    GT_MBLNR,
    GT_MJAHR,
    GT_ZSDSMMT016,
    GT_ZSDSMMT017,
    GT_CUSTOMER,
    GT_VENDOR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_HEADER_CSV
*&---------------------------------------------------------------------*
*& Add Header text to CSV file
*&---------------------------------------------------------------------*
FORM F_ADD_HEADER_CSV
  CHANGING CT_INTF_TXT TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA LS_INTF_TXT  TYPE  TS_INTF_TXT .
  CONCATENATE TEXT-F01
              TEXT-F02
              TEXT-F03
              TEXT-F04
              TEXT-F05
              TEXT-F06
              TEXT-F07
              TEXT-F08
              TEXT-F09
              TEXT-F10
              TEXT-F11
              TEXT-F12
              TEXT-F13
              TEXT-F14
              TEXT-F15
              TEXT-F16
     INTO LS_INTF_TXT SEPARATED BY ',' .

  APPEND LS_INTF_TXT TO CT_INTF_TXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_ITEM_CSV
*&---------------------------------------------------------------------*
*& Add Item text to CSV file
*&---------------------------------------------------------------------*
FORM F_ADD_ITEM_CSV
  USING    US_INTF_TXT  TYPE  TS_INTF_TXT
  CHANGING CT_INTF_TXT  TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA LS_INTF_TXT  TYPE  TS_INTF_TXT .
  CONCATENATE '"' US_INTF_TXT-ZFLAG '",'
              '"' US_INTF_TXT-CUST_ID '",'
              '"' US_INTF_TXT-MBLNR '",'
              US_INTF_TXT-BUDAT ','
              US_INTF_TXT-CPUDT ','
              '"' US_INTF_TXT-ZEILE '",'
              '"' US_INTF_TXT-MATNR '",'
              '"' US_INTF_TXT-MAKTX '",'
              US_INTF_TXT-MENGE ','
              '"' US_INTF_TXT-SUPPLIER_CODE '",'
              '"' US_INTF_TXT-SUPPLIER_NAME '",'
              '"' US_INTF_TXT-DELIVERY_TO '",'
              '"' US_INTF_TXT-REMARK '",'
              '"' US_INTF_TXT-PICHON '",'
              '"' US_INTF_TXT-REMARK_PICHON '",'
              '"' US_INTF_TXT-LGORT '"'
         INTO LS_INTF_TXT  .

  APPEND LS_INTF_TXT TO CT_INTF_TXT.

ENDFORM.
