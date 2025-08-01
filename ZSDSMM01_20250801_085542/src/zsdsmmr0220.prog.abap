*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0220
*  Creation Date      : 18.04.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : ZMMI010 & ZMMR002
*  Description        : Visibility of Delivery from DIT(Detail)
*  Purpose            : Visibility of Delivery from DIT(Detail)
*  Copied from        : ZR_MM_DELIVERY_LIST_ACC
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0220.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  EKKO,
  EKPO,
  EKES,
  LIKP,
  MKPF,
  RBKP,
  BKPF.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSMMS016.
TYPES:   LINECOLOR  TYPE  CHAR4,
         SHRTSTK    TYPE  FLAG,
         EKPO_WERKS TYPE  EKPO-WERKS,
         EKPO_LGORT TYPE  EKPO-LGORT,
         MJAHR      TYPE  MKPF-MJAHR,
         BUKRS      TYPE  BKPF-BUKRS,
         GJAHR_IR   TYPE  RBKP-GJAHR,
         GJAHR_WE   TYPE  BKPF-GJAHR,
         GJAHR_RE   TYPE  BKPF-GJAHR,
       END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_EKES,
         EBELN     TYPE  EKPO-EBELN,
         EBELP     TYPE  EKPO-EBELP,
         ETENS     TYPE  EKES-ETENS,
         EINDT_EST TYPE  EKES-EINDT,
         FRGZU     TYPE  EKKO-FRGZU,
         ZZPRJTYP  TYPE  EKKO-ZZ1_PRJ_C_PO,
         AEDAT     TYPE  EKKO-AEDAT,
         MATNR     TYPE  EKPO-MATNR,
         WAERS     TYPE  EKKO-WAERS,
         WERKS     TYPE  EKPO-WERKS,
         LGORT     TYPE  EKPO-LGORT,
         LGOBE     TYPE  T001L-LGOBE,
         MEINS     TYPE  EKPO-MEINS,
         INSMK     TYPE  EKPO-INSMK,
         EBTYP     TYPE  EKES-EBTYP,
         VBELN     TYPE  EKES-VBELN,
         VBELP     TYPE  EKES-VBELP,
         XBLNR     TYPE  EKES-XBLNR,
         TRAID     TYPE  LIKP-ZTRUCKNO,
         EINDT_PGI TYPE  LIKP-ZWADAT_IST,
         EINTM_PGI TYPE  LIKP-ZWATIM_IST,
         NETPR     TYPE  EKPO-NETPR,
         MENGE_REQ TYPE  EKPO-MENGE,
         MENGE_DLV TYPE  EKES-MENGE,
         DABMG     TYPE  EKES-DABMG,
       END OF TS_EKES.
TYPES: TT_EKES TYPE SORTED TABLE OF TS_EKES
                      WITH UNIQUE KEY EBELN
                                      EBELP
                                      ETENS.

TYPES: BEGIN OF TS_GR,
         EBELN TYPE  EKPO-EBELN,
         EBELP TYPE  EKPO-EBELP,
         VBELN TYPE  EKES-VBELN,
         VBELP TYPE  EKES-VBELP,
         MBLNR TYPE  MSEG-MBLNR,
         MJAHR TYPE  MSEG-MJAHR,
         ZEILE TYPE  MSEG-ZEILE,
         WAERS TYPE  MSEG-WAERS,
         DMBTR TYPE  MSEG-DMBTR,
         MEINS TYPE  MSEG-MEINS,
         MENGE TYPE  MSEG-MENGE,
         CPUDT TYPE  MKPF-CPUDT,
         CPUTM TYPE  MKPF-CPUTM,
         BUKRS TYPE  BKPF-BUKRS,
         BELNR TYPE  BKPF-BELNR,
         GJAHR TYPE  BKPF-GJAHR,
       END OF TS_GR.
TYPES: TT_GR TYPE SORTED TABLE OF TS_GR
                    WITH UNIQUE KEY EBELN
                                    EBELP
                                    VBELN
                                    VBELP.

TYPES: BEGIN OF TS_IR,
         EBELN    TYPE  EKPO-EBELN,
         EBELP    TYPE  EKPO-EBELP,
         XBLNR    TYPE  EKES-XBLNR,
         BELNR    TYPE  RSEG-BELNR,
         GJAHR    TYPE  RSEG-GJAHR,
         BUZEI    TYPE  RSEG-BUZEI,
         WAERS    TYPE  RBKP-WAERS,
         WRBTR    TYPE  RSEG-WRBTR,
         BSTME    TYPE  RSEG-BSTME,
         MENGE    TYPE  RSEG-MENGE,
         RBSTAT   TYPE  RBKP-RBSTAT,
         BUDAT    TYPE  RBKP-BUDAT,
         FI_BUKRS TYPE  BKPF-BUKRS,
         FI_BELNR TYPE  BKPF-BELNR,
         FI_GJAHR TYPE  BKPF-GJAHR,
       END OF TS_IR.
TYPES: TT_IR TYPE SORTED TABLE OF TS_IR
                    WITH UNIQUE KEY EBELN
                                    EBELP
                                    XBLNR.

TYPES: BEGIN OF TS_EKET,
         EBELN     TYPE  EKPO-EBELN,
         EBELP     TYPE  EKPO-EBELP,
         EBTYP     TYPE  EKES-EBTYP,
         EINDT_EST TYPE  EKES-EINDT,
         EINDT_REQ TYPE  EKET-EINDT,
         MENGE     TYPE  EKET-MENGE,
       END OF TS_EKET.
TYPES: TT_EKET  TYPE SORTED TABLE OF TS_EKET
                       WITH UNIQUE KEY EBELN
                                       EBELP
                                       EBTYP
                                       EINDT_EST.

TYPES: BEGIN OF TS_LIPS,
         VBELN TYPE  LIPS-VBELN,
         VBELP TYPE  LIPS-POSNR,
       END OF TS_LIPS.
TYPES: TT_LIPS TYPE SORTED TABLE OF TS_LIPS
                      WITH UNIQUE KEY VBELN
                                      VBELP.

TYPES: BEGIN OF TS_EKPO,
         EBELN TYPE  EKPO-EBELN,
         EBELP TYPE  EKPO-EBELP,
       END OF TS_EKPO.
TYPES: TT_EKPO TYPE SORTED TABLE OF TS_EKPO
                      WITH UNIQUE KEY EBELN
                                      EBELP.

TYPES: BEGIN OF TS_XBLNR,
         EBELN TYPE  EKPO-EBELN,
         EBELP TYPE  EKPO-EBELP,
         XBLNR TYPE  EKES-XBLNR,
       END OF TS_XBLNR.
TYPES: TT_XBLNR TYPE SORTED TABLE OF TS_XBLNR
                       WITH UNIQUE KEY EBELN
                                       EBELP
                                       XBLNR.

TYPES: BEGIN OF TS_MATDOC,
         MBLNR TYPE  MSEG-MBLNR,
         MJAHR TYPE  MSEG-MJAHR,
         AWKEY TYPE  BKPF-AWKEY,
       END OF TS_MATDOC.
TYPES: TT_MATDOC TYPE SORTED TABLE OF TS_MATDOC
                        WITH UNIQUE KEY MBLNR
                                        MJAHR.

TYPES: BEGIN OF TS_INV,
         BELNR TYPE  RSEG-BELNR,
         GJAHR TYPE  RSEG-GJAHR,
         AWKEY TYPE  BKPF-AWKEY,
       END OF TS_INV.
TYPES: TT_INV TYPE SORTED TABLE OF TS_INV
                     WITH UNIQUE KEY BELNR
                                     GJAHR.

TYPES: BEGIN OF TS_EKES2,
         EBELN     TYPE  EKPO-EBELN,
         EBELP     TYPE  EKPO-EBELP,
         EBTYP     TYPE  EKES-EBTYP,
         EINDT     TYPE  EKES-EINDT,
         MENGE     TYPE  EKET-MENGE,
         MENGE_REQ TYPE  EKET-MENGE,
       END OF TS_EKES2.
TYPES: TT_EKES2  TYPE  SORTED TABLE OF TS_EKES2
                         WITH UNIQUE KEY EBELN
                                         EBELP
                                         EBTYP
                                         EINDT.

TYPES: BEGIN OF TS_INTF_TXT,
         ZINV_DIT TYPE CHAR10,
         VBELN    TYPE CHAR10,
         EBELN    TYPE CHAR10,
         EBELP    TYPE CHAR5,
         MATNR    TYPE CHAR18,
         MENGE    TYPE CHAR13,
         LGORT    TYPE CHAR4,
         EINDT    TYPE CHAR8,
       END OF TS_INTF_TXT.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE   TYPE  CHAR1     VALUE 'X',
  GC_TCODE  TYPE  SY-TCODE  VALUE 'ZSDSMM006',

  GC_INTFNO TYPE  ZSDSCAC004-INTFNO VALUE 'MMI010',
  GC_LA     TYPE  EKES-EBTYP VALUE 'LA'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT  TYPE  TT_RESULT                                   ##NEEDED.

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
  GR_BSART      TYPE  RANGE OF EKKO-BSART                      ##NEEDED,
  GR_INTF_LGORT TYPE RANGE OF EKPO-LGORT                       ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS016'.

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
* Text-s01: Conditions
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_VENDR FOR EKKO-LIFNR OBLIGATORY,   " Vender Code
    S_EBTYP FOR EKES-EBTYP,              " Conf. Status
    S_EINDT FOR EKES-EINDT OBLIGATORY,   " Estimate ETD
    S_XBLNR FOR EKES-XBLNR,              " TAX Invoice No
    S_TRAID FOR LIKP-ZTRUCKNO,           " Truck No
    S_MATNR FOR EKPO-MATNR,              " Material Code
    S_LGORT FOR EKPO-LGORT,              " Storage Location
    S_AEDAT FOR EKKO-AEDAT,              " PO Data
    S_GRDAT FOR MKPF-CPUDT,              " GR Data
    S_IRDAT FOR RBKP-BUDAT,              " IR Data
    S_EBELN FOR EKKO-EBELN,              " PO No
    S_VBELN FOR EKES-VBELN,              " ID No
    S_MBLNR FOR MKPF-MBLNR,              " Mat Doc(WE) No
    S_BELNR FOR RBKP-BELNR,              " Mat Doc(RE) No
    S_FIWE  FOR BKPF-BELNR,              " FI Doc(WE) No
    S_FIRE  FOR BKPF-BELNR.              " FI Doc(RE) No
* Text-s02: Display Record
  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02
    NO INTERVALS.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS RB_UPD RADIOBUTTON GROUP G1 DEFAULT 'X'.
*     Text-s03: Latest Only
      SELECTION-SCREEN COMMENT (20) TEXT-S03.
      PARAMETERS RB_ALL RADIOBUTTON GROUP G1 ##NEEDED.
*     Text-s04: All
      SELECTION-SCREEN COMMENT (20) TEXT-S04.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN END OF BLOCK B1.
PARAMETERS:
  P_VARI TYPE DISVARIANT-VARIANT.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANTS.
  PERFORM F_DEFAULT_SELECTION_SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F_LIST_VARIANT CHANGING P_VARI.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get data
  PERFORM F_GET_DATA CHANGING GT_RESULT.
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
    LC_BSART      TYPE  ZSDSDE_PARAM_NAME VALUE 'PO_TYPE',
    LC_INTF_LGORT TYPE  ZSDSDE_PARAM_NAME VALUE 'SLOC_INTERFACE'.

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
  CLEAR: GR_BSART,
         GR_INTF_LGORT.

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
*     PO Type
*     ------------------------------------
      WHEN LC_BSART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_BSART.

*     ------------------------------------
*     SLOC to generate interface file
*     ------------------------------------
      WHEN LC_INTF_LGORT.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_INTF_LGORT.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DEFAULT_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Default Selection Screen
*----------------------------------------------------------------------*
FORM F_DEFAULT_SELECTION_SCREEN .

  DATA:
    LS_VARIANT  TYPE  DISVARIANT.


* Default Variant Layout
  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = GC_SAVE_ALL
    CHANGING
      CS_VARIANT = LS_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    P_VARI = LS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN ##NEEDED.

* Validate Selection Screen here...

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
    RETURN.
  ENDIF.

* Assign Output
  CF_VARI = LS_VARIANT-VARIANT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_DATA
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LT_EKES TYPE  TT_EKES,
    LT_GR   TYPE  TT_GR,
    LT_IR   TYPE  TT_IR,
    LT_EKET TYPE  TT_EKET.


* Get PO Confirmation Data
  PERFORM F_GET_PO_CONFIRMATION CHANGING LT_EKES.
  IF LT_EKES IS INITIAL.
    RETURN.
  ENDIF.

* Get Goods Receive Data
  PERFORM F_GET_GR  USING  LT_EKES
                  CHANGING LT_GR.

* Get Invoice Receive Data
  PERFORM F_GET_IR  USING  LT_EKES
                  CHANGING LT_IR.

* Get PO Schedule Lines
  PERFORM F_GET_PO_SCHED  USING  LT_EKES
                        CHANGING LT_EKET.

* Collect Result
  PERFORM F_COLLECT_RESULT  USING  LT_EKES
                                   LT_GR
                                   LT_IR
                                   LT_EKET
                          CHANGING CT_RESULT.

* Create Interface File
  PERFORM F_CREATE_INTF_FILE  USING  CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_PO_confirmation
*----------------------------------------------------------------------*
*  Get PO Confirmation Data
*----------------------------------------------------------------------*
FORM F_GET_PO_CONFIRMATION  CHANGING CT_EKES TYPE TT_EKES.

* Initialize Output
  CLEAR: CT_EKES.

* Show progress
* Text-p01 : Reading PO Confirmation data . . .
  MC_SHOW_PROGRESS 25 TEXT-P01.

* Get Data from EKES
  SELECT A~EBELN AS EBELN,           " PO No
         B~EBELP AS EBELP,           " PO Item
         D~ETENS AS ETENS,
         D~EINDT AS EINDT_EST,       " Estimate ETD
         A~FRGZU AS FRGZU,           " PO Status
         A~ZZ1_PRJ_C_PO AS ZZPRJTYP, " Project Type
         A~AEDAT AS AEDAT,           " PO Created Data
         B~MATNR AS MATNR,           " Material Code
         A~WAERS AS WAERS,
         B~WERKS AS WERKS,
         B~LGORT AS LGORT,           " Storage Location
         F~LGOBE AS LGOBE,
         B~MEINS AS MEINS,           " PO Unit of Measure
         B~INSMK AS INSMK,           " Stock Type
         D~EBTYP AS EBTYPE,
         D~VBELN AS VBELN,
         D~VBELP AS VBELP,
         D~XBLNR AS XBLNR,
         E~ZTRUCKNO AS TRAID,
         E~ZWADAT_IST AS EINDT_PGI,
         E~ZWATIM_IST AS EINTM_PGI,
         B~NETPR AS NETPR,     " PO Price
         B~MENGE AS MENGE_REQ, " Request Qty
         D~MENGE AS MENGE_DLV, " Delivery Qty
         D~DABMG AS DABMG      " Reduce Qty
    FROM EKKO AS A
           INNER JOIN EKES AS D
             ON  D~EBELN = A~EBELN
           INNER JOIN EKPO AS B
             ON  B~EBELN = D~EBELN
             AND B~EBELP = D~EBELP
           LEFT OUTER JOIN LIKP AS E
             ON  E~VBELN = D~VBELN
           LEFT OUTER JOIN T001L AS F                  "#EC CI_BUFFJOIN
             ON  F~WERKS = B~WERKS
             AND F~LGORT = B~LGORT

   WHERE A~LIFNR    IN @S_VENDR
     AND A~AEDAT    IN @S_AEDAT
     AND A~EBELN    IN @S_EBELN
     AND A~BSART    IN @GR_BSART
     AND D~EINDT    IN @S_EINDT
     AND D~EBTYP    IN @S_EBTYP
     AND D~XBLNR    IN @S_XBLNR
     AND D~VBELN    IN @S_VBELN
     AND B~MATNR    IN @S_MATNR
     AND B~LGORT    IN @S_LGORT
     AND B~LOEKZ    NE 'L'            " Item Delete Flag
     AND E~ZTRUCKNO IN @S_TRAID
    INTO TABLE @CT_EKES.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Latest Only
  IF RB_UPD EQ GC_TRUE.
*   Remove all which not in MD04
    LOOP AT CT_EKES ASSIGNING FIELD-SYMBOL(<L_EKES>).
      IF <L_EKES>-MENGE_DLV EQ <L_EKES>-DABMG.
        DELETE CT_EKES.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_GR
*----------------------------------------------------------------------*
*  Get GR Data
*----------------------------------------------------------------------*
FORM F_GET_GR  USING  UT_EKES  TYPE  TT_EKES
             CHANGING CT_GR    TYPE  TT_GR.

  DATA:
    LT_LIPS   TYPE  TT_LIPS,
    LT_EKPO   TYPE  TT_EKPO,
    LT_GR     TYPE  TT_GR,
    LT_TMP    TYPE  TT_GR,
    LT_MATDOC TYPE  TT_MATDOC.


* Initialize Output
  CLEAR: CT_GR.

* Show progress
* Text-p02 : Reading GR data . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

* -------------------------
* Inbound Delivery
* -------------------------
* Collect Inbound Delivery List
  LT_LIPS = CORRESPONDING TT_LIPS( UT_EKES DISCARDING DUPLICATES
                                     MAPPING VBELN = VBELN
                                             VBELP = VBELP ).
  DELETE LT_LIPS WHERE VBELN IS INITIAL
                   AND VBELP IS INITIAL.

* Get GR for Inbound Delivery
  SELECT X~VBELN,
         X~VBELP,
         C~CPUDT AS ERDAT,
         C~CPUTM AS ERZET,
         B~MBLNR,
         B~MJAHR,
         B~ZEILE,
         B~WAERS,
         B~DMBTR,
         B~MEINS,
         B~MENGE,
         B~EBELN,
         B~EBELP,
         C~CPUDT,
         C~CPUTM
    FROM @LT_LIPS AS X
           INNER JOIN MSEG AS B
             ON  B~VBELN_IM = X~VBELN
             AND B~VBELP_IM = X~VBELP
           INNER JOIN MKPF AS C
             ON  C~MBLNR = B~MBLNR
             AND C~MJAHR = B~MJAHR
    ORDER BY X~VBELN ASCENDING,
             X~VBELP ASCENDING,
             C~CPUDT DESCENDING,
             C~CPUTM DESCENDING
    INTO TABLE @DATA(LT_VBFA).
  IF SY-SUBRC NE 0.
    CLEAR LT_VBFA.
  ENDIF.
* Only last GR of Inbound item
  DELETE ADJACENT DUPLICATES FROM LT_VBFA COMPARING VBELN
                                                    VBELP.
* Collect Result for Inbound Delivery
  LT_GR = CORRESPONDING TT_GR( LT_VBFA ).
  FREE LT_VBFA.

* -------------------------
* PO Item
* -------------------------
* Collect PO Item List
  LT_EKPO = CORRESPONDING TT_EKPO( UT_EKES DISCARDING DUPLICATES
                                     MAPPING EBELN = EBELN
                                             EBELP = EBELP ).

* Get GR for PO Item
  SELECT A~EBELN,
         A~EBELP,
         A~MBLNR,
         A~MJAHR,
         A~ZEILE,
         A~WAERS,
         A~DMBTR,
         A~MEINS,
         A~MENGE,
         B~CPUDT,
         B~CPUTM
    FROM @LT_EKPO AS X
           INNER JOIN MSEG AS A
             ON  A~EBELN = X~EBELN
             AND A~EBELP = X~EBELP
           INNER JOIN MKPF AS B
             ON  B~MBLNR = A~MBLNR
             AND B~MJAHR = A~MJAHR
             AND B~TCODE2 = 'MIGO_GR'
             AND B~LE_VBELN = @SPACE
    ORDER BY A~EBELN ASCENDING,
             A~EBELP ASCENDING,
             B~CPUDT DESCENDING,
             B~CPUTM DESCENDING
    INTO TABLE @DATA(LT_MSEG).
  IF SY-SUBRC NE 0.
    CLEAR LT_MSEG.
  ENDIF.
* Only last GR of PO item
  DELETE ADJACENT DUPLICATES FROM LT_MSEG COMPARING EBELN
                                                    EBELP.
  LT_TMP = CORRESPONDING TT_GR( LT_MSEG ).
  FREE: LT_MSEG.
  INSERT LINES OF LT_TMP INTO TABLE LT_GR.
  FREE: LT_TMP.

* -------------------------
* FI Document Ref Matdoc
* -------------------------
  IF LT_GR IS INITIAL.
    RETURN.
  ENDIF.

* Collect MatDoc
  CLEAR LT_MATDOC.
  LT_MATDOC = CORRESPONDING TT_MATDOC( LT_GR DISCARDING DUPLICATES
                                         MAPPING MBLNR = MBLNR
                                                 MJAHR = MJAHR ).
* Assign AWKEY
  LOOP AT LT_MATDOC ASSIGNING FIELD-SYMBOL(<L_MATDOC>).
    <L_MATDOC>-AWKEY = <L_MATDOC>-MBLNR && <L_MATDOC>-MJAHR.
  ENDLOOP.

* Get FI Doc Ref GR
  SELECT X~MBLNR,
         X~MJAHR,
         A~BUKRS,
         A~BELNR,
         A~GJAHR
    FROM @LT_MATDOC AS X
           INNER JOIN BKPF AS A
             ON  A~AWTYP = 'MKPF'
             AND A~AWKEY = X~AWKEY
             AND A~BLART = 'WE'
   ORDER BY X~MBLNR ASCENDING,
            X~MJAHR ASCENDING
    INTO TABLE @DATA(LT_BKPF).
  IF SY-SUBRC NE 0.
    CLEAR LT_BKPF.
  ENDIF.

* Collect result
  IF LT_BKPF IS NOT INITIAL.
    LOOP AT LT_GR ASSIGNING FIELD-SYMBOL(<L_GR>).
      READ TABLE LT_BKPF ASSIGNING FIELD-SYMBOL(<L_BKPF>)
                         WITH KEY MBLNR = <L_GR>-MBLNR
                                  MJAHR = <L_GR>-MJAHR
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_GR>-BUKRS = <L_BKPF>-BUKRS.
        <L_GR>-BELNR = <L_BKPF>-BELNR.
        <L_GR>-GJAHR = <L_BKPF>-GJAHR.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Assign Result
  CT_GR = LT_GR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_IR
*----------------------------------------------------------------------*
*  Get Invoice receive data
*----------------------------------------------------------------------*
FORM F_GET_IR  USING  UT_EKES  TYPE  TT_EKES
             CHANGING CT_IR    TYPE  TT_IR.

  DATA:
    LT_XBLNR TYPE  TT_XBLNR,
    LT_IR    TYPE  TT_IR,
    LT_INV   TYPE  TT_INV.


* Initialize Output
  CLEAR: CT_IR.

* Show progress
* Text-p03 : Reading IR data . . .
  MC_SHOW_PROGRESS 60 TEXT-P03.

* Collect Key data
  LT_XBLNR = CORRESPONDING TT_XBLNR( UT_EKES DISCARDING DUPLICATES
                                       MAPPING EBELN = EBELN
                                               EBELP = EBELP
                                               XBLNR = XBLNR ).
  DELETE LT_XBLNR WHERE XBLNR IS INITIAL.               "#EC CI_SORTSEQ

* --------------------------------------------
* Get Invoice Receive Batch from Reference
* --------------------------------------------
  SELECT A~EBELN,
         A~EBELP,
         B~XBLNR,
         B~BELNR,
         B~GJAHR,
         A~RBLGP AS BUZEI,
         B~WAERS,
         A~WRBTR,
         A~MEINS AS BSTME,
         A~MENGE,
         B~RBSTAT,
         B~BUDAT,
         B~CPUDT,
         B~CPUTM
    FROM @LT_XBLNR AS X
           INNER JOIN RBDRSEG AS A
             ON  A~EBELN = X~EBELN
             AND A~EBELP = X~EBELP
           INNER JOIN RBKP AS B
             ON  B~BELNR = A~RBLNR
             AND B~GJAHR = A~RJAHR
             AND B~XBLNR = X~XBLNR
             AND B~STBLG = @SPACE
   WHERE B~RBSTAT NE '2'
   ORDER BY A~EBELN ASCENDING,
            A~EBELP ASCENDING,
            B~XBLNR ASCENDING,
            B~CPUDT DESCENDING,
            B~CPUTM DESCENDING
    INTO TABLE @DATA(LT_RSEG1).
  IF SY-SUBRC NE 0.
    CLEAR LT_RSEG1.
  ENDIF.
* Only last IR of Key
  DELETE ADJACENT DUPLICATES FROM LT_RSEG1 COMPARING EBELN
                                                     EBELP
                                                     XBLNR.
  LT_IR = CORRESPONDING TT_IR( LT_RSEG1 ).
  FREE LT_RSEG1.

* --------------------------------------------
* Get Invoice Receive from Reference
* --------------------------------------------
  SELECT A~EBELN,
         A~EBELP,
         B~XBLNR,
         B~BELNR,
         B~GJAHR,
         A~BUZEI,
         B~WAERS,
         A~WRBTR,
         A~BSTME,
         A~MENGE,
         B~RBSTAT,
         B~BUDAT,
         B~CPUDT,
         B~CPUTM
    FROM @LT_XBLNR AS X
           INNER JOIN RSEG AS A
             ON  A~EBELN = X~EBELN
             AND A~EBELP = X~EBELP
           INNER JOIN RBKP AS B
             ON  B~BELNR = A~BELNR
             AND B~GJAHR = A~GJAHR
             AND B~XBLNR = X~XBLNR
             AND B~STBLG = @SPACE
   WHERE B~RBSTAT NE '2'
   ORDER BY A~EBELN ASCENDING,
            A~EBELP ASCENDING,
            B~XBLNR ASCENDING,
            B~CPUDT DESCENDING,
            B~CPUTM DESCENDING
    INTO TABLE @DATA(LT_RSEG2).
  IF SY-SUBRC NE 0.
    CLEAR LT_RSEG2.
  ENDIF.
* Only last IR of Key
  DELETE ADJACENT DUPLICATES FROM LT_RSEG2 COMPARING EBELN
                                                     EBELP
                                                     XBLNR.
  LOOP AT LT_RSEG2 ASSIGNING FIELD-SYMBOL(<L_RSEG2>).
    READ TABLE LT_IR TRANSPORTING NO FIELDS
                     WITH KEY EBELN = <L_RSEG2>-EBELN
                              EBELP = <L_RSEG2>-EBELP
                              XBLNR = <L_RSEG2>-XBLNR
                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
*     Already found, ignore it
      CONTINUE.
    ENDIF.
    INSERT CORRESPONDING TS_IR( <L_RSEG2> ) INTO TABLE LT_IR.
  ENDLOOP.
  FREE LT_RSEG2.

* -------------------------
* FI Document Ref Matdoc
* -------------------------
  IF LT_IR IS INITIAL.
    RETURN.
  ENDIF.

* Collect Invoice
  CLEAR LT_INV.
  LT_INV = CORRESPONDING TT_INV( LT_IR DISCARDING DUPLICATES
                                         MAPPING BELNR = BELNR
                                                 GJAHR = GJAHR ).
* Assign AWKEY
  LOOP AT LT_INV ASSIGNING FIELD-SYMBOL(<L_INV>).
    <L_INV>-AWKEY = <L_INV>-BELNR && <L_INV>-GJAHR.
  ENDLOOP.

* Get FI Doc Ref GR
  SELECT X~BELNR,
         X~GJAHR,
         A~BUKRS AS FI_BUKRS,
         A~BELNR AS FI_BELNR,
         A~GJAHR AS FI_GJAHR
    FROM @LT_INV AS X
           INNER JOIN BKPF AS A
             ON  A~AWTYP = 'RMRP'
             AND A~AWKEY = X~AWKEY
             AND A~BLART = 'RE'
   ORDER BY X~BELNR ASCENDING,
            X~GJAHR ASCENDING
    INTO TABLE @DATA(LT_BKPF).
  IF SY-SUBRC NE 0.
    CLEAR LT_BKPF.
  ENDIF.

* Collect result
  IF LT_BKPF IS NOT INITIAL.
    LOOP AT LT_IR ASSIGNING FIELD-SYMBOL(<L_IR>).
      READ TABLE LT_BKPF ASSIGNING FIELD-SYMBOL(<L_BKPF>)
                         WITH KEY BELNR = <L_IR>-BELNR
                                  GJAHR = <L_IR>-GJAHR
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_IR>-FI_BUKRS = <L_BKPF>-FI_BUKRS.
        <L_IR>-FI_BELNR = <L_BKPF>-FI_BELNR.
        <L_IR>-FI_GJAHR = <L_BKPF>-FI_GJAHR.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Assign Result
  CT_IR = LT_IR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_PO_SCHED
*----------------------------------------------------------------------*
*  Get PO Schedule Lines
*----------------------------------------------------------------------*
FORM F_GET_PO_SCHED  USING  UT_EKES  TYPE TT_EKES
                   CHANGING CT_EKET  TYPE TT_EKET.

  DATA:
    LT_EKPO TYPE  TT_EKPO,
    LT_EKES TYPE  TT_EKES2.

  DATA:
    LS_EKET  TYPE  TS_EKET.

  DATA:
    LF_MENGE   TYPE  EKES-MENGE,
    LF_EBTYP   TYPE  EKES-EBTYP,
    LF_EBTYP_O TYPE  EKES-EBTYP.


* Initialize Output
  CLEAR: CT_EKET.

* Show progress
* Text-p04 : Reading PO Schedule line data . . .
  MC_SHOW_PROGRESS 80 TEXT-P04.

* -------------------------
* PO Item
* -------------------------
* Collect PO Item List
  LT_EKPO = CORRESPONDING TT_EKPO( UT_EKES DISCARDING DUPLICATES
                                     MAPPING EBELN = EBELN
                                             EBELP = EBELP ).

* Get PO Schedule Lines
  SELECT A~EBELN,
         A~EBELP,
         A~ETENR,
         A~EINDT,
         A~MENGE
    FROM @LT_EKPO AS X
           INNER JOIN EKET AS A
             ON  A~EBELN = X~EBELN
             AND A~EBELP = X~EBELP
    INTO TABLE @DATA(LT_EKET).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get Confirmation
  SELECT A~EBELN,
         A~EBELP,
         A~EBTYP,
         A~EINDT,
         SUM( A~MENGE ) AS MENGE
    FROM @LT_EKPO AS X
           INNER JOIN EKES AS A
             ON  A~EBELN = X~EBELN
             AND A~EBELP = X~EBELP
             AND A~LOEKZ = @SPACE
   GROUP BY A~EBELN,
            A~EBELP,
            A~EBTYP,
            A~EINDT
    INTO TABLE @LT_EKES ##TOO_MANY_ITAB_FIELDS.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Collect Result
  LOOP AT LT_EKET ASSIGNING FIELD-SYMBOL(<L_EKET>).

    CLEAR: LF_EBTYP,
           LF_EBTYP_O.
    LOOP AT LT_EKES ASSIGNING FIELD-SYMBOL(<L_EKES>)
                    WHERE EBELN = <L_EKET>-EBELN
                      AND EBELP = <L_EKET>-EBELP.
      LF_EBTYP = <L_EKES>-EBTYP.
*     On Change of EBTYP
      IF LF_EBTYP NE LF_EBTYP_O.
*       Assign Request Qty
        LF_MENGE = <L_EKET>-MENGE.
        LF_EBTYP_O = LF_EBTYP.
      ENDIF.

      CHECK LF_MENGE GT 0.
      CHECK <L_EKES>-MENGE GT <L_EKES>-MENGE_REQ.

      CLEAR LS_EKET.
      LS_EKET-EBELN     = <L_EKES>-EBELN.
      LS_EKET-EBELP     = <L_EKES>-EBELP.
      LS_EKET-EBTYP     = <L_EKES>-EBTYP.
      LS_EKET-EINDT_EST = <L_EKES>-EINDT.
      LS_EKET-EINDT_REQ = <L_EKET>-EINDT.

      IF LF_MENGE LE <L_EKES>-MENGE.
        LS_EKET-MENGE     = LF_MENGE.
      ELSE.
        LS_EKET-MENGE     = <L_EKES>-MENGE.
      ENDIF.

      INSERT LS_EKET INTO TABLE CT_EKET.

      <L_EKES>-MENGE_REQ = <L_EKES>-MENGE_REQ + LS_EKET-MENGE.
      LF_MENGE = LF_MENGE - LS_EKET-MENGE.

    ENDLOOP.

*   If Requirement left (LF_MENGE for each EKES Group), processing here...

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT
*----------------------------------------------------------------------*
*  Collect Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  UT_EKES  TYPE  TT_EKES
                              UT_GR    TYPE  TT_GR
                              UT_IR    TYPE  TT_IR
                              UT_EKET  TYPE  TT_EKET
                     CHANGING CT_RESULT TYPE  TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.

  DATA:
    LF_MENGE TYPE  RSEG-MENGE,
    LF_WRBTR TYPE  RSEG-WRBTR.


* Initialize Output
  CLEAR: CT_RESULT.

* Show progress
* Text-p05 : Collecting Result . . .
  MC_SHOW_PROGRESS 90 TEXT-P05.

  LOOP AT UT_EKES ASSIGNING FIELD-SYMBOL(<L_EKES>).

    CLEAR LS_RESULT.
    LS_RESULT-EBELN     = <L_EKES>-EBELN.
    LS_RESULT-EBELP     = <L_EKES>-EBELP.
    LS_RESULT-AEDAT     = <L_EKES>-AEDAT.
    LS_RESULT-MATNR     = <L_EKES>-MATNR.
    LS_RESULT-MEINS     = <L_EKES>-MEINS.
    LS_RESULT-MENGE_REQ = <L_EKES>-MENGE_REQ.
    LS_RESULT-WAERS     = <L_EKES>-WAERS.
    LS_RESULT-NETPR     = <L_EKES>-NETPR.
    LS_RESULT-LGORT     = <L_EKES>-LGOBE.
    LS_RESULT-EINDT_EST = <L_EKES>-EINDT_EST.
    LS_RESULT-EINDT_PGI = <L_EKES>-EINDT_PGI.
    LS_RESULT-EINTM_PGI = <L_EKES>-EINTM_PGI.
    LS_RESULT-XBLNR     = <L_EKES>-XBLNR.
    LS_RESULT-TRAID     = <L_EKES>-TRAID.
    LS_RESULT-EBTYP     = <L_EKES>-EBTYP.
    LS_RESULT-VBELN_ID  = <L_EKES>-VBELN.
    LS_RESULT-VBELP_ID  = <L_EKES>-VBELP.
    LS_RESULT-MENGE_DLV = <L_EKES>-MENGE_DLV.
    LS_RESULT-DABMG     = <L_EKES>-DABMG.
    LS_RESULT-ZZPRJTYP  = <L_EKES>-ZZPRJTYP.

    LS_RESULT-EKPO_WERKS = <L_EKES>-WERKS.
    LS_RESULT-EKPO_LGORT = <L_EKES>-LGORT.

*   PO Status X -> Released
    IF <L_EKES>-FRGZU = 'X'.
*     Text-a01: Released
      LS_RESULT-FRGZU = TEXT-A01.
    ENDIF.

*   Stock Type
    CASE <L_EKES>-INSMK.
      WHEN ' '.
*       Text-a02: Unrestricted use
        LS_RESULT-INSMK = TEXT-A02.
      WHEN 'X'.
*       Text-a03: Quality inspection
        LS_RESULT-INSMK = TEXT-A03.
      WHEN 'S'.
*       Text-a04: Blocked stock
        LS_RESULT-INSMK = TEXT-A04.
    ENDCASE.

    LS_RESULT-NETWR = LS_RESULT-MENGE_DLV * LS_RESULT-NETPR.

*   Only for LA
    IF <L_EKES>-EBTYP EQ GC_LA.

*     Read GR By Inbound Delivery
      READ TABLE UT_GR ASSIGNING FIELD-SYMBOL(<L_GR>)
                       WITH KEY EBELN = <L_EKES>-EBELN
                                EBELP = <L_EKES>-EBELP
                                VBELN = <L_EKES>-VBELN
                                VBELP = <L_EKES>-VBELP
                       BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_RESULT-MBLNR    = <L_GR>-MBLNR.
        LS_RESULT-MJAHR    = <L_GR>-MJAHR.
        LS_RESULT-ZEILE    = <L_GR>-ZEILE.
        LS_RESULT-CPUDT_WE = <L_GR>-CPUDT.
        LS_RESULT-CPUTM    = <L_GR>-CPUTM.
        LS_RESULT-FI_WE    = <L_GR>-BELNR.
        LS_RESULT-BUKRS    = <L_GR>-BUKRS.
        LS_RESULT-GJAHR_WE = <L_GR>-GJAHR.

*       Convert Currency & UOM here....
        LF_MENGE = <L_GR>-MENGE.
        LF_WRBTR = <L_GR>-DMBTR.

        IF LF_MENGE IS NOT INITIAL.
          LS_RESULT-INVPR = LF_WRBTR / LF_MENGE.
          LS_RESULT-WRBTR = ( LF_WRBTR * LS_RESULT-MENGE_DLV ) / LF_MENGE.
        ENDIF.

      ENDIF.

      IF <L_EKES>-XBLNR IS NOT INITIAL.

*       Read IR
        READ TABLE UT_IR ASSIGNING FIELD-SYMBOL(<L_IR>)
                         WITH KEY EBELN = <L_EKES>-EBELN
                                  EBELP = <L_EKES>-EBELP
                                  XBLNR = <L_EKES>-XBLNR
                         BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LS_RESULT-BELNR    = <L_IR>-BELNR.
          LS_RESULT-GJAHR_IR = <L_IR>-GJAHR.
          LS_RESULT-BUZEI    = <L_IR>-BUZEI.
          LS_RESULT-BUDAT_RE = <L_IR>-BUDAT.
          LS_RESULT-FI_RE    = <L_IR>-FI_BELNR.
          LS_RESULT-BUKRS    = <L_IR>-FI_BUKRS.
          LS_RESULT-GJAHR_RE = <L_IR>-FI_GJAHR.

*         Convert Currency & UOM here....
          LF_MENGE = <L_IR>-MENGE.
          LF_WRBTR = <L_IR>-WRBTR.

          IF LF_MENGE IS NOT INITIAL.
            LS_RESULT-INVPR = LF_WRBTR / LF_MENGE.
            LS_RESULT-WRBTR = ( LF_WRBTR * LS_RESULT-MENGE_DLV ) / LF_MENGE.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

*   Additional Filter
    CHECK LS_RESULT-CPUDT_WE IN S_GRDAT.
    CHECK LS_RESULT-BUDAT_RE IN S_IRDAT.
    CHECK LS_RESULT-MBLNR    IN S_MBLNR.
    CHECK LS_RESULT-BELNR    IN S_BELNR.
    CHECK LS_RESULT-FI_WE    IN S_FIWE.
    CHECK LS_RESULT-FI_RE    IN S_FIRE.

*   Get Request Date
    READ TABLE UT_EKET ASSIGNING FIELD-SYMBOL(<L_EKET>)
                       WITH KEY EBELN = LS_RESULT-EBELN
                                EBELP = LS_RESULT-EBELP
                                EBTYP = LS_RESULT-EBTYP
                                EINDT_EST = LS_RESULT-EINDT_EST
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      READ TABLE UT_EKET ASSIGNING <L_EKET>
                         WITH KEY EBELN = LS_RESULT-EBELN
                                  EBELP = LS_RESULT-EBELP
                                  EBTYP = LS_RESULT-EBTYP
                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
        READ TABLE UT_EKET ASSIGNING <L_EKET>
                           WITH KEY EBELN = LS_RESULT-EBELN
                                    EBELP = LS_RESULT-EBELP
                           BINARY SEARCH.
      ENDIF.
    ENDIF.
    IF SY-SUBRC EQ 0.
      LS_RESULT-EINDT_REQ = <L_EKET>-EINDT_REQ.
    ENDIF.

    IF LS_RESULT-EINDT_REQ < LS_RESULT-EINDT_EST.
      LS_RESULT-F_ETD = GC_TRUE.
    ENDIF.

    IF LS_RESULT-INVPR IS NOT INITIAL AND
     ( LS_RESULT-NETPR NE LS_RESULT-INVPR OR
       LS_RESULT-NETWR NE LS_RESULT-WRBTR ).
      LS_RESULT-F_PRICE = GC_TRUE.
    ENDIF.

*   Not yet GR But IR, Check Short Stock?
    IF  LS_RESULT-VBELN_ID IS NOT INITIAL AND
        LS_RESULT-BELNR IS NOT INITIAL AND
        LS_RESULT-MBLNR IS INITIAL.
      PERFORM F_CHECK_SHORT_STOCK  USING  LS_RESULT-MATNR
                                          <L_EKES>-WERKS
                                 CHANGING LS_RESULT-SHRTSTK.
    ENDIF.

*   Assign Line Color
    PERFORM F_ASSIGN_LINECOLOR  USING  LS_RESULT
                              CHANGING LS_RESULT-LINECOLOR.

    INSERT LS_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHECK_SHORT_STOCK
*----------------------------------------------------------------------*
*  Check Stock Shortage?
*----------------------------------------------------------------------*
FORM F_CHECK_SHORT_STOCK  USING  UF_MATNR  TYPE  EKPO-MATNR
                                 UF_WERKS  TYPE  EKPO-WERKS
                        CHANGING CF_SHRTSTK TYPE  TS_RESULT-SHRTSTK.

  DATA:
    LS_STOCK TYPE BAPI_MRP_STOCK_DETAIL.

  DATA:
    LF_CONF  TYPE  VBBE-VMENG.


* Initialize Output
  CLEAR: CF_SHRTSTK.

* Get SO Requirements Qty
  SELECT SUM( VMENG )
    FROM VBBE
    INTO @LF_CONF
   WHERE MATNR  EQ @UF_MATNR
     AND MBDAT  LE @SY-DATUM
     AND WERKS  EQ @UF_WERKS.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get SDS Available Stock
  CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
    EXPORTING
      MATERIAL_LONG    = UF_MATNR        " Material Code
      PLANT            = UF_WERKS        " Plant
    IMPORTING
      MRP_STOCK_DETAIL = LS_STOCK.      " Stock Detail

  IF ( LS_STOCK-UNRESTRICTED_STCK - LF_CONF ) < 0.
    CF_SHRTSTK = GC_TRUE.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_linecolor
*----------------------------------------------------------------------*
*  Assign Line Color
*----------------------------------------------------------------------*
FORM F_ASSIGN_LINECOLOR  USING  US_RESULT  TYPE  TS_RESULT
                       CHANGING CF_LINECOLOR TYPE TS_RESULT-LINECOLOR.

  CONSTANTS:
    LC_YELLOW  TYPE  TS_RESULT-LINECOLOR VALUE 'C300',
    LC_YELLOW2 TYPE  TS_RESULT-LINECOLOR VALUE 'C310',
    LC_RED     TYPE  TS_RESULT-LINECOLOR VALUE 'C600',
    LC_GREEN   TYPE  TS_RESULT-LINECOLOR VALUE 'C500',
    LC_BLUE    TYPE  TS_RESULT-LINECOLOR VALUE 'C410'.

* Initialize Output
  CLEAR: CF_LINECOLOR.

  IF US_RESULT-F_ETD EQ GC_TRUE.
    CF_LINECOLOR = LC_YELLOW.
  ENDIF.

  IF US_RESULT-F_PRICE EQ GC_TRUE.
    CF_LINECOLOR = LC_RED.
  ELSEIF US_RESULT-MBLNR IS NOT INITIAL AND
         US_RESULT-BELNR IS INITIAL.
    CF_LINECOLOR = LC_GREEN.
  ELSEIF US_RESULT-MBLNR IS INITIAL AND
         US_RESULT-FI_RE IS NOT INITIAL.
    CF_LINECOLOR = LC_BLUE.
  ENDIF.

  IF US_RESULT-SHRTSTK EQ GC_TRUE.
    CF_LINECOLOR = LC_YELLOW2.
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
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_VARIANT-VARIANT = P_VARI.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

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
      WHEN 'F_PRICE'.
*       Text-c01 : Price
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'F_ETD'.
*       Text-c02 : ETD
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'FRGZU'.
*       Text-c03 : PO Status
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EBELN'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       Text-c04 : PO No
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EBELP'.
*       Text-c05 : PO Item No
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'AEDAT'.
*       Text-c06 : PO Date
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MATNR'.
*       Text-c07 : Material Code
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'INSMK'.
*       Text-c08 : Stock Type
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MEINS'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MENGE_REQ'.
*       Text-c09 : Request Qty
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WAERS'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'NETPR'.
*       Text-c10 : PO Price
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'NETWR'.
*       Text-c11 : PO Amount
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'LGORT'.
*       Text-c12 : Storage Location
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EINDT_REQ'.
*       Text-c13 : Request ETD
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EBTYP'.
*       Text-c14 : Delivery Status
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EINDT_EST'.
*       Text-c15 : Estimate ETD
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EINDT_PGI'.
*       Text-c16 : PGI Date
        LF_TEXT                = TEXT-C16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EINTM_PGI'.
*       Text-c17 : PGI Time
        LF_TEXT                = TEXT-C17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'XBLNR'.
*       Text-c18 : TAX Invoice No
        LF_TEXT                = TEXT-C18.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'TRAID'.
*       Text-c19 : Truck No
        LF_TEXT                = TEXT-C19.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'VBELN_ID'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       Text-c20 : ID No
        LF_TEXT                = TEXT-C20.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'VBELP_ID'.
*       Text-c21 : ID Item No
        LF_TEXT                = TEXT-C21.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MENGE_DLV'.
*       Text-c22 : Delivery Qty
        LF_TEXT                = TEXT-C22.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'DABMG'.
*       Text-c23 : Reduced Qty
        LF_TEXT                = TEXT-C23.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'INVPR'.
*       Text-c24 : INV Price
        LF_TEXT                = TEXT-C24.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WRBTR'.
*       Text-c25 : INV Amount
        LF_TEXT                = TEXT-C25.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MBLNR'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       Text-c26 : Mat Doc(WE)
        LF_TEXT                = TEXT-C26.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ZEILE'.
*       Text-c27 : Mat Doc(WE) Item
        LF_TEXT                = TEXT-C27.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CPUDT_WE'.
*       Text-c28 : GR Date
        LF_TEXT                = TEXT-C28.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CPUTM'.
*       Text-c29 : GR Time
        LF_TEXT                = TEXT-C29.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'FI_WE'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       Text-c30 : FI Doc(WE)
        LF_TEXT                = TEXT-C30.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BELNR'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       Text-c31 : Mat Doc(RE)
        LF_TEXT                = TEXT-C31.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BUZEI'.
*       Text-c32 : Mat Doc(RE) Item
        LF_TEXT                = TEXT-C32.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BUDAT_RE'.
*       Text-c33 : IR Date
        LF_TEXT                = TEXT-C33.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'FI_RE'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
*       Text-c34 : FI Doc(RE)
        LF_TEXT                = TEXT-C34.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ZZPRJTYP'.
*       Text-c35 : Project Type
        LF_TEXT                = TEXT-C35.
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
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UREF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  DATA:
    LS_BUTTON TYPE STB_BUTTON.


* Add Refresh Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = '&REF'.
  LS_BUTTON-ICON     = '@42@'.
* Text-a05: Refresh
  LS_BUTTON-QUICKINFO = TEXT-A05.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  CASE UF_UCOMM.
    WHEN '&REF'.
*     Get data
      PERFORM F_GET_DATA CHANGING GT_RESULT.
*     Force PBO Process
      SUPPRESS DIALOG.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_1
*----------------------------------------------------------------------*
*  ALV Hotspot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                             US_COLUMN_ID TYPE LVC_S_COL.

* Read Line
  READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'EBELN'.
      PERFORM F_DISPLAY_PO  USING  <L_RESULT>-EBELN
                                   <L_RESULT>-EBELP.
    WHEN 'MATNR'.
      PERFORM F_DISPLAY_MATERIAL  USING  <L_RESULT>-MATNR
                                         <L_RESULT>-EKPO_WERKS
                                         <L_RESULT>-EKPO_LGORT.
    WHEN 'VBELN_ID'.
      IF <L_RESULT>-VBELN_ID IS INITIAL.
        RETURN.
      ENDIF.
      IF <L_RESULT>-F_PRICE EQ GC_TRUE.
*       Text-e01: PO item no:
*       Text-e02: , Price is not corresponding to INV Price.
        MESSAGE S000(38) DISPLAY LIKE 'E'
                         WITH TEXT-E01
                              <L_RESULT>-EBELP
                              TEXT-E02
                              SPACE.
        RETURN.
      ELSEIF <L_RESULT>-BELNR IS INITIAL.
*       Text-e03: Mat Doc(RE) is not yet created.
        MESSAGE S000(38) DISPLAY LIKE 'E'
                         WITH TEXT-E03 SPACE SPACE SPACE.
        RETURN.
      ENDIF.
      PERFORM F_DISPLAY_INB_DELV  USING  <L_RESULT>-VBELN_ID.

    WHEN 'MBLNR'.
      PERFORM F_DISPLAY_MATDOC  USING  <L_RESULT>-MBLNR
                                       <L_RESULT>-MJAHR.


    WHEN 'FI_WE'.
      PERFORM F_DISPLAY_FIDOC  USING  <L_RESULT>-BUKRS
                                      <L_RESULT>-FI_WE
                                      <L_RESULT>-GJAHR_WE.

    WHEN 'BELNR'.
      IF <L_RESULT>-BELNR IS NOT INITIAL AND
         <L_RESULT>-MBLNR IS INITIAL.
*       Text-e04: Post Goods Recipt is not yet completed.
        MESSAGE S000(38) DISPLAY LIKE 'E'
                         WITH TEXT-E04 SPACE SPACE SPACE.
        RETURN.
      ENDIF.

      PERFORM F_DISPLAY_INVOICE  USING  <L_RESULT>-BELNR
                                        <L_RESULT>-GJAHR_IR.

    WHEN 'FI_RE'.
      PERFORM F_DISPLAY_FIDOC  USING  <L_RESULT>-BUKRS
                                      <L_RESULT>-FI_RE
                                      <L_RESULT>-GJAHR_RE.

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_display_po
*----------------------------------------------------------------------*
*  Display PO
*----------------------------------------------------------------------*
FORM F_DISPLAY_PO  USING UF_EBELN TYPE EKPO-EBELN
                         UF_EBELP TYPE EKPO-EBELP.

  IF UF_EBELN IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'BES' FIELD UF_EBELN.
  SET PARAMETER ID 'BSP' FIELD UF_EBELP.
  CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.       "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_MATERIAL
*----------------------------------------------------------------------*
*  Display Material
*----------------------------------------------------------------------*
FORM F_DISPLAY_MATERIAL  USING  UF_MATNR  TYPE  MARA-MATNR
                                UF_WERKS  TYPE  MARC-WERKS
                                UF_LGORT  TYPE  MARD-LGORT.

  IF UF_MATNR IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'MAT' FIELD UF_MATNR.
  SET PARAMETER ID 'WRK' FIELD UF_WERKS.
  SET PARAMETER ID 'LAG' FIELD UF_LGORT.
  SET PARAMETER ID 'MXX' FIELD 'L' .
  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN .        "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_INB_DELV
*----------------------------------------------------------------------*
*  Display Inbound Delivery
*----------------------------------------------------------------------*
FORM F_DISPLAY_INB_DELV  USING UF_VBELN TYPE LIKP-VBELN.

  IF UF_VBELN IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'VLM' FIELD UF_VBELN.
  CALL TRANSACTION 'VL33N' AND SKIP  FIRST SCREEN.       "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_MATDOC
*----------------------------------------------------------------------*
*  Display Material Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_MATDOC  USING  UF_MBLNR TYPE  MKPF-MBLNR
                              UF_MJAHR TYPE  MKPF-MJAHR.

  IF UF_MBLNR IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'MIGO_DIALOG'
    EXPORTING
      I_ACTION            = 'A04'
      I_REFDOC            = 'R02'
      I_NOTREE            = 'X'
*     I_NO_AUTH_CHECK     =
      I_SKIP_FIRST_SCREEN = 'X'
      I_DEADEND           = 'X'
      I_OKCODE            = 'OK_GO'
      I_MBLNR             = UF_MBLNR
      I_MJAHR             = UF_MJAHR
    EXCEPTIONS
      ILLEGAL_COMBINATION = 1
      OTHERS              = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_FIDOC
*----------------------------------------------------------------------*
*  Display FI Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_FIDOC  USING  UF_BUKRS  TYPE  BKPF-BUKRS
                             UF_BELNR  TYPE  BKPF-BELNR
                             UF_GJAHR  TYPE  BKPF-GJAHR.

  IF UF_BELNR IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD UF_BELNR.
  SET PARAMETER ID 'BUK' FIELD UF_BUKRS.
  SET PARAMETER ID 'GJR' FIELD UF_GJAHR.
  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.        "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_INVOICE
*----------------------------------------------------------------------*
*  Display Invoice
*----------------------------------------------------------------------*
FORM F_DISPLAY_INVOICE  USING  UF_BELNR TYPE RBKP-BELNR
                               UF_GJAHR TYPE RBKP-GJAHR.

  IF UF_BELNR IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'RBN' FIELD UF_BELNR.
  SET PARAMETER ID 'GJR' FIELD UF_GJAHR.
  CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.        "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_INTF_FILE
*----------------------------------------------------------------------*
*  Create Interface File
*----------------------------------------------------------------------*
FORM F_CREATE_INTF_FILE  USING  UT_RESULT TYPE TT_RESULT.

  DATA:
    LT_KEY      TYPE  TT_XBLNR,
    LT_INV      TYPE  STANDARD TABLE OF ZSDSMMT007,
    LT_INTF_TXT TYPE  ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  DATA:
    LS_INTF_TXT TYPE  TS_INTF_TXT,
    LS_RETURN   TYPE  BAPIRET2.

  DATA:
    LF_DATUM      TYPE  SY-DATUM,
    LF_UZEIT      TYPE  SY-UZEIT,
    LF_FILENAME_I TYPE  STRING,
    LF_FILEPATH_O TYPE  STRING   ##NEEDED,
    LF_FILENAME_O TYPE  STRING   ##NEEDED.


* Assign Key
  LT_KEY = CORRESPONDING TT_XBLNR( UT_RESULT DISCARDING DUPLICATES ).
  DELETE LT_KEY WHERE XBLNR IS INITIAL.                 "#EC CI_SORTSEQ

* Get List Invoice already interfaced
  SELECT DISTINCT A~ZINV_DIT,
                  A~EBELN,
                  A~EBELP
    FROM @LT_KEY AS X
           INNER JOIN ZSDSMMT007 AS A
             ON  A~ZINV_DIT = X~XBLNR
             AND A~EBELN    = X~EBELN
             AND A~EBELP    = X~EBELP
   ORDER BY A~ZINV_DIT ASCENDING,
            A~EBELN    ASCENDING,
            A~EBELP    ASCENDING
    INTO TABLE @DATA(LT_INV_OF_DIT).
  IF SY-SUBRC NE 0.
    CLEAR LT_INV_OF_DIT.
  ENDIF.
  FREE: LT_KEY.

* Processing Log
  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.

  CLEAR: LT_INV,
         LT_INTF_TXT.
  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE EKPO_LGORT IN GR_INTF_LGORT
                      AND XBLNR IS NOT INITIAL.

    READ TABLE LT_INV_OF_DIT TRANSPORTING NO FIELDS
                             WITH KEY ZINV_DIT = <L_RESULT>-XBLNR
                                      EBELN    = <L_RESULT>-EBELN
                                      EBELP    = <L_RESULT>-EBELP
                             BINARY SEARCH.
    IF SY-SUBRC EQ 0.
*     The entries was interfaced
      CONTINUE.
    ENDIF.

*   Log Entries
    APPEND VALUE  ZSDSMMT007( ZINV_DIT = <L_RESULT>-XBLNR
                              EBELN    = <L_RESULT>-EBELN
                              EBELP    = <L_RESULT>-EBELP
                              VBELN    = <L_RESULT>-VBELN_ID
                              MATNR    = <L_RESULT>-MATNR
                              MENGE    = <L_RESULT>-MENGE_DLV
                              LGORT    = <L_RESULT>-EKPO_LGORT
                              EINDT    = <L_RESULT>-EINDT_EST
                              ERDAT    = LF_DATUM
                              ERZET    = LF_UZEIT
                            ) TO LT_INV.

*   Interface File data
    CLEAR LS_INTF_TXT.
    LS_INTF_TXT-ZINV_DIT  = <L_RESULT>-XBLNR.
    LS_INTF_TXT-VBELN     = <L_RESULT>-VBELN_ID.
    LS_INTF_TXT-EBELN     = <L_RESULT>-EBELN.
    LS_INTF_TXT-EBELP     = <L_RESULT>-EBELP.
    LS_INTF_TXT-MATNR     = <L_RESULT>-MATNR.
    LS_INTF_TXT-MENGE     = <L_RESULT>-MENGE_DLV.
    LS_INTF_TXT-LGORT     = <L_RESULT>-EKPO_LGORT.
    LS_INTF_TXT-EINDT     = <L_RESULT>-EINDT_EST.
    APPEND LS_INTF_TXT TO LT_INTF_TXT.

  ENDLOOP.

* Create Interface file
  IF LT_INTF_TXT IS NOT INITIAL.
*   Assign Filename
    LF_FILENAME_I = 'SDSGR' && SY-DATUM && '_' && SY-TIMLO && '.txt' ##NO_TEXT.
    CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
      EXPORTING
        IF_INTFNO   = GC_INTFNO
        IF_FILENAME = LF_FILENAME_I
        IT_DATATXT  = LT_INTF_TXT
      IMPORTING
        ES_RETURN   = LS_RETURN
        EF_FILEPATH = LF_FILEPATH_O
        EF_FILENAME = LF_FILENAME_O.
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

  IF LT_INV IS NOT INITIAL.
*   Update Log entries to Table
    MODIFY ZSDSMMT007 FROM TABLE LT_INV.
    IF SY-SUBRC NE 0.
*     Text-e06: Error during updating table ZSDSMMT007.
      MESSAGE I000(38) DISPLAY LIKE 'E'
                       WITH TEXT-E06 SPACE SPACE SPACE.
      RETURN.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
