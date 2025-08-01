*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0080
*  Creation Date      : 20.03.2023
*  Author             : Jakarin S
*  Add-on ID          : ZFIAPR002
*  Description        : AP Invoice Reconcilation
*  Purpose            :
*  Copied from        : ZR_FIAP_INVOICE_RECON
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

REPORT ZSDSFIR0080 MESSAGE-ID ZSDSFI01.
*--------------------------------------------------------------------------
* T A B L E S . . .
*--------------------------------------------------------------------------
TABLES : BKPF,  "Accounting Document Header
         BSAK,  "Vendor Close
         BSIK,  "vendor Open
         BSEC,  "One-time
         EKPO,  "Purchasing Document Item
         MARA,  "material data
         ZSDSCAC002,  "Constant Table for SDS Development
         LFA1.  "vendor

TYPES : BEGIN OF TY_RESULT,
          BUKRS      TYPE  BUKRS,      "Company code
          LIFNR      TYPE  LIFNR,       "Vendor number
          CNAME(150) TYPE  C    ,     "Concatenate name1 name2
          BELNR      TYPE  BSEG-BELNR, "Document number
          BUZEI      TYPE  BUZEI,      "Item
          BLDAT      TYPE  BLDAT,      "Document date
          BUDAT      TYPE  BUDAT,      "Posting date
          XBLNR      TYPE  XBLNR,      "reference
          SHKZG      TYPE  SHKZG,      "D/C indicator
          MENGE      TYPE  I,      "Quantity
          BPRME      TYPE  BPRME,      "Units of Measurement of Various Types
          MATNR      TYPE  MATNR,      "Material
          TXZ01      TYPE  TXZ01,      "Short text for PO item
          MATKL      TYPE  MATKL,      "Material group
          PRDHA      TYPE  PRODH,      "product hierachy
          WERKS      TYPE  WERKS,      "plant
          PRCTR      TYPE  PRCTR,      "profit center
          WAERS      TYPE  WAERS,      "Currency Key
          KURSF      TYPE  KURSF,      "Exchange rate
          MWSKZ      TYPE  MWSKZ,      "Tax Code
          FWBAS      TYPE  FWBAS,      "Tax Base Amount in Document Currency
          HWBAS      TYPE  HWBAS,      "Tax Base Amount in Local Currency
          FWSTE      TYPE  FWSTE,      "Tax Amount in Document Currency
          HWSTE      TYPE  HWSTE,      "Tax Amount in Local Currency
          WRBTR      TYPE  WRBTR,      "Amount in Document Currency
          DMBTR      TYPE  DMBTR,      "Amount in Local Currency
          CMPRE      TYPE  DMBTR,      "price per unit
          NTGEW      TYPE  NTGEW,      "Net Weight
          BRGEW      TYPE  BRGEW,      "Gross Weight
          GEWEI      TYPE  GEWEI,      "Weight Unit
          GROES      TYPE  GROES,      "dimension
          USNAM      TYPE  USNAM,      "created by
          EBELN      TYPE  EBELN,      "Purchasing Document Number
          EBELP      TYPE  EBELP,      "Purchasing Document Line
          SGTXT      TYPE  SGTXT,      "header text
          KOART      TYPE  KOART,
        END OF TY_RESULT.

TYPES : BEGIN OF TY_VEND,
          LIFNR TYPE  LIFNR,      "vendor no.
          NAME1 TYPE  NAME1,                            "name1
          NAME2 TYPE  NAME2,                            "name2
          KTOKK TYPE  KTOKK,      "vendor group
        END OF TY_VEND.

TYPES : BEGIN OF TY_BELNR,
          BUKRS TYPE BUKRS,
          BELNR TYPE BELNR_D,
          GJAHR TYPE GJAHR,
          LIFNR TYPE LIFNR,
          BLART TYPE BLART,
          POSTK TYPE BSCHL,
        END OF TY_BELNR.

TYPES : BEGIN OF TY_EKPO,
          EBELN TYPE EBELN,
          EBELP TYPE EBELP,
          TXZ01 TYPE TXZ01,
          MATKL TYPE MATKL,
          MATNR TYPE MATNR,
        END OF TY_EKPO.

* Data for ALV display
TYPE-POOLS SLIS.
TYPES: BEGIN OF TY_COL_STRUC,
         COL_POS   LIKE SY-CUCOL,
         FIELDNAME TYPE SLIS_FIELDNAME,
         TABNAME   TYPE SLIS_TABNAME,
       END OF TY_COL_STRUC.
TYPES: TY_COL_QUEUE TYPE TY_COL_STRUC OCCURS 1.
DATA: GS_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: INT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.
DATA: GS_COL_QUEUE TYPE TY_COL_STRUC.
DATA: GT_COL_QUEUE LIKE TABLE OF GS_COL_QUEUE.
DATA: GV_FILTER_AUART TYPE C.
DATA: ALV_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      ALV_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      ALV_EVENTS   TYPE SLIS_T_EVENT,
      ALV_SORT     TYPE SLIS_T_SORTINFO_ALV.
DATA : V_POS TYPE I .
DEFINE DEF_LIST_HEAD.
  CLEAR: LW_LISTLINE.
  LW_LISTLINE-TYP  = &1.
  LW_LISTLINE-KEY  = &2.
  LW_LISTLINE-INFO = &3.
  APPEND LW_LISTLINE TO LT_LISTHEAD.
END-OF-DEFINITION.

*&-----------------------------------------------------------------------------------*
*& I N T E R N A L    T A B L E
*&-----------------------------------------------------------------------------------*
DATA: GS_BKPF     TYPE  BKPF,
      GS_BKPF_ORG TYPE  BKPF,
      GS_BKPF2    TYPE  BKPF,
      GS_BSEG     TYPE  BSEG,
      GS_BSEG_ORG TYPE  BSEG,
      GS_BSEG_TMP TYPE  BSEG,
      GS_BSEG_BEL TYPE  BSEG,
      GS_VEND     TYPE  TY_VEND,
      GS_VEND_TMP TYPE  TY_VEND,
      GS_BELNR    TYPE  TY_BELNR,
      GS_EKPO     TYPE  TY_EKPO,
      GS_RESULT   TYPE  TY_RESULT,
      GS_MARA     TYPE  MARA,

      GT_RESULT   TYPE STANDARD TABLE OF TY_RESULT,
      GT_RESULT2  TYPE STANDARD TABLE OF TY_RESULT,
      GT_BKPF     TYPE STANDARD TABLE OF BKPF,
      GT_BKPF_ORG TYPE STANDARD TABLE OF BKPF,
      GT_BKPF2    TYPE STANDARD TABLE OF BKPF,
      GT_BSEG     TYPE STANDARD TABLE OF BSEG,
      GT_BSEG_ORG TYPE STANDARD TABLE OF BSEG,
      GT_BSEG_TMP TYPE STANDARD TABLE OF BSEG,
      GT_BSEG_BEL TYPE STANDARD TABLE OF BSEG,
      GT_VEND     TYPE STANDARD TABLE OF TY_VEND,
      GT_VENDOT   TYPE STANDARD TABLE OF BSEC,
      GT_LFA1     TYPE STANDARD TABLE OF TY_VEND,
      GT_BELNR    TYPE STANDARD TABLE OF TY_BELNR,
      GT_BELNR_T  TYPE STANDARD TABLE OF TY_BELNR,
      GT_BELNR_G  TYPE STANDARD TABLE OF TY_BELNR,
      GT_EKPO     TYPE STANDARD TABLE OF TY_EKPO,
      GT_EKPO_T   TYPE STANDARD TABLE OF TY_EKPO,
      GT_MARA     TYPE STANDARD TABLE OF MARA.

DATA : GT_BSED   TYPE STANDARD TABLE OF BSED,
       GT_BSED_1 TYPE STANDARD TABLE OF BSED,
       GT_BSID   TYPE STANDARD TABLE OF BSID_VIEW,
       GT_BSID_1 TYPE STANDARD TABLE OF BSID_VIEW,
       GT_BSIS   TYPE STANDARD TABLE OF BSIS_VIEW,
       GT_BSAS   TYPE STANDARD TABLE OF BSAS_VIEW,
       GT_BSIS_1 TYPE STANDARD TABLE OF BSIS_VIEW,
       GT_BSAS_1 TYPE STANDARD TABLE OF BSAS_VIEW.

*       GT_BSAD   TYPE STANDARD TABLE OF BSAD_VIEW,
*       GT_BSAD_1 TYPE STANDARD TABLE OF BSAD_VIEW,
*       GT_BSID   TYPE STANDARD TABLE OF BSID_VIEW,
*       GT_BSID_1 TYPE STANDARD TABLE OF BSID_VIEW,
*       GT_BSIS   TYPE STANDARD TABLE OF BSIS_VIEW,
*       GT_BSAS   TYPE STANDARD TABLE OF BSAS_VIEW,
*       GT_BSIS_1 TYPE STANDARD TABLE OF BSIS_VIEW,
*       GT_BSAS_1 TYPE STANDARD TABLE OF BSAS_VIEW.


DATA : TLINE TYPE STANDARD TABLE OF TLINE WITH HEADER LINE,
       THEAD TYPE THEAD.

*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
DATA : GV_NAME(150)     TYPE C,
       GV_ITEM1         TYPE I,
       GV_ITEM          TYPE I,
       GV_HEADER(100)   TYPE C,
       GV_HEADER_C(100) TYPE C,
       GV_P_BY(50)      TYPE C,
       GV_P_DAT(50)     TYPE C,
       GV_S_DAT(150)    TYPE C,
       GV_PAGE(20)      TYPE C,
       V_HEAD(25)       TYPE C,
       V_LINE(50)       TYPE C,
       GV_AMOUNT_D      TYPE P LENGTH 15 DECIMALS 4,
       GV_TOTAL_D       TYPE DMBTR,
       GV_AMOUNT_L      TYPE P LENGTH 15 DECIMALS 4,
       GV_TOTAL_L       TYPE WRBTR,
       GV_TAX_D         TYPE P LENGTH 15 DECIMALS 4,
       GV_TAX_TOT_D     TYPE FWSTE,
       GV_TAX_L         TYPE P LENGTH 15 DECIMALS 4,
       GV_TAX_TOT_L     TYPE HWSTE,
       GV_BFTAX_D       TYPE P LENGTH 15 DECIMALS 4,
       GV_BFTOT_D       TYPE DMBTR,
       GV_BFTAX_L       TYPE P LENGTH 15 DECIMALS 4,
       GV_BFTOT_L       TYPE WRBTR,
       GV_BELNR_O       TYPE BELNR,
       GV_TXZ01         TYPE TXZ01,
       GV_EBELN         TYPE EBELN,
       GV_EBELP         TYPE EBELP,
       GV_MATKL         TYPE MATKL,
       GV_PRDHA         TYPE PRODH,

       GV_TAX_J         TYPE N,
       GV_BFT_J         TYPE N,
       GV_AMT_J         TYPE N.

DATA:
  GD_REPID    LIKE SY-REPID,
  GD_SAVE(1)  TYPE C,
  GD_EXIT(1)  TYPE C,
  GS_XVARIANT LIKE DISVARIANT,
  GS_VARIANT  LIKE DISVARIANT.

DATA : GT_MONTH_NAMES	TYPE TABLE OF	T247,
       GS_MONTH_NAMES LIKE LINE OF GT_MONTH_NAMES.

*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
CONSTANTS : GC_REPID3    TYPE REPID VALUE 'ZR_FIAP_INVOICE_RECON',
            GC_CN        TYPE ZSDSCAC002-CONST VALUE 'SD*',
            GC_AR        TYPE ZSDSCAC002-CONST VALUE 'FI*',
            GC_SD        TYPE ZSDSCAC002-CONST VALUE 'SD*',
            GC_MASK_TIME TYPE CHAR8  VALUE '__:__',
            GC_MASK_DATE TYPE CHAR10 VALUE '__.__.____'.
*&-----------------------------------------------------------------------------------*
*& R A N G E S
*&-----------------------------------------------------------------------------------*
RANGES : R_CN   FOR BKPF-BLART,
         R_AP   FOR BKPF-BLART,
         R_RE   FOR BKPF-BLART.
*---------------------------------------------------------------------------
*   S E L E C T I O N    S C R E E N
*---------------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.

  PARAMETERS: P_BUKRS TYPE BKPF-BUKRS DEFAULT '1000' OBLIGATORY.

  SELECT-OPTIONS: S_GJAHR FOR BKPF-GJAHR,
                  S_KTOKK FOR LFA1-KTOKK OBLIGATORY,      "vendor group
                  S_LIFNR FOR LFA1-LIFNR ,      "vendor no.
                  S_BUDAT FOR BKPF-BUDAT,       "posting date
                  S_BELNR FOR BKPF-BELNR.       "document no
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
  PARAMETERS : R_SUMM RADIOBUTTON GROUP TYP,
               R_DETL RADIOBUTTON GROUP TYP DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B2.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_GET_MONTH.
*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION .
*  PERFORM get_genc .
  PERFORM GET_DATA .
  PERFORM PREPARE_DATA.
  PERFORM PRINT_REPORT.

**&---------------------------------------------------------------------*
**&      Form  GET_GENC
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM get_genc .
*
*  DATA : lw_genc TYPE ZSDSCAC002.
*
*  REFRESH : lt_gencr, lt_gencd.
*  CLEAR lw_genc.
*
*  SELECT const value
*  INTO TABLE lt_gencd
*  FROM ZSDSCAC002
*  WHERE repid = gc_repid3
*    and const LIKE 'FI%'.
*
*  SELECT const value
*  INTO TABLE lt_gencr
*  FROM ZSDSCAC002
*  WHERE repid = gc_repid3
*    and const LIKE 'PO%' .
*
*ENDFORM.                    " GET_GENC

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
*For Vendors
  SELECT LIFNR
         NAME1
         NAME2
         KTOKK
    INTO TABLE GT_VEND
    FROM LFA1
   WHERE LIFNR IN S_LIFNR
     AND KTOKK IN S_KTOKK.

* Get invoice document data
  SELECT  BUKRS
          BELNR
          GJAHR
          LIFNR
          BLART
          BSCHL
    INTO TABLE GT_BELNR_G
    FROM  BSIK
    FOR ALL ENTRIES IN GT_VEND
    WHERE LIFNR EQ GT_VEND-LIFNR
      AND BUKRS EQ P_BUKRS
      AND GJAHR IN S_GJAHR
      AND BUDAT IN S_BUDAT
      AND BELNR IN S_BELNR.

    APPEND LINES OF GT_BELNR_G TO GT_BELNR.

    SELECT  BUKRS
            BELNR
            GJAHR
            LIFNR
            BLART
            BSCHL
      INTO TABLE GT_BELNR_T
      FROM  BSAK
      FOR ALL ENTRIES IN GT_VEND
      WHERE LIFNR EQ GT_VEND-LIFNR
        AND BUKRS EQ P_BUKRS
        AND GJAHR IN S_GJAHR
        AND BUDAT IN S_BUDAT
        AND BELNR IN S_BELNR.
    APPEND LINES OF GT_BELNR_T TO GT_BELNR.

    IF S_KTOKK NE SPACE OR S_LIFNR NE SPACE.
* Accounting document segment
* Select line detail
      SELECT *
        INTO TABLE GT_BSEG_ORG
        FROM BSEG
       FOR ALL ENTRIES IN GT_BELNR
       WHERE BUKRS EQ GT_BELNR-BUKRS
         AND BELNR EQ GT_BELNR-BELNR
         AND GJAHR EQ GT_BELNR-GJAHR
         AND KTOSL IN ('KBS','EGK')   "KBS = invoice from PO / EGK = invoice FI direct
         AND KOART EQ 'K'.

      SELECT *
        INTO TABLE GT_BSEG_BEL
        FROM BSEG
       FOR ALL ENTRIES IN GT_BSEG_ORG
       WHERE BUKRS EQ GT_BSEG_ORG-BUKRS
         AND BELNR EQ GT_BSEG_ORG-BELNR
         AND BUZEI NE GT_BSEG_ORG-BUZEI
         AND GJAHR EQ GT_BSEG_ORG-GJAHR
         AND ( KOART EQ 'S' OR UMSKZ NE SPACE )
         AND BUZID NE 'T'.

      SELECT *
      INTO TABLE GT_BSEG
      FROM BSEG
     FOR ALL ENTRIES IN GT_BSEG_ORG
     WHERE BUKRS EQ GT_BSEG_ORG-BUKRS
       AND BELNR EQ GT_BSEG_ORG-BELNR
       AND GJAHR EQ GT_BSEG_ORG-GJAHR
       AND MWART EQ 'V'.

      CHECK NOT GT_BSEG_BEL IS INITIAL.

      SELECT *
        INTO TABLE GT_BKPF
        FROM BKPF
       FOR ALL ENTRIES IN GT_BSEG_ORG
       WHERE BUKRS EQ GT_BSEG_ORG-BUKRS
         AND GJAHR EQ GT_BSEG_ORG-GJAHR
         AND BELNR EQ GT_BSEG_ORG-BELNR
*       AND bldat IN s_budat
         AND BUDAT IN S_BUDAT.
*       AND blart NE 'KX'  " Remove by Wantanee 20191010 T41K933707
*       AND stblg EQ space. " Remove by Wantanee 20191010 T41K933707
    ELSE.
      SELECT *
        INTO TABLE GT_BKPF
        FROM BKPF
       WHERE BUKRS EQ P_BUKRS
         AND GJAHR IN S_GJAHR
         AND BELNR IN S_BELNR
         AND BLDAT IN S_BUDAT.
*       AND blart NE 'KX'  " Remove by Wantanee 20191010 T41K933707
*       AND stblg EQ space. " Remove by Wantanee 20191010 T41K933707
      CHECK NOT GT_BKPF IS INITIAL.
      SELECT *
        INTO TABLE GT_BSEG_BEL
        FROM BSEG
      FOR ALL ENTRIES IN GT_BKPF
       WHERE BUKRS EQ GT_BKPF-BUKRS
         AND GJAHR EQ GT_BKPF-GJAHR
         AND BELNR EQ GT_BKPF-BELNR
         AND BUZID NE 'T'.

    ENDIF.

    SELECT EBELN EBELP TXZ01 MATKL MATNR
     INTO TABLE GT_EKPO
     FROM EKPO
    FOR ALL ENTRIES IN GT_BSEG_BEL
    WHERE EBELN  EQ  GT_BSEG_BEL-EBELN
      AND EBELP  EQ  GT_BSEG_BEL-EBELP.

    IF SY-SUBRC = 0.
      SELECT *
        INTO TABLE GT_MARA
        FROM MARA
      FOR ALL ENTRIES IN GT_EKPO
       WHERE MATNR = GT_EKPO-MATNR.
    ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_DATA .
  DATA : GV_BELNR    TYPE BSEG-BELNR,
         GV_MATKL    TYPE EKPO-MATKL,
         GV_PRDHA    TYPE MARA-PRDHA,
         LV_BELNR    TYPE BSEG-BELNR,
         GV_TAXCD(2) TYPE C,
         GV_PAGE     TYPE I,
         GV_XX       TYPE P LENGTH 1 DECIMALS 3,
         GV_PPUN     TYPE CMPRE,
         GV_NAME1    TYPE NAME,
         GV_NAME2    TYPE NAME,
         GS_BSEG_LOP TYPE BSEG.
  CLEAR:   GV_NAME,
           GV_AMOUNT_D,
           GV_TOTAL_D,
           GV_AMOUNT_L ,
           GV_TOTAL_L,
           GV_TAX_D,
           GV_TAX_TOT_D,
           GV_TAX_L,
           GV_TAX_TOT_L,
           GV_BFTAX_D,
           GV_BFTOT_D,
           GV_BFTAX_L,
           GV_BFTOT_L,
           V_HEAD, V_LINE.

  GV_XX = ( 1 / 1000 ).
  SORT GT_VEND BY LIFNR ASCENDING.
  IF R_DETL IS NOT INITIAL.
    LOOP AT GT_BKPF INTO GS_BKPF WHERE GLVOR = 'RFBU' AND STGRD = SPACE  . "FI DIRECT
      LOOP AT GT_BSEG_BEL INTO GS_BSEG WHERE BELNR = GS_BKPF-BELNR AND GJAHR = GS_BKPF-GJAHR AND KOART NE 'K' AND BUZID NE 'T' .
        CLEAR : GV_AMOUNT_D,
                GV_TOTAL_D,
                GV_AMOUNT_L ,
                GV_TOTAL_L,
                GV_TAX_D,
                GV_TAX_TOT_D,
                GV_TAX_L,
                GV_TAX_TOT_L,
                GV_BFTAX_D, GV_TAX_J, GV_BFT_J, GV_AMT_J,
                GV_BFTOT_D,
                GV_BFTAX_L,
                GV_BFTOT_L,
                GV_TAXCD .

        READ TABLE GT_BSEG_ORG INTO GS_BSEG_ORG WITH KEY BELNR = GS_BSEG-BELNR
                                                         GJAHR = GS_BSEG-GJAHR.
        IF GS_BSEG_ORG-LIFNR = 'OT01'.
          SELECT NAME1 NAME2
            INTO (GV_NAME1, GV_NAME2)
            FROM BSEC
            WHERE BELNR  EQ  GS_BSEG_ORG-BELNR
              AND GJAHR  EQ  GS_BSEG_ORG-GJAHR
              AND BUKRS  EQ  GS_BSEG_ORG-BUKRS.

            GS_VEND-LIFNR = GS_BSEG_ORG-LIFNR.
            CONCATENATE GV_NAME1 GV_NAME2 INTO GV_NAME.
          ENDSELECT.
        ELSE.
          READ TABLE GT_VEND INTO GS_VEND WITH KEY LIFNR = GS_BSEG_ORG-LIFNR.
          CONCATENATE GS_VEND-NAME1 GS_VEND-NAME2 INTO GV_NAME.
        ENDIF.

        READ TABLE GT_BSEG INTO GS_BSEG_BEL WITH KEY BELNR = GS_BSEG-BELNR
                                                     GJAHR = GS_BSEG-GJAHR
                                                        MWART = 'V'.

        IF GS_BSEG-SHKZG = 'S'.
          GV_BFTAX_D  = GS_BSEG_BEL-FWBAS * 1 .
          GV_BFTAX_L  = GS_BSEG_BEL-HWBAS * 1 .
          GV_TAX_D    = GS_BSEG_BEL-WRBTR * 1 .
          GV_TAX_L    = GS_BSEG_BEL-DMBTR * 1 .
          GV_AMOUNT_D = ( GS_BSEG-WRBTR * 1 ) + ( GV_TAX_D * 1 ).
          GV_AMOUNT_L = ( GS_BSEG-DMBTR * 1 ) + ( GV_TAX_L * 1 ).
        ELSE.
          GV_BFTAX_D  = GS_BSEG_BEL-FWBAS * -1 .
          GV_BFTAX_L  = GS_BSEG_BEL-HWBAS * -1 .
          GV_TAX_D    = GS_BSEG_BEL-WRBTR * -1 .
          GV_TAX_L    = GS_BSEG_BEL-DMBTR * -1 .
          GV_AMOUNT_D = ( GS_BSEG-WRBTR * -1 ) + ( GV_TAX_D * -1 ).
          GV_AMOUNT_L = ( GS_BSEG-DMBTR * -1 ) + ( GV_TAX_L * -1 ).
        ENDIF.
        IF GS_BSEG-MWSKZ = SPACE OR GS_BSEG-MWSKZ = 'IX' OR GS_BSEG-MWSKZ = '' .
          GV_TAX_D = 0 .
          GV_TAX_L = 0 .
          GV_BFTAX_D = GS_BSEG-WRBTR.
          GV_BFTAX_L = GS_BSEG-DMBTR.
          GV_AMOUNT_D = ( GV_BFTAX_D ) + ( GV_TAX_D ).
          GV_AMOUNT_L = ( GV_BFTAX_L ) + ( GV_TAX_L ).
        ELSE.
          GV_TAX_D = ( ( GS_BSEG-WRBTR * 7 ) / 100 ) + GV_XX.
          GV_TAX_L = ( ( GS_BSEG-DMBTR * 7 ) / 100 ) + GV_XX.
          GV_BFTAX_D = GS_BSEG-WRBTR.
          GV_BFTAX_L = GS_BSEG-DMBTR.
          GV_AMOUNT_D = ( GV_BFTAX_D ) + ( GV_TAX_D ).
          GV_AMOUNT_L = ( GV_BFTAX_L ) + ( GV_TAX_L ).
        ENDIF.

        IF GS_BSEG-SHKZG = 'S'.
          GV_TAX_D    =   GV_TAX_D * 1.
          GV_TAX_L    =   GV_TAX_L * 1.
          GV_BFTAX_D  =   GV_BFTAX_D * 1.
          GV_BFTAX_L  =   GV_BFTAX_L * 1.
          GV_AMOUNT_D =   GV_AMOUNT_D * 1.
          GV_AMOUNT_L =   GV_AMOUNT_L * 1.
        ELSE.
          GV_TAX_D    =   GV_TAX_D * -1.
          GV_TAX_L    =   GV_TAX_L * -1.
          GV_BFTAX_D  =   GV_BFTAX_D * -1.
          GV_BFTAX_L  =   GV_BFTAX_L * -1.
          GV_AMOUNT_D =   GV_AMOUNT_D * -1.
          GV_AMOUNT_L =   GV_AMOUNT_L * -1.
        ENDIF.

        IF GS_BKPF-KURSF = SPACE.
          GS_BKPF-KURSF = 1.
        ENDIF.

        IF GS_BKPF-WAERS = 'JPY'.
          GV_TAX_D    = GV_TAX_D * 100.
          GV_BFTAX_D  = GV_BFTAX_D * 100.
          GV_AMOUNT_D = GV_AMOUNT_D * 100.
        ENDIF.

        MOVE : GS_BKPF-BUKRS TO GS_RESULT-BUKRS,
               GS_VEND-LIFNR TO GS_RESULT-LIFNR,
               GV_NAME       TO GS_RESULT-CNAME,
               GS_BKPF-BELNR TO GS_RESULT-BELNR,
               GS_BSEG-BUZEI TO GS_RESULT-BUZEI,
               GS_BKPF-BLDAT TO GS_RESULT-BLDAT,
               GS_BKPF-BUDAT TO GS_RESULT-BUDAT,
               GS_BSEG-SHKZG TO GS_RESULT-SHKZG,
               GS_BKPF-XBLNR TO GS_RESULT-XBLNR,
               GS_BSEG_ORG-SGTXT TO GS_RESULT-SGTXT,
               GS_BSEG-SGTXT TO GS_RESULT-TXZ01,
               GS_BSEG-PRCTR TO GS_RESULT-PRCTR,
               GS_BKPF-WAERS TO GS_RESULT-WAERS,
               GS_BKPF-KURSF TO GS_RESULT-KURSF,
               GS_BSEG-KOART TO GS_RESULT-KOART,
               GS_BSEG-MWSKZ TO GS_RESULT-MWSKZ,
               GS_BKPF-USNAM TO GS_RESULT-USNAM,
               GV_AMOUNT_L   TO GS_RESULT-DMBTR,
               GV_TAX_L      TO GS_RESULT-HWSTE,
               GV_BFTAX_L    TO GS_RESULT-HWBAS,
               GV_AMT_J      TO GS_RESULT-WRBTR,
               GV_TAX_J      TO GS_RESULT-FWSTE,
               GV_BFT_J      TO GS_RESULT-FWBAS,
               GV_AMOUNT_D   TO GS_RESULT-WRBTR,
               GV_TAX_D      TO GS_RESULT-FWSTE,
               GV_BFTAX_D    TO GS_RESULT-FWBAS.

        APPEND GS_RESULT TO GT_RESULT.
      ENDLOOP.
    ENDLOOP.
* ENDLOOP.
  ELSE.
*LOOP AT lt_gencr INTO lw_genc.
    LOOP AT GT_BKPF INTO GS_BKPF.
      LOOP AT GT_BELNR INTO GS_BELNR WHERE BELNR = GS_BKPF-BELNR AND GJAHR = GS_BKPF-GJAHR.
        IF GS_BELNR-BELNR NE GV_BELNR_O.
          CLEAR : GV_AMOUNT_D,
                  GV_TOTAL_D,
                  GV_AMOUNT_L ,
                  GV_TOTAL_L,
                  GV_TAX_D,
                  GV_TAX_TOT_D,
                  GV_TAX_L,
                  GV_TAX_TOT_L,
                  GV_BFTAX_D,
                  GV_BFTOT_D,
                  GV_BFTAX_L,
                  GV_BFTOT_L,
                  GS_VEND,
                  GV_TAXCD,
                  GV_MATKL,
                  GV_PRDHA.
        ENDIF.

        READ TABLE GT_BSEG_ORG INTO GS_BSEG_ORG WITH KEY BELNR = GS_BELNR-BELNR
                                                         GJAHR = GS_BELNR-GJAHR.

        IF GS_BSEG_ORG-LIFNR = 'OT01'.
          SELECT NAME1 NAME2
            INTO (GV_NAME1, GV_NAME2)
            FROM BSEC
            WHERE BELNR  EQ  GS_BSEG_ORG-BELNR
              AND GJAHR  EQ  GS_BSEG_ORG-GJAHR
              AND BUKRS  EQ  GS_BSEG_ORG-BUKRS.

            GS_VEND-LIFNR = GS_BSEG_ORG-LIFNR.
            CONCATENATE GV_NAME1 GV_NAME2 INTO GV_NAME.
          ENDSELECT.
        ELSE.
          READ TABLE GT_VEND INTO GS_VEND WITH KEY LIFNR = GS_BSEG_ORG-LIFNR.
          CONCATENATE GS_VEND-NAME1 GS_VEND-NAME2 INTO GV_NAME.
        ENDIF.

        LOOP AT GT_BSEG_BEL INTO GS_BSEG_BEL WHERE BELNR EQ GS_BELNR-BELNR AND GJAHR EQ GS_BELNR-GJAHR .
          IF GS_BSEG_BEL-SHKZG = 'S'.
            GV_BFTAX_D    =   GV_BFTAX_D + GS_BSEG_BEL-WRBTR * 1.
            GV_BFTAX_L    =   GV_BFTAX_L + GS_BSEG_BEL-DMBTR * 1.
          ELSE.
            GV_BFTAX_D    =   GV_BFTAX_D + GS_BSEG_BEL-WRBTR * -1.
            GV_BFTAX_L    =   GV_BFTAX_L + GS_BSEG_BEL-DMBTR * -1.
          ENDIF.
        ENDLOOP.

        LOOP AT GT_BSEG INTO GS_BSEG WHERE BELNR EQ GS_BELNR-BELNR AND GJAHR EQ GS_BELNR-GJAHR .
          IF GS_BSEG-SHKZG = 'S'.
            GV_TAX_D    =   GV_TAX_D + GS_BSEG-WRBTR * 1.
            GV_TAX_L    =   GV_TAX_L + GS_BSEG-DMBTR * 1.
          ELSE.
            GV_TAX_D    =   GV_TAX_D + GS_BSEG-WRBTR * -1.
            GV_TAX_L    =   GV_TAX_L + GS_BSEG-DMBTR * -1.
          ENDIF.
        ENDLOOP.

        LOOP AT GT_BSEG_ORG INTO GS_BSEG_ORG WHERE BELNR EQ GS_BELNR-BELNR AND GJAHR EQ GS_BELNR-GJAHR .
          IF GS_BSEG-SHKZG = 'S'.
            GV_AMOUNT_D    =   GV_AMOUNT_D + GS_BSEG_ORG-WRBTR * 1.
            GV_AMOUNT_L    =   GV_AMOUNT_L + GS_BSEG_ORG-DMBTR * 1.
          ELSE.
            GV_AMOUNT_D    =   GV_AMOUNT_D + GS_BSEG_ORG-WRBTR * -1.
            GV_AMOUNT_L    =   GV_AMOUNT_L + GS_BSEG_ORG-DMBTR * -1.
          ENDIF.
        ENDLOOP.

        IF GS_BKPF-WAERS = 'JPY'.
          GV_TAX_D    = GV_TAX_D * 100.
          GV_BFTAX_D  = GV_BFTAX_D * 100.
          GV_AMOUNT_D = GV_AMOUNT_D * 100.
        ENDIF.

        MOVE:  GS_BELNR-BUKRS  TO  GS_RESULT-BUKRS,
               GS_VEND-LIFNR   TO  GS_RESULT-LIFNR,           "Customer number
               GV_NAME         TO  GS_RESULT-CNAME,           "Concatenate name1 name2
               GS_BELNR-BELNR  TO  GS_RESULT-BELNR,           "Document number
               GS_BKPF-BLDAT   TO  GS_RESULT-BLDAT,           "Document date
               GS_BKPF-BUDAT   TO  GS_RESULT-BUDAT,           "Posting date
               GS_BKPF-XBLNR   TO  GS_RESULT-XBLNR,           "reference
               GS_BKPF-BKTXT   TO GS_RESULT-SGTXT,           "document header text
               GS_BSEG_ORG-SGTXT TO  GS_RESULT-TXZ01,           "Short text for sales order header
               GS_BKPF-WAERS   TO  GS_RESULT-WAERS,           "Currency Key
               GS_BKPF-KURSF   TO  GS_RESULT-KURSF,           "Exchange rate
               GS_BSEG_ORG-MWSKZ   TO  GS_RESULT-MWSKZ,           "Tax Code
               GV_BFTAX_L      TO  GS_RESULT-HWBAS,           "Tax Base Amount in Local Currency
               GV_TAX_L        TO  GS_RESULT-HWSTE,           "Tax Amount in Local Currency
               GV_AMOUNT_L     TO  GS_RESULT-DMBTR,           "Amount in Local Currency
               GS_BKPF-USNAM   TO  GS_RESULT-USNAM,
               GV_AMT_J        TO  GS_RESULT-WRBTR,
               GV_TAX_J        TO  GS_RESULT-FWSTE,
               GV_BFT_J        TO  GS_RESULT-FWBAS,
               GV_AMOUNT_D     TO  GS_RESULT-WRBTR,
               GV_TAX_D        TO  GS_RESULT-FWSTE,
               GV_BFTAX_D      TO  GS_RESULT-FWBAS.
        APPEND GS_RESULT TO GT_RESULT.
        GV_BELNR_O = GS_BELNR-BELNR .
      ENDLOOP.
    ENDLOOP.

  ENDIF.

  DELETE ADJACENT DUPLICATES FROM GT_RESULT.
  DELETE GT_RESULT WHERE KOART EQ 'K'.

ENDFORM.                    " PREPARE_DATA

*&---------------------------------------------------------------------*
*&      Form  REPORT_HEADER
*&---------------------------------------------------------------------*
FORM REPORT_HEADER.
  DATA : LT_LISTHEAD TYPE SLIS_T_LISTHEADER,
         LW_LISTLINE TYPE SLIS_LISTHEADER.
  DATA : LV_DATE(10)    TYPE C,
         LV_TIME(5)     TYPE C,
         LV_TEXT_L(100) TYPE C,
         LV_TEXT_H(100) TYPE C,
         GV_ACC_G(200)  TYPE C,
         GV_S_CUST(150) TYPE C,
         GV_S_DOC(150)  TYPE C,
         GV_S_TYPE(150) TYPE C,
         GV_S_BUDT(200) TYPE C.
  CLEAR : GV_HEADER,GV_HEADER_C,
          GV_P_BY,
          GV_P_DAT,
          GV_S_DAT.

  DATA : LV_PERIOD TYPE  POPER,
         LV_GJAHR  TYPE  GJAHR.

  DATA : LV_PERIOD1 TYPE C LENGTH 20.

  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA : ES_VARIANT TYPE  DISVARIANT,
         E_SAVE     TYPE  CHAR1.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    IF REF_GRID IS NOT INITIAL.
      CALL METHOD REF_GRID->GET_VARIANT
        IMPORTING
          ES_VARIANT = ES_VARIANT
          E_SAVE     = E_SAVE.
    ENDIF.
  ENDIF.

  WRITE SY-DATUM TO LV_DATE USING EDIT MASK GC_MASK_DATE.
  WRITE SY-UZEIT TO LV_TIME USING EDIT MASK GC_MASK_TIME.
  CLEAR: GV_HEADER, GV_HEADER_C.
  GV_HEADER_C = 'Siam Daikin Sales Co.,Ltd.' .
  IF R_DETL IS NOT INITIAL.
    GV_HEADER = 'Payable Invoice Results (Detail)'.
  ELSE.
    GV_HEADER = 'Payable Invoice Results (Summary)'.
  ENDIF.

  IF ES_VARIANT-VARIANT EQ '/ONCHUMA'   OR
   ( S_LIFNR-LOW        EQ '0000132005' AND
     S_LIFNR-HIGH       IS INITIAL ).
    GV_HEADER = 'Payment Voucher Approval Report'.

    CALL FUNCTION 'ZFM_GET_PERIOD_FISCAL_YEAR'
      EXPORTING
        I_BUKRS  = '1000'
        I_DATUM  = SY-DATUM
      IMPORTING
        E_PERIOD = LV_PERIOD
        E_GJAHR  = LV_GJAHR.

    READ TABLE GT_MONTH_NAMES	INTO GS_MONTH_NAMES
    WITH KEY MNR = SY-DATUM+4(2).
    IF SY-SUBRC = 0.
      CONCATENATE GS_MONTH_NAMES-LTX SY-DATUM+0(4) INTO LV_PERIOD1 SEPARATED BY SPACE.
    ENDIF.

    CONCATENATE 'Company Name :' GV_HEADER_C INTO GV_HEADER_C SEPARATED BY SPACE.
*    CONCATENATE 'Report Title :' gv_header   INTO gv_header   SEPARATED BY space.
    CONCATENATE 'Period     :'       LV_PERIOD1  INTO LV_PERIOD1  SEPARATED BY SPACE.

    DEF_LIST_HEAD 'H' '' GV_HEADER_C.
    DEF_LIST_HEAD 'S' '' ''.
    DEF_LIST_HEAD 'S' 'Report Title :' GV_HEADER.
*    def_list_head 'S' '*' ''.
    DEF_LIST_HEAD 'A' '' 'Name  : DAIKIN INDUSTRIES(THAILAND)LTD'.
    DEF_LIST_HEAD 'A' '' LV_PERIOD1.

  ELSE.

    CONCATENATE 'Print on :'  LV_DATE LV_TIME INTO GV_P_DAT SEPARATED BY SPACE.    "print date
    CONCATENATE 'Print by :'  SY-UNAME  INTO GV_P_BY SEPARATED BY SPACE.           "print by
    DEF_LIST_HEAD 'S' '' GV_HEADER_C.
    DEF_LIST_HEAD 'H' '' GV_HEADER.
    DEF_LIST_HEAD 'S' '' GV_P_DAT.
    DEF_LIST_HEAD 'S' '' GV_P_BY.

    CLEAR : LV_TEXT_L,
            LV_TEXT_H.
* customer group:
    IF S_KTOKK-LOW = SPACE.
      S_KTOKK-LOW = 'All'.
    ELSE.
      CASE S_KTOKK-LOW.
        WHEN ' '.
          LV_TEXT_L = '000All' .
        WHEN '1000'.
          CONCATENATE S_KTOKK-LOW 'Local Trade' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN '2000'.
          CONCATENATE S_KTOKK-LOW 'Subcontractor' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN '3000'.
          CONCATENATE S_KTOKK-LOW 'Oversea Trade' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN '4000'.
          CONCATENATE S_KTOKK-LOW 'Other Payables' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN '5000'.
          CONCATENATE S_KTOKK-LOW 'Import Payables' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN '6000'.
          CONCATENATE S_KTOKK-LOW 'Advance Employee'  INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN '7000'.
          CONCATENATE S_KTOKK-LOW 'Associated' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN 'OT01'.
          CONCATENATE S_KTOKK-LOW 'One-Time' INTO LV_TEXT_L SEPARATED BY SPACE.
        WHEN 'PT01'.
          CONCATENATE S_KTOKK-LOW 'Petty Cash' INTO LV_TEXT_L SEPARATED BY SPACE.
      ENDCASE.
    ENDIF.
    IF S_KTOKK-HIGH = SPACE.
      S_KTOKK-HIGH = S_KTOKK-LOW.
    ENDIF.
    CASE S_KTOKK-HIGH.
      WHEN '1000'.
        CONCATENATE S_KTOKK-HIGH 'Local Trade' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN '2000'.
        CONCATENATE S_KTOKK-HIGH 'Subcontractor' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN '3000'.
        CONCATENATE S_KTOKK-HIGH 'Oversea Trade' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN '4000'.
        CONCATENATE S_KTOKK-HIGH 'Other Payables' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN '5000'.
        CONCATENATE S_KTOKK-HIGH 'Import Payables' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN '6000'.
        CONCATENATE S_KTOKK-HIGH 'Advance Employee'  INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN '7000'.
        CONCATENATE S_KTOKK-HIGH 'Associated' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN 'OT01'.
        CONCATENATE S_KTOKK-HIGH 'One-Time' INTO LV_TEXT_H SEPARATED BY SPACE.
      WHEN 'PT01'.
        CONCATENATE S_KTOKK-HIGH 'Petty Cash' INTO LV_TEXT_H SEPARATED BY SPACE.
    ENDCASE.

    CONCATENATE 'Customer Group :'  LV_TEXT_L 'TO' LV_TEXT_H INTO GV_ACC_G SEPARATED BY SPACE.
    DEF_LIST_HEAD 'S' '' GV_ACC_G .
*Print Customer No.
    IF S_LIFNR-LOW = SPACE.
      S_LIFNR-LOW = 'All'.
    ENDIF.
    IF S_LIFNR-HIGH = SPACE.
      S_LIFNR-HIGH = S_LIFNR-LOW.
    ENDIF.
    CONCATENATE 'Customer :' S_LIFNR-LOW 'TO' S_LIFNR-HIGH INTO GV_S_CUST SEPARATED BY SPACE.
    DEF_LIST_HEAD 'S' '' GV_S_CUST.
*Print Document No.
    IF S_BELNR-LOW = SPACE.
      S_BELNR-LOW = 'All'.
    ENDIF.
    IF S_BELNR-HIGH = SPACE.
      S_BELNR-HIGH = S_BELNR-LOW.
    ENDIF.
    CONCATENATE 'Document No. :' S_BELNR-LOW 'TO' S_BELNR-HIGH INTO GV_S_DOC SEPARATED BY SPACE.
    DEF_LIST_HEAD 'S' '' GV_S_DOC.
*Print Document Date
    CLEAR : LV_TEXT_L,
            LV_TEXT_H.
    IF S_BUDAT-LOW = SPACE.
      S_BUDAT-LOW = '01.01.0000'.
    ENDIF.
    IF S_BUDAT-HIGH = SPACE.
      S_BUDAT-HIGH = '31.12.9999' .
    ENDIF.
    WRITE S_BUDAT-LOW TO LV_TEXT_L USING EDIT MASK GC_MASK_DATE.
    WRITE S_BUDAT-HIGH TO LV_TEXT_H USING EDIT MASK GC_MASK_DATE.
    CONCATENATE 'Document Date :' LV_TEXT_L 'TO' LV_TEXT_H INTO GV_S_BUDT SEPARATED BY SPACE.
    DEF_LIST_HEAD 'S' '' GV_S_BUDT.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_LISTHEAD
    EXCEPTIONS
      OTHERS             = 0.

ENDFORM.                    " REPORT_HEADER

*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
FORM PRINT_REPORT.
  PERFORM PREPARE_FIELDCAT USING 'GT_RESULT'.
  PERFORM BUILD_EVENT      USING ALV_EVENTS.
  PERFORM F_BUILD_LAYOUT   USING ALV_LAYOUT.
  PERFORM F_BUILD_SORT     USING ALV_SORT.
  PERFORM DISPLAY_OUTPUT.
ENDFORM.                    "print_report
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM F_BUILD_LAYOUT USING  P_GTYP_LAYOUT TYPE SLIS_LAYOUT_ALV.
  P_GTYP_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  P_GTYP_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.                    " F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_SORT
*&---------------------------------------------------------------------*
FORM F_BUILD_SORT USING  P_GTYP_SORT TYPE SLIS_T_SORTINFO_ALV.
  DATA: LW_SORT TYPE SLIS_SORTINFO_ALV.

* company
  LW_SORT-FIELDNAME = 'BUKRS'.
  LW_SORT-UP        = 'X'.
  APPEND LW_SORT TO P_GTYP_SORT.

ENDFORM.                    " F_BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_EVENT
*&---------------------------------------------------------------------*
FORM BUILD_EVENT USING  P_GTYP_EVENT TYPE SLIS_T_EVENT.
  FIELD-SYMBOLS <FS_EVENTS> LIKE LINE OF P_GTYP_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      ET_EVENTS       = P_GTYP_EVENT
    EXCEPTIONS
      LIST_TYPE_WRONG = 0
      OTHERS          = 0.

  LOOP AT P_GTYP_EVENT ASSIGNING <FS_EVENTS>
                       WHERE NAME = 'TOP_OF_PAGE'.
    <FS_EVENTS>-FORM = 'REPORT_HEADER'.
  ENDLOOP.

ENDFORM.                    "build_event
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM DISPLAY_OUTPUT.

  GD_REPID = SY-REPID.
  CLEAR GS_VARIANT.
  IF R_DETL IS NOT INITIAL.
    GS_VARIANT-VARIANT = '/DETAIL'.
  ELSE.
    GS_VARIANT-VARIANT = '/SUMMARY'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = 'ZR_FIAP_INVOICE_RECON'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IS_LAYOUT               = ALV_LAYOUT
      IT_FIELDCAT             = INT_FIELDCAT
      I_DEFAULT               = 'X'
      I_SAVE                  = 'A'
      IS_VARIANT              = GS_VARIANT
      IT_EVENTS               = ALV_EVENTS
      IT_SORT                 = ALV_SORT
    TABLES
      T_OUTTAB                = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.

*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*\

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1473   text
*----------------------------------------------------------------------*
FORM PREPARE_FIELDCAT  USING TABNAME TYPE SLIS_TABNAME.
*** Prepare table fields
* COMPANY CODE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BUKRS'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Co.Code'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Vendor
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'LIFNR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Vendor'.
  GS_FIELDCAT-REF_FIELDNAME = 'LIFNR'.
  GS_FIELDCAT-REF_TABNAME   = 'LFA1'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* CUST NAME
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'CNAME'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Vendor Name'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* DOC. NUMBER
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BELNR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Doc.No.'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* item
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BUZEI'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Item'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* DOC DATE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BLDAT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Doc.date'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* POSTING DATE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BUDAT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Post date'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* REFERENCE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'XBLNR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Reference'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* D/C
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SHKZG'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'D/C'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Invoice Quantity
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'MENGE'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Quantity'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Invoice sales unit
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BPRME'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Measure'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Material
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'MATNR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Material'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Short text for sales order item
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'TXZ01'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Text'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Material Group
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'MATKL'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Mat.Grp.'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Product Hierachy
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'PRDHA'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Prod.HRCH.'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Division
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WERKS'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Plant'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Profit Center
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'PRCTR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Prof.Ctr.'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* CURRENCY
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WAERS'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Currency'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* EXCHANGE RATE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'KURSF'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Rate'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* SALES TAX CODE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'MWSKZ'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Tax Code'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* TAX BASE AMOUNT DOCUMENT CURR.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'FWBAS'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Base Amount(Doc)'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* TAX BASE AMOUNT DOCUMENT LOCAL CURR
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'HWBAS'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Base Amount(Loc)'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* TAX AMOUNT DOC. CURR
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'FWSTE'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Tax Amount(Doc)'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* TAX AMOUNT LOC. CURR
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'HWSTE'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Tax Amount(Loc)'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* AMOUNT IN DOC.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WRBTR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Total Amount(Doc)'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* AMOUNT IN LOC.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'DMBTR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Total Amount(Loc)'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* USER NAME.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'USNAM'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Created By'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* price per unit
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'CMPRE'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Price/Unit'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Net Weight
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'NTGEW'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'NetWeight'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Gross Weight
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BRGEW'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'GrossWeight'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Weight Unit
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'GEWEI'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'WeightUnit'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Size/Dimensions
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'GROES'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Size/Dimensions'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* PO No.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'EBELN'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'PO Number'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* PO Line
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'EBELP'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'PO Line'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Doc. Header Text
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SGTXT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Doc.Header Text'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
* Account type
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'KOART'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Acc.Type'.
  APPEND GS_FIELDCAT TO INT_FIELDCAT.
ENDFORM.                    " PREPARE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_GET_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_MONTH .

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = SY-LANGU
    TABLES
      MONTH_NAMES           = GT_MONTH_NAMES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_GET_MONTH
