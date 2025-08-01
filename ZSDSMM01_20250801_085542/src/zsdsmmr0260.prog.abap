*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0260
*  Creation Date      : 09.05.2024
*  Author             : Thanapong C. (Eviden)
*  Add-on ID          : ZMMR017
*  Description        : Report to show landed cost in GR compare with
*                       landed cost in IR
*  Purpose            : Comparison of Landed Costs for GR and IR
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0260.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  ACDOCA,
  T001,
  T001W,
  T161,
  MARA.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSMMS023.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_GR,
         BELNR     TYPE ACDOCA-BELNR,
         GJAHR     TYPE ACDOCA-GJAHR,
         DOCLN     TYPE ACDOCA-DOCLN,
         AWREF     TYPE ACDOCA-AWREF,
         AWORG     TYPE ACDOCA-AWORG,
         AWITEM    TYPE ACDOCA-AWITEM,
         KTOSL     TYPE ACDOCA-KTOSL,
         WSL       TYPE ACDOCA-WSL,
         RWCUR     TYPE ACDOCA-RWCUR,
         HSL       TYPE ACDOCA-HSL,
         RHCUR     TYPE ACDOCA-RHCUR,
         RBUKRS    TYPE ACDOCA-RBUKRS,
         WERKS     TYPE ACDOCA-WERKS,
         EBELN     TYPE ACDOCA-EBELN,
         EBELP     TYPE ACDOCA-EBELP,
         LIFNR     TYPE ACDOCA-LIFNR,
         BUDAT     TYPE ACDOCA-BUDAT,
         BSART     TYPE EKKO-BSART,
         MATNR     TYPE ACDOCA-MATNR,
         PO_OBJKEY TYPE OBJKEY,
       END OF TS_GR.
TYPES: TT_GR TYPE STANDARD TABLE OF TS_GR
                  WITH EMPTY KEY.

TYPES: BEGIN OF TS_IR,
         XREF1_HD TYPE BKPF-XREF1_HD,
         BUKRS    TYPE BKPF-BUKRS,
         EBELN    TYPE ACDOCA-EBELN,
         EBELP    TYPE ACDOCA-EBELP,
         KTOSL    TYPE ACDOCA-KTOSL,
         BELNR    TYPE ACDOCA-BELNR,
         GJAHR    TYPE ACDOCA-GJAHR,
         DOCLN    TYPE ACDOCA-DOCLN,
         AWREF    TYPE ACDOCA-AWREF,
         AWORG    TYPE ACDOCA-AWORG,
         AWITEM   TYPE ACDOCA-AWITEM,
         WSL      TYPE ACDOCA-WSL,
         RWCUR    TYPE ACDOCA-RWCUR,
         HSL      TYPE ACDOCA-HSL,
         RHCUR    TYPE ACDOCA-RHCUR,
         WERKS    TYPE ACDOCA-WERKS,
         LIFNR    TYPE ACDOCA-LIFNR,
         BUDAT    TYPE ACDOCA-BUDAT,
         MATNR    TYPE ACDOCA-MATNR,
         KURSF    TYPE BKPF-KURSF,
         WRBTR    TYPE RSEG-WRBTR,
         DMBTR    TYPE BSEG-DMBTR,
         X_KTOSL  TYPE XFELD,
         KTOPL    TYPE ACDOCA-KTOPL,
         RACCT    TYPE ACDOCA-RACCT,
         DRCRK    TYPE ACDOCA-DRCRK,
       END OF TS_IR.
TYPES: TT_IR TYPE STANDARD TABLE OF TS_IR
                  WITH EMPTY KEY.

TYPES: BEGIN OF TS_SKA1_KEY,
         KTOPL TYPE SKA1-KTOPL,
         SAKNR TYPE SKA1-SAKNR,
       END OF TS_SKA1_KEY.
TYPES TT_SKA1_KEY TYPE HASHED TABLE OF TS_SKA1_KEY
                       WITH UNIQUE KEY KTOPL
                                       SAKNR.

TYPES: BEGIN OF TS_SKAT,
         KTOPL TYPE SKAT-KTOPL,
         SAKNR TYPE SKAT-SAKNR,
         TXT20 TYPE SKAT-TXT20,
       END OF TS_SKAT.
TYPES TT_SKAT TYPE HASHED TABLE OF TS_SKAT
                   WITH UNIQUE KEY KTOPL
                                   SAKNR.

TYPES: BEGIN OF TS_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BKTXT TYPE BKPF-BKTXT,
         KURSF TYPE BKPF-KURSF,
       END OF TS_BKPF.
TYPES: TT_GR_BKPF  TYPE SORTED TABLE OF TS_BKPF
                        WITH UNIQUE KEY BUKRS
                                        BELNR
                                        GJAHR.
TYPES: BEGIN OF TS_IR_BKPF,
         BUKRS    TYPE BKPF-BUKRS,
         BELNR    TYPE BKPF-BELNR,
         GJAHR    TYPE BKPF-GJAHR,
         KURSF    TYPE BKPF-KURSF,
         XREF1_HD TYPE BKPF-XREF1_HD,
         WAERS    TYPE BKPF-WAERS,
         HWAER    TYPE BKPF-HWAER,
       END OF TS_IR_BKPF.
TYPES: TT_IR_BKPF TYPE SORTED TABLE OF TS_IR_BKPF
                        WITH UNIQUE KEY BUKRS
                                        BELNR
                                        GJAHR.

TYPES: BEGIN OF TS_MSEG,
         MBLNR    TYPE MSEG-MBLNR,
         MJAHR    TYPE MSEG-MJAHR,
         ZEILE    TYPE MSEG-ZEILE,
         VBELN_IM TYPE MSEG-VBELN_IM,
         VBELP_IM TYPE MSEG-VBELP_IM,
       END OF TS_MSEG.
TYPES: TT_MSEG TYPE SORTED TABLE OF TS_MSEG
                    WITH UNIQUE KEY MBLNR
                                    MJAHR
                                    ZEILE.
TYPES: BEGIN OF TS_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME  TYPE CHAR140,
       END OF TS_LFA1.
TYPES:  TT_LFA1 TYPE SORTED TABLE OF TS_LFA1
                     WITH UNIQUE KEY LIFNR.

TYPES: BEGIN OF TS_KTOSL,
         KTOSL TYPE T030W-KTOSL,
       END OF TS_KTOSL.
TYPES: TT_KTOSL TYPE HASHED TABLE OF TS_KTOSL
                     WITH UNIQUE KEY KTOSL.

TYPES: BEGIN OF TS_T030W,
         KTOSL TYPE T030W-KTOSL,
         LTEXT TYPE T030W-LTEXT,
       END OF TS_T030W.
TYPES: TT_T030W TYPE SORTED TABLE OF TS_T030W
                     WITH UNIQUE KEY KTOSL.


TYPES: BEGIN OF TS_LFA1_PK,
         LIFNR TYPE LFA1-LIFNR,
       END OF TS_LFA1_PK.
TYPES: TT_LFA1_PK  TYPE SORTED TABLE OF TS_LFA1_PK
                        WITH UNIQUE KEY LIFNR.

TYPES: BEGIN OF TS_BKPF_PK,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
       END OF TS_BKPF_PK.
TYPES:  TT_BKPF_PK  TYPE SORTED TABLE OF TS_BKPF_PK
                         WITH UNIQUE KEY BUKRS
                                         BELNR
                                         GJAHR.

TYPES: BEGIN OF TS_XREF1_HD,
         BUKRS    TYPE BKPF-BUKRS,
         XREF1_HD TYPE BKPF-XREF1_HD,
       END OF TS_XREF1_HD.
TYPES:  TT_XREF1_HD TYPE SORTED TABLE OF TS_XREF1_HD
                         WITH UNIQUE KEY BUKRS
                                         XREF1_HD.

TYPES: BEGIN OF TS_EKPO_PK,
         EBELN TYPE  EKPO-EBELN,
         EBELP TYPE  EKPO-EBELP,
       END OF TS_EKPO_PK.
TYPES: TT_EKPO_PK TYPE SORTED TABLE OF TS_EKPO_PK
                       WITH UNIQUE KEY EBELN
                                       EBELP.

TYPES: BEGIN OF TS_MSEG_PK,
         MBLNR TYPE MSEG-MBLNR,
         MJAHR TYPE MSEG-MJAHR,
         ZEILE TYPE MSEG-ZEILE,
       END OF TS_MSEG_PK.
TYPES:  TT_MSEG_PK TYPE SORTED TABLE OF TS_MSEG_PK
                        WITH UNIQUE KEY MBLNR
                                        MJAHR
                                        ZEILE.

TYPES: BEGIN OF TS_ETD_DAYS,
         LIFNR TYPE LFA1-LIFNR,
         DAYS  TYPE I,
       END OF TS_ETD_DAYS.
TYPES: TT_ETD_DAYS TYPE STANDARD TABLE OF TS_ETD_DAYS.

TYPES: BEGIN OF TS_EKET,
         EBELN TYPE EKET-EBELN,
         EBELP TYPE EKET-EBELP,
         ETENR TYPE EKET-ETENR,
         EINDT TYPE EKET-EINDT,
       END OF TS_EKET.
TYPES: TT_EKET TYPE STANDARD TABLE OF TS_EKET.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
TYPES: TT_RACCT_RANGE TYPE RANGE OF ACDOCA-RACCT.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE CHAR1       VALUE 'X',
  GC_TCODE TYPE SY-TCODE    VALUE 'ZSDSMM009'.

CONSTANTS:
  BEGIN OF GC_KAPPL,
    PURCHASING TYPE T687T-KAPPL VALUE 'M',
  END OF GC_KAPPL.

CONSTANTS:
  BEGIN OF GC_BTTYPE,
    GR_FOR_PO TYPE ACDOCA-BTTYPE VALUE 'RMWE',
    INVOICE   TYPE ACDOCA-BTTYPE VALUE 'RMRP',
  END OF GC_BTTYPE.

CONSTANTS:
  BEGIN OF GC_BLART,
    INVOICE_GROSS TYPE BKPF-BLART VALUE 'RE',
  END OF GC_BLART.

CONSTANTS:
  BEGIN OF GC_RLDNR,
    LEADING_LEDGER TYPE ACDOCA-VRGNG VALUE '0L',
  END OF GC_RLDNR.

CONSTANTS:
  BEGIN OF GC_DRCRK,
    CREDIT TYPE ACDOCA-DRCRK VALUE 'H',
    DEBIT  TYPE ACDOCA-DRCRK VALUE 'S',
  END OF GC_DRCRK.

CONSTANTS:
  BEGIN OF GC_FNAME,
    GR_BUDAT    TYPE LVC_FNAME VALUE 'GR_BUDAT',
    GR_BKTXT    TYPE LVC_FNAME VALUE 'GR_BKTXT',
    GR_BELNR    TYPE LVC_FNAME VALUE 'GR_BELNR',
    GR_GJAHR    TYPE LVC_FNAME VALUE 'GR_GJAHR',
    GR_DOCLN    TYPE LVC_FNAME VALUE 'GR_DOCLN',
    GR_AWREF    TYPE LVC_FNAME VALUE 'GR_AWREF',
    GR_AWORG    TYPE LVC_FNAME VALUE 'GR_AWORG',
    GR_AWITEM   TYPE LVC_FNAME VALUE 'GR_AWITEM',
    GR_RWCUR    TYPE LVC_FNAME VALUE 'GR_RWCUR',
    GR_KURSF    TYPE LVC_FNAME VALUE 'GR_KURSF',
    KTOSL       TYPE LVC_FNAME VALUE 'KTOSL',
    KTOSL_TX    TYPE LVC_FNAME VALUE 'KTOSL_TX',
    GR_WSL      TYPE LVC_FNAME VALUE 'GR_WSL',
    GR_HSL      TYPE LVC_FNAME VALUE 'GR_HSL',
    IR_BUDAT    TYPE LVC_FNAME VALUE 'IR_BUDAT',
    IR_BELNR    TYPE LVC_FNAME VALUE 'IR_BELNR',
    IR_GJAHR    TYPE LVC_FNAME VALUE 'IR_GJAHR',
    IR_DOCLN    TYPE LVC_FNAME VALUE 'IR_DOCLN',
    IR_AWREF    TYPE LVC_FNAME VALUE 'IR_AWREF',
    IR_AWORG    TYPE LVC_FNAME VALUE 'IR_AWORG',
    IR_RWCUR    TYPE LVC_FNAME VALUE 'IR_RWCUR',
    IR_KURSF    TYPE LVC_FNAME VALUE 'IR_KURSF',
    IR_WSL      TYPE LVC_FNAME VALUE 'IR_WSL',
    IR_HSL      TYPE LVC_FNAME VALUE 'IR_HSL',
    IR_RACCT    TYPE LVC_FNAME VALUE 'IR_RACCT',
    IR_RACCT_TX TYPE LVC_FNAME VALUE 'IR_RACCT_TX',
    DIFF_HSL    TYPE LVC_FNAME VALUE 'DIFF_HSL',
    MATNR       TYPE LVC_FNAME VALUE 'MATNR',
    BUKRS       TYPE LVC_FNAME VALUE 'BUKRS',
    WERKS       TYPE LVC_FNAME VALUE 'WERKS',
    EBELN       TYPE LVC_FNAME VALUE 'EBELN',
    EBELP       TYPE LVC_FNAME VALUE 'EBELP',
    LIFNR       TYPE LVC_FNAME VALUE 'LIFNR',
    LIFNR_TX    TYPE LVC_FNAME VALUE 'LIFNR_TX',
    VBELN_IM    TYPE LVC_FNAME VALUE 'VBELN_IM',
    VBELP_IM    TYPE LVC_FNAME VALUE 'VBELP_IM',
    ETD         TYPE LVC_FNAME VALUE 'ETD',
    GR_RHCUR    TYPE LVC_FNAME VALUE 'GR_RHCUR',
    IR_RHCUR    TYPE LVC_FNAME VALUE 'IR_RHCUR',
  END OF GC_FNAME.

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
  GF_ETD_DAYS_DEFAULT TYPE I                                       ##NEEDED,
  GT_ETD_DAYS         TYPE TT_ETD_DAYS                             ##NEEDED,
  GR_KTOSL            TYPE RANGE OF KTOSL                          ##NEEDED,
  GR_UNPLAN_KTOSL     TYPE RANGE OF KTOSL                          ##NEEDED,
  GR_UNPLAN_GKOAR     TYPE RANGE OF GKOAR                          ##NEEDED,
  GR_RACCT_LAND_COST  TYPE TT_RACCT_RANGE                          ##NEEDED,
  GR_ETD_BSART        TYPE RANGE OF BSART                          ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS023'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 10,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 90.

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
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_BUKRS  FOR T001-BUKRS OBLIGATORY,
    S_WERKS  FOR T001W-WERKS OBLIGATORY,
    S_BSART  FOR T161-BSART OBLIGATORY,
    S_BUDAT  FOR ACDOCA-BUDAT OBLIGATORY DEFAULT SY-DATUM,
    S_EBELN  FOR ACDOCA-EBELN,
    S_MATNR  FOR MARA-MATNR.
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

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
*   Get Data
  PERFORM F_GET_DATA USING GR_RACCT_LAND_COST
                  CHANGING GT_RESULT.
  IF GT_RESULT IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
*    RETURN.
  ENDIF.


*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
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
    LC_BUKRS                TYPE  ZSDSDE_PARAM_NAME    VALUE 'BUKRS',
    LC_WERKS                TYPE  ZSDSDE_PARAM_NAME    VALUE 'WERKS',
    LC_BSART                TYPE  ZSDSDE_PARAM_NAME    VALUE 'BSART',
    LC_KTOSL                TYPE  ZSDSDE_PARAM_NAME    VALUE 'KTOSL',
    LC_GKOAR                TYPE  ZSDSDE_PARAM_NAME    VALUE 'GKOAR',
    LC_ETD_BSART            TYPE  ZSDSDE_PARAM_NAME    VALUE 'ETD_BSART',
    LC_GL_ACCOUNT_LAND_COST TYPE  ZSDSDE_PARAM_NAME    VALUE 'GL_ACCOUNT_LAND_COST',
    LC_ETD_DAYS             TYPE  ZSDSDE_PARAM_NAME    VALUE 'ETD_DAYS'
    .

  CONSTANTS: BEGIN OF LC_PARAM_EXT,
               ETD_DAYS_DEFAULT  TYPE ZSDSCAC001-PARAM_EXT VALUE 'DEFAULT',
               KTOSL_UNPLAN_COST TYPE ZSDSCAC001-PARAM_EXT VALUE 'UNPLANNED_COST',
             END OF  LC_PARAM_EXT.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID  TYPE PROGRAMM,
    LF_NUMBER TYPE I.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GF_ETD_DAYS_DEFAULT,
         GT_ETD_DAYS,
         GR_KTOSL,
         GR_UNPLAN_KTOSL,
         GR_UNPLAN_GKOAR,
         GR_ETD_BSART,
         GR_RACCT_LAND_COST.

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

    CLEAR LF_NUMBER.

    CASE <L_GENC>-PARAM.

      WHEN LC_BUKRS.

        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO S_BUKRS.

      WHEN LC_WERKS.

        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO S_WERKS.

      WHEN LC_BSART.

        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO S_BSART.

      WHEN LC_KTOSL.

        CASE <L_GENC>-PARAM_EXT.
          WHEN LC_PARAM_EXT-KTOSL_UNPLAN_COST.
            APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                            OPTION = <L_GENC>-PARAM_OPTION
                            LOW    = <L_GENC>-VALUE_LOW
                            HIGH   = <L_GENC>-VALUE_HIGH )
                   TO GR_UNPLAN_KTOSL.

          WHEN OTHERS.
            APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                            OPTION = <L_GENC>-PARAM_OPTION
                            LOW    = <L_GENC>-VALUE_LOW
                            HIGH   = <L_GENC>-VALUE_HIGH )
                   TO GR_KTOSL.
        ENDCASE.


      WHEN LC_ETD_BSART.

        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_ETD_BSART .

      WHEN LC_ETD_DAYS.

        IF <L_GENC>-PARAM_EXT IS NOT INITIAL.

          TRY.
              LF_NUMBER = <L_GENC>-VALUE_LOW.

              IF <L_GENC>-PARAM_EXT = LC_PARAM_EXT-ETD_DAYS_DEFAULT.
                GF_ETD_DAYS_DEFAULT = LF_NUMBER.
              ELSE.
                APPEND VALUE #( LIFNR  = |{ <L_GENC>-PARAM_EXT ALPHA = IN }|
                                DAYS   = LF_NUMBER )
                          TO GT_ETD_DAYS .
              ENDIF.

            CATCH  CX_SY_CONVERSION_NO_NUMBER.
              CONTINUE.
          ENDTRY.

        ENDIF.

      WHEN LC_GKOAR.

        CASE <L_GENC>-PARAM_EXT.
          WHEN LC_PARAM_EXT-KTOSL_UNPLAN_COST.
            APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                            OPTION = <L_GENC>-PARAM_OPTION
                            LOW    = <L_GENC>-VALUE_LOW
                            HIGH   = <L_GENC>-VALUE_HIGH )
                   TO GR_UNPLAN_GKOAR .
          WHEN OTHERS.
        ENDCASE.

      WHEN LC_GL_ACCOUNT_LAND_COST.

        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = |{ <L_GENC>-VALUE_LOW ALPHA = IN }|
                        HIGH   = |{ <L_GENC>-VALUE_HIGH ALPHA = IN }| )
               TO GR_RACCT_LAND_COST.

    ENDCASE.

  ENDLOOP.

  SORT GT_ETD_DAYS BY LIFNR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_DATA
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA    USING UT_RACCT_RANGE TYPE TT_RACCT_RANGE
                CHANGING CT_RESULT      TYPE TT_RESULT.

  DATA:
    LT_GR       TYPE TT_GR,
    LT_IR       TYPE TT_IR,
    LT_GR_BKPF  TYPE TT_GR_BKPF,
    LT_IR_BKPF  TYPE TT_IR_BKPF,
    LT_MSEG     TYPE TT_MSEG,
    LT_LFA1     TYPE TT_LFA1,
    LT_EKET     TYPE TT_EKET,
    LT_SKA1_KEY TYPE TT_SKA1_KEY,
    LT_SKAT     TYPE TT_SKAT,
    LT_KTOSL    TYPE TT_KTOSL,
    LT_T030W    TYPE TT_T030W.

* Initialize Output
  CLEAR: CT_RESULT.

  PERFORM F_GET_GOODS_RECEIPT CHANGING LT_GR.

  PERFORM F_GET_FI_DOCUMENT USING LT_GR
                         CHANGING LT_GR_BKPF
                                  LT_IR_BKPF.

  PERFORM F_GET_MATERIAL_DOCUMENT USING LT_GR
                               CHANGING LT_MSEG.

  PERFORM F_GET_INVOICE_RECEIPT   USING LT_GR
                                        LT_IR_BKPF
                                        UT_RACCT_RANGE
                               CHANGING LT_IR.

  PERFORM F_GET_SCHEDULE_LINES  USING LT_GR
                             CHANGING LT_EKET.

  PERFORM F_GET_MD_VENDOR USING LT_GR
                                LT_IR
                       CHANGING LT_LFA1.


  LT_KTOSL = CORRESPONDING #( LT_GR DISCARDING DUPLICATES ).

  LT_KTOSL = CORRESPONDING #( BASE ( LT_KTOSL )
                              LT_IR DISCARDING DUPLICATES ).

  PERFORM F_GET_MD_TRANSACTION    USING LT_KTOSL
                               CHANGING LT_T030W.

  LT_SKA1_KEY = CORRESPONDING #( LT_IR DISCARDING DUPLICATES
                                       MAPPING KTOPL = KTOPL
                                               SAKNR = RACCT ).

  PERFORM F_GET_ACCT_TEXT USING SY-LANGU
                                LT_SKA1_KEY
                       CHANGING LT_SKAT.

  PERFORM F_COLLECT_RESULT USING LT_GR
                                 LT_GR_BKPF
                                 LT_MSEG
                                 LT_LFA1
                                 LT_T030W
                                 LT_EKET
                                 LT_SKAT
                                 UT_RACCT_RANGE
                        CHANGING LT_IR
                                 CT_RESULT.

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
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
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
    LF_TEXT        TYPE TEXT60.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN GC_FNAME-GR_BUDAT.
*        TEXT-C01 : GR Posting Date
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 10.
      WHEN GC_FNAME-GR_BKTXT.
*        TEXT-C02 : HeaderText
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_BELNR.
*        TEXT-C03 : GR Acct. Doc. No.
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_GJAHR.
*      TEXT-C04 : GR Fiscal Year
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_DOCLN.
*      TEXT-C05 : GR Acct. Doc. Item
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_AWREF.
*      TEXT-C06 : GR Mat. Doc. No.
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_AWORG.
*      TEXT-C07 : GR Mat. Doc. Year
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_AWITEM.
*      TEXT-C08 : GR Mat. Doc. Item
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_RWCUR.
*      TEXT-C09 : GR Doc. Currency
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_KURSF.
*      TEXT-C10 : GR Exchange Rate
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-KTOSL.
*      TEXT-C11 : Transaction
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-KTOSL_TX.
*      TEXT-C12 : Transaction Description
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_WSL.
*      TEXT-C13 : GR Amt in Doc. Currency
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-GR_HSL.
*      TEXT-C14 : GR Amt in LC
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_BUDAT.
*      TEXT-C15 : IR Posting Date
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_BELNR.
*      TEXT-C16 : IR Acct. Doc. No.
        LF_TEXT                = TEXT-C16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_GJAHR.
*      TEXT-C17 : IR Fiscal Year
        LF_TEXT                = TEXT-C17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_DOCLN.
*      TEXT-C18 : IR Acct. Doc. Item
        LF_TEXT                = TEXT-C18.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_AWREF.
*      TEXT-C19 : IR Doc. No.
        LF_TEXT                = TEXT-C19.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_AWORG.
*      TEXT-C20: IR Doc .Year
        LF_TEXT                = TEXT-C20.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_RWCUR.
*      TEXT-C21: IR Doc. Currency
        LF_TEXT                = TEXT-C21.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_KURSF.
*      TEXT-C22: IR Exchange Rate
        LF_TEXT                = TEXT-C22.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_WSL.
*      TEXT-C23: IR Amt in Doc. Currency
        LF_TEXT                = TEXT-C23.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_HSL.
*      TEXT-C24: IR Amt in LC
        LF_TEXT                = TEXT-C24.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-DIFF_HSL.
*      TEXT-C25: Diff Amt in LC
        LF_TEXT                = TEXT-C25.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-MATNR.
*      TEXT-C26: Material
        LF_TEXT                = TEXT-C26.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-BUKRS.
*      TEXT-C27: Comp Code
        LF_TEXT                = TEXT-C27.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-WERKS.
*      TEXT-C28: Plant
        LF_TEXT                = TEXT-C28.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-EBELN.
*      TEXT-C29: PO No.
        LF_TEXT                = TEXT-C29.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-EBELP.
*      TEXT-C30:PO Item
        LF_TEXT                = TEXT-C30.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-LIFNR.
*      TEXT-C31: Supplier of Landed Cost
        LF_TEXT                = TEXT-C31.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-LIFNR_TX.
*      TEXT-C32: Supplier
        LF_TEXT                = TEXT-C32.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-VBELN_IM.
*      TEXT-C33: Inbound Delivery
        LF_TEXT                = TEXT-C33.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-VBELP_IM.
*      TEXT-C34: Inbound Delivery Item
        LF_TEXT                = TEXT-C34.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-ETD.
*      TEXT-C35: ETD
        LF_TEXT                = TEXT-C35.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_RACCT.
*      TEXT-C36: IR Account No.
        LF_TEXT                = TEXT-C36.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN GC_FNAME-IR_RACCT_TX.
*      TEXT-C37: IR Account No. Descr.
        LF_TEXT                = TEXT-C37.
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
*       FORM  F_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE EVENT
**----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '18%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '82%'.

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

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 18,
    LF_COL02 TYPE  I VALUE 35.


* Text-h01 : Report:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-TITLE NO-GAP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_GET_GOODS_RECEIPT
*&---------------------------------------------------------------------*
*& Get Goods Receipt
*&---------------------------------------------------------------------*
FORM F_GET_GOODS_RECEIPT  CHANGING CT_GR TYPE TT_GR.

  CLEAR CT_GR.

  SELECT DOCA~BELNR ,
         DOCA~GJAHR ,
         DOCA~DOCLN ,
         DOCA~AWREF ,
         DOCA~AWORG ,
         DOCA~AWITEM,
         DOCA~KTOSL ,
         DOCA~WSL   ,
         DOCA~RWCUR ,
         DOCA~HSL   ,
         DOCA~RHCUR ,
         DOCA~RBUKRS,
         DOCA~WERKS ,
         DOCA~EBELN ,
         DOCA~EBELP ,
         DOCA~GKONT  AS LIFNR,
         DOCA~BUDAT ,
         EKKO~BSART ,
         DOCA~MATNR,
         CONCAT( DOCA~EBELN, DOCA~EBELP ) AS PO_OBJKEY
  FROM ACDOCA AS DOCA
  INNER JOIN EKPO
          ON DOCA~EBELN = EKPO~EBELN AND
             DOCA~EBELP = EKPO~EBELP
  INNER JOIN EKKO
          ON EKKO~EBELN = EKPO~EBELN
  WHERE RBUKRS     IN @S_BUKRS
    AND EKPO~WERKS IN @S_WERKS
    AND BUDAT      IN @S_BUDAT
    AND EKKO~EBELN IN @S_EBELN
    AND EKPO~MATNR IN @S_MATNR
    AND EKKO~BSART IN @S_BSART
    AND KTOSL      IN @GR_KTOSL
    AND XREVERSED   = @SPACE
    AND XREVERSING  = @SPACE
    AND EKKO~LOEKZ  = @SPACE
    AND DOCA~BTTYPE = @GC_BTTYPE-GR_FOR_PO
    AND RLDNR       = @GC_RLDNR-LEADING_LEDGER
    INTO TABLE @CT_GR.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_FI_DOCUMENT
*&---------------------------------------------------------------------*
*& Get FI Document
*&---------------------------------------------------------------------*
FORM F_GET_FI_DOCUMENT  USING    UT_GR   TYPE TT_GR
                        CHANGING CT_GR_BKPF TYPE TT_GR_BKPF
                                 CT_IR_BKPF TYPE TT_IR_BKPF.

  DATA:
    LT_BKPF_PK  TYPE TT_BKPF_PK,
    LT_XREF1_HD TYPE TT_XREF1_HD.

  CLEAR: CT_GR_BKPF,
         CT_IR_BKPF.

  LT_BKPF_PK  = CORRESPONDING TT_BKPF_PK( UT_GR DISCARDING DUPLICATES
                                          MAPPING BUKRS = RBUKRS
                                                  BELNR = BELNR
                                                  GJAHR = GJAHR ).

  DELETE LT_BKPF_PK WHERE BUKRS IS INITIAL
                      AND BELNR IS INITIAL
                      AND GJAHR IS INITIAL.

  IF LT_BKPF_PK IS INITIAL.
    RETURN.
  ENDIF.

  " Find invoice number with accounting GR
  SELECT BUKRS
         BELNR
         GJAHR
         BKTXT
         KURSF
 INTO TABLE CT_GR_BKPF
 FROM BKPF
 FOR ALL ENTRIES IN LT_BKPF_PK
 WHERE BUKRS = LT_BKPF_PK-BUKRS
   AND BELNR = LT_BKPF_PK-BELNR
   AND GJAHR = LT_BKPF_PK-GJAHR
   AND STBLG = SPACE.

  LT_XREF1_HD = CORRESPONDING #( CT_GR_BKPF DISCARDING DUPLICATES
                                            MAPPING XREF1_HD = BKTXT ).

  IF LT_XREF1_HD IS INITIAL.
    RETURN.
  ENDIF.

  "FInd Accounting IR
  SELECT BUKRS,
         BELNR,
         GJAHR,
         KURSF,
         XREF1_HD,
         WAERS,
         HWAER
  INTO TABLE @CT_IR_BKPF
  FROM BKPF
  FOR ALL ENTRIES IN @LT_XREF1_HD
  WHERE BUKRS     = @LT_XREF1_HD-BUKRS
    AND XREF1_HD  = @LT_XREF1_HD-XREF1_HD
    AND BLART     = @GC_BLART-INVOICE_GROSS
    AND STBLG     = @SPACE
    AND XREF1_HD <> @SPACE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_INVOICE_RECEIPT
*&---------------------------------------------------------------------*
*& Get Invoice Receipt
*&---------------------------------------------------------------------*
FORM F_GET_INVOICE_RECEIPT  USING    UT_GR         TYPE TT_GR
                                     UT_IR_BKPF    TYPE TT_IR_BKPF
                                     UT_RCCT_RANGE TYPE TT_RACCT_RANGE
                            CHANGING CT_IR         TYPE TT_IR.

  DATA:
    LT_EKPO_FILTER TYPE TT_EKPO_PK.

  CLEAR CT_IR.

  IF UT_IR_BKPF IS INITIAL.
    RETURN.
  ENDIF.

  LT_EKPO_FILTER = CORRESPONDING TT_EKPO_PK( UT_GR DISCARDING DUPLICATES ).

  SELECT IR_BKPF~XREF1_HD,
         DOCA~RBUKRS AS BUKRS,
         DOCA~KTOSL  ,
         DOCA~BELNR  ,
         DOCA~GJAHR  ,
         DOCA~DOCLN  ,
         DOCA~AWREF  ,
         DOCA~AWORG  ,
         DOCA~AWITEM ,
         DOCA~WSL    ,
         DOCA~RWCUR  ,
         DOCA~HSL    ,
         DOCA~RHCUR  ,
         DOCA~WERKS  ,
         DOCA~EBELN  ,
         DOCA~EBELP  ,
         RBKP~LIFNR  ,
         DOCA~BUDAT  ,
         DOCA~MATNR  ,
         IR_BKPF~KURSF,
         RSEG~WRBTR,
         DOCA~KTOPL,
         DOCA~RACCT,
         DOCA~DRCRK
FROM ACDOCA AS DOCA
INNER JOIN @UT_IR_BKPF AS IR_BKPF
   ON DOCA~RBUKRS = IR_BKPF~BUKRS AND
      DOCA~BELNR  = IR_BKPF~BELNR AND
      DOCA~GJAHR  = IR_BKPF~GJAHR
INNER JOIN RSEG
        ON RSEG~BELNR = DOCA~AWREF AND
           RSEG~GJAHR = DOCA~AWORG AND
           RSEG~BUZEI = DOCA~AWITEM
INNER JOIN RBKP
        ON RBKP~BELNR = RSEG~BELNR AND
           RBKP~GJAHR = RSEG~GJAHR
WHERE BTTYPE      = @GC_BTTYPE-INVOICE
  AND RLDNR       = @GC_RLDNR-LEADING_LEDGER
  AND XREVERSED   = @SPACE
  AND XREVERSING  = @SPACE
  AND ( KTOSL    IN @GR_KTOSL OR
        ( KTOSL  IN @GR_UNPLAN_KTOSL AND
          GKOAR  IN @GR_UNPLAN_GKOAR ) )
INTO TABLE @DATA(LT_IR).

  IF LT_IR IS INITIAL.
    RETURN.
  ENDIF.

  " Check GenC constant for G/L Account Landed Cost Variance
  IF UT_RCCT_RANGE IS NOT INITIAL.

    " Get Invoice Receipt for G/L Account Landed Cost Variance
    SELECT IR_BKPF~XREF1_HD,
           DOCA~RBUKRS AS BUKRS,
           DOCA~KTOSL  ,
           DOCA~BELNR  ,
           DOCA~GJAHR  ,
           DOCA~DOCLN  ,
           DOCA~AWREF  ,
           DOCA~AWORG  ,
           DOCA~AWITEM ,
           DOCA~WSL    ,
           DOCA~RWCUR  ,
           DOCA~HSL    ,
           DOCA~RHCUR  ,
           DOCA~WERKS  ,
           DOCA~EBELN  ,
           DOCA~EBELP  ,
           RBKP~LIFNR  ,
           DOCA~BUDAT  ,
           DOCA~MATNR  ,
           IR_BKPF~KURSF,
           DOCA~KTOPL,
           DOCA~RACCT,
           DOCA~DRCRK
  FROM ACDOCA AS DOCA
  INNER JOIN @UT_IR_BKPF AS IR_BKPF
     ON DOCA~RBUKRS = IR_BKPF~BUKRS AND
        DOCA~BELNR  = IR_BKPF~BELNR AND
        DOCA~GJAHR  = IR_BKPF~GJAHR
  INNER JOIN RBKP
          ON RBKP~BELNR = DOCA~AWREF AND
             RBKP~GJAHR = DOCA~AWORG
  WHERE BTTYPE      = @GC_BTTYPE-INVOICE
    AND RLDNR       = @GC_RLDNR-LEADING_LEDGER
    AND XREVERSED   = @SPACE
    AND XREVERSING  = @SPACE
    AND DOCA~RACCT IN @UT_RCCT_RANGE
  INTO TABLE @DATA(LT_IR_LAND_COST).

  ENDIF.

  CT_IR = CORRESPONDING #(
                 FILTER #( LT_IR IN LT_EKPO_FILTER
                           WHERE EBELN = EBELN
                             AND EBELP = EBELP ) ).

  CT_IR = CORRESPONDING #( BASE ( CT_IR ) LT_IR_LAND_COST ).


  SORT CT_IR BY XREF1_HD
                BUKRS
                EBELN
                EBELP
                KTOSL.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GET_GL_ACCT_TEXT
*&---------------------------------------------------------------------*
*       Get G/L Account Description
*----------------------------------------------------------------------*
FORM F_GET_ACCT_TEXT USING UF_LANGU    TYPE SY-LANGU
                           UT_SKA1_KEY TYPE TT_SKA1_KEY
                  CHANGING CT_SKAT     TYPE TT_SKAT.

  CLEAR: CT_SKAT.

  IF UT_SKA1_KEY IS INITIAL.
    RETURN.
  ENDIF.

  "G/L Account Description
  SELECT FROM SKAT
      FIELDS KTOPL,
             SAKNR,
             TXT20
      FOR ALL ENTRIES IN @UT_SKA1_KEY
       WHERE KTOPL = @UT_SKA1_KEY-KTOPL
         AND SAKNR = @UT_SKA1_KEY-SAKNR
         AND SPRAS = @UF_LANGU
        INTO TABLE @CT_SKAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MD_VENDOR
*&---------------------------------------------------------------------*
*& Get vendor master data
*&---------------------------------------------------------------------*
FORM F_GET_MD_VENDOR  USING    UT_GR   TYPE TT_GR
                               UT_IR   TYPE TT_IR
                      CHANGING CT_LFA1 TYPE TT_LFA1.

  DATA:
    LT_LFA1    TYPE TT_LFA1,
    LS_LFA1    TYPE TS_LFA1,

    LS_LFA1_PK TYPE TS_LFA1_PK,
    LT_LFA1_PK TYPE TT_LFA1_PK.

  CLEAR CT_LFA1.

  LT_LFA1_PK = CORRESPONDING TT_LFA1_PK( UT_GR DISCARDING DUPLICATES ).
  LT_LFA1_PK = CORRESPONDING TT_LFA1_PK( BASE ( LT_LFA1_PK ) UT_IR DISCARDING DUPLICATES ).

  DELETE LT_LFA1_PK WHERE LIFNR IS INITIAL.

  LOOP AT LT_LFA1_PK INTO LS_LFA1_PK.
    CLEAR LS_LFA1.
    LS_LFA1-LIFNR = |{ LS_LFA1_PK-LIFNR ALPHA = IN }|.

    INSERT LS_LFA1 INTO TABLE LT_LFA1.

  ENDLOOP.

  SELECT LFA1~LIFNR,
         CONCAT( CONCAT( NAME1, NAME2 ) ,
                 CONCAT( NAME3, NAME4 ) ) AS NAME
  FROM @LT_LFA1 AS KEY
  INNER JOIN LFA1
          ON LFA1~LIFNR = KEY~LIFNR
  INTO TABLE @CT_LFA1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MD_TRANSACTION
*&---------------------------------------------------------------------*
*& Get transaction text
*&---------------------------------------------------------------------*
FORM F_GET_MD_TRANSACTION    USING UT_KTOSL  TYPE TT_KTOSL
                          CHANGING CT_T030W  TYPE TT_T030W.

  CLEAR CT_T030W.

  IF UT_KTOSL IS INITIAL.
    RETURN.
  ENDIF.

  SELECT KAPPL,
         SPRAS,
         KVSL1 AS KTOSL,
         VTEXT AS LTEXT
  FROM T687T
  FOR ALL ENTRIES IN @UT_KTOSL
  WHERE SPRAS = @SY-LANGU
    AND KAPPL = @GC_KAPPL-PURCHASING
    AND KVSL1 = @UT_KTOSL-KTOSL
  ORDER BY PRIMARY KEY
   INTO TABLE @DATA(LT_T687T).

  CT_T030W = CORRESPONDING #( LT_T687T DISCARDING DUPLICATES ).

  SELECT SPRAS,
         KTOSL,
         LTEXT
  FROM T030W
  FOR ALL ENTRIES IN @UT_KTOSL
  WHERE SPRAS = @SY-LANGU
    AND KTOSL = @UT_KTOSL-KTOSL
  ORDER BY PRIMARY KEY
   INTO TABLE @DATA(LT_T030W).

  LOOP AT LT_T030W ASSIGNING FIELD-SYMBOL(<L_T003W_UPD>).

    READ TABLE CT_T030W ASSIGNING FIELD-SYMBOL(<L_T003W>)
    WITH KEY KTOSL = <L_T003W_UPD>-KTOSL.
    IF SY-SUBRC IS INITIAL.

      IF <L_T003W>-LTEXT IS INITIAL.
        <L_T003W>-LTEXT = <L_T003W_UPD>-LTEXT.
      ENDIF.

    ELSE.
      INSERT VALUE #( KTOSL = <L_T003W_UPD>-KTOSL
                      LTEXT = <L_T003W_UPD>-LTEXT )
        INTO TABLE CT_T030W.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_MATERIAL_DOCUMENT
*&---------------------------------------------------------------------*
*& Get material document
*&---------------------------------------------------------------------*
FORM F_GET_MATERIAL_DOCUMENT  USING    UT_GR    TYPE TT_GR
                              CHANGING CT_MSEG  TYPE TT_MSEG.

  DATA:
    LT_MSEG_PK TYPE TT_MSEG_PK.

  CLEAR CT_MSEG.

  LT_MSEG_PK = CORRESPONDING TT_MSEG_PK( UT_GR DISCARDING DUPLICATES
                                         MAPPING MBLNR = AWREF
                                                 MJAHR = AWORG
                                                 ZEILE = AWITEM ).

  DELETE LT_MSEG_PK WHERE MBLNR IS INITIAL
                      AND MJAHR IS INITIAL
                      AND ZEILE IS INITIAL.

  IF LT_MSEG_PK IS INITIAL.
    RETURN.
  ENDIF.

  SELECT MBLNR
         MJAHR
         ZEILE
         VBELN_IM
         VBELP_IM
  FROM MSEG
  INTO TABLE CT_MSEG
  FOR ALL ENTRIES IN LT_MSEG_PK
  WHERE MBLNR = LT_MSEG_PK-MBLNR
    AND MJAHR = LT_MSEG_PK-MJAHR
    AND ZEILE = LT_MSEG_PK-ZEILE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_SCHEDULE_LINES
*&---------------------------------------------------------------------*
*& Get Schedule lines
*&---------------------------------------------------------------------*
FORM F_GET_SCHEDULE_LINES  USING    UT_GR   TYPE TT_GR
                           CHANGING CT_EKET TYPE TT_EKET.

  DATA:
     LT_EKPO_PK TYPE TT_EKPO_PK.

  CLEAR CT_EKET.

  LT_EKPO_PK = CORRESPONDING TT_EKPO_PK( UT_GR DISCARDING DUPLICATES
                                         MAPPING EBELN = EBELN
                                                 EBELP = EBELP ).

  DELETE LT_EKPO_PK WHERE EBELN IS INITIAL
                      AND EBELP IS INITIAL.

  IF LT_EKPO_PK IS INITIAL.
    RETURN.
  ENDIF.

  SELECT EKET~EBELN,
         EKET~EBELP,
         EKET~ETENR,
         EKET~EINDT
  FROM @LT_EKPO_PK AS KEY
  INNER JOIN EKET
    ON EKET~EBELN = KEY~EBELN AND
       EKET~EBELP = KEY~EBELP
  INTO TABLE @CT_EKET.

  SORT CT_EKET BY EBELN EBELP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_COLLECT_RESULT
*&---------------------------------------------------------------------*
*& Collect result
*&---------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING UT_GR          TYPE TT_GR
                             UT_GR_BKPF     TYPE TT_GR_BKPF
                             UT_MSEG        TYPE TT_MSEG
                             UT_LFA1        TYPE TT_LFA1
                             UT_T030W       TYPE TT_T030W
                             UT_EKET        TYPE TT_EKET
                             UT_SKAT        TYPE TT_SKAT
                             UT_RACCT_RANGE TYPE TT_RACCT_RANGE
                    CHANGING CT_IR          TYPE TT_IR
                             CT_RESULT      TYPE TT_RESULT.

  DATA:
    LS_RESULT      TYPE TS_RESULT,
    LT_IR_RESULT   TYPE TT_RESULT,
    LT_GR_XREF1_HD TYPE TT_XREF1_HD,
    LF_EINDT       TYPE TS_EKET-EINDT,
    LF_XREF1_HD    TYPE TS_IR-XREF1_HD,
    LF_TABIX       TYPE SY-TABIX.

  FIELD-SYMBOLS:
    <L_IR>     TYPE TS_IR,
    <L_RESULT> TYPE TS_RESULT.

  LOOP AT UT_GR INTO DATA(LS_GROUP_GR) GROUP BY LS_GROUP_GR-PO_OBJKEY.

    CLEAR: LF_EINDT,
           LT_IR_RESULT,
           LT_GR_XREF1_HD.

    PERFORM F_READ_SCHEDULE_LINES USING LS_GROUP_GR-EBELN
                                        LS_GROUP_GR-EBELP
                                        UT_EKET
                               CHANGING LF_EINDT.

    LOOP AT GROUP LS_GROUP_GR INTO DATA(LS_GR).

      CLEAR: LF_XREF1_HD,
             LS_RESULT.

      LS_RESULT = CORRESPONDING #( LS_GR
                                   MAPPING GR_BUDAT  = BUDAT
                                           GR_BELNR  = BELNR
                                           GR_GJAHR  = GJAHR
                                           GR_DOCLN  = DOCLN
                                           GR_AWREF  = AWREF
                                           GR_AWORG  = AWORG
                                           GR_AWITEM = AWITEM
                                           GR_RWCUR  = RWCUR
                                           GR_RHCUR  = RHCUR
                                           BUKRS     = RBUKRS
                                  ).

      LS_RESULT-GR_WSL = ABS( LS_GR-WSL ).
      LS_RESULT-GR_HSL = ABS( LS_GR-HSL ).

      PERFORM F_READ_TRANSACTION_TEXT USING LS_RESULT-KTOSL
                                            UT_T030W
                                   CHANGING LS_RESULT-KTOSL_TX.

      PERFORM F_CALC_ETD_DATE USING LS_RESULT-LIFNR
                                    LS_GROUP_GR-BSART
                                    LF_EINDT
                           CHANGING LS_RESULT-ETD.

      READ TABLE UT_MSEG INTO DATA(LS_MSEG)
      WITH KEY MBLNR = LS_RESULT-GR_AWREF
               MJAHR = CONV #( LS_RESULT-GR_AWORG )
               ZEILE = CONV #( LS_RESULT-GR_AWITEM ).
      IF SY-SUBRC IS INITIAL."If data is found
        LS_RESULT-VBELN_IM =  LS_MSEG-VBELN_IM.
        LS_RESULT-VBELP_IM =  LS_MSEG-VBELP_IM.
      ENDIF. "End read table UT_MSEG

      READ TABLE UT_GR_BKPF INTO DATA(LS_GR_BKPF)
      WITH KEY BUKRS = LS_GR-RBUKRS
               BELNR = LS_GR-BELNR
               GJAHR = LS_GR-GJAHR.
      IF SY-SUBRC IS INITIAL. "If data is found

        LS_RESULT-GR_KURSF = LS_GR_BKPF-KURSF.
        LS_RESULT-GR_BKTXT = LS_GR_BKPF-BKTXT.

        LF_XREF1_HD = LS_GR_BKPF-BKTXT.

        INSERT VALUE #( BUKRS    = LS_GR_BKPF-BUKRS
                        XREF1_HD = LF_XREF1_HD )
          INTO TABLE LT_GR_XREF1_HD.

*        " Find Accounting IR with invoice no.
        READ TABLE CT_IR ASSIGNING <L_IR>
        WITH KEY XREF1_HD = LF_XREF1_HD
                 BUKRS    = LS_GR_BKPF-BUKRS
                 EBELN    = LS_GR-EBELN
                 EBELP    = LS_GR-EBELP
                 KTOSL    = LS_GR-KTOSL
        BINARY SEARCH.
        IF SY-SUBRC IS INITIAL. "If data is found

          LS_RESULT-IR_BUDAT  = <L_IR>-BUDAT.
          LS_RESULT-IR_BELNR  = <L_IR>-BELNR.
          LS_RESULT-IR_GJAHR  = <L_IR>-GJAHR.
          LS_RESULT-IR_DOCLN  = <L_IR>-DOCLN.
          LS_RESULT-IR_AWREF  = <L_IR>-AWREF.
          LS_RESULT-IR_AWORG  = <L_IR>-AWORG.
          LS_RESULT-IR_RWCUR  = <L_IR>-RWCUR.
          LS_RESULT-IR_WSL    = <L_IR>-WRBTR.
          LS_RESULT-IR_HSL    = <L_IR>-WRBTR * <L_IR>-KURSF. "Calculate amount in LC currency
          LS_RESULT-IR_RHCUR  = <L_IR>-RHCUR.
          LS_RESULT-IR_KURSF  = <L_IR>-KURSF.
          LS_RESULT-LIFNR     = <L_IR>-LIFNR.
          LS_RESULT-DIFF_HSL  = LS_RESULT-IR_HSL - LS_RESULT-GR_HSL.
          LS_RESULT-IR_RACCT   = <L_IR>-RACCT.

          "G/L Account Text
          READ TABLE UT_SKAT ASSIGNING FIELD-SYMBOL(<L_SKAT>)
          WITH KEY KTOPL = <L_IR>-KTOPL
                   SAKNR = <L_IR>-RACCT.
          IF SY-SUBRC IS INITIAL.
            LS_RESULT-IR_RACCT_TX = <L_SKAT>-TXT20.
          ENDIF.

          <L_IR>-X_KTOSL       = GC_TRUE.

        ENDIF.

      ENDIF. "End read table UT_GR_BKPF

      PERFORM F_READ_VENDOR_NAME USING LS_RESULT-LIFNR
                                       UT_LFA1
                              CHANGING LS_RESULT-LIFNR
                                       LS_RESULT-LIFNR_TX.

      APPEND LS_RESULT TO CT_RESULT.

    ENDLOOP.

    DELETE CT_IR WHERE X_KTOSL IS NOT INITIAL.

    LOOP AT LT_GR_XREF1_HD ASSIGNING FIELD-SYMBOL(<L_XREF1_HD>).

      " Find Accounting IR with invoice no.
      READ TABLE CT_IR
      TRANSPORTING NO FIELDS
      WITH KEY XREF1_HD = <L_XREF1_HD>-XREF1_HD
               BUKRS    = <L_XREF1_HD>-BUKRS
      BINARY SEARCH.
      IF SY-SUBRC IS INITIAL. "If data is found

        LF_TABIX = SY-TABIX.

        LOOP AT CT_IR ASSIGNING <L_IR> FROM LF_TABIX.

          IF <L_IR>-XREF1_HD = <L_XREF1_HD>-XREF1_HD AND
             <L_IR>-BUKRS    = <L_XREF1_HD>-BUKRS .

            DO 1 TIMES.

              "Check if transaction not match
              IF <L_IR>-X_KTOSL = GC_TRUE.
                EXIT.
              ENDIF.

              IF ( <L_IR>-EBELN  = LS_GROUP_GR-EBELN AND
                   <L_IR>-EBELP  = LS_GROUP_GR-EBELP )
              OR ( <L_IR>-RACCT IN UT_RACCT_RANGE ).

                <L_IR>-X_KTOSL   = GC_TRUE.

                APPEND INITIAL LINE TO LT_IR_RESULT ASSIGNING <L_RESULT>.
                <L_RESULT>-GR_BKTXT = <L_XREF1_HD>-XREF1_HD.
                <L_RESULT>-KTOSL    = <L_IR>-KTOSL.
                <L_RESULT>-BUKRS    = <L_IR>-BUKRS.
                <L_RESULT>-MATNR    = <L_IR>-MATNR.

                <L_RESULT>-WERKS    = COND #( WHEN <L_IR>-WERKS IS NOT INITIAL
                                                THEN <L_IR>-WERKS
                                              ELSE LS_GROUP_GR-WERKS ).

                <L_RESULT>-EBELN    = COND #( WHEN <L_IR>-EBELN IS NOT INITIAL
                                                THEN <L_IR>-EBELN
                                              ELSE LS_GROUP_GR-EBELN ).

                <L_RESULT>-EBELP    = <L_IR>-EBELP.
                <L_RESULT>-LIFNR    = <L_IR>-LIFNR.

                <L_RESULT>-IR_RACCT  = <L_IR>-RACCT.
                <L_RESULT>-IR_BUDAT  = <L_IR>-BUDAT.
                <L_RESULT>-IR_BELNR  = <L_IR>-BELNR.
                <L_RESULT>-IR_GJAHR  = <L_IR>-GJAHR.
                <L_RESULT>-IR_DOCLN  = <L_IR>-DOCLN.
                <L_RESULT>-IR_AWREF  = <L_IR>-AWREF.
                <L_RESULT>-IR_AWORG  = <L_IR>-AWORG.
                <L_RESULT>-IR_RWCUR  = <L_IR>-RWCUR.
                <L_RESULT>-IR_WSL    = <L_IR>-WSL.
                <L_RESULT>-IR_HSL    = <L_IR>-HSL.
                <L_RESULT>-IR_RHCUR  = <L_IR>-RHCUR.
                <L_RESULT>-IR_KURSF  = <L_IR>-KURSF.

                CASE <L_IR>-DRCRK.
                  WHEN GC_DRCRK-CREDIT.
                    <L_RESULT>-IR_WSL = ABS( <L_RESULT>-IR_WSL ) * -1.
                    <L_RESULT>-IR_HSL = ABS( <L_RESULT>-IR_HSL ) * -1.
                ENDCASE.

                <L_RESULT>-DIFF_HSL = <L_RESULT>-IR_HSL.

                PERFORM F_READ_TRANSACTION_TEXT USING <L_RESULT>-KTOSL
                                                      UT_T030W
                                             CHANGING <L_RESULT>-KTOSL_TX.

                PERFORM F_READ_VENDOR_NAME USING <L_RESULT>-LIFNR
                                                 UT_LFA1
                                        CHANGING <L_RESULT>-LIFNR
                                                 <L_RESULT>-LIFNR_TX.

                PERFORM F_CALC_ETD_DATE USING <L_RESULT>-LIFNR
                                              LS_GROUP_GR-BSART
                                              LF_EINDT
                                     CHANGING <L_RESULT>-ETD.

                "G/L Account Text
                READ TABLE UT_SKAT ASSIGNING <L_SKAT>
                WITH KEY KTOPL = <L_IR>-KTOPL
                         SAKNR = <L_IR>-RACCT.
                IF SY-SUBRC IS INITIAL.
                  <L_RESULT>-IR_RACCT_TX = <L_SKAT>-TXT20.
                ENDIF.

              ENDIF.

            ENDDO.

          ELSE.
            EXIT.
          ENDIF.

        ENDLOOP.

      ENDIF. " End read table CT_IR (IR transaction)

    ENDLOOP.

    APPEND LINES OF LT_IR_RESULT TO CT_RESULT.

*    " Find IR with different transaction
*    READ TABLE UT_GR_BKPF INTO LS_GR_BKPF
*    WITH KEY BUKRS = LS_GROUP_GR-RBUKRS
*             BELNR = LS_GROUP_GR-BELNR
*             GJAHR = LS_GROUP_GR-GJAHR.
*    IF SY-SUBRC IS INITIAL. "If data is found
*
*      LF_XREF1_HD =  LS_GR_BKPF-BKTXT.
*
*      " Find Accounting IR with invoice no.
*      READ TABLE CT_IR
*      TRANSPORTING NO FIELDS
*      WITH KEY XREF1_HD = LF_XREF1_HD
*               BUKRS    = LS_GR_BKPF-BUKRS
*      BINARY SEARCH.
*      IF SY-SUBRC IS INITIAL. "If data is found
*
*        LF_TABIX = SY-TABIX.
*
*        LOOP AT CT_IR ASSIGNING <L_IR> FROM LF_TABIX.
*
*          IF <L_IR>-XREF1_HD = LF_XREF1_HD        AND
*             <L_IR>-BUKRS    = LS_GR_BKPF-BUKRS.
*
*            DO 1 TIMES.
*
*              "Check if transaction not match
*              IF <L_IR>-X_KTOSL = GC_TRUE.
*                EXIT.
*              ENDIF.
*
*              IF ( <L_IR>-EBELN  = LS_GROUP_GR-EBELN AND
*                   <L_IR>-EBELP  = LS_GROUP_GR-EBELP )
*              OR ( <L_IR>-RACCT IN UT_RACCT_RANGE ).
*
*                <L_IR>-X_KTOSL   = GC_TRUE.
*
*                APPEND INITIAL LINE TO LT_IR_RESULT ASSIGNING <L_RESULT>.
*                <L_RESULT>-GR_BKTXT = LS_RESULT-GR_BKTXT.
*                <L_RESULT>-KTOSL    = <L_IR>-KTOSL.
*                <L_RESULT>-BUKRS    = <L_IR>-BUKRS.
*                <L_RESULT>-WERKS    = <L_IR>-WERKS.
*                <L_RESULT>-MATNR    = <L_IR>-MATNR.
*                <L_RESULT>-EBELN    = <L_IR>-EBELN.
*                <L_RESULT>-EBELP    = <L_IR>-EBELP.
*                <L_RESULT>-LIFNR    = <L_IR>-LIFNR.
*
*                <L_RESULT>-IR_RACCT  = <L_IR>-RACCT.
*                <L_RESULT>-IR_BUDAT  = <L_IR>-BUDAT.
*                <L_RESULT>-IR_BELNR  = <L_IR>-BELNR.
*                <L_RESULT>-IR_GJAHR  = <L_IR>-GJAHR.
*                <L_RESULT>-IR_DOCLN  = <L_IR>-DOCLN.
*                <L_RESULT>-IR_AWREF  = <L_IR>-AWREF.
*                <L_RESULT>-IR_AWORG  = <L_IR>-AWORG.
*                <L_RESULT>-IR_RWCUR  = <L_IR>-RWCUR.
*                <L_RESULT>-IR_WSL    = <L_IR>-WSL.
*                <L_RESULT>-IR_HSL    = <L_IR>-HSL.
*                <L_RESULT>-IR_RHCUR  = <L_IR>-RHCUR.
*                <L_RESULT>-IR_KURSF  = <L_IR>-KURSF.
*
*                CASE <L_IR>-DRCRK.
*                  WHEN GC_DRCRK-CREDIT.
*                    <L_RESULT>-IR_WSL = <L_RESULT>-IR_WSL * -1.
*                    <L_RESULT>-IR_HSL = <L_RESULT>-IR_HSL * -1.
*                ENDCASE.
*
*                <L_RESULT>-DIFF_HSL = <L_RESULT>-IR_HSL.
*
*                PERFORM F_READ_TRANSACTION_TEXT USING <L_RESULT>-KTOSL
*                                                      UT_T030W
*                                             CHANGING <L_RESULT>-KTOSL_TX.
*
*                PERFORM F_READ_VENDOR_NAME USING <L_RESULT>-LIFNR
*                                                 UT_LFA1
*                                        CHANGING <L_RESULT>-LIFNR
*                                                 <L_RESULT>-LIFNR_TX.
*
*                PERFORM F_CALC_ETD_DATE USING <L_RESULT>-LIFNR
*                                              LS_GROUP_GR-BSART
*                                              LF_EINDT
*                                     CHANGING <L_RESULT>-ETD.
*
*                "G/L Account Text
*                READ TABLE UT_SKAT ASSIGNING <L_SKAT>
*                WITH KEY KTOPL = <L_IR>-KTOPL
*                         SAKNR = <L_IR>-RACCT.
*                IF SY-SUBRC IS INITIAL.
*                  <L_RESULT>-IR_RACCT_TX = <L_SKAT>-TXT20.
*                ENDIF.
*
*              ENDIF.
*
*            ENDDO.
*
*          ELSE.
*            EXIT.
*          ENDIF.
*
*        ENDLOOP.
*
*      ENDIF. " End read table CT_IR (IR transaction)
*
*      APPEND LINES OF LT_IR_RESULT TO CT_RESULT.
*
*    ENDIF. "End read table UT_GR_BKPF


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_VENDOR_NAME
*&---------------------------------------------------------------------*
*& Read Vendor name
*&---------------------------------------------------------------------*
FORM F_READ_VENDOR_NAME  USING    UF_LIFNR    TYPE TS_LFA1_PK-LIFNR
                                  UT_LFA1     TYPE TT_LFA1
                         CHANGING CF_LIFNR    TYPE TS_LFA1_PK-LIFNR
                                  CF_LIFNR_TX TYPE TS_RESULT-LIFNR_TX.

  DATA LF_LIFNR TYPE TS_LFA1_PK-LIFNR.

  CLEAR CF_LIFNR_TX.

  LF_LIFNR = |{ UF_LIFNR ALPHA = IN }|.

  READ TABLE UT_LFA1
  INTO DATA(LS_LFA1)
  WITH KEY LIFNR = LF_LIFNR.
  IF SY-SUBRC IS INITIAL.
    CF_LIFNR_TX = LS_LFA1-NAME.
    CONDENSE CF_LIFNR_TX.
  ELSE.
    CLEAR CF_LIFNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_TRANSACTION_TEXT
*&---------------------------------------------------------------------*
*& Read Transaction text
*&---------------------------------------------------------------------*
FORM F_READ_TRANSACTION_TEXT  USING   UF_KTOSL    TYPE TS_T030W-KTOSL
                                      UT_T687T    TYPE TT_T030W
                             CHANGING CF_KTOSL_TX TYPE TS_T030W-LTEXT.

  CLEAR CF_KTOSL_TX.

  CF_KTOSL_TX = VALUE #( UT_T687T[ KTOSL = UF_KTOSL ]-LTEXT OPTIONAL ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CALC_ETD_DATE
*&---------------------------------------------------------------------*
*& Calculate ETD Date from delivery date
*&---------------------------------------------------------------------*
FORM F_CALC_ETD_DATE  USING    UF_LIFNR    TYPE TS_LFA1-LIFNR
                               UF_BSART    TYPE EKKO-BSART
                               UF_EINDT    TYPE TS_EKET-EINDT
                      CHANGING CF_ETD      TYPE TS_RESULT-ETD.

  CLEAR CF_ETD.

  IF UF_EINDT IS INITIAL.
    RETURN.
  ENDIF.

  CF_ETD = UF_EINDT.

  IF UF_BSART IN GR_ETD_BSART.

    READ TABLE GT_ETD_DAYS
    INTO DATA(LS_ETD_DAYS)
    WITH KEY LIFNR = UF_LIFNR
    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      CF_ETD = UF_EINDT - LS_ETD_DAYS-DAYS.
    ELSE.
      CF_ETD = UF_EINDT - GF_ETD_DAYS_DEFAULT.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_SCHEDULE_LINES
*&---------------------------------------------------------------------*
*& Read schedule lines
*&---------------------------------------------------------------------*
FORM F_READ_SCHEDULE_LINES  USING    UF_EBELN TYPE TS_EKPO_PK-EBELN
                                     UF_EBELP TYPE TS_EKPO_PK-EBELP
                                     UT_EKET  TYPE TT_EKET
                            CHANGING CF_EINDT TYPE TS_EKET-EINDT.

  CLEAR: CF_EINDT.

  READ TABLE UT_EKET
  INTO DATA(LS_EKET)
  WITH KEY EBELN = UF_EBELN
           EBELP = UF_EBELP
  BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    CF_EINDT = LS_EKET-EINDT.
  ENDIF.

ENDFORM.
