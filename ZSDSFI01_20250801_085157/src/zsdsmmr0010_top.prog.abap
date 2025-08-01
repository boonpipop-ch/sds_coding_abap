*&---------------------------------------------------------------------*
*& Include          ZRTMMR0010_TOP
*&---------------------------------------------------------------------*
TABLES : MARA,
         MARD,
         SSCRFIELDS.

TYPE-POOLS SLIS.
*&-----------------------------------------------------------------------------------*
*& T Y P E S
*&-----------------------------------------------------------------------------------*
TYPES : BEGIN OF TY_MAT,  " material\ data
        MATNR LIKE MARA-MATNR,
        MATKL LIKE MARA-MATKL,
        MEINS LIKE MARA-MEINS,
        PRDHA LIKE MARA-PRDHA,
        MFRNR LIKE MARA-MFRNR,
        MAKTX LIKE MAKT-MAKTX,
        MEINH LIKE MARM-MEINH,
        VOLUM LIKE MARM-VOLUM,
END OF TY_MAT.

TYPES : BEGIN OF TY_VENDOR,  " material\ data
        LIFNR LIKE LFA1-LIFNR,
        NAME1 LIKE LFA1-NAME1,
END OF TY_VENDOR.



TYPES : BEGIN OF TY_CSTK,
        MATNR LIKE MARA-MATNR,
        WERKS LIKE MARD-WERKS,
        LGORT LIKE MARD-LGORT,
        LABST LIKE MARD-LABST,
        SPEME LIKE MARD-SPEME,
        VERPR LIKE MBEW-VERPR,
        SALK3 LIKE MBEW-SALK3,
        PEINH LIKE MBEW-PEINH,
        LBKUM LIKE MBEW-LBKUM,
        LGOBE LIKE T001L-LGOBE,
  END OF TY_CSTK.

TYPES : BEGIN OF TY_CSTK2,
        MATNR LIKE MARA-MATNR,
        WERKS LIKE MARD-WERKS,
        LGORT LIKE MARD-LGORT,
        LABST LIKE MARD-LABST,
        SPEME LIKE MARD-SPEME,
        VERPR LIKE MBEW-VERPR,
        SALK3 LIKE MBEW-SALK3,
        PEINH LIKE MBEW-PEINH,
        LBKUM LIKE MBEW-LBKUM,
        LGOBE LIKE T001L-LGOBE,
        LFMON LIKE MBEW-LFMON,
        LFGJA LIKE MBEW-LFGJA, " Add on 30.12.2014
  END OF TY_CSTK2.


TYPES : BEGIN OF TY_MSEG ,
          MBLNR LIKE MKPF-MBLNR,
          ZEILE LIKE MSEG-ZEILE,
          BUDAT LIKE MKPF-BUDAT,
          BWART LIKE MSEG-BWART,
          WERKS LIKE MSEG-WERKS,
          LGORT LIKE MSEG-LGORT,
          MATNR LIKE MSEG-MATNR,
          MENGE LIKE MSEG-MENGE,
          SHKZG LIKE MSEG-SHKZG,
          DMBTR LIKE MSEG-DMBTR,
END OF TY_MSEG.

TYPES : BEGIN OF TY_MDOC ,
          WERKS LIKE MSEG-WERKS,
          LGORT LIKE MSEG-LGORT,
          MATNR LIKE MSEG-MATNR,
          MENGE LIKE MSEG-MENGE,
          DMBTR LIKE MSEG-DMBTR,
END OF TY_MDOC.

TYPES : BEGIN OF TY_FI_DOC,
          MATNR LIKE BSIM-MATNR,
          BWKEY LIKE BSIM-BWKEY,
          BELNR LIKE BSIM-BELNR,
          GJAHR LIKE BSIM-GJAHR,
          BUZEI LIKE BSIM-BUZEI,
          SHKZG LIKE BSIM-SHKZG,
          DMBTR LIKE BSIM-DMBTR,
          BUDAT LIKE BSIM-BUDAT,
          BLART LIKE BSIM-BLART,
  END OF TY_FI_DOC.

TYPES : BEGIN OF TY_FI_X,
          BELNR LIKE BSEG-BELNR,
          GJAHR LIKE BSEG-GJAHR,
          BUZEI LIKE BSEG-BUZEI,
          XREF3 LIKE BSEG-XREF3,
          EBELN  LIKE BSEG-EBELN,
          EBELP  LIKE BSEG-EBELP,
          GJAHR1 LIKE MSEG-GJAHR,
          MBLNR  LIKE MSEG-MBLNR,
          ZEILE  LIKE MSEG-ZEILE,
   END OF TY_FI_X.

TYPES : BEGIN OF TY_FI_A,
          MATNR LIKE MARA-MATNR,
          WERKS LIKE MSEG-WERKS,
          LGORT LIKE MSEG-LGORT,
          DMBTR LIKE BSIM-DMBTR,
  END OF TY_FI_A.

*

TYPES : BEGIN OF TY_RESULT,
        MATNR LIKE MARA-MATNR,
        CATE(5),                 " Category
        CLASS(3),                " Class
        SCLASS(8),
        MFRNR LIKE MARA-MFRNR,
        NAME1 LIKE LFA1-NAME1,
        LGORT LIKE MARD-LGORT,
        LGOBE LIKE T001L-LGOBE,
        BQTY  LIKE MSEG-MENGE,
        BVAL  LIKE MSEG-DMBTR,
        RQTY  LIKE MSEG-MENGE,
        RVAL  LIKE MSEG-DMBTR,
        SQTY  LIKE MSEG-MENGE,
        SVAL  LIKE MSEG-DMBTR,
        AQTY  LIKE MSEG-MENGE,
        AVAL  LIKE MSEG-DMBTR,
        EQTY  LIKE MSEG-MENGE,
        EVAL  LIKE MSEG-DMBTR,
        PUNT  LIKE MSEG-DMBTR,
        MEINS LIKE MARA-MEINS,
        MAKTX LIKE MAKT-MAKTX,
        TYPE(1),
        PREIS TYPE F,
        VOLUM  LIKE MARM-VOLUM,
        T_VOLUM LIKE MARM-VOLUM,
        ATWRT  LIKE AUSP-ATWRT,
END OF TY_RESULT.

*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
DATA : GT_MAT TYPE TY_MAT OCCURS 0 WITH HEADER LINE,
       GT_CSTK TYPE TY_CSTK OCCURS 0 WITH HEADER LINE,
       GT_CSTK3 TYPE TY_CSTK OCCURS 0 WITH HEADER LINE,
       GT_CSTK2 TYPE TY_CSTK2 OCCURS 0 WITH HEADER LINE,
       GT_MSEG TYPE TY_MSEG OCCURS 0 WITH HEADER LINE,
       GT_MDOC TYPE TY_MDOC OCCURS 0 WITH HEADER LINE,
       GT_VENDOR TYPE TY_VENDOR OCCURS 0 WITH HEADER LINE,
       GT_RESULT TYPE TY_RESULT OCCURS 0 WITH HEADER LINE,
       GT_GENC TYPE ZSDSCAC002 OCCURS 0 WITH HEADER LINE,
       GT_FI_DOC TYPE TY_FI_DOC OCCURS 0 WITH HEADER LINE,
       GT_FI_X TYPE TY_FI_X OCCURS 0 WITH HEADER LINE,
       GT_FI_A TYPE TY_FI_A OCCURS 0 WITH HEADER LINE.

DATA: GS_FIELDCAT   TYPE    SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT     TYPE    SLIS_LAYOUT_ALV,
      GT_EVENTS     TYPE    SLIS_T_EVENT,
      GT_SORT       TYPE    SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GT_FILT       TYPE    SLIS_T_FILTER_ALV WITH HEADER LINE,
      GT_PRINT      TYPE    SLIS_PRINT_ALV,
      GW_COUNT TYPE I,
      GW_PAGE  TYPE I,
      GW_PAGE_PRINT(16).

DATA : GW_START_OF_LIST(1).
*&-----------------------------------------------------------------------------------*
*& Constant
*&-----------------------------------------------------------------------------------*
CONSTANTS : GC_MVT_N TYPE ZSDSDE_CONST VALUE 'MVT_N_*',
            GC_MVT_R TYPE ZSDSDE_CONST VALUE 'MVT_R_*',
            GC_MVT_I TYPE ZSDSDE_CONST VALUE 'MVT_I_*',
            GC_MVT_M TYPE ZSDSDE_CONST VALUE 'MVT_M_*',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
            GC_TOP_OF_LIST TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST'.

*&-----------------------------------------------------------------------------------*
*& Ranges
*&-----------------------------------------------------------------------------------*
RANGES : R_MVT_N FOR MSEG-BWART,
         R_MVT_R FOR MSEG-BWART,
         R_MVT_M FOR MSEG-BWART,
         R_MVT_I FOR MSEG-BWART.
