*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0140_TOP
*&---------------------------------------------------------------------*
***INCLUDE RVADTABL .
DATA: GS_DATA     TYPE  ZSDSFIS005,
      GT_DATA_TAB TYPE  ZSDSFIS006_TT.

DATA: RETCODE      LIKE SY-SUBRC.      "Returncode
DATA: XSCREEN(1)   TYPE C.             "Output on printer or screen
DATA: REPEAT(1)    TYPE C.
DATA: NAST_ANZAL   LIKE NAST-ANZAL.    "Number of outputs (Orig. + Cop.)
DATA: NAST_TDARMOD LIKE NAST-TDARMOD.  "Archiving only one time

* current language for read buffered.
DATA: GF_LANGUAGE LIKE SY-LANGU.

DATA: GCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

CONSTANTS : BEGIN OF GC_CON,
              I             TYPE C LENGTH 1     VALUE 'I',
              PARTN_ROLE    TYPE C LENGTH 2     VALUE 'VE',
              AG            TYPE C LENGTH 2     VALUE 'AG',
              REMARK_ID     TYPE THEAD-TDID     VALUE 'VBBK',
              REMARK_OBJECT TYPE THEAD-TDOBJECT VALUE 'Z011',
              THB           TYPE C LENGTH 3     VALUE 'THB',
              REPID         type c LENGTH 12    VALUE 'RECEIPT_PATH',
              PARAM         type c LENGTH 14    VALUE 'GET_CONFIG_001',
            END OF GC_CON.
