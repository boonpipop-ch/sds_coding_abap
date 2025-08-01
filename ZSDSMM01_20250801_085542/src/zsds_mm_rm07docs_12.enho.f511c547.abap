"Name: \PR:RM07DOCS\EX:RM07DOCS_10\EI
ENHANCEMENT 0 ZSDS_MM_RM07DOCS_12.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_MM_RM07DOCS_12 (Enhancement Implementation)
*  Creation Date      : 09.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MME012
*  Description        : Enhancement to add PH1,PH2,PH3 and MATKL to output
*  Purpose            : Fill in PH1,PH2,PH3 and MATKL to LIST
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
* To fill in product hierarchy data and material group to LIST
  IF NOT LIST-MATNR IS INITIAL.
    READ TABLE GT_PRODUCT_HIER INTO DATA(LS_PRODUCT_HIER)
                               WITH KEY MATNR = LIST-MATNR
                               BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LIST-PRDHA1 = LS_PRODUCT_HIER-PRDHA1.
      LIST-PRDHA2 = LS_PRODUCT_HIER-PRDHA2.
      LIST-PRDHA3 = LS_PRODUCT_HIER-PRDHA3.
      LIST-MATKL = LS_PRODUCT_HIER-MATKL.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
