"Name: \PR:RM07DOCS\EX:RM07DOCS_13\EI
ENHANCEMENT 0 ZSDS_MM_RM07DOCS_12.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_MM_RM07DOCS_12 (Enhancement Implementation)
*  Creation Date      : 09.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MME012
*  Description        : Enhancement to add PH1,PH2,PH3 and MATKL to output
*  Purpose            : Get data from material master
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
* To get product hierarchy from material master
  IF NOT IMAKT_KEY[] IS INITIAL.
    SELECT MATNR PRDHA MATKL
      FROM MARA
      INTO CORRESPONDING FIELDS OF TABLE GT_PRODUCT_HIER
      FOR ALL ENTRIES IN IMAKT_KEY
      WHERE MATNR EQ IMAKT_KEY-MATNR.

    IF SY-SUBRC EQ 0.
      LOOP AT GT_PRODUCT_HIER ASSIGNING FIELD-SYMBOL(<LFS_PRDHA>).
        <LFS_PRDHA>-PRDHA1 = <LFS_PRDHA>-PRDHA(5).
        <LFS_PRDHA>-PRDHA2 = <LFS_PRDHA>-PRDHA+5(5).
        <LFS_PRDHA>-PRDHA3 = <LFS_PRDHA>-PRDHA+10(8).
      ENDLOOP.

      SORT GT_PRODUCT_HIER BY MATNR.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
