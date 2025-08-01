"Name: \PR:RM07DOCS\EX:RM07DOCS_12\EI
ENHANCEMENT 0 ZSDS_MM_RM07DOCS_12.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_MM_RM07DOCS_12 (Enhancement Implementation)
*  Creation Date      : 09.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MME012
*  Description        : Enhancement to add PH1,PH2,PH3 and MATKL to output
*  Purpose            : Enhancement in declaration of LIST
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
* Field for Product Hierarchy
  DATA: PRDHA1 TYPE ZSDSMMS022-PRDHA1,    "SBU
        PRDHA2 TYPE ZSDSMMS022-PRDHA2,    "Type of product
        PRDHA3 TYPE ZSDSMMS022-PRDHA3,    "Series
        MATKL  TYPE ZSDSMMS022-MATKL.     "Material Group
ENDENHANCEMENT.
