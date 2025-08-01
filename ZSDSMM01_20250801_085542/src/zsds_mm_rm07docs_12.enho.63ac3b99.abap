"Name: \PR:RM07DOCS\FO:BUILD_FIELDCATALOG\SE:END\EI
ENHANCEMENT 0 ZSDS_MM_RM07DOCS_12.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_MM_RM07DOCS_12 (Enhancement Implementation)
*  Creation Date      : 09.05.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : MME012
*  Description        : Enhancement to add PH1,PH2,PH3 and MATKL to output
*  Purpose            : Build fieldcatalog for PH1,PH2,PH3 and MATKL
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
* Build field catalog for product hierarchy
  CLEAR: FC_FLAT.

  FC_FLAT-NO_OUT = ABAP_TRUE.

* SBU
  FC_FLAT-TABNAME = 'LIST'.
  FC_FLAT-FIELDNAME = 'PRDHA1'.
  FC_FLAT-REF_TABNAME = 'ZSDSMMS022'.
  FC_FLAT-REF_FIELDNAME = 'PRDHA1'.
  APPEND FC_FLAT.

* Type of product
  FC_FLAT-TABNAME = 'LIST'.
  FC_FLAT-FIELDNAME = 'PRDHA2'.
  FC_FLAT-REF_TABNAME = 'ZSDSMMS022'.
  FC_FLAT-REF_FIELDNAME = 'PRDHA2'.
  APPEND FC_FLAT.

* Series
  FC_FLAT-TABNAME = 'LIST'.
  FC_FLAT-FIELDNAME = 'PRDHA3'.
  FC_FLAT-REF_TABNAME = 'ZSDSMMS022'.
  FC_FLAT-REF_FIELDNAME = 'PRDHA3'.
  APPEND FC_FLAT.

* Material Group
  FC_FLAT-TABNAME = 'LIST'.
  FC_FLAT-FIELDNAME = 'MATKL'.
  FC_FLAT-REF_TABNAME = 'ZSDSMMS022'.
  FC_FLAT-REF_FIELDNAME = 'MATKL'.
  APPEND FC_FLAT.

ENDENHANCEMENT.
