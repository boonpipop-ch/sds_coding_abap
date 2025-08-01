*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0570_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_FKART FOR VBRK-FKART DEFAULT 'ZI04',
                   S_vbeln FOR VBRK-VBELN,
                   S_FKDAT FOR VBRK-FKDAT.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS P_AUTO AS CHECKBOX.
