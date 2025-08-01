*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0650_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_BWART FOR MSEG-BWART,
                   S_MJAHR FOR MKPF-MJAHR,
                   S_MBLNR FOR MKPF-MBLNR.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS P_AUTO AS CHECKBOX.
