*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0140_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_VBELN FOR VBAP-VBELN,
                   S_POSNR FOR VBAP-POSNR,
                   S_MATNR FOR VBAP-MATNR.
SELECTION-SCREEN END OF BLOCK BLOCK1.

PARAMETERS : P_AUTO TYPE C NO-DISPLAY.
PARAMETERS : P_ZERO TYPE C NO-DISPLAY.
