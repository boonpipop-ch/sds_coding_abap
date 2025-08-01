*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0710_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_RSNUM  FOR RESB-RSNUM.
SELECTION-SCREEN END OF BLOCK BLOCK1.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R_1 RADIOBUTTON GROUP APP DEFAULT 'X',
               R_2 RADIOBUTTON GROUP APP.
SELECTION-SCREEN END OF BLOCK BLOCK2.
