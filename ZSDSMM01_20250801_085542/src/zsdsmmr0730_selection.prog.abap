*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0730_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : R_OPEN RADIOBUTTON GROUP PER DEFAULT 'X',
               R_CLOS RADIOBUTTON GROUP PER.
SELECTION-SCREEN END OF BLOCK BLOCK1.
