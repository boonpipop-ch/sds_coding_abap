*&---------------------------------------------------------------------*
*& Include          ZSDSFII0060_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_CONTN  FOR ZSDSFIT002-CONTN,
                   S_LIFNR  FOR ZSDSFIT002-LIFNR,
                   S_RUNNG  FOR ZSDSFIT002-RUNNG.
SELECTION-SCREEN END OF BLOCK BLOCK1.
