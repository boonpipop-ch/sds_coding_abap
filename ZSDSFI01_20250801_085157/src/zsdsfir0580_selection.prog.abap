*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0580_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_PSPNR FOR PRPS-PSPNR,
                   S_AUFNR FOR AUFK-AUFNR.
SELECTION-SCREEN END OF BLOCK BLOCK1.
