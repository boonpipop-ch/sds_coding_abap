*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0640_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_DATUM TYPE VBRK-FKDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS : S_VBELN  FOR VBAK-VBELN OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK2.
