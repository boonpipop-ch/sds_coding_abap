*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0500_SELECTION
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

*SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-003.
*  PARAMETERS  : R_BR RADIOBUTTON GROUP DOC DEFAULT 'X' ,
*                R_DO RADIOBUTTON GROUP DOC,
*                R_SO RADIOBUTTON GROUP DOC.
*SELECTION-SCREEN END OF BLOCK BLOCK3.
