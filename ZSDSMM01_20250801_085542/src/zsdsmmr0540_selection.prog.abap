*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0540_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY LOWER CASE.
*              P_CHECK AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: R1 RADIOBUTTON GROUP MAT DEFAULT 'X',
              R2 RADIOBUTTON GROUP MAT,
              R3 RADIOBUTTON GROUP MAT,
              R4 RADIOBUTTON GROUP MAT,
              R5 RADIOBUTTON GROUP MAT,
              R6 RADIOBUTTON GROUP MAT,
              R7 RADIOBUTTON GROUP MAT,
              R8 RADIOBUTTON GROUP MAT.
SELECTION-SCREEN END OF BLOCK B2.
