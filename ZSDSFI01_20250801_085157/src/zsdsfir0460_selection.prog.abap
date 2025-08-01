*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0460_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY LOWER CASE,
              P_CHECK AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.
