*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0880_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_DATE_F TYPE SY-DATUM OBLIGATORY,
               P_DATE_L TYPE SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.
