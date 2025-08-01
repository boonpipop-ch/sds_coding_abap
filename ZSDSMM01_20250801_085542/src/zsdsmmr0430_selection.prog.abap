*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0430_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_WEBNO FOR ZSDSMMT006-WEBNO OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.
