*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0100_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_EQUNR FOR EQUI-EQUNR,
                   S_SERNR FOR EQUI-SERNR,
                   S_MATNR FOR EQUI-MATNR,
                   S_TXT04 FOR TJ02T-TXT04.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R1 RADIOBUTTON GROUP LOG DEFAULT 'X',
               R2 RADIOBUTTON GROUP LOG.
SELECTION-SCREEN END OF BLOCK BLOCK2.
