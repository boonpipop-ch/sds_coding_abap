*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0580_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_MJAHR FOR MKPF-MJAHR DEFAULT SY-DATUM+0(4),
                   S_MBLNR FOR MKPF-MBLNR,
                   S_BUDAT FOR MKPF-BUDAT,
                   S_USNAM FOR MKPF-USNAM.
SELECTION-SCREEN END OF BLOCK BLOCK1.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R1 RADIOBUTTON GROUP LOG DEFAULT 'X',
               R2 RADIOBUTTON GROUP LOG.
SELECTION-SCREEN END OF BLOCK BLOCK2.
