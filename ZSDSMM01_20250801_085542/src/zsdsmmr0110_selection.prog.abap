*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0110_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_EBELN FOR EKKO-EBELN OBLIGATORY,
                 S_ERNAM FOR ZSDSMMT002-ERNAM,
                 S_ERDAT FOR ZSDSMMT002-ERDAT.
PARAMETERS : P_EBELN TYPE EKKO-EBELN NO-DISPLAY.
*SELECT-OPTIONS : s_lifnr FOR lfa1-lifnr,
*                 s_name1 FOR lfa1-name1,
*                 s_natio FOR adrc-nation DEFAULT space,
*                 s_sperr FOR lfa1-sperr,
*                 s_kverm FOR lfb1-kverm.
SELECTION-SCREEN END OF BLOCK BLOCK1.
