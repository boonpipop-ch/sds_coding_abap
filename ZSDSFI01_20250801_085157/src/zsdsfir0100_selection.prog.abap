*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0100_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_BUKRS FOR ACDOCA-RBUKRS NO INTERVALS NO-EXTENSION DEFAULT '1000',
                   S_GJAHR FOR BKPF-GJAHR DEFAULT SY-DATUM(4) NO INTERVALS NO-EXTENSION OBLIGATORY.

*  PARAMETERS     : P_SPRAS TYPE SPRAS DEFAULT 'E' OBLIGATORY.

  SELECT-OPTIONS : S_BLART FOR ACDOCA-BLART  NO INTERVALS DEFAULT 'DV',
                   S_BELNR FOR ACDOCA-BELNR,
                   S_KUNNR FOR ACDOCA-KUNNR,
                   S_BUDAT FOR ACDOCA-BUDAT,
                   S_XBLNR FOR BKPF-XBLNR.
*                   S_VBELN FOR VBRK-VBELN OBLIGATORY,
*                   S_KUNRG FOR VBRK-KUNRG,
*                   s_FKDAT FOR VBRK-FKDAT,
*                   S_ERDAT FOR VBRK-ERDAT.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
PARAMETERS : R_EN RADIOBUTTON GROUP RAD1 ,
             R_TH RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BLOCK2.
