*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0590_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     P_BUKRS TYPE ACDOCA-PBUKRS   DEFAULT '1000' OBLIGATORY.

  SELECT-OPTIONS:
                  S_GJAHR FOR PAYR-GJAHR   DEFAULT SY-DATUM MODIF ID PRC,
                  S_AUGBL FOR PAYR-VBLNR,
                  S_AUGDT FOR ACDOCA-AUGDT,
                  S_LIFNR FOR ACDOCA-LIFNR,
                  S_BELNR FOR ACDOCA-BELNR,
                  S_XBLNR FOR BKPF-XBLNR,
                  S_CHECT FOR PAYR-CHECT.
SELECTION-SCREEN END OF BLOCK BLOCK1.
