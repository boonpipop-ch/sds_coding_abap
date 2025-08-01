*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0690_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_FILE LIKE RLGRAP-FILENAME LOWER CASE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: R1 RADIOBUTTON GROUP LOG DEFAULT 'X',
              R2 RADIOBUTTON GROUP LOG.

SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK MAIN WITH  FRAME TITLE TEXT-003,SKIP,
*BEGIN OF BLOCK REP1  WITH  FRAME TITLE TEXT-C01,
PUSHBUTTON /1(35)  REP01 USER-COMMAND REP01,
*END OF BLOCK REP1,
END OF BLOCK MAIN.
