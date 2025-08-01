*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0390_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_BANFN  FOR EBAN-BANFN,
                   S_WEBNO  FOR ZSDSMMT006-WEBNO.
SELECTION-SCREEN END OF BLOCK BLOCK1.

PARAMETERS : P_EMAIL AS CHECKBOX USER-COMMAND UCOMM.
PARAMETERS : P_SUBJE TYPE CHAR50 MODIF ID MIL.
PARAMETERS : P_SENDD TYPE SOMLRECI1-RECEIVER MODIF ID MIL.
PARAMETERS : P_SENDN TYPE SOMLRECI1-RECEIVER MODIF ID MIL.
SELECT-OPTIONS : s_RECEI FOR SOMLRECI1-RECEIVER NO INTERVALS MODIF ID MIL.
SELECT-OPTIONS : S_CC    FOR  SOMLRECI1-RECEIVER NO INTERVALS MODIF ID MIL.
