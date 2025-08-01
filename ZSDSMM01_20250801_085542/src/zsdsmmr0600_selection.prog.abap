*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0600_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_REFID FOR MMIM_PREDOC_ORG-REFID,
                   S_DATLO FOR MMIM_PREDOC_ORG-DATLO,
                   S_UNAME FOR MMIM_PREDOC_ORG-UNAME.
SELECTION-SCREEN END OF BLOCK BLOCK1.
