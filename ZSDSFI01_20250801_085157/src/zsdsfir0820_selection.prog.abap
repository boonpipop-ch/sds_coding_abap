*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0820_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_RLDNR for ACDOCA-RLDNR DEFAULT '0L',
                 s_BUDAT FOR ACDOCA-BUDAT ,
                 s_RACCT FOR ACDOCA-RACCT .
SELECTION-SCREEN END OF BLOCK BLOCK1.
