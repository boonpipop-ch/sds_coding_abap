*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0610_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_RLDNR  FOR ACDOCA-RLDNR DEFAULT '0L',
                   S_RBUKRS FOR ACDOCA-RBUKRS DEFAULT '1000',
                   S_GJAHR  FOR ACDOCA-GJAHR,
                   S_BELNR  FOR ACDOCA-BELNR,
                   S_BUDAT  FOR ACDOCA-BUDAT,
                   S_POSID  FOR ACDOCA-PS_POSID,
                   S_PRCTR  FOR ACDOCA-PRCTR,
                   S_AUFNR  FOR ACDOCA-AUFNR,
                   S_MATNR  FOR ACDOCA-MATNR,
                   S_FKART  FOR ACDOCA-FKART,
                   S_VKORG  FOR ACDOCA-VKORG,
                   S_VTWEG  FOR ACDOCA-VTWEG,
                   S_VKGRP  FOR VBRP-VKGRP,
                   S_VKBUR  FOR VBRP-VKBUR,
                   S_PRODH  FOR VBRP-PRODH,
                   S_RACCT  FOR ACDOCA-RACCT.

SELECTION-SCREEN END OF BLOCK BLOCK1.
