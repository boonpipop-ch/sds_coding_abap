*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0120_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*SELECT-OPTIONS : s_vkgrp  FOR knvv-vkgrp NO INTERVALS NO-EXTENSION,
*                 s_vhvin  FOR ztsd201-vhvin ,
*                 s_vhcex  FOR ztsd201-vhcex ,
*                 s_frsede FOR ztsd201-frsede,
*                 s_tobude FOR ztsd201-tobude,
*                 s_moved  FOR ztsd201-created_on,
*                 s_modcd  FOR vlcvehicle-zzmodcd,
*                 s_matnr  FOR vlcvehicle-matnr.
*SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_ITREQ TYPE CHAR20,
               P_SUBJT TYPE CHAR50,
               P_INFMT TYPE CHAR255,
               P_TCODE TYPE CHAR20,
               P_REQNM TYPE RP50G-PERNR.

SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS :      P_EMAIL TYPE CHAR20.
  SELECT-OPTIONS :  S_CCEML FOR  SOMLRECI1-RECEIVER NO INTERVALS    .
  PARAMETERS :      P_SENDD TYPE SOMLRECI1-RECEIVER.
  PARAMETERS :      P_FILES TYPE ZSDSCAS019-FILENAME.
SELECTION-SCREEN END OF BLOCK BLOCK2.
