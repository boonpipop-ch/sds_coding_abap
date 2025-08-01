*&---------------------------------------------------------------------*
*& INCLUDE          ZSDSSDR0600_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS : S_VKGRP  FOR KNVV-VKGRP NO INTERVALS NO-EXTENSION,
*                 S_VHVIN  FOR ZTSD201-VHVIN ,
*                 S_VHCEX  FOR ZTSD201-VHCEX ,
*                 S_FRSEDE FOR ZTSD201-FRSEDE,
*                 S_TOBUDE FOR ZTSD201-TOBUDE,
*                 S_MOVED  FOR ZTSD201-CREATED_ON,
*                 S_MODCD  FOR VLCVEHICLE-ZZMODCD,
*                 S_MATNR  FOR VLCVEHICLE-MATNR.
*SELECTION-SCREEN END OF BLOCK BLOCK1.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_USR02  FOR ZSDSSDC031-USR02 .
SELECTION-SCREEN END OF BLOCK BLOCK1.
