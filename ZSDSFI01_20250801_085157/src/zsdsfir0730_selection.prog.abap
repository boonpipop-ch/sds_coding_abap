*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0730_SELECTION
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

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_WBSE  FOR VBAP-PS_PSP_PNR.

SELECTION-SCREEN END OF BLOCK block1.
