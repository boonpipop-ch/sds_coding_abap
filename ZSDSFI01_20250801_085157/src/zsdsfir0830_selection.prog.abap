*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0830_SELECTION
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
SELECT-OPTIONS : s_BUKRS FOR ZSDSFIT061-BUKRS,
                 s_BELNR  FOR ZSDSFIT061-BELNR OBLIGATORY,
                 s_GJAHR  FOR ZSDSFIT061-GJAHR.
SELECTION-SCREEN END OF BLOCK block1.
