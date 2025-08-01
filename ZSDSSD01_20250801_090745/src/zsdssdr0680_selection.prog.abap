*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0680_SELECTION
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
  SELECT-OPTIONS : S_DO_NO  FOR ZSDSSDT025-DO_NO,
                   S_INVNO  FOR ZSDSSDT025-INVNO,
                   S_SMTDT  FOR ZSDSSDT025-SMART_TRACKING_DATE,
                   S_FLAG   FOR ZSDSSDT025-FLAG,
                   S_PETAX  FOR ZSDSSDT025-PETAX.
SELECTION-SCREEN END OF BLOCK BLOCK1.
