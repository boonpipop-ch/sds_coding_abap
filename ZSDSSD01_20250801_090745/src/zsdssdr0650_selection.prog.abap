*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0650_SELECTION
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
  PARAMETERS P_KSCHL  TYPE NASE-KSCHL    DEFAULT 'ZIV5' OBLIGATORY.
  PARAMETERS P_PADEST TYPE TSP03D-PADEST DEFAULT 'SCAN' OBLIGATORY.
  SELECT-OPTIONS : S_VBELN FOR VBRK-VBELN,
                   S_REFDC FOR ZSDSSDT026-REFDC,
                   S_STATU FOR ZSDSSDT026-STATU,
                   S_FKDAT FOR VBRK-FKDAT.
  PARAMETERS : P_ETAX TYPE VBCO7-ALLEF.

SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R1 RADIOBUTTON GROUP LOG DEFAULT 'X',
               R2 RADIOBUTTON GROUP LOG.
SELECTION-SCREEN END OF BLOCK BLOCK2.
