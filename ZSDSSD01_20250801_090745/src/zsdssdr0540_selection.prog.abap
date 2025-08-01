*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0540_SELECTION
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
  SELECT-OPTIONS : S_AUART  FOR VBAK-AUART,
                   S_VBELN  FOR VBAK-VBELN,
                   S_ERDAT  FOR VBAK-ERDAT,
                   S_ERNAM  FOR VBAK-ERNAM.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R1 RADIOBUTTON GROUP STU DEFAULT 'X',
               R2 RADIOBUTTON GROUP STU,
               R3 RADIOBUTTON GROUP STU.

SELECTION-SCREEN END OF BLOCK BLOCK2.
