*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0770_SELECTION
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
SELECT-OPTIONS :S_RLDNR       FOR ACDOCA-RLDNR DEFAULT '0L',
                s_RBUKRS      FOR ACDOCA-RBUKRS,
                s_GJAHR       FOR ACDOCA-GJAHR,
                S_BELNR       FOR ACDOCA-BELNR,
                s_BUDAT       FOR ACDOCA-BUDAT,
                s_BLDAT       FOR ACDOCA-BLDAT,
                S_PSPNR       FOR ACDOCA-MAT_PSPNR,
                S_POSID       FOR ACDOCA-MAT_PS_POSID,
                S_PRODH       FOR ACDOCA-PRODH_PA,
                S_VKBUR       FOR ACDOCA-VKBUR_PA,
                S_VKGRP       FOR ACDOCA-VKGRP_PA,
                S_PRCTR       FOR ACDOCA-PRCTR,
                S_RCNTR       FOR ACDOCA-RCNTR,
                S_AUFNR       FOR ACDOCA-AUFNR,
                S_RACCT       FOR SKAT-TXT50,
                S_KUNNR       FOR ACDOCA-KUNNR.

SELECTION-SCREEN END OF BLOCK block1.
