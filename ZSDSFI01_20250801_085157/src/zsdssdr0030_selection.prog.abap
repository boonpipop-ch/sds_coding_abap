*&---------------------------------------------------------------------*
*& Include          ZSDSSDI0030_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_LFART FOR LIKP-LFART ,
                   S_DONUM FOR ZSDSSDT001-DONUM,
                   S_LFDAT FOR LIKP-LFDAT DEFAULT SY-DATUM,
                   S_ERDAT FOR LIKP-ERDAT DEFAULT SY-DATUM,
                   S_ERZET FOR LIKP-ERZET,
                   S_VTWEG FOR VBAK-VTWEG,
                   S_VKGRP FOR VBAK-VKGRP,
                   S_VKBUR FOR VBAK-VKBUR,
                   S_WMSDT FOR ZSDSSDT001-WMSDT,
                   S_STAPC FOR ZSDSSDT001-STAPC,
                   S_LOADD FOR ZSDSSDT001-LOADD,
                   S_CONTD FOR ZSDSSDT001-CONTD,
                   S_DEALC FOR KNA1-KUNNR,
                   S_SERNF FOR ZSDSSDT001-SERNF NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 1(15) BUT_UPS USER-COMMAND UPS.
