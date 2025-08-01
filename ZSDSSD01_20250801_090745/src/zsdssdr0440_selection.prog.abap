*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0440_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-999.
  PARAMETERS:
     P_VKORG TYPE VBRK-VKORG DEFAULT '1000' OBLIGATORY.

  SELECT-OPTIONS:
    S_VTWEG FOR VBRK-VTWEG,
    S_SPART FOR VBRP-SPART,
    S_VKBUR FOR VBRP-VKBUR,
    S_VKGRP FOR VBRP-VKGRP,
    S_KUNAG FOR VBRK-KUNAG,
    S_FKART FOR VBRK-FKART,
    S_FKDAT FOR VBRK-FKDAT DEFAULT SY-DATUM,
    S_VBELN FOR VBRK-VBELN,
    S_MATNR FOR VBRP-MATNR,
    S_PRODH FOR VBRP-PRODH,
    S_PERNR FOR VBPA-PERNR,
    S_BSTKD FOR VBKD-BSTKD,
    S_KVGR2 FOR VBRP-KVGR2,
    S_BNAME FOR VBAK-BNAME.
SELECTION-SCREEN END OF BLOCK BLOCK1.
