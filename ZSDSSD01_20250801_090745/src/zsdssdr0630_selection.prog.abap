*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0630_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS P_KSCHL TYPE NASE-KSCHL DEFAULT 'ZIV5'.
  SELECT-OPTIONS : S_VBELN  FOR LIKP-VBELN,
                   S_ERDAT  FOR LIKP-ERDAT,
                   S_LSTEL  FOR LIKP-LSTEL,
                   S_SMTDT  FOR ZSDSSDT025-SMART_TRACKING_DATE,
                   S_ZZPOB  FOR VBAK-ZZPOB DEFAULT 'Z1'.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS P_AUTO AS CHECKBOX.
PARAMETERS P_MHA  AS CHECKBOX.
PARAMETERS P_GIS  AS CHECKBOX.
PARAMETERS P_BAT  AS CHECKBOX.
*SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
*  SELECT-OPTIONS : S_KDKG5  FOR KNA1-KDKG5.
*SELECTION-SCREEN END OF BLOCK BLOCK2.
