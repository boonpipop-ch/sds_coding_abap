*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0120_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_SERNR FOR EQUI-SERNR,
                   S_MATNR FOR EQUI-MATNR,
                   S_GWLDT FOR ZSDSCMT010-GWLDT,
                   S_GWLEN FOR ZSDSCMT010-GWLEN.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS : P_AUTO AS CHECKBOX.
