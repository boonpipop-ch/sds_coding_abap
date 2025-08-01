*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0180_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_EQUNR FOR ZSDSCMT003-EQUNR,
                   S_MATNR FOR ZSDSCMT003-MATNR,
                   S_WADAT FOR ZSDSCMT003-WADAT_IST,
                   S_SERNR FOR ZSDSCMT003-SERNR,
                   S_ERNAM FOR ZSDSCMT003-ERNAM,
                   S_ERDAT FOR ZSDSCMT003-ERDAT,
                   S_ERZET FOR ZSDSCMT003-ERZET,
                   S_AENAM FOR ZSDSCMT003-AENAM,
                   S_AEDAT FOR ZSDSCMT003-AEDAT,
                   S_AEZET FOR ZSDSCMT003-AEZET.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS : P_AUTO AS CHECKBOX.
