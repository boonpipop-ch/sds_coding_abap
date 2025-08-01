*&---------------------------------------------------------------------*
*& Include          ZSDSSDI0020_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
SELECT-OPTIONS: S_ERDAT FOR LIKP-ERDAT, "Create date
                S_LFDAT FOR LIKP-LFDAT, "delivery date
                S_KODAT FOR LIKP-KODAT, "pick date
                S_LDDAT FOR LIKP-LDDAT, "load date
                S_WADAT FOR LIKP-WADAT, "gi date
                S_VBELN FOR LIKP-VBELN, "delivery date
                S_WERKS FOR LIPS-WERKS, "plant
                S_VTWEG FOR LIPS-VTWEG, "Distribution Chanel
                S_VKBUR FOR LIPS-VKBUR, "Sale office
                S_VKGRP FOR LIPS-VKGRP, "Sale group
                S_MATNR FOR LIPS-MATNR, "material
                S_SERNR FOR LIPS-SERNR, "Serial
                S_AUART FOR VBAK-AUART, "Type Sale
                S_KUNNR FOR VBAK-KUNNR, "Customer Code
                S_PRODH FOR LIPS-PRODH. "Prosuct hierarchy

PARAMETERS : S_KOSTK TYPE VBUK-KOSTK.      "Type status delivery
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
PARAMETERS : R_WAIT RADIOBUTTON GROUP TYP DEFAULT 'X',
             R_ALL  RADIOBUTTON GROUP TYP.
SELECTION-SCREEN END OF BLOCK B2.
