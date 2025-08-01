*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0500_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_VKORG  FOR VBRK-VKORG MODIF ID SD,
                   S_VTWEG  FOR VBRK-VTWEG MODIF ID SD,
                   S_FKART  FOR VBRK-FKART MODIF ID SD,
                   S_VBELN  FOR VBAK-VBELN MODIF ID SD,
                   S_ERDAT  FOR VBAK-ERDAT MODIF ID SD,
                   S_ERNAM  FOR VBAK-ERNAM MODIF ID SD.

  SELECT-OPTIONS : S_BUKRS FOR BKPF-BUKRS MODIF ID FI DEFAULT '1000',
                   S_BELNR FOR BKPF-BELNR MODIF ID FI,
                   S_BLART FOR BKPF-BLART MODIF ID FI,
                   S_GJAHR FOR BKPF-GJAHR MODIF ID FI,
                   S_BUDAT FOR BKPF-BUDAT MODIF ID FI,
                   S_USNAM FOR BKPF-USNAM MODIF ID FI.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R1 RADIOBUTTON GROUP SED DEFAULT 'X' USER-COMMAND COMM,
               R2 RADIOBUTTON GROUP SED.
SELECTION-SCREEN END OF BLOCK BLOCK2.

PARAMETERS : P_AUTO AS CHECKBOX.
