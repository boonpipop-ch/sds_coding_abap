*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0720_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : P_BLDAT TYPE MKPF-BLDAT OBLIGATORY,
               P_BUDAT TYPE MKPF-BUDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK2.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_RSNUM  FOR RESB-RSNUM.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS C_TEST AS CHECKBOX DEFAULT 'X'.
