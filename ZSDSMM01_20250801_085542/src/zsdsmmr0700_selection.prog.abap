*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0700_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_VBELN  FOR LIKP-VBELN OBLIGATORY,
                  S_BLDAT  FOR LIKP-BLDAT,
                  S_LGORT  FOR LIPS-LGORT.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: R_NEW RADIOBUTTON GROUP GR4 USER-COMMAND UCOM DEFAULT 'X',
              R_UP  RADIOBUTTON GROUP GR4.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R_MAP  AS CHECKBOX,
               R_INV  AS CHECKBOX,
               R_AM   AS CHECKBOX,
               R_PM   AS CHECKBOX,
               R_1700 AS  CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.
