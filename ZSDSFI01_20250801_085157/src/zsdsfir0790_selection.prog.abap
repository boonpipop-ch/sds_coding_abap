*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0790_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_RLDNR TYPE ACDOCA-RLDNR DEFAULT '0L'.
  PARAMETERS: P_GJAHR TYPE BKPF-GJAHR OBLIGATORY DEFAULT SY-DATUM+0(4).
  PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY LOWER CASE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: CB_M1  AS CHECKBOX DEFAULT 'X',
              CB_M2  AS CHECKBOX DEFAULT 'X',
              CB_M3  AS CHECKBOX DEFAULT 'X',
              CB_M4  AS CHECKBOX DEFAULT 'X',
              CB_M5  AS CHECKBOX DEFAULT 'X',
              CB_M6  AS CHECKBOX DEFAULT 'X',
              CB_M7  AS CHECKBOX DEFAULT 'X',
              CB_M8  AS CHECKBOX DEFAULT 'X',
              CB_M9  AS CHECKBOX DEFAULT 'X',
              CB_M10 AS CHECKBOX DEFAULT 'X',
              CB_M11 AS CHECKBOX DEFAULT 'X',
              CB_M12 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B2.
