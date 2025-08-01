*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0640_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : MMIM_PREDOC_ORG.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          DELIVERY TYPE ZSDSMMT026-DELIVERY,
          ITEM     TYPE ZSDSMMT026-ITEM,
          SERIAL   TYPE ZSDSMMT026-SERIAL,
          MBLNR    TYPE ZSDSMMT026-MBLNR,
          MJAHR    TYPE ZSDSMMT026-MJAHR,
          CHECK    TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF TY_S_GOSERIAL,
          SELECTED TYPE XFELD,
          SERIALNO TYPE GERNR,
          UII      TYPE UII_CHAR72,              "EHP603 IUID
          SUBRK    TYPE XFELD,                   "EHP604 TT 2007.12.21
        END OF TY_S_GOSERIAL,
        TY_T_GOSERIAL TYPE STANDARD TABLE OF TY_S_GOSERIAL WITH
                        NON-UNIQUE DEFAULT KEY.

TYPES : BEGIN OF TY_S_GOSERIAL_KERNEL,
          GLOBAL_COUNTER TYPE MIGO_GLOBAL_COUNTER,
          T_GOSERIAL     TYPE TY_T_GOSERIAL,
        END OF TY_S_GOSERIAL_KERNEL.
TYPES : TTY_S_GOSERIAL_KERNEL TYPE TABLE OF TY_S_GOSERIAL_KERNEL.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS: GC_I  TYPE C LENGTH 1 VALUE 'I',
           GC_EQ TYPE C LENGTH 2 VALUE 'EQ',
           GC_S  TYPE C LENGTH 1 VALUE 'S',
           GC_E  TYPE C LENGTH 1 VALUE 'E',
           GC_X  TYPE C LENGTH 1 VALUE 'X',
           GC_A  TYPE C LENGTH 1 VALUE 'A',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.

*-----------------------------------------------------------------------
* R A N G E S
*-----------------------------------------------------------------------
*DATA:
*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:
