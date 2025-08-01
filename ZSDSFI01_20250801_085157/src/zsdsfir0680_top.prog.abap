*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0630_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : ANLA,ANLZ,SSCRFIELDS.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          ANLN1    TYPE ANLA-ANLN1,
          ANLN2    TYPE ANLA-ANLN2,
          MCOA1    TYPE ANLA-MCOA1,
          TXT50    TYPE ANLA-TXT50,
          INVZU    TYPE ANLA-INVZU,
          KOSTL    TYPE ANLZ-KOSTL,
          STORT    TYPE ANLZ-STORT,
          RAUMN    TYPE ANLZ-RAUMN,
          PERNR    TYPE ANLZ-PERNR,
          PERNR_IN TYPE ZSDSFIT055-PERNR,
          REMARK   TYPE ZSDSFIT055-REMARK,
          STATU    TYPE ZSDSFIT055-STATU,
          ERNAM    TYPE ZSDSFIT055-ERNAM,
          ERDAT    TYPE ZSDSFIT055-ERDAT,
          ERZET    TYPE ZSDSFIT055-ERZET,
          RUNNG    TYPE ZSDSFIT055-RUNNG,
          OWNNA    TYPE C LENGTH 100,
          MGNNA    TYPE C LENGTH 100,
          INPNA    TYPE C LENGTH 100,
          KTEXT    TYPE T499S-KTEXT,
          LTEXT    TYPE CSKT-LTEXT,
          CHECK    TYPE C,
        END OF GY_RESULT.

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

DATA : GC_YES TYPE C LENGTH 3  VALUE 'Yes',
       GC_NO  TYPE C LENGTH 2  VALUE 'No',
       GC_ONE TYPE C LENGTH 1  VALUE '1'.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:GC_I  TYPE C LENGTH 1 VALUE 'I',
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
