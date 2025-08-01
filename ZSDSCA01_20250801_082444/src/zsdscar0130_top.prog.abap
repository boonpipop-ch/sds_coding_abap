*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0130_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : e070.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF gy_result,
          trkorr  TYPE e070-trkorr,
          as4user TYPE e070-as4user,
          as4text TYPE e07t-as4text,
          check   TYPE c,
        END OF gy_result.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : gt_result TYPE TABLE OF gy_result,
       gs_result TYPE gy_result.

DATA : bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE.

DATA : messtab   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       t_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       s_messtab LIKE bdcmsgcoll.

DATA : dynpfields  LIKE dynpread OCCURS 5 WITH HEADER LINE.

DATA : gt_fcat   TYPE slis_t_fieldcat_alv,
       gs_layout TYPE slis_layout_alv,
       gt_sort   TYPE slis_t_sortinfo_alv,
       gs_sort   TYPE slis_sortinfo_alv.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS: gc_i  TYPE c LENGTH 1 VALUE 'I',
           gc_eq TYPE c LENGTH 2 VALUE 'EQ',
           gc_s  TYPE c LENGTH 1 VALUE 'S',
           gc_e  TYPE c LENGTH 1 VALUE 'E',
           gc_x  TYPE c LENGTH 1 VALUE 'X',
           gc_a  TYPE c LENGTH 1 VALUE 'A',
           gc_l  TYPE c LENGTH 1 VALUE 'L'.

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
