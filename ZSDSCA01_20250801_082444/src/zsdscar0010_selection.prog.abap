*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0010_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_FOWID  TYPE C LENGTH 4, " DEFAULT '83'.
               P_FORIO  TYPE C LENGTH 50,
               P_PARA1  TYPE C LENGTH 255 LOWER CASE,
               P_PARA2  TYPE C LENGTH 255 LOWER CASE,
               P_PARA3  TYPE C LENGTH 255 LOWER CASE,
               P_PARA4  TYPE C LENGTH 255 LOWER CASE,
               P_PARA5  TYPE C LENGTH 255 LOWER CASE,
               P_PARA6  TYPE C LENGTH 255 LOWER CASE,
               P_PARA7  TYPE C LENGTH 255 LOWER CASE,
               P_PARA8  TYPE C LENGTH 255 LOWER CASE,
               P_PARA9  TYPE C LENGTH 255 LOWER CASE,
               P_PARA10 TYPE C LENGTH 255 LOWER CASE,
               P_PARA11 TYPE C LENGTH 255 LOWER CASE,
               P_PARA12 TYPE C LENGTH 255 LOWER CASE,
               P_PARA13 TYPE C LENGTH 255 LOWER CASE,
               P_PARA14 TYPE C LENGTH 255 LOWER CASE,
               P_PARA15 TYPE C LENGTH 255 LOWER CASE,
               P_PARA16 TYPE C LENGTH 255 LOWER CASE,
               P_PARA17 TYPE C LENGTH 255 LOWER CASE,
               P_PARA18 TYPE C LENGTH 255 LOWER CASE,
               P_PARA19 TYPE C LENGTH 255 LOWER CASE,
               P_PARA20 TYPE C LENGTH 255 LOWER CASE.
SELECTION-SCREEN END OF BLOCK BLOCK1.

*SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-003.
*  PARAMETERS : P_USER TYPE C LENGTH 30 LOWER CASE,
*               P_PASS TYPE C LENGTH 30 LOWER CASE,
*               P_URL  TYPE C LENGTH 255 LOWER CASE.
*SELECTION-SCREEN END OF BLOCK BLOCK3.
