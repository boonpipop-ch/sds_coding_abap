*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0620_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_ASOF TYPE SY-DATUM OBLIGATORY.
SELECT-OPTIONS : S_LAGER FOR EQBS-B_LAGER OBLIGATORY,
                 S_MATNR FOR EQUI-MATNR,
                 S_BWART FOR MSEG-BWART OBLIGATORY.
PARAMETERS : P_YEAR TYPE CHAR4.
SELECTION-SCREEN END OF BLOCK BLOCK1.
