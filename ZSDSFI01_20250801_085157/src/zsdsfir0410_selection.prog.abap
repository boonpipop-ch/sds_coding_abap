*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0410_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_ANLN1 FOR ANLA-ANLN1,
                 S_ANLN2 FOR ANLA-ANLN2,
                 S_MCOA1 FOR ANLA-MCOA1.
SELECTION-SCREEN END OF BLOCK BLOCK1.
