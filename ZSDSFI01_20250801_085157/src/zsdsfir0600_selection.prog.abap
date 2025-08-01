*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0600_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : P_ERDAT TYPE ANLA-ERDAT DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK2.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_BUKRS  FOR ANLA-BUKRS DEFAULT '1000',
                   S_ANLN1  FOR ANLA-ANLN1,
                   S_ANLN2  FOR ANLA-ANLN2,
                   S_MCOA1  FOR ANLA-MCOA1,
                   s_KOSTL  FOR ANLZ-KOSTL,
                   s_INVZU  FOR ANLA-INVZU,
                   s_STORT  FOR ANLZ-STORT.
SELECTION-SCREEN END OF BLOCK BLOCK1.
