*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0560
*  Creation Date      : 30.11.2024
*  Author             : Jutamas Y.(Eviden)
*  Add-on ID          : ZFIARE004
*  Description        : This is a program to upload text file AR
*                       Copy from T41( program : ZFIAR_INF_COLLECTOR )
*  Purpose            : To Delete data in table ZSDSFIT029, ZSDSFIT038
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0560.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZSDSFIT029 .


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
*CONSTANTS:

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
##NEEDED
DATA: LF_DATE TYPE SY-DATUM,
      LF_TIME TYPE SY-UZEIT.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME .
  PARAMETERS:     P_BUKRS  TYPE BUKRS DEFAULT '1000'.
  SELECT-OPTIONS: S_BELNR  FOR ZSDSFIT029-BELNR.
  SELECT-OPTIONS: S_GJAHR  FOR ZSDSFIT029-GJAHR  DEFAULT '2024'  .

SELECTION-SCREEN END OF BLOCK B01.


"Update status
IF S_BELNR IS NOT INITIAL .
  LF_DATE  = SY-DATUM .
  LF_TIME = SY-UZEIT .

  UPDATE ZSDSFIT029 SET DELETE_FLAG = 'X'
                        DELETE_DATE = LF_DATE
                        UPDATE_BY   = SY-UNAME
                        UPDATE_ON   = LF_DATE
                        UPDATE_TIME = LF_TIME
   WHERE BUKRS = P_BUKRS
     AND BELNR IN S_BELNR
     AND GJAHR IN S_GJAHR .
  IF SY-SUBRC = 0 .
    WRITE: / TEXT-000.
  ENDIF.


  UPDATE ZSDSFIT038 SET DELETE_FLAG = 'X'
                        DELETE_DATE = LF_DATE
                        UPDATE_BY   = SY-UNAME
                        UPDATE_ON   = LF_DATE
                        UPDATE_TIME = LF_TIME
 WHERE BUKRS = P_BUKRS
   AND BELNR IN S_BELNR
   AND GJAHR IN S_GJAHR .
  IF SY-SUBRC = 0 .
    WRITE: / TEXT-001.
  ENDIF.


ENDIF.
