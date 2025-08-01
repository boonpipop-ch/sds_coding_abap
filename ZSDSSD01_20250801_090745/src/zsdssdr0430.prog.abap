*&---------------------------------------------------------------------*
*& Report ZSDSSDR430
*&---------------------------------------------------------------------*
*  Creation Date      : 13.12.2024
*  Author             : Wantanee Pr.
*  Add-on ID          :
*  Description        : VF11 Cancel billing
*  Purpose            :
*  Copied from        : ZSD_CANCEL_BILLING
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0430.
*----------------------------------------------------------------------*
*INCLUDE                                                               *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*TYPE-POOL                                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*TABLES                                                                *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*CONSTANTS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*TYPES                                                                 *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*INTERNAL TABLE                                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*GLOBAL VARIABLE                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
PARAMETERS      : P_FKDAT LIKE VBRK-FKDAT OBLIGATORY DEFAULT SY-DATUM,
                  P_VBELN LIKE VBRK-VBELN OBLIGATORY MEMORY ID VF.
SELECTION-SCREEN END OF BLOCK B01.
*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                  *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON P_VBELN.

  PERFORM VALIDATE_BILL_DOC.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM CALL_VF11.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  VALIDATE_BILL_DOC
*&---------------------------------------------------------------------*
FORM VALIDATE_BILL_DOC .

  DATA : LWA_VBRK TYPE VBRK.

  SELECT SINGLE *
    FROM VBRK
    INTO LWA_VBRK
   WHERE VBELN = P_VBELN.

  IF SY-SUBRC <> 0.

    MESSAGE E000(38) WITH 'BILLING DOCUMENT' P_VBELN 'DOES NOT EXIST.'.

  ENDIF.

* CHECK LOCK OBJECT
  CALL FUNCTION 'ENQUEUE_EVVBRKE'
    EXPORTING
      MODE_VBRK      = 'E'
      MANDT          = SY-MANDT
      VBELN          = P_VBELN
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

  IF SY-SUBRC = 1.

    MESSAGE E342(VF) WITH P_VBELN.

  ENDIF.

  CALL FUNCTION 'DEQUEUE_EVVBRKE'
    EXPORTING
      MODE_VBRK = 'E'
      MANDT     = SY-MANDT
      VBELN     = P_VBELN.


ENDFORM.                    " VALIDATE_BILL_DOC
*&---------------------------------------------------------------------*
*&      FORM  CALL_VF11
*&---------------------------------------------------------------------*
FORM CALL_VF11 .

  DATA: LWA_BDCDATA   TYPE BDCDATA,
        LWA_OPT       TYPE CTU_PARAMS,
        LT_BDCDATA    TYPE TABLE OF BDCDATA.

  DATA: LV_FKDAT(10)  TYPE C.

  REFRESH LT_BDCDATA.

  CLEAR LWA_BDCDATA.
  LWA_BDCDATA-PROGRAM  = 'SAPMV60A'.
  LWA_BDCDATA-DYNPRO   = '0102'.
  LWA_BDCDATA-DYNBEGIN = 'X'.
  APPEND LWA_BDCDATA TO LT_BDCDATA.

  WRITE P_FKDAT TO LV_FKDAT.

  CLEAR LWA_BDCDATA.
  LWA_BDCDATA-FNAM = 'RV60A-FKDAT'.
  LWA_BDCDATA-FVAL = LV_FKDAT.
  APPEND LWA_BDCDATA TO LT_BDCDATA.

  CLEAR LWA_BDCDATA.
  LWA_BDCDATA-FNAM = 'KOMFK-VBELN(01)'.
  LWA_BDCDATA-FVAL = P_VBELN.
  APPEND LWA_BDCDATA TO LT_BDCDATA.

  LWA_OPT-DISMODE = 'A'.  "'A'
  LWA_OPT-DEFSIZE = 'X'.

  LWA_OPT-RACOMMIT = 'X'. "+300000432 CONTINUE WHEN COMMIT WORK

  CALL TRANSACTION 'VF11' USING LT_BDCDATA OPTIONS FROM LWA_OPT.

ENDFORM.                                                    " CALL_VF11
