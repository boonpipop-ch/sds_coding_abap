*&---------------------------------------------------------------------*
*& Include          ZXF01U01
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FEBEP) LIKE  FEBEP STRUCTURE  FEBEP
*"     VALUE(I_FEBKO) LIKE  FEBKO STRUCTURE  FEBKO
*"     VALUE(I_TESTRUN) TYPE  XFLAG
*"  EXPORTING
*"     VALUE(E_FEBEP) LIKE  FEBEP STRUCTURE  FEBEP
*"     VALUE(E_FEBKO) LIKE  FEBKO STRUCTURE  FEBKO
*"     VALUE(E_MSGTEXT) LIKE  FEBMKA-MESSG
*"     VALUE(E_MSGTYP) LIKE  FEBMKA-MSTYP
*"     VALUE(E_UPDATE) LIKE  FEBMKA-MSTYP
*"  TABLES
*"      T_FEBCL STRUCTURE  FEBCL
*"      T_FEBRE STRUCTURE  FEBRE
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*  Program            : ZXF01U01
*  Creation Date      : 01.07.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : N/A
*  Description        : Electronic account statement
*  Purpose            : update Bank Transaction to FEBEP-ZUONR
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
DATA: LR_BUKRS TYPE RANGE OF BUKRS.

*...Get active company code
ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZXF01U01'
                                                IF_PARAM = 'BUKRS'
                                      IMPORTING ET_RANGE = LR_BUKRS ).

*...Update Bank Transaction to FEBEP-ZUONR
IF I_FEBKO-BUKRS IN LR_BUKRS AND LINES( LR_BUKRS ) GT 0.
  CALL FUNCTION 'Z_SDSFI_EBS_UPDATE'
    EXPORTING
      I_FEBEP   = I_FEBEP
      I_FEBKO   = I_FEBKO
      I_TESTRUN = I_TESTRUN
    IMPORTING
      E_FEBEP   = E_FEBEP
      E_FEBKO   = E_FEBKO
      E_MSGTEXT = E_MSGTEXT
      E_MSGTYP  = E_MSGTYP
      E_UPDATE  = E_UPDATE
    TABLES
      T_FEBCL   = T_FEBCL
      T_FEBRE   = T_FEBRE.
ENDIF.
