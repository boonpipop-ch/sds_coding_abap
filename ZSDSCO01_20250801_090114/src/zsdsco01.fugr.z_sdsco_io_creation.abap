*-----------------------------------------------------------------------
*  Program ID         : Z_SDSCO_IO_CREATION
*  Creation Date      : 06.06.2024
*  Author             : Nadtaya T. (Eviden)
*  Add-on ID          : COIOI001
*  Description        : Inbound IO Master, Planning and Budgeting
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  12.09.2024  F36K905480  Waraporn S. CH01 CR24_046
*                                      Modify logic for number range of IO
*-----------------------------------------------------------------------
FUNCTION Z_SDSCO_IO_CREATION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_ORDERMASTER) TYPE  ZSDSCOS006 OPTIONAL
*"  EXPORTING
*"     VALUE(EX_AUFNR) TYPE  AUFNR
*"  TABLES
*"      ET_PLANNING TYPE  ZSDSCOS004_TT OPTIONAL
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
*   ZCOIOI001 Inbound IO Master, Planning and Budgeting
*"----------------------------------------------------------------------
  DATA: LF_AUFNR TYPE AUFK-AUFNR,
        LF_MODE  TYPE C,
        LF_SUBRC TYPE SYSUBRC.
*...............................................................
* Validation
*...............................................................
*..check existing IO / required field
  PERFORM F_ORD_VALIDATION TABLES ET_RETURN
                           USING IS_ORDERMASTER
                           CHANGING LF_AUFNR LF_MODE.
  READ TABLE ET_RETURN   WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
*    CLEAR ls_return.
    RETURN.
  ENDIF.
*
**...............................................................
**  set IO number
**...............................................................
  PERFORM F_GET_IO_NUMBER TABLES ET_RETURN USING IS_ORDERMASTER CHANGING LF_AUFNR.
  READ TABLE ET_RETURN WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.
*
**...............................................................
**  Simulate
**...............................................................
  PERFORM F_ORD_CREATE TABLES ET_RETURN USING    ABAP_TRUE
                                                 LF_MODE
                                                 IS_ORDERMASTER
                                                 LF_AUFNR.
  READ TABLE ET_RETURN WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.
*
*
  PERFORM F_PLANNING_VALIDATION TABLES ET_PLANNING
                                       ET_RETURN USING IS_ORDERMASTER-K2_YEAR.
  READ TABLE ET_RETURN  WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.
*
*...............................................................
* Update
*...............................................................
  PERFORM F_ORD_CREATE TABLES ET_RETURN USING    ABAP_FALSE
                                                 LF_MODE
                                                 IS_ORDERMASTER
                                                 LF_AUFNR.
  READ TABLE ET_RETURN WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

  EX_AUFNR = LF_AUFNR.
*

*...Maintain Planning data
  PERFORM F_PLANNING_UPDATE TABLES ET_PLANNING
                                       ET_RETURN
                                       USING ABAP_TRUE LF_AUFNR IS_ORDERMASTER-K2_YEAR
                                             CHANGING LF_SUBRC.
  IF  LF_SUBRC NE 0.
    RETURN.
  ELSE.
    PERFORM F_PLANNING_UPDATE TABLES ET_PLANNING
                                           ET_RETURN
                                           USING ABAP_FALSE LF_AUFNR IS_ORDERMASTER-K2_YEAR
                                                 CHANGING LF_SUBRC.
  ENDIF.

*...Maintain Budget data
  IF LF_SUBRC EQ 0.
    CALL FUNCTION 'Z_SDSCO_BUDGET_CREATION'
      EXPORTING
        IS_ORDERMASTER = IS_ORDERMASTER
        IM_AUFNR       = LF_AUFNR
*   IMPORTING
*       EX_AUFNR       =
      TABLES
        ET_PLANNING    = ET_PLANNING
        ET_RETURN      = ET_RETURN.
  ENDIF.

ENDFUNCTION.
