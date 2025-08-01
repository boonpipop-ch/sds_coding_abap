class ZCL_SDSPS_CAPEX_AVAIL_BDGT definition
  public
  final
  create public .

public section.

  class-methods RETRIEVE_CAPEX_DATA
    changing
      !CS_DATA type ZSDSPSS004 .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF TS_PRPS,
      PSPNR TYPE PRPS-PSPNR,
      POSID TYPE PRPS-POSID,
      STUFE TYPE PRPS-STUFE,
      OBJNR TYPE PRPS-OBJNR,
    END OF TS_PRPS .

  CONSTANTS GC_REPID TYPE PROGRAMM VALUE 'ZCL_SDSPS_CAPEX_AVAIL_BDGT' ##NO_TEXT.
  CONSTANTS GC_ERROR TYPE ZSDSPSS004-RESPONSESTATUS VALUE 'E' ##NO_TEXT.
  CONSTANTS GC_SUCCESS TYPE ZSDSPSS004-RESPONSESTATUS VALUE 'S' ##NO_TEXT.
  CONSTANTS GC_LEVEL_CAPEX_BUDGET TYPE PRPS-STUFE VALUE 2 ##NO_TEXT.
  CONSTANTS GC_LEVEL_CAPEX_EMEMO TYPE PRPS-STUFE VALUE 3 ##NO_TEXT.

  CLASS-METHODS VALIDATE_INPUT_DATA
    EXPORTING
      !ES_PRPS TYPE TS_PRPS
    CHANGING
      !CS_DATA TYPE ZSDSPSS004 .
ENDCLASS.



CLASS ZCL_SDSPS_CAPEX_AVAIL_BDGT IMPLEMENTATION.


  METHOD RETRIEVE_CAPEX_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_CAPEX_AVAIL_BDGT
*  Creation Date      : 24.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : PSE002
*  Description        : CAPEX budget data
*  Purpose            : Using in Inbound interface for CAPEX Available Budget
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CONSTANTS: LC_WBS_LEDNR    TYPE BPJA-LEDNR VALUE '0001',
               LC_CO_LEDNR     TYPE COSP_BAK-LEDNR VALUE '00',
               LC_RLDNR_LEDGER TYPE ACDOCA-RLDNR VALUE '0L'.
    DATA: LS_PRPS  TYPE TS_PRPS,
          LT_GEN_C TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.
    DATA: LRT_VORGA_BUDGET     TYPE RANGE OF BPJA-VORGA,
          LRT_WBS_RMA          TYPE RANGE OF PRPS-POSID,
          LRT_WRTTP_BUDGET     TYPE RANGE OF BPJA-WRTTP,
          LRT_WRTTP_COMMITMENT TYPE RANGE OF BPJA-WRTTP,
          LRT_GJAHR            TYPE RANGE OF COSP_BAK-GJAHR,
          LRT_AWTYP_ACTUAL     TYPE RANGE OF ACDOCA-AWTYP,
          LRT_RACCT_ACTUAL     TYPE RANGE OF ACDOCA-RACCT.


*===================== validate input
    CALL METHOD ZCL_SDSPS_CAPEX_AVAIL_BDGT=>VALIDATE_INPUT_DATA
      IMPORTING
        ES_PRPS = LS_PRPS
      CHANGING
        CS_DATA = CS_DATA.

    IF CS_DATA-RESPONSESTATUS IS NOT INITIAL.
      RETURN.
    ENDIF.

*===================== Get GENC
    CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
      EXPORTING
        IF_REPID = GC_REPID
      IMPORTING
        ET_GEN_C = LT_GEN_C.

    LOOP AT LT_GEN_C ASSIGNING FIELD-SYMBOL(<L_GEN_C>).
      CASE <L_GEN_C>-PARAM.
        WHEN 'VORGA_BUDGET'.
          INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                          OPTION =  <L_GEN_C>-PARAM_OPTION
                          LOW    =  <L_GEN_C>-VALUE_LOW
                          HIGH   =  <L_GEN_C>-VALUE_HIGH )
                 INTO TABLE LRT_VORGA_BUDGET.
        WHEN 'WBS_RMA'.
          INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                          OPTION =  <L_GEN_C>-PARAM_OPTION
                          LOW    =  <L_GEN_C>-VALUE_LOW
                          HIGH   =  <L_GEN_C>-VALUE_HIGH )
                 INTO TABLE LRT_WBS_RMA.
        WHEN 'WRTTP_BUDGET'.
          INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                          OPTION =  <L_GEN_C>-PARAM_OPTION
                          LOW    =  <L_GEN_C>-VALUE_LOW
                          HIGH   =  <L_GEN_C>-VALUE_HIGH )
                 INTO TABLE LRT_WRTTP_BUDGET.

        WHEN 'WRTTP_COMMITMENT'.
          INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                          OPTION =  <L_GEN_C>-PARAM_OPTION
                          LOW    =  <L_GEN_C>-VALUE_LOW
                          HIGH   =  <L_GEN_C>-VALUE_HIGH )
                 INTO TABLE LRT_WRTTP_COMMITMENT.

        WHEN 'AWTYP_ACTUAL'.
          INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                          OPTION =  <L_GEN_C>-PARAM_OPTION
                          LOW    =  <L_GEN_C>-VALUE_LOW
                          HIGH   =  <L_GEN_C>-VALUE_HIGH )
                 INTO TABLE LRT_AWTYP_ACTUAL.
        WHEN 'RACCT_ACTUAL'.
          INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                          OPTION =  <L_GEN_C>-PARAM_OPTION
                          LOW    =  <L_GEN_C>-VALUE_LOW
                          HIGH   =  <L_GEN_C>-VALUE_HIGH )
                 INTO TABLE LRT_RACCT_ACTUAL  .
      ENDCASE.
    ENDLOOP.


*===================== select data

    IF LS_PRPS-POSID IN LRT_WBS_RMA.
      INSERT VALUE #(  SIGN = 'I'
                       OPTION = 'EQ'
                       LOW = CS_DATA-FISCALYEAR
                       HIGH = '' )
            INTO TABLE LRT_GJAHR  .

      "-- budget, buget distributed
      SELECT WLJHR, WLJHV                               "#EC CI_NOORDER
      INTO @DATA(LS_BPJA)
      UP TO 1 ROWS
      FROM BPJA
      WHERE LEDNR = @LC_WBS_LEDNR
      AND   OBJNR = @LS_PRPS-OBJNR
      AND   WRTTP IN @LRT_WRTTP_BUDGET
      AND   VORGA IN @LRT_VORGA_BUDGET
      AND   GJAHR IN @LRT_GJAHR.
      ENDSELECT.

    ELSE.
      CLEAR LRT_GJAHR.

      "-- budget, buget distributed
      SELECT WLGES, WLGEV                               "#EC CI_NOORDER
      INTO @DATA(LS_BPGE)
      UP TO 1 ROWS
      FROM BPGE
      WHERE LEDNR = @LC_WBS_LEDNR
      AND   OBJNR = @LS_PRPS-OBJNR
      AND   WRTTP IN @LRT_WRTTP_BUDGET
      AND   VORGA IN @LRT_VORGA_BUDGET.
      ENDSELECT.
    ENDIF.

    "-- actual
    SELECT SUM( HSL )
    INTO @DATA(LF_ACTUAL_AMT)
    FROM ACDOCA
    WHERE PS_POSID = @LS_PRPS-POSID
    AND   GJAHR IN @LRT_GJAHR
    AND   RLDNR = @LC_RLDNR_LEDGER
    AND   AWTYP IN @LRT_AWTYP_ACTUAL
    AND   RACCT IN @LRT_RACCT_ACTUAL.


    "commitment
    SELECT SUM( WKG001 ) AS WKG001,
           SUM( WKG002 ) AS WKG002,
           SUM( WKG003 ) AS WKG003,
           SUM( WKG004 ) AS WKG004,
           SUM( WKG005 ) AS WKG005,
           SUM( WKG006 ) AS WKG006,
           SUM( WKG007 ) AS WKG007,
           SUM( WKG008 ) AS WKG008 ,
           SUM( WKG009 ) AS WKG009,
           SUM( WKG010 ) AS WKG010,
           SUM( WKG011 ) AS WKG011,
           SUM( WKG012 ) AS WKG012
    INTO @DATA(LS_COSP_COMMIT)
    FROM V_COSP_VIEW "cosp
    WHERE LEDNR = @LC_CO_LEDNR
    AND   OBJNR = @LS_PRPS-OBJNR
    AND   GJAHR IN @LRT_GJAHR
    AND   WRTTP IN @LRT_WRTTP_COMMITMENT.

*===================== prepare data

    CS_DATA-RESPONSESTATUS = GC_SUCCESS.
    CS_DATA-RESPONSEMESSAGE = 'Budget data is sent.'(m05).
    IF LS_PRPS-STUFE = GC_LEVEL_CAPEX_BUDGET. "2
      IF LS_PRPS-POSID IN LRT_WBS_RMA.
        CS_DATA-BUDGET = LS_BPJA-WLJHR.
        CS_DATA-BUDGETDISTRIBUTED = LS_BPJA-WLJHV.
      ELSE.
        CS_DATA-BUDGET = LS_BPGE-WLGES.
        CS_DATA-BUDGETDISTRIBUTED = LS_BPGE-WLGEV.
      ENDIF.

      CS_DATA-REMAINBUDGET = CS_DATA-BUDGET - CS_DATA-BUDGETDISTRIBUTED.
    ELSEIF LS_PRPS-STUFE = GC_LEVEL_CAPEX_EMEMO ."3
      IF LS_PRPS-POSID IN LRT_WBS_RMA.
        CS_DATA-BUDGET = LS_BPJA-WLJHR.
      ELSE.
        CS_DATA-BUDGET = LS_BPGE-WLGES.
      ENDIF     .
      CS_DATA-ACTUAL  = LF_ACTUAL_AMT .

      CS_DATA-COMMITMENT = LS_COSP_COMMIT-WKG001 + LS_COSP_COMMIT-WKG002 + LS_COSP_COMMIT-WKG003 +
                           LS_COSP_COMMIT-WKG004 + LS_COSP_COMMIT-WKG005 + LS_COSP_COMMIT-WKG006 +
                           LS_COSP_COMMIT-WKG007 + LS_COSP_COMMIT-WKG008 + LS_COSP_COMMIT-WKG009 +
                           LS_COSP_COMMIT-WKG010 + LS_COSP_COMMIT-WKG011 + LS_COSP_COMMIT-WKG012 .
      CS_DATA-AVAILABLE = CS_DATA-BUDGET - CS_DATA-ACTUAL - CS_DATA-COMMITMENT.
    ENDIF.
  ENDMETHOD.


  METHOD VALIDATE_INPUT_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_CAPEX_AVAIL_BDGT
*  Creation Date      : 24.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : PSE002
*  Description        : CAPEX budget data
*  Purpose            : Validate input data before retriving capex
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR ES_PRPS.
    IF CS_DATA-FISCALYEAR IS INITIAL.
      CS_DATA-RESPONSESTATUS = GC_ERROR.
      CS_DATA-RESPONSEMESSAGE = 'FiscalYear is required'(m01).
      RETURN.
    ENDIF.

    IF CS_DATA-WBS IS INITIAL
    AND CS_DATA-EMEMO IS INITIAL.
      CS_DATA-RESPONSESTATUS = GC_ERROR.
      CS_DATA-RESPONSEMESSAGE = 'WBS or eMemo is required'(m02).
      RETURN.
    ENDIF.

    IF CS_DATA-WBS IS NOT INITIAL.
      SELECT PSPNR ,
             POSID ,
             STUFE ,
             OBJNR
      UP TO 1 ROWS
      INTO @ES_PRPS
      FROM PRPS
      WHERE POSID = @CS_DATA-WBS
      ORDER BY PSPNR.
      ENDSELECT.
      IF SY-SUBRC <> 0.
        CS_DATA-RESPONSESTATUS = GC_ERROR.
        CS_DATA-RESPONSEMESSAGE = 'Invalid WBS'(m03).
        RETURN.
      ELSE.
        IF ES_PRPS-STUFE <> GC_LEVEL_CAPEX_BUDGET
        AND ES_PRPS-STUFE <> GC_LEVEL_CAPEX_EMEMO.
          CS_DATA-RESPONSESTATUS = GC_ERROR.
          CS_DATA-RESPONSEMESSAGE = |WBS must be level { GC_LEVEL_CAPEX_BUDGET } or { GC_LEVEL_CAPEX_EMEMO }| ##NO_TEXT.
          RETURN.
        ENDIF.
      ENDIF.
    ELSEIF CS_DATA-EMEMO IS NOT INITIAL.
      SELECT PSPNR ,
             POSID ,
             STUFE ,
             OBJNR
      UP TO 1 ROWS
      INTO @ES_PRPS
      FROM PRPS
      WHERE USR03 = @CS_DATA-EMEMO
      ORDER BY PSPNR.
      ENDSELECT.
      IF SY-SUBRC <> 0.
        CS_DATA-RESPONSESTATUS = GC_ERROR.
        CS_DATA-RESPONSEMESSAGE = 'Invalid eMemo'(m04).
        RETURN.
      ELSE.
        CS_DATA-WBS = ES_PRPS-POSID.
        IF ES_PRPS-STUFE <> GC_LEVEL_CAPEX_BUDGET
        AND ES_PRPS-STUFE <> GC_LEVEL_CAPEX_EMEMO.
          CS_DATA-RESPONSESTATUS = GC_ERROR.
          CS_DATA-RESPONSEMESSAGE = |WBS must be level { GC_LEVEL_CAPEX_BUDGET } or { GC_LEVEL_CAPEX_EMEMO }| ##NO_TEXT.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.



  ENDMETHOD.
ENDCLASS.
