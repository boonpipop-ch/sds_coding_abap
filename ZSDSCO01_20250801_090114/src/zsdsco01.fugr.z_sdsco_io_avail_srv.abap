*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*  24.06.2025  420000674  Wuthichai L.  Add business Transaction to be
*                                       able to check as a calculation condition
*-----------------------------------------------------------------------
FUNCTION Z_SDSCO_IO_AVAIL_SRV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_K2_MEMO) TYPE  ZSDSDE_K2MEMO OPTIONAL
*"     VALUE(IM_K2_BG_YEAR) TYPE  ZSDSDE_K2YEAR OPTIONAL
*"     VALUE(IM_K2_PERIOD_FR) TYPE  ZSDSDE_K2PERF OPTIONAL
*"     VALUE(IM_K2_PERIOD_TO) TYPE  ZSDSDE_K2PERT OPTIONAL
*"  EXPORTING
*"     VALUE(ES_PARAM) TYPE  ZSDSCOS001
*"  TABLES
*"      ET_ACTUAL TYPE  ZSDSCOS003_TT OPTIONAL
*"      ET_COMMIT TYPE  ZSDSCOS003_TT OPTIONAL
*"      ET_ASSIGNED TYPE  ZSDSCOS003_TT OPTIONAL
*"      ET_PLAN TYPE  ZSDSCOS003_TT OPTIONAL
*"      ET_AVAILABLE TYPE  ZSDSCOS003_TT OPTIONAL
*"      ET_RETURN TYPE  ZSDSCOS002_TT OPTIONAL
*"----------------------------------------------------------------------
*  ZCOIOE001 Function Call: Internal Order Available Budget
*"----------------------------------------------------------------------
  DATA: LT_COSP   TYPE TT_COSP,
        LS_RETURN TYPE ZSDSCOS003,
        LF_AUFEX  TYPE AUFK-AUFEX.

  LF_AUFEX = |{ IM_K2_MEMO ALPHA = IN }|.

  ES_PARAM-K2_RESP_CCA = IM_K2_MEMO.
  ES_PARAM-K2_BG_YEAR = IM_K2_BG_YEAR.
  ES_PARAM-K2_PERIOD_FR = IM_K2_PERIOD_FR.
  ES_PARAM-K2_PERIOD_TO = IM_K2_PERIOD_TO.

*Get “Budget” already allocated to Internal Order
  SELECT * INTO TABLE @DATA(LT_AUFK) FROM AUFK
    WHERE AUART EQ 'ZI04'
      AND AUFEX EQ @LF_AUFEX.
  IF SY-SUBRC NE 0.
    LS_RETURN-TYPE = 'E'.
    LS_RETURN-MESSAGE = 'External Order no. does not exist'(005).
*..return Message type E
    APPEND LS_RETURN TO ET_RETURN.
    RETURN.

  ELSE.

*    SELECT lednr, versn, objnr, wrttp, gjahr, kstar, twaer, wog001, wog002, wog003, "#EC CI_NO_TRANSFORM "-420000674
    SELECT LEDNR, VERSN, OBJNR, WRTTP, GJAHR, KSTAR, VRGNG, TWAER, WOG001, WOG002, WOG003, "#EC CI_NO_TRANSFORM "+420000674
             WOG004, WOG005, WOG006, WOG007, WOG008, WOG009, WOG010, WOG011, WOG012
        INTO TABLE @LT_COSP FROM V_COSP_VIEW
          FOR ALL ENTRIES IN @LT_AUFK
          WHERE OBJNR EQ @LT_AUFK-OBJNR
            AND LEDNR EQ '00'
            AND GJAHR EQ @IM_K2_BG_YEAR
            AND VERSN EQ '000'.
    IF SY-SUBRC EQ 0.
      SORT LT_COSP BY LEDNR VERSN OBJNR WRTTP GJAHR KSTAR.
      LOOP AT LT_COSP INTO DATA(LS_COSP).
        READ TABLE LT_AUFK INTO DATA(LS_AUFK) WITH KEY OBJNR = LS_COSP-OBJNR.
        CHECK SY-SUBRC EQ 0.
        CASE LS_COSP-WRTTP.
          WHEN '11'.
            PERFORM F_GET_COSP_DATA TABLES ET_ACTUAL USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                           IM_K2_PERIOD_TO
                                                           LS_COSP.

            PERFORM F_GET_COSP_DATA TABLES ET_ASSIGNED USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                           IM_K2_PERIOD_TO
                                                           LS_COSP.

            PERFORM F_REVERSE_SIGN CHANGING LS_COSP.

            PERFORM F_GET_COSP_DATA TABLES ET_AVAILABLE USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                         IM_K2_PERIOD_TO
                                                         LS_COSP.
            LS_RETURN-TYPE = 'S'.
            LS_RETURN-MESSAGE = 'Success'(004).

          WHEN '21' OR '22'.
            PERFORM F_GET_COSP_DATA TABLES ET_COMMIT USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                         IM_K2_PERIOD_TO
                                                         LS_COSP.
            PERFORM F_GET_COSP_DATA TABLES ET_ASSIGNED USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                         IM_K2_PERIOD_TO
                                                         LS_COSP.
            PERFORM F_REVERSE_SIGN CHANGING LS_COSP.
            PERFORM F_GET_COSP_DATA TABLES ET_AVAILABLE USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                         IM_K2_PERIOD_TO
                                                         LS_COSP.
            LS_RETURN-TYPE = 'S'.
            LS_RETURN-MESSAGE = 'Success'(004).

          WHEN '10'.
*<-- Start of Insertion 420000674 24.06.2025 (Ignore Bus.Tran SDOR)
*           Ignore Business Transaction SDOR
            IF LS_COSP-VRGNG EQ 'SDOR'.
              CONTINUE.
            ENDIF.
*--> End of Insertion 420000674 24.06.2025
            PERFORM F_GET_COSP_DATA TABLES ET_PLAN USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                         IM_K2_PERIOD_TO
                                                         LS_COSP.
            PERFORM F_GET_COSP_DATA TABLES ET_AVAILABLE USING IM_K2_MEMO LS_AUFK-AUFNR IM_K2_PERIOD_FR
                                                         IM_K2_PERIOD_TO
                                                         LS_COSP.
            LS_RETURN-TYPE = 'S'.
            LS_RETURN-MESSAGE = 'Success'(004).
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF LS_RETURN IS NOT INITIAL.
    APPEND LS_RETURN TO ET_RETURN.
  ELSE.
    LS_RETURN-TYPE = 'E'.
    LS_RETURN-MESSAGE = 'No data found'(006).
*..return Message type E
    APPEND LS_RETURN TO ET_RETURN.
  ENDIF.

ENDFUNCTION.
