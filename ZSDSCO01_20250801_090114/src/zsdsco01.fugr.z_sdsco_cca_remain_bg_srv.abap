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
FUNCTION Z_SDSCO_CCA_REMAIN_BG_SRV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_K2_RESP_CCA) TYPE  ZSDSDE_K2CCA OPTIONAL
*"     VALUE(IM_K2_BG_YEAR) TYPE  ZSDSDE_K2YEAR OPTIONAL
*"     VALUE(IM_K2_PERIOD_FR) TYPE  ZSDSDE_K2PERF OPTIONAL
*"     VALUE(IM_K2_PERIOD_TO) TYPE  ZSDSDE_K2PERT OPTIONAL
*"     VALUE(IM_K2_GL) TYPE  ZSDSDE_K2GL OPTIONAL
*"  EXPORTING
*"     VALUE(ES_PARAM) TYPE  ZSDSCOS001
*"  TABLES
*"      ET_DATA TYPE  ZSDSCOS001_TT OPTIONAL
*"      ET_RETURN TYPE  ZSDSCOS002_TT OPTIONAL
*"----------------------------------------------------------------------
*  ZCOCCAE001 Function Call: Cost Center Available Budget
*"----------------------------------------------------------------------
  DATA: LT_COSP_IO  TYPE TT_COSP,
        LT_COSP_CCA TYPE TT_COSP,
        LS_PARAM    TYPE ZSDSCOS001,
        LS_RETURN   TYPE ZSDSCOS003,
        LF_KSTAR    TYPE V_COSP_VIEW-KSTAR,
        LF_KOSTV    TYPE AUFK-KOSTV,
        LF_OBJNR    TYPE V_COSP_VIEW-OBJNR.

  ES_PARAM-K2_RESP_CCA = IM_K2_RESP_CCA.
  ES_PARAM-K2_BG_YEAR = IM_K2_BG_YEAR.
  ES_PARAM-K2_PERIOD_FR = IM_K2_PERIOD_FR.
  ES_PARAM-K2_PERIOD_TO = IM_K2_PERIOD_TO.
  ES_PARAM-K2_GL = IM_K2_GL.

  LF_KOSTV = |{ IM_K2_RESP_CCA ALPHA = IN }|.
  LF_KSTAR = |{ IM_K2_GL ALPHA = IN }|.
  LF_OBJNR = |KS1000{ LF_KOSTV }|.

  LS_PARAM-K2_RESP_CCA  = IM_K2_RESP_CCA.
  LS_PARAM-K2_BG_YEAR   = IM_K2_BG_YEAR.
  LS_PARAM-K2_PERIOD_FR = IM_K2_PERIOD_FR.
  LS_PARAM-K2_PERIOD_TO = IM_K2_PERIOD_TO.
  LS_PARAM-K2_GL        = IM_K2_GL.

**Get “Budget” already allocated to Internal Order
  SELECT * INTO TABLE @DATA(LT_AUFK) FROM AUFK
    WHERE AUART EQ 'ZI04'
      AND KOSTV EQ @LF_KOSTV.
  IF SY-SUBRC EQ 0.
*...Budget already allocated to Internal Order
*    SELECT lednr, versn, objnr, wrttp, gjahr, kstar, twaer, wog001, wog002, wog003, "#EC CI_NO_TRANSFORM "-420000674
    SELECT LEDNR, VERSN, OBJNR, WRTTP, GJAHR, KSTAR, VRGNG, TWAER, WOG001, WOG002, WOG003, "#EC CI_NO_TRANSFORM "+420000674
             WOG004, WOG005, WOG006, WOG007, WOG008, WOG009, WOG010, WOG011, WOG012
        INTO TABLE @LT_COSP_IO  FROM V_COSP_VIEW
          FOR ALL ENTRIES IN @LT_AUFK
          WHERE OBJNR EQ @LT_AUFK-OBJNR
            AND LEDNR EQ '00'
            AND GJAHR EQ @LS_PARAM-K2_BG_YEAR
            AND WRTTP EQ '10'
            AND VERSN EQ '000'
            AND KSTAR EQ @LF_KSTAR.
    SORT LT_COSP_IO BY LEDNR VERSN OBJNR WRTTP GJAHR KSTAR.
  ENDIF.

*  Get “Plan” amount posted to Cost Center by G/L account and by period
*  SELECT lednr, versn, objnr, wrttp, gjahr, kstar, twaer, wog001, wog002, wog003, "#EC CI_NO_TRANSFORM "-420000674
  SELECT LEDNR, VERSN, OBJNR, WRTTP, GJAHR, KSTAR, VRGNG, TWAER, WOG001, WOG002, WOG003, "#EC CI_NO_TRANSFORM "+420000674
           WOG004, WOG005, WOG006, WOG007, WOG008, WOG009, WOG010, WOG011, WOG012
      INTO TABLE @LT_COSP_CCA FROM V_COSP_VIEW
     WHERE OBJNR EQ @LF_OBJNR
       AND LEDNR EQ '00'
       AND GJAHR EQ @LS_PARAM-K2_BG_YEAR
       AND WRTTP EQ '01'
       AND VERSN EQ '000'
       AND KSTAR EQ @LF_KSTAR ORDER BY LEDNR, VERSN, OBJNR, WRTTP, GJAHR, KSTAR.

  IF LINES( LT_COSP_IO ) EQ 0 AND LINES( LT_COSP_CCA ) EQ 0.
    LS_RETURN-TYPE = 'E'.
    LS_RETURN-MESSAGE = 'Resp. Cost Center does not exist'(001).
    APPEND LS_RETURN TO ET_RETURN.
  ELSE.

    LOOP AT LT_COSP_IO INTO DATA(LS_COSP).
      PERFORM F_REVERSE_SIGN CHANGING LS_COSP.
      PERFORM F_GET_COSP_TAB TABLES ET_DATA USING IM_K2_RESP_CCA IM_K2_PERIOD_FR
                                                                 IM_K2_PERIOD_TO
                                                                 LS_COSP.
    ENDLOOP.

*...“Plan” amount posted to Cost Center by G/L account and by period
    LOOP AT LT_COSP_CCA INTO LS_COSP.
      PERFORM F_GET_COSP_TAB TABLES ET_DATA USING IM_K2_RESP_CCA IM_K2_PERIOD_FR
                                                                 IM_K2_PERIOD_TO
                                                                 LS_COSP.
    ENDLOOP.

    LS_RETURN-TYPE = 'S'.
    LS_RETURN-MESSAGE = 'Success'(004).
    COLLECT LS_RETURN INTO ET_RETURN.
  ENDIF.

ENDFUNCTION.
