*----------------------------------------------------------------------*
***INCLUDE LZSDSCO01F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_cosp_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_ACTUAL
*&      --> IM_K2_PERIOD_FR
*&      --> IM_K2_PERIOD_TO
*&      --> LS_COSP
*&---------------------------------------------------------------------*
FORM F_GET_COSP_DATA  TABLES   PT_TABLE TYPE ZSDSCOS004_TT
                      USING    UV_K2_MEMO TYPE ZSDSDE_K2MEMO
                               UV_AUFNR TYPE AUFK-AUFNR
                               UV_FROM TYPE POPER
                               UV_TO TYPE POPER
                               US_COSP TYPE TY_COSP.
  DATA: LF_FROM   TYPE POPER,
        LS_DATA   TYPE ZSDSCOS004,
        LF_SOURCE TYPE C LENGTH 20,
        LF_TARGET TYPE C LENGTH 20,
        LF_TOTAL  TYPE C LENGTH 20.

  LF_FROM = UV_FROM.
  CLEAR LS_DATA.
*
  DO.
    LF_SOURCE = |US_COSP-WOG{ LF_FROM }|.
    LF_TARGET = |LS_DATA-WOG{ LF_FROM }|.
    LF_TOTAL  = 'LS_DATA-WOGTOT'.

    ASSIGN (LF_SOURCE) TO FIELD-SYMBOL(<L_SOURCE>).
    IF SY-SUBRC EQ 0.
      ASSIGN (LF_TARGET) TO FIELD-SYMBOL(<L_TARGET>).
      IF SY-SUBRC EQ 0.
*        ADD <l_source> TO <l_target>.
        <L_TARGET> = <L_TARGET> + <L_SOURCE>.
        ASSIGN (LF_TOTAL) TO FIELD-SYMBOL(<L_TOTAL>).
        IF SY-SUBRC EQ 0.
*          ADD <l_source> TO <l_total>.
          <L_TOTAL> = <L_TOTAL> + <L_SOURCE>.
        ENDIF.
      ENDIF.
    ENDIF.

    LF_FROM = LF_FROM + 1.

    IF LF_FROM GT UV_TO.
      EXIT.
    ENDIF.
  ENDDO.

  LS_DATA-K2MEMO = UV_K2_MEMO.
  LS_DATA-AUFNR  = UV_AUFNR.
  LS_DATA-KSTAR  = US_COSP-KSTAR.
  LS_DATA-WAERS  = 'THB'.
*  APPEND ls_data TO pt_table.
  COLLECT LS_DATA INTO PT_TABLE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ORD_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_RETURN
*&      --> IS_ORDERMASTER
*&---------------------------------------------------------------------*
FORM F_ORD_CREATE  TABLES   PT_RETURN TYPE BAPIRET2_T
                   USING    UF_SIM TYPE ABAP_BOOL
                            UF_MODE TYPE C
                            US_ORDERMASTER TYPE ZSDSCOS006
                            UF_AUFNR TYPE AUFK-AUFNR.

  DATA: LS_MASTER_DATA TYPE BAPI2075_7.

  CHECK UF_MODE EQ 'I'.

  IF UF_SIM EQ ABAP_FALSE.
    FREE PT_RETURN.
  ENDIF.

  LS_MASTER_DATA-ORDER        = UF_AUFNR.
  LS_MASTER_DATA-ORDER_TYPE   = 'ZI04'.
  LS_MASTER_DATA-ORDER_NAME   = US_ORDERMASTER-K2_ORDER_NAME.
  LS_MASTER_DATA-CO_AREA      = '1000'.
  LS_MASTER_DATA-COMP_CODE    = '1000'.
  LS_MASTER_DATA-RESPCCTR     = US_ORDERMASTER-K2_RESPCCTR.
  LS_MASTER_DATA-REQUEST_CCTR = US_ORDERMASTER-K2_REQUEST_CCTR.
  LS_MASTER_DATA-EXT_ORD_NO   = US_ORDERMASTER-K2_MEMO.

  CALL FUNCTION 'BAPI_INTERNALORDER_CREATE'
    EXPORTING
      I_MASTER_DATA = LS_MASTER_DATA
      TESTRUN       = UF_SIM
*     I_MASTER_DATB =
*    IMPORTING
*     E_MASTER_DATA =
*     E_MASTER_DATB =
*     orderid       = cv_orderid
    TABLES
*     SRULES        =
      RETURN        = PT_RETURN
*     EXTENSIONIN   =
    .
  READ TABLE PT_RETURN WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0 AND UF_SIM EQ ABAP_FALSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.
    DELETE PT_RETURN WHERE TYPE NE 'S'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ord_validation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_PLANNING
*&      --> ET_RETURN
*&---------------------------------------------------------------------*
FORM F_ORD_VALIDATION  TABLES PT_RETURN   TYPE BAPIRET2_T
                        USING US_ORDERMASTER TYPE ZSDSCOS006
                        CHANGING CH_AUFNR TYPE AUFK-AUFNR
                                 CH_MODE  TYPE ABAP_BOOLEAN.

  DATA: LT_SYSSTATUS TYPE STANDARD TABLE OF BAPI2075_3.

  SELECT AUFNR, OBJNR INTO @DATA(LS_AUFK)
    FROM AUFK UP TO 1 ROWS
    WHERE AUART EQ 'ZI04'
      AND AUFEX EQ @US_ORDERMASTER-K2_MEMO ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF SY-SUBRC EQ 0.

    CH_AUFNR = LS_AUFK-AUFNR.
    CH_MODE  = 'U'.

    CALL FUNCTION 'BAPI_INTERNALORDER_GETDETAIL'
      EXPORTING
        ORDERID       = LS_AUFK-AUFNR
      TABLES
        SYSTEM_STATUS = LT_SYSSTATUS.

    READ TABLE LT_SYSSTATUS WITH KEY SY_ST_TEXT = 'REL' TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
      <L_RETURN>-TYPE = 'E'.
      <L_RETURN>-MESSAGE = 'An order with the External Order No.(E-Memo no.) already exists'(002).
    ELSE.
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING <L_RETURN>.
      <L_RETURN>-TYPE = 'W'.
      <L_RETURN>-MESSAGE = 'An order already exists'(003).
    ENDIF.

  ELSE.

    CH_MODE = 'I'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_planning_validation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_PLANNING
*&      --> ET_RETURN
*&---------------------------------------------------------------------*
FORM F_PLANNING_VALIDATION  TABLES   PT_PLANNING TYPE ZSDSCOS004_TT
                                     PT_RETURN   TYPE BAPIRET2_T
                             USING   UF_GJAHR    TYPE GJAHR.

  LOOP AT PT_PLANNING INTO DATA(LS_PLANNING).

    PERFORM F_CE_CHECK TABLES PT_RETURN
                       USING LS_PLANNING-KSTAR
                             UF_GJAHR.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ce_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_RETURN
*&      --> LS_PLANNING_KSTAR
*&      --> LS_PLANNING_GJAHR
*&---------------------------------------------------------------------*
FORM F_CE_CHECK  TABLES   PT_RETURN TYPE BAPIRET2_T
                 USING    UF_KSTAR TYPE V_COSP_VIEW-KSTAR
                          UF_GJAHR TYPE GJAHR.
  DATA:LT_KSTAR_TAB TYPE STANDARD TABLE OF COSEL2,
       LT_PERIODS   TYPE STANDARD TABLE OF PERIODS,
       LT_CSKAF     TYPE STANDARD TABLE OF CSKAF,
       LT_CSKAU     TYPE STANDARD TABLE OF CSKAU,
       LV_KSTAR     TYPE KSTAR.

  FREE LT_KSTAR_TAB.
  LV_KSTAR = |{ UF_KSTAR ALPHA  = IN }|.
  LT_KSTAR_TAB = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = LV_KSTAR ) ).

  PERFORM F_PERIODS_GET TABLES LT_PERIODS USING UF_GJAHR.

  CALL FUNCTION 'K_COSTELEMS_SELECT_CSKAF_TAB'
    EXPORTING
      KOKRS            = '1000'
*     LANGU            = SY-LANGU
      ONLY_COMPLETE    = 'X'
      WITH_TEXT        = ''
      BYPASSING_BUFFER = 'X'
*     FILL_BUFFER      = 'X'
    TABLES
      KSTAR_TAB        = LT_KSTAR_TAB
      IT_CSKAF         = LT_CSKAF
      IT_CSKAU         = LT_CSKAU
      IT_PERIODS       = LT_PERIODS
    EXCEPTIONS
      NO_RECORD_FOUND  = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
    APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
    <L_RETURN>-TYPE   = 'E'.
    <L_RETURN>-ID     = SY-MSGID.
    <L_RETURN>-NUMBER = SY-MSGNO.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO <L_RETURN>-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_periods_get
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PERIOD
*&      --> LS_PLANNING_GJAHR
*&---------------------------------------------------------------------*
FORM F_PERIODS_GET  TABLES   PT_PERIOD TYPE FINS_T_PERIODS
                    USING    UF_GJAHR TYPE GJAHR.

  DATA:
    LV_BEGDA      TYPE BEGDA,
    LV_ENDDA      TYPE ENDDA,
    LS_RNG_PERIOD TYPE TRGS_POSTING_PERIOD.

  CONSTANTS LC_NUM TYPE NUMC2 VALUE 12.

  DO LC_NUM TIMES.

    LS_RNG_PERIOD-SIGN = 'I'. LS_RNG_PERIOD-OPTION = 'EQ'.
    UNPACK SY-INDEX TO LS_RNG_PERIOD-LOW.

    CALL FUNCTION 'FTR_GDPDU_CONVERT_PERIOD2DATE'
      EXPORTING
        IM_RNG_PERIOD     = LS_RNG_PERIOD
        IM_FISCAL_YEAR    = UF_GJAHR
*       IM_PERIV          =
        IM_COMPANY_CODE   = '1000'
      IMPORTING
*       EX_RNG_DATE       =
        EX_START_DATE     = LV_BEGDA
        EX_END_DATE       = LV_ENDDA
      EXCEPTIONS
        INVALID_INPUT     = 1
        CONVERSION_FAILED = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    APPEND INITIAL LINE TO PT_PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>).
    UNPACK LS_RNG_PERIOD-LOW TO <L_PERIOD>-BUPER.
    <L_PERIOD>-DATAB = LV_BEGDA.
    <L_PERIOD>-DATBI = LV_ENDDA.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_planning_update
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_PLANNING
*&      --> ET_RETURN
*&---------------------------------------------------------------------*
FORM F_PLANNING_UPDATE  TABLES  PT_PLANNING TYPE ZSDSCOS004_TT
                                PT_RETURN  TYPE BAPIRET2_T
                          USING UF_SIM     TYPE ABAP_BOOLEAN
                                UF_AUFNR TYPE AUFK-AUFNR
                                UF_GJAHR TYPE GJAHR
                       CHANGING CH_SUBRC TYPE SYSUBRC.

  DATA: LT_ITRKU01JA TYPE TABLE OF RKU01JA,
        LS_RKU01_CUR TYPE RKU01_CUR,
        LF_KSTAR     TYPE COSP-KSTAR,
        LF_COMMIT    TYPE C.

  CH_SUBRC = 0.

  IF UF_SIM EQ ABAP_FALSE.
    LF_COMMIT = ABAP_TRUE.
  ENDIF.

*   planning in object currency
  LS_RKU01_CUR-WOG_MAN = 'X'.

  ##INTO_OK  LOOP AT PT_PLANNING INTO DATA(LS_PLANNING).
    FREE: LT_ITRKU01JA.

    APPEND INITIAL LINE TO LT_ITRKU01JA ASSIGNING FIELD-SYMBOL(<L_ITRKU01JA>).
    ##ENH_OK MOVE-CORRESPONDING LS_PLANNING TO <L_ITRKU01JA>.
    LF_KSTAR = |{ LS_PLANNING-KSTAR ALPHA  = IN }|.

    <L_ITRKU01JA>-AUFNR = UF_AUFNR.
    <L_ITRKU01JA>-KSTAR = LF_KSTAR.
    <L_ITRKU01JA>-FCWKG    = '1'.                    "must be filled
    <L_ITRKU01JA>-FCWKF    = '1'.                    "must be filled
    <L_ITRKU01JA>-FCWKV    = '1'.                    "must be filled
    <L_ITRKU01JA>-FCMEG    = '1'.                    "must be filled
    <L_ITRKU01JA>-FCMEF    = '1'.                    "must be filled
    <L_ITRKU01JA>-FCMEV    = '1'.                    "must be filled

    CALL FUNCTION 'K_COSTS_PLAN_INTERFACE_PERIOD'
      EXPORTING
*       BLTXT            = ' '
        COMMIT           = LF_COMMIT
        DELTA            = ' '
        GJAHR            = UF_GJAHR
        KOKRS            = '1000'
        MESSAGES_SHOW    = 'X'
        PERAB            = '001'
        PERBI            = '012'
        UPDATE_VALUES    = 'X'
        VERSN            = '0'
        VRGNG            = 'RKP1'
        ONLINE_VB        = 'X'
        IRKU01_CUR       = LS_RKU01_CUR
        TESTMODE         = UF_SIM
*       WSVALUE          = 1
*       KEEP_TWAER       = ' '
      TABLES
        IRKU01JA         = LT_ITRKU01JA
*       IRKU0RJA         =
      EXCEPTIONS
        MESSAGES_OCCURED = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
      CH_SUBRC = SY-SUBRC.

      APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
      <L_RETURN>-TYPE   = 'E'.
      <L_RETURN>-ID     = SY-MSGID.
      <L_RETURN>-NUMBER = SY-MSGNO.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO <L_RETURN>-MESSAGE.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_budget_update
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_PLANNING
*&      --> ET_RETURN
*&      --> LF_AUFNR
*&      <-- LF_SUBRC
*&---------------------------------------------------------------------*
FORM F_BUDGET_UPDATE  TABLES   PT_PLANNING TYPE ZSDSCOS004_TT
                               PT_RETURN   TYPE BAPIRET2_T
                      USING    UF_AUFNR TYPE AUFK-AUFNR
                               UF_GJAHR TYPE GJAHR
                            CHANGING CH_SUBRC TYPE SYSUBRC.
  DATA:
    LF_AMOUNT      TYPE BPJA-WLJHR,
    LT_RETURN      TYPE BAPIRET2_T,
    LT_BPAK        TYPE STANDARD TABLE OF BPAK,
    LF_ERROR_FOUND TYPE OAX.

  CH_SUBRC = 0.

  WAIT UP TO 3 SECONDS.

  FREE: LT_RETURN, LT_BPAK.

  CLEAR LF_AMOUNT.
  ##INTO_OK LOOP AT PT_PLANNING INTO DATA(LS_PLANNING).
    LF_AMOUNT =   LS_PLANNING-WOG001 + LS_PLANNING-WOG002 + LS_PLANNING-WOG003 +
                  LS_PLANNING-WOG004 + LS_PLANNING-WOG005 + LS_PLANNING-WOG006 +
                  LS_PLANNING-WOG007 + LS_PLANNING-WOG008 + LS_PLANNING-WOG009 +
                  LS_PLANNING-WOG010 + LS_PLANNING-WOG011 + LS_PLANNING-WOG012.

    APPEND INITIAL LINE TO LT_BPAK ASSIGNING FIELD-SYMBOL(<L_BPAK>).
    <L_BPAK>-E_OBJNR = 'OR' && UF_AUFNR.
    <L_BPAK>-E_VORGA = 'KBUD'.
    <L_BPAK>-E_GJAHR = UF_GJAHR.
    <L_BPAK>-WERT      = LF_AMOUNT.
    <L_BPAK>-TWAER     = 'THB'.
  ENDLOOP.

  CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
    EXPORTING
      I_BUDGET_ACTIVITY = 'KBUD'
*     I_BUDGET_ACTIV_SUP_RET              = ' '
*     I_BUDGET_DISTRIBUTION_ALLOWED       = ' '
*     i_commit_data     = ''
      I_DELTA_AMOUNTS   = ''
*     i_rollup_data     = ''
*     i_check_plan_data = ''
      I_APPLICATION     = 'O'
      I_COMMIT_ALL      = 'X'
    IMPORTING
      E_ERRORS_FOUND    = LF_ERROR_FOUND
    TABLES
      IT_BPAK           = LT_BPAK
      IT_RETURN         = LT_RETURN
    EXCEPTIONS
      NO_UPDATE         = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0 OR LF_ERROR_FOUND IS NOT INITIAL.
* Implement suitable error handling here
    ##INTO_OK LOOP AT LT_RETURN INTO DATA(LS_RETURN). ##INTO_OK
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
      MOVE-CORRESPONDING LS_RETURN TO <L_RETURN>.
    ENDLOOP.
    CH_SUBRC = SY-SUBRC.
  ELSE.
    APPEND INITIAL LINE TO PT_RETURN ASSIGNING <L_RETURN>.
    <L_RETURN>-TYPE   = 'S'.
    <L_RETURN>-ID     = SY-MSGID.
    <L_RETURN>-NUMBER = SY-MSGNO.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO <L_RETURN>-MESSAGE.
    CH_SUBRC = SY-SUBRC.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bdc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM F_BDC  USING UF_PROGRAM TYPE BDCDATA-PROGRAM
                  UF_DYNPRO  TYPE BDCDATA-DYNPRO
                  UF_FNAM    TYPE BDCDATA-FNAM
                  UF_FVAL    TYPE BDCDATA-FVAL.
  CLEAR GS_BDCDATA.
  GS_BDCDATA-PROGRAM  = UF_PROGRAM.
  GS_BDCDATA-DYNPRO   = UF_DYNPRO.
  IF GF_LAST_PROGRAM = UF_PROGRAM AND GF_LAST_DYNPRO = UF_DYNPRO.
    GS_BDCDATA-DYNBEGIN = SPACE.
  ELSE.
    GS_BDCDATA-DYNBEGIN = 'X'.
    GF_LAST_PROGRAM = UF_PROGRAM.
    GF_LAST_DYNPRO  = UF_DYNPRO.
  ENDIF.
  GS_BDCDATA-FNAM     = UF_FNAM.
  GS_BDCDATA-FVAL     = UF_FVAL.
  APPEND GS_BDCDATA TO GT_BDCDATA.

  IF UF_FNAM = 'BDC_OKCODE'.
    CLEAR: GF_LAST_PROGRAM, GF_LAST_DYNPRO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_IO_REL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_RETURN
*&      --> LF_AUFNR
*&---------------------------------------------------------------------*
FORM F_IO_REL  TABLES   PT_RETURN   TYPE BAPIRET2_T
                USING   UF_AUFNR    TYPE AUFK-AUFNR.

  DATA: LF_VALUE TYPE BDCDATA-FVAL.

  WAIT UP TO 5 SECONDS.

  LF_VALUE = UF_AUFNR.

  FREE: GT_BDCDATA, GT_MESSTAB.

  PERFORM F_BDC USING 'SAPMKAUF' '0110' 'BDC_CURSOR'  'COAS-AUFNR'.
  PERFORM F_BDC USING 'SAPMKAUF' '0110' 'COAS-AUFNR'  LF_VALUE.
  PERFORM F_BDC USING 'SAPMKAUF' '0110' 'BDC_OKCODE'  '/00'.

  PERFORM F_BDC USING 'SAPMKAUF' '0600' 'BDC_CURSOR'  'COAS-KTEXT'.
  PERFORM F_BDC USING 'SAPMKAUF' '0600' 'BDC_OKCODE'  '=PHS1'.


  PERFORM F_BDC USING 'SAPMKAUF' '0600' 'BDC_CURSOR'  'COAS-KTEXT'.
  PERFORM F_BDC USING 'SAPMKAUF' '0600' 'BDC_OKCODE'  '=SICH'.

*---------------------------------------------------------------------*
* Batch-Input-Daten per CALL TRANSACTION abspielen                    *
*---------------------------------------------------------------------*
  CALL TRANSACTION 'KO02' USING GT_BDCDATA MODE GF_MODE  "#EC CI_CALLTA
                          MESSAGES INTO GT_MESSTAB.

  ##INTO_OK LOOP AT GT_MESSTAB INTO DATA(LS_MESSTAB).
    SELECT SINGLE * INTO @DATA(LS_T100) FROM T100 WHERE SPRSL = @LS_MESSTAB-MSGSPRA
                              AND   ARBGB = @LS_MESSTAB-MSGID
                              AND   MSGNR = @LS_MESSTAB-MSGNR.
    REPLACE '&1' WITH LS_MESSTAB-MSGV1 INTO LS_T100-TEXT.
    REPLACE '&2' WITH LS_MESSTAB-MSGV2 INTO LS_T100-TEXT.
    REPLACE '&3' WITH LS_MESSTAB-MSGV3 INTO LS_T100-TEXT.
    REPLACE '&4' WITH LS_MESSTAB-MSGV4 INTO LS_T100-TEXT.
    REPLACE '&'  WITH LS_MESSTAB-MSGV1 INTO LS_T100-TEXT.
    REPLACE '&'  WITH LS_MESSTAB-MSGV2 INTO LS_T100-TEXT.
    REPLACE '&'  WITH LS_MESSTAB-MSGV3 INTO LS_T100-TEXT.
    REPLACE '&'  WITH LS_MESSTAB-MSGV4 INTO LS_T100-TEXT.
    APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
    <L_RETURN>-TYPE    = LS_MESSTAB-MSGTYP.
    <L_RETURN>-ID      = LS_MESSTAB-MSGID.
    <L_RETURN>-NUMBER  = LS_MESSTAB-MSGNR.
    <L_RETURN>-MESSAGE = LS_T100-TEXT.
    IF LS_MESSTAB-MSGTYP EQ 'S'.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
* BOD - CH01 - CR24_046
**&---------------------------------------------------------------------*
**& Form f_get_io_number
**&---------------------------------------------------------------------*
**& No range object -> ZCONNN[ NNN = 1st digit of K2code in number,A=001,Z=026 ]
**& No range number -> NN[ Last 2 digits of K2CODE, 01,02,99 ] with toyear flag
**& From Number -> 0000001000 To Number 0000001999
**& From Number -> 0000002000 To Number 0000002999
**& From Number -> 0000099000 To Number 0000099999
**&---------------------------------------------------------------------*
**&      <-- LF_AUFNR
**&---------------------------------------------------------------------*
*FORM f_get_io_number TABLES pt_return TYPE bapiret2_t
*                    USING us_ordermaster TYPE zsdscos006
*                    CHANGING cf_aufnr TYPE aufk-aufnr.
*
*  DATA: lf_number   TYPE numc10,
*        lf_alphabet TYPE c,
*        lf_object   TYPE inri-object,
*        lf_nr       TYPE inri-nrrangenr,
*        lf_toyear   TYPE gjahr,
*        lf_pos      TYPE numc3.
*
*  CHECK cf_aufnr IS INITIAL.
*
*  lf_alphabet =  us_ordermaster-k2_code(1).
*  TRANSLATE lf_alphabet TO UPPER CASE.
*  FIND lf_alphabet IN sy-abcde MATCH OFFSET lf_pos.
*  IF sy-subrc = 0.
*    lf_pos = lf_pos + 1.
*    UNPACK lf_pos TO lf_pos.
*  ENDIF.
*
*  lf_object = |ZCO{ lf_pos }|.
*  lf_nr = us_ordermaster-k2_code+1(2).
*  lf_toyear = us_ordermaster-k2_year.
*
**..1 check if no range object exist
*  PERFORM f_nr_object_check USING lf_object lf_alphabet.
*
**..2 Check if no range no. exist
*  PERFORM f_nr_range_check USING lf_object lf_toyear lf_nr.
*
**..3 Get IO number
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      nr_range_nr             = lf_nr
*      object                  = lf_object
*      quantity                = '1'
**     SUBOBJECT               = ' '
*      toyear                  = lf_toyear
**     IGNORE_BUFFER           = ' '
*    IMPORTING
*      number                  = lf_number
**     QUANTITY                =
**     RETURNCODE              =
*    EXCEPTIONS
*      interval_not_found      = 1
*      number_range_not_intern = 2
*      object_not_found        = 3
*      quantity_is_0           = 4
*      quantity_is_not_1       = 5
*      interval_overflow       = 6
*      buffer_overflow         = 7
*      OTHERS                  = 8.
*  IF sy-subrc EQ 0.
*    cf_aufnr = |{ us_ordermaster-k2_code }{ us_ordermaster-k2_year+2 }{ lf_number+7 }|.
*  ELSE.
*    APPEND INITIAL LINE TO pt_return ASSIGNING FIELD-SYMBOL(<l_return>).
*    <l_return>-type   = 'E'.
*    <l_return>-id     = sy-msgid.
*    <l_return>-number = sy-msgno.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO <l_return>-message.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_nr_object_check
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LF_OBJNR
**&---------------------------------------------------------------------*
*FORM F_NR_OBJECT_CHECK  USING    UF_OBJECT TYPE INRI-OBJECT
*                                 UF_ALPHABET TYPE C.
*
*  DATA: LS_ATTRIBUTES TYPE TNRO,
*        LS_TEXT       TYPE TNROT,
*        LT_ERROR      TYPE STANDARD TABLE OF INOER.
*
*  CALL FUNCTION 'NUMBER_RANGE_OBJECT_GET_INFO'
*    EXPORTING
*      OBJECT           = UF_OBJECT
*    EXCEPTIONS
*      OBJECT_NOT_FOUND = 1
*      OTHERS           = 2.
*  IF SY-SUBRC <> 0.
*    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
*      EXPORTING
*        OBJECT            = 'ZCO001'
*      IMPORTING
*        OBJECT_ATTRIBUTES = LS_ATTRIBUTES
*        OBJECT_TEXT       = LS_TEXT
*      EXCEPTIONS
*        OBJECT_NOT_FOUND  = 1
*        OTHERS            = 2.
*    IF SY-SUBRC EQ 0.
*
*      LS_ATTRIBUTES-OBJECT = UF_OBJECT.
*      REPLACE 'A%%' IN LS_TEXT-TXT WITH |{ UF_ALPHABET }%%|.
*      CLEAR: LS_ATTRIBUTES-CHANGED_BY, LS_ATTRIBUTES-CHANGED_AT,
*            LS_ATTRIBUTES-UNAME,
*      LS_ATTRIBUTES-UDATE,
*      LS_ATTRIBUTES-UTIME.
*      LS_ATTRIBUTES-ENAME = SY-UNAME.
*      LS_ATTRIBUTES-EDATE = SY-DATUM.
*      LS_ATTRIBUTES-ETIME = SY-UZEIT.
*
*      LS_TEXT-OBJECT = UF_OBJECT.
*      CLEAR: LS_ATTRIBUTES-CHANGED_BY, LS_ATTRIBUTES-CHANGED_AT,
*      LS_TEXT-UNAME,
*      LS_TEXT-UDATE,
*      LS_TEXT-UTIME.
*      LS_TEXT-ENAME = LS_ATTRIBUTES-ENAME.
*      LS_TEXT-EDATE = LS_ATTRIBUTES-EDATE.
*      LS_TEXT-ETIME = LS_ATTRIBUTES-ETIME.
*
** Implement suitable error handling here
*      CALL FUNCTION 'NUMBER_RANGE_OBJECT_UPDATE'
*        EXPORTING
*          INDICATOR                 = 'I'
*          OBJECT_ATTRIBUTES         = LS_ATTRIBUTES
*          OBJECT_TEXT               = LS_TEXT
*        TABLES
*          ERRORS                    = LT_ERROR
*        EXCEPTIONS
*          OBJECT_ALREADY_EXISTS     = 1
*          OBJECT_ATTRIBUTES_MISSING = 2
*          OBJECT_NOT_FOUND          = 3
*          OBJECT_TEXT_MISSING       = 4
*          WRONG_INDICATOR           = 5
*          OTHERS                    = 6.
*      IF SY-SUBRC EQ 0.
*        CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
*          EXPORTING
*            OBJECT                 = LS_ATTRIBUTES-OBJECT
*          EXCEPTIONS
*            OBJECT_NOT_INITIALIZED = 1.
*        IF SY-SUBRC NE 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ELSE.
*          COMMIT WORK AND WAIT.
*        ENDIF.
*      ELSE.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_nr_range_check
**&---------------------------------------------------------------------*
**& No range number -> NN[ Last 2 digits of K2CODE, 01,02,99 ] with toyear flag
**& From Number -> 0000001000 To Number 0000001999
**& From Number -> 0000002000 To Number 0000002999
**& From Number -> 0000099000 To Number 0000099999
**&---------------------------------------------------------------------*
**&      --> LF_OBJECT
**&      --> LF_TOYEAR
**&      --> LF_NR
**&---------------------------------------------------------------------*
*FORM F_NR_RANGE_CHECK  USING    UF_OBJECT TYPE INRI-OBJECT
*                                UF_TOYEAR TYPE GJAHR
*                                UF_NR     TYPE INRI-NRRANGENR.
*
*  DATA: LT_INTERVALS TYPE STANDARD TABLE OF INRIV,
*        LT_ERROR     TYPE STANDARD TABLE OF INRIV.
*
*  SELECT * FROM NRIV INTO @DATA(LS_NRIV) UP TO 1 ROWS ##NEEDED "#EC CI_ALL_FIELDS_NEEDED
*    WHERE OBJECT EQ @UF_OBJECT
*      AND SUBOBJECT EQ @SPACE
*      AND NRRANGENR EQ @UF_NR
*      AND TOYEAR    EQ @UF_TOYEAR .
*  ENDSELECT.
*  IF SY-SUBRC NE 0.
*    APPEND INITIAL LINE TO LT_INTERVALS ASSIGNING FIELD-SYMBOL(<L_INTERVALS>).
*    <L_INTERVALS>-NRRANGENR = UF_NR.
*    <L_INTERVALS>-TOYEAR = UF_TOYEAR.
*    <L_INTERVALS>-FROMNUMBER = '0000001000'.
*    REPLACE '01' IN <L_INTERVALS>-FROMNUMBER WITH UF_NR.
*    <L_INTERVALS>-TONUMBER   = '0000001999'.
*    REPLACE '01' IN <L_INTERVALS>-TONUMBER WITH UF_NR.
*    <L_INTERVALS>-PROCIND = 'I'.
*
*    CALL FUNCTION 'NUMBER_RANGE_UPDATE_INIT'
*      EXPORTING
*        OBJECT           = UF_OBJECT
*      EXCEPTIONS
*        OBJECT_NOT_FOUND = 1
*        OTHERS           = 2.
*    IF SY-SUBRC NE 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
*      EXPORTING
*        OBJECT           = UF_OBJECT
*      TABLES
*        ERROR_IV         = LT_ERROR
*        INTERVAL         = LT_INTERVALS
*      EXCEPTIONS
*        OBJECT_NOT_FOUND = 1
*        OTHERS           = 2.
*    IF SY-SUBRC NE 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
*      EXPORTING
*        OBJECT                 = UF_OBJECT
*        COMMIT                 = 'X'
*      EXCEPTIONS
*        NO_CHANGES_MADE        = 1
*        OBJECT_NOT_INITIALIZED = 2
*        OTHERS                 = 3.
*    IF SY-SUBRC NE 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ELSE.
*    CLEAR LS_NRIV.
*  ENDIF.
*
*ENDFORM.
* EOD - CH01 - CR24_046
*&---------------------------------------------------------------------*
*& Form f_get_cosp_tab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_DATA
*&      --> SPACE
*&      --> LS_AUFK_AUFNR
*&      --> IM_K2_PERIOD_FR
*&      --> IM_K2_PERIOD_TO
*&      --> LS_COSP
*&---------------------------------------------------------------------*
FORM F_GET_COSP_TAB TABLES   PT_TABLE TYPE ZSDSCOS001_TT
                      USING    UV_CCTR TYPE ZSDSDE_K2CCA
                               UV_FROM TYPE POPER
                               UV_TO TYPE POPER
                               US_COSP TYPE TY_COSP.
  DATA: LF_FROM   TYPE POPER,
        LS_DATA   TYPE ZSDSCOS002,
        LF_SOURCE TYPE C LENGTH 20,
        LF_TARGET TYPE C LENGTH 20,
        LF_TOTAL  TYPE C LENGTH 20.

  LF_FROM = UV_FROM.
  CLEAR LS_DATA.
*
  DO.
    LF_SOURCE = |US_COSP-WOG{ LF_FROM }|.
    LF_TARGET = |LS_DATA-WOG{ LF_FROM }|.
    LF_TOTAL  = 'LS_DATA-WOGTOT'.

    ASSIGN (LF_SOURCE) TO FIELD-SYMBOL(<L_SOURCE>).
    IF SY-SUBRC EQ 0.
      ASSIGN (LF_TARGET) TO FIELD-SYMBOL(<L_TARGET>).
      IF SY-SUBRC EQ 0.
        <L_TARGET> = <L_TARGET> + <L_SOURCE>.
        ASSIGN (LF_TOTAL) TO FIELD-SYMBOL(<L_TOTAL>).
        IF SY-SUBRC EQ 0.
*          ADD <l_source> TO <l_total>.
          <L_TOTAL> = <L_TOTAL> + <L_SOURCE>.
        ENDIF.
      ENDIF.
    ENDIF.

    LF_FROM = LF_FROM + 1.

    IF LF_FROM GT UV_TO.
      EXIT.
    ENDIF.
  ENDDO.

  LS_DATA-WAERS  = 'THB'.
  LS_DATA-K2_RESP_CCA = UV_CCTR.
  LS_DATA-K2_GL  = US_COSP-KSTAR.
  LS_DATA-K2_PERIOD_FR = UV_FROM.
  LS_DATA-K2_PERIOD_TO = UV_TO.
  LS_DATA-K2_BG_YEAR = US_COSP-GJAHR.
  COLLECT LS_DATA INTO PT_TABLE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_reverse_sign
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_COSP
*&      <-- LS_COSP
*&---------------------------------------------------------------------*
FORM F_REVERSE_SIGN
##NEEDED                    CHANGING CS_COSP TYPE TY_COSP.
  ##NEEDED
  DATA: LF_FROM   TYPE POPER,
        LF_TARGET TYPE C LENGTH 20.
  CONSTANTS: LC_NUM    TYPE NUMC2 VALUE 12.

  DO LC_NUM TIMES.
    LF_FROM = LF_FROM + 1.
    LF_TARGET = |CS_COSP-WOG{ LF_FROM }|.
    ASSIGN (LF_TARGET) TO FIELD-SYMBOL(<L_TARGET>).
    IF SY-SUBRC EQ 0.
      <L_TARGET> = <L_TARGET> * -1.
    ENDIF.
  ENDDO.

ENDFORM.
* BOI - CH01 - CR24_046
*---------------------------------------------------------------------*
* Form f_get_io_number
*---------------------------------------------------------------------*
* Get number range object and number range number by K2 activity code
* from table ZSDSCOC001
*---------------------------------------------------------------------*
FORM F_GET_IO_NUMBER TABLES PT_RETURN TYPE BAPIRET2_T
                      USING US_ORDERMASTER TYPE ZSDSCOS006
                   CHANGING CF_AUFNR TYPE AUFK-AUFNR.

  DATA: LF_NUMBER TYPE NUMC10,
        LF_OBJECT TYPE INRI-OBJECT,
        LF_NR     TYPE INRI-NRRANGENR,
        LF_TOYEAR TYPE GJAHR.

  CHECK CF_AUFNR IS INITIAL.

* Get number range object from K2 activity code
  SELECT SINGLE K2_CODE,
                NR_OBJECT,
                NR_NUMBER
    FROM ZSDSCOC001
    INTO @DATA(LS_ZSDSCOC001)
    WHERE K2_CODE EQ @US_ORDERMASTER-K2_CODE.

  IF SY-SUBRC NE 0.
*   Number range object not found for activity &. Please check ZSDSCOC001.
    APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).

    <L_RETURN>-TYPE   = 'E'.
    <L_RETURN>-ID     = 'ZSDSCO01'.
    <L_RETURN>-NUMBER = '001'.
    MESSAGE ID <L_RETURN>-ID TYPE <L_RETURN>-TYPE NUMBER <L_RETURN>-NUMBER
      WITH US_ORDERMASTER-K2_CODE INTO <L_RETURN>-MESSAGE.
    RETURN.
  ENDIF.

  LF_OBJECT = LS_ZSDSCOC001-NR_OBJECT.
  LF_NR = LS_ZSDSCOC001-NR_NUMBER.
  LF_TOYEAR = US_ORDERMASTER-K2_YEAR.

*..1 check if no range object exist
  PERFORM F_NR_OBJECT_CHECK TABLES PT_RETURN
                             USING LF_OBJECT.
  READ TABLE PT_RETURN WITH KEY TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

*..2 Check if no range no. exist
  PERFORM F_NR_RANGE_CHECK TABLES PT_RETURN
                            USING LF_OBJECT
                                  LF_TOYEAR
                                  LF_NR.

*..3 Get IO number
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = LF_NR
      OBJECT                  = LF_OBJECT
      QUANTITY                = '1'
      TOYEAR                  = LF_TOYEAR
    IMPORTING
      NUMBER                  = LF_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC EQ 0.
    CF_AUFNR = |{ US_ORDERMASTER-K2_CODE }{ US_ORDERMASTER-K2_YEAR+2 }{ LF_NUMBER+6(4) }|.
  ELSE.
    APPEND INITIAL LINE TO PT_RETURN ASSIGNING <L_RETURN>.
    <L_RETURN>-TYPE   = 'E'.
    <L_RETURN>-ID     = SY-MSGID.
    <L_RETURN>-NUMBER = SY-MSGNO.
    MESSAGE ID <L_RETURN>-ID TYPE <L_RETURN>-TYPE NUMBER <L_RETURN>-NUMBER
      INTO <L_RETURN>-MESSAGE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form f_nr_object_check
*---------------------------------------------------------------------*
* Check number range object exist?
*---------------------------------------------------------------------*
FORM F_NR_OBJECT_CHECK  TABLES PT_RETURN TYPE BAPIRET2_T
                         USING UF_OBJECT TYPE INRI-OBJECT.

  CALL FUNCTION 'NUMBER_RANGE_OBJECT_GET_INFO'
    EXPORTING
      OBJECT           = UF_OBJECT
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.

  IF SY-SUBRC NE 0.
    APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
    <L_RETURN>-TYPE   = 'E'.
    <L_RETURN>-ID     = SY-MSGID.
    <L_RETURN>-NUMBER = SY-MSGNO.
    MESSAGE ID <L_RETURN>-ID TYPE <L_RETURN>-TYPE NUMBER <L_RETURN>-NUMBER
      INTO <L_RETURN>-MESSAGE.
    RETURN.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form f_nr_range_check
*---------------------------------------------------------------------*
* Check number range number to-year flag
* If not found number range number for the specified year
* Then copy the number range number interval setting from previous year
*---------------------------------------------------------------------*
FORM F_NR_RANGE_CHECK   TABLES PT_RETURN TYPE BAPIRET2_T
                         USING UF_OBJECT TYPE INRI-OBJECT
                               UF_TOYEAR TYPE GJAHR
                               UF_NR     TYPE INRI-NRRANGENR.

  DATA: LT_INTERVALS TYPE STANDARD TABLE OF INRIV,
        LT_ERROR     TYPE STANDARD TABLE OF INRIV,
        LF_PREV_YEAR TYPE GJAHR.

* Check number range number exist for the specified year?
  SELECT *
    FROM NRIV
    INTO @DATA(LS_NRIV)
    UP TO 1 ROWS                ##NEEDED      "#EC CI_ALL_FIELDS_NEEDED
    WHERE OBJECT EQ @UF_OBJECT
      AND SUBOBJECT EQ @SPACE
      AND NRRANGENR EQ @UF_NR
      AND TOYEAR    EQ @UF_TOYEAR .
  ENDSELECT.

  IF SY-SUBRC NE 0.
*   If not found, then copy the number range number interval setting from
*   previous year
    LF_PREV_YEAR = UF_TOYEAR - 1.

    SELECT *
      FROM NRIV
      INTO @DATA(LS_NRIV_PREV)
      UP TO 1 ROWS                 ##NEEDED   "#EC CI_ALL_FIELDS_NEEDED
      WHERE OBJECT EQ @UF_OBJECT
        AND SUBOBJECT EQ @SPACE
        AND NRRANGENR EQ @UF_NR
        AND TOYEAR    EQ @LF_PREV_YEAR.
    ENDSELECT.

    IF SY-SUBRC NE 0.
*     NR Internal not found for <NR Object> <NR Number> <To-Year>
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).

      <L_RETURN>-TYPE   = 'E'.
      <L_RETURN>-ID     = 'ZSDSCO01'.
      <L_RETURN>-NUMBER = '002'.

      MESSAGE ID <L_RETURN>-ID TYPE <L_RETURN>-TYPE NUMBER <L_RETURN>-NUMBER
           WITH UF_OBJECT UF_NR UF_TOYEAR INTO <L_RETURN>-MESSAGE.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO LT_INTERVALS ASSIGNING FIELD-SYMBOL(<L_INTERVALS>).
    <L_INTERVALS>-NRRANGENR = UF_NR.
    <L_INTERVALS>-TOYEAR = UF_TOYEAR.
    <L_INTERVALS>-FROMNUMBER = LS_NRIV_PREV-FROMNUMBER.
    <L_INTERVALS>-TONUMBER   = LS_NRIV_PREV-TONUMBER.
    <L_INTERVALS>-PROCIND = 'I'.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_INIT'
      EXPORTING
        OBJECT           = UF_OBJECT
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.
    IF SY-SUBRC NE 0.
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING <L_RETURN>.
      <L_RETURN>-TYPE   = 'E'.
      <L_RETURN>-ID     = SY-MSGID.
      <L_RETURN>-NUMBER = SY-MSGNO.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO <L_RETURN>-MESSAGE.
      RETURN.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
      EXPORTING
        OBJECT           = UF_OBJECT
      TABLES
        ERROR_IV         = LT_ERROR
        INTERVAL         = LT_INTERVALS
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.
    IF SY-SUBRC NE 0.
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING <L_RETURN>.
      <L_RETURN>-TYPE   = 'E'.
      <L_RETURN>-ID     = SY-MSGID.
      <L_RETURN>-NUMBER = SY-MSGNO.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO <L_RETURN>-MESSAGE.
      RETURN.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        OBJECT                 = UF_OBJECT
        COMMIT                 = 'X'
      EXCEPTIONS
        NO_CHANGES_MADE        = 1
        OBJECT_NOT_INITIALIZED = 2
        OTHERS                 = 3.
    IF SY-SUBRC NE 0.
      APPEND INITIAL LINE TO PT_RETURN ASSIGNING <L_RETURN>.
      <L_RETURN>-TYPE   = 'E'.
      <L_RETURN>-ID     = SY-MSGID.
      <L_RETURN>-NUMBER = SY-MSGNO.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO <L_RETURN>-MESSAGE.
      RETURN.
    ENDIF.
  ELSE.
    CLEAR LS_NRIV.
  ENDIF.

ENDFORM.
* EOI - CH01 - CR24_046
