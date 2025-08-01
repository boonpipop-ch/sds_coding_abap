*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0110_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_DATA .
*  IF go_data IS INITIAL.
*    CREATE OBJECT go_data.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM PF_STATUS_1 USING US_EXTAB TYPE SLIS_T_EXTAB.

  CONSTANTS LC_STATUS TYPE C LENGTH 9 VALUE 'ZSTANDARD'.

  SET PF-STATUS LC_STATUS EXCLUDING US_EXTAB.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE P_UCOMM.
    WHEN '&IC1'.
      PERFORM F_CALL_TRANSECTION.
    WHEN 'ALL'.
      PERFORM F_CHECK_BOX USING 'X'.
    WHEN 'SAL'.
      PERFORM F_CHECK_BOX USING SPACE.
  ENDCASE.

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_TRANSECTION.
  DATA: LE_ROW     TYPE I,
        LE_VALUE   TYPE C,
        LE_COL     TYPE I,
        LES_ROW_ID TYPE LVC_S_ROW,
        LES_COL_ID TYPE LVC_S_COL,
        LES_ROW_NO TYPE LVC_S_ROID.

  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.
  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
  ENDIF.

  CALL METHOD REF_GRID->GET_CURRENT_CELL
    IMPORTING
      E_ROW     = LE_ROW
      E_VALUE   = LE_VALUE
      E_COL     = LE_COL
      ES_ROW_ID = LES_ROW_ID
      ES_COL_ID = LES_COL_ID.

  CLEAR : BDCDATA[],MESSTAB[].

  READ TABLE GT_RESULT INTO GS_RESULT INDEX LES_ROW_ID-INDEX.
  IF SY-SUBRC = 0.

  ENDIF.

ENDFORM.                    " F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = GC_X.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION  USING    TCODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
  DATA: LV_OPT  TYPE CTU_PARAMS.

  LV_OPT-DISMODE  = GC_E."'A'
  LV_OPT-UPDMODE  = GC_L.
  LV_OPT-NOBINPT  = GC_X.
  "lv_opt-cattmode = 'A'.
  "lv_opt-defsize  = 'X'.

  CALL TRANSACTION TCODE USING BDCDATA
*                   MODE   'E'"N
*                   UPDATE 'L'
                   OPTIONS FROM LV_OPT
                   MESSAGES INTO MESSTAB.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0645   text
*----------------------------------------------------------------------*
FORM F_CHECK_BOX  USING LV_CHECK.
  GS_RESULT-CHECK = LV_CHECK.
  MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING CHECK
                                         WHERE CHECK NE GS_RESULT-CHECK.

ENDFORM.                    " F_CHECK_BOX
*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN .

*  IF SY-UNAME CS 'TIS'.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ 'M01'.
*        SCREEN-INPUT = 0.
*
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.

ENDFORM. " SET_SCREEN

*&      Form  SET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATE .

  DATA LV_DATE TYPE SY-DATUM.

  LV_DATE = SY-DATUM.
  LV_DATE = LV_DATE - 1.

  P_DATE = LV_DATE.

ENDFORM. " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATE .

  DATA: LV_DATE         TYPE CHAR25.

  DATA: LV_CURRENT_DATE TYPE SY-DATUM.

  DATA: L_TBL_HOLIDAY   TYPE TABLE OF ISCAL_DAY.

  DATA: LV_VALID        TYPE TCURR-GDATU.


  DATA: L_TBL_DATA      TYPE TABLE OF TY_DATA.
  DATA: LW_DATA         TYPE TY_DATA.

  IF P_DATE IS INITIAL.
    MESSAGE 'Valid Date must be input'(E01) TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
**Check bank holiday
    IF P_MANUAL ='X'.
      LV_CURRENT_DATE = SY-DATUM.

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          HOLIDAY_CALENDAR = 'ZB'
          DATE_FROM        = P_DATE
          DATE_TO          = P_DATE
        TABLES
          HOLIDAYS         = L_TBL_HOLIDAY.
      IF L_TBL_HOLIDAY IS NOT INITIAL.
        MESSAGE 'Data not found because valid date is not Bank working day'(E01) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.

*.. Beg of Deletion by Ratchapol K. TISAD-2852
*      PERFORM check_duplicate.
*
*    ELSE.
*      READ TABLE gt_data_xml INTO gw_data_xml WITH KEY tag_name = 'dc:date'.
*      IF gw_data_xml IS NOT INITIAL.
*        lv_date = gw_data_xml-tag_value.
*        CONCATENATE   lv_date+0(4) lv_date+5(2) lv_date+8(2) INTO gv_date.
*      ENDIF.
*.. End of Deletion By Ratchapol K. TISAD-2852
    ENDIF.

  ENDIF.
ENDFORM. " CHECK_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_xml .
FORM GET_XML USING LV_RATE TYPE TCURR-FCURR.
*.. Start of Insertion by Ratchapol K. TISAD-2852
  DATA: LV_DATE         TYPE CHAR25.

  DATA: LV_START_DATE TYPE CHAR10,
        LV_END_DATE   TYPE CHAR10,
        LV_CURRENCY   TYPE CHAR10.

  DATA: LV_MESTYPE TYPE CHAR1,
        LV_MESSAGE TYPE STRING.

  DATA: LV_XMLDATA      TYPE XSTRING.

  CONCATENATE P_DATE+0(4)
              '-'
              P_DATE+4(2)
              '-'
              P_DATE+6(2)
         INTO LV_START_DATE.

  CONCATENATE P_DATE+0(4)
              '-'
              P_DATE+4(2)
              '-'
              P_DATE+6(2)
         INTO LV_END_DATE.
  LV_CURRENCY = LV_RATE.

  CALL FUNCTION 'Z_SDSFI_GET_EXCHANGE_RATE'
    EXPORTING
      IV_START_DATE      = LV_START_DATE
      IV_END_DATE        = LV_END_DATE
      IV_CURRENCY        = LV_CURRENCY
      IV_UPDATE_K2       = CB_K2
    IMPORTING
      ES_EXCHANGE_RATE   = GT_EXCHANGE_RATE
      EV_MESTYPE         = LV_MESTYPE
      EV_MESSAGE         = LV_MESSAGE
    EXCEPTIONS
      CURRENCY_NOT_EXIST = 1
      DATE_IS_INVALID    = 2
      API_URL_NOT_FOUND  = 3
      OTHERS             = 4.
  IF LV_MESTYPE       EQ GC_S AND
     GT_EXCHANGE_RATE IS NOT INITIAL.
    READ TABLE GT_EXCHANGE_RATE INTO GS_EXCHANGE_RATE INDEX 1.
    IF P_MANUAL ='X' AND
     GV_DATE IS INITIAL.
      PERFORM CHECK_DUPLICATE.
    ENDIF.

    IF P_AUTO = 'X' AND
       GV_DATE IS INITIAL.
      IF GS_EXCHANGE_RATE-PERIOD IS NOT INITIAL.
        LV_DATE = GS_EXCHANGE_RATE-PERIOD.
        CONCATENATE   LV_DATE+0(4) LV_DATE+5(2) LV_DATE+8(2) INTO GV_DATE.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE LV_MESSAGE TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM. " GET_XML
*&---------------------------------------------------------------------*
*&      Form  GET_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RATE .

  DATA: BEGIN OF LS_RATE,
          REPID        TYPE  ZSDSCAC001-REPID,
          PARAM        TYPE  ZSDSCAC001-PARAM,
          PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
          SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
          PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
          PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
          VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
          VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
          VDESC        TYPE  ZSDSCAC001-VDESC,
        END OF LS_RATE.

  DATA : LT_GEN_C  LIKE STANDARD TABLE OF LS_RATE.

  DATA : LV_REPID TYPE PROGRAMM,
         LT_PARAM TYPE RANGE OF ZSDSCAC001-PARAM,
         LT_EXT   TYPE RANGE OF ZSDSCAC001-PARAM_EXT.

  DATA : LS_PARAM LIKE LINE OF lt_PARAM,
         LS_EXT   LIKE LINE OF lt_EXT.

  CONSTANTS : BEGIN OF LC_CON,
                SIGN   TYPE C LENGTH 1 VALUE 'I',
                OPTION TYPE C LENGTH 2 VALUE 'EQ',
                LOW    TYPE C LENGTH 7 VALUE 'EX_RATE',
              END OF LC_CON.

  LV_REPID = SY-REPID.

  LS_PARAM-SIGN   = LC_CON-SIGN.
  LS_PARAM-OPTION = LC_CON-OPTION.
  LS_PARAM-LOW    = LC_CON-LOW.
  APPEND LS_PARAM TO LT_PARAM.

  ZCL_SDSCA_UTILITIES=>GET_GEN_C( EXPORTING IF_REPID  = LV_REPID
                                            IRT_PARAM = LT_PARAM
                                            IRT_EXT   = LT_EXT
                                 IMPORTING  ET_GEN_C  = LT_GEN_C  ).

  LOOP AT lt_GEN_C INTO lS_RATE.
    GW_RATE = LS_RATE-VALUE_LOW.
    APPEND GW_RATE TO GT_RATE.
  ENDLOOP.
ENDFORM. " GET_RATE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
*-> variables
  DATA: L_HEAD       TYPE CHAR300,
        L_RATE_TYPE  TYPE TCURR-KURST,
        L_RATE_D     TYPE CHAR100,
        L_TO_CURR    TYPE TCURR-TCURR,
        L_FROM_CURR  TYPE TCURR-FCURR,
        L_EX_RATE    TYPE TCURR-UKURS,
        L_RATIO_FROM TYPE TCURR-FFACT,
        L_RATIO_TO   TYPE TCURR-TFACT,
        L_CAL        TYPE I.

  DATA: LS_DATA_XML     LIKE GW_DATA_XML.

  DATA: LV_RATIO_FROM   TYPE STRING.

  CLEAR: L_RATE_TYPE,L_TO_CURR,L_TO_CURR,L_FROM_CURR ,L_EX_RATE,L_RATIO_FROM,L_RATIO_TO,L_CAL.

*.. Beg of Insertion by Ratchapol K. TISAD-2852
  SELECT KURST FCURR TCURR FFACT TFACT
    INTO TABLE GT_RATIO
    FROM TCURF
    FOR ALL ENTRIES IN GT_RATE
    WHERE FCURR = GT_RATE-RATE
      AND TCURR = 'THB'.

  LOOP AT GT_RATE INTO GW_RATE.
    CLEAR GT_DATA_XML[].
    PERFORM GET_XML USING GW_RATE-RATE.

    IF GT_EXCHANGE_RATE IS NOT INITIAL.
*.. To Currency
      L_TO_CURR  =  'THB'.
*.. From Currency Rate (USD, JPY, Etc..)
      L_FROM_CURR = GS_EXCHANGE_RATE-CURRENCY_ID.
**.. Buying Sight

*.. Thailand Average Buying Transfer, Buying Rate
      IF GS_EXCHANGE_RATE-BUYING_TRANSFER IS NOT INITIAL.
        L_CAL       = L_CAL + 1.
        L_RATE_TYPE = 'G'.
        L_EX_RATE   = GS_EXCHANGE_RATE-BUYING_TRANSFER.
        "Check Convert Rate - Example BOT API compare 1:1 but the accutally is 100:1
*        READ TABLE GT_RATIO INTO GW_RATIO WITH KEY FROM_CURR = L_FROM_CURR
*                                                   TO_CURR   = 'THB'.
*        IF SY-SUBRC IS INITIAL.
*          LV_RATIO_FROM = GW_RATIO-RATIO_FROM.
*          CONDENSE LV_RATIO_FROM.
*          IF  GS_EXCHANGE_RATE-CURRENCY_ID IS NOT INITIAL  AND
*              GS_EXCHANGE_RATE-CURRENCY_ID NE LV_RATIO_FROM AND
*              GW_RATIO-RATIO_FROM          GT 1.
*            L_EX_RATE   = GS_EXCHANGE_RATE-BUYING_TRANSFER * GW_RATIO-RATIO_FROM.
*          ENDIF.
*        ENDIF.

        IF CB_G EQ ABAP_TRUE.
          APPEND INITIAL LINE TO GT_XML ASSIGNING <FS_XML>.
          <FS_XML>-CAL        = L_CAL.
          <FS_XML>-TO_CURR    = L_TO_CURR.
          <FS_XML>-FROM_CURR  = L_FROM_CURR .
          <FS_XML>-EX_RATE    = L_EX_RATE.
          <FS_XML>-RATE_TYPE  = L_RATE_TYPE.
          <FS_XML>-RATE_DE    = L_RATE_D.
          <FS_XML>-VALID_FROM = GV_DATE.
        ENDIF.
      ENDIF.

*.. Average Selling, Selling Rate
      IF GS_EXCHANGE_RATE-SELLING IS NOT INITIAL.
        L_CAL       = L_CAL + 1.
        L_RATE_TYPE = 'B'.
        L_EX_RATE   = GS_EXCHANGE_RATE-SELLING.
        "Check Convert Rate - Example BOT API compare 1:1 but the accutally is 100:1
*        READ TABLE GT_RATIO INTO GW_RATIO WITH KEY FROM_CURR = L_FROM_CURR
*                                                   TO_CURR   = 'THB'.
*        IF SY-SUBRC IS INITIAL.
*          LV_RATIO_FROM = GW_RATIO-RATIO_FROM.
*          CONDENSE LV_RATIO_FROM.
*          IF  GS_EXCHANGE_RATE-CURRENCY_ID IS NOT INITIAL  AND
*              GS_EXCHANGE_RATE-CURRENCY_ID NE LV_RATIO_FROM AND
*              GW_RATIO-RATIO_FROM          GT 1.
*            L_EX_RATE   = GS_EXCHANGE_RATE-SELLING * GW_RATIO-RATIO_FROM.
*          ENDIF.
*        ENDIF.
        IF CB_B EQ ABAP_TRUE.
          APPEND INITIAL LINE TO GT_XML ASSIGNING <FS_XML>.
          <FS_XML>-CAL        = L_CAL.
          <FS_XML>-TO_CURR    = L_TO_CURR.
          <FS_XML>-FROM_CURR  = L_FROM_CURR .
          <FS_XML>-EX_RATE    = L_EX_RATE.
          <FS_XML>-RATE_TYPE  = L_RATE_TYPE.
          <FS_XML>-RATE_DE    = L_RATE_D.
          <FS_XML>-VALID_FROM = GV_DATE.
        ENDIF.
        IF CB_M EQ ABAP_TRUE.
          APPEND INITIAL LINE TO GT_XML ASSIGNING <FS_XML>.
          L_CAL               = L_CAL + 1.
          <FS_XML>-CAL        = L_CAL.
          <FS_XML>-TO_CURR    = L_TO_CURR.
          <FS_XML>-FROM_CURR  = L_FROM_CURR .
          <FS_XML>-EX_RATE    = L_EX_RATE.
          <FS_XML>-RATE_DE  = L_RATE_D.
          <FS_XML>-RATE_TYPE  = 'M'.
          <FS_XML>-VALID_FROM = GV_DATE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_XML INTO GW_XML.
    READ TABLE GT_RATE INTO GW_RATE WITH KEY RATE = GW_XML-FROM_CURR.
    IF SY-SUBRC IS NOT INITIAL.
      DELETE GT_XML .
    ENDIF.

    READ TABLE GT_RATIO INTO GW_RATIO WITH KEY RATE_TYPE = GW_XML-RATE_TYPE
                                               FROM_CURR = GW_XML-FROM_CURR
                                               TO_CURR   = GW_XML-TO_CURR.
    IF SY-SUBRC IS INITIAL.
      GW_XML-RATIO_FROM = GW_RATIO-RATIO_FROM.
      GW_XML-RATIO_TO   = GW_RATIO-RATIO_TO.
      MODIFY GT_XML FROM GW_XML.
    ENDIF.

  ENDLOOP.

  SORT GT_XML BY FROM_CURR CAL."BY cal rate_type from_curr.
  SORT GT_RATE BY RATE.

  LOOP AT GT_RATE INTO GW_RATE. "for sort rate type by B M G
    LOOP AT GT_XML ASSIGNING <FS_XML> WHERE FROM_CURR = GW_RATE-RATE.
      IF <FS_XML>-RATE_TYPE = 'B'.
        <FS_XML>-CAL  = 1.
      ELSEIF <FS_XML>-RATE_TYPE = 'M'.
        <FS_XML>-CAL  = 2.
      ELSEIF <FS_XML>-RATE_TYPE = 'G'.
        <FS_XML>-CAL  = 3.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT GT_XML BY FROM_CURR CAL.


ENDFORM. " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT .

  PERFORM BUILD_FCATALOG USING:
             'RATE_TYPE'  'GT_XML' 'Exchange Rate Type',
             'VALID_FROM' 'GT_XML' 'Valid Date',
             'FROM_CURR'  'GT_XML' 'Currency From',
             'TO_CURR'    'GT_XML' 'Currency To',
             'EX_RATE'    'GT_XML' 'Exchange Rate'.
ENDFORM. " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_EVENTS .
  CLEAR :GT_EVENTS, GT_EVENTS[].
  GW_EVENTS-NAME = 'TOP_OF_PAGE'."Event Name
  GW_EVENTS-FORM = 'TOP_OF_PAGE'."Callback event subroutine
  APPEND GW_EVENTS TO GT_EVENTS.
  CLEAR  GW_EVENTS.
ENDFORM. " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
*  gw_layout-colwidth_optimize = 'X'.
  GW_LAYOUT-ZEBRA             = 'X'.

ENDFORM. " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0777   text
*      -->P_0778   text
*      -->P_0779   text
*----------------------------------------------------------------------*
FORM BUILD_FCATALOG USING L_FIELD
                             L_TAB
                             L_TEXT.

  GW_FIELDCAT-FIELDNAME      = L_FIELD.
  GW_FIELDCAT-TABNAME        = L_TAB.
  GW_FIELDCAT-SELTEXT_M      = L_TEXT.
  GW_FIELDCAT-JUST           = 'C'.
  GW_FIELDCAT-OUTPUTLEN      = '30'.

  APPEND GW_FIELDCAT TO GT_FIELDCAT.
  CLEAR GW_FIELDCAT.

ENDFORM. " BUILD_FCATALOG
*&---------------------------------------------------------------------*
*&      Form  LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_DISPLAY .

  DATA:L_PROGRAM TYPE SY-REPID.
  L_PROGRAM = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = L_PROGRAM
      I_CALLBACK_PF_STATUS_SET = 'SET_STATUS'
      I_CALLBACK_USER_COMMAND  = 'SET_USER_COMMAND'
      IS_LAYOUT                = GW_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
      IT_EVENTS                = GT_EVENTS
    TABLES
      T_OUTTAB                 = GT_XML
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM. " LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DATA :
    LT_HEADER TYPE SLIS_T_LISTHEADER,
    LW_HEADER LIKE LINE OF LT_HEADER.
  DATA:
  L_DATE TYPE CHAR10.
  LW_HEADER-TYP  = 'H'.

  CONCATENATE  GV_DATE+6(2) GV_DATE+4(2) GV_DATE+0(4)  INTO L_DATE SEPARATED BY '.'.

  CONCATENATE  'Foreign Exchage Rate Valid Date as of :' L_DATE  INTO LW_HEADER-INFO SEPARATED BY SPACE.
  APPEND LW_HEADER TO LT_HEADER.
  CLEAR LW_HEADER.

  LW_HEADER-TYP  = 'S'.
  LW_HEADER-INFO = SY-TITLE.
  APPEND LW_HEADER TO LT_HEADER.
  CLEAR LW_HEADER.

  LW_HEADER-TYP  = 'S'.
  CONCATENATE  'Update by : '  SY-UNAME INTO LW_HEADER-INFO SEPARATED BY SPACE.
  APPEND LW_HEADER TO LT_HEADER.
  CLEAR LW_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_HEADER.

ENDFORM. "top_of_page

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_STATUS USING F_TBL_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'PF_STATUS'.

*  SET PF-STATUS 'STANDARD' EXCLUDING f_tbl_extab.
ENDFORM. " SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  SET_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_USER_COMMAND USING E_UCOMM LIKE SY-UCOMM
                              F_SELFIELD TYPE SLIS_SELFIELD.

  CASE E_UCOMM.
    WHEN '&EXE'.
      PERFORM CHECK_DUPLICATE.
      PERFORM UPDATE_RATE_MANUAL.
  ENDCASE.

ENDFORM. " SET_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_RATE_MANUAL .
  DATA :LV_EXCH_RATE     LIKE BAPI1093_0.
  DATA :LV_FLAG TYPE CHAR1.
** Update Exchange Rate.

  IF GT_XML IS NOT INITIAL.
    LOOP AT GT_XML INTO GW_XML.

      LV_EXCH_RATE-RATE_TYPE = GW_XML-RATE_TYPE.
      LV_EXCH_RATE-FROM_CURR = GW_XML-FROM_CURR.
      LV_EXCH_RATE-TO_CURRNCY = GW_XML-TO_CURR.
      LV_EXCH_RATE-VALID_FROM = GW_XML-VALID_FROM .
      LV_EXCH_RATE-EXCH_RATE = GW_XML-EX_RATE.
      LV_EXCH_RATE-FROM_FACTOR = GW_XML-RATIO_FROM.
      LV_EXCH_RATE-TO_FACTOR = GW_XML-RATIO_TO .

      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
        EXPORTING
          EXCH_RATE = LV_EXCH_RATE
          UPD_ALLOW = ''
          CHG_FIXED = ''
        IMPORTING
          RETURN    = GW_RETURN.
      APPEND GW_RETURN TO GT_RETURN.

      IF GW_RETURN-TYPE NE 'E' and
         GW_RETURN-TYPE NE 'A'.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT   = 'X'
          IMPORTING
            RETURN = GW_COMMIT_RETURN.
        LV_FLAG = 'X'.
      ELSE.
        MESSAGE 'Can’t update exchange rate due to duplicate data'(E05) TYPE 'S' DISPLAY LIKE 'E'.
*        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
*      CLEAR: wa_intern, return, exch_rate.
    ENDLOOP.
    IF LV_FLAG = 'X'.
      MESSAGE 'Update Successful'(E07) TYPE 'S'  .
      LEAVE TO CURRENT TRANSACTION.
    ENDIF.
  ELSE.
    MESSAGE 'No data for update Exchange Rate'(E05) TYPE 'E'.
  ENDIF.

ENDFORM. " UPDATE_RATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_DUPLICATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DUPLICATE .

  DATA: LV_DATE         TYPE CHAR25.
  DATA: LV_TEXT         TYPE CHAR100.

  DATA: LV_VALID        TYPE TCURR-GDATU.


  DATA: L_TBL_DATA      TYPE TABLE OF TY_DATA.
  DATA: LW_DATA         TYPE TY_DATA.

  IF GS_EXCHANGE_RATE-PERIOD IS NOT INITIAL.
    LV_DATE = GS_EXCHANGE_RATE-PERIOD.
    CONCATENATE LV_DATE+0(4) LV_DATE+5(2) LV_DATE+8(2) INTO GV_DATE.
  ENDIF.

  IF GV_DATE <> P_DATE.
    CONCATENATE P_DATE+6(2) '.' P_DATE+4(2) '.' P_DATE+0(4) INTO LV_DATE.
    CONCATENATE 'Can’t update exchange rate due to Valid date' LV_DATE 'is not correct'  INTO LV_TEXT SEPARATED BY SPACE.
    MESSAGE LV_TEXT TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO CURRENT TRANSACTION.
  ELSE.

    CONCATENATE P_DATE+6(2) P_DATE+4(2) P_DATE+0(4) INTO LV_VALID.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = LV_VALID
      IMPORTING
        OUTPUT = LV_VALID.

    SELECT KURST TCURR FCURR UKURS FFACT TFACT GDATU FROM TCURR
      INTO TABLE L_TBL_DATA  WHERE GDATU = LV_VALID
                               AND KURST = 'B'
                               AND FCURR = 'JPY'
                               AND TCURR = 'THB'.
    IF L_TBL_DATA IS NOT INITIAL.
      MESSAGE 'Can’t update exchange rate due to duplicate data'(E05) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO CURRENT TRANSACTION.
    ENDIF.
  ENDIF.
ENDFORM. "check_duplicate
" CHECK_DUPLICATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RATE_AUTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_RATE_AUTO .
  DATA :LV_EXCH_RATE     LIKE BAPI1093_0.

** Update Exchange Rate.

  IF GT_XML IS NOT INITIAL.
    SORT GT_XML BY RATE_TYPE.
    IF CB_B EQ ABAP_FALSE.
      DELETE GT_XML WHERE RATE_TYPE EQ 'B'.
    ENDIF.
    IF CB_M EQ ABAP_FALSE.
      DELETE GT_XML WHERE RATE_TYPE EQ 'M'.
    ENDIF.
    IF CB_G EQ ABAP_FALSE.
      DELETE GT_XML WHERE RATE_TYPE EQ 'G'.
    ENDIF.

    LOOP AT GT_XML INTO GW_XML.
      LV_EXCH_RATE-RATE_TYPE = GW_XML-RATE_TYPE.
      LV_EXCH_RATE-FROM_CURR = GW_XML-FROM_CURR.
      LV_EXCH_RATE-TO_CURRNCY = GW_XML-TO_CURR.
      LV_EXCH_RATE-VALID_FROM = GW_XML-VALID_FROM .
      LV_EXCH_RATE-EXCH_RATE = GW_XML-EX_RATE.
      LV_EXCH_RATE-FROM_FACTOR = GW_XML-RATIO_FROM.
      LV_EXCH_RATE-TO_FACTOR = GW_XML-RATIO_TO .

      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
        EXPORTING
          EXCH_RATE = LV_EXCH_RATE
          UPD_ALLOW = ''
          CHG_FIXED = ''
        IMPORTING
          RETURN    = GW_RETURN.
      APPEND GW_RETURN TO GT_RETURN.

      IF GW_RETURN-TYPE NE 'E' AND
         GW_RETURN-TYPE NE 'A'.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT   = 'X'
          IMPORTING
            RETURN = GW_COMMIT_RETURN.
        MESSAGE 'Update Successful'(E07) TYPE 'S' .
      ELSE.
        MESSAGE 'Cannot update exchange reage' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE 'No data for update Exchange Rate'(E05) TYPE 'E'.
  ENDIF.

ENDFORM. " UPDATE_RATE_AUTO
*&---------------------------------------------------------------------*
*& Form F_GET_RATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_RATE .
  LOOP AT S_FCURR.
    GW_RATE = S_FCURR-LOW.
    APPEND GW_RATE TO GT_RATE.
  ENDLOOP.
ENDFORM.
