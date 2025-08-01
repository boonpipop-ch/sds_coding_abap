class ZCL_SDSSD_SO_DEALER_CLAIM_SRV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  types:
    BEGIN OF TS_BILLINGPLAN,
        BILLINGPLANSTARTDATE   TYPE C LENGTH 10,
        BILLINGPLANSTARTDATE1  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT1     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE2  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT2     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE3  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT3     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE4  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT4     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE5  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT5     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE6  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT6     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE7  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT7     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE8  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT8     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE9  TYPE C LENGTH 10,
        BILLINGPLANAMOUNT9     TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE10 TYPE C LENGTH 10,
        BILLINGPLANAMOUNT10    TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE11 TYPE C LENGTH 10,
        BILLINGPLANAMOUNT11    TYPE P LENGTH 6 DECIMALS 2,
        BILLINGPLANSTARTDATE12 TYPE C LENGTH 10,
        BILLINGPLANAMOUNT12    TYPE P LENGTH 6 DECIMALS 2,
      END OF TS_BILLINGPLAN .
  types:
    TT_BILLINGPLAN TYPE STANDARD TABLE OF TS_BILLINGPLAN .

  constants GC_CREATE type CHAR1 value 'C' ##NO_TEXT.
  constants GC_UPDATE type CHAR1 value 'U' ##NO_TEXT.
  constants GC_SUCCESS type CHAR1 value 'S' ##NO_TEXT.
  constants GC_ERROR type CHAR1 value 'E' ##NO_TEXT.
  constants GC_WARNING type CHAR1 value 'W' ##NO_TEXT.

  class-methods SALES_ORDER_PROCESS
    importing
      value(IF_TESTRUN) type FLAG optional
    exporting
      value(EF_SALES_DOC) type VBELN
      value(EF_RESPONSE) type ZSDSCAS006
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_SALESORDERITEM type ZSDSSDS035_TT optional
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_INPUT
    importing
      !IF_RECORD_MODE type CHAR1
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_SALESORDERITEM type ZSDSSDS035_TT
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_HEADER_CREATE
    importing
      !IF_RECORD_MODE type CHAR1
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_HEADER
    importing
      !IF_RECORD_MODE type CHAR1
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_ITEM
    importing
      !IF_RECORD_MODE type CHAR1
      !IS_SALESORDER type ZSDSSDS034
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CT_SALESORDERITEM type ZSDSSDS035_TT
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_HEADER_CHANGE
    importing
      !IF_RECORD_MODE type CHAR1
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_ITEM_CHANGE
    importing
      !IF_RECORD_MODE type CHAR1
    exporting
      value(EF_INVALID) type FLAG
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_SALESORDERITEM type ZSDSSDS035_TT
      !CT_RESPONSE type ZSDSCAS006_TT .

  methods PROCESS_DATA
    redefinition .
  PROTECTED SECTION.
private section.

  class-data GRT_COMPANYCODE type BUKRS_RAN_ITAB .

  class-methods VALIDATE_DATE
    importing
      !DATE_IN type CHAR10
    exporting
      value(DATE_OUT) type CHAR10
      !CS_RESPONSE type ZSDSCAS006 .
  class-methods GET_CONSTANTS
    changing
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods APPEND_RESPONSE
    importing
      !IF_STATUS type CHAR1
      !IF_MESSAGE type ZSDSDE_REST_MESSAGE
    changing
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods SALES_ORDER_CREATE
    importing
      !IF_TESTRUN type FLAG optional
    exporting
      !EF_SALES_DOC type VBELN
      value(EF_RESPONSE) type ZSDSCAS006
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_SALESORDERITEM type ZSDSSDS035_TT optional
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods SALES_ORDER_CHANGE
    importing
      !IF_TESTRUN type FLAG optional
    exporting
      value(EF_SALES_DOC) type VBELN
      value(EF_RESPONSE) type ZSDSCAS006
    changing
      !CS_SALESORDER type ZSDSSDS034
      !CT_SALESORDERITEM type ZSDSSDS035_TT optional
      !CT_RESPONSE type ZSDSCAS006_TT .
  class-methods VALIDATE_AMOUNT
    importing
      value(AMOUNT_IN) type CHAR20
    exporting
      value(AMOUNT_OUT) type CHAR20
      !CS_RESPONSE type ZSDSCAS006 .
ENDCLASS.



CLASS ZCL_SDSSD_SO_DEALER_CLAIM_SRV IMPLEMENTATION.


  METHOD APPEND_RESPONSE.

    DATA: LS_RESPONSE TYPE ZSDSCAS006.

    LS_RESPONSE-RESP_STATUS = IF_STATUS.
    LS_RESPONSE-RESP_MESSAGE = IF_MESSAGE.

    APPEND LS_RESPONSE TO CT_RESPONSE.

*    IF CS_SALESORDER-RESPONSESTATUS IS INITIAL AND
*       CS_SALESORDER-RESPONSEMESSAGE IS INITIAL.
*      CS_SALESORDER-RESPONSESTATUS = IF_STATUS.
*      CS_SALESORDER-RESPONSEMESSAGE = IF_MESSAGE.
*    ENDIF.

  ENDMETHOD.


  METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / GET_CONSTANTS
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI026
*  Description        : Get general constants from table ZSDSCAC001
*  Purpose            : Get general constants from table ZSDSCAC001
*                       Maintenance view: ZSDSV_GEN_C
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA: LV_MSG TYPE ZSDSDE_REST_MESSAGE.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_SO_DEALER_CLAIM_SRV'
                                                    IF_PARAM = 'COMPANYCODE'
                                          IMPORTING ET_RANGE = GRT_COMPANYCODE ).
    IF GRT_COMPANYCODE IS INITIAL.
      LV_MSG = TEXT-E01.
      REPLACE '&' IN LV_MSG WITH TEXT-T03.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

  ENDMETHOD.


  METHOD PROCESS_DATA.
*CALL METHOD SUPER->PROCESS_DATA
*  EXPORTING
*    IREF_REQUEST_DATA  =
**  IMPORTING
**    EREF_RESPONSE_DATA =
**    EF_STATUS          =
**    EF_MESSAGE         =
**    EF_HTTP_ERROR      =
*    .


    DATA: LS_REQUEST        TYPE ZSDSSDS038,
          LS_SALESORDER     TYPE ZSDSSDS034,
          LT_SALESORDERITEM TYPE ZSDSSDS035_TT,
          LS_RESPONSE	      TYPE ZSDSCAS006.

    FIELD-SYMBOLS: <L_RESPONSE> TYPE ZSDSSDS038.

* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
    IF SY-SUBRC NE 0.
*   Critical error
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING LS_REQUEST TO LS_SALESORDER.
    MOVE-CORRESPONDING LS_REQUEST-SALESORDERITEM[] TO LT_SALESORDERITEM.

    SALES_ORDER_PROCESS(
      EXPORTING
        IF_TESTRUN        = ''
      IMPORTING
        EF_SALES_DOC      = LS_SALESORDER-SAPSALESORDERNO
        EF_RESPONSE       = LS_RESPONSE
      CHANGING
        CS_SALESORDER     = LS_SALESORDER
        CT_SALESORDERITEM = LT_SALESORDERITEM
        CT_RESPONSE       = <L_RESPONSE>-RESPONSE ).


    MOVE-CORRESPONDING LS_SALESORDER TO <L_RESPONSE>.
    MOVE-CORRESPONDING LT_SALESORDERITEM TO <L_RESPONSE>-SALESORDERITEM.

* Assign Log status from Response structure
    EF_STATUS  = LS_RESPONSE-RESP_STATUS .
    EF_MESSAGE = LS_RESPONSE-RESP_MESSAGE.

  ENDMETHOD.


  METHOD SALES_ORDER_CHANGE ##NEEDED.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / SALES_ORDER_CHANGE
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : CHANGE SALES ORDER
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA:
      LT_ITEM                 TYPE STANDARD TABLE OF BAPISDITM,
      LT_ITEMX                TYPE STANDARD TABLE OF BAPISDITMX,
      LT_SCHEDULES_IN         TYPE TABLE OF  BAPISCHDL,
      LT_SCHEDULES_INX        TYPE TABLE OF  BAPISCHDLX,
      LT_RETURN               TYPE STANDARD TABLE OF BAPIRET2,
      LT_PARTNERS             TYPE STANDARD TABLE OF BAPIPARNR,
      LT_PARTNERCHANGES       TYPE STANDARD TABLE OF BAPIPARNRC,
      LS_PARTNERCHANGES       TYPE BAPIPARNRC,
      LS_BAPE_VBAP            TYPE BAPE_VBAP,
      LS_BAPE_VBAPX           TYPE BAPE_VBAPX,
      LS_EXTENSIONIN          TYPE BAPIPAREX,
      LT_EXTENSIONIN          TYPE BAPIPAREX_T,
      LS_HEADER               TYPE BAPISDH1,
      LS_HEADERX              TYPE BAPISDH1X,
      LV_SALES_UNIT_CONV      TYPE VRKME,
      LS_RESPONSE             TYPE ZSDSCAS006,
      LT_ORDER_CONDITIONS_IN  TYPE STANDARD TABLE OF BAPICOND,
      LT_ORDER_CONDITIONS_INX TYPE STANDARD TABLE OF BAPICONDX,
      LT_CONDITION_ITEM       TYPE ZSDSSDS006_TT.

*for extension
    CONSTANTS:
      LC_PART1 TYPE I                    VALUE 240,
      LC_PART2 TYPE I                    VALUE 480,
      LC_PART3 TYPE I                    VALUE 720,
      LC_PART4 TYPE I                    VALUE 960.

    FIELD-SYMBOLS:
                   <LF_STRUCTURE> TYPE ANY.
    DATA:
      LF_OFF1        TYPE I,
      LF_OFF2        TYPE I,
      LF_OFF3        TYPE I,
      LF_OFF4        TYPE I,
      LF_LEN1        TYPE I,
      LF_LEN2        TYPE I,
      LF_LEN3        TYPE I,
      LF_LEN4        TYPE I,
      LF_LENGTH      TYPE I,
      LF_LENGTH1     TYPE I,
      LF_LENGTH2     TYPE I,
      LF_LENGTH3     TYPE I,
      LF_LENGTH4     TYPE I,
      LF_LENGTH_COMP TYPE I,
      LF_FLG_PART2   TYPE FLAG,
      LF_FLG_PART3   TYPE FLAG,
      LF_FLG_PART4   TYPE FLAG.

    "Header
    LS_HEADER-DOC_DATE  = COND #( WHEN CS_SALESORDER-CREATEDATE IS NOT INITIAL THEN CS_SALESORDER-CREATEDATE ).
    LS_HEADER-PURCH_NO_C  = COND #( WHEN CS_SALESORDER-PONUMBER IS NOT INITIAL THEN CS_SALESORDER-PONUMBER ).
    LS_HEADER-PMNTTRMS  = COND #( WHEN CS_SALESORDER-PAYMENTTERMS IS NOT INITIAL THEN CS_SALESORDER-PAYMENTTERMS ).

    LS_HEADERX-UPDATEFLAG  = 'U'.
    LS_HEADERX-DOC_DATE    = COND #( WHEN CS_SALESORDER-CREATEDATE IS NOT INITIAL THEN ABAP_TRUE ).
    LS_HEADERX-PURCH_NO_C   = COND #( WHEN CS_SALESORDER-PONUMBER IS NOT INITIAL THEN ABAP_TRUE ).
    LS_HEADERX-PMNTTRMS   = COND #( WHEN CS_SALESORDER-PAYMENTTERMS IS NOT INITIAL THEN ABAP_TRUE ).

    "Partner
    SELECT SINGLE KUNNR,VKORG,VTWEG,SPART,WAERS
      FROM KNVV
     WHERE KUNNR = @CS_SALESORDER-BPSOLDTONUMBER
       AND VKORG = @CS_SALESORDER-SALESORG
       AND VTWEG = '00'  "Common
       AND SPART = '00'  "Common
      INTO @DATA(LS_KNVV).

    SELECT VBELN,POSNR,PARVW,KUNNR,ADRNR
      FROM VBPA
     WHERE VBELN = @CS_SALESORDER-SAPSALESORDERNO
      AND  POSNR = '000000'
      INTO TABLE @DATA(LT_VBPA).
    IF SY-SUBRC = 0.
      SELECT SINGLE KUNNR,ADRNR,KTOKD INTO @DATA(LS_KNA1) ##NEEDED
        FROM KNA1
       WHERE KUNNR = @CS_SALESORDER-BPSOLDTONUMBER.
      IF SY-SUBRC = 0.
        LOOP AT LT_VBPA INTO DATA(LS_VBPA).
          CLEAR: LS_PARTNERCHANGES.

          CASE LS_VBPA-PARVW.
            WHEN 'AG'.
              IF LS_VBPA-KUNNR <> CS_SALESORDER-BPSOLDTONUMBER.
                LS_PARTNERCHANGES-DOCUMENT   = CS_SALESORDER-SAPSALESORDERNO.
                LS_PARTNERCHANGES-ITM_NUMBER = LS_VBPA-POSNR.
                LS_PARTNERCHANGES-UPDATEFLAG = 'U'.
                LS_PARTNERCHANGES-PARTN_ROLE = LS_VBPA-PARVW.
                LS_PARTNERCHANGES-P_NUMB_OLD = LS_VBPA-KUNNR.
                LS_PARTNERCHANGES-P_NUMB_NEW = |{ CS_SALESORDER-BPSOLDTONUMBER ALPHA = IN }|.
                APPEND LS_PARTNERCHANGES TO LT_PARTNERCHANGES.
              ENDIF.
            WHEN 'RE'.
              IF LS_VBPA-KUNNR <> CS_SALESORDER-BPBILLTONUMBER.
                LS_PARTNERCHANGES-DOCUMENT   = CS_SALESORDER-SAPSALESORDERNO.
                LS_PARTNERCHANGES-ITM_NUMBER = LS_VBPA-POSNR.
                LS_PARTNERCHANGES-UPDATEFLAG = 'U'.
                LS_PARTNERCHANGES-PARTN_ROLE = LS_VBPA-PARVW.
                LS_PARTNERCHANGES-P_NUMB_OLD = LS_VBPA-KUNNR.
                LS_PARTNERCHANGES-P_NUMB_NEW = |{ CS_SALESORDER-BPBILLTONUMBER ALPHA = IN }|.
                APPEND LS_PARTNERCHANGES TO LT_PARTNERCHANGES.
              ENDIF.
            WHEN 'RG'.
              IF LS_VBPA-KUNNR <> CS_SALESORDER-BPPAYERNUMBER.
                LS_PARTNERCHANGES-DOCUMENT   = CS_SALESORDER-SAPSALESORDERNO.
                LS_PARTNERCHANGES-ITM_NUMBER = LS_VBPA-POSNR.
                LS_PARTNERCHANGES-UPDATEFLAG = 'U'.
                LS_PARTNERCHANGES-PARTN_ROLE = LS_VBPA-PARVW.
                LS_PARTNERCHANGES-P_NUMB_OLD = LS_VBPA-KUNNR.
                LS_PARTNERCHANGES-P_NUMB_NEW = |{ CS_SALESORDER-BPPAYERNUMBER ALPHA = IN }|.
                APPEND LS_PARTNERCHANGES TO LT_PARTNERCHANGES.
              ENDIF.
            WHEN 'WE'.
              IF LS_VBPA-KUNNR <> CS_SALESORDER-BPSHIPTONUMBER.
                LS_PARTNERCHANGES-DOCUMENT   = CS_SALESORDER-SAPSALESORDERNO.
                LS_PARTNERCHANGES-ITM_NUMBER = LS_VBPA-POSNR.
                LS_PARTNERCHANGES-UPDATEFLAG = 'U'.
                LS_PARTNERCHANGES-PARTN_ROLE = LS_VBPA-PARVW.
                LS_PARTNERCHANGES-P_NUMB_OLD = LS_VBPA-KUNNR.
                LS_PARTNERCHANGES-P_NUMB_NEW = |{ CS_SALESORDER-BPSHIPTONUMBER ALPHA = IN }|.
                APPEND LS_PARTNERCHANGES TO LT_PARTNERCHANGES.
              ENDIF.
            WHEN 'VE'.
              IF LS_VBPA-KUNNR <> CS_SALESORDER-BPSALESEMPLOYEE.
                LS_PARTNERCHANGES-DOCUMENT   = CS_SALESORDER-SAPSALESORDERNO.
                LS_PARTNERCHANGES-ITM_NUMBER = LS_VBPA-POSNR.
                LS_PARTNERCHANGES-UPDATEFLAG = 'U'.
                LS_PARTNERCHANGES-PARTN_ROLE = LS_VBPA-PARVW.
                LS_PARTNERCHANGES-P_NUMB_OLD = LS_VBPA-KUNNR.
                LS_PARTNERCHANGES-P_NUMB_NEW = |{ CS_SALESORDER-BPSALESEMPLOYEE ALPHA = IN }|.
                APPEND LS_PARTNERCHANGES TO LT_PARTNERCHANGES.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDIF.

*Condition Header
    CLEAR: LT_CONDITION_ITEM.
*'ZDH5' Header Special Discount (%)
    APPEND VALUE #( ITM_NUMBER = '000000'
                    COND_TYPE = 'ZDH5'
                    COND_VALUE = CS_SALESORDER-HEADERSPECIALDISCOUNTPER * 10
                    CURRENCY = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS_IN.
    APPEND VALUE #( ITM_NUMBER = '000000'
                    COND_TYPE  = 'ZDH5'
                    COND_VALUE = ABAP_TRUE
                    CURRENCY   = ABAP_TRUE
                    UPDATEFLAG = 'U' ) TO LT_ORDER_CONDITIONS_INX.

*ZDH6 'ZDH6' Header Special Discount (Val.)
    APPEND VALUE #( ITM_NUMBER = '000000'
                    COND_TYPE = 'ZDH6'
                    COND_VALUE = CS_SALESORDER-HEADERSPECIALDISCOUNTVAL
                    CURRENCY = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS_IN.
    APPEND VALUE #( ITM_NUMBER = '000000'
                    COND_TYPE  = 'ZDH6'
                    COND_VALUE = ABAP_TRUE
                    CURRENCY   = ABAP_TRUE
                    UPDATEFLAG = 'U' ) TO LT_ORDER_CONDITIONS_INX.

    CLEAR: LT_ITEM.

    SELECT VBELN,POSNR FROM VBAP
      WHERE VBELN = @CS_SALESORDER-SAPSALESORDERNO
      INTO TABLE @DATA(LT_VBAP).
    IF SY-SUBRC = 0.
      SORT LT_VBAP BY VBELN POSNR.
    ENDIF.

    LOOP AT CT_SALESORDERITEM ASSIGNING FIELD-SYMBOL(<LFS_ITEM>).
*Items
      "Convert Sales Unit to Internal Value
      IF <LFS_ITEM>-SALESUNIT IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT          = <LFS_ITEM>-SALESUNIT
          IMPORTING
            OUTPUT         = LV_SALES_UNIT_CONV
          EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC <> 0.
          CLEAR: LV_SALES_UNIT_CONV.
        ENDIF.
      ENDIF.

      READ TABLE LT_VBAP WITH KEY VBELN = CS_SALESORDER-SAPSALESORDERNO
                                  POSNR = <LFS_ITEM>-ITEMNUMBER           ##WARN_OK
                                  BINARY SEARCH
                                  TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0.

        APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                        MATERIAL    = <LFS_ITEM>-MATERIALNUMBER
                        TARGET_QTY  = <LFS_ITEM>-QUANTITY
                        SALES_UNIT  = LV_SALES_UNIT_CONV
                        PLANT       = <LFS_ITEM>-PLANT
                        STORE_LOC   = <LFS_ITEM>-STORAGELOCATION
                        REASON_REJ  = <LFS_ITEM>-REASONFORREJECT
                        )
                        TO LT_ITEM.
        APPEND VALUE #( UPDATEFLAG  = <LFS_ITEM>-FLAG
                        ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                        MATERIAL    =  ABAP_TRUE
                        TARGET_QTY  =  ABAP_TRUE
                        SALES_UNIT  =  ABAP_TRUE
                        PLANT       =  ABAP_TRUE
                        STORE_LOC   =  ABAP_TRUE
                        REASON_REJ  =  ABAP_TRUE
                        )
                        TO LT_ITEMX.


        APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                        SCHED_LINE  = '0001'
                        REQ_QTY     = <LFS_ITEM>-QUANTITY
                        REQ_DATE    = SY-DATUM
                        )
                        TO LT_SCHEDULES_IN.

        APPEND VALUE #( UPDATEFLAG  = <LFS_ITEM>-FLAG
                        ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                        SCHED_LINE  = '0001'
                        REQ_QTY     = ABAP_TRUE
                        REQ_DATE    = ABAP_TRUE
                        )
                        TO LT_SCHEDULES_INX.
      ELSE.
        IF <LFS_ITEM>-FLAG = 'I'.
          APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                          MATERIAL    = <LFS_ITEM>-MATERIALNUMBER
                          TARGET_QTY  = <LFS_ITEM>-QUANTITY
                          SALES_UNIT  = LV_SALES_UNIT_CONV
                          PLANT       = <LFS_ITEM>-PLANT
                          STORE_LOC   = <LFS_ITEM>-STORAGELOCATION
                          ITEM_CATEG  = <LFS_ITEM>-ITEMCATEGORY
                          REASON_REJ  = <LFS_ITEM>-REASONFORREJECT
                          )
                          TO LT_ITEM.
          APPEND VALUE #( UPDATEFLAG  = <LFS_ITEM>-FLAG
                          ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                          MATERIAL    =  ABAP_TRUE
                          TARGET_QTY  =  ABAP_TRUE
                          SALES_UNIT  =  ABAP_TRUE
                          PLANT       =  ABAP_TRUE
                          STORE_LOC   =  ABAP_TRUE
                          ITEM_CATEG  =  ABAP_TRUE      "Add item cat
                          REASON_REJ  =  ABAP_TRUE
                          )
                          TO LT_ITEMX.

          APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                          SCHED_LINE  = '0001'
                          REQ_QTY     = <LFS_ITEM>-QUANTITY
                          REQ_DATE    = SY-DATUM
                          )
                          TO LT_SCHEDULES_IN.

          APPEND VALUE #( UPDATEFLAG  = <LFS_ITEM>-FLAG
                          ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                          SCHED_LINE  = '0001'
                          REQ_QTY     = ABAP_TRUE
                          REQ_DATE    = ABAP_TRUE
                          )
                          TO LT_SCHEDULES_INX.

        ENDIF.
      ENDIF.
      IF <LFS_ITEM>-FLAG = 'I' OR <LFS_ITEM>-FLAG = 'U'.
*  Conditions
*ZPR1
        APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                        KSCHL = 'ZPR1'
                        KBETR = <LFS_ITEM>-LISTPRICE
                        WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI1' ITEM DISCOUNT (%)
        APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                        KSCHL = 'ZDI1'
                        KBETR = <LFS_ITEM>-ITEMDISCOUNTPER * 10
                        WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI2' ITEM DISCOUNT (VAL.)
        APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                        KSCHL = 'ZDI2'
                        KBETR = <LFS_ITEM>-ITEMDISCOUNTVAL
                        WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

      ELSEIF <LFS_ITEM>-FLAG = 'D'.
        APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                        COND_TYPE  = 'ZPR1'
                        COND_VALUE = <LFS_ITEM>-LISTPRICE
                        CURRENCY   = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS_IN.
        APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                        COND_TYPE  = 'ZPR1'
                        COND_VALUE = ABAP_TRUE
                        CURRENCY   = ABAP_TRUE
                        UPDATEFLAG = 'D' ) TO LT_ORDER_CONDITIONS_INX.

        APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                        COND_TYPE  = 'ZDI1'
                        COND_VALUE    = <LFS_ITEM>-ITEMDISCOUNTPER * 10
                        CURRENCY = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS_IN.
        APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                        COND_TYPE  = 'ZDI1'
                        COND_VALUE = ABAP_TRUE
                        UPDATEFLAG = 'D' ) TO LT_ORDER_CONDITIONS_INX.

        APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                        COND_TYPE  = 'ZDI2'
                        COND_VALUE    = <LFS_ITEM>-ITEMDISCOUNTVAL
                        CURRENCY = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS_IN.
        APPEND VALUE #( ITM_NUMBER = <LFS_ITEM>-ITEMNUMBER
                        COND_TYPE  = 'ZDI2'
                        COND_VALUE = ABAP_TRUE
                        CURRENCY   = ABAP_TRUE
                        UPDATEFLAG = 'D' ) TO LT_ORDER_CONDITIONS_INX.

      ENDIF.


*  Extensions
      CLEAR LS_EXTENSIONIN.
      LS_BAPE_VBAP-POSNR          = <LFS_ITEM>-ITEMNUMBER.
      LS_BAPE_VBAP-ZZ1_LOB_SO_SDI = <LFS_ITEM>-LOB.

      LS_EXTENSIONIN-STRUCTURE = 'BAPE_VBAP'.

      CLEAR: LF_LENGTH,LF_LENGTH1,LF_LENGTH2,LF_LENGTH3,LF_LENGTH4,
             LF_FLG_PART2,LF_FLG_PART3,LF_FLG_PART4.
      ASSIGN LS_BAPE_VBAP TO <LF_STRUCTURE>.
      IF <LF_STRUCTURE> IS ASSIGNED.
        DO.
          ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LF_STRUCTURE> TO FIELD-SYMBOL(<LF_FIELD>).
          DESCRIBE FIELD <LF_FIELD> LENGTH LF_LENGTH_COMP IN CHARACTER MODE.

          IF SY-SUBRC <> 0.
            EXIT.
          ENDIF.
          LF_LENGTH = LF_LENGTH + LF_LENGTH_COMP.
          IF LF_LENGTH <= LC_PART1.
            LF_LENGTH1  = LF_LENGTH.
          ENDIF.
          IF LF_LENGTH >= LC_PART1 AND
             LF_LENGTH <= LC_PART2.
            LF_OFF2 = LF_LENGTH1.
            LF_LENGTH2  = LF_LENGTH.
            LF_FLG_PART2 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART2 AND
             LF_LENGTH <= LC_PART3.
            LF_OFF3 = LF_LENGTH2.
            LF_LENGTH3  = LF_LENGTH.
            LF_FLG_PART3 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART3 AND
             LF_LENGTH <= LC_PART4.
            LF_OFF4 = LF_LENGTH3.
            LF_LENGTH4  = LF_LENGTH.
            LF_FLG_PART4 = ABAP_ON.
          ENDIF.
        ENDDO.
        CLEAR: LF_LEN1,LF_LEN2,LF_LEN3,LF_LEN4.
        LF_LEN1 = LF_LENGTH1.
        LF_LEN2 = LF_LENGTH2.
        LF_LEN3 = LF_LENGTH3.
        LF_LEN4 = LF_LENGTH4.

        LS_EXTENSIONIN-VALUEPART1 = <LF_STRUCTURE>+LF_OFF1(LF_LEN1).
        IF LF_FLG_PART2 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART2 = <LF_STRUCTURE>+LF_OFF2(LF_LEN2).
        ENDIF.
        IF LF_FLG_PART3 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART3 = <LF_STRUCTURE>+LF_OFF3(LF_LEN3).
        ENDIF.
        IF LF_FLG_PART4 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART4 = <LF_STRUCTURE>+LF_OFF4(LF_LEN4).
        ENDIF.
      ENDIF.

      APPEND LS_EXTENSIONIN TO LT_EXTENSIONIN.

*  ExtensionsX
      CLEAR LS_EXTENSIONIN.
      LS_BAPE_VBAPX-POSNR          = <LFS_ITEM>-ITEMNUMBER.
      LS_BAPE_VBAPX-ZZ1_LOB_SO_SDI = ABAP_TRUE.

      LS_EXTENSIONIN-STRUCTURE = 'BAPE_VBAPX'.

      CLEAR: LF_LENGTH,LF_LENGTH1,LF_LENGTH2,LF_LENGTH3,LF_LENGTH4,
             LF_FLG_PART2,LF_FLG_PART3,LF_FLG_PART4.
      ASSIGN LS_BAPE_VBAPX TO <LF_STRUCTURE>.
      IF <LF_STRUCTURE> IS ASSIGNED.
        DO.
          ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LF_STRUCTURE> TO <LF_FIELD>.
          DESCRIBE FIELD <LF_FIELD> LENGTH LF_LENGTH_COMP IN CHARACTER MODE.

          IF SY-SUBRC <> 0.
            EXIT.
          ENDIF.
          LF_LENGTH = LF_LENGTH + LF_LENGTH_COMP.
          IF LF_LENGTH <= LC_PART1.
            LF_LENGTH1  = LF_LENGTH.
          ENDIF.
          IF LF_LENGTH >= LC_PART1 AND
             LF_LENGTH <= LC_PART2.
            LF_OFF2 = LF_LENGTH1.
            LF_LENGTH2  = LF_LENGTH.
            LF_FLG_PART2 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART2 AND
             LF_LENGTH <= LC_PART3.
            LF_OFF3 = LF_LENGTH2.
            LF_LENGTH3  = LF_LENGTH.
            LF_FLG_PART3 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART3 AND
             LF_LENGTH <= LC_PART4.
            LF_OFF4 = LF_LENGTH3.
            LF_LENGTH4  = LF_LENGTH.
            LF_FLG_PART4 = ABAP_ON.
          ENDIF.
        ENDDO.
        CLEAR: LF_LEN1,LF_LEN2,LF_LEN3,LF_LEN4.
        LF_LEN1 = LF_LENGTH1.
        LF_LEN2 = LF_LENGTH2.
        LF_LEN3 = LF_LENGTH3.
        LF_LEN4 = LF_LENGTH4.

        LS_EXTENSIONIN-VALUEPART1 = <LF_STRUCTURE>+LF_OFF1(LF_LEN1).
        IF LF_FLG_PART2 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART2 = <LF_STRUCTURE>+LF_OFF2(LF_LEN2).
        ENDIF.
        IF LF_FLG_PART3 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART3 = <LF_STRUCTURE>+LF_OFF3(LF_LEN3).
        ENDIF.
        IF LF_FLG_PART4 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART4 = <LF_STRUCTURE>+LF_OFF4(LF_LEN4).
        ENDIF.
      ENDIF.

      APPEND LS_EXTENSIONIN TO LT_EXTENSIONIN.

      CLEAR:
        LV_SALES_UNIT_CONV.

    ENDLOOP.

    "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
    EXPORT LT_CONDITION_ITEM FROM LT_CONDITION_ITEM TO MEMORY ID 'CONDTYP_SO_FROM_SF'.
*-------------------------------------
* Sales Order Change
*-------------------------------------

    DATA: LOGIC_SWITCH  TYPE BAPISDLS.
    LOGIC_SWITCH-NOSTRUCTURE = 'X'.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        SALESDOCUMENT    = CS_SALESORDER-SAPSALESORDERNO
        ORDER_HEADER_IN  = LS_HEADER
        ORDER_HEADER_INX = LS_HEADERX
        SIMULATION       = IF_TESTRUN
        LOGIC_SWITCH     = LOGIC_SWITCH
      TABLES
        RETURN           = LT_RETURN
        ORDER_ITEM_IN    = LT_ITEM
        ORDER_ITEM_INX   = LT_ITEMX
        PARTNERS         = LT_PARTNERS
        PARTNERCHANGES   = LT_PARTNERCHANGES
        SCHEDULE_LINES   = LT_SCHEDULES_IN
        SCHEDULE_LINESX  = LT_SCHEDULES_INX
        CONDITIONS_IN    = LT_ORDER_CONDITIONS_IN
        CONDITIONS_INX   = LT_ORDER_CONDITIONS_INX
        EXTENSIONIN      = LT_EXTENSIONIN.

    IF LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ) OR
       LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.

      READ TABLE LT_RETURN  WITH KEY TYPE   = 'S'
                                     ID     = 'V1'
                                     NUMBER = '311' TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        EF_SALES_DOC = CS_SALESORDER-SAPSALESORDERNO.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'
*         IMPORTING
*           RETURN        =
          .
      ENDIF.
    ENDIF.

    READ TABLE LT_RETURN INTO data(LS_RETURN) WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
      EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
    ELSE.
      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'S'
                                                   ID = 'V1'
                                                   NUMBER = '311'.
      IF SY-SUBRC = 0.
        EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
        EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
      ELSE.
        READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
        IF SY-SUBRC = 0.
          EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
          EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<LFS_RETURN>).
      LS_RESPONSE-RESP_STATUS  = <LFS_RETURN>-TYPE.
      LS_RESPONSE-RESP_MESSAGE = <LFS_RETURN>-MESSAGE.
      APPEND LS_RESPONSE TO CT_RESPONSE.
    ENDLOOP.

  ENDMETHOD.


  METHOD SALES_ORDER_CREATE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / SALES_ORDER_CREATE
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : CREATE SALES ORDER
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA:
      LT_ITEM            TYPE STANDARD TABLE OF BAPISDITM,
      LT_SCHEDULES_IN    TYPE TABLE OF  BAPISCHDL,
      LT_RETURN          TYPE STANDARD TABLE OF BAPIRET2,
      LT_PARTNERS        TYPE STANDARD TABLE OF BAPIPARNR,
      LS_BAPE_VBAP       TYPE BAPE_VBAP,
      LS_EXTENSIONIN     TYPE BAPIPAREX,
      LT_EXTENSIONIN     TYPE BAPIPAREX_T,
      LS_HEADER          TYPE BAPISDHD1,
      LV_SALES_UNIT_CONV TYPE VRKME,
      LS_RESPONSE        TYPE ZSDSCAS006,
      LT_ORDER_CONDITIONS TYPE table of BAPICOND,
      LT_CONDITION_ITEM  TYPE ZSDSSDS006_TT.

*for extension
    CONSTANTS:
      LC_PART1 TYPE I                    VALUE 240,
      LC_PART2 TYPE I                    VALUE 480,
      LC_PART3 TYPE I                    VALUE 720,
      LC_PART4 TYPE I                    VALUE 960.

    FIELD-SYMBOLS:
                   <LF_STRUCTURE> TYPE ANY.
    DATA:
      LF_OFF1        TYPE I,
      LF_OFF2        TYPE I,
      LF_OFF3        TYPE I,
      LF_OFF4        TYPE I,
      LF_LEN1        TYPE I,
      LF_LEN2        TYPE I,
      LF_LEN3        TYPE I,
      LF_LEN4        TYPE I,
      LF_LENGTH      TYPE I,
      LF_LENGTH1     TYPE I,
      LF_LENGTH2     TYPE I,
      LF_LENGTH3     TYPE I,
      LF_LENGTH4     TYPE I,
      LF_LENGTH_COMP TYPE I,
      LF_FLG_PART2   TYPE FLAG,
      LF_FLG_PART3   TYPE FLAG,
      LF_FLG_PART4   TYPE FLAG.

* Header
    LS_HEADER = VALUE #( NAME       = CS_SALESORDER-WEBORDERNO
                         DOC_DATE   = CS_SALESORDER-CREATEDATE
                         DOC_TYPE   = CS_SALESORDER-DOCUMENTTYPE
                         SALES_ORG  = CS_SALESORDER-SALESORG
                         DISTR_CHAN = CS_SALESORDER-DISTRIBUTIONCHANNEL
                         DIVISION   = CS_SALESORDER-DIVISION
                         SALES_OFF  = CS_SALESORDER-SALESOFFICE
                         SALES_GRP  = CS_SALESORDER-SALESGROUP
                         PURCH_NO_C = CS_SALESORDER-PONUMBER
                         PMNTTRMS   = CS_SALESORDER-PAYMENTTERMS
                       ).

* Partner
    "Currency
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = CS_SALESORDER-BPSOLDTONUMBER
      IMPORTING
        OUTPUT = CS_SALESORDER-BPSOLDTONUMBER.

    SELECT SINGLE KUNNR,VKORG,VTWEG,SPART,WAERS
      FROM KNVV
     WHERE KUNNR = @CS_SALESORDER-BPSOLDTONUMBER
       AND VKORG = @CS_SALESORDER-SALESORG
       AND VTWEG = '00'  "Common
       AND SPART = '00'  "Common
      INTO @DATA(LS_KNVV).

    IF CS_SALESORDER-BPSOLDTONUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'AG'
                      PARTN_NUMB = |{ CS_SALESORDER-BPSOLDTONUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF CS_SALESORDER-BPBILLTONUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RE'
                      PARTN_NUMB = |{ CS_SALESORDER-BPBILLTONUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF CS_SALESORDER-BPPAYERNUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'RG'
                      PARTN_NUMB = |{ CS_SALESORDER-BPPAYERNUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

    IF CS_SALESORDER-BPSHIPTONUMBER IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'WE'
                      PARTN_NUMB = |{ CS_SALESORDER-BPSHIPTONUMBER ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.


    IF CS_SALESORDER-BPSALESEMPLOYEE IS NOT INITIAL.
      APPEND VALUE #( PARTN_ROLE = 'VE'
                      PARTN_NUMB = |{ CS_SALESORDER-BPSALESEMPLOYEE ALPHA = IN }| ) TO LT_PARTNERS.
    ENDIF.

*  Conditions
    CLEAR: LT_CONDITION_ITEM.
*Condition Header
*'ZDH5' Header Special Discount (%)
    APPEND VALUE #( ITM_NUMBER = '000000'
                    COND_TYPE = 'ZDH5'
                    COND_VALUE = CS_SALESORDER-HEADERSPECIALDISCOUNTPER * 10
                    CURRENCY = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS.

*ZDH6 'ZDH6' Header Special Discount (Val.)
    APPEND VALUE #( ITM_NUMBER = '000000'
                    COND_TYPE = 'ZDH6'
                    COND_VALUE = CS_SALESORDER-HEADERSPECIALDISCOUNTVAL
                    CURRENCY = LS_KNVV-WAERS ) TO LT_ORDER_CONDITIONS.


    CLEAR: LT_ITEM.
    LOOP AT CT_SALESORDERITEM ASSIGNING FIELD-SYMBOL(<LFS_ITEM>).

*Items
      "Convert Sales Unit to Internal Value
      IF <LFS_ITEM>-SALESUNIT IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT          = <LFS_ITEM>-SALESUNIT
          IMPORTING
            OUTPUT         = LV_SALES_UNIT_CONV
          EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC <> 0.
          CLEAR: LV_SALES_UNIT_CONV.
        ENDIF.
      ENDIF.

      APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                      MATERIAL    = <LFS_ITEM>-MATERIALNUMBER
                      TARGET_QTY  = <LFS_ITEM>-QUANTITY
                      SALES_UNIT  = LV_SALES_UNIT_CONV
                      PLANT       = <LFS_ITEM>-PLANT
                      STORE_LOC   = <LFS_ITEM>-STORAGELOCATION
                      ITEM_CATEG  = <LFS_ITEM>-ITEMCATEGORY
                      REASON_REJ  = <LFS_ITEM>-REASONFORREJECT
                      )
                      TO LT_ITEM.

      APPEND VALUE #( ITM_NUMBER  = <LFS_ITEM>-ITEMNUMBER
                      SCHED_LINE  = '0001'
                      REQ_QTY     = <LFS_ITEM>-QUANTITY
                      REQ_DATE    = SY-DATUM
                      )
                      TO LT_SCHEDULES_IN.
*  Conditions
*Condition Item
*ZPR1
      APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                      KSCHL = 'ZPR1'
                      KBETR = <LFS_ITEM>-LISTPRICE
                      WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI1' ITEM DISCOUNT (%)
      APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                      KSCHL = 'ZDI1'
                      KBETR = <LFS_ITEM>-ITEMDISCOUNTPER * 10
                      WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.

*'ZDI2' ITEM DISCOUNT (VAL.)
      APPEND VALUE #( KPOSN = <LFS_ITEM>-ITEMNUMBER
                      KSCHL = 'ZDI2'
                      KBETR = <LFS_ITEM>-ITEMDISCOUNTVAL
                      WAERK = LS_KNVV-WAERS ) TO LT_CONDITION_ITEM.


*  Extensions
      CLEAR LS_EXTENSIONIN.
      LS_BAPE_VBAP-POSNR          = <LFS_ITEM>-ITEMNUMBER.
      LS_BAPE_VBAP-ZZ1_LOB_SO_SDI = <LFS_ITEM>-LOB.

      LS_EXTENSIONIN-STRUCTURE = 'BAPE_VBAP'.

      CLEAR: LF_LENGTH,LF_LENGTH1,LF_LENGTH2,LF_LENGTH3,LF_LENGTH4,
             LF_FLG_PART2,LF_FLG_PART3,LF_FLG_PART4.
      ASSIGN LS_BAPE_VBAP TO <LF_STRUCTURE>.
      IF <LF_STRUCTURE> IS ASSIGNED.
        DO.
          ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LF_STRUCTURE> TO FIELD-SYMBOL(<LF_FIELD>).
          DESCRIBE FIELD <LF_FIELD> LENGTH LF_LENGTH_COMP IN CHARACTER MODE.

          IF SY-SUBRC <> 0.
            EXIT.
          ENDIF.
          LF_LENGTH = LF_LENGTH + LF_LENGTH_COMP.
          IF LF_LENGTH <= LC_PART1.
            LF_LENGTH1  = LF_LENGTH.
          ENDIF.
          IF LF_LENGTH >= LC_PART1 AND
             LF_LENGTH <= LC_PART2.
            LF_OFF2 = LF_LENGTH1.
            LF_LENGTH2  = LF_LENGTH.
            LF_FLG_PART2 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART2 AND
             LF_LENGTH <= LC_PART3.
            LF_OFF3 = LF_LENGTH2.
            LF_LENGTH3  = LF_LENGTH.
            LF_FLG_PART3 = ABAP_ON.
          ENDIF.
          IF LF_LENGTH >= LC_PART3 AND
             LF_LENGTH <= LC_PART4.
            LF_OFF4 = LF_LENGTH3.
            LF_LENGTH4  = LF_LENGTH.
            LF_FLG_PART4 = ABAP_ON.
          ENDIF.
        ENDDO.
        CLEAR: LF_LEN1,LF_LEN2,LF_LEN3,LF_LEN4.
        LF_LEN1 = LF_LENGTH1.
        LF_LEN2 = LF_LENGTH2.
        LF_LEN3 = LF_LENGTH3.
        LF_LEN4 = LF_LENGTH4.

        LS_EXTENSIONIN-VALUEPART1 = <LF_STRUCTURE>+LF_OFF1(LF_LEN1).
        IF LF_FLG_PART2 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART2 = <LF_STRUCTURE>+LF_OFF2(LF_LEN2).
        ENDIF.
        IF LF_FLG_PART3 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART3 = <LF_STRUCTURE>+LF_OFF3(LF_LEN3).
        ENDIF.
        IF LF_FLG_PART4 = ABAP_ON.
          LS_EXTENSIONIN-VALUEPART4 = <LF_STRUCTURE>+LF_OFF4(LF_LEN4).
        ENDIF.
      ENDIF.

      APPEND LS_EXTENSIONIN TO LT_EXTENSIONIN.

      CLEAR:
        LV_SALES_UNIT_CONV.

    ENDLOOP.


    "Export to routine 902 IN VOFM  -> FRM_KONDI_WERT_902
    EXPORT LT_CONDITION_ITEM FROM LT_CONDITION_ITEM TO MEMORY ID 'CONDTYP_SO_FROM_SF'.
*-------------------------------------
* Sales Order Create
*-------------------------------------
    DATA: LOGIC_SWITCH  TYPE BAPISDLS.
    LOGIC_SWITCH-NOSTRUCTURE = 'X'.

    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
        ORDER_HEADER_IN    = LS_HEADER
        TESTRUN            = IF_TESTRUN
        LOGIC_SWITCH       = LOGIC_SWITCH
      IMPORTING
        SALESDOCUMENT      = EF_SALES_DOC
      TABLES
        RETURN             = LT_RETURN
        ORDER_ITEMS_IN     = LT_ITEM
        ORDER_SCHEDULES_IN = LT_SCHEDULES_IN
        ORDER_PARTNERS     = LT_PARTNERS
       ORDER_CONDITIONS_IN = LT_ORDER_CONDITIONS
        EXTENSIONIN        = LT_EXTENSIONIN.

    IF LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ) OR
       LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


    ELSE.

      READ TABLE LT_RETURN WITH KEY TYPE   = 'S'
                                    ID     = 'V1'
                                    NUMBER = '311' TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'
*         IMPORTING
*           RETURN        =
          .
      ENDIF.
    ENDIF.

    READ TABLE LT_RETURN INTO data(LS_RETURN) WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
      EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
    ELSE.
      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'S'
                                                   ID = 'V1'
                                                   NUMBER = '311'.
      IF SY-SUBRC = 0.
        EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
        EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
      ELSE.
        READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
        IF SY-SUBRC = 0.
          EF_RESPONSE-RESP_STATUS  = LS_RETURN-TYPE.
          EF_RESPONSE-RESP_MESSAGE = LS_RETURN-MESSAGE.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<LS_RETURN>).
      LS_RESPONSE-RESP_STATUS  = <LS_RETURN>-TYPE.
      LS_RESPONSE-RESP_MESSAGE = <LS_RETURN>-MESSAGE.
      APPEND LS_RESPONSE TO CT_RESPONSE.
    ENDLOOP.

  ENDMETHOD.


  METHOD SALES_ORDER_PROCESS.

*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : Sales Order Create/Change via BAPI
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA: LV_INVALID    TYPE FLAG,
          LV_SO_PROCESS TYPE C.

* Get constant from table ZSDSCAC001
    GET_CONSTANTS( CHANGING CT_RESPONSE = CT_RESPONSE ).

    READ TABLE CT_RESPONSE INTO DATA(LS_RESPONSE) WITH KEY RESP_STATUS = GC_ERROR.
    IF SY-SUBRC = 0.
* Assign Log status from Response structure
      EF_RESPONSE-RESP_STATUS  = LS_RESPONSE-RESP_STATUS .
      EF_RESPONSE-RESP_MESSAGE = LS_RESPONSE-RESP_MESSAGE.
      RETURN.
    ENDIF.

    IF CS_SALESORDER-SAPSALESORDERNO = SPACE.
      LV_SO_PROCESS = 'X'.          "Create
    ELSE.
      LV_SO_PROCESS = 'U'.          "Update
    ENDIF.

* Process input validation
    VALIDATE_INPUT(
      EXPORTING
        IF_RECORD_MODE    = LV_SO_PROCESS
      IMPORTING
        EF_INVALID        = LV_INVALID
      CHANGING
        CS_SALESORDER     = CS_SALESORDER
        CT_SALESORDERITEM = CT_SALESORDERITEM
        CT_RESPONSE       = CT_RESPONSE
        ).

    IF LV_INVALID = SPACE. "No Error
      CASE LV_SO_PROCESS.
        WHEN  'X'.
          SALES_ORDER_CREATE(
            EXPORTING
              IF_TESTRUN        = IF_TESTRUN
            IMPORTING
              EF_SALES_DOC      = EF_SALES_DOC
              EF_RESPONSE       = EF_RESPONSE
            CHANGING
              CS_SALESORDER     = CS_SALESORDER
              CT_SALESORDERITEM = CT_SALESORDERITEM
              CT_RESPONSE       = CT_RESPONSE ).
        WHEN 'U'.
          SALES_ORDER_CHANGE(
            EXPORTING
              IF_TESTRUN        = IF_TESTRUN
            IMPORTING
              EF_SALES_DOC      = EF_SALES_DOC
              EF_RESPONSE       = EF_RESPONSE
            CHANGING
              CS_SALESORDER     = CS_SALESORDER
              CT_SALESORDERITEM = CT_SALESORDERITEM
              CT_RESPONSE       = CT_RESPONSE ).
      ENDCASE.
    ELSE.
      READ TABLE CT_RESPONSE INTO LS_RESPONSE WITH KEY RESP_STATUS = GC_ERROR.
      IF SY-SUBRC = 0.
* Assign Log status from Response structure
        EF_RESPONSE-RESP_STATUS  = LS_RESPONSE-RESP_STATUS .
        EF_RESPONSE-RESP_MESSAGE = LS_RESPONSE-RESP_MESSAGE.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD VALIDATE_AMOUNT.

    CLEAR: CS_RESPONSE.

* Only when value exist
    IF AMOUNT_IN IS NOT INITIAL.

      CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
        EXPORTING
          INPUT      = AMOUNT_IN
*         INTERNAL   = 'X'
*   IMPORTING
*         OUTPUT     =
        EXCEPTIONS
          NO_NUMERIC = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
        "Please input valid amount
        CS_RESPONSE-RESP_STATUS  = GC_ERROR.
        CS_RESPONSE-RESP_MESSAGE = TEXT-E34.
        RETURN.
      ELSE.
        AMOUNT_OUT = AMOUNT_IN.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD VALIDATE_DATE.
    DATA:
      LV_LENGTH  TYPE  I.

    CLEAR: CS_RESPONSE.

* Length?
    LV_LENGTH = STRLEN( DATE_IN ).
    IF LV_LENGTH NE 8.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E05.
      RETURN.
    ENDIF.

    IF NOT DATE_IN+6(2) BETWEEN 01 AND 31 ##NUMBER_OK.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E05.
      RETURN.
    ENDIF.

* Check month
    IF NOT DATE_IN+4(2) BETWEEN 01 AND 12 ##NUMBER_OK.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E05.
      RETURN.
    ENDIF.

* 1-4th digit is year
    IF NOT DATE_IN+0(4) BETWEEN 1900 AND 2200 AND
       DATE_IN+0(4) NE '9999' ##NUMBER_OK.
*   Wrong Date format. Please use format YYYYMMDD
      CS_RESPONSE-RESP_STATUS  = GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E05.
      RETURN.
    ENDIF.

* Assign Output
    DATE_OUT = DATE_IN.
  ENDMETHOD.


  METHOD VALIDATE_HEADER.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREATE_CHANGE_SO_SRV / VALIDATE_HEADER
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI026
*  Description        : Validate data header data From Daikin Web
*  Purpose            : Validate Header
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*  26.03.2025  420000518  Zulkiff B.  fixing error term of payment
*-----------------------------------------------------------------------

    DATA: LS_RESPONSE    TYPE	ZSDSCAS006,
          LV_AMOUNT      TYPE CHAR20.

    IF CS_SALESORDER-CREATEDATE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E04
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ELSE.
      "Check date
      VALIDATE_DATE( EXPORTING DATE_IN     = CS_SALESORDER-CREATEDATE
                     IMPORTING DATE_OUT    = CS_SALESORDER-CREATEDATE
                               CS_RESPONSE = LS_RESPONSE
      ).
      IF CS_SALESORDER-CREATEDATE IS INITIAL OR LS_RESPONSE-RESP_STATUS = GC_ERROR.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ENDIF.
    ENDIF.


    "Sales Order Type
    IF CS_SALESORDER-DOCUMENTTYPE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E09
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE AUART                           ##NEEDED
        FROM TVAK
       WHERE AUART = @CS_SALESORDER-DOCUMENTTYPE
        INTO @DATA(LV_AUART).
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E10
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "Sales Org
    IF CS_SALESORDER-SALESORG IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E07
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VKORG                              ##NEEDED
        FROM TVKO
       WHERE VKORG = @CS_SALESORDER-SALESORG
        INTO @DATA(LV_VKORG).
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E08
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "DistributionChannel
    IF CS_SALESORDER-DISTRIBUTIONCHANNEL IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E11
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VTWEG                               ##NEEDED
        FROM TVTW
       WHERE VTWEG = @CS_SALESORDER-DISTRIBUTIONCHANNEL
        INTO @DATA(LV_VTWEG).
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E12
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "Division
    IF CS_SALESORDER-DIVISION IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E26
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE SPART                              ##NEEDED
        FROM TSPA
       WHERE SPART = @CS_SALESORDER-DIVISION
        INTO @DATA(LV_SPART).
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E27
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "Sales Office
    IF CS_SALESORDER-SALESOFFICE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E13
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VKBUR                         ##NEEDED
        FROM TVBUR
       WHERE VKBUR = @CS_SALESORDER-SALESOFFICE
        INTO @DATA(LV_VKBUR).
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E14
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "Sales Group
    IF CS_SALESORDER-SALESGROUP IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E15
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      SELECT SINGLE VKGRP                 ##NEEDED
        FROM TVKGR
       WHERE VKGRP = @CS_SALESORDER-SALESGROUP
        INTO @DATA(LV_VKGRP).
      IF SY-SUBRC <> 0.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E16
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    "Customer PO Number
    IF CS_SALESORDER-PONUMBER = SPACE.
      CS_SALESORDER-PONUMBER = '-'.
    ENDIF.


    "Terms of Payment
    IF CS_SALESORDER-PAYMENTTERMS IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E17
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
*DEL- IMS 420000518 >>>
*    ELSE.
*      SELECT SINGLE ZTERM,ZTAGG,ZTAG1   ##WARN_OK "#EC CI_NOORDER
*        FROM T052
*       WHERE ZTERM = @CS_SALESORDER-PAYMENTTERMS
*        INTO @DATA(LS_SF_ZTERM).
*      IF SY-SUBRC <> 0.
*        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
*                                   IF_MESSAGE = TEXT-E18
*                          CHANGING CT_RESPONSE = CT_RESPONSE ).
*      ELSE.
*        "Compare 'Days from Baseline Date for Payment' between BP Master and data from salesforce
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = CS_SALESORDER-BPPAYERNUMBER
*          IMPORTING
*            OUTPUT = CS_SALESORDER-BPPAYERNUMBER.
*
*        SELECT SINGLE KUNNR,BUKRS,ZTERM   ##WARN_OK    "#EC CI_NOORDER
*          FROM KNB1
*          WHERE KUNNR = @CS_SALESORDER-BPPAYERNUMBER
*            AND BUKRS IN @GRT_COMPANYCODE
*           INTO @DATA(LV_KNB1_ZTERM).
*        IF SY-SUBRC <> 0.
*          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
*                                     IF_MESSAGE = TEXT-E21
*                            CHANGING CT_RESPONSE = CT_RESPONSE ).
*        ELSE.
*          SELECT SINGLE ZTERM,ZTAGG,ZTAG1     ##WARN_OK  "#EC CI_NOORDER
*            FROM T052
*           WHERE ZTERM = @LV_KNB1_ZTERM-ZTERM
*            INTO @DATA(LS_KNB1_ZTERM).
*          IF SY-SUBRC = 0.
*            "Compare between BP Master and data from salesforce
*            IF LS_SF_ZTERM-ZTAG1 > LS_KNB1_ZTERM-ZTAG1.
*              APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
*                                         IF_MESSAGE = TEXT-E22
*                                CHANGING CT_RESPONSE = CT_RESPONSE ).
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*<<<DEL- IMS 420000518
    ENDIF.

    "Customer Sold-to
    IF CS_SALESORDER-BPSOLDTONUMBER IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E23
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      "Check One-time customer in (KNA1) If customer account (KNA1-KTOKD) = ‘Z060’ One Time
      "Define require field : OnetimeName1, Street
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPSOLDTONUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPSOLDTONUMBER.

      SELECT SINGLE KUNNR                         ##NEEDED
        FROM KNA1
       WHERE KUNNR = @CS_SALESORDER-BPSOLDTONUMBER
        INTO @DATA(LS_KNA1).
      IF SY-SUBRC <> 0.
        "Customer Sold-to does not exist
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E25
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ENDIF.
    ENDIF.

    IF CS_SALESORDER-BPSHIPTONUMBER IS INITIAL.
      "Missing Customer Ship-to
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E28
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPSHIPTONUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPSHIPTONUMBER.
    ENDIF.

    IF CS_SALESORDER-BPBILLTONUMBER IS INITIAL.
      "Missing Customer Bill-to
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E29
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPBILLTONUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPBILLTONUMBER.
    ENDIF.

    IF CS_SALESORDER-BPPAYERNUMBER IS INITIAL.
      "Missing Customer Payer
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E30
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPPAYERNUMBER
        IMPORTING
          OUTPUT = CS_SALESORDER-BPPAYERNUMBER.
    ENDIF.

    IF CS_SALESORDER-BPSALESEMPLOYEE IS INITIAL.
      "Missing Customer Sales Employee
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E31
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = CS_SALESORDER-BPSALESEMPLOYEE
        IMPORTING
          OUTPUT = CS_SALESORDER-BPSALESEMPLOYEE.
    ENDIF.

*ZDH5' Header Special Discount (%)
      IF CS_SALESORDER-HeaderSpecialDiscountPer IS NOT INITIAL.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = CS_SALESORDER-HeaderSpecialDiscountPer.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          CS_SALESORDER-HeaderSpecialDiscountPer = LV_AMOUNT.
        ENDIF.
      ENDIF.

*ZDH6' Header Special Discount (Val.)
      IF CS_SALESORDER-HeaderSpecialDiscountVal IS NOT INITIAL.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = CS_SALESORDER-HeaderSpecialDiscountVal.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          CS_SALESORDER-HeaderSpecialDiscountVal = LV_AMOUNT.
        ENDIF.
      ENDIF.

    READ TABLE CT_RESPONSE INTO LS_RESPONSE WITH KEY RESP_STATUS = GC_ERROR.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD VALIDATE_HEADER_CHANGE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / VALIDATE_HEADER_CHANGE
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI026
*  Description        : Validate data header data From Daikin Web
*  Purpose            : Validate Header(Change)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA: LS_RESPONSE TYPE  ZSDSCAS006,
          LV_MSG      TYPE ZSDSDE_REST_MESSAGE.

    IF CS_SALESORDER-CREATEDATE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E04
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ELSE.
      "Check date
      VALIDATE_DATE( EXPORTING DATE_IN     = CS_SALESORDER-CREATEDATE
                     IMPORTING DATE_OUT    = CS_SALESORDER-CREATEDATE
                               CS_RESPONSE = LS_RESPONSE
      ).
      IF CS_SALESORDER-CREATEDATE IS INITIAL OR LS_RESPONSE-RESP_STATUS = GC_ERROR.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    SELECT SINGLE VBELN         ##NEEDED
      FROM VBAK
     WHERE VBELN = @CS_SALESORDER-SAPSALESORDERNO
      INTO @DATA(LV_VBELN).
    IF SY-SUBRC <> 0.
      "// Error : Sales Order XXXXXXXXXX does not exist
      LV_MSG = TEXT-E59.
      REPLACE '&' IN LV_MSG WITH CS_SALESORDER-SAPSALESORDERNO .
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = LV_MSG
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.


    READ TABLE CT_RESPONSE WITH KEY RESP_STATUS = GC_ERROR TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD VALIDATE_HEADER_CREATE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / VALIDATE_HEADER_CREATE
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI023
*  Description        : Validate data header data from Daikin Web
*  Purpose            : Validate Header
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA: LS_RESPONSE  TYPE	ZSDSCAS006.

    "Salesforce Order ID
    IF CS_SALESORDER-WEBORDERNO IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E02
                        CHANGING CT_RESPONSE = CT_RESPONSE ).
    ENDIF.

    "Check if duplicate
    IF CS_SALESORDER-WEBORDERNO IS NOT INITIAL.
      SELECT SINGLE BNAME                         ##NEEDED  ##WARN_OK
        FROM VBAK
       WHERE BNAME = @CS_SALESORDER-WEBORDERNO
        INTO @DATA(LV_BNAME).
      IF SY-SUBRC = 0.
        "// Error duplicate
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E03
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ENDIF.
    ENDIF.

    IF CS_SALESORDER-CREATEDATE IS INITIAL.
      APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                 IF_MESSAGE = TEXT-E04
                        CHANGING CT_RESPONSE = CT_RESPONSE  ).
    ELSE.
      "Check date
      VALIDATE_DATE( EXPORTING DATE_IN     = CS_SALESORDER-CREATEDATE
                     IMPORTING DATE_OUT    = CS_SALESORDER-CREATEDATE
                               CS_RESPONSE = LS_RESPONSE
      ).
      IF CS_SALESORDER-CREATEDATE IS INITIAL OR LS_RESPONSE-RESP_STATUS = GC_ERROR.
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDIF.

    READ TABLE CT_RESPONSE INTO LS_RESPONSE WITH KEY RESP_STATUS = GC_ERROR.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD VALIDATE_INPUT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / VALIDATE_INPUT
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI023
*  Description        : Validate data input from Daikin Web
*  Purpose            : Validate Header / Item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Initialize Output
    CLEAR: EF_INVALID.

    CASE IF_RECORD_MODE.
      WHEN  'X' .
        VALIDATE_HEADER_CREATE(
           EXPORTING
             IF_RECORD_MODE    = IF_RECORD_MODE
           IMPORTING
             EF_INVALID        = EF_INVALID
           CHANGING
             CS_SALESORDER     = CS_SALESORDER
             CT_RESPONSE       = CT_RESPONSE
       ).

      WHEN 'U'.
        VALIDATE_HEADER_CHANGE(
           EXPORTING
             IF_RECORD_MODE    = IF_RECORD_MODE
           IMPORTING
             EF_INVALID        = EF_INVALID
           CHANGING
             CS_SALESORDER     = CS_SALESORDER
             CT_RESPONSE       = CT_RESPONSE
       ).

        IF EF_INVALID = SPACE." No Error
          VALIDATE_ITEM_CHANGE(
             EXPORTING
               IF_RECORD_MODE    = IF_RECORD_MODE
             IMPORTING
               EF_INVALID        = EF_INVALID
             CHANGING
               CS_SALESORDER     = CS_SALESORDER
               CT_SALESORDERITEM = CT_SALESORDERITEM
               CT_RESPONSE       = CT_RESPONSE
         ).
        ENDIF.

    ENDCASE.

    "Validate both Create and Change
    VALIDATE_HEADER(
           EXPORTING
             IF_RECORD_MODE    = IF_RECORD_MODE
           IMPORTING
             EF_INVALID        = EF_INVALID
           CHANGING
             CS_SALESORDER     = CS_SALESORDER
             CT_RESPONSE       = CT_RESPONSE
       ).

    VALIDATE_ITEM(
       EXPORTING
         IF_RECORD_MODE    = IF_RECORD_MODE
         IS_SALESORDER     = CS_SALESORDER
       IMPORTING
         EF_INVALID        = EF_INVALID
       CHANGING
         CT_SALESORDERITEM = CT_SALESORDERITEM
         CT_RESPONSE       = CT_RESPONSE
   ).

    READ TABLE CT_RESPONSE WITH KEY RESP_STATUS = GC_ERROR TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD VALIDATE_ITEM.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / VALIDATE_ITEM
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI026
*  Description        : Validate data item data From Daikin Web
*  Purpose            : Validate Header
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LS_RESPONSE    TYPE ZSDSCAS006,
          LV_MATNR       TYPE MATNR,
          LV_AMOUNT      TYPE CHAR20,
          LV_MEINS       TYPE MEINS,
          LV_MSG         TYPE ZSDSDE_REST_MESSAGE.

    LOOP AT CT_SALESORDERITEM ASSIGNING FIELD-SYMBOL(<LFS_SALESORDERITEM>).

      IF <LFS_SALESORDERITEM>-ITEMNUMBER IS INITIAL.
        "Missing Item Number
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E35
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-ITEMNUMBER.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E51
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          <LFS_SALESORDERITEM>-ITEMNUMBER = LV_AMOUNT.
        ENDIF.
      ENDIF.

      IF <LFS_SALESORDERITEM>-MATERIALNUMBER IS INITIAL.
        "Missing material number
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E36
                          CHANGING CT_RESPONSE = CT_RESPONSE  ).
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT        = <LFS_SALESORDERITEM>-MATERIALNUMBER
          IMPORTING
            OUTPUT       = LV_MATNR
          EXCEPTIONS
            LENGTH_ERROR = 1
            OTHERS       = 2.
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E37
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.

        SELECT SINGLE MATNR     ##NEEDED  ##WARN_OK
          FROM MVKE
         WHERE MATNR = @LV_MATNR
           AND VKORG = @IS_SALESORDER-SALESORG
         INTO @DATA(LS_MVKE).
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E37
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ENDIF.
      ENDIF.

      "Quantity
      IF <LFS_SALESORDERITEM>-QUANTITY IS INITIAL.
        "Missing target quantity
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E38
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-QUANTITY.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE   = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ELSE.
          <LFS_SALESORDERITEM>-QUANTITY = LV_AMOUNT.
        ENDIF.
      ENDIF.

      "SalesUnit
      IF <LFS_SALESORDERITEM>-SALESUNIT IS INITIAL.
        "Missing Sales Unit
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E39
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        CLEAR: LV_MEINS.
        LV_MEINS = <LFS_SALESORDERITEM>-SALESUNIT.

* Validate in table
        SELECT SINGLE MSEHI
          INTO LV_MEINS
          FROM T006
         WHERE MSEHI EQ LV_MEINS.
        IF SY-SUBRC NE 0.

          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              INPUT          = <LFS_SALESORDERITEM>-SALESUNIT
            IMPORTING
              OUTPUT         = LV_MEINS
            EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
          IF SY-SUBRC <> 0.
            LV_MSG = |{ TEXT-E40 } { <LFS_SALESORDERITEM>-SALESUNIT }|.
            APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                       IF_MESSAGE = LV_MSG
                              CHANGING CT_RESPONSE = CT_RESPONSE ).
          ENDIF.
          SELECT SINGLE MSEHI
            INTO LV_MEINS
            FROM T006
           WHERE MSEHI EQ LV_MEINS.
          IF SY-SUBRC NE 0.
            CLEAR: LV_MSG.
*         Invalid unit:
            LV_MSG = |{ TEXT-E40 } { <LFS_SALESORDERITEM>-SALESUNIT }|.
            APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                       IF_MESSAGE = LV_MSG
                              CHANGING CT_RESPONSE = CT_RESPONSE ).
          ENDIF.
        ENDIF.
      ENDIF.

      "Plant
      IF <LFS_SALESORDERITEM>-PLANT IS INITIAL.
        "Missing Plant
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E41
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        SELECT SINGLE WERKS           ##NEEDED
          FROM T001W
         WHERE WERKS = @<LFS_SALESORDERITEM>-PLANT
          INTO @DATA(LV_WERKS).
        IF SY-SUBRC <> 0.
          "Plant does not exist
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E42
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.
      ENDIF.


      "StorageLocation
      IF <LFS_SALESORDERITEM>-STORAGELOCATION IS NOT INITIAL.
        SELECT SINGLE WERKS,LGORT         ##NEEDED
          FROM T001L
         WHERE WERKS = @<LFS_SALESORDERITEM>-PLANT
           AND LGORT = @<LFS_SALESORDERITEM>-STORAGELOCATION
          INTO @DATA(LS_T001L).
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E43
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ENDIF.
      ENDIF.

      "ItemCategory
      IF <LFS_SALESORDERITEM>-ITEMCATEGORY IS INITIAL.
        "Missing Item Category
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E44
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ELSE.
        SELECT SINGLE PSTYV             ##NEEDED
          FROM TVAP
         WHERE PSTYV = @<LFS_SALESORDERITEM>-ITEMCATEGORY
          INTO @DATA(LV_PSTYV).
        IF SY-SUBRC <> 0.
          "Item Category does not exist
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E45
                            CHANGING CT_RESPONSE = CT_RESPONSE  ).
        ENDIF.
      ENDIF.

      "LOB
      IF <LFS_SALESORDERITEM>-LOB IS NOT INITIAL.
        SELECT SINGLE LOB           ##NEEDED  ##WARN_OK
          FROM ZDSMMC033
         WHERE LOB = @<LFS_SALESORDERITEM>-LOB
          INTO @DATA(LS_LOB).
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E47
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.
      ENDIF.

      "ReasonForReject
      IF <LFS_SALESORDERITEM>-REASONFORREJECT IS NOT INITIAL.
        SELECT SINGLE ABGRU         ##NEEDED
          FROM TVAG
         WHERE ABGRU = @<LFS_SALESORDERITEM>-REASONFORREJECT
          INTO @DATA(LS_ABGRU).
        IF SY-SUBRC <> 0.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = TEXT-E48
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ENDIF.
      ENDIF.


      "ListPrice":
      IF <LFS_SALESORDERITEM>-LISTPRICE IS NOT INITIAL.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-LISTPRICE.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          <LFS_SALESORDERITEM>-LISTPRICE = LV_AMOUNT.
        ENDIF.
      ENDIF.

      "ItemDiscountPer":
      IF <LFS_SALESORDERITEM>-ITEMDISCOUNTPER IS NOT INITIAL.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-ITEMDISCOUNTPER.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          <LFS_SALESORDERITEM>-ITEMDISCOUNTPER = LV_AMOUNT.
        ENDIF.
      ENDIF.

      "ItemDiscountVal":
      IF <LFS_SALESORDERITEM>-ITEMDISCOUNTVAL IS NOT INITIAL.
        CLEAR: LV_AMOUNT .
        LV_AMOUNT = <LFS_SALESORDERITEM>-ITEMDISCOUNTVAL.
        CONDENSE LV_AMOUNT.
        VALIDATE_AMOUNT( EXPORTING AMOUNT_IN     = LV_AMOUNT
                         IMPORTING AMOUNT_OUT    = LV_AMOUNT
                                   CS_RESPONSE = LS_RESPONSE
                        ).
        IF LS_RESPONSE-RESP_STATUS = GC_ERROR.
          APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                     IF_MESSAGE = LS_RESPONSE-RESP_MESSAGE
                            CHANGING CT_RESPONSE = CT_RESPONSE ).
        ELSE.
          <LFS_SALESORDERITEM>-ITEMDISCOUNTVAL = LV_AMOUNT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    READ TABLE CT_RESPONSE INTO LS_RESPONSE WITH KEY RESP_STATUS = GC_ERROR.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD VALIDATE_ITEM_CHANGE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_SO_DEALER_CLAIM_SRV / VALIDATE_ITEM_CHANGE
*  Creation Date      : 11.06.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI026
*  Description        : Validate data header data From Daikin Web (Dealer Claim)
*  Purpose            : Validate Header(Change)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    LOOP AT CT_SALESORDERITEM ASSIGNING FIELD-SYMBOL(<LFS_SALESORDERITEM>).

      IF NOT ( <LFS_SALESORDERITEM>-FLAG = 'I' OR <LFS_SALESORDERITEM>-FLAG = 'U' OR <LFS_SALESORDERITEM>-FLAG = 'D' ).
        "Item: Missing indicator flag : Missing indicator flag : 'U' for Change or 'D' for Delete
        APPEND_RESPONSE( EXPORTING IF_STATUS = GC_ERROR
                                   IF_MESSAGE = TEXT-E61
                          CHANGING CT_RESPONSE = CT_RESPONSE ).
      ENDIF.
    ENDLOOP.

    READ TABLE CT_RESPONSE WITH KEY RESP_STATUS = GC_ERROR TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      EF_INVALID = 'X'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
