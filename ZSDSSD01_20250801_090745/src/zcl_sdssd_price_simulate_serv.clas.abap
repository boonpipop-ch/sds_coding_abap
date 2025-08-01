CLASS ZCL_SDSSD_PRICE_SIMULATE_SERV DEFINITION
  PUBLIC
  INHERITING FROM ZCL_SDSCA_REST_SERVICE
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TS_MARA,
        MATNR TYPE MARA-MATNR,
        MEINS TYPE MARA-MEINS,
        SPART TYPE MARA-SPART,
      END OF TS_MARA .
    TYPES:
      TT_MARA TYPE SORTED TABLE OF TS_MARA
                            WITH UNIQUE KEY MATNR .
    TYPES:
      BEGIN OF TS_MVKE,
        MATNR TYPE MVKE-MATNR,
        VKORG TYPE MVKE-VKORG,
        VTWEG TYPE MVKE-VTWEG,
      END OF TS_MVKE .
    TYPES:
      TT_MVKE TYPE SORTED TABLE OF TS_MVKE
                            WITH UNIQUE KEY MATNR
                                            VKORG
                                            VTWEG .
    TYPES:
      BEGIN OF TS_T006,
        MSEHI TYPE T006-MSEHI,
      END OF TS_T006 .
    TYPES:
      TT_T006 TYPE SORTED TABLE OF TS_T006
                            WITH UNIQUE KEY MSEHI .
    TYPES:
      BEGIN OF TS_KNVV,
        KUNNR TYPE KNVV-KUNNR,
        VKORG TYPE KNVV-VKORG,
        VTWEG TYPE KNVV-VTWEG,
        SPART TYPE KNVV-SPART,
        KVGR1 TYPE KNVV-KVGR1,
        KVGR2 TYPE KNVV-KVGR2,
      END OF TS_KNVV .

    TYPES: BEGIN OF TY_KSCHL,
             KSCHL TYPE KSCHL,
           END OF TY_KSCHL.
    TYPES TTY_KSCHL TYPE HASHED TABLE OF TY_KSCHL
                         WITH UNIQUE KEY KSCHL.

    TYPES:
      TT_KSCHL_RANGE TYPE RANGE OF KSCHL .

    METHODS PROCESS_DATA
        REDEFINITION .
    METHODS INITIALIZE_DATA
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF GC_DEFAULT,
        ETENR        TYPE ETENR VALUE '0001',
        PARVW_SHIPTO TYPE PARVW VALUE 'WE',
      END OF GC_DEFAULT .
  PRIVATE SECTION.

    DATA GR_COND_TYPE_NET TYPE TT_KSCHL_RANGE .
    DATA GR_COND_TYPE_PCT TYPE TT_KSCHL_RANGE .
    DATA GF_DIVISION TYPE SPART .
    DATA GF_SALEORG TYPE VKORG .
    CONSTANTS GC_REPID TYPE REPID VALUE 'ZCL_SDSSD_PRICE_SIMULATE_SERV' ##NO_TEXT.

    METHODS COLLECT_RESPONSE
      IMPORTING
        !IF_REST_STATUS  TYPE ZSDSDE_REST_STATUS
        !IF_REST_MESSAGE TYPE ZSDSDE_REST_MESSAGE
      CHANGING
        !CT_RESPONSE     TYPE ZSDSCAS006_TT .
    METHODS CONCAT_MESSAGE
      IMPORTING
        !IT_RESPONSE           TYPE ZSDSCAS006_TT
      RETURNING
        VALUE(RF_REST_MESSAGE) TYPE ZSDSDE_REST_MESSAGE .
    METHODS CONVERT_UNIT_IN
      IMPORTING
        !IF_UNIT    TYPE MEINS
      EXPORTING
        !EF_UNIT_IN TYPE MEINS
        !EF_RC      TYPE SYST_SUBRC
        !EF_MSG_TX  TYPE ZSDSDE_REST_MESSAGE .
    METHODS CONVERT_UNIT_OUT
      IMPORTING
        !IF_UNIT     TYPE MEINS
      EXPORTING
        !EF_UNIT_OUT TYPE MEINS
        !EF_RC       TYPE SYST_SUBRC
        !EF_MSG_TX   TYPE ZSDSDE_REST_MESSAGE .
    METHODS GET_CONSTANT .
    METHODS GET_MD_UNIT
      IMPORTING
        !IT_KEY  TYPE TT_T006
      EXPORTING
        !ET_T006 TYPE TT_T006 .
    METHODS GET_SALE_FOR_MATERIAL
      IMPORTING
        !IT_MVKE TYPE TT_MVKE
      EXPORTING
        !ET_MVKE TYPE TT_MVKE .
    METHODS PROCESS_PRICE_SIMULATE
      IMPORTING
        !IS_REQUEST     TYPE ZSDSSDS007
        !IS_BAPI_SDHEAD TYPE BAPISDHEAD
        !IT_BAPI_ITEMIN TYPE BAPIITEMIN_TT
        !IT_BAPI_PARTNR TYPE ESALES_BAPIPARTNR_TAB
        !IT_BAPI_SCHDL  TYPE ESALES_BAPISCHDL_TAB
      CHANGING
        !CS_RESPONSE    TYPE ZSDSSDS007 .
    METHODS REPLACE_MESSAGE
      IMPORTING
        !IF_MSGTX       TYPE ZSDSDE_REST_MESSAGE
        !IF_MSG1        TYPE CLIKE OPTIONAL
        !IF_MSG2        TYPE CLIKE OPTIONAL
        !IF_MSG3        TYPE CLIKE OPTIONAL
        !IF_MSG4        TYPE CLIKE OPTIONAL
      RETURNING
        VALUE(RF_MSGTX) TYPE ZSDSDE_REST_MESSAGE .
    METHODS UPDATE_QUOTE_ITEM
      IMPORTING
        !IT_QUOTE_ITEM   TYPE ZSDSSDS007_TT
        !IT_BAPI_ITEMOUT TYPE ESALES_BAPIITEMEX_TAB
        !IT_BAPI_COND    TYPE ESALES_BAPICOND_TAB
      CHANGING
        !CT_QUOTE_ITEM   TYPE ZSDSSDS007_TT .
    METHODS VALIDATE_CUSTOMER
      IMPORTING
        !IF_KUNNR       TYPE KUNNR
        !IF_VKORG       TYPE VKORG
        !IF_VTWEG       TYPE VTWEG
        !IF_SPART       TYPE SPART
      EXPORTING
        !EF_KVGR1       TYPE KVGR1
        !EF_KVGR2       TYPE KVGR2
        !ET_BAPI_PARTNR TYPE ESALES_BAPIPARTNR_TAB
      CHANGING
        !CT_RESPONSE    TYPE ZSDSCAS006_TT .
    METHODS VALIDATE_DIST_CHANNEL
      IMPORTING
        !IF_VTWEG    TYPE VTWEG
      EXPORTING
        !EF_VTWEG    TYPE VTWEG
      CHANGING
        !CT_RESPONSE TYPE ZSDSCAS006_TT .
    METHODS VALIDATE_DOC_TYPE
      IMPORTING
        !IF_AUART    TYPE AUART
      EXPORTING
        !EF_AUART    TYPE AUART
      CHANGING
        !CT_RESPONSE TYPE ZSDSCAS006_TT .
    METHODS VALIDATE_HEADER
      IMPORTING
        !IS_HEADER      TYPE ZSDSSDS007
      EXPORTING
        !ES_BAPI_SDHEAD TYPE BAPISDHEAD
        !ET_BAPI_PARTNR TYPE ESALES_BAPIPARTNR_TAB
      CHANGING
        !CS_RESPONSE    TYPE ZSDSSDS007 .
    METHODS VALIDATE_ITEM
      IMPORTING
        !IF_VKORG       TYPE VKORG
        !IF_VTWEG       TYPE VTWEG
      EXPORTING
        !ET_BAPI_ITEMIN TYPE BAPIITEMIN_TT
        !ET_BAPI_SCHDL  TYPE ESALES_BAPISCHDL_TAB
      CHANGING
        !CS_RESPONSE    TYPE ZSDSSDS007 .
    METHODS VALIDATE_PAYMENT_TERM
      IMPORTING
        !IF_ZTERM    TYPE DZTERM
      EXPORTING
        !EF_ZTERM    TYPE DZTERM
      CHANGING
        !CT_RESPONSE TYPE ZSDSCAS006_TT .
    METHODS VALIDATE_REQUEST
      IMPORTING
        !IS_REQUEST     TYPE ZSDSSDS007
      EXPORTING
        !EF_INVALID     TYPE FLAG
        !ES_BAPI_SDHEAD TYPE BAPISDHEAD
        !ET_BAPI_ITEMIN TYPE BAPIITEMIN_TT
        !ET_BAPI_PARTNR TYPE ESALES_BAPIPARTNR_TAB
        !ET_BAPI_SCHDL  TYPE ESALES_BAPISCHDL_TAB
      CHANGING
        !CS_RESPONSE    TYPE ZSDSSDS007 .
    METHODS VALIDATE_SALE_GROUP
      IMPORTING
        !IF_VKGRP    TYPE VKGRP
      EXPORTING
        !EF_VKGRP    TYPE VKGRP
      CHANGING
        !CT_RESPONSE TYPE ZSDSCAS006_TT .
ENDCLASS.



CLASS ZCL_SDSSD_PRICE_SIMULATE_SERV IMPLEMENTATION.


  METHOD COLLECT_RESPONSE.

    DATA LS_RESPONSE TYPE ZSDSCAS006.

    IF IF_REST_STATUS IS INITIAL OR
       IF_REST_MESSAGE IS INITIAL.
      RETURN.
    ENDIF.

    LS_RESPONSE-RESP_STATUS  = IF_REST_STATUS.
    LS_RESPONSE-RESP_MESSAGE = IF_REST_MESSAGE.

    COLLECT LS_RESPONSE INTO CT_RESPONSE.

  ENDMETHOD.


  METHOD CONCAT_MESSAGE.

    CLEAR RF_REST_MESSAGE.

    DATA(LT_RESPONSE) = IT_RESPONSE.
    SORT LT_RESPONSE BY RESP_STATUS
                        RESP_MESSAGE.

    DELETE ADJACENT DUPLICATES FROM LT_RESPONSE COMPARING RESP_STATUS
                                                          RESP_MESSAGE.

    LOOP AT LT_RESPONSE ASSIGNING FIELD-SYMBOL(<L_RESPONSE>)
   WHERE RESP_STATUS = 'E'.

      IF RF_REST_MESSAGE IS INITIAL.
        RF_REST_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
      ELSE.
        RF_REST_MESSAGE = |{ RF_REST_MESSAGE },{ <L_RESPONSE>-RESP_MESSAGE }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD CONVERT_UNIT_IN.

    CLEAR:
      EF_RC,
      EF_UNIT_IN,
      EF_MSG_TX.

    IF IF_UNIT IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT          = IF_UNIT
      IMPORTING
        OUTPUT         = EF_UNIT_IN
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      EF_RC = SY-SUBRC.
      MESSAGE ID SY-MSGID TYPE 'I'
      NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              INTO EF_MSG_TX.
    ENDIF.

  ENDMETHOD.


  METHOD CONVERT_UNIT_OUT.

    CLEAR:
      EF_RC,
      EF_UNIT_OUT,
      EF_MSG_TX.

    IF IF_UNIT IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT          = IF_UNIT
      IMPORTING
        OUTPUT         = EF_UNIT_OUT
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      EF_RC = SY-SUBRC.
      MESSAGE ID SY-MSGID TYPE 'I'
      NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              INTO EF_MSG_TX.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CONSTANT.

    CONSTANTS:
      LC_CONDTYPE_NET  TYPE  ZSDSDE_PARAM_NAME VALUE 'COND_TYPE_NET',
      LC_DIVISION      TYPE  ZSDSDE_PARAM_NAME VALUE 'DIVISION',
      LC_SALEORG       TYPE  ZSDSDE_PARAM_NAME VALUE 'SALESORG',
      LC_PARAM_EXT_PCT TYPE ZSDSCAC001-PARAM_EXT VALUE 'PERCENT'.

    DATA:
      LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

* Initialize Output
    CLEAR: GF_DIVISION,
           GF_SALEORG,
           GR_COND_TYPE_NET,
           GR_COND_TYPE_PCT.

* Read All GenC constants for program
    CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
      EXPORTING
        IF_REPID = GC_REPID
      IMPORTING
        ET_GEN_C = LT_GENC.


    LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

      CASE <L_GENC>-PARAM.
        WHEN LC_CONDTYPE_NET.

          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                 INTO TABLE GR_COND_TYPE_NET.

          CASE <L_GENC>-PARAM_EXT.

            WHEN LC_PARAM_EXT_PCT.

              INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                              OPTION = <L_GENC>-PARAM_OPTION
                              LOW    = <L_GENC>-VALUE_LOW
                              HIGH   = <L_GENC>-VALUE_HIGH )
                     INTO TABLE GR_COND_TYPE_PCT.
          ENDCASE.

        WHEN LC_DIVISION.
          GF_DIVISION = <L_GENC>-VALUE_LOW.
        WHEN LC_SALEORG.
          GF_SALEORG = <L_GENC>-VALUE_LOW.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_MD_UNIT.

    CLEAR ET_T006.

    IF IT_KEY IS INITIAL.
      RETURN.
    ENDIF.

    SELECT MSEHI
    INTO TABLE ET_T006
    FROM T006
    FOR ALL ENTRIES IN IT_KEY
    WHERE MSEHI = IT_KEY-MSEHI.

  ENDMETHOD.


  METHOD GET_SALE_FOR_MATERIAL.

    CLEAR ET_MVKE.

    IF IT_MVKE IS INITIAL.
      RETURN.
    ENDIF.

    SELECT MATNR
           VKORG
           VTWEG
    FROM MVKE
    INTO TABLE ET_MVKE
    FOR ALL ENTRIES IN IT_MVKE
    WHERE MATNR = IT_MVKE-MATNR
      AND VKORG = IT_MVKE-VKORG.

  ENDMETHOD.


  METHOD INITIALIZE_DATA.

    GET_CONSTANT( ).

  ENDMETHOD.


  METHOD PROCESS_DATA.

    DATA:
      LF_INVALID     TYPE FLAG,
      LS_REQUEST     TYPE ZSDSSDS007,
      LS_RESPONSE    TYPE ZSDSSDS007,
      LS_BAPI_SDHEAD TYPE BAPISDHEAD,
      LT_BAPI_ITEMIN TYPE BAPIITEMIN_TT,
      LT_BAPI_PARTNR TYPE ESALES_BAPIPARTNR_TAB,
      LT_BAPI_SCHDL  TYPE ESALES_BAPISCHDL_TAB.

    FIELD-SYMBOLS:
      <L_RESPONSE> TYPE ZSDSSDS007.


* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
    IF SY-SUBRC <> 0.
*   Critical error
      RETURN.
    ENDIF.

* Code Processing and Assign Response data
    VALIDATE_REQUEST(
      EXPORTING
        IS_REQUEST     = LS_REQUEST                 " JSON Structure Simulation Price From Salesforce
      IMPORTING
        ES_BAPI_SDHEAD = LS_BAPI_SDHEAD                 " Communication Fields: Sales and Distribution Document Header
        ET_BAPI_ITEMIN = LT_BAPI_ITEMIN                 " TT Kommunikationsfelder Vertriebsbelegposition anlegen WWW
        ET_BAPI_PARTNR = LT_BAPI_PARTNR                 " Communications Fields: SD Document Partner: WWW
        ET_BAPI_SCHDL  = LT_BAPI_SCHDL                  " Table Category for BAPISCHDL
        EF_INVALID     = LF_INVALID
      CHANGING
        CS_RESPONSE    = LS_RESPONSE       " JSON Structure Simulation Price From Salesforce
    ).

    IF LF_INVALID IS INITIAL.
      PROCESS_PRICE_SIMULATE(
        EXPORTING
          IS_REQUEST     = LS_REQUEST           " JSON Structure Simulation Price From Salesforce
          IS_BAPI_SDHEAD = LS_BAPI_SDHEAD       " Communication Fields: Sales and Distribution Document Header
          IT_BAPI_ITEMIN = LT_BAPI_ITEMIN       " TT Kommunikationsfelder Vertriebsbelegposition anlegen WWW
          IT_BAPI_PARTNR = LT_BAPI_PARTNR       " Communications Fields: SD Document Partner: WWW
          IT_BAPI_SCHDL  = LT_BAPI_SCHDL        " Table Category for BAPISCHDL
        CHANGING
          CS_RESPONSE    = LS_RESPONSE        " JSON Structure Simulation Price From Salesforce
      ).
    ENDIF.


    IF LS_RESPONSE-RESP_STATUS IS INITIAL.
* Text-s00: Price simulate success
      LS_RESPONSE-RESP_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_SUCCESS.
      LS_RESPONSE-RESP_MESSAGE = TEXT-S00.
    ENDIF.

    <L_RESPONSE> = LS_RESPONSE.

* Assign Result to Log
    EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
    EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.

  ENDMETHOD.


  METHOD PROCESS_PRICE_SIMULATE.

    DATA: LS_BAPI_SDHEAD  TYPE BAPISDHEAD,
          LT_BAPI_ITEMIN  TYPE BAPIITEMIN_TT,
          LT_BAPI_COND    TYPE ESALES_BAPICOND_TAB,
          LT_BAPI_ITEMOUT TYPE ESALES_BAPIITEMEX_TAB,
          LT_BAPI_PARTNR  TYPE ESALES_BAPIPARTNR_TAB,
          LT_BAPI_MSG     TYPE TT_BAPIRET2,
          LT_BAPI_SCHDL   TYPE ESALES_BAPISCHDL_TAB.

    LS_BAPI_SDHEAD  = IS_BAPI_SDHEAD.
    LT_BAPI_ITEMIN  = IT_BAPI_ITEMIN.
    LT_BAPI_PARTNR  = IT_BAPI_PARTNR.
    LT_BAPI_SCHDL   = IT_BAPI_SCHDL.

    CALL FUNCTION 'BAPI_SALESORDER_SIMULATE'
      EXPORTING
        ORDER_HEADER_IN    = LS_BAPI_SDHEAD
*       CONVERT_PARVW_AUART       = ' '
*   IMPORTING
*       SALESDOCUMENT      =
*       SOLD_TO_PARTY      =
*       SHIP_TO_PARTY      =
*       BILLING_PARTY      =
*       RETURN             =
      TABLES
        ORDER_ITEMS_IN     = LT_BAPI_ITEMIN
        ORDER_PARTNERS     = LT_BAPI_PARTNR
        ORDER_SCHEDULE_IN  = LT_BAPI_SCHDL
        ORDER_ITEMS_OUT    = LT_BAPI_ITEMOUT
*       ORDER_CFGS_REF     =
*       ORDER_CFGS_INST    =
*       ORDER_CFGS_PART_OF =
*       ORDER_CFGS_VALUE   =
*       ORDER_CFGS_BLOB    =
*       ORDER_CCARD        =
*       ORDER_CCARD_EX     =
*       ORDER_SCHEDULE_EX  =
        ORDER_CONDITION_EX = LT_BAPI_COND
*       ORDER_INCOMPLETE   =
        MESSAGETABLE       = LT_BAPI_MSG
*       EXTENSIONIN        =
*       PARTNERADDRESSES   =
*       NFMETALLITMS_IM    =
*       NFMETALLITMS_EX    =
      .


    IF LT_BAPI_COND IS NOT INITIAL.
      UPDATE_QUOTE_ITEM(
        EXPORTING
          IT_QUOTE_ITEM   = IS_REQUEST-QUOTE_ITEM       " Table type for QuoteItem: Simulation Price From Salesforce
          IT_BAPI_ITEMOUT = LT_BAPI_ITEMOUT             " Communication Fields: Issue SD Document Item: WWW
          IT_BAPI_COND    = LT_BAPI_COND                " Table Category for BAPICOND
        CHANGING
          CT_QUOTE_ITEM   = CS_RESPONSE-QUOTE_ITEM      " Table type for QuoteItem: Simulation Price From Salesforce
      ).
    ELSE.
      CS_RESPONSE-RESP_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
      LOOP AT LT_BAPI_MSG ASSIGNING FIELD-SYMBOL(<L_BAPI_MSG>) WHERE TYPE CA 'EAX'.
        COLLECT_RESPONSE( EXPORTING IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
                                    IF_REST_MESSAGE = CONV #( <L_BAPI_MSG>-MESSAGE )
                          CHANGING  CT_RESPONSE     = CS_RESPONSE-RESPONSE ).

        IF CS_RESPONSE-RESP_MESSAGE IS INITIAL.
          CS_RESPONSE-RESP_MESSAGE = <L_BAPI_MSG>-MESSAGE.
        ELSE.
          CS_RESPONSE-RESP_MESSAGE = |{ CS_RESPONSE-RESP_MESSAGE },{ <L_BAPI_MSG>-MESSAGE }|.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD REPLACE_MESSAGE.

    RF_MSGTX = IF_MSGTX.

    REPLACE:
      '&1' WITH IF_MSG1 INTO RF_MSGTX,
      '&2' WITH IF_MSG2 INTO RF_MSGTX,
      '&3' WITH IF_MSG3 INTO RF_MSGTX,
      '&4' WITH IF_MSG4 INTO RF_MSGTX.

    CONDENSE RF_MSGTX.


  ENDMETHOD.


  METHOD UPDATE_QUOTE_ITEM.

    DATA:
      LF_ITM_NUMBER TYPE BAPIITEMEX-ITM_NUMBER,
      LF_UNIT_OUT   TYPE MEINS,
      LF_RC         TYPE SY-SUBRC,
      LF_TABIX      TYPE SY-TABIX,
      LF_FNAME      TYPE LVC_FNAME,
      LF_COND_VALUE TYPE BAPICOND-COND_VALUE.

    FIELD-SYMBOLS:
        <L_CONDVALUE> TYPE BAPIKBETR1.

    CLEAR CT_QUOTE_ITEM.

    IF IT_QUOTE_ITEM IS INITIAL.
      RETURN.
    ENDIF.

    DATA LT_FILTER_KSCHL TYPE TTY_KSCHL.

    LT_FILTER_KSCHL = CORRESPONDING #( GR_COND_TYPE_NET DISCARDING DUPLICATES
                                                        MAPPING KSCHL = LOW ).

    DATA(LT_BAPI_COND)   = FILTER #( IT_BAPI_COND IN LT_FILTER_KSCHL WHERE COND_TYPE = KSCHL ).
    DATA(LT_QUOTE_ITEM)  = IT_QUOTE_ITEM.

    SORT LT_BAPI_COND BY ITM_NUMBER.

    LOOP AT LT_QUOTE_ITEM ASSIGNING FIELD-SYMBOL(<L_QUOTE_ITEM>).

      LF_ITM_NUMBER = <L_QUOTE_ITEM>-ITM_NUMBER.

      APPEND INITIAL LINE TO CT_QUOTE_ITEM
       ASSIGNING FIELD-SYMBOL(<L_QUOTE_ITEM_NEW>).

      <L_QUOTE_ITEM_NEW>-ITM_NUMBER = <L_QUOTE_ITEM>-ITM_NUMBER.
      <L_QUOTE_ITEM_NEW>-MATERIAL   = <L_QUOTE_ITEM>-MATERIAL.
      <L_QUOTE_ITEM_NEW>-TARGET_QTY = <L_QUOTE_ITEM>-TARGET_QTY.

      READ TABLE IT_BAPI_ITEMOUT
      INTO DATA(LS_ITEMOUT)
      WITH KEY ITM_NUMBER = LF_ITM_NUMBER.
      IF SY-SUBRC IS INITIAL.

        <L_QUOTE_ITEM_NEW>-TARGET_QTY  = LS_ITEMOUT-TARGET_QTY * 1000.

        CONVERT_UNIT_OUT(
          EXPORTING
            IF_UNIT     = LS_ITEMOUT-TARGET_QU                 " Base Unit of Measure
          IMPORTING
            EF_UNIT_OUT = LF_UNIT_OUT
            EF_RC       = LF_RC
        ).

        IF LF_RC = 0.
          <L_QUOTE_ITEM_NEW>-TARGET_QU = LF_UNIT_OUT.
        ELSE.
          <L_QUOTE_ITEM_NEW>-TARGET_QU = LS_ITEMOUT-TARGET_QU .
        ENDIF.

      ENDIF.

      READ TABLE LT_BAPI_COND
      TRANSPORTING NO FIELDS
      WITH KEY ITM_NUMBER = LF_ITM_NUMBER
      BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LF_TABIX = SY-TABIX.
        LOOP AT LT_BAPI_COND ASSIGNING FIELD-SYMBOL(<L_BAPI_COND>) FROM LF_TABIX.

          CLEAR LF_COND_VALUE.

          IF <L_BAPI_COND>-ITM_NUMBER = LF_ITM_NUMBER.

            IF <L_BAPI_COND>-COND_TYPE IN GR_COND_TYPE_NET.

              " Check the condition type value as a percentage
              IF <L_BAPI_COND>-COND_TYPE IN GR_COND_TYPE_PCT.

                " Calculate price per unit
                IF <L_QUOTE_ITEM_NEW>-TARGET_QTY IS NOT INITIAL.

                  LF_COND_VALUE = ABS( <L_BAPI_COND>-CONDVALUE / <L_QUOTE_ITEM_NEW>-TARGET_QTY ).

                  LF_COND_VALUE = ROUND( VAL = LF_COND_VALUE DEC = 2 ).
                ELSE.
                  LF_COND_VALUE = ABS( <L_BAPI_COND>-CONDVALUE ).
                ENDIF.

              ELSE.
                LF_COND_VALUE = ABS( <L_BAPI_COND>-COND_VALUE ).
              ENDIF.

              " Fill condition value
              LF_FNAME = |CONDVALUE_{ <L_BAPI_COND>-COND_TYPE }|.
              ASSIGN COMPONENT LF_FNAME
                  OF STRUCTURE <L_QUOTE_ITEM_NEW>
                            TO <L_CONDVALUE>.
              IF SY-SUBRC IS INITIAL.
                <L_CONDVALUE> = LF_COND_VALUE.
              ENDIF.

              "Summary discount
              <L_QUOTE_ITEM_NEW>-CONDVALUE_NET = <L_QUOTE_ITEM_NEW>-CONDVALUE_NET + LF_COND_VALUE.
            ENDIF.

          ELSE.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD VALIDATE_CUSTOMER.

    DATA: LF_KUNNR TYPE KUNNR,
          LF_MSGTX TYPE ZSDSDE_REST_MESSAGE.

    CLEAR: EF_KVGR1,
           EF_KVGR2,
           ET_BAPI_PARTNR.


    IF IF_KUNNR IS INITIAL.
*  Text-m05: Customer Sold-to is required.
      LF_MSGTX = TEXT-M05.
    ELSE.
      LF_KUNNR = |{ IF_KUNNR ALPHA = IN }|.

      ET_BAPI_PARTNR =  VALUE #( ( PARTN_ROLE = GC_DEFAULT-PARVW_SHIPTO
                                   PARTN_NUMB = LF_KUNNR ) ).

      SELECT KUNNR,
             VKORG,
             VTWEG,
             SPART,
             KVGR1,
             KVGR2
        FROM KNVV
        INTO @DATA(LS_KNVV) UP TO 1 ROWS
        WHERE KUNNR = @LF_KUNNR
          AND VKORG = @IF_VKORG
          AND VTWEG = @IF_VTWEG
          AND SPART = @IF_SPART
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      EF_KVGR1 = LS_KNVV-KVGR1.
      EF_KVGR2 = LS_KNVV-KVGR2.
    ENDIF.

    COLLECT_RESPONSE(
      EXPORTING
        IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_REST_MESSAGE = LF_MSGTX
      CHANGING
        CT_RESPONSE     = CT_RESPONSE ).

  ENDMETHOD.


  METHOD VALIDATE_DIST_CHANNEL.

    DATA:
      LF_MSGTX TYPE ZSDSDE_REST_MESSAGE.

    CLEAR EF_VTWEG.

    IF IF_VTWEG IS INITIAL.
*  Text-m03: Distribution Channel is required.
      LF_MSGTX  = TEXT-M03.
    ELSE.
      SELECT VTWEG
        FROM TVTW
        INTO EF_VTWEG UP TO 1 ROWS
        WHERE VTWEG = IF_VTWEG
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      "Text-e05 :Distribution Channel: & does not exist
      IF SY-SUBRC <> 0.
        LF_MSGTX = REPLACE_MESSAGE( IF_MSGTX = TEXT-E05
                                    IF_MSG1  = IF_VTWEG ).
      ENDIF.
    ENDIF.

    COLLECT_RESPONSE(
      EXPORTING
        IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR   " REST Response Status
        IF_REST_MESSAGE = LF_MSGTX                                " Response Message
      CHANGING
        CT_RESPONSE     = CT_RESPONSE                              " Table Type for Response Message Strcuture
    ).

  ENDMETHOD.


  METHOD VALIDATE_DOC_TYPE.

    DATA:
      LF_MSGTX TYPE ZSDSDE_REST_MESSAGE.

    CLEAR EF_AUART.

    IF IF_AUART IS INITIAL.
* Text-m02: Quotation Order Type is required
      LF_MSGTX = TEXT-M02.
    ELSE.
*     Validate with Database
      SELECT AUART
        FROM TVAK
        INTO EF_AUART UP TO 1 ROWS
        WHERE AUART = IF_AUART
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC <> 0.
* Text-e07: Quotation Order Type: &1 does not exist.
        LF_MSGTX = REPLACE_MESSAGE( IF_MSGTX = TEXT-E07
                                    IF_MSG1  = IF_AUART ).

      ENDIF.
    ENDIF.

    COLLECT_RESPONSE(
      EXPORTING
        IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR             " REST Response Status
        IF_REST_MESSAGE = LF_MSGTX                                          " Response Message
      CHANGING
        CT_RESPONSE     = CT_RESPONSE                                       " Table Type for Response Message Strcuture
    ).

  ENDMETHOD.


  METHOD VALIDATE_HEADER.

    CLEAR: ES_BAPI_SDHEAD,
           ET_BAPI_PARTNR.

* Set default
    ES_BAPI_SDHEAD-SALES_ORG = GF_SALEORG.
    ES_BAPI_SDHEAD-DIVISION  = GF_DIVISION.

    IF IS_HEADER-NAME IS INITIAL.
*  Text-m00: Sales Order Type is required
      COLLECT_RESPONSE(
        EXPORTING
          IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR    " REST Response Status
          IF_REST_MESSAGE = TEXT-M00                                 " Response Message
        CHANGING
          CT_RESPONSE     = CS_RESPONSE-RESPONSE                     " Table Type for Response Message Strcuture
      ).
    ELSE.
      ES_BAPI_SDHEAD-NAME  = IS_HEADER-NAME.
    ENDIF.

    IF IS_HEADER-PRICE_DATE IS INITIAL.
*  Text-m01: Pricing Date is required
      COLLECT_RESPONSE(
        EXPORTING
          IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR    " REST Response Status
          IF_REST_MESSAGE = TEXT-M01                                 " Response Message
        CHANGING
          CT_RESPONSE     = CS_RESPONSE-RESPONSE                     " Table Type for Response Message Strcuture
      ).
    ELSE.
      ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_ISO_TO_DATE_TIME(
        EXPORTING
          IF_INPUT         = IS_HEADER-PRICE_DATE   " Date in ISO Format
        IMPORTING
          EF_DATUM         = ES_BAPI_SDHEAD-PRICE_DATE                 " Date
        EXCEPTIONS
          CONVERSION_ERROR = 1
          OTHERS           = 2
      ).
      IF SY-SUBRC <> 0.
* text-e00: Invalid Pricing date value &1.
        COLLECT_RESPONSE(
          EXPORTING
            IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR               " REST Response Status
            IF_REST_MESSAGE = REPLACE_MESSAGE( IF_MSGTX = TEXT-E00                " Response Message
                                               IF_MSG1  = IS_HEADER-PRICE_DATE )
          CHANGING
            CT_RESPONSE     = CS_RESPONSE-RESPONSE                     " Table Type for Response Message Strcuture
        ).
      ENDIF.

    ENDIF.

    VALIDATE_DOC_TYPE(
      EXPORTING
        IF_AUART    = IS_HEADER-DOC_TYPE                " Sales Document Type
      IMPORTING
        EF_AUART    = ES_BAPI_SDHEAD-DOC_TYPE           " Sales Document Type
      CHANGING
        CT_RESPONSE = CS_RESPONSE-RESPONSE                 " Table Type for Response Message Structure
    ).

    VALIDATE_DIST_CHANNEL(
      EXPORTING
        IF_VTWEG    = IS_HEADER-DISTR_CHAN                 " Distribution Channel
      IMPORTING
        EF_VTWEG    = ES_BAPI_SDHEAD-DISTR_CHAN                 " Distribution Channel
      CHANGING
        CT_RESPONSE = CS_RESPONSE-RESPONSE                   " Table Type for Response Message Structure
    ).

    VALIDATE_SALE_GROUP(
      EXPORTING
        IF_VKGRP    = IS_HEADER-SALES_GRP " Sales Document Type
      IMPORTING
        EF_VKGRP    = ES_BAPI_SDHEAD-SALES_GRP
      CHANGING
        CT_RESPONSE = CS_RESPONSE-RESPONSE  " Table Type for Response Message Structure
    ).

    VALIDATE_CUSTOMER(
      EXPORTING
        IF_KUNNR       = IS_HEADER-PARTN_NUMB
        IF_VKORG       = ES_BAPI_SDHEAD-SALES_ORG
        IF_VTWEG       = ES_BAPI_SDHEAD-DISTR_CHAN
        IF_SPART       = ES_BAPI_SDHEAD-DIVISION
      IMPORTING
        ET_BAPI_PARTNR = ET_BAPI_PARTNR
        EF_KVGR1       = ES_BAPI_SDHEAD-CUST_GRP1
        EF_KVGR2       = ES_BAPI_SDHEAD-CUST_GRP2
      CHANGING
        CT_RESPONSE    = CS_RESPONSE-RESPONSE                  " Table Type for Response Message Structure
    ).

    VALIDATE_PAYMENT_TERM(
      EXPORTING
        IF_ZTERM    = IS_HEADER-PMNTTRMS                 " Terms of Payment Key
      IMPORTING
        EF_ZTERM    = ES_BAPI_SDHEAD-PMNTTRMS             " Terms of Payment Key
      CHANGING
        CT_RESPONSE = CS_RESPONSE-RESPONSE                 " Table Type for Response Message Structure
    ).

    IF LINE_EXISTS( CS_RESPONSE-RESPONSE[ RESP_STATUS = 'E' ] ).
      CS_RESPONSE-RESP_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = CONCAT_MESSAGE( CS_RESPONSE-RESPONSE ).
    ENDIF.


  ENDMETHOD.


  METHOD VALIDATE_ITEM.

    DATA:
      LF_RC         TYPE SY-SUBRC,
      LF_ITEMNO     TYPE POSNR_VA,
      LF_ITEMNO_TX  TYPE STRING,
      LF_TARGET_QTY TYPE KWMENG,
      LF_UNIT_IN    TYPE MEINS,
      LF_MATNR      TYPE MATNR,
      LF_MSG_TX     TYPE ZSDSDE_REST_MESSAGE,
      LT_RESPONSE   TYPE ZSDSCAS006_TT,
      LT_T006_KEY   TYPE TT_T006,
      LT_MVKE_KEY   TYPE TT_MVKE.

    CLEAR: ET_BAPI_ITEMIN,
           ET_BAPI_SCHDL.

    DATA(LT_MARA_TMP) = CORRESPONDING TT_MVKE( CS_RESPONSE-QUOTE_ITEM DISCARDING DUPLICATES
                                               MAPPING MATNR = MATERIAL ).

    DATA(LT_T006_TMP) = CORRESPONDING TT_T006( CS_RESPONSE-QUOTE_ITEM DISCARDING DUPLICATES
                                               MAPPING MSEHI = TARGET_QU ).

    LOOP AT LT_MARA_TMP ASSIGNING FIELD-SYMBOL(<L_MARA>).

* Convert to internal format

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = <L_MARA>-MATNR
        IMPORTING
          OUTPUT       = LF_MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.

      INSERT VALUE TS_MVKE( MATNR = LF_MATNR
                            VKORG = IF_VKORG
                            VTWEG = IF_VTWEG )
        INTO TABLE LT_MVKE_KEY.

    ENDLOOP.

    LOOP AT LT_T006_TMP ASSIGNING FIELD-SYMBOL(<L_T006>).

      CONVERT_UNIT_IN(
        EXPORTING
          IF_UNIT    = <L_T006>-MSEHI                 " Base Unit of Measure
        IMPORTING
          EF_UNIT_IN = LF_UNIT_IN               " Base Unit of Measure
          EF_RC      = LF_RC                 " ABAP System Field: Return Code of ABAP Statements
      ).

      IF LF_RC <> 0.
        CONTINUE.
      ENDIF.

      INSERT VALUE TS_T006( MSEHI = LF_UNIT_IN )
        INTO TABLE LT_T006_KEY.

    ENDLOOP.

    GET_SALE_FOR_MATERIAL( EXPORTING IT_MVKE = LT_MVKE_KEY
                           IMPORTING ET_MVKE = DATA(LT_MVKE) ).

    GET_MD_UNIT( EXPORTING IT_KEY  = LT_T006_KEY
                 IMPORTING ET_T006 = DATA(LT_T006) ).

    LOOP AT CS_RESPONSE-QUOTE_ITEM ASSIGNING FIELD-SYMBOL(<L_QUOTE_ITEM>).

      CLEAR: LF_MATNR,
             LF_TARGET_QTY,
             LT_RESPONSE.

      IF <L_QUOTE_ITEM>-ITM_NUMBER IS NOT INITIAL.
        LF_ITEMNO   = <L_QUOTE_ITEM>-ITM_NUMBER.
      ELSE.
        LF_ITEMNO   = SY-TABIX.
      ENDIF.

      LF_ITEMNO_TX = |ItemNumber { LF_ITEMNO } :|.

      IF <L_QUOTE_ITEM>-MATERIAL IS INITIAL.
* Text-m06: Material is required.
        COLLECT_RESPONSE(
          EXPORTING
            IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR  " REST Response Status
            IF_REST_MESSAGE = TEXT-M06                               " Response Message
          CHANGING
            CT_RESPONSE     = LT_RESPONSE                          " Table Type for Response Message Strcuture
        ).
      ELSE.

* Convert to internal format
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT        = <L_QUOTE_ITEM>-MATERIAL
          IMPORTING
            OUTPUT       = LF_MATNR
          EXCEPTIONS
            LENGTH_ERROR = 1
            OTHERS       = 2.
        IF SY-SUBRC <> 0.

          MESSAGE ID SY-MSGID TYPE 'I'
          NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                  INTO LF_MSG_TX.

          COLLECT_RESPONSE(
            EXPORTING
              IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR  " REST Response Status
              IF_REST_MESSAGE = LF_MSG_TX
            CHANGING
              CT_RESPONSE     = LT_RESPONSE                " Table Type for Response Message Strcuture
          ).

* Implement suitable error handling here
        ENDIF.

        READ TABLE LT_MVKE
        TRANSPORTING NO FIELDS
        WITH KEY MATNR = LF_MATNR
                 VKORG = IF_VKORG.
        IF SY-SUBRC <> 0.
* Text-e03: Material: &1 does not exist.
          COLLECT_RESPONSE(
            EXPORTING
              IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR  " REST Response Status
              IF_REST_MESSAGE = REPLACE_MESSAGE( IF_MSGTX = TEXT-E03    " Response Message
                                                 IF_MSG1  = LF_MATNR )
            CHANGING
              CT_RESPONSE     = LT_RESPONSE                " Table Type for Response Message Strcuture
          ).
        ENDIF.

      ENDIF.

      IF <L_QUOTE_ITEM>-ITM_NUMBER IS INITIAL.
* Text-m07: Item Number is required.
        COLLECT_RESPONSE(
          EXPORTING
            IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR  " REST Response Status
            IF_REST_MESSAGE = TEXT-M07                               " Response Message
          CHANGING
            CT_RESPONSE     = LT_RESPONSE                           " Table Type for Response Message Strcuture
        ).
      ENDIF.

      IF <L_QUOTE_ITEM>-TARGET_QU IS INITIAL.
* Text-m08: Sale unit is required.
        COLLECT_RESPONSE(
          EXPORTING
            IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR  " REST Response Status
            IF_REST_MESSAGE = TEXT-M08                               " Response Message
          CHANGING
            CT_RESPONSE     = LT_RESPONSE                         " Table Type for Response Message Strcuture
        ).
      ELSE.

        CLEAR LF_MSG_TX.

        CONVERT_UNIT_IN(
          EXPORTING
            IF_UNIT    = <L_QUOTE_ITEM>-TARGET_QU                 " Base Unit of Measure
          IMPORTING
            EF_UNIT_IN = LF_UNIT_IN                 " Base Unit of Measure
            EF_RC      = LF_RC               " ABAP System Field: Return Code of ABAP Statements
            EF_MSG_TX  = LF_MSG_TX                 " Response Message
        ).

        IF LF_RC = 0.
          READ TABLE LT_T006
          TRANSPORTING NO FIELDS
          WITH KEY MSEHI = LF_UNIT_IN .
          IF SY-SUBRC <> 0.
            LF_RC = SY-SUBRC.
* Text-e04: Sale unit: &1 does not exist.
            LF_MSG_TX = REPLACE_MESSAGE( IF_MSGTX = TEXT-E04    " Response Message
                                         IF_MSG1  = <L_QUOTE_ITEM>-TARGET_QU ).

          ENDIF.
        ENDIF.

        IF LF_RC <> 0.
          COLLECT_RESPONSE(
            EXPORTING
              IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR   " REST Response Status
              IF_REST_MESSAGE = LF_MSG_TX                                " Response Message
            CHANGING
              CT_RESPONSE     = LT_RESPONSE                " Table Type for Response Message Strcuture
          ).
        ENDIF.
      ENDIF.


      IF LT_RESPONSE IS NOT INITIAL.
        <L_QUOTE_ITEM>-RESP_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
        <L_QUOTE_ITEM>-RESP_MESSAGE = CONCAT_MESSAGE( IT_RESPONSE = LT_RESPONSE ).

        LOOP AT LT_RESPONSE INTO DATA(LS_RESPONSE).
          LS_RESPONSE-RESP_MESSAGE = |{ LF_ITEMNO_TX } { LS_RESPONSE-RESP_MESSAGE }|.
          APPEND LS_RESPONSE TO CS_RESPONSE-RESPONSE.
        ENDLOOP.

        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO ET_BAPI_ITEMIN ASSIGNING FIELD-SYMBOL(<L_ITEM>).
      <L_ITEM>-ITM_NUMBER   = <L_QUOTE_ITEM>-ITM_NUMBER.
      <L_ITEM>-MATERIAL     = LF_MATNR.
      <L_ITEM>-TARGET_QU    = LF_UNIT_IN.
      LF_TARGET_QTY         = COND #( WHEN <L_QUOTE_ITEM>-TARGET_QTY IS NOT INITIAL
                                      THEN <L_QUOTE_ITEM>-TARGET_QTY
                                      ELSE 1 ).

      <L_ITEM>-TARGET_QTY   = LF_TARGET_QTY.

      APPEND INITIAL LINE TO ET_BAPI_SCHDL ASSIGNING FIELD-SYMBOL(<L_SCHEDULE>).
      <L_SCHEDULE>-ITM_NUMBER = <L_QUOTE_ITEM>-ITM_NUMBER.
      <L_SCHEDULE>-REQ_QTY    = <L_ITEM>-TARGET_QTY.
      <L_SCHEDULE>-SCHED_LINE = GC_DEFAULT-ETENR.

    ENDLOOP.

  ENDMETHOD.


  METHOD VALIDATE_PAYMENT_TERM.

    DATA: LF_MSGTX TYPE ZSDSDE_REST_MESSAGE.

    CLEAR EF_ZTERM.

    IF IF_ZTERM IS INITIAL.
*   Text-m09: Payment Term is required.
      LF_MSGTX = TEXT-M09.
    ELSE.
      SELECT ZTERM
      INTO EF_ZTERM  UP TO 1 ROWS
      FROM T052
      WHERE ZTERM = IF_ZTERM
      ORDER BY PRIMARY KEY.
      ENDSELECT.

      IF SY-SUBRC <> 0.
*    Text-e01: Payment Term: &1 does not exist.
        LF_MSGTX = REPLACE_MESSAGE( IF_MSGTX = TEXT-E01
                                    IF_MSG1  = IF_ZTERM ).
      ENDIF.

    ENDIF.

    COLLECT_RESPONSE(
      EXPORTING
        IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_REST_MESSAGE = LF_MSGTX
      CHANGING
        CT_RESPONSE     = CT_RESPONSE ).

  ENDMETHOD.


  METHOD VALIDATE_REQUEST.

    CLEAR: EF_INVALID,
           ES_BAPI_SDHEAD,
           ET_BAPI_ITEMIN,
           ET_BAPI_PARTNR,
           ET_BAPI_SCHDL.

*  Copy request to response
    CS_RESPONSE = CORRESPONDING #( IS_REQUEST EXCEPT RESPONSE ).

    VALIDATE_HEADER(
      EXPORTING
        IS_HEADER      = IS_REQUEST            " JSON Structure Simulation Price From Salesforce
      IMPORTING
        ES_BAPI_SDHEAD = ES_BAPI_SDHEAD       " Communication Fields: Sales and Distribution Document Header
        ET_BAPI_PARTNR = ET_BAPI_PARTNR       " Communications Fields: SD Document Partner: WWW
      CHANGING
        CS_RESPONSE    = CS_RESPONSE          " JSON Structure Simulation Price From Salesforce
    ).
*  Raise error in header level
    IF LINE_EXISTS( CS_RESPONSE-RESPONSE[ RESP_STATUS = 'E' ] ).
      EF_INVALID = ABAP_TRUE.
      RETURN.
    ENDIF.

    VALIDATE_ITEM(
      EXPORTING
        IF_VKORG       = ES_BAPI_SDHEAD-SALES_ORG         " Sales Organization
        IF_VTWEG       = ES_BAPI_SDHEAD-DISTR_CHAN        " Distribution Channel
      IMPORTING
        ET_BAPI_ITEMIN = ET_BAPI_ITEMIN
        ET_BAPI_SCHDL  = ET_BAPI_SCHDL
      CHANGING
        CS_RESPONSE    = CS_RESPONSE
    ).

    IF LINE_EXISTS( CS_RESPONSE-QUOTE_ITEM[ RESP_STATUS = 'E' ] ).
*     Text-e99: An error occurred processing at QuoteItem level.
      EF_INVALID = ABAP_TRUE.
      CS_RESPONSE-RESP_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
      CS_RESPONSE-RESP_MESSAGE = TEXT-E99.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD VALIDATE_SALE_GROUP.
    DATA: LF_MSGTX TYPE ZSDSDE_REST_MESSAGE.

    CLEAR EF_VKGRP.

    IF IF_VKGRP IS INITIAL.
*  Text-m04: Sales Group is required.
      LF_MSGTX = TEXT-M04.
    ELSE.
      SELECT VKGRP
        INTO EF_VKGRP UP TO 1 ROWS
        FROM TVKGR
        WHERE VKGRP = IF_VKGRP
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      IF SY-SUBRC <> 0.
* Text-e02: Sales Group: &1 does not exist.
        LF_MSGTX = REPLACE_MESSAGE( IF_MSGTX = TEXT-E02
                                    IF_MSG1  = IF_VKGRP ).
      ENDIF.
    ENDIF.

    COLLECT_RESPONSE(
      EXPORTING
        IF_REST_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR    " REST Response Status
        IF_REST_MESSAGE = LF_MSGTX
      CHANGING
        CT_RESPONSE     = CT_RESPONSE                            " Table Type for Response Message Strcuture
    ).

  ENDMETHOD.
ENDCLASS.
