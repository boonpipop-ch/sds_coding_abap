FUNCTION Z_SDSFI_CUSTOMER_JSON.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(IR_KUNNR) TYPE  J_3RS_SO_KUNNR OPTIONAL
*"     VALUE(IR_ERDAT) TYPE  /ACCGO/CAS_TT_BUDAT_RANGE OPTIONAL
*"     VALUE(I_ETAX) TYPE  FLAG OPTIONAL
*"     VALUE(I_EMAIL) TYPE  FLAG OPTIONAL
*"     VALUE(I_AL11) TYPE  CHAR255 OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  STRING_TABLE
*"     VALUE(E_MESSAGE) TYPE  CHAR255
*"     VALUE(E_MESTYPE) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA : BEGIN OF LS_KNA1,
           STCD3 TYPE KNA1-STCD3,
           KUNNR TYPE KNA1-KUNNR,
           NAME1 TYPE KNA1-NAME1,
           NAME2 TYPE KNA1-NAME2,
           NAME3 TYPE KNA1-NAME3,
           NAME4 TYPE KNA1-NAME4,
           TELF2 TYPE KNA1-TELF2,
           ADRNR TYPE KNA1-ADRNR,
           NODEL TYPE KNA1-NODEL,
           SPERR TYPE KNB1-SPERR,
           LOEVM TYPE KNB1-LOEVM,
           ERDAT TYPE KNA1-ERDAT,
         END OF LS_KNA1.
  DATA LT_KNA1 LIKE TABLE OF LS_KNA1.

  DATA LS_TMP LIKE LS_KNA1.

  DATA : BEGIN OF LS_ADR6,
           ADDRNUMBER TYPE ADR6-ADDRNUMBER,
           SMTP_ADDR  TYPE ADR6-SMTP_ADDR,
         END OF LS_ADR6.
  DATA LT_ADR6 LIKE TABLE OF LS_ADR6.

  DATA : BEGIN OF LS_ZTEXTAX_REGIS,
           KUNNR     TYPE ZSDSFIC002-KUNNR,
           SMTP_ADDR TYPE ZSDSFIC002-SMTP_ADDR,
         END OF LS_ZTEXTAX_REGIS.
  DATA LT_ZTEXTAX_REGIS LIKE TABLE OF LS_ZTEXTAX_REGIS.

  DATA LV_TEXT TYPE C LENGTH 255.

  DATA : LV_RUN_I TYPE I,
         LV_RUN_C TYPE C LENGTH 255.

  DATA : LV_TABIX TYPE SY-TABIX.

  DATA : LV_TIME TYPE SY-UZEIT.

  DATA : LS_RETURN LIKE LINE OF ET_RETURN.

  CONSTANTS : BEGIN OF LC_CON,
                TAX0 TYPE C LENGTH 13 VALUE '0000000000000',
              END OF LC_CON.

*  IF SY-BATCH EQ 'X'.
*    CONCATENATE 'Master_' SY-DATUM SY-UZEIT '.json' INTO P_NAME.
*  ENDIF.

  IF I_ETAX EQ ABAP_TRUE.
    SELECT *
       FROM ZSDSFIC001
       INNER JOIN KNA1 ON ZSDSFIC001~KUNNR EQ KNA1~KUNNR
       INTO CORRESPONDING FIELDS OF TABLE GT_ZTE_CUST_EMAIL
       WHERE ZSDSFIC001~KUNNR IN ir_KUNNR[]
         AND ( ZSDSFIC001~CREATED_DATE IN ir_ERDAT[] OR
               ZSDSFIC001~UPDATED_DATE IN ir_ERDAT[] ).
*         AND KNA1~BRSCH IN S_BRSCH[].
    IF GT_ZTE_CUST_EMAIL[] IS NOT INITIAL.
      SELECT KNA1~STCD3
             KNA1~KUNNR
             KNA1~NAME1
             KNA1~NAME2
             KNA1~NAME3
             KNA1~NAME4
             KNA1~TELF2
             KNA1~ADRNR
             KNA1~NODEL
             KNB1~SPERR
             KNB1~LOEVM
             KNA1~ERDAT
        FROM KNA1
        INNER JOIN KNB1 ON KNA1~KUNNR EQ KNB1~KUNNR AND
                           KNB1~BUKRS EQ I_BUKRS
        INTO TABLE LT_KNA1
        FOR ALL ENTRIES IN GT_ZTE_CUST_EMAIL
        WHERE KNA1~KUNNR EQ GT_ZTE_CUST_EMAIL-KUNNR.
    ENDIF.
  ELSE.
    SELECT KNA1~STCD3
           KNA1~KUNNR
           KNA1~NAME1
           KNA1~NAME2
           KNA1~NAME3
           KNA1~NAME4
           KNA1~TELF2
           KNA1~ADRNR
           KNA1~NODEL
           KNB1~SPERR
           KNB1~LOEVM
           KNA1~ERDAT
      FROM KNA1
      INNER JOIN KNB1 ON KNA1~KUNNR EQ KNB1~KUNNR AND
                         KNB1~BUKRS EQ i_BUKRS
      INTO TABLE LT_KNA1
      WHERE KNA1~KUNNR IN ir_KUNNR[]
        AND KNA1~ERDAT IN ir_ERDAT[].
  ENDIF.

  IF LT_KNA1[] IS NOT INITIAL.


**    SELECT addrnumber
**           smtp_addr
**      FROM adr6
**      INTO TABLE lt_adr6
**      FOR ALL ENTRIES IN lt_kna1
**      WHERE addrnumber EQ lt_kna1-adrnr.

    SELECT KUNNR
           SMTP_ADDR
      FROM ZSDSFIC002
      INTO TABLE LT_ZTEXTAX_REGIS
      FOR ALL ENTRIES IN LT_KNA1
      WHERE KUNNR EQ LT_KNA1-KUNNR.

***    PERFORM f_insert_table_zetx002 TABLES lt_kna1[]
***                                          lt_ztextax_regis[].

*    IF p_5min EQ 'X'.
*      lv_time = sy-uzeit - 300.
*
*      SELECT *
*       FROM zte_cust_email
*       INTO TABLE gt_zte_cust_email
*       FOR ALL ENTRIES IN lt_kna1
*       WHERE kunnr EQ lt_kna1-kunnr
*         AND ( created_date EQ sy-datum OR
*               updated_date EQ sy-datum )
*         AND ( created_time BETWEEN lv_time AND sy-uzeit OR
*               updated_time BETWEEN lv_time AND sy-uzeit ).
    IF I_EMAIL EQ ABAP_TRUE.
      SELECT *
       FROM ZSDSFIC001
       INTO TABLE GT_ZTE_CUST_EMAIL
       FOR ALL ENTRIES IN LT_KNA1
       WHERE KUNNR EQ LT_KNA1-KUNNR
         AND ( CREATED_DATE IN ir_ERDAT[] OR
               UPDATED_DATE IN ir_ERDAT[] ).
    ENDIF.

    SORT LT_KNA1 BY KUNNR.

    DELETE LT_KNA1 WHERE STCD3 EQ LC_CON-TAX0 OR
                         STCD3 EQ SPACE.

    IF I_EMAIL EQ ABAP_TRUE.
      LOOP AT LT_KNA1 INTO LS_TMP.
        LV_TABIX = SY-TABIX.
        READ TABLE GT_ZTE_CUST_EMAIL INTO GS_ZTE_CUST_EMAIL
        WITH KEY KUNNR = LS_TMP-KUNNR.
        IF SY-SUBRC EQ 0.
          DELETE LT_KNA1 INDEX LV_TABIX.
        ENDIF.
        CLEAR : LS_TMP,GS_ZTE_CUST_EMAIL.
      ENDLOOP.
    ELSE.

    ENDIF.

    LOOP AT LT_KNA1 INTO LS_TMP.
      MOVE-CORRESPONDING LS_TMP TO LS_KNA1.
      ADD 1 TO LV_RUN_I.
      LV_RUN_C = LV_RUN_I.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_RUN_C WITH SPACE.
      AT FIRST.
        CLEAR : LS_RETURN.
        LS_RETURN = '[{'.
        APPEND LS_RETURN TO ET_RETURN.
      ENDAT.

      IF LS_RETURN NE '[{'.
        CLEAR : LS_RETURN.
        CONCATENATE '  ' '},{' INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
        APPEND LS_RETURN TO ET_RETURN.
      ENDIF.

      CLEAR : LS_RETURN.
      CONCATENATE '"' LS_KNA1-STCD3 '"' INTO LV_TEXT.
      CONCATENATE '  ' '"buyerTaxId" :' LV_TEXT  INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
      CONCATENATE LS_RETURN ',' INTO LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.

      CLEAR : LS_RETURN.
      CONCATENATE LS_KNA1-NAME1
                  LS_KNA1-NAME2
                  LS_KNA1-NAME3
                  LS_KNA1-NAME4 INTO LV_TEXT SEPARATED BY SPACE.

      CONCATENATE '"' LV_TEXT '"' INTO LV_TEXT.

      CONCATENATE '  ' '"buyerName" :' LV_TEXT
                                         INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
      CONCATENATE LS_RETURN ',' INTO LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.

*      LOOP AT lt_adr6 INTO ls_adr6 WHERE addrnumber = ls_kna1-adrnr.
*        CLEAR : ls_return.
*        CONCATENATE '"' ls_adr6-smtp_addr '"' INTO lv_text.
*
*        CONCATENATE '  ' '"buyerEmail" :' lv_text  INTO ls_return-text SEPARATED BY space RESPECTING BLANKS.
*        CONCATENATE ls_return-text ',' INTO ls_return-text.
*        APPEND ls_return TO ET_RETURN.
*        CLEAR : ls_adr6.
*      ENDLOOP.
*      IF sy-subrc NE 0.
*        CONCATENATE '  ' '"buyerEmail" :' '""'  INTO ls_return-text SEPARATED BY space RESPECTING BLANKS.
*        CONCATENATE ls_return-text ',' INTO ls_return-text.
*        APPEND ls_return TO ET_RETURN.
*      ENDIF.

      READ TABLE GT_ZTE_CUST_EMAIL INTO GS_ZTE_CUST_EMAIL
      WITH KEY KUNNR = LS_KNA1-KUNNR.
      IF GS_ZTE_CUST_EMAIL-E_MAIL IS NOT INITIAL.
        CONCATENATE '"' GS_ZTE_CUST_EMAIL-E_MAIL '"' INTO LV_TEXT.
        CONCATENATE '  ' '"buyerEmail" :' LV_TEXT      INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
        CONCATENATE LS_RETURN ','                 INTO LS_RETURN.
        APPEND LS_RETURN TO ET_RETURN.
        CLEAR : GS_ZTE_CUST_EMAIL.
      ELSE.
        READ TABLE LT_ZTEXTAX_REGIS INTO LS_ZTEXTAX_REGIS
        WITH KEY KUNNR = LS_KNA1-KUNNR.
        IF LS_ZTEXTAX_REGIS-SMTP_ADDR  IS NOT INITIAL.
          CONCATENATE '"' LS_ZTEXTAX_REGIS-SMTP_ADDR '"' INTO LV_TEXT.
          CONCATENATE '  ' '"buyerEmail" :' LV_TEXT      INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
          CONCATENATE LS_RETURN ','                 INTO LS_RETURN.
          APPEND LS_RETURN TO ET_RETURN.
          CLEAR : LS_ZTEXTAX_REGIS.
        ELSE.
*          CONCATENATE '"' 'ITAPP_' lv_run_c '@daikin.co.th' '"' INTO lv_text.
*          CONCATENATE '"' 'ITAPP@daikin.co.th' '"' INTO lv_text.
          CONCATENATE '"' '' '"' INTO LV_TEXT.
          CONCATENATE '  ' '"buyerEmail" :' LV_TEXT      INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
          CONCATENATE LS_RETURN ','                 INTO LS_RETURN.
          APPEND LS_RETURN TO ET_RETURN.
        ENDIF.
      ENDIF.

      CLEAR : LS_RETURN.
      REPLACE ALL OCCURRENCES OF PCRE '-' IN LS_KNA1-TELF2 WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN LS_KNA1-TELF2 WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN LS_KNA1-TELF2 WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_KNA1-TELF2 WITH ''.
*      CONCATENATE '"' ls_kna1-telf2 '"' INTO lv_text.
      CONCATENATE '"' '"' INTO LV_TEXT.
      CONCATENATE '  ' '"buyerPhoneNumber" :' LV_TEXT INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
      CONCATENATE LS_RETURN ',' INTO LS_RETURN.
      APPEND LS_RETURN TO ET_RETURN.

*      IF ls_kna1-nodel EQ 'X' OR
*         ls_kna1-sperr EQ 'X' OR
*         ls_kna1-loevm EQ 'X'.
*        CLEAR : ls_return.
*        CONCATENATE '  ' '"activeFlag" :' 'false' INTO ls_return-text SEPARATED BY space RESPECTING BLANKS.
*        APPEND ls_return TO ET_RETURN.
*
*      ELSE.
      CLEAR : LS_RETURN.
      CONCATENATE '  ' '"activeFlag" :' 'true' INTO LS_RETURN SEPARATED BY SPACE RESPECTING BLANKS.
      APPEND LS_RETURN TO ET_RETURN.
*      ENDIF.

      AT LAST.
        CLEAR : LS_RETURN.
        LS_RETURN = '}]'.
        APPEND LS_RETURN TO ET_RETURN.
        CLEAR : LS_KNA1,LS_ADR6.
        CONTINUE.
      ENDAT.

*      CLEAR : ls_return.
*      CONCATENATE '  ' '},{' INTO ls_return-text SEPARATED BY space RESPECTING BLANKS.
*      APPEND ls_return TO ET_RETURN.

      CLEAR : LS_KNA1,LS_ADR6.
    ENDLOOP.
  ENDIF.

*    IF ET_RETURN IS NOT INITIAL.

  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.

  DATA : LV_FILE_NAME TYPE STRING.

  DATA: LV_PATH(100) TYPE C.

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

  CONCATENATE 'Master_' SY-DATUM SY-UZEIT '.json' INTO LV_FILE_NAME.

  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_AL11_PATH   = I_AL11"'/tmp'
                                     I_FILE_NAME   = LV_FILE_NAME
*                                       I_USER        = 'ds'
*                                       I_PASS        = 'ds=20240521'
*                                       I_IP          = '172.31.136.249'
*                                       I_PORT        = '21'
                                     I_DATA_SPIDER = ABAP_TRUE
                                     IT_DATA       = ET_RETURN ).
  IF LV_STATUS EQ 'S'.
    E_MESTYPE = 'S'.
  ELSE.
    E_MESTYPE = 'E'.
  ENDIF.
*    ENDIF.



ENDFUNCTION.
