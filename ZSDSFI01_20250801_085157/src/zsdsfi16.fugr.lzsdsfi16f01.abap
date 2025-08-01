*&---------------------------------------------------------------------*
*& Include          LZSDSFI16F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_RF05R
*&      <-- GT_COLL_LOG
*&---------------------------------------------------------------------*
FORM f_get_coll_log  USING    uf_rf05r    TYPE rf05r
                     CHANGING ct_coll_log TYPE tt_coll_log
                              ct_bank     TYPE tt_bank_statement.

  SELECT coll_log~*
    INTO TABLE @ct_coll_log
    FROM zsdsfit037 AS clr
    INNER JOIN zsdsfit029 AS coll_log
    ON  coll_log~data_type = clr~data_type
    AND coll_log~bukrs     = clr~bukrs
    AND coll_log~belnr     = clr~belnr
    AND coll_log~gjahr     = clr~gjahr
    AND coll_log~buzei     = clr~buzei
    AND coll_log~seq       = clr~seq
    WHERE clr~bukrs_clr = @uf_rf05r-bukrs
    AND   clr~belnr_clr = @uf_rf05r-augbl
    AND   clr~gjahr_clr = @uf_rf05r-gjahr
    AND   coll_log~zbank_item <> ''   ##TOO_MANY_ITAB_FIELDS.

  SELECT bnk~*
    FROM zsdsfit042 AS bnk
    INNER JOIN @ct_coll_log AS coll_log
    ON  bnk~hbkid      = coll_log~hbkid
    AND bnk~zbank_item = coll_log~zbank_item
    WHERE bnk~trnfer_number <> ''
    INTO TABLE @ct_bank               ##TOO_MANY_ITAB_FIELDS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_reverse_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_COLL_LOG
*&---------------------------------------------------------------------*
FORM f_reverse_coll_log  USING  ut_coll_log TYPE tt_coll_log.

  LOOP AT ut_coll_log ASSIGNING FIELD-SYMBOL(<ls_coll_log>).
    <ls_coll_log>-delete_flag = gc_true.
    <ls_coll_log>-delete_date = sy-datum.
    <ls_coll_log>-update_by   = sy-uname.
    <ls_coll_log>-update_on   = sy-datum.
    <ls_coll_log>-update_time = sy-uzeit.

    <ls_coll_log>-col_ind-delete_flag = gc_true.
    <ls_coll_log>-col_ind-delete_date = gc_true.
    <ls_coll_log>-col_ind-update_by   = gc_true.
    <ls_coll_log>-col_ind-update_on   = gc_true.
    <ls_coll_log>-col_ind-update_time = gc_true.
  ENDLOOP.

  IF ut_coll_log IS NOT INITIAL.
    UPDATE zsdsfit029
      FROM TABLE @ut_coll_log INDICATORS SET STRUCTURE col_ind.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_bank_statement
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BANK_STATEMENT
*&---------------------------------------------------------------------*
FORM f_update_bank_statement  USING ut_bank_statement TYPE tt_bank_statement.

  LOOP AT ut_bank_statement ASSIGNING FIELD-SYMBOL(<ls_bank>).
    CLEAR: <ls_bank>-trnfer_number,
           <ls_bank>-fyear_trnferno,
           <ls_bank>-map_status,
           <ls_bank>-fi_clearing_no,
           <ls_bank>-fyear_clearing,
           <ls_bank>-clearing_user,
           <ls_bank>-date_clear,
           <ls_bank>-time_clear.

    <ls_bank>-col_ind-trnfer_number   = gc_true.
    <ls_bank>-col_ind-fyear_trnferno  = gc_true.
    <ls_bank>-col_ind-map_status      = gc_true.
    <ls_bank>-col_ind-fi_clearing_no  = gc_true.
    <ls_bank>-col_ind-fyear_clearing  = gc_true.
    <ls_bank>-col_ind-clearing_user   = gc_true.
    <ls_bank>-col_ind-date_clear      = gc_true.
    <ls_bank>-col_ind-time_clear      = gc_true.
  ENDLOOP.

  IF ut_bank_statement IS NOT INITIAL.
    SORT ut_bank_statement BY hbkid zbank_item trnfer_number DESCENDING.
    DELETE ADJACENT DUPLICATES FROM ut_bank_statement COMPARING hbkid	zbank_item.

    UPDATE zsdsfit042
      FROM TABLE @ut_bank_statement INDICATORS SET STRUCTURE col_ind.
  ENDIF.

ENDFORM.
