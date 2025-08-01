FUNCTION z_sdsco_budget_creation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_ORDERMASTER) TYPE  ZSDSCOS006 OPTIONAL
*"     VALUE(IM_AUFNR) TYPE  AUFK-AUFNR OPTIONAL
*"  EXPORTING
*"     VALUE(EX_AUFNR) TYPE  AUFNR
*"  TABLES
*"      ET_PLANNING TYPE  ZSDSCOS004_TT OPTIONAL
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
*   ZCOIOI001 Inbound IO Master, Planning and Budgeting
*"----------------------------------------------------------------------
  DATA: lf_aufnr TYPE aufk-aufnr,
        lf_subrc TYPE sysubrc.

  CALL FUNCTION 'KAUF_REFRESH_ORIND'.

  lf_aufnr = im_aufnr.
  ex_aufnr = im_aufnr.

  PERFORM f_budget_update TABLES et_planning
                                         et_return
                                         USING  lf_aufnr is_ordermaster-k2_year
                                               CHANGING lf_subrc.
  IF lf_subrc EQ 0.
*
    READ TABLE et_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      PERFORM f_io_rel TABLES et_return USING lf_aufnr.
    ENDIF.
  ENDIF.

ENDFUNCTION.
