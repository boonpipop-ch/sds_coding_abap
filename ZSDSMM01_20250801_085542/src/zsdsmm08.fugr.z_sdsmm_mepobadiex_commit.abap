FUNCTION Z_SDSMM_MEPOBADIEX_COMMIT.
*"--------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  TABLES
*"      IMT_DATA_NEW STRUCTURE  MEPO_BADI_EXAMPL
*"      IMT_DATA_OLD STRUCTURE  MEPO_BADI_EXAMPL
*"--------------------------------------------------------------------

  DATA: ls_data_new      LIKE LINE OF gt_data,
        ls_data_old      LIKE LINE OF gt_data,
        data_ins         TYPE STANDARD TABLE OF mepo_badi_exampl,
        data_upd         TYPE STANDARD TABLE OF mepo_badi_exampl,
        data_del         TYPE STANDARD TABLE OF mepo_badi_exampl.

* new state
  LOOP AT imt_data_new INTO ls_data_new.
    READ TABLE imt_data_old INTO ls_data_old WITH KEY
                                           mandt = sy-mandt
                                           ebeln = ls_data_new-ebeln
                                           ebelp = ls_data_new-ebelp.
    IF sy-subrc IS INITIAL.
      DELETE imt_data_old INDEX sy-tabix.
      IF ls_data_new NE ls_data_old.
* existing entry was changed
        APPEND ls_data_new TO data_upd.
      ENDIF.
    ELSE.
* a new entry was added
      APPEND ls_data_new TO data_ins.
    ENDIF.
  ENDLOOP.

* remaining old state: can be deleted
  APPEND LINES OF imt_data_old TO data_del.

*---------------------------------------------------------------------*
* actual update operations
*---------------------------------------------------------------------*

* insert
  IF NOT data_ins[] IS INITIAL.
    INSERT mepo_badi_exampl FROM TABLE data_ins.
    IF sy-subrc NE 0.
      MESSAGE a807(me) WITH 'MEPO_BADI_EXAMPL'.
    ENDIF.
  ENDIF.
* update
  IF NOT data_upd[] IS INITIAL.
    UPDATE mepo_badi_exampl FROM TABLE data_upd.
    IF sy-subrc NE 0.
      MESSAGE a808(me) WITH 'MEPO_BADI_EXAMPL'.
    ENDIF.
  ENDIF.
* delete
  IF NOT data_del[] IS INITIAL.
    DELETE mepo_badi_exampl FROM TABLE data_del.
    IF sy-subrc NE 0.
      MESSAGE a809(me) WITH 'MEPO_BADI_EXAMPL'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
