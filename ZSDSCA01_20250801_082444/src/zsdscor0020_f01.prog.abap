*&---------------------------------------------------------------------*
*& Include          ZSDSCOR0020_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_prepare_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_prepare_data.

  DATA:
    lw_file TYPE gty_file,
    lw_data TYPE gty_data.

  DATA:
    lref_except  TYPE  REF TO cx_root.

* Show Progress
* Text-p02 : Converting data from file into SAP format...
  mc_show_progress 20 TEXT-p02.

* Prepare data into SAP format
  LOOP AT gt_file INTO lw_file.

    CLEAR lw_data.

    PERFORM f_prepare_item CHANGING lw_file
                                    lw_data.

    TRY.

        MOVE-CORRESPONDING lw_file TO lw_data.

      CATCH cx_sy_conversion_no_number
        INTO lref_except.
*       Unable to interpret value as a number.
        PERFORM f_set_message USING 'E'  'ZTEC' '033'
                                    space
                                    space
                                    space
                                    space
                           CHANGING lw_data-status
                                    lw_data-message.
        lw_data-message =       lref_except->get_text( ).
    ENDTRY.

    APPEND lw_data TO gt_data.
  ENDLOOP.

  SORT gt_bukrs BY bukrs.
  SORT gt_kostl BY kostl kokrs.
  SORT gt_prctr BY prctr kokrs.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_prepare_item  CHANGING ps_file TYPE gty_file
                              ps_data TYPE gty_data.

  DATA:
    lw_bukrs TYPE gty_bukrs,
    lw_waers TYPE gty_waers,
    lw_kostl TYPE gty_kostl,
    lw_prctr TYPE gty_prctr.

*-------------------------------
* Convert to SAP Format
*-------------------------------
  IF ps_file-valid_from IS NOT INITIAL.
    PERFORM f_convert_date_internal USING ps_file-valid_from
                                 CHANGING ps_file-valid_from.
  ENDIF.
  IF ps_file-valid_to IS NOT INITIAL.
    PERFORM f_convert_date_internal USING ps_file-valid_to
                                 CHANGING ps_file-valid_to.
  ENDIF.
  IF ps_file-costctr IS NOT INITIAL.
    PERFORM f_conversion_exit USING 'ALPHA' 'INPUT' ps_file-costctr
                           CHANGING ps_file-costctr.
  ENDIF.
  IF ps_file-profitctr IS NOT INITIAL.
    PERFORM f_conversion_exit USING 'ALPHA' 'INPUT' ps_file-profitctr
                           CHANGING ps_file-profitctr.
  ENDIF.

  IF ps_file-ccode IS NOT INITIAL.
    lw_bukrs-bukrs = ps_file-ccode.
    COLLECT lw_bukrs INTO gt_bukrs.
  ENDIF.

  IF ps_file-crncy IS NOT INITIAL.
    lw_waers-waers = ps_file-crncy.
    COLLECT lw_waers INTO gt_waers.
  ENDIF.

  IF ps_file-costctr IS NOT INITIAL.
    lw_kostl-kostl = ps_file-costctr.
    COLLECT lw_kostl INTO gt_kostl.
  ENDIF.
  IF ps_file-profitctr IS NOT INITIAL.
    lw_prctr-prctr = ps_file-profitctr.
    COLLECT lw_prctr INTO gt_prctr.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_data
*----------------------------------------------------------------------*
*  Validate File data
*----------------------------------------------------------------------*
FORM f_validate_data.

  DATA:
    lw_data     TYPE gty_data.

  DATA:
    lv_tabix  TYPE sy-tabix.

* Show Progress
* Text-p03 : Validating File data...
  mc_show_progress 30 TEXT-p03.


*--------------------------------------
* Get master data
*--------------------------------------
  PERFORM f_get_additional.

*--------------------------------------
* Validate data
*--------------------------------------
  LOOP AT gt_data INTO lw_data.

    lv_tabix = sy-tabix.

    PERFORM f_validate_item CHANGING lw_data.

    MODIFY gt_data FROM lw_data INDEX lv_tabix
                         TRANSPORTING status
                                      message.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDITIONAL
*&---------------------------------------------------------------------*
*       Get master data to validate input data
*----------------------------------------------------------------------*
FORM f_get_additional .

* Company Code
  IF gt_bukrs[] IS NOT INITIAL.
    SELECT t001~bukrs
           t001~butxt
           t001~waers
           t001~land1
           t001~ktopl
           tka02~kokrs
           t005~kalsm
    FROM t001
    INNER JOIN tka02 ON tka02~bukrs EQ t001~bukrs
    INNER JOIN t005 ON t005~land1 EQ t001~land1
    INTO TABLE gt_t001
    FOR ALL ENTRIES IN gt_bukrs
    WHERE t001~bukrs EQ gt_bukrs-bukrs.
  ENDIF.

* Currency
  IF gt_waers[] IS NOT INITIAL.
    SELECT *
    FROM tcurc
    INTO TABLE gt_tcurc
    FOR ALL ENTRIES IN gt_waers
    WHERE waers EQ gt_waers-waers.
    SORT gt_tcurc BY waers.
  ENDIF.

* Cost Center
  IF gt_kostl[] IS NOT INITIAL.
    SELECT kostl
           kokrs
           datbi
           datab
    FROM csks
    INTO TABLE gt_csks
    FOR ALL ENTRIES IN gt_kostl
    WHERE kostl EQ gt_kostl-kostl
      AND datbi GE sy-datum
      AND datab LE sy-datum.
    SORT gt_csks BY kostl kokrs.
  ENDIF.

* Profit center
  IF gt_prctr[] IS NOT INITIAL.
    SELECT  prctr
            kokrs
            datbi
    FROM cepc
    INTO TABLE gt_cepc
    FOR ALL ENTRIES IN gt_prctr
    WHERE prctr EQ gt_prctr-prctr
      AND datbi GE sy-datum
      AND datab LE sy-datum.
    SORT gt_cepc BY prctr kokrs.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_validate_item CHANGING ps_data TYPE gty_data.

  DATA:
    lw_t001  TYPE gty_t001    ##NEEDED,
    lw_tcurc TYPE tcurc       ##NEEDED,
    lw_csks  TYPE gty_csks    ##NEEDED,
    lw_cepc  TYPE gty_cepc    ##NEEDED.

*//Company Code
  IF ps_data-ccode IS INITIAL.
    PERFORM f_set_message USING 'E' 'ZFI001' '101'
                                'Company Code'(m02)
                                space
                                space
                                space
                       CHANGING ps_data-status
                                ps_data-message.
    RETURN.
  ELSE.
    READ TABLE gt_t001 WITH KEY bukrs = ps_data-ccode
                                INTO lw_t001
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      PERFORM f_set_message USING 'E' 'F5' '165'
                                  ps_data-ccode
                                  space
                                  space
                                  space
                         CHANGING ps_data-status
                                  ps_data-message.
      RETURN.
    ENDIF.
  ENDIF.

* //Cost center
  IF ps_data-costctr IS INITIAL.
    PERFORM f_set_message USING 'E' 'ZFI001' '101'
                                'Cost Center'(m06)
                                space
                                space
                                space
                       CHANGING ps_data-status
                                ps_data-message.
    RETURN.
  ELSE.
    CLEAR lw_csks.
    READ TABLE gt_csks WITH KEY kokrs = ps_data-coar
                                kostl = ps_data-costctr
                                INTO lw_csks
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      "Cost center already exists from & to &
      PERFORM f_set_message USING 'E' 'KS' '003'
                                  lw_csks-datbi
                                  lw_csks-datab
                                  space
                                  space
                         CHANGING ps_data-status
                                  ps_data-message.
      RETURN.
    ENDIF.
  ENDIF.

*//Person Responsible
  IF ps_data-pers_resp IS INITIAL.
    PERFORM f_set_message USING 'E' 'ZFI001' '101'
                                'Person Responsible'(m07)
                                space
                                space
                                space
                       CHANGING ps_data-status
                                ps_data-message.
    RETURN.
  ENDIF.

*//Cost Center Category
  IF ps_data-costctr_cat IS INITIAL.
    PERFORM f_set_message USING 'E' 'ZFI001' '101'
                                'Cost Center Category'(m08)
                                space
                                space
                                space
                       CHANGING ps_data-status
                                ps_data-message.
    RETURN.
  ENDIF.

*//Hierarchy area
  IF ps_data-hier_area IS INITIAL.
    PERFORM f_set_message USING 'E' 'ZFI001' '101'
                                'Hierarchy area'(m09)
                                space
                                space
                                space
                       CHANGING ps_data-status
                                ps_data-message.
    RETURN.
  ENDIF.

* //Currency
  IF ps_data-crncy IS NOT INITIAL.
    CLEAR lw_tcurc.
    READ TABLE gt_tcurc WITH KEY waers = ps_data-crncy
                                 INTO lw_tcurc
                                 BINARY SEARCH.
    IF sy-subrc NE 0.
      "Currency key & invalid. Enter a valid currency key
      PERFORM f_set_message USING 'E' 'F5A' '100'
                                  ps_data-crncy
                                  space
                                  space
                                  space
                         CHANGING ps_data-status
                                  ps_data-message.
      RETURN.
    ENDIF.
  ENDIF.

* //Profit center
  IF ps_data-profitctr IS NOT INITIAL.
    CLEAR lw_cepc.
    READ TABLE gt_cepc WITH KEY prctr = ps_data-profitctr
                                kokrs = ps_data-coar
                                INTO lw_cepc
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      "Profit center &1/&2 does not exist
      PERFORM f_set_message USING 'E' 'KM' '701'
                                  ps_data-coar
                                  ps_data-profitctr
                                  space
                                  space
                         CHANGING ps_data-status
                                  ps_data-message.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_upload_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_upload_data  USING pv_test TYPE flag.

  DATA:
    lw_data     TYPE gty_data,
    lw_data_tmp TYPE gty_data.

  LOOP AT gt_data INTO lw_data_tmp.

    CHECK lw_data_tmp-status NE icon_red_light.

    lw_data = lw_data_tmp.
    CLEAR: lw_data-status,
           lw_data-message.

    CLEAR: gt_costctr.

*   Fill item data
    PERFORM f_fill_item USING lw_data.

*   Call BAPI to create Cost Center
    PERFORM f_call_bapi_create USING pv_test
                                     lw_data
                            CHANGING lw_data-status
                                     lw_data-message.

    MODIFY gt_data  FROM lw_data
            TRANSPORTING status
                         message.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ITEM
*&---------------------------------------------------------------------*
*       Fill items
*----------------------------------------------------------------------*
FORM f_fill_item USING ps_data TYPE gty_data.

  DATA:
    lw_costctr TYPE bapi0012_ccinputlist.

  lw_costctr-costcenter   = ps_data-costctr.
  lw_costctr-valid_from   = ps_data-valid_from.
  lw_costctr-valid_to     = ps_data-valid_to.
  lw_costctr-name         = ps_data-name.
  lw_costctr-descript     = ps_data-desc.
  lw_costctr-person_in_charge = ps_data-pers_resp.
  lw_costctr-costcenter_type  = ps_data-costctr_cat.
  lw_costctr-costctr_hier_grp = ps_data-hier_area.
  lw_costctr-comp_code    = ps_data-ccode.
  lw_costctr-func_area    = ps_data-func_area.
  lw_costctr-currency     = ps_data-crncy.
  lw_costctr-profit_ctr   = ps_data-profitctr.

  lw_costctr-budget_carrying_cost_ctr = ps_data-budget_cc.
  lw_costctr-avc_profile  = ps_data-budget_prof.
  lw_costctr-avc_active   = ps_data-budget_act.
  APPEND lw_costctr TO gt_costctr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_call_bapi_create
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_call_bapi_create USING pv_test TYPE flag
                              ps_data    TYPE gty_data
                     CHANGING pv_status  TYPE gty_data-status
                              pv_message TYPE gty_data-message.

  DATA:
    lt_return TYPE TABLE OF bapiret2,
    ls_return TYPE bapiret2.

  CALL FUNCTION 'BAPI_COSTCENTER_CREATEMULTIPLE'
    EXPORTING
      controllingarea = ps_data-coar
      testrun         = space
    TABLES
      costcenterlist  = gt_costctr
      return          = lt_return.

  CLEAR ls_return.
  LOOP AT lt_return INTO ls_return
                   WHERE ( type EQ 'E' OR
                           type EQ 'A' ).
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    PERFORM f_set_message USING ls_return-type
                                ls_return-id
                                ls_return-number
                                ls_return-message_v1
                                ls_return-message_v2
                                ls_return-message_v3
                                ls_return-message_v4
                       CHANGING pv_status
                                pv_message.
  ELSE.
    IF pv_test EQ space.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = ls_return.
      PERFORM f_set_message USING 'S' 'KS' '005'
                                  space
                                  space
                                  space
                                  space
                         CHANGING pv_status
                                  pv_message.
    ENDIF.
  ENDIF.

ENDFORM.
