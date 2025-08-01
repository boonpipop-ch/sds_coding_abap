FUNCTION Z_SDSMM_IDOC_INPUT_DESADV1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) TYPE  BDWFAP_PAR-INPUTMETHD OPTIONAL
*"     VALUE(MASS_PROCESSING) TYPE  BDWFAP_PAR-MASS_PROC OPTIONAL
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) TYPE  BDWFAP_PAR-RESULT
*"     VALUE(APPLICATION_VARIABLE) TYPE  BDWFAP_PAR-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) TYPE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) TYPE  BDWFAP_PAR-CALLTRANS
*"  CHANGING
*"     VALUE(IDOC_CONTRL) TYPE  EDIDC OPTIONAL
*"     VALUE(IDOC_DATA) TYPE  EDIDD_TT OPTIONAL
*"     VALUE(IDOC_STATUS) TYPE  T_IDOC_STATUS OPTIONAL
*"     VALUE(RETURN_VARIABLES) TYPE  BDWFRETVAR OPTIONAL
*"     VALUE(SERIALIZATION_INFO) TYPE  BDI_SER OPTIONAL
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------
 DATA: l_e1edt13     TYPE e1edt13,
        l_e1edl20     TYPE e1edl20,
        l_e1edl24     TYPE e1edl24,
        l_e1edl41     TYPE e1edl41,
*        l_e1txth8     TYPE e1txth8,
*        l_e1txtp8_1   TYPE e1txtp8,
*        l_e1txtp8_2   TYPE e1txtp8,
*        l_e1edl26     TYPE e1edl26,
*        l_zzetd       TYPE zekes-zzetd,
*        l_zekes       TYPE zekes,
*        l_flgshipname TYPE c,
*        l_kvgr1       TYPE zekes-kvgr1,
*        l_kvgr2       TYPE zekes-kvgr2,
        l_str         TYPE string,
        l_int         TYPE i,
        l_xblnr       TYPE ekes-xblnr,
        l_eindt       TYPE ekes-eindt,
        l_uzeit       TYPE ekes-uzeit,
        l_paqty       TYPE ekes-menge,
        l_abqty       TYPE ekes-menge,
        l_etens       TYPE ekes-etens,
        ltab_xekes    TYPE TABLE OF uekes WITH HEADER LINE,
        ltab_xekes2   TYPE TABLE OF uekes WITH HEADER LINE,
        ltab_yekes    TYPE TABLE OF uekes WITH HEADER LINE.
  DATA: idoc_data1 TYPE TABLE OF EDIDD WITH HEADER LINE .
  DATA: idoc_status1 TYPE TABLE OF BDIDOCSTAT WITH HEADER LINE .
  CLEAR: ltab_xekes, ltab_yekes.
  REFRESH: ltab_xekes, ltab_yekes.


  idoc_data1[] = idoc_data.
  LOOP AT idoc_data1 WHERE docnum = idoc_contrl-docnum.

    CASE idoc_data1-segnam.
      WHEN 'E1EDL20'.  "header
        l_e1edl20 = idoc_data1-sdata.
        l_xblnr   = l_e1edl20-lifex. "Invoice No

      WHEN 'E1EDT13'.
        l_e1edt13 = idoc_data1-sdata.
*        IF l_e1edt13-qualf = '006'.
*          l_zzetd = l_e1edt13-ntanf.
*        ENDIF.
        IF l_e1edt13-qualf = '007'.
          mv l_e1edt13-ntanf l_eindt.
          mv l_e1edt13-ntanz l_uzeit.
        ENDIF.

*      WHEN 'E1TXTH8'.
*        l_e1txth8 = idoc_data-sdata.
*
*      WHEN 'E1TXTP8'.
*        IF l_e1txth8-tdid = 'X010'.
*          l_e1txtp8_1 = idoc_data-sdata.
*        ENDIF.
*        IF l_e1txth8-tdid = 'X020'.
*          l_e1txtp8_2 = idoc_data-sdata.
*        ENDIF.

      WHEN 'E1EDL24'.
        l_e1edl24 = idoc_data1-sdata.
        IF l_e1edl24-lfimg IS INITIAL.
          idoc_status1-docnum = idoc_contrl-docnum.
          idoc_status1-msgty  = 'E'.
          idoc_status1-msgid  = 'VL'.
          idoc_status1-msgno  = 243.
          idoc_status1-msgv1  = 'LFIMG'.
          idoc_status1-msgv2  = idoc_data1-segnum.
          idoc_status1-status = '51'.
          APPEND idoc_status1 TO idoc_status.
          EXIT.
        ENDIF.
        ltab_xekes-menge = l_e1edl24-lfimg.

*      WHEN 'E1EDL26'.
*        l_e1edl26 = idoc_data-sdata.
*        l_kvgr1   = l_e1edl26-kvgr1.
*        l_kvgr2   = l_e1edl26-kvgr2.

      WHEN 'E1EDL41'.
        l_e1edl41 = idoc_data1-sdata.
        IF l_e1edl41-quali = '001'.
          IF l_e1edl41-bstnr IS INITIAL.
            idoc_status1-docnum = idoc_contrl-docnum.
            idoc_status1-msgty  = 'E'.
            idoc_status1-msgid  = 'VL'.
            idoc_status1-msgno  = 243.
            idoc_status1-msgv1  = 'BSTNR'.
            idoc_status1-msgv2  = idoc_data1-segnum.
            idoc_status1-status = '51'.
            APPEND idoc_status1 TO idoc_status.
            EXIT.
          ELSEIF l_e1edl41-posex IS INITIAL.
            idoc_status1-docnum = idoc_contrl-docnum.
            idoc_status1-msgty  = 'E'.
            idoc_status1-msgid  = 'VL'.
            idoc_status1-msgno  = 243.
            idoc_status1-msgv1  = 'POSEX'.
            idoc_status1-msgv2  = idoc_data1-segnum.
            idoc_status1-status = '51'.
            APPEND idoc_status1 TO idoc_status.
            EXIT.
          ENDIF.

          SPLIT l_e1edl41-bstnr AT '-' INTO ltab_xekes-ebeln l_str.
          l_int = l_e1edl41-posex.
          ltab_xekes-ebelp = l_int.

          ltab_xekes-ebtyp = 'PA'.
          ltab_xekes-kz    = 'I'.
          ltab_xekes-estkz = '3'.
          ltab_xekes-kzdis = 'X'.
          ltab_xekes-erdat = sy-datum.
          ltab_xekes-ezeit = sy-uzeit.
          ltab_xekes-lpein = 1.
          ltab_xekes-xblnr = l_xblnr.
          ltab_xekes-eindt = l_eindt.
          ltab_xekes-uzeit = l_uzeit.

          SELECT MAX( etens ) INTO l_etens
            FROM ekes
            WHERE ebeln = ltab_xekes-ebeln
            AND   ebelp = ltab_xekes-ebelp.
          l_etens = l_etens + 1.
          ltab_xekes-etens = l_etens.

          APPEND ltab_xekes.       "PA lines

          SELECT * FROM ekes INTO CORRESPONDING FIELDS OF TABLE ltab_xekes2
            WHERE ebeln = ltab_xekes-ebeln
            AND   ebelp = ltab_xekes-ebelp
            AND   ( ebtyp = 'AB' OR ebtyp = 'PA' )
            ORDER BY PRIMARY KEY.

          l_paqty = l_e1edl24-lfimg.
          LOOP AT ltab_xekes2.
*            MOVE-CORRESPONDING ltab_xekes2 TO ltab_yekes.
*            ltab_yekes-kz    = 'D'.
*            APPEND ltab_yekes.     "AB lines
*
            l_abqty = ltab_xekes2-menge - ltab_xekes2-dabmg.
            IF l_abqty < 0.
            ELSEIF l_abqty <= l_paqty.
              ltab_xekes2-dabmg = ltab_xekes2-menge.
              l_paqty = l_paqty - l_abqty.
            ELSE.
              ltab_xekes2-dabmg = l_paqty.
              l_paqty = 0.
            ENDIF.
            MOVE-CORRESPONDING ltab_xekes2 TO ltab_xekes.
            ltab_xekes-kz    = 'U'.
            APPEND ltab_xekes.     "AB lines

*            MOVE-CORRESPONDING ltab_xekes2 TO ltab_xekes.
*           ltab_xekes-kz    = 'I'.
*            l_etens = l_etens + 1.
*            ltab_xekes-etens = l_etens.
*            APPEND ltab_xekes.     "AB lines
*
          ENDLOOP.

          CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
            EXPORTING
              i_ebeln = ltab_xekes-ebeln
            TABLES
              xekes   = ltab_xekes
              yekes   = ltab_yekes.

          CLEAR: ltab_xekes, ltab_yekes.
          REFRESH: ltab_xekes, ltab_yekes.
        ENDIF.

    ENDCASE.

  ENDLOOP.

  LOOP AT idoc_status1.
  ENDLOOP.
  IF sy-subrc <> 0.
    idoc_status1-docnum = idoc_contrl-docnum.
    idoc_status1-msgty  = 'S'.
    idoc_status1-msgid  = 'ME'.
    idoc_status1-msgno  = 440.
    idoc_status1-msgv1  = l_e1edl20-lifex.
    idoc_status1-msgv2  = '(PA)'.
    idoc_status1-status = '53'.
    APPEND idoc_status1 TO idoc_status.
  ENDIF.




ENDFUNCTION.
