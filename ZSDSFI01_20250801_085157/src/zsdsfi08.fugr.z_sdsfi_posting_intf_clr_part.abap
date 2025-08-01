FUNCTION z_sdsfi_posting_intf_clr_part.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(I_AUGLV) LIKE  T041A-AUGLV
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"     VALUE(I_SGFUNCT) LIKE  RFIPI-SGFUNCT DEFAULT SPACE
*"     VALUE(I_NO_AUTH) DEFAULT SPACE
*"     REFERENCE(I_XSIMU) TYPE  CHAR1 DEFAULT SPACE
*"     REFERENCE(I_ACCEPT) TYPE  CHAR1 DEFAULT SPACE
*"  EXPORTING
*"     VALUE(E_MSGID) TYPE  SY-MSGID
*"     VALUE(E_MSGNO) TYPE  SY-MSGNO
*"     VALUE(E_MSGTY) TYPE  SY-MSGTY
*"     VALUE(E_MSGV1) TYPE  SY-MSGV1
*"     VALUE(E_MSGV2) TYPE  SY-MSGV2
*"     VALUE(E_MSGV3) TYPE  SY-MSGV3
*"     VALUE(E_MSGV4) TYPE  SY-MSGV4
*"     VALUE(E_SUBRC) TYPE  SY-SUBRC
*"  TABLES
*"      T_BLNTAB STRUCTURE  BLNTAB
*"      T_FTCLEAR STRUCTURE  FTCLEAR
*"      T_FTPOST STRUCTURE  FTPOST
*"      T_FTTAX STRUCTURE  FTTAX
*"      T_PARTIAL STRUCTURE  ZSDSFIS093
*"  EXCEPTIONS
*"      CLEARING_PROCEDURE_INVALID
*"      CLEARING_PROCEDURE_MISSING
*"      TABLE_T041A_EMPTY
*"      TRANSACTION_CODE_INVALID
*"      AMOUNT_FORMAT_ERROR
*"      TOO_MANY_LINE_ITEMS
*"      COMPANY_CODE_INVALID
*"      SCREEN_NOT_FOUND
*"      NO_AUTHORIZATION
*"----------------------------------------------------------------------

*------- Belegdaten initialisieren -------------------------------------
  tcode = i_tcode.
  auglv = i_auglv.
  sgfunct = i_sgfunct.
  xsimu   = i_xsimu.
  PERFORM init_posting.
  CLEAR:   xftclear, defsize.
  REFRESH: xftclear.
  CLEAR:   yftclear.                                      "31i

*------- Transactionscode prüfen ---------------------------------------
  IF tcode NE 'FB05' AND tcode NE 'FB05L'.                  "1527033
    MESSAGE e006 WITH tcode RAISING transaction_code_invalid.
  ENDIF.

*------- Reportname setzen  -----------------------------------------
  rep_name = rep_name_a.                        " Belegvorerfassung

*------- Tabellendaten übertragen --------------------------------------
  LOOP AT t_ftpost.
    xftpost = t_ftpost.
    APPEND xftpost.
  ENDLOOP.
  LOOP AT t_ftclear.
*------- Data without selection field to be appended after sort --------
*------- Only one such item is allowed and it must be last      --------
*------- Additionally, the all-inclusive selection must be last also --
    IF t_ftclear-selfd = space.                          "31i
      yftclear = t_ftclear.
    ELSEIF t_ftclear-selfd   = 'BELNR'         "all other o/i via lbox
        AND t_ftclear-selvon = space
        AND t_ftclear-selbis = 'ZZZZZZZZZZ'.
      yftclear = t_ftclear.
    ELSE.
      xftclear = t_ftclear.
      APPEND xftclear.
    ENDIF.
  ENDLOOP.

* BOD CH00-
*  SORT xftclear BY agkoa agkon agbuk hbkid hktid xnops agums.
* EOD CH00-
* BOI CH00+
  SORT xftclear BY agkoa agkon agbuk hbkid hktid xnops agums selfd selvon.
* EOI CH00+
  IF NOT yftclear = space.                                "31i
    APPEND yftclear TO xftclear.
  ENDIF.

  PERFORM auglv_tabix_ermitteln.

  LOOP AT t_fttax.
    xfttax = t_fttax.
    APPEND xfttax.
  ENDLOOP.
  DESCRIBE TABLE xfttax LINES tfill_xfttax.

*------- Buchungsdatentabelle (XFTPOST) abarbeiten im Loop -------------
  PERFORM xftpost_loop.

*------- Letzte Belegzeile übertragen ----------------------------------
  PERFORM position_uebertragen.

*------- Ausgleichsdaten (XFTCLEAR) abarbeiten -------------------------
  CLEAR dynnr.
  LOOP AT xftclear.
    AT NEW agkoa.
      PERFORM fcode_f06_f07.
    ENDAT.

    AT NEW agkon.
      PERFORM fcode_f06_f07.
    ENDAT.

    AT NEW agbuk.
      PERFORM fcode_f06_f07.
    ENDAT.

    AT NEW hbkid.
      PERFORM fcode_f06_f07.
    ENDAT.

    AT NEW hktid.
      PERFORM fcode_f06_f07.
    ENDAT.

    AT NEW xnops.
      PERFORM fcode_f06_f07.
    ENDAT.

    AT NEW agums.
      PERFORM fcode_f06_f07.
    ENDAT.

    IF dynnr = '0710'.
      PERFORM bselk_uebergeben.
      CLEAR dynnr.
    ENDIF.

    loopc = ( loopc + 1 ) MOD 18.
    IF loopc = 0.
      loopc = 18.
    ENDIF.
    IF loopc = 1.
      PERFORM fcode_f05.
    ENDIF.
    PERFORM bselp_uebergeben.

    DESCRIBE TABLE ft LINES index.
  ENDLOOP.

* BOI CH00+
  IF t_partial[] IS NOT INITIAL.
    PERFORM f_partial_clearing.
  ENDIF.
* EOI CH00+

* BOI CH01+
  IF i_accept = 'X'.
    PERFORM f_accept_popup.
  ENDIF.
* EOI CH01+

*------- Transaktion abschließen ---------------------------------------

  PERFORM fcode_f11.
  IF xsimu = 'X'.
    PERFORM document_simulation.
  ENDIF.
  PERFORM transaktion_beenden USING i_no_auth.

*------- Exportparameter zurückgeben (bei Call Transaction .. Using ..)-
  IF funct   = 'C'
  OR sgfunct = 'C'.
    e_subrc = subrc.
    e_msgty = msgty.
    e_msgid = msgid.
    e_msgno = msgno.
    e_msgv1 = msgv1.
    e_msgv2 = msgv2.
    e_msgv3 = msgv3.
    e_msgv4 = msgv4.
    LOOP AT xbltab.
      t_blntab = xbltab.
      APPEND t_blntab.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
