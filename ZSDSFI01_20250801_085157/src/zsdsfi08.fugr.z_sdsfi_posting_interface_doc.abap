FUNCTION z_sdsfi_posting_interface_doc.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"     VALUE(I_SGFUNCT) LIKE  RFIPI-SGFUNCT DEFAULT SPACE
*"     VALUE(I_NO_AUTH) DEFAULT SPACE
*"     REFERENCE(I_XSIMU) TYPE  CHAR1 OPTIONAL
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
*"      T_FTPOST STRUCTURE  FTPOST
*"      T_FTTAX STRUCTURE  FTTAX
*"  EXCEPTIONS
*"      ACCOUNT_MISSING
*"      COMPANY_CODE_MISSING
*"      POSTING_KEY_INVALID
*"      POSTING_KEY_MISSING
*"      RECORD_TYPE_INVALID
*"      TRANSACTION_CODE_INVALID
*"      AMOUNT_FORMAT_ERROR
*"      TOO_MANY_LINE_ITEMS
*"      COMPANY_CODE_INVALID
*"      SCREEN_NOT_FOUND
*"      NO_AUTHORIZATION
*"----------------------------------------------------------------------
  DATA ls_ft             LIKE bdcdata.                      "Note2387419

  CLEAR xsimu.                                              "Note2387419
  xsimu = i_xsimu.                                          "Note2387419

  sgfunct = i_sgfunct.

*------- Belegdaten initialisieren -------------------------------------
  PERFORM init_posting.
  CLEAR: auglv, tabix_041a, defsize.

*------- Transactionscode prüfen ---------------------------------------
  tcode = i_tcode.
  IF  tcode NE 'FB01'
  AND tcode NE 'FBS1'             " Abgrenzungsbeleg erfassen
  AND tcode NE 'FB41'             " Umbuchung der Steuerlast
  AND tcode NE 'ABF1'             " FB01 der Anlagenbuchhaltung
  AND tcode NE 'FBB1'             " Fremdwährungsbewertung
  AND tcode NE 'FBCB'             " Saldovortragsbuchungen 1562986
  AND tcode NE 'FBVB'             " Vorerfassten Beleg buchen
  AND tcode NE 'FBV1'             " Beleg vorerfassen
  AND tcode NE 'FBD5'.            " Dauerbuchungsbeleg buchen
    MESSAGE e006 WITH tcode RAISING transaction_code_invalid.
  ENDIF.

*------- Reportname setzen  -----------------------------" QHA940705
  IF  tcode =  'FBV1'.
    rep_name = rep_name_bv.                       " Belegvorerfassung
  ELSE.
    rep_name = rep_name_a.                        " Belegvorerfassung
  ENDIF.

*------- Tabellendaten übertragen --------------------------------------
  LOOP AT t_ftpost.
    xftpost = t_ftpost.
    APPEND xftpost.
  ENDLOOP.
* LOOP AT T_FTTAX.                                          "N2205481
* Entries without tax code are meaningless                  "N2205481
  LOOP AT t_fttax WHERE NOT mwskz IS INITIAL                "N2205481
                     OR NOT txkrs IS INITIAL                "N2618516
                     OR NOT ctxkrs IS INITIAL               "N2618516
                     OR NOT txdat IS INITIAL.
    xfttax = t_fttax.
    APPEND xfttax.
  ENDLOOP.
  DESCRIBE TABLE xfttax LINES tfill_xfttax.

*------- Buchungsdatentabelle abarbeiten im Loop -----------------------
  PERFORM xftpost_loop.

*------- Letzte Belegzeile übergeben / Beleg sichern -------------------
  PERFORM position_uebertragen.

  IF send_ok17 = 'X'.
    PERFORM fcode_pbbp.
  ELSE.
    PERFORM fcode_f11.
  ENDIF.

  IF xsimu = 'X'.                                         "Note2387419
    READ TABLE ft INTO ls_ft WITH KEY fval = '/11'.       "Note2387419
    ls_ft-fval = 'BS'.                                    "Note2387419
    MODIFY ft FROM ls_ft INDEX sy-tabix.                  "Note2387419
  ENDIF.                                                  "Note2387419

  PERFORM f_adjust_fb01.

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
