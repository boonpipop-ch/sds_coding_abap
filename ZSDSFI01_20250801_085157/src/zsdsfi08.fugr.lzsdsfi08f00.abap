*&---------------------------------------------------------------------*
*& Include          LZSDSFI08F00
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Function module : Z_SDSFI_POSTING_INTF_CLR_PART
* Function  Desc  : COPY FM POSTING_INTERFACE_CLEARING
*                   AND ADD MORE BDC STEPS FOR PARTIAL CLEARING
*                   ->Additional part will be marked with CH00
* Start Date      : 07.06.2024
* Developer       : Apichat C.
* SAP Version     : S/4 HANA
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        :
* Date          :
* Change Request:
* Transport no. :
* Search term   : CH01
* Description   :
***********************************************************************

***INCLUDE LFIPIF00 .

*eject
*----------------------------------------------------------------------*
*       Form  ABWEICHENDE_ZENTRALE
*----------------------------------------------------------------------*
FORM abweichende_zentrale.

  SELECT SINGLE * FROM t001 WHERE bukrs = bukrs.
  IF sy-subrc NE 0.
    MESSAGE a016 WITH bukrs.
  ENDIF.
  SELECT SINGLE * FROM t019w WHERE mpool = 'SAPMF05A'
                               AND winfk = 'FIL'
                               AND buvar = t001-buvar.
  IF sy-subrc NE 0
  AND t001-buvar NE space.
    SELECT SINGLE * FROM t019w WHERE mpool = 'SAPMF05A'
                                 AND winfk = 'FIL'
                                 AND buvar = space.
  ENDIF.
  IF sy-subrc NE 0.
    MESSAGE a018 WITH 'T019W' 'SAPMF05A' 'FIL' t001-buvar.
  ENDIF.

  CLEAR ft.
  ft-program  = rep_name.
  ft-dynpro   = t019w-winnr.
  ft-dynbegin = 'X'.
  APPEND ft.

  CLEAR ft.
* FT-FNAM = 'RF05A-NEWKO'.                                     "30F
  ft-fnam = fnam_konto.                                     "30F
  ft-fval = knrze.
  APPEND ft.
  CLEAR ft.

ENDFORM.                               " ABWEICHENDE_ZENTRALE

*eject
*-----------------------------------------------------------------------
*        Form  AUGLV_TABIX_ERMITTELN
*-----------------------------------------------------------------------
*        Tabellenindex für Ausgleichsvorgang aus T041A ermitteln
*-----------------------------------------------------------------------
FORM auglv_tabix_ermitteln.
  IF auglv = space AND tcode NE 'FB05L'.                    "1755035
    MESSAGE e009 WITH tcode RAISING clearing_procedure_missing.
  ENDIF.

*------- interne Tabelle für Ausgleichsvorgänge füllen -----------------
  IF tfill_041a = 0.
    SELECT * FROM t041a.
      xt041a-auglv = t041a-auglv.
      APPEND xt041a.
    ENDSELECT.
    DESCRIBE TABLE xt041a LINES tfill_041a.
    IF tfill_041a = 0.
      MESSAGE a010 RAISING table_t041a_empty.
    ENDIF.
  ENDIF.

*------- Tabix für Ausgleichsvorgang merken ----------------------------
  tabix_041a = 0.
  LOOP AT xt041a WHERE auglv = auglv.
    tabix_041a = sy-tabix.
    EXIT.
  ENDLOOP.
  IF tabix_041a = 0.
    MESSAGE e011 WITH auglv RAISING clearing_procedure_invalid.
  ENDIF.
ENDFORM.                    "auglv_tabix_ermitteln

*eject
*-----------------------------------------------------------------------
*        Form  BSELK_UEBERGEBEN
*-----------------------------------------------------------------------
*        Selektionskopfdaten aus FTCLEAR auf Dynpro 710 übergeben.
*-----------------------------------------------------------------------
FORM bselk_uebergeben.
  IF xftclear-agkoa NE space.
    CLEAR ft.
    ft-fnam = 'RF05A-AGKOA'.
    ft-fval = xftclear-agkoa.
    APPEND ft.
  ENDIF.

  CLEAR ft.
  ft-fnam = 'RF05A-AGKON'.
  ft-fval = xftclear-agkon.
  APPEND ft.

  CLEAR ft.
  ft-fnam = 'RF05A-AGBUK'.
  ft-fval = xftclear-agbuk.
  APPEND ft.

* Bank Account Reduction - Support HBKID/HKTID for bank accounts
  IF xftclear-agkoa EQ if_fins_acdoc_constants=>gc_account_type-gl.
    IF xftclear-hbkid IS NOT INITIAL.
      CLEAR ft.
      ft-fnam = 'RF05A-HBKID'.
      ft-fval = xftclear-hbkid.
      APPEND ft.
    ENDIF.

    IF xftclear-hktid IS NOT INITIAL.
      CLEAR ft.
      ft-fnam = 'RF05A-HKTID'.
      ft-fval = xftclear-hktid.
      APPEND ft.
    ENDIF.
  ENDIF.

  IF tcode NE 'FB05L'.                                      "1876263
    CLEAR ft.
    ft-fnam = 'RF05A-XNOPS'.
    ft-fval = xftclear-xnops.
    APPEND ft.
  ENDIF.

  IF tcode NE 'FB05L'.                                      "1876263
    CLEAR ft.
    ft-fnam = 'RF05A-AGUMS'.
    ft-fval = xftclear-agums.
    APPEND ft.
  ENDIF.

  IF NOT xftclear-xfifo IS INITIAL.
    CLEAR ft.
    ft-fnam = 'RF05A-XFIFO'.
    ft-fval = xftclear-xfifo.
    APPEND ft.
  ENDIF.

  IF NOT xftclear-avsid IS INITIAL.
    CLEAR ft.
    ft-fnam = 'RF05A-AVSID'.
    ft-fval = xftclear-avsid.
    APPEND ft.
  ENDIF.

*   Cursor setzen, sonst funktioniert Matchcode Eingabe für Konto nicht
  CLEAR ft.
  ft-fnam = 'BDC_CURSOR'.
  ft-fval = 'RF05A-AGKON'.
  APPEND ft.
ENDFORM.                    "bselk_uebergeben

*eject
*-----------------------------------------------------------------------
*        Form  BSELP_UEBERGEBEN
*-----------------------------------------------------------------------
*        Selektionspositionsdaten aus FTCLEAR auf Dynpro 733 übergeben.
*-----------------------------------------------------------------------
FORM bselp_uebergeben.
*------- falls keine Selektionsdaten Daten nicht senden -----------
  CHECK NOT xftclear-selfd IS INITIAL.

  CLEAR ft.
  ft-fnam(12)    = 'RF05A-FELDN('.
  ft-fnam+12(02) = loopc.
  ft-fnam+14(01) = ')'.
  CONDENSE ft-fnam NO-GAPS.
  ft-fval = xftclear-selfd.
  APPEND ft.

  CLEAR ft.
  ft-fnam(12)    = 'RF05A-SEL01('.
  ft-fnam+12(02) = loopc.
  ft-fnam+14(01) = ')'.
  CONDENSE ft-fnam NO-GAPS.
  ft-fval = xftclear-selvon.
  APPEND ft.

  CLEAR ft.
  ft-fnam(12)    = 'RF05A-SEL02('.
  ft-fnam+12(02) = loopc.
  ft-fnam+14(01) = ')'.
  CONDENSE ft-fnam NO-GAPS.
  ft-fval = xftclear-selbis.
  APPEND ft.

* DESCRIBE TABLE FT LINES INDEX.
ENDFORM.                    "bselp_uebergeben

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_ERMITTELN
*-----------------------------------------------------------------------
*        Nummer des Standardbildes und Zusatzbildes ermitteln.
*-----------------------------------------------------------------------
FORM dynpro_ermitteln.
  DATA: konto_n(10)   TYPE n.
  DATA: ld_tcode      LIKE tcode.                           "1527033

*------- Tabelle TBSL  lesen: (1. interne Tabelle, 2. ATAB-Tabelle) ----
  LOOP AT xtbsl WHERE bschl = bschl.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM tbsl WHERE bschl = bschl.
    IF sy-subrc = 0.
      xtbsl = tbsl.
      APPEND xtbsl.
    ELSE.
*     exit, wg  FB05 ohne Buchungszeilen
      EXIT.
*     MESSAGE E008 WITH BSCHL RAISING POSTING_KEY_INVALID.
    ENDIF.
  ENDIF.

*------- Windowfunktion setzen -----------------------------------------
  CLEAR winfk.
  CASE xtbsl-koart.
    WHEN 'D'.
      winfk = 'ZKOD'.
    WHEN 'K'.
      winfk = 'ZKOK'.
    WHEN 'S'.
      winfk = 'ZKOS'.
    WHEN 'A'.
      winfk = 'ZKOA'.
  ENDCASE.

*------- Steuerkategorie ermitteln -------------------------------------
  CLEAR mwskzs.
  IF xtbsl-koart = 'S'.
    IF konto CO ' 0123456789'.
      konto_n = konto.
      konto   = konto_n.
    ENDIF.
    SELECT SINGLE * FROM skb1 WHERE bukrs = bukrs
                                AND saknr = konto.
    IF sy-subrc EQ 0.
      mwskzs = skb1-mwskz.
    ENDIF.
  ENDIF.

  IF tcode = 'FB05L'.                                       "1527033
    ld_tcode = 'FB05'.                                      "1527033
  ELSEIF tcode = 'FBCB'.                                    "1562986
    ld_tcode = 'FBB1'.                                      "1562986
  ELSE.                                                     "1527033
    ld_tcode = tcode.                                       "1527033
  ENDIF.                                                    "1527033

*------- Dynpronummern ermitteln ---------------------------------------
  CALL FUNCTION 'NEXT_DYNPRO_SEARCH'
    EXPORTING
      i_bschl  = bschl
      i_bukrs  = bukrs
      i_mwskzs = mwskzs
      i_tcode  = ld_tcode
      i_umskz  = umskz
      i_winfk  = winfk
    IMPORTING
      e_dynnra = dynnr
      e_mpool  = mpool
      e_winnrz = winnr
    EXCEPTIONS  "nur noch nicht bereits geprueften Ausnahmen
      bukrs_nf = 1
      dynnr_nf = 2
      tcodd_nf = 3
      tcodm_nf = 4
      winnr_nf = 5
      OTHERS   = 6.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN '1'.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING company_code_invalid.
      WHEN '2' OR '5' OR '6'.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING screen_not_found.
      WHEN '3' OR '4'.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING transaction_code_invalid.
    ENDCASE.
  ENDIF.

* Dynpro ändern bei der Umbuchung der Steuerlast
  IF ( tcode = 'FB41' OR tcode = 'FBCB' ) AND dynnr = '312'.
    dynnr = '300'.
  ENDIF.
ENDFORM.                    "dynpro_ermitteln

*eject
*----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_VV_BER_BESTAND.
*----------------------------------------------------------------------
*        Dieses Dynpro ist für Vermögensverwaltung und soll vor dem
*        Anlagedynpro gesendet werden, um LFVI9-SBERI zu übergeben.
*        LFVI9-SBERI ist in FTVK, da es auch über den Kontierungsblock
*        der Vermögensverwaltung eingegeben werden kann.
*----------------------------------------------------------------------
FORM dynpro_senden_vv_ber_bestand.
  CLEAR ft.
  ft-program  = rep_name_vk.
  ft-dynpro   = '0200'.
  ft-dynbegin = 'X'.
  APPEND ft.
  LOOP AT ftvk.
    ft = ftvk.
    APPEND ft.
  ENDLOOP.
  REFRESH ftvk.
  DESCRIBE TABLE ftvk LINES tfill_ftvk.
ENDFORM.                    "dynpro_senden_vv_ber_bestand

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTA
*-----------------------------------------------------------------------
*        Daten für Standardbild aus FTA in FT übertragen
*        Für Übertragung des BSCHL/KONTO/.. aus nächster Position
*        muß der Index gemerkt werden.
*-----------------------------------------------------------------------
FORM dynpro_senden_fta USING a_dynnr.
  CLEAR ft.
  ft-program  = rep_name.
  ft-dynpro   = a_dynnr.
  ft-dynbegin = 'X'.
  APPEND ft.

* Falls es sich um das Dynpro 312 (Steuerdynpro) handelt, existiert
* das Flag BKPF-XMWST nicht und muß deswegen auf das nächste
* Hauptdynpro verschoben werden.  Dazu merkt man sich mittels XMWST_SET
* fuer die naechste Belegzeile, ob BKPF-XMWST noch angekreuzt werden
* muss.

  IF ( a_dynnr = '0304' OR a_dynnr = '0312' OR
       a_dynnr = '2320' OR a_dynnr = '0320' )
     AND xmwst = 'X'.
    IF a_dynnr = '0304'.
      CLEAR ft.
      ft-fnam = 'RF05A-XMWST'.
      ft-fval = 'X'.
      APPEND ft.
    ENDIF.
    DELETE fta WHERE fnam = 'BKPF-XMWST'.
    IF sy-subrc = 0.
      xmwst_set = 'X'.
    ENDIF.
  ENDIF.

  LOOP AT fta.
    IF fta-fnam = 'BKPF-XMWST'.
      CLEAR xmwst_set.
    ENDIF.
    IF a_dynnr = '0304'.
      IF fta-fnam = 'BKPF-XMWST'
      OR fta-fnam = 'BSEG-VORNR'.
        SHIFT fta-fnam RIGHT.
        fta-fnam(5) = 'RF05A'.
      ENDIF.
    ENDIF.
    ft = fta.
    APPEND ft.
  ENDLOOP.

  IF a_dynnr <> '0304' AND
     a_dynnr <> '0312' AND
     a_dynnr <> '0320' AND
     a_dynnr <> '2320' AND
     xmwst_set = 'X'.
    CLEAR ft.
    ft-fnam = 'BKPF-XMWST'.
    ft-fval = 'X'.
    APPEND ft.
    CLEAR xmwst_set.
  ENDIF.

  IF a_dynnr = '0300'
  OR a_dynnr = '0301'
  OR a_dynnr = '0302'
  OR a_dynnr = '0312'
  OR a_dynnr = '0304'
  OR a_dynnr = '0305'.
    CLEAR ft.
    ft-fnam = 'BDC_CURSOR'.
    ft-fval = fnam_konto.                                   "30F
*   IF REP_NAME = REP_NAME_BV.                              "30F
*     Bei Belegvorerfassung                                 "30F
*     FT-FVAL = 'RF05V-NEWKO'.                              "30F
*   ELSE.                                                   "30F
*     FT-FVAL = 'RF05A-NEWKO'.                              "30F
*   ENDIF.                                                  "30F
    APPEND ft.
  ENDIF.

  DESCRIBE TABLE ft LINES index.
ENDFORM.                    "dynpro_senden_fta

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTC
*-----------------------------------------------------------------------
*        CPD-Daten aus FTC in FT übertragen
*-----------------------------------------------------------------------
FORM dynpro_senden_ftc.

  DATA: move_to_iban(1) TYPE c.

  FIELD-SYMBOLS <ftc> TYPE bdcdata.

  CLEAR ft.
  ft-program  = rep_name_c.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.

* IBAN without bank account number
* if BANKN is available: move BANKL and BANKS to CPD screen
* if BANKN is empty:     move BANKL and BANKS to IBAN popup
  CLEAR move_to_iban.
  IF tfill_ftiban NE 0.
    READ TABLE ftc WITH KEY fnam = 'BSEC-BANKN' ASSIGNING <ftc>.
    IF sy-subrc NE 0 OR <ftc>-fval IS INITIAL.
      move_to_iban = 'X'.
    ENDIF.
  ENDIF.

  LOOP AT ftc.
    IF ( ftc-fnam = 'BSEC-BANKL' OR ftc-fnam = 'BSEC-BANKS' )
       AND move_to_iban = 'X'.
      CLEAR ftiban.
      CONCATENATE 'TIBAN-' ftc-fnam+5 INTO ftiban-fnam.
      ftiban-fval = ftc-fval.
      APPEND ftiban.
      CONTINUE.
    ENDIF.

    ft = ftc.
    APPEND ft.
  ENDLOOP.
ENDFORM.                    "dynpro_senden_ftc

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTK
*-----------------------------------------------------------------------
*        Kontierungsblockdaten aus FTK in FT übertragen
*        Dynpro '0002' enthält alle Daten des Kontierungsblocks
*        außerdem:
*        Daten des VV-Kontierungsblock-Dynpros aus aus FTVK in FT
*        übertragen (nur dann, wenn COBL-IMKEY nicht übergeben wurde)
*-----------------------------------------------------------------------
FORM dynpro_senden_ftk.
  DATA: imkey LIKE bdcdata-fnam VALUE 'COBL-IMKEY'.
  DATA: ld_kontl TYPE kontl_fi  ##NEEDED,                  "Note 604733
        ld_kontt TYPE kontt_fi.                            "Note 604733
  DATA: lt_bukrs       TYPE fagl_t_bukrs,                        "Note1605537
        ls_bukrs       TYPE fagl_s_bukrs,                        "Note1605537
        ld_splt_active TYPE xfeld.                         "Note1605537
  DATA: ld_aufnr LIKE cobl-aufnr,                           "1733058
        ls_ftk   LIKE LINE OF ftk,                          "1733058
        ld_tabix LIKE sy-tabix.                             "1733058

  IF tfill_fttxt NE 0.                                      "2142438
* make sure that ok code for test screen is sent on the GL screen
    CLEAR ft.                                               "2142438
    ft-fnam = 'BDC_OKCODE'.                                 "2142438
    ft-fval = '=TEXT'.                                      "2142438
    APPEND ft.                                              "2142438
  ENDIF.                                                    "2142438
*------- tax screen: only with NewGL/doc.split active ----- Note1137272*
  IF  dynnr = '0312'.                                      "Note1137272
    CHECK: NOT glflex_active IS INITIAL.                   "Note1605537
*                                                               "
    CLEAR: ld_splt_active.                                      "
    REFRESH: lt_bukrs.                                          "
    MOVE bukrs TO ls_bukrs.                                     "
    APPEND ls_bukrs TO lt_bukrs.                                "
    CALL METHOD cl_fagl_split_services=>check_activity          "
      EXPORTING                                                 "
        it_bukrs  = lt_bukrs                                    "
        id_budat  = budat_wt                               "time-dep split
      RECEIVING                                                 "
        rb_active = ld_splt_active.                             "
    CHECK: NOT ld_splt_active IS INITIAL.                  "Note1605537
  ENDIF.                                                   "Note1137272

  CLEAR ft.
  ft-program  = rep_name_k.
  ft-dynpro   = '0002'.
  ft-dynbegin = 'X'.
  APPEND ft.
  PERFORM fcode_enter.
  CLEAR in_cobl.
  CLEAR: ld_aufnr, ld_tabix, ls_ftk.                        "1733058
  LOOP AT ftk WHERE fnam = 'COBL-AUFNR'.                    "1733058
    ld_aufnr = ftk-fval.                                    "1733058
  ENDLOOP.                                                  "1733058
  IF ld_aufnr IS NOT INITIAL.                               "1733058
    IF cl_erp_co_olc_tools=>is_rel_for_order( iv_aufnr = ld_aufnr )
             IS NOT INITIAL.                                "1733058
      LOOP AT ftk INTO ls_ftk.                              "1733058
        IF ls_ftk-fnam = 'COBL-VORNR'.                      "1733058
          ls_ftk-fnam = 'COBL-VORNR_AUF'.                   "1733058
          ld_tabix = sy-tabix.                              "1733058
          EXIT.                                             "1733058
        ENDIF.                                              "1733058
      ENDLOOP.                                              "1733058
      IF ld_tabix > 0.                                      "1733058
        MODIFY ftk INDEX ld_tabix FROM ls_ftk.              "1733058
      ENDIF.                                                "1733058
    ENDIF.                                                  "1733058
  ENDIF.                                                    "1733058
  LOOP AT ftk.

    IF tfill_ftcopa > 0.
      PERFORM fill_in_cobl USING ftk-fnam+5(127) ftk-fval.
    ENDIF.

    ft = ftk.
    APPEND ft.
  ENDLOOP.

* Check if Insurance screen needs to be sent.              Note 604733
* Maybe more details screens from other components.
  IF tfill_ft_generic_kontl > 0.
    READ TABLE ft_generic_kontl WITH KEY fnam = 'BSEG-KONTL'.
    ld_kontl = ft_generic_kontl-fval.

    READ TABLE ft_generic_kontl WITH KEY fnam = 'BSEG-KONTT'.
    ld_kontt = ft_generic_kontl-fval.

    CLEAR ft.

    CASE ld_kontt.
      WHEN 'VV' OR 'VX'.    " account assignment type of IS insurance
* set 'details' flag
        ft-fnam = 'DKACB-XINSUR'.
        ft-fval = 'X'.
        APPEND ft.
      WHEN OTHERS.
*  call whatever field here, similar to example above
*  See routine generic_kontl_data.
    ENDCASE.
  ENDIF.
* End of insertion                                         Note 604733

* Prüfen, ob zusätzl. Kontierungsblockbild der VermögVerw. zu senden ist
  DESCRIBE TABLE ftvk LINES tfill_ftvk.
  IF tfill_ftvk = 0.
    EXIT.
  ENDIF.

* nicht senden, wenn COBL-IMKEY gefüllt ist (FTVK löschen)
  LOOP AT ftk WHERE fnam = imkey.
    REFRESH ftvk.
  ENDLOOP.

* Übertragen in FT
  DESCRIBE TABLE ftvk LINES tfill_ftvk.
  IF tfill_ftvk > 0.
*   'Weiter-Flag' setzen auf Dynpro '0002'
    CLEAR ft.
    ft-fnam     = 'DKACB-XIMKY'.
    ft-fval     = 'X'.
    APPEND ft.
    CLEAR ft.

*   VV-Kontierungsblock-Dynpro
    ft-program  = rep_name_vk.
    ft-dynpro   = '0100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    LOOP AT ftvk.
      ft = ftvk.
      APPEND ft.
    ENDLOOP.

*   zurück zum Standard-Kontierungsblock-Dynpro
    CLEAR ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    APPEND ft.
  ENDIF.

ENDFORM.                    "dynpro_senden_ftk

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTZ
*-----------------------------------------------------------------------
*        Zusatzdaten aus FTZ in FT übertragen
*-----------------------------------------------------------------------
FORM dynpro_senden_ftz USING z_dynnr.
  CLEAR ft.
  ft-program  = rep_name.
  ft-dynpro   = z_dynnr.
  ft-dynbegin = 'X'.
  APPEND ft.
  LOOP AT ftz.
    ft = ftz.
    APPEND ft.
  ENDLOOP.
* Index für Fußzeile hochsetzen
  DESCRIBE TABLE ft LINES index.
ENDFORM.                    "dynpro_senden_ftz

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTVV
*-----------------------------------------------------------------------
*        Daten für Vermögensverwalter aus FTVV in FT übertragen
*        vorher muß noch auf dem alten Bild der OK_Code 'SOPT' gesendet
*        werden.
*-----------------------------------------------------------------------
FORM dynpro_senden_ftvv.
  CLEAR ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = 'SOPT'.
  index = index + 1.
  INSERT ft INDEX index.
  CLEAR ft.
  ft-program  = rep_name_vv.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.
  LOOP AT ftvv.
    ft = ftvv.
    APPEND ft.
  ENDLOOP.
  CLEAR ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '/11'.
  APPEND ft.

ENDFORM.                    "dynpro_senden_ftvv


*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SETZEN_EINSTIEG
*-----------------------------------------------------------------------
*        Einstiegsbild setzen in Tabelle FT.
*-----------------------------------------------------------------------
FORM dynpro_setzen_einstieg.
  DATA ld_tcode          LIKE sy-tcode.                     "1562986
* CASE TCODE.
*   WHEN 'ABF1'.
*     DYNNR = '0100'.
*   WHEN 'FB01'.
*     DYNNR = '0100'.
*   WHEN 'FB41'.
*     DYNNR = '0100'.
*   WHEN 'FBB1'.
*     DYNNR = '0100'.
*   WHEN 'FB05'.
*     DYNNR = '0122'.
*   WHEN 'FBVB'.
*     DYNNR = '0100'.
*   WHEN 'FBD5'.
*     DYNNR = '0125'.
* ENDCASE.
  IF tcode = 'FBCB'.                                        "1562986
    ld_tcode = 'FBB1'.                                      "1562986
  ELSEIF tcode = 'FB05L'.                                   "1717861
    ld_tcode = 'FB05'.                                      "1717861
  ELSE.                                                     "1562986
    ld_tcode = tcode.                                       "1562986
  ENDIF.                                                    "1562986
  SELECT SINGLE * FROM tstc WHERE tcode = ld_tcode.         "1562986
  IF sy-subrc = 0.
    dynnr = tstc-dypno.
  ELSE.
    MESSAGE e018 WITH 'TSTC' ld_tcode.                      "1562986
  ENDIF.

  CLEAR ft.
  ft-program  = rep_name.
  ft-dynpro   = dynnr.
  ft-dynbegin = 'X'.
  APPEND ft.
  index = index + 1.

*-------- set fieldname for new account -------------------------------
  IF rep_name = rep_name_bv.                                "30F
*   preliminary posting                                   "30F
    fnam_konto = 'RF05V-NEWKO'.                             "30F
  ELSE.                                                     "30F
    fnam_konto = 'RF05A-NEWKO'.                             "30F
  ENDIF.                                                    "30F

*-------- Cursor setzen, wg. MatchCode eingabe -------------------------
  CLEAR ft.
  ft-fnam = 'BDC_CURSOR'.
  ft-fval = fnam_konto.                                     "30F
* IF REP_NAME = REP_NAME_BV.                              "30F
*   Bei Belegvorerfassung                                 "30F
*   FT-FVAL = 'RF05V-NEWKO'.                              "30F
* ELSE.                                                   "30F
*   FT-FVAL = 'RF05A-NEWKO'.                              "30F
* ENDIF.                                                  "30F
  APPEND ft.
  index = index + 1.


*------- Ausgleichsvorgang im Loop ankreuzen (bei FB05 etc.) -----------
  IF auglv NE space AND tcode NE 'FB05L'.                   "1755035
    CLEAR ft.
    ft-fnam(12)   = 'RF05A-XPOS1('.
    ft-fnam+12(2) = tabix_041a.
    ft-fnam+14(1) = ')'.
    CONDENSE ft-fnam NO-GAPS.
    ft-fval       = 'X'.
    APPEND ft.
    index = index + 1.
  ENDIF.
ENDFORM.                    "dynpro_setzen_einstieg

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_ENTER
*-----------------------------------------------------------------------
*        Fcode /00 setzen um zu verhindern, dass ein leeres
*        Kontierungsblock übersprungen wird. Es hat zum fehlerhaften
*        Einmischen der Daten der nächsten Position in die aktuellen
*        Daten.
*-----------------------------------------------------------------------
FORM fcode_enter.
  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/00'.
  APPEND ft.
ENDFORM.                    "fcode_enter

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_F05
*-----------------------------------------------------------------------
*        Fcode /05 für 'Batch-Input Selektion' übergeben
*        Dynpro 733 setzen
*-----------------------------------------------------------------------
FORM fcode_f05.
*------- falls keine Selektionsdaten Dynpro 733 nicht senden -----------
  CHECK NOT xftclear-selfd IS INITIAL.

*------- Absprung auf Dynpro 733 --------------------------------------
  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/05'.
  APPEND ft.

  CLEAR ft.
  ft-program  = rep_name_a.
  ft-dynpro   = '0733'.
  ft-dynbegin = 'X'.
  APPEND ft.
  dynnr = '0733'.
ENDFORM.                                                    "fcode_f05

*eject
*-----------------------------------------------------------------------
*        Form  fcode_f06_f07
*-----------------------------------------------------------------------
*        Fcode /06 für 'OP auswählen' bzw. 'Anderes Konto' übergeben
*        Dynpro 710 setzen
*-----------------------------------------------------------------------
FORM fcode_f06_f07.
  IF dynnr NE '0710'.
    CLEAR ft.
    ft-fnam = 'BDC_OKCODE'.

    IF dynnr = '0733'.
      ft-fval = '/07'.
      APPEND ft.
    ELSE.
      ft-fval = '/06'.
      index = index + 1.
      INSERT ft INDEX index.
    ENDIF.

    CLEAR ft.
    ft-program  = rep_name_a.
    ft-dynpro   = '0710'.
    ft-dynbegin = 'X'.
    APPEND ft.

    dynnr = '0710'.
    CLEAR loopc.
  ENDIF.
ENDFORM.                    "fcode_f06_f07

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_F07
*-----------------------------------------------------------------------
*        Fcode /07 für 'Zusatzdaten' übergeben
*-----------------------------------------------------------------------
FORM fcode_f07.
  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/07'.
  APPEND ft.
ENDFORM.                                                    "fcode_f07

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_F11
*-----------------------------------------------------------------------
*        Fcode /11 für 'Sichern' auf dem richtigen Dynpro setzen
*-----------------------------------------------------------------------
FORM fcode_f11.

  IF tfill_ftvv NE 0.
*   Dynpro für Vermögensverwaltung wurde gesendet.
*   Rücksprung auf das vorige Standad-Dynpro
    CLEAR ft.
    ft-program  = rep_name.
    ft-dynpro   = dynnr.
    ft-dynbegin = 'X'.
    APPEND ft.
    IF dynnr = 300 OR dynnr = 305.
      PERFORM leeres_cobl_to_ft.
    ENDIF.
  ENDIF.
* Steuerkurs auch bei Steuerrechnen übernehmen            "N960639
  IF tfill_xfttax NE 0. " AND xmwst NE 'X'.               "N960639
    PERFORM tax_dynpro.
* begin of note 1880631
* deactivate transfer of VATDATE from screen MF05A 0100
* to screen TAX1 0300so that VATDATE is known in MF05A
* from the beginning on
* begin of note 1023317
*    LOOP AT ft WHERE fnam CS 'VATDATE'.
*      APPEND ft.
*      EXIT.
*    ENDLOOP.
*    IF sy-subrc IS INITIAL.
*      LOOP AT ft WHERE fnam CS 'VATDATE'.
*        DELETE ft.
*        EXIT.
*      ENDLOOP.
*    ENDIF.
* end of note 1023317
* end of note 1880631
    DESCRIBE TABLE ft LINES index.
  ENDIF.

* PF11- Sichern
  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/11'.
  index = index + 1.
  INSERT ft INDEX index.

* BOI CH00
  PERFORM f_adj_fcode.
* EOI CH00

ENDFORM.                                                    "fcode_f11

*eject
*-----------------------------------------------------------------------
*        FORM TAX_DYNPRO.
*-----------------------------------------------------------------------
*        Steuern-Dynpro
*-----------------------------------------------------------------------
FORM tax_dynpro.
* Funktionscode setzten
* CLEAR FT.                                                 "STEG
* FT-FNAM     = 'BDC_OKCODE'.                               "STEG
* FT-FVAL     = 'STEB'.                                     "STEG
* INDEX = INDEX + 1.                                        "STEG
* INSERT FT INDEX INDEX.                                    "STEG

  SELECT SINGLE * FROM t001 WHERE bukrs = bkpf_bukrs.      "Note 641889
  IF sy-subrc NE 0.
    MESSAGE a016 WITH bukrs.
  ENDIF.
  SELECT SINGLE * FROM t005 WHERE land1 = t001-land1.
  IF sy-subrc NE 0.
    MESSAGE a017 WITH t001-land1.
  ENDIF.

*-------- XFTTAX komprimieren vor Aufruf der Tax-Dynpros----------------
  PERFORM xfttax_komprimieren.
  DATA(lv_txa_active) = cl_fot_txa_utilities=>agent->is_tax_abroad_active( bkpf_bukrs ).
  DATA(lv_kalsm) = COND #( WHEN lv_txa_active = abap_true
                             THEN cl_fot_common_dao=>agent->get_country_data( gv_tax_country )-kalsm
                           ELSE t005-kalsm ).
  SELECT SINGLE * FROM ttxd WHERE kalsm = lv_kalsm.
  IF sy-subrc = 0.
    PERFORM tax_dynpro_us.
  ELSE.
    PERFORM tax_dynpro_not_us.
  ENDIF.
ENDFORM.                    "tax_dynpro


*eject
*-----------------------------------------------------------------------
*        FORM TAX_DYNPRO_US.
*-----------------------------------------------------------------------
*        Steuern-Dynpro für USA
*-----------------------------------------------------------------------
FORM tax_dynpro_us.
  DATA: tax_screen     LIKE tstc-dypno." Screen Number      "STEG
  PERFORM find_us_tax_screen_number USING tax_screen.           "STEG
  CLEAR ft.
  ft-program  = rep_name_t.
  ft-dynpro   = tax_screen.            "STEG
  ft-dynbegin = 'X'.
  APPEND ft.
  IF tax_screen = '0450'.              "STEG
    PERFORM set_function_code_steb.    "STEG
    CLEAR loopc.
    LOOP AT xfttax.
      loopc = loopc + 1.

      IF loopc = 1.
        PERFORM tax_exchange_rate.                           " Note 564235
        IF xfttax-bschl IS INITIAL.                         "N960639
          EXIT.                                             "N960639
        ENDIF.                                              "N960639
      ENDIF.

      IF loopc > 16.
        defsize = 'X'.             "N849676 set default screen size
        CLEAR ft.
        ft-fnam     = 'BDC_OKCODE'.
        ft-fval     = 'P+'.
        APPEND ft.

        CLEAR ft.
        ft-program  = rep_name_t.
        ft-dynpro   = tax_screen.
        ft-dynbegin = 'X'.
        APPEND ft.

        loopc = 1.
      ENDIF.

      PERFORM append_taxline_to_ft USING 'BSET-FWSTE'  xfttax-fwste.
      PERFORM append_taxline_to_ft USING 'BSET-MWSKZ'  xfttax-mwskz.
      PERFORM append_taxline_to_ft USING 'BSEG-BSCHL'  xfttax-bschl.
      PERFORM append_taxline_to_ft USING 'BSET-TXJCD'  xfttax-txjcd.
      PERFORM append_taxline_to_ft USING 'BSET-KSCHL'  xfttax-kschl.
    ENDLOOP.
  ELSE.                                "STEG
    PERFORM set_function_code_steg.    "STEG
    CLEAR loopc.
    CLEAR xfttax_count.                                     "N1899628
    CLEAR count_valid_gl_items.                             "N3254798
    CLEAR: g_ftpost_count_max, g_ftpost_count.              "N3254798
* only for FBV1 parking:                                     N1899628
* if tax linewise then tax data must be provide line-by-line N1899628
* but NOT on detail level, this means kschl must be empty    N1899628
* this means tax data must be summarized per item            N1899628
*                                                            N1899628
* for FB01 tax data can be per item and per kschl            N1899628
* but the screen 450 is called                               N1899628
    LOOP AT xfttax.
      ADD 1 TO xfttax_count.                                "N1899628
      loopc = loopc + 1.
      IF loopc > 13.                       "N2277780 was 14 "N3253786
        defsize = 'X'.             "N849676 set default screen size
        CLEAR ft.
        ft-fnam     = 'BDC_OKCODE'.
        ft-fval     = 'P+'.
        APPEND ft.

        CLEAR ft.
        ft-program  = rep_name_t.
        ft-dynpro   = tax_screen.
        ft-dynbegin = 'X'.
        APPEND ft.

        loopc = 1.
      ENDIF.

* Start of note 1899628.
      CLEAR: gl_tax_code_found.
*            count_valid_gl_items.                          "N3254798
      IF tcode = 'FBV1'.
* gl-item can be missing or gl-amount can be 0;
* then mwskz, bschl, txjcd, hwste must be transferred to to tax screen
* otherwise they will be proposed and screen fields are closed for input;
* for FB01 and FB05 etc this cannot happen !!

* look for gl-item with the same tax code
*                  and amount > 0
* if tax line-by-line: the  same order or count (?)
*       CLEAR: g_ftpost_count_max, g_ftpost_count.          "N3254798
        IF g_ftpost_count_max = 0.                          "N3254798
          LOOP AT xftpost ASSIGNING <ftpost1>.
            IF <ftpost1>-count > g_ftpost_count_max.
              g_ftpost_count_max = <ftpost1>-count.
            ENDIF.
          ENDLOOP.
        ENDIF.                                              "N3254798

        WHILE g_ftpost_count LT g_ftpost_count_max
          AND gl_tax_code_found IS INITIAL.

          LOOP AT xftpost ASSIGNING <ftpost1>
                          WHERE fnam CS 'MWSKZ'
* the same tax code
                          AND   fval = xfttax-mwskz
                          AND   fval NE '**'
                          AND   fval NE space
                          AND   stype = 'P'
* next item
                          AND   count > g_ftpost_count.
* store item number
            g_ftpost_count = <ftpost1>-count.
            EXIT.
          ENDLOOP.
          IF sy-subrc IS INITIAL.
* amount of this item > 0 ?
            LOOP AT xftpost ASSIGNING <ftpost2>
                            WHERE count = <ftpost1>-count
                            AND   fnam  CS 'WRBTR'
                            AND   fval  NE space.   "ne 0
              EXIT.
            ENDLOOP.
            IF sy-subrc IS INITIAL.
* posting key of this item is neither vendor nor customer
              LOOP AT xftpost ASSIGNING <ftpost2>
                                       WHERE count = <ftpost1>-count
                                       AND   fnam  CS 'NEWBS'
                                       AND   fval  NE '31'     "ne vendor item
                                       AND   fval  NE '21'
                                       AND   fval  NE '11'
                                       AND   fval  NE '01'.
                EXIT.
              ENDLOOP.
            ENDIF.
            IF sy-subrc IS INITIAL.
              ADD 1 TO count_valid_gl_items.
              IF NOT tax_linewise IS INITIAL.
                CHECK xfttax_count = count_valid_gl_items.
              ENDIF.
* stop while
              gl_tax_code_found = 'X'.
            ENDIF.
          ELSE.
* stop while
            g_ftpost_count = g_ftpost_count_max.
          ENDIF.
        ENDWHILE.

      ENDIF.
* End of note 1899628.

      PERFORM append_taxline_to_ft USING 'BSET-FWSTE'  xfttax-fwste.
* Start of note 1899628.
*     PERFORM append_taxline_to_ft USING 'BSET-KSCHL'  xfttax-kschl.
      IF tcode = 'FBV1' AND gl_tax_code_found IS INITIAL.
        PERFORM append_taxline_to_ft USING 'BSET-MWSKZ'  xfttax-mwskz.
        PERFORM append_taxline_to_ft USING 'BSEG-BSCHL'  xfttax-bschl.
        PERFORM append_taxline_to_ft USING 'BSET-TXJCD'  xfttax-txjcd.
        PERFORM append_taxline_to_ft USING 'BSET-HWSTE'  xfttax-hwste.
      ELSEIF tcode = 'FB01' AND tax_screen = '0300'.        "N2481799
        PERFORM append_taxline_to_ft USING 'BSET-HWSTE'  xfttax-hwste.
      ENDIF.
* End of note 1899628.
    ENDLOOP.
  ENDIF.                               "STEG

ENDFORM.                    "tax_dynpro_us


*eject
*-----------------------------------------------------------------------
*        FORM TAX_DYNPRO_NOT_US.
*-----------------------------------------------------------------------
*        Steuern-Dynpro für alle Länder außer USA
*-----------------------------------------------------------------------
FORM tax_dynpro_not_us.

  PERFORM set_function_code_steb.      "STEG
  CLEAR ft.
  ft-program  = rep_name_t.
  DATA(lv_txa_active) = cl_fot_txa_utilities=>agent->is_tax_abroad_active( bkpf_bukrs ).
  IF cl_fot_tdt_cmn_util=>get( )->is_time_dep_and_no_taxjur(
            iv_bukrs = bkpf_bukrs
            iv_land1 = COND #( WHEN lv_txa_active = abap_true
                                 THEN gv_tax_country ) ).
    ft-dynpro   = '0310'.
  ELSE.
    ft-dynpro   = '0300'.
  ENDIF.
  ft-dynbegin = 'X'.
  APPEND ft.

  SORT xfttax BY mwskz bschl.
  CLEAR loopc.
  LOOP AT xfttax.
    loopc = loopc + 1.

    IF loopc = 1.
      PERFORM tax_exchange_rate.                           " Note 564235
      PERFORM tax_calcdate.
      PERFORM tax_fulfilldate.
      IF xfttax-bschl IS INITIAL.                           "N960639
        EXIT.                                               "N960639
      ENDIF.                                                "N960639
    ENDIF.

    IF loopc > 13.                         "N2882701 was 14 "N3253786
      defsize = 'X'.             "N849676 set default screen size
      CLEAR ft.
      ft-fnam     = 'BDC_OKCODE'.
      ft-fval     = 'P+'.
      APPEND ft.

      CLEAR ft.
      ft-program  = rep_name_t.
      IF cl_fot_tdt_cmn_util=>get( )->is_time_dep_and_no_taxjur(
                iv_bukrs = bkpf_bukrs
                iv_land1 = COND #( WHEN lv_txa_active = abap_true
                                     THEN gv_tax_country ) ).
        ft-dynpro   = '0310'.
      ELSE.
        ft-dynpro   = '0300'.
      ENDIF.
      ft-dynbegin = 'X'.
      APPEND ft.

      loopc = 1.
    ENDIF.
    PERFORM append_taxline_to_ft USING 'BSET-FWSTE'  xfttax-fwste.
    PERFORM append_taxline_to_ft USING 'BSET-MWSKZ'  xfttax-mwskz.
    PERFORM append_taxline_to_ft USING 'BSEG-BSCHL'  xfttax-bschl.
    PERFORM append_taxline_to_ft USING 'BSET-TXJCD'  xfttax-txjcd.
    PERFORM append_taxline_to_ft USING 'BSET-HWSTE'  xfttax-hwste.
  ENDLOOP.

ENDFORM.                    "tax_dynpro_not_us

*---------------------------------------------------------------------*
*       FORM APPEND_TAXLINE_TO_FT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FIELDNAME                                                     *
*  -->  FIELDVALUE                                                    *
*---------------------------------------------------------------------*
FORM append_taxline_to_ft USING fieldname fieldvalue.       " XBETRAG.
  CHECK NOT fieldvalue IS INITIAL.

  CLEAR ft.
  ft-fnam(14)    = fieldname.

  ft-fnam+14(01) = '('.
  ft-fnam+15(02) = loopc.
  ft-fnam+17(01) = ')'.

  CONDENSE ft-fnam NO-GAPS.
  ft-fval = fieldvalue.
  APPEND ft.

ENDFORM.                    "append_taxline_to_ft


*---------------------------------------------------------------------*
*       FORM FCODE_F11_OLD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fcode_f11_old  ##CALLED.
  IF dynnr = 300.
    PERFORM leeres_cobl_to_ft.
  ENDIF.
  IF tfill_ftvv EQ 0.
    IF tfill_xfttax EQ 0.
      CLEAR ft.
      ft-fnam = 'BDC_OKCODE'.
      ft-fval = '/11'.
      index = index + 1.
      INSERT ft INDEX index.
    ENDIF.

  ELSE.
*   Dynpro für Vermögensverwaltung wurde gesendet.
*   Rücksprung auf das vorige Standad-Dynpro und dann /11
    CLEAR ft.
    ft-program  = rep_name.
    ft-dynpro   = dynnr.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam = 'BDC_OKCODE'.
    ft-fval = '/11'.
    APPEND ft.
  ENDIF.

ENDFORM.                    "fcode_f11_old

*eject
*-----------------------------------------------------------------------
*        FORM  LEERES_COBL_TO_FT.
*-----------------------------------------------------------------------
*        leeres Kontierungsblock-Dynpro senden
*-----------------------------------------------------------------------
FORM leeres_cobl_to_ft.
  CLEAR ft.
  ft-program  = rep_name_k.
  ft-dynpro   = '0002'.
  ft-dynbegin = 'X'.
  APPEND ft.
ENDFORM.                    "leeres_cobl_to_ft

*eject
*-----------------------------------------------------------------------
*        Form  INIT_POSTING
*-----------------------------------------------------------------------
*        Belegdaten initialisieren
*-----------------------------------------------------------------------
FORM init_posting.
  CLEAR:   umskz.
  CLEAR:   bukrs, waers, xmwst, xmwst_set, send_ok17.
  CLEAR:   blart, budat, budat_int.

  CLEAR:   ft, fta, ftc, ftf, ftz, ftk, ftvk, ftvv, xftpost, xfttax.
  CLEAR:   ftps.                                            "30F
  CLEAR:   ftab.                                              "KJV
  CLEAR:   ftw.
  CLEAR:   ftsplt.
  CLEAR:   ftsplt_wt.                                       "1414479
  CLEAR:   ftcopa.
  CLEAR:   fttxt.                                           "2939893
  REFRESH: ft, fta, ftc, ftf, ftz, ftk, ftvk, ftvv, xftpost, xfttax.
  REFRESH: ftfkto, ftps.                                    "30F
  REFRESH: ftw.
  REFRESH: ftab.                                             "KJV
  REFRESH: ftsplt.
  REFRESH: ftsplt_wt.
  REFRESH: ftcopa.                                          "1414479
  REFRESH: fttxt.                                           "2939893

* BOI CH00+
  CLEAR: e_msgid,
         e_msgno,
         e_msgty,
         e_msgv1,
         e_msgv2,
         e_msgv3,
         e_msgv4,
         e_subrc.
* EOI CH00+

ENDFORM.                    "init_posting

*eject
*-----------------------------------------------------------------------
*        Form  MAPPE_ABSPIELEN_IM_BATCH.
*-----------------------------------------------------------------------
FORM mappe_abspielen_im_batch.
  GET RUN TIME FIELD runtime.
  jobname    = 'RSBDCSUB-FIPI'.
  jobname+14 = runtime.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = jobname
      jobgroup         = 'FIPI'
    IMPORTING
      jobcount         = jobcount
    EXCEPTIONS
      cant_create_job  = 01
      invalid_job_data = 02
      jobname_missing  = 03.

  IF sy-subrc NE 0.
    MESSAGE e015  RAISING session_not_processable.
  ENDIF.


  SUBMIT rsbdcsub AND RETURN
                  USER sy-uname
                  VIA JOB jobname NUMBER jobcount
                  WITH queue_id =  queue_id
                  WITH z_verarb =  'X'.


  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname              = jobname
      jobcount             = jobcount
      strtimmed            = bdcimmed
      sdlstrtdt            = bdcstrtdt
      sdlstrttm            = bdcstrttm
    EXCEPTIONS
      cant_start_immediate = 01
      jobname_missing      = 02
      job_close_failed     = 03
      job_nosteps          = 04
      job_notex            = 05
      lock_failed          = 06
      invalid_startdate    = 07
      OTHERS               = 99.

  IF sy-subrc NE 0.
    MESSAGE e015  RAISING session_not_processable.
  ENDIF.

  CLEAR  bdcimmed.
  bdcstrtdt = space.
  bdcstrttm = space.
ENDFORM.                    "mappe_abspielen_im_batch

*eject
*-----------------------------------------------------------------------
*        Form  MAPPE_OEFFNEN
*-----------------------------------------------------------------------
*        Öffnen der BDC-Queue für Datentransfer
*-----------------------------------------------------------------------
FORM mappe_oeffnen.
  CLEAR queue_id.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client       = mandt
      group        = group
      holddate     = holdd
      keep         = xkeep
      user         = usnam
      app_area     = bdc_app_area
    IMPORTING
      qid          = queue_id
    EXCEPTIONS
      user_invalid = 1              "2291261
      OTHERS       = 2.
  IF sy-subrc = 0.
    group_open = 'X'.
  ELSEIF sy-subrc = 1.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno       "2291261
      WITH sy-msgv1 sy-msgv2 RAISING user_invalid.          "2291261
  ENDIF.
ENDFORM.                    "mappe_oeffnen

*eject
*-----------------------------------------------------------------------
*        Form  MAPPE_SCHLIESSEN
*-----------------------------------------------------------------------
FORM mappe_schliessen.
  IF group_open = 'X'.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    CLEAR group_open.
  ENDIF.
ENDFORM.                    "mappe_schliessen

*eject
*-----------------------------------------------------------------------
*        Form  POSITION_UEBERTRAGEN
*-----------------------------------------------------------------------
*        Gruppenwechsel bei Belegposition:
*        Die in den internen Tabellen FTA, FTF, etc. gesammelten
*        Daten einer Belegposition werden in der richtigen
*        Reihenfolge in Tabelle FT übertragen.
*        Vor der Übertragung erfolgt die Dynproermittlung.
*-----------------------------------------------------------------------
FORM position_uebertragen.

* prüfen, ob konkurrierende Felder für NEWKO vorhanden.
  DESCRIBE TABLE ftfkto LINES tfill_ftfkto.
  IF tfill_ftfkto = 1.
*   alles klar FTF ergänzen
    LOOP AT ftfkto.
      ftfkto-fnam = fnam_konto.                             "30F
      CLEAR ftf.
      ftf = ftfkto.
      APPEND ftf.
      konto = ftf-fval.
    ENDLOOP.
  ELSEIF tfill_ftfkto = 2.
*   der erste Treffer wird als Mitbuchkonto angesehen
*   danach darf nur noch genau ein Konto da sein, das ungleich
*    BSEG-HKONT sein muß
    LOOP AT ftfkto WHERE fnam = 'BSEG-HKONT'.
      ftfkto_indx = sy-tabix.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      CLEAR fta.
      fta = ftfkto.
      APPEND fta.
      DELETE ftfkto INDEX ftfkto_indx.
      LOOP AT ftfkto.
        IF ftfkto-fnam = 'BSEG-HKONT'.
          MESSAGE e019.
        ELSE.
          ftfkto-fnam = fnam_konto.                         "30F
          CLEAR ftf.
          ftf = ftfkto.
          APPEND ftf.
          konto = ftf-fval.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e020.
    ENDIF.
  ELSEIF tfill_ftfkto > 2.
    MESSAGE e021.
  ELSEIF tfill_ftfkto = 0.
    EXIT.
  ENDIF.
  REFRESH ftfkto.

*------- Fußzeile vollständig? -----------------------------------------
  IF bukrs = space.
    MESSAGE e012 RAISING company_code_missing ##FM_RAISE_OK.
  ENDIF.

  IF bschl = space.
    MESSAGE e013 WITH xftpost-count RAISING posting_key_missing ##MG_MISSING ##FM_RAISE_OK.
  ENDIF.
  IF konto = space.
    MESSAGE e014 WITH xftpost-count RAISING account_missing ##MG_MISSING ##FM_RAISE_OK.
  ENDIF.


*------- Fußzeiledaten übertragen --------------------------------------
  LOOP AT ftf.
    ft = ftf.
    index = index + 1.
    INSERT ft INDEX index.
  ENDLOOP.

*------- Übertragen einer abweichenden Zentrale ------------------------
  IF konto(1) = '+'.
    PERFORM abweichende_zentrale.
  ENDIF.

*------- nächste DYNNR ermitteln (Standardbild + Zusatzdaten) ----------
  PERFORM dynpro_ermitteln.

*------- PublicSector-Daten bei Debitor/Kreditor auf Zusatz-Daten   ----
*-------                    bei Anlagen auf Haupt-Dynpro            ----
  LOOP AT ftps.                                             "30F
    IF dynnr = '0303' OR dynnr = '0304' OR
      dynnr = '0305' OR dynnr = '0312'.                     "Note885510
      fta = ftps.                                           "30F
      APPEND fta.                                           "30F
    ELSE.                                                   "30F
      ftz = ftps.                                           "30F
      APPEND ftz.                                           "30F
    ENDIF.                                                  "30F
  ENDLOOP.                                                  "30F


*------ Falls es sich um das Dynpro "Belegposition Kreditor handelt,
*------ muessen die Felder BSEG-VERTN und BSEG-VERTT auf das
*------ Zusatzdaten-Dynpro

  IF dynnr = '0302'.
    LOOP AT fta.
      IF fta-fnam = 'BSEG-VERTT' OR
         fta-fnam = 'BSEG-VERTN' OR
         fta-fnam = 'BSEG-VBEWA'.
        ftz = fta.
        APPEND ftz.
        DELETE fta.
      ENDIF.
    ENDLOOP.
  ENDIF.
*        budget period (EhP4)                              "Note1677826
  IF dynnr = '0302'                    "vendor                  "
  OR dynnr = '0301'.                   "customer                "
    LOOP AT fta                                                 "
     WHERE fnam = 'BSEG-BUDGET_PD'.    "budget-period           "
      MOVE fta TO ftz.                                          "
      APPEND ftz.                                               "
      DELETE fta.                                               "
    ENDLOOP.                                                    "
  ENDIF.                                                   "Note1677826

*------ Falls es sich um das Dynpro "Direkte Steuerbuchung" handelt,
*------ müssen die Felder FISTL,FIPOS,GRANT_NBR,FKBER,GEBER  aus
*------ dem Kontierungsblock auf das allgemeine Dynpro
*------ Tabelle ist dann von COBL auf BSEG umzusetzen
*------ Felder KOSTL,AUFNR,PS_PSP_PNR,RECID   sind auf Joint Venture
*------ Subscreen SAPLGJTS 0001 dort aber als COBL-Felder und müssen
*------ daher nicht auf BSEG umgesetzt werden
*------ Neu mit Hinweis 1137272
  IF dynnr = '0312'.
    LOOP AT ftk.
      IF ftk-fnam = 'COBL-FISTL'          OR
         ftk-fnam = 'COBL-FIPOS'          OR
         ftk-fnam = 'COBL-GRANT_NBR'      OR
         ftk-fnam = 'COBL-GEBER'          OR
         ftk-fnam = 'COBL-GSBER'          OR            "Note1630382
         ftk-fnam = 'COBL-BUDGET_PD'.                   "Note1630382
        fta = ftk.
        fta-fnam(4) = 'BSEG'.
        APPEND fta.
        DELETE ftk.
      ENDIF.
      IF ftk-fnam = 'COBL-KOSTL'          OR
         ftk-fnam = 'COBL-AUFNR'          OR
         ftk-fnam = 'COBL-PS_PSP_PNR'     OR
         ftk-fnam = 'COBL-RECID'.
        fta = ftk.
        APPEND fta.
        DELETE ftk.
      ENDIF.
      IF ftk-fnam = 'COBL-FKBER'.
        fta = ftk.
        fta-fnam = 'BSEG-FKBER_LONG'.
        APPEND fta.
        DELETE ftk.
      ENDIF.
    ENDLOOP.
    LOOP AT ftz.                                            "N2120000
      IF ftz-fnam = 'BSEG-J_1TPBUPL'.
        fta = ftz.
        APPEND fta.
        DELETE ftz.
      ENDIF.
    ENDLOOP.                                                "N2120000
  ENDIF.


*------ ALC Payment supplement on cash relevant gl items on detail
*------ screen 330
  IF dynnr = '0300'.
    LOOP AT fta WHERE fnam = 'BSEG-UZAWE'.
      ftz = fta.
      APPEND ftz.
      DELETE fta.
      EXIT.
    ENDLOOP.
    LOOP AT fta WHERE fnam = 'BSEG-KIDNO'.
      ftz = fta.
      APPEND ftz.
      DELETE fta.
      EXIT.
    ENDLOOP.
    "Begin enable branch code value filled in the field, copied from screen 312
    LOOP AT ftz.
      IF ftz-fnam = 'BSEG-J_1TPBUPL'.
        fta = ftz.
        APPEND fta.
        DELETE ftz.
      ENDIF.
    ENDLOOP.
    "End enable branch code value filled in the field, copied from screen 312
  ENDIF.

* Mandate reference (MNDID)                                   "N1923657
  IF xtbsl-koart = 'D'.                                     "N1923657
    DESCRIBE TABLE ftc     LINES tfill_ftc.                 "N1923657
    DESCRIBE TABLE ftmndid LINES tfill_ftmndid.             "N1923657
    IF tfill_ftc EQ 0 AND tfill_ftmndid NE 0.               "N1923657
      LOOP AT ftmndid.                                      "N1923657
        ftz = ftmndid.                                      "N1923657
        APPEND ftz.                                         "N1923657
        DELETE ftmndid.                                     "N1923657
*        EXIT.                                                "N1923657
      ENDLOOP.                                              "N1923657
    ENDIF.                                                  "N1923657
  ELSE.                                                     "N1923657
    CLEAR ftmndid.                                          "N1923657
    REFRESH ftmndid.                                        "N1923657
  ENDIF.                                                    "N1923657

* G/L account reduction: HBKID/HKTID are available on screen 0300 and 0330
* For BTC fill always field on screen 300 (FTA)
  IF dynnr = '0300'
  AND NOT ftz[] IS INITIAL.
    LOOP AT ftz WHERE fnam = 'BSEG-HBKID'
                    OR fnam = 'BSEG-HKTID'.
      fta = ftz.
      APPEND fta.
      DELETE ftz.
    ENDLOOP.
  ENDIF.

*------ Auskommentiert, da ab 4.0C das Feld nicht mehr auf dem Dynpro
*------ SAPMF05A/0305 vorhanden ist  ---> Kontierungsblock.
*------ Bei Anlagen (Dynpro 305) kann man auf dem Hauptbild auch
*------ das Kontierungsblockfeld COBL-PRCTR eingeben.
* LOOP AT FTK WHERE FNAM = 'COBL-PRCTR'.
*   IF DYNNR = '0305' AND REP_NAME = REP_NAME_A.
*     REPLACE 'COBL' WITH 'BSEG' INTO FTK-FNAM.
*     FTA = FTK.
*     APPEND FTA.
*     DELETE FTK.
*   ENDIF.
* ENDLOOP.

*------- Daten für Sonder-Dynpros vorhanden? ---------------------------
  DESCRIBE TABLE ftc       LINES  tfill_ftc.
  DESCRIBE TABLE ftiban    LINES  tfill_ftiban.
  DESCRIBE TABLE ftmndid   LINES  tfill_ftmndid.            "N1923657
  DESCRIBE TABLE ftk       LINES  tfill_ftk.
  DESCRIBE TABLE ftz       LINES  tfill_ftz.
  DESCRIBE TABLE ftvv      LINES  tfill_ftvv.
  DESCRIBE TABLE ftvk      LINES  tfill_ftvk.
  DESCRIBE TABLE ftcopa    LINES  tfill_ftcopa.
  DESCRIBE TABLE ftisis    LINES  tfill_ftisis.
  DESCRIBE TABLE ft_generic_kontl LINES  tfill_ft_generic_kontl.
  DESCRIBE TABLE ftab      LINES  tfill_ftab  .                   "KJV
*---------------------------------------------- extended withholding tax
  DESCRIBE TABLE ftw       LINES  tfill_ftw.
  DESCRIBE TABLE ftsplt    LINES  tfill_ftsplt.
  DESCRIBE TABLE fttxt     LINES  tfill_fttxt.              "2142438

  PERFORM wechseldaten_frankreich.

*------- CpD-Daten senden? ---------------------------------------------
  IF  tfill_ftc   NE  0
  AND regul       NE 'X'.
    PERFORM dynpro_senden_ftc.
*------- IBAN-Daten senden?
    IF tfill_ftiban NE 0.
      PERFORM iban_data.
* ------ Call mandate screens --------------------------------"N1923657
      IF tfill_ftmndid NE 0.                                "N1923657
        PERFORM mandate_data.                               "N1923657
      ENDIF.                                                "N1923657
    ENDIF.
  ENDIF.

*------ Vermögensverwaltung Vordynpro vor Anlage -----------------------
  INCLUDE ifre_begin_of_re_classic.
  IF  xtbsl-koart = 'A'
  AND tfill_ftvk  > 0.
    PERFORM dynpro_senden_vv_ber_bestand.
  ENDIF.
  INCLUDE ifre_end_of_re_classic.

*------- Standardbild senden (+F07) ------------------------------------
  PERFORM dynpro_senden_fta USING dynnr.
  IF tfill_fttxt NE 0 AND                                   "2198397
     dynnr NE 300     AND                                   "2198397
     dynnr NE 305     AND                                   "2198397
     dynnr NE 312.                                          "2198397
    PERFORM dynpro_senden_fttxt.                            "2198397
    IF  tfill_ftc   NE  0                                   "2198397
      AND regul       NE 'X'.                               "2198397
      PERFORM dynpro_senden_ftc.                            "2198397
    ENDIF.                                                  "2198397
    PERFORM dynpro_senden_fta USING dynnr.                  "2198397
  ENDIF.                                                    "2198397

  IF tfill_ftz NE 0.                                        "i
    PERFORM fcode_f07.                                      "i
  ENDIF.                                                    "i
*---------------------------------------------- extended withholding tax
* send popup for withholding tax data
  PERFORM dynpro_senden_wt.

*  if tfill_ftz ne 0.
*    perform fcode_f07.
*  endif.

*------- Popup für Betragssplitt (FTSPLT)-------------------------------
  IF tfill_ftsplt NE 0.
    CALL FUNCTION 'AC_APAR_SPLIT_FILL_FT'
      EXPORTING
        i_dynnr       = dynnr
      TABLES
        t_ft_split    = ftsplt
        t_ft          = ft
        t_ft_split_wt = ftsplt_wt.

  ENDIF.

*------- Kontierungsblock-Dynpro senden, falls Sachkontenbild ----------
  IF dynnr = 300 OR dynnr = 305 OR dynnr = 312.             "Note1137272
    PERFORM dynpro_senden_ftk.

*------- Send all additional screens here:

* Profitability analysis (CO-PA) screen
    PERFORM copa_daten.
* Insurance screen (IS Insurance)
    PERFORM isis_daten.
* Also Insurance screen, maybe screen from other industry solutions
    PERFORM generic_kontl_data.      " field KONTL filled ?

*------- Textscreen senden? --------------------------------------------
    IF tfill_fttxt NE 0.                                    "2142438
      PERFORM dynpro_senden_fttxt.                          "2142438

* back to the coding block.                             "2142438
      CLEAR ft.                                             "2142438
      ft-program  = rep_name_k.                             "2142438
      ft-dynpro   = '0002'.                                 "2142438
      ft-dynbegin = 'X'.                                    "2142438
      APPEND ft.                                            "2142438

    ENDIF.                                                  "2142438
  ENDIF.

*------- Kontierungblock Full Screen von VV, wenn Anzahlung -----------
  IF tfill_ftvk > 0
  AND (    xtbsl-koart = 'D'
       OR  xtbsl-koart = 'K' ).
    PERFORM dynpro_senden_vv_anzahlungen.
  ENDIF.

*------- Daten für abweich. Zahlungsempf. senden ? ---------------------
  IF  tfill_ftc   NE  0
  AND regul       EQ 'X'.
    PERFORM dynpro_senden_ftc.
*------- IBAN-Daten senden?
    IF tfill_ftiban NE 0.
      PERFORM iban_data.
* ------ Call mandate screens ------------------------------- "N1923657
      IF tfill_ftmndid NE 0.                                "N1923657
        PERFORM mandate_data.                               "N1923657
      ENDIF.                                                "N1923657
    ENDIF.
  ENDIF.

*------- Daten für Anlagen anteilige Ab/Zuschreibungen--------"KJV------
  IF  tfill_ftab  NE  0.                                      "KJV
    PERFORM dynpro_senden_ftab.                               "KJV
  ENDIF.                                                      "KJV


*------- Zusatzbild senden ? -------------------------------------------
  IF tfill_ftz NE 0.
    PERFORM dynpro_senden_ftz USING winnr.
    IF tfill_ftab NE 0.                                        "KJV
      PERFORM dynpro_senden_ftab.                              "KJV
    ENDIF.                                                     "KJV
  ENDIF.

*------- Vermögensverwaltungsbild senden ? -----------------------------
  IF  tfill_ftvv NE 0
  AND tfill_ftz  EQ 0
  AND tfill_ftc  EQ 0.
    PERFORM dynpro_senden_ftvv.
  ENDIF.

*------- Positionsdaten initialisieren ---------------------------------
  REFRESH: fta, ftc, ftf, ftz, ftk, ftvk, ftvv, ftcopa, ftiban.
  REFRESH: ftisis, ftps, ftw, ftab, ftsplt, ftsplt_wt, ft_generic_kontl.
  REFRESH: ftmndid.                                         "N1923657
  REFRESH: fttxt.                                           "2142438

  CLEAR:   anbwa, bschl, konto, umskz, regul, knrze.
  CLEAR:   tfill_ftc, tfill_ftz, tfill_ftcopa, tfill_ft_generic_kontl.
  CLEAR:   tfill_ftisis, tfill_ftw, tfill_ftab, ftsplt, ftsplt_wt.
  CLEAR:   tfill_ftiban.
  CLEAR:   tfill_ftmndid.                                   "N1923657
  CLEAR:   tfill_fttxt.                                     "2142438
  CLEAR:   g_wt_filled.                                     "2481799

ENDFORM.                    "position_uebertragen


*eject
*-----------------------------------------------------------------------
*        Form  RESET_CLEARING.
*-----------------------------------------------------------------------
*        Rücknahme eines Ausgleichs
*-----------------------------------------------------------------------
FORM reset_clearing USING p_no_auth TYPE c  ##CALLED..
  CLEAR ft.
  ft-program  = rep_name_r.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.
  IF NOT augbl IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'RF05R-AUGBL'.
    ft-fval     = augbl.
    APPEND ft.
  ENDIF.
  IF NOT bukrs IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'RF05R-BUKRS'.
    ft-fval     = bukrs.
    APPEND ft.
  ENDIF.
  IF NOT gjahr IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'RF05R-GJAHR'.
    ft-fval     = gjahr.
    APPEND ft.
  ENDIF.

  CLEAR ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '/11'.
  APPEND ft.

  PERFORM transaktion_beenden USING p_no_auth.
ENDFORM.                    "reset_clearing


*eject
*-----------------------------------------------------------------------
*        Form  REVERSE_DOCUMENT.
*-----------------------------------------------------------------------
*        Beleg stornieren
*-----------------------------------------------------------------------
FORM reverse_document USING p_no_auth ##CALLED.
  CLEAR ft.
  ft-program  = rep_name_a.
  ft-dynpro   = '0105'.
  ft-dynbegin = 'X'.
  APPEND ft.
  IF NOT belns IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'RF05A-BELNS'.
    ft-fval     = belns.
    APPEND ft.
  ENDIF.
  IF NOT bukrs IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'BKPF-BUKRS'.
    ft-fval     = bukrs.
    APPEND ft.
  ENDIF.
  IF NOT gjahs IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'RF05A-GJAHS'.
    ft-fval     = gjahs.
    APPEND ft.
  ENDIF.
  IF NOT budat IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'BSIS-BUDAT'.
    ft-fval     = budat.
    APPEND ft.
  ENDIF.
  IF NOT monat IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'BSIS-MONAT'.
    ft-fval     = monat.
    APPEND ft.
  ENDIF.

  IF NOT stgrd IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'UF05A-STGRD'.
    ft-fval     = stgrd.
    APPEND ft.
  ENDIF.

  IF NOT voidr IS INITIAL.
    CLEAR ft.
    ft-fnam     = 'RF05A-VOIDR'.
    ft-fval     = voidr.
    APPEND ft.
  ENDIF.

  CLEAR ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '/11'.
  APPEND ft.

  PERFORM transaktion_beenden USING p_no_auth.
ENDFORM.                    "reverse_document


*eject
*-----------------------------------------------------------------------
*        Form  TRANSAKTION_BEENDEN
*-----------------------------------------------------------------------
FORM transaktion_beenden USING p_no_auth TYPE c.
  DATA: local_funct LIKE rfipi-funct,
        tab_msg     LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        _subrc      LIKE sy-subrc,
        no_auth     TYPE c.
  DATA: wa_opt TYPE ctu_params.                             "N811562

  IF sgfunct IS INITIAL.
    local_funct = funct.
  ELSE.
    local_funct = sgfunct.
  ENDIF.

  CASE local_funct.
*------- Funktion: Batch-Input -----------------------------------------
    WHEN 'B'.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode     = tcode
        TABLES
          dynprotab = ft.

*------- Funktion: Call Transaction ... Using ... ----------------------
    WHEN 'C'.
      REFRESH xbltab.
      CLEAR xbltab.
      EXPORT xbltab TO MEMORY ID 'FI_XBLTAB'.

      IF p_no_auth = space.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = tcode
          EXCEPTIONS
            ok     = 0
            not_ok = 1
            OTHERS = 2.
      ENDIF.
      IF sy-subrc NE 0.
        IF xbdcc NE 'X'.
          MESSAGE e172(00) WITH tcode RAISING no_authorization.
        ELSE.
          MESSAGE s172(00) WITH tcode.
        ENDIF.
        no_auth = 'X'.
      ELSE.

* Bei IDOC Verarbeitung muss Sperre von der Datenbank genommen
* werden
        CALL FUNCTION 'IDOC_INVOIC_UNLOCK'.
        REFRESH tab_msg.
        wa_opt-dismode = mode.                              "N811562
        wa_opt-updmode = update.                            "N811562
        wa_opt-defsize = defsize.                           "N849676

* Starting with CE1802 we do not give the cloud user the authorization for FB01/FB05
* So, the calling report/method/... decides if the authority should be checked or not
* a) in the case the authority should be checked and the user does not have it, the processing
* is already stopped in the statement above (CALL FUNCTION 'AUTHORITY_CHECK_TCODE')
* b) in the case the authority should not be checked and the user does not have it, the
* Posting transaction (FB01/FB05,...) will be executed
* IMPORTANT: Until 20210414 There was no change in the behaviour for On Premise. But we got OP customer incidents
* like 206288/2021. The users were forced to have the FB01 authorization altough the calling report RFEBBU00
* calls the function module with p_no_auth = abap_true. So we decided to call the transactions always without
* an authority cgeck if the parameter p_no_auth was set. No matter if we are in cloud or not.
        IF p_no_auth = abap_false.
          "AND sy-uname <> 'SAP_WFRT'. "for unit test execution CL_FDC_UT_POST_GL_PRK_FBVB
          CALL TRANSACTION tcode USING  ft
                             OPTIONS FROM wa_opt            "N811562
                             MESSAGES INTO tab_msg.
        ELSE.
          CALL TRANSACTION tcode WITHOUT AUTHORITY-CHECK USING  ft
                             OPTIONS FROM wa_opt
                             MESSAGES INTO tab_msg.
        ENDIF.

      ENDIF.

      subrc = sy-subrc.
      IF no_auth EQ 'X'.
        msgty = 'E'.
      ELSE.
        msgty = sy-msgty.
      ENDIF.

* BOD CH00-
*        msgid = sy-msgid.
*        msgno = sy-msgno.
*        msgv1 = sy-msgv1.
*        msgv2 = sy-msgv2.
*        msgv3 = sy-msgv3.
*        msgv4 = sy-msgv4.
* EOD CH00-

* BOI CH00+
      IF tab_msg[] IS INITIAL OR
        lines( tab_msg[] ) = 1.
        msgid = sy-msgid.
        msgno = sy-msgno.
        msgv1 = sy-msgv1.
        msgv2 = sy-msgv2.
        msgv3 = sy-msgv3.
        msgv4 = sy-msgv4.
      ELSE.
        DELETE tab_msg WHERE msgid = '00' AND msgnr = '344' AND NOT (
          msgv1 = 'SAPMF05A' AND msgv2 = '0700' ).
        DATA(lv_lines) = lines( tab_msg[] ).
        READ TABLE tab_msg INDEX lv_lines
          INTO DATA(ls_msg).
        IF sy-subrc EQ 0.
          msgid = ls_msg-msgid.
          msgno = ls_msg-msgnr.
          msgty = ls_msg-msgtyp.
          msgv1 = ls_msg-msgv1.
          msgv2 = ls_msg-msgv2.
          msgv3 = ls_msg-msgv3.
          msgv4 = ls_msg-msgv4.
        ENDIF.
      ENDIF.
* EOI CH00+

*                                                " beg_ins " n. 2994074
*     Send warnings from CO availability control
*     when saving FI documents as completed.
      IF cl_fin_flags=>get_flag_stat(
            iv_appl = 'IM'
            iv_sub  = 'IM_AVC'
            iv_para = 'SEND_AVC_SAVE_AS_COMPL_WARNING' ) <> space.

        DATA: ld_save_subrc LIKE sy-subrc.
        DATA: tab_bp_warning LIKE TABLE OF bdcmsgcoll
                             WITH HEADER LINE ##FM_SUBRC_OK.
*       Save SUBRC.
*        ld_save_subrc = sy-subrc.
*       Warnings from availability control occured?
        LOOP AT tab_msg INTO tab_bp_warning
          WHERE msgtyp = 'W'
          AND   msgid  = 'BP'.
          APPEND tab_bp_warning.
        ENDLOOP.
*       Yes, then export to memory.
        FREE MEMORY ID 'IM_TAB_BP_WARNING'.
        IF NOT tab_bp_warning[] IS INITIAL.
          EXPORT tab_bp_warning[] TO MEMORY
            ID 'IM_TAB_BP_WARNING'.
        ENDIF.
*       Restore SUBRC.
        sy-subrc = ld_save_subrc.

      ENDIF.
*                                                " end_ins " n. 2994074
*
*                                                " beg_ins " n. 3151913
*     Send warnings from PSM availability control
*     when saving FI documents as completed:
      IF cl_fin_flags=>get_flag_stat(
           iv_appl = 'PSM'
           iv_sub  = 'AVC_OP'
           iv_para = 'SEND_AVC_SAVE_AS_COMPL_WARNING' ) <> space.

        DATA: ld_save_subrc_psm  LIKE sy-subrc,
              lt_psm_avc_warning LIKE TABLE OF bdcmsgcoll
                                 WITH HEADER LINE.
*       Save SUBRC:
*        ld_save_subrc_psm = sy-subrc.
*        IMPORT lt_psm_avc_warning[] FROM MEMORY
*            ID 'PSM_AVC_WARNING_TAB'.

*       Warnings from PSM availability control occurred?
        LOOP AT tab_msg INTO lt_psm_avc_warning
          WHERE ( msgtyp = 'W' )
            AND ( msgid  = 'FMAVC' OR
                  msgid  = 'FMCE' OR
                  msgid  = 'GMAVC' ).
          APPEND lt_psm_avc_warning.
        ENDLOOP.
*       Yes, then export to memory:
        FREE MEMORY ID 'PSM_AVC_WARNING_TAB'.
        IF NOT lt_psm_avc_warning[] IS INITIAL.
          EXPORT lt_psm_avc_warning[] TO MEMORY
              ID 'PSM_AVC_WARNING_TAB'.
        ENDIF.
*       Restore SUBRC:
        sy-subrc = ld_save_subrc_psm.

      ENDIF.
*                                                " end_ins " n. 3151913
*
* overwrite msg-fields when error occured
      IF sy-subrc NE 0 AND no_auth NE 'X'.
        _subrc = sy-subrc.
        READ TABLE tab_msg WITH KEY msgtyp = 'E'.
        IF sy-subrc EQ 0.
          msgty = tab_msg-msgtyp.
          msgid = tab_msg-msgid.
          msgno = tab_msg-msgnr.
          msgv1 = tab_msg-msgv1.
          msgv2 = tab_msg-msgv2.
          msgv3 = tab_msg-msgv3.
          msgv4 = tab_msg-msgv4.
        ENDIF.
        sy-subrc = _subrc.
      ENDIF.

      IF sy-subrc = 0.
        IMPORT xbltab FROM MEMORY ID 'FI_XBLTAB'.
      ENDIF.

*------- bei Fehlern und XBDCC = 'X': Batch Input erzeugen
      IF subrc NE 0 AND xbdcc = 'X'.
        IF group_open NE 'X'.
          PERFORM mappe_oeffnen.
        ENDIF.
        CALL FUNCTION 'BDC_INSERT'
          EXPORTING
            tcode     = tcode
          TABLES
            dynprotab = ft
          EXCEPTIONS
            OTHERS    = 1.
        IF sy-subrc = 0.
          MESSAGE s008(fb) WITH '' group.
        ENDIF.
      ENDIF.


*------- Funktion: Interaktive Buchungsschnittstelle
    WHEN 'I'.
  ENDCASE.
ENDFORM.                    "transaktion_beenden

*eject
*-----------------------------------------------------------------------
*        Form  WECHSELDATEN_FRANKREICH
*-----------------------------------------------------------------------
*        Auf dem Wechselbild für Frankreich (2320) werden einige Daten
*        in BSEC-Feldern übergeben. Diese BSEC-Daten müssen nicht
*        auf das CpD-Bild, sondern das Wechselbild übergeben werden.
*-----------------------------------------------------------------------
FORM wechseldaten_frankreich.
  CHECK tfill_ftc NE 0.

  IF xtbsl-xsonu NE space.
    SELECT SINGLE * FROM  t074u
        WHERE  koart       = xtbsl-koart
        AND    umskz       = umskz.
  ENDIF.

  IF t074u-umsks = 'W'.
    LOOP AT ftc WHERE fnam = 'BSEC-NAME1'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT ftc.
        fta = ftc.
        APPEND fta.
      ENDLOOP.
      REFRESH ftc.
      CLEAR tfill_ftc.
    ENDIF.
  ENDIF.
ENDFORM.                    "wechseldaten_frankreich

*eject
*-----------------------------------------------------------------------
*        Form  XFTPOST_ANALYSIEREN
*-----------------------------------------------------------------------
*        Buchungsdaten aus FTPOST (Feldname,Feldwert) analysieren:
*        Daten werden aus der Schnittstellentabelle FTPOST in
*        dynprobezogenen Feldtabellen gesammelt.
*        - Kopfdaten:            direkt in Tabelle FT stellen
*        - Fußzeiledaten:        in FTF       sammeln
*        - Fußzeiledaten (Konto) in FTFKTO    sammeln
*        - Standarddynpro:       in FTA       sammeln
*        - CpD-Daten:            in FTC       sammeln
*        - Zusatzdaten:          in FTZ       sammeln
*        - Kontierungsblockdaten in FTK       sammeln
*        - Vermögensverwaltung   in FTV       sammeln
*        - VV-Kont.Blockdaten    in FTVK      sammeln
*        - COPA-DATEN            in FTCOPA    sammeln
*        - Quellensteuerdaten    in FTW       sammeln
*        - Betragssplitt         in FTSPLT    sammeln
*        - Betragssplitt WT      in FTSPLT_WT sammeln

*-----------------------------------------------------------------------
FORM xftpost_analysieren.

*------- Daten von ISIS (insurance)
  STATICS: s_vlvz_in_bbseg TYPE c.
  DATA: lv_length TYPE i.
  DATA: ls_ftpost TYPE ftpost.

  CONSTANTS:
    lc_fnam_glo   TYPE ftpost-fnam VALUE 'BSEC-GLO_RE1_OT',     "Note 2939337
    lc_fnam_bankn TYPE ftpost-fnam VALUE 'BSEC-BANKN',          "Note 2949101
    lc_fnam_bankl TYPE ftpost-fnam VALUE 'BSEC-BANKL',          "Note 2949101
    lc_fnam_banks TYPE ftpost-fnam VALUE 'BSEC-BANKS',          "Note 2949101
    lc_fnam_bkont TYPE ftpost-fnam VALUE 'BSEC-BKONT',          "Note 2949101
    lc_fnam_bkref TYPE ftpost-fnam VALUE 'BSEC-BKREF',          "Note 2949101
    lc_tcode      TYPE sy-tcode    VALUE 'FBVB',                "Note 2949101
    lc_waers_chf  TYPE bkpf-waers  VALUE 'CHF',                 "Note 3045169
    lc_waers_eur  TYPE bkpf-waers  VALUE 'EUR'.                 "Note 3045169
  DATA: ls_xftpost TYPE ftpost.
*        lv_qr_act  TYPE abap_bool.                               "Note 2939337
  CONSTANTS:
    lc_spras_fnam TYPE ftpost-fnam  VALUE 'BSEC-SPRAS'.           "Note 3139261
  DATA: language_delete TYPE abap_bool.                           "Note 3139261

*----- Falls BKPF-XPRFG gefuellt ist, Flag setzen, das sicherstellt,
*      dass okcode /17 gesendet wird.
  IF xftpost-fnam   = 'BKPF-XPRFG' AND
     xftpost-fval   NE space       AND
     tcode          = 'FBV1'.
    send_ok17      = 'X'.
    EXIT.
  ENDIF.

*------- VBUND eingabe auf Kopfebene-----------------------------------
*        verarbeiten bei AT NEW SYTPE
  IF xftpost-fnam    = 'BKPF-VBUND'
  OR xftpost-fnam    = 'RF014-VBUND'.
    vbund = xftpost-fval.
    EXIT.
  ENDIF.

*------- Kopfdaten übertragen (direkt in Tabelle FT stellen) -----------
  IF xftpost-fnam(4) = 'BKPF'
  OR xftpost-fnam    = 'RF05A-TAX_COUNTRY'
  OR xftpost-fnam    = 'RF05A-AUGTX'   "neues Feld Ausgl.-text 45A-HP
  OR xftpost-fnam    = 'RF05A-PARGB'   "jetzt COBL
  OR xftpost-fnam    = 'VBKPF-PARGB'   "Belegvorerfassung
  OR xftpost-fnam    = 'VBKPF-XBWAE'   "Belegvorerfassung
  OR xftpost-fnam    = 'FS006-DOCID'   "BarCodeübernahme
  OR xftpost-fnam    = 'FS006-BARCD'   "BarCodeübernahme
  OR xftpost-fnam    = 'RF05A-PORTF'.                   "CH00+
    IF xftpost-fnam  = 'BKPF-XMWST'
    AND NOT xftpost-fval  IS INITIAL.
      xmwst = 'X'.
*     XMWST muß in FTA damit es auf nächstes Hauptdynpro muß
      CLEAR fta.
      MOVE-CORRESPONDING xftpost TO fta.
      APPEND fta.
      EXIT.
    ENDIF.
    IF xftpost-fnam = 'BKPF-BUKRS'.
      bukrs = xftpost-fval.
      bkpf_bukrs = bukrs.                                  "Note 641889
    ENDIF.
    IF xftpost-fnam = 'RF05A-TAX_COUNTRY'.
      gv_tax_country = xftpost-fval.
    ENDIF.
    IF xftpost-fnam = 'BKPF-WAERS'.
      waers = xftpost-fval.
    ENDIF.
    IF xftpost-fnam = 'BKPF-BLART'.
      blart = xftpost-fval.
    ENDIF.
    IF xftpost-fnam = 'BKPF-BUDAT'.
      budat = xftpost-fval.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = xftpost-fval
        IMPORTING
          date_internal            = budat_wt
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2 ##FM_SUBRC_OK.
    ENDIF.
    CLEAR ft.
    IF xftpost-fnam = 'BKPF-CUP' OR                         "2142438
       xftpost-fnam = 'BKPF-CIG'.                           "2142438
      MOVE-CORRESPONDING xftpost TO fttxt.                  "2142438
      APPEND fttxt.                                         "2142438
      EXIT.                                                 "2142438
    ENDIF.                                                  "2142438
*   BKPF-FULFILLDATE is input field on Tax Screen move to xfttax
    IF xftpost-fnam = 'BKPF-FULFILLDATE'.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = xftpost-fval
        IMPORTING
          date_internal            = xfttax-fulfilldate
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2  ##FM_SUBRC_OK.
      IF tfill_xfttax IS INITIAL.
        APPEND xfttax.
        DESCRIBE TABLE xfttax LINES tfill_xfttax.
      ELSE.
        MODIFY xfttax INDEX 1 TRANSPORTING fulfilldate.
      ENDIF.
      EXIT.
    ENDIF.
    MOVE-CORRESPONDING xftpost TO ft.
    APPEND ft.
    index = index + 1.
    EXIT.
  ENDIF.

*------- Fußzeiledaten analysieren / merken (FTF) ohne Konto -----------
  IF xftpost-fnam = 'BSEG-BSCHL'
  OR xftpost-fnam = 'BSEG-UMSKZ'
  OR xftpost-fnam = 'BSEG-ANBWA'
  OR xftpost-fnam = 'BSEG-BUKRS'

  OR xftpost-fnam = 'RF05A-NEWBS'
  OR xftpost-fnam = 'RF05A-NEWUM'
  OR xftpost-fnam = 'RF05A-NEWBW'
  OR xftpost-fnam = 'RF05A-NEWBK'

  OR xftpost-fnam = 'RF05V-NEWBS'
  OR xftpost-fnam = 'RF05V-NEWUM'
  OR xftpost-fnam = 'RF05V-NEWBK'
  OR xftpost-fnam = 'RF05V-NEWBW'.                                 .

*------- Fußzeiledaten in Hilfsfeldern speichern / Feldname anpassen ---
    IF xftpost-fnam = 'BSEG-BUKRS'
    OR xftpost-fnam = 'RF05A-NEWBK'
    OR xftpost-fnam = 'RF05V-NEWBK'.
      IF rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWBK'.
      ELSE.
        xftpost-fnam = 'RF05A-NEWBK'.
      ENDIF.
      bukrs       = xftpost-fval.
    ENDIF.

    IF xftpost-fnam = 'BSEG-BSCHL'
    OR xftpost-fnam = 'RF05A-NEWBS'
    OR xftpost-fnam = 'RF05V-NEWBS'.
      IF rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWBS'.
      ELSE.
        xftpost-fnam = 'RF05A-NEWBS'.
      ENDIF.
      bschl       = xftpost-fval.
    ENDIF.


    IF xftpost-fnam = 'BSEG-UMSKZ'
    OR xftpost-fnam = 'RF05A-NEWUM'
    OR xftpost-fnam = 'RF05V-NEWUM'.
      IF rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWUM'.
      ELSE.
        xftpost-fnam = 'RF05A-NEWUM'.
      ENDIF.
      umskz       = xftpost-fval.
    ENDIF.

    IF xftpost-fnam = 'BSEG-ANBWA'
    OR xftpost-fnam = 'RF05A-NEWBW'
    OR xftpost-fnam = 'RF05V-NEWBW'.
      IF rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWBW'.
      ELSE.
        xftpost-fnam = 'RF05A-NEWBW'.
      ENDIF.
      anbwa       = xftpost-fval.
    ENDIF.

    CLEAR ftf.
    MOVE-CORRESPONDING xftpost TO ftf.
    APPEND ftf.
    EXIT.

  ENDIF.

*------- Fußzeiledaten analysieren / merken (FTF) nur  Konto ---------
*------- Spezialbehandlung Konto                      -----------------
  IF xftpost-fnam = 'BSEG-KONTO'
  OR xftpost-fnam = 'BSEG-KUNNR'
  OR xftpost-fnam = 'BSEG-LIFNR'
  OR xftpost-fnam = 'BSEG-HKONT'
  OR xftpost-fnam = 'RF05A-NEWKO'
  OR xftpost-fnam = 'RF05V-NEWKO'.
    gv_supplier_account = xftpost-fval.       "Note 3139261
    CLEAR ftfkto.
    MOVE-CORRESPONDING xftpost TO ftfkto.
    APPEND ftfkto.
    EXIT.
  ENDIF.

* Changes for QR bill
* Required to process dynpro for parked document
* Check if QR is active for company code
  IF gv_bukrs NE bukrs.
    TRY.
        CALL FUNCTION 'IDFI_QRIBAN_QR_ACT'
          EXPORTING
            iv_bukrs  = bukrs
          IMPORTING
            ev_qr_act = gv_qr_act.
        gv_bukrs  = bukrs.
      CATCH cx_root.
    ENDTRY.
  ENDIF.
  IF gv_qr_act  EQ abap_true.
    IF xftpost-fnam = 'BSEU-VOZEI'.
      CLEAR: lineitem_number.
      lineitem_number = xftpost-fval.
    ENDIF.
    IF xftpost-fnam = 'BKPF-BELNR'.
      CLEAR: document_number.
      document_number = xftpost-fval.
    ENDIF.
  ENDIF.
* End of changes for QR bill

*------- CpD-Daten analysieren / merken (FTC); Feldname anpassen -------
  IF xftpost-fnam(4) = 'BSEC'
  OR xftpost-fnam    = 'BSEG-STCEG'.
*   Changes for Note 2939337
*   If Additional Information with QR Bill is passed then map to
*   Screen field of FB02 instead of CPD screen
* Start of changes for Note 2949101
* If QR is active then check for the company before reading data
    IF gv_qr_act              EQ abap_true    AND    "Note 3045169
       ( waers   EQ lc_waers_chf    OR                  "Note 3045169
         waers   EQ lc_waers_eur ).                     "Note 3045169
      IF xftpost-fnam   = lc_fnam_glo.
        MOVE-CORRESPONDING xftpost TO ls_xftpost.
*         Move BDCDATA and call the IDFI function to fill
        TRY.
            CALL FUNCTION 'IDFI_QRIBAN_IDOC_INVOIC_FTA'
              EXPORTING
                is_xftpost = ls_xftpost
                iv_tcode   = i_tcode
              CHANGING
                ct_fta     = fta[]
                ct_ftc     = ftc[].
          CATCH cx_root.
        ENDTRY.
        EXIT.
      ENDIF.
*     End of changes for Note 2939337
*     Start of changes for Note 2949101
*     If bank details exist then
      IF i_tcode    EQ lc_tcode.
        IF xftpost-fnam = lc_fnam_bankn.
          gv_bankn    = xftpost-fval.
        ENDIF.
        IF xftpost-fnam = lc_fnam_bankl.
          gv_bankl    = xftpost-fval.
        ENDIF.
        IF xftpost-fnam = lc_fnam_banks.
          gv_banks    = xftpost-fval.
        ENDIF.
        IF xftpost-fnam = lc_fnam_bkont.
          gv_bkont    = xftpost-fval.
        ENDIF.
        IF xftpost-fnam = lc_fnam_bkref.
          gv_bkref    = xftpost-fval.
        ENDIF.
      ENDIF.
    ENDIF.
*   End of changes for Note 2949101

    IF xftpost-fnam = 'BSEC-STCEG'.
      xftpost-fnam = 'BSEG-STCEG'.
    ENDIF.
    CLEAR ftc.
    MOVE-CORRESPONDING xftpost TO ftc.
    APPEND ftc.
*   Changes for Note 3139261
*   Check if the field name is SPRAS
*   Check the fieldstatus of the supplier account group
*   Remove it from the structure
    IF xftpost-fnam = lc_spras_fnam.
      IF gv_qr_act  EQ abap_true        AND
         i_tcode    EQ lc_tcode         AND
         ( waers    EQ lc_waers_chf     OR
           waers    EQ lc_waers_eur ).
        TRY.
            CALL FUNCTION 'IDFI_BDC_SPRAS_CHECK'
              EXPORTING
                language_field   = lc_spras_fnam
                supplier_account = gv_supplier_account
                company_code     = bukrs
              IMPORTING
                language_delete  = language_delete
              CHANGING
                ct_xftpost       = xftpost[]
                ct_ftc           = ftc[].
            IF language_delete  EQ abap_true.
              CLEAR: xftpost.
            ENDIF.
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDIF.
*   End of changes for Note 3139261
    EXIT.
  ENDIF.

*------- IBAN Daten merken (FTIBAN).
  IF xftpost-fnam(5) = 'TIBAN'.
    gv_iban = xftpost-fval.
    CLEAR ftiban.
    MOVE-CORRESPONDING xftpost TO ftiban.
    APPEND ftiban.
    EXIT.
  ENDIF.
* Fill QRIBAN screen field
* Get the debit/credit indicator
* If indicator is S then no need to fill QRIBAN
  IF gv_qr_act        EQ abap_true  AND
     lineitem_number  GT 0.
    IF gv_qr_act              EQ abap_true  AND
       gv_bankl               IS NOT INITIAL  AND
       gv_banks               IS NOT INITIAL  AND
       ( gv_bankn             IS NOT INITIAL  OR
         gv_iban              IS NOT INITIAL ).
      READ TABLE xftpost WITH KEY fnam = 'BKPF-BELNR'
                        INTO ls_ftpost
                        TRANSPORTING ALL FIELDS.
      document_number = ls_ftpost-fval.
      TRY.
          CALL FUNCTION 'IDFI_QRIBAN_BDC_GET_INDICATOR'
            EXPORTING
              lineitem_number = lineitem_number
              company_code    = bkpf_bukrs
              document_number = document_number
              posting_date    = budat_wt
            IMPORTING
              result          = debit_credit_indicator.
        CATCH cx_root.
      ENDTRY.
*     If all bank details exist then fill the field for QR-IBAN
*     Changes for QR-Bill and to add QRIBAN value
      IF debit_credit_indicator EQ 'H'.
        TRY.
            CALL FUNCTION 'IDFI_BDC_QRIBAN_FILL'
              EXPORTING
                iv_bankn = gv_bankn
                iv_bankl = gv_bankl
                iv_banks = gv_banks
                iv_bkont = gv_bkont
                iv_bkref = gv_bkref
                iv_bukrs = gv_bukrs
                iv_iban  = gv_iban
                iv_tcode = i_tcode
              CHANGING
                ct_fta   = fta[].
          CATCH cx_root.
        ENDTRY.
*       Clear bank details to prevent calling of FM again
*       Also prevent retention of value for multiple document processing
        CLEAR: gv_banks,
               gv_bankn,
               gv_bankl,
               gv_iban.
      ENDIF.
    ENDIF.
  ENDIF.

*------- Mandate (FTMNDID).  --------------------------------- "N1923657
  IF xftpost-fnam(10) = 'BSEG-MNDID' OR
     xftpost-fnam = 'RFSEPA_INTKEY-MNDID'.                  "2014651
    IF xftpost-fnam = 'RFSEPA_INTKEY-MNDID'.                "2014651
      xftpost-fnam = 'BSEG-MNDID'.                          "2014651
    ENDIF.                                                  "2014651
    CLEAR ftmndid.
    MOVE-CORRESPONDING xftpost TO ftmndid.
    APPEND ftmndid.
    EXIT.
  ENDIF.
*------- Daten vom Kontierungsblock analysieren / merken (FTK) ---------
  IF xftpost-fnam(4) = 'COBL'.
    CLEAR ftk.
    MOVE-CORRESPONDING xftpost TO ftk.
    APPEND ftk.
    EXIT.
  ENDIF.

*------- Daten von Vermögensverwaltung analysieren / merken (FTVV, FTVK)
*begin of insertion note 1115584
  IF xftpost-fnam(5) = 'LFVI9'
  OR xftpost-fnam(21) = 'REIT_TAX_CORRECTION_S'.
*end of insertion note 1115584
*     Felder für Kontierungsblockdynpro der Vermögensverwaltung
*     ( SAPLFVI8 0100) in Tab FTVK sammeln
    CLEAR ftvk.
    MOVE-CORRESPONDING xftpost TO ftvk.
    APPEND ftvk.
    EXIT.
  ENDIF.

*------- Daten von COPA merken für die Übergabe an FB
  lv_length = numofchar( xftpost-fnam ) - 4 .
  IF lv_length GT 0.                                        "3254447
    IF xftpost-fnam(9) = 'BSEG-RKE_' OR
     xftpost-fnam+lv_length(4) = '_MSE'.
      CLEAR ftcopa.
      MOVE-CORRESPONDING xftpost TO ftcopa.
      APPEND ftcopa.
      EXIT.
    ENDIF.
  ENDIF.                                                    "3254447

*------- Daten von ISIS (insurance) merken für die Übergabe an FB
  IF s_vlvz_in_bbseg IS INITIAL.
    PERFORM check_bbseg_for_vzk CHANGING s_vlvz_in_bbseg.
    TRANSLATE s_vlvz_in_bbseg USING ' O'.
  ENDIF.

  IF s_vlvz_in_bbseg = 'X'.
    lv_length = numofchar( xftpost-fnam ) - 5 .
    IF xftpost-fnam(10) = 'BSEG-ISCD_' OR
       ( xftpost-fnam(5) = 'BSEG-' AND xftpost-fnam+lv_length(5) = '_ISCD' ).
      CLEAR ftisis.
      MOVE-CORRESPONDING xftpost TO ftisis.
      APPEND ftisis.
      EXIT.
    ENDIF.
  ENDIF.

* ----------------------------------------------------------------------
* Note 499049: Support Posting of parked documents of
* other compoments (IS Insurance etc: ) which
* use field VBSEG-KONTL (KONTT) for a generic storage of their
* accounting assignments.
* ----------------------------------------------------------------------
  IF xftpost-fnam = 'BSEG-KONTL' OR xftpost-fnam = 'BSEG-KONTT'.
    CLEAR ft_generic_kontl.
    MOVE-CORRESPONDING xftpost TO ft_generic_kontl.
    APPEND ft_generic_kontl.
    EXIT.
  ENDIF.

*------- Extended Withholding tax (FTW) --------------------------------
  IF xftpost-fnam(4) = 'WITH'.
    CALL FUNCTION 'FI_CHECK_EXTENDED_WT'
      EXPORTING
        i_bukrs              = bukrs
      EXCEPTIONS
        component_not_active = 1
        not_found            = 2
        OTHERS               = 3.
    IF sy-subrc = 0.
      CALL FUNCTION 'FI_WT_FIPI_FILL_FTW_TAB'
        EXPORTING
          i_ftpost = xftpost
        TABLES
          i_ftw    = ftw
        EXCEPTIONS
          OTHERS   = 0.
      EXIT.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

*------- Daten für Betragssplitt (FTSPLT) ---------
  IF xftpost-fnam(6) = 'ACSPLT'.
    CLEAR ftsplt.
    MOVE-CORRESPONDING xftpost TO ftsplt.
    APPEND ftsplt.
    EXIT.
  ENDIF.

  IF xftpost-fnam(9) = 'ACWT_ITEM'.
    CLEAR ftsplt_wt.
    MOVE-CORRESPONDING xftpost TO ftsplt_wt.
    APPEND ftsplt_wt.
    EXIT.
  ENDIF.

*------- Zusatzdaten analysieren / merken (FTZ) ------------------------
*-----------------------------------------------------------------------
*        Neue Zusatzdaten-Felder müssen hier ebenfalls
*        aufgenommen werden !!!
*-----------------------------------------------------------------------

  IF xftpost-fnam    = 'BSEG-DMBE2'
  OR xftpost-fnam    = 'BSEG-DMBE3'
  OR xftpost-fnam    = 'BSEG-ZOLLT'
  OR xftpost-fnam    = 'BSEG-EGRUP'
  OR xftpost-fnam    = 'BSEG-BTYPE'
  OR xftpost-fnam    = 'BSEG-VNAME'
  OR xftpost-fnam    = 'BSEG-RECID'                         "< 3106641
* OR XFTPOST-FNAM    = 'BSEG-FIPOS'                           "30F
* OR XFTPOST-FNAM    = 'BSEG-GEBER'                           "30F
* OR XFTPOST-FNAM    = 'BSEG-FISTL'                           "30F
  OR xftpost-fnam    = 'BSEG-VBUND'
  OR xftpost-fnam    = 'BSEG-ABPER'
  OR xftpost-fnam    = 'BSEG-GBETR'
  OR xftpost-fnam    = 'BSEG-KURSR'
  OR xftpost-fnam    = 'BSEG-RSTGR'                         "30F

  OR xftpost-fnam    = 'BSEG-MANSP'
  OR xftpost-fnam    = 'BSEG-MSCHL'
  OR xftpost-fnam    = 'BSEG-HBKID'
  OR xftpost-fnam    = 'BSEG-HKTID'                                 "RE
  OR xftpost-fnam    = 'BSEG-BVTYP'
*  OR xftpost-fnam    = 'BSEG-MNDID'                           "N1888262
  OR xftpost-fnam    = 'BSEZ-EGMLD'
  OR xftpost-fnam    = '*KNA1-KUNNR'   "Warenempfg. EG-Dreiecksgeschaeft
  OR xftpost-fnam    = 'BSEG-EGMLD'
* OR XFTPOST-FNAM    = 'BSEG-EGBLD'
  OR xftpost-fnam    = 'BSEG-XEGDR'
  OR xftpost-fnam    = 'BSEZ-XEGDR_HU'                     "Note1009677
  OR xftpost-fnam    = 'BSEG-ANFBN'
  OR xftpost-fnam    = 'BSEG-ANFBU'
  OR xftpost-fnam    = 'BSEG-ANFBJ'
  OR xftpost-fnam    = 'BSEG-LZBKZ'
  OR xftpost-fnam    = 'BSEG-LANDL'
  OR xftpost-fnam    = 'BSEG-DIEKZ'
  OR xftpost-fnam    = 'BSEG-ZOLLD'
  OR xftpost-fnam    = 'BSEG-ZOLLT'
  OR xftpost-fnam    = 'BSEG-FDTAG'
  OR xftpost-fnam    = 'BSEG-VRSDT'
  OR xftpost-fnam    = 'BSEG-FDLEV'
  OR xftpost-fnam    = 'BSEG-VRSKZ'
  OR xftpost-fnam    = 'BSEG-ZINKZ'
  OR xftpost-fnam    = 'BSEG-HZUON'
  OR xftpost-fnam    = 'BSEG-XREF1'
  OR xftpost-fnam    = 'BSEG-XREF2'
  OR xftpost-fnam    = 'BSEG-CCBTC'
  OR xftpost-fnam    = 'BSEG-XNEGP'    "4.0
  OR xftpost-fnam    = 'BSEG-IDXSP'    "4.0
  OR xftpost-fnam    = 'BSEG-KKBER'    "4.0
  OR xftpost-fnam    = 'BSEG-XREF3'    "4.0
  OR xftpost-fnam    = 'BSEG-DTWS1'    "4.0
  OR xftpost-fnam    = 'BSEG-DTWS2'    "4.0
  OR xftpost-fnam    = 'BSEG-DTWS3'    "4.0
  OR xftpost-fnam    = 'BSEG-DTWS4'    "4.0
  OR xftpost-fnam    = 'BSEG-BLNBT'                         "4.0C
  OR xftpost-fnam    = 'BSEG-BLNPZ'                         "4.0C
  OR xftpost-fnam    = 'BSEG-BLNKZ'                         "4.0C
  OR xftpost-fnam    = 'BSEG-CESSION_KZ'
  OR xftpost-fnam    = 'BSEG-BEWAR'                        "note 658991
*  or xftpost-fnam    = 'BSEG-FKBER_LONG'                  "note 607502
*  or xftpost-fnam    = 'BSEG-GRANT_NBR'                   "note 607502
  OR xftpost-fnam    = 'BSEG-PENRC'     "PromptPaymentAct (Note 571833)
  OR xftpost-fnam    = 'BSEG-J_1TPBUPL'                     "N2120000
  OR xftpost-fnam    = 'BSEG-HSN_SAC'    "GST note: 2654071
  OR xftpost-fnam    = 'BSEG-PLC_SUP'    "GST note: 2654071
  OR xftpost-fnam    = 'BSEG-GST_PART'   "GST note: 2654071
  OR xftpost-fnam    = 'BSEG-IRN'
  OR xftpost-fnam    = 'BSEG-INWARD_NO'  "KSA note: 2699500
  OR xftpost-fnam    = 'BSEG-INWARD_DT'  "KSA note: 2699500
  OR xftpost-fnam    = 'BSEG-GROUND_NO'  "KSA note: 2699500
  OR xftpost-fnam    = 'BSEG-GROUND_DT'  "KSA note: 2699500
  OR xftpost-fnam    = 'BSEG-GROUND_TYP' "KSA note: 2699500
  OR xftpost-fnam    = 'BSEG-VALOBJTYPE'
  OR xftpost-fnam    = 'BSEG-VALOBJ_ID'
  OR xftpost-fnam    = 'BSEG-VALSOBJ_ID'.

    IF xftpost-fnam = 'BSEG-EGMLD'.
      xftpost-fnam = 'BSEZ-EGMLD'.
    ENDIF.
    CLEAR ftz.
    MOVE-CORRESPONDING xftpost TO ftz.
    APPEND ftz.
    EXIT.
  ENDIF.

*------- Funds Management Felder parken in FTFM --------------------
  IF xftpost-fnam    = 'BSEG-FIPOS'                         "30F
  OR xftpost-fnam    = 'BSEG-GEBER'                         "30F
* or xftpost-fnam    = 'BSEG-FISTL'.                       "note 607502
  OR xftpost-fnam    = 'BSEG-FISTL'                        "note 607502
  OR xftpost-fnam    = 'BSEG-FKBER_LONG'                   "note 607502
  OR xftpost-fnam    = 'BSEG-GRANT_NBR'                    "note 607502
  OR xftpost-fnam    = 'BSEG-KBLNR'                        "note 607502
  OR xftpost-fnam    = 'BSEG-KBLPOS'                       "note 607502
  OR xftpost-fnam    = 'BSEZ-ERLKZ'                        "note 607502
  OR xftpost-fnam    = 'BSEG-ERLKZ'.                       "note 607502
    IF xftpost-fnam = 'BSEG-ERLKZ'.                        "note 607502
      xftpost-fnam = 'BSEZ-ERLKZ'.                         "note 607502
    ENDIF.                                                 "note 607502
    CLEAR ftps.                                             "30F
    MOVE-CORRESPONDING xftpost TO ftps.                     "30F
    APPEND ftps.                                            "30F
    EXIT.                                                   "30F
  ENDIF.                                                    "30F

*------- Abweichende Zentrale  ---------------------------------------
  IF xftpost-fnam = 'BSEG-KNRZE'.
    knrze = xftpost-fval.
    EXIT.
  ENDIF.

*------- Daten für AfA-Bereiche Anteilswerte-------------------"KJV----
  IF xftpost-fnam(4) = 'ANEA'.                                 "KJV
    MOVE-CORRESPONDING xftpost TO ftab.                        "KJV
    APPEND ftab.                                               "KJV
    EXIT.                                                      "KJV
  ENDIF.                                                       "KJV

*------- Daten für Standardbilder (FTA) --------------------------------
*------- alle bisher noch nicht verarbeiteten Felder -------------------
  IF xftpost-fnam = 'BSEG-REGUL'
  OR xftpost-fnam = 'RF05A-REGUL'
  OR xftpost-fnam = 'RF05V-REGUL'.
    IF rep_name = rep_name_bv.
      xftpost-fnam = 'RF05V-REGUL'.
    ELSE.
      xftpost-fnam = 'RF05A-REGUL'.
    ENDIF.
    regul        = xftpost-fval.
  ENDIF.
  CLEAR fta.
  MOVE-CORRESPONDING xftpost TO fta.
  APPEND fta.
ENDFORM.                    "xftpost_analysieren


*eject
*-----------------------------------------------------------------------
*        Form  XFTPOST_LOOP
*-----------------------------------------------------------------------
*        Buchungsdatentabelle abarbeiten
*-----------------------------------------------------------------------
FORM xftpost_loop.

  LOOP AT xftpost.
    AT FIRST.
      CLEAR index.
      PERFORM dynpro_setzen_einstieg.
    ENDAT.

*------- Gruppenwechsel Satztyp (K=Kopfsatz, P=Position) ---------------
    AT NEW stype.
      IF xftpost-stype CN 'KP'.
        MESSAGE e007 WITH xftpost-stype RAISING record_type_invalid ##FM_RAISE_OK.
      ENDIF.
      IF NOT vbund IS INITIAL.
*       Aufruf beim ersten mal unterdrücken
        PERFORM vbund_auf_kopf_uebertragen.
      ENDIF.
    ENDAT.

*------- Gruppenwechsel Satzzähler -------------------------------------
    AT NEW count.
      IF xftpost-stype = 'P'.
        IF tcode NE 'FBVB' AND xftpost-count > 950.
          MESSAGE e023 WITH '950' RAISING too_many_line_items.
        ELSEIF tcode EQ 'FBVB' AND xftpost-count > 999.
          MESSAGE e023 WITH '999' RAISING too_many_line_items.
        ENDIF.
        IF xftpost-count > 1.
          PERFORM position_uebertragen.
        ENDIF.
      ENDIF.
    ENDAT.

*------- FTPOST-Daten in interne Tabellen übertragen/sortieren ------
* check commented out because program SAPF103 generates BI sessions where the tax date is entered
* on line item level, for example when posting to UMSKZ = A (down payment): On screen SAPMF05A 304
* the tax date is available:
    CHECK "xftpost-fnam NE 'BSEG-TXDAT' AND   "needed for program SAPF103
           xftpost-fnam NE 'BSEG-TXDAT_FROM'.
    PERFORM xftpost_analysieren.
  ENDLOOP.

ENDFORM.                    "xftpost_loop

*&---------------------------------------------------------------------*
*&      Form  DYNPRO_SENDEN_VV_ANZAHLUNGEN
*&---------------------------------------------------------------------*
*       Das Dynpro wird bei Anzahlungen auf Immobilienobjekte          *
*       gesendet. (Vermögensverwaltung Kontierungsblock Fullscreen)    *
*----------------------------------------------------------------------*
FORM dynpro_senden_vv_anzahlungen.

* 'Weiter-Flag' setzen für RE               "MPR
  CLEAR ft.                                                 "MPR
  ft-fnam     = 'RF05A-XIMKO'.                              "MPR
  ft-fval     = 'X'.                                        "MPR
  APPEND ft.                                                "MPR

*   VV-Kontierungsblock-Dynpro
  CLEAR ft.
  ft-program  = rep_name_vk.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.
  LOOP AT ftvk.
    ft = ftvk.
    APPEND ft.
  ENDLOOP.
  REFRESH ftvk.
ENDFORM.                               " DYNPRO_SENDEN_VV_ANZAHLUNGEN


*eject
*&-------------------------------------------------------------------
*&      Form  VBUND_AUF_KOPF_UEBERTRAGEN
*&-------------------------------------------------------------------
*&      Routine komplett neu durch QHA941207
*&-------------------------------------------------------------------
*&      INDEX nicht hochsetzen, da Fußzeile vor VBUND
*&      kommen muß
*&      CLEAR VBUND  sonst wird das Bild mehrfach gesendet
*&------------------------------------------------------------------
FORM vbund_auf_kopf_uebertragen.
* OK-Code auf Kopf-Dynpro setzen
  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = 'PG'.
  APPEND ft.

* VBUND-Dynpro
  CLEAR ft.
  ft-program  = 'SAPLF014'.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.

  CLEAR ft.
  ft-fnam = 'RF014-VBUND'.
  ft-fval = vbund.
  APPEND ft.

  CLEAR vbund.
ENDFORM.                    "vbund_auf_kopf_uebertragen


*&---------------------------------------------------------------------*
*&      Form  XFTTAX_KOMPRIMIEREN
*&---------------------------------------------------------------------*
*&      Diese Routine komprimiert die FTTAX-Zeilen
*&      Zeilen mit gleichem MWSKZ, BSCHL, TXJCD und KSCHL können
*&      zusammengefaßt werden
*&---------------------------------------------------------------------*
FORM xfttax_komprimieren.
* begin of note 794969
* check whether jurisdiction codes are active
* and whether then 'calculate tax line by line' is active
* data now global in TOP                                    "N1899628
*  DATA: jurcode_active,                                    "N1899628
*        external_system_aktive,                            "N1899628
*        tax_linewise.                                      "N1899628

  CLEAR: jurcode_active,
         tax_linewise,
         external_system_aktive,
         txkrs,                          " note 1257048
         txdat,
         ctxkrs,                                            "N2054102
         gv_fulfilldate.

  CALL FUNCTION 'CHECK_JURISDICTION_ACTIVE'
    EXPORTING
      i_bukrs    = bkpf_bukrs
      i_land     = gv_tax_country
    IMPORTING
      e_isactive = jurcode_active
      e_external = external_system_aktive
      e_xtxit    = tax_linewise.
* end of note 794969

  REFRESH cxfttax.
  LOOP AT xfttax.
* der erste Wert, der in XFTTAX-TXKRS übergeben wird, in TXKRS sichern (1257048)
    IF txkrs IS INITIAL.                 " note 1257048
      txkrs = xfttax-txkrs.              " note 1257048
    ENDIF.                               " note 1257048
    CLEAR xfttax-txkrs.                  " note 1257048

* begin of note 2054102
    IF ctxkrs IS INITIAL.
      ctxkrs = xfttax-ctxkrs.
    ENDIF.
    CLEAR xfttax-ctxkrs.
* end of note 2054102

    IF txdat IS INITIAL.
      txdat = xfttax-txdat.
    ENDIF.
    CLEAR xfttax-txdat.

    IF gv_fulfilldate IS INITIAL.
      gv_fulfilldate = xfttax-fulfilldate.
    ENDIF.
    CLEAR xfttax-fulfilldate.

    CLEAR cxfttax.                                          "N2879391
    MOVE-CORRESPONDING xfttax TO cxfttax.
    IF NOT xfttax-fwste IS INITIAL.
*-------  Waers enthält die Belegwährung  ----------------------------
* ------  WAERS IST INITIAL, WENN ERFASSUNG NUR IN HAUSWÄHRUNG (FB00) -
      IF waers IS INITIAL.
        waers = t001-waers.
      ENDIF.

      PERFORM betrag_aufbereiten USING xfttax-fwste
                                       waers
                                       cxfttax-pfwste
                                       rcode.
      IF rcode NE 0.
        MESSAGE e022 WITH TEXT-001 xfttax-fwste
                RAISING amount_format_error.
      ENDIF.
    ENDIF.
    IF NOT xfttax-hwste IS INITIAL.
*     T001-Waers enthält die Buchungskreiswährung
      PERFORM betrag_aufbereiten USING xfttax-hwste
                                       t001-waers
                                       cxfttax-phwste
                                       rcode.
      IF rcode NE 0.
        MESSAGE e022 WITH TEXT-001 xfttax-fwste
                RAISING amount_format_error.
      ENDIF.
    ENDIF.
    IF tax_linewise IS INITIAL.                             "N794969
* begin of insertion note 2481799
* debits and credits for the same tax code are collected to only one
* line on the tax screen SAPLTAX1
      READ TABLE xtbsl WITH KEY bschl = xfttax-bschl.
      IF sy-subrc = 0.
        cxfttax-shkzg = xtbsl-shkzg.
      ELSE.
        SELECT SINGLE * FROM tbsl WHERE bschl = xfttax-bschl.
        cxfttax-shkzg = tbsl-shkzg.
      ENDIF.
      IF cxfttax-shkzg = 'H'.
        cxfttax-pfwste = 0 - cxfttax-pfwste.
        cxfttax-phwste = 0 - cxfttax-phwste.
      ENDIF.
* end of insertion note 2481799
      COLLECT cxfttax.
    ELSE.                                                   "N794969
      APPEND cxfttax.                                       "N794969
    ENDIF.                                                  "N794969
  ENDLOOP.

  REFRESH xfttax.
* begin of insertion note 2481799
  REFRESH cxfttax_tmp.
  REFRESH cxfttax_taxcode.
  cxfttax_tmp[] = cxfttax[].
  IF tax_linewise IS INITIAL.
    LOOP AT cxfttax.
      CLEAR cxfttax_taxcode.
      MOVE-CORRESPONDING cxfttax TO cxfttax_taxcode.
      COLLECT cxfttax_taxcode.
    ENDLOOP.
    REFRESH cxfttax.
    LOOP AT cxfttax_taxcode.
      CLEAR cxfttax.
      MOVE-CORRESPONDING cxfttax_taxcode TO cxfttax.

      IF cxfttax_taxcode-pfwste < 0 OR
         cxfttax_taxcode-phwste < 0.
        READ TABLE cxfttax_tmp
           WITH KEY mwskz  = cxfttax_taxcode-mwskz
                    txjcd  = cxfttax_taxcode-txjcd
                    kschl  = cxfttax_taxcode-kschl
                    txkrs  = cxfttax_taxcode-txkrs
                    ctxkrs = cxfttax_taxcode-ctxkrs
                    shkzg =  'H'.
        IF sy-subrc = 0.
          cxfttax-bschl = cxfttax_tmp-bschl.
          cxfttax-pfwste = 0 - cxfttax_taxcode-pfwste.
          cxfttax-phwste = 0 - cxfttax_taxcode-phwste.
        ELSE. "cant occur
          READ TABLE cxfttax_tmp
            WITH KEY mwskz  = cxfttax_taxcode-mwskz
                     txjcd  = cxfttax_taxcode-txjcd
                     kschl  = cxfttax_taxcode-kschl
                     txkrs  = cxfttax_taxcode-txkrs
                     ctxkrs = cxfttax_taxcode-ctxkrs
                     shkzg =  'S'.
          IF sy-subrc = 0.
            cxfttax-bschl = cxfttax_tmp-bschl.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE cxfttax_tmp
           WITH KEY mwskz  = cxfttax_taxcode-mwskz
                    txjcd  = cxfttax_taxcode-txjcd
                    kschl  = cxfttax_taxcode-kschl
                    txkrs  = cxfttax_taxcode-txkrs
                    ctxkrs = cxfttax_taxcode-ctxkrs
                    shkzg =  'S'.
        IF sy-subrc <> 0.   " 0 amount?
          READ TABLE cxfttax_tmp
             WITH KEY mwskz  = cxfttax_taxcode-mwskz
                      txjcd  = cxfttax_taxcode-txjcd
                      kschl  = cxfttax_taxcode-kschl
                      txkrs  = cxfttax_taxcode-txkrs
                      ctxkrs = cxfttax_taxcode-ctxkrs.
        ENDIF.
        cxfttax-bschl = cxfttax_tmp-bschl.
      ENDIF.
      APPEND cxfttax.
    ENDLOOP.
  ENDIF.
* end of insertion note 2481799

  LOOP AT cxfttax.
    CLEAR xfttax.
    MOVE-CORRESPONDING cxfttax TO xfttax.
    IF NOT cxfttax-pfwste IS INITIAL.
*     Waers enthält die Belegwährung
      WRITE cxfttax-pfwste TO xfttax-fwste CURRENCY waers
                                           LEFT-JUSTIFIED.  "N2906109
    ENDIF.
    IF NOT cxfttax-phwste IS INITIAL.
*     T001-Waers enthält die Buchungskreiswährung
      WRITE cxfttax-phwste TO xfttax-hwste CURRENCY t001-waers
                                           LEFT-JUSTIFIED.  "N2906109
    ENDIF.
    APPEND xfttax.
  ENDLOOP.
ENDFORM.                               " XFTTAX_KOMPRIMIEREN



*eject
*---------------------------------------------------------------------*
*       FORM BETRAG_AUFBEREITEN                                       *
*---------------------------------------------------------------------*
*       Die Betrag wird konvertiert in ein betragsfeld                *
*---------------------------------------------------------------------*
*  -->  BETRAG    aufzubereitender Betrag.                            *
*  -->  waers     Währung                                             *
*  <--  BETRAG_OUT aufbereiteter Betrag.                              *
*  <--  RC        Return Code                                         *
*---------------------------------------------------------------------*
FORM betrag_aufbereiten USING betrag waers betrag_out rc.
  DATA:  addez               TYPE i.   " zu addierende Dezst.
  DATA:  anzdez              TYPE i.   " Anzahl Dezimalstellen
  DATA:  betrag_in(16)       TYPE c.
  DATA:  betrag_n(18)        TYPE n.
  DATA:  betrag_pd2(15)      TYPE p DECIMALS 2.
  DATA:  blen                TYPE i.   " Länge
  DATA:  char4(4)            TYPE c.   " Hilfsfeld
  DATA:  char18(18)          TYPE c.   " Hilfsfeld (allg.)
  DATA:  char30(30)          TYPE c.   " Hilfsfeld (allg.)
  DATA:  deznul(4)           TYPE c.   " Hilfsfeld
  DATA:  eins(1)             TYPE c VALUE '1'.              " Char. 1
  DATA:  refe(16)            TYPE p.   " Rechenfeld

  CLEAR rc.
  CONDENSE betrag NO-GAPS."AFLE
  "begin of 3168028
  DO.
    IF betrag(1) = '0'.
      betrag(1) = space.
      CONDENSE betrag NO-GAPS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  "end of 3168028
  betrag_in = betrag.
  CONDENSE betrag_in NO-GAPS.

* ------ Dezimalstellen der Währung ------------------------------------
  IF waers NE waers_old.
    SELECT SINGLE * FROM tcurx WHERE currkey = waers.
    IF sy-subrc = 0.
      dezstellen = tcurx-currdec.
    ELSE.
      dezstellen = 2.
    ENDIF.
    waers_old = waers.

  ENDIF.

  IF dezzeichen IS INITIAL.
* ------ Dezimalzeichen: Komma oder Punkt ? ----------------------------
    CLEAR char4.
    refe = 12.
    WRITE refe TO char4 CURRENCY eins.
    dezzeichen = char4+1(1).

* ------ Fixpunktarithmetik akiv ? ------------------------------------
    SELECT SINGLE * FROM trdir WHERE name = sy-repid.
    fixpt = trdir-fixpt.
  ENDIF.

* ------ Unzulässige Zeichen? ------------------------------------------
  IF betrag_in CN '0123456789,.- '.
    rc = 4.
    EXIT.
  ENDIF.

* ------ Tausenderpunkte eliminieren -----------------------------------
  IF dezzeichen = ','.
    TRANSLATE betrag_in USING '. '.
  ELSE.
    TRANSLATE betrag_in USING ', '.
  ENDIF.
* ------ Vorzeichen eliminieren ----------------------------------------
  TRANSLATE betrag_in USING '- '.
  CONDENSE betrag_in NO-GAPS.

* ------ Dezimalzeichen in Punkt ändern -------------------------------
  IF dezzeichen = ','.
    TRANSLATE betrag_in USING ',.'.
  ENDIF.

* ------ Prüfen, ob mehrere Dezimalzeichen übergeben wurden -----------
  IF betrag_in CS '.'.
    char18 = betrag_in.
    SHIFT char18 LEFT BY sy-fdpos PLACES.
    SHIFT char18 LEFT.
    IF char18 CS '.'.
      rc = 4.
      EXIT.
    ENDIF.
  ENDIF.

* ------ evtl. Dezimalstellen ergänzen --------------------------------
  blen = strlen( betrag_in ).
  IF betrag_in CS '.'.
    anzdez = blen - sy-fdpos - 1.
  ELSE.
    anzdez = 0.
  ENDIF.

  addez = dezstellen - anzdez.
  IF addez >= 0.
    DO addez TIMES.
      deznul+0(1) = '0'.
      SHIFT deznul RIGHT.
    ENDDO.
  ELSE.
    rc = 4.
    EXIT.
  ENDIF.

  char30 = betrag_in.
  char30+20 = deznul.
  TRANSLATE char30 USING '. '.
  CONDENSE char30 NO-GAPS.
  betrag_in  = char30.
  betrag_n   = betrag_in.
  betrag_pd2 = betrag_n.

  IF fixpt = 'X'.
    betrag_pd2 = betrag_pd2 / 100.
  ENDIF.

  betrag_out = betrag_pd2.
  rc = 0.
ENDFORM.                    "betrag_aufbereiten

*eject
*&---------------------------------------------------------------------*
*&      Form  DIRECT_TAX_POSTING
*&---------------------------------------------------------------------*
*&     Beim direkten Bebuchen von Steuerkonten kann der Geschäftsbereich
*&     als COBL-GSBER angeliefert werden und ist deshalb in FTK
*&     Auf Dynpro 312 muß er aber als BSEG-GSBER ohne Kontierungsblock
*&     übertragen werden.
*&---------------------------------------------------------------------*
FORM direct_tax_posting ##CALLED.
  LOOP AT ftk.
* Felder KOSTL, AUFNR und COBL-PS_PSP_PNR werden wegen dem
* Kontierungsblock SAPLGJTS/0001 auf dem Steuerbild (Joint-Venture)
* NICHT umbenannt (CSP-Hinweis 81413).
    IF NOT ( ftk-fnam = 'COBL-KOSTL'
            OR ftk-fnam = 'COBL-AUFNR'
            OR ftk-fnam = 'COBL-RECID'
            OR ftk-fnam = 'COBL-PS_PSP_PNR' ).
      REPLACE 'COBL' WITH 'BSEG' INTO ftk-fnam.
    ENDIF.
    ft = ftk.
    APPEND ft.
  ENDLOOP.

  DESCRIBE TABLE ft LINES index.
ENDFORM.                               " DIRECT_TAX_POSTING

*eject
*&---------------------------------------------------------------------*
*&      Form  COPA_DATEN
*&---------------------------------------------------------------------*
*       falls FTCOPA Daten enthält COPA FB aufrufen und                *
*       anschließend FT füllen                                         *
*----------------------------------------------------------------------*
FORM copa_daten.
  CHECK tfill_ftcopa > 0.
  DATA: BEGIN OF ft_bdc OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF ft_bdc.

* Wegen User-Exit im RKE_FILL_BDCDATA_WITH_CRITERIA
* werden wichtige Daten im IN_COBL uebergeben.
* HKONT wird bei Anlagen nicht gefüllt, da in der Fusszeile
* (Fusszeile-Konto gespeichert in 'konto')
* das Anlagekonto und nicht das Sachkonto der Hauptbuchhaltung
* uebergeben wird.
* Note 2792948: Removed restriction to some transaction codes
  PERFORM fill_in_cobl USING 'VORGN' 'RFBU'.

  PERFORM fill_in_cobl USING 'BLART' blart.
  PERFORM fill_in_cobl USING 'BUKRS' bukrs.
  IF xtbsl-koart = 'S'.
    PERFORM fill_in_cobl USING 'HKONT' konto.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
    EXPORTING
      input  = budat
    IMPORTING
      output = budat_int.
  IF budat_int = space.            "error case
    budat_int = budat.
  ENDIF.
  PERFORM fill_in_cobl USING 'BUDAT' budat_int.

  CALL FUNCTION 'RKE_FILL_BDCDATA_WITH_CRITERIA'
    EXPORTING
      i_cobl         = in_cobl
    TABLES
      i_copadata     = ftcopa
      i_bdcdata      = ft_bdc
    EXCEPTIONS
      no_bukrs_found = 1
      no_erkrs_found = 2
      OTHERS         = 3  ##FM_SUBRC_OK.

  DESCRIBE TABLE ft_bdc LINES  tfill_ftcopa.
  IF tfill_ftcopa > 0.
*--------- Flag setzen für Aufruf von Detailbild -----------------------
    CLEAR ft.
    ft-fnam = 'DKACB-XERGO'.
    ft-fval = 'X'.
    APPEND ft.
    CLEAR ft.
*--------- COPA Daten --------------------------------------------------
    LOOP AT ft_bdc.
      ft = ft_bdc.
      APPEND ft.
    ENDLOOP.
  ENDIF.
*--------- Rücksprung zum Kontierungsblockbild -------------------------
  IF sy-subrc = 0.
    CLEAR ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    APPEND ft.
  ENDIF.
ENDFORM.                               " COPA_DATEN

*&---------------------------------------------------------------------*
*&      Form  FIND_US_TAX_SCREEN_NUMBER           "new with 30E
*&---------------------------------------------------------------------*
*       decide if detailed screen 450 or general screen 300            *
*       if there is a jurisdiction that has only space and zero        *
*       after the prefix (leng1) the data are detailed -> screen 0450  *
*----------------------------------------------------------------------*
FORM find_us_tax_screen_number USING screen.
  DATA: detail_screen(1) TYPE c.
  LOOP AT xfttax.
    IF xfttax-txjcd IS INITIAL
* if tax entered on detail level then kschl must be filled as well
    OR xfttax-kschl IS INITIAL.                             "N864459
      EXIT.
    ENDIF.
    SHIFT xfttax-txjcd LEFT BY ttxd-leng1 PLACES.
    IF  xfttax-txjcd CO ' 0'.
      detail_screen = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF detail_screen = 'X'.
    screen = '0450'.
  ELSE.
    screen = '0300'.
  ENDIF.
ENDFORM.                               " FIND_US_TAX_SCREEN_NUMBER

*&---------------------------------------------------------------------*
*&      Form  SET_FUNCTION_CODE_STEB
*&---------------------------------------------------------------------*
*       Set function code STEB (Detail Tax Batch Input)                *
*----------------------------------------------------------------------*
FORM set_function_code_steb.
  CLEAR ft.                            "STEG
  ft-fnam     = 'BDC_OKCODE'.          "STEG
  ft-fval     = 'STEB'.                "STEG
  index = index + 1.                   "STEG
  INSERT ft INDEX index.               "STEG

ENDFORM.                               " SET_FUNCTION_CODE_STEB

*&---------------------------------------------------------------------*
*&      Form  SET_FUNCTION_CODE_STEG
*&---------------------------------------------------------------------*
*       Set function code STEG (Summary tax with Jurisdiction Code)    *
*----------------------------------------------------------------------*
FORM set_function_code_steg.
  CLEAR ft.                            "STEG
  ft-fnam     = 'BDC_OKCODE'.          "STEG
  ft-fval     = 'STEG'.                "STEG
  index = index + 1.                   "STEG
  INSERT ft INDEX index.               "STEG

ENDFORM.                               " SET_FUNCTION_CODE_STEB
*&---------------------------------------------------------------------*
*&      Form  FILL_IN_COBL
*&---------------------------------------------------------------------*
*       Der Header IN_COBL wird mit den COBL-Feldern gefüllt, die in
*       der BBSEG übergeben wurden.
*----------------------------------------------------------------------*
*  -->  I_FIELDNAME        Feldname
*  -->  I_FIELDVALUE       Feldinhalt
*----------------------------------------------------------------------*
FORM fill_in_cobl USING i_fieldname
                        i_fieldvalue.

* Füllen des Export-Parameters I_COBL des FBs
* RKE_FILL_BDCDATA_WITH_CRITERIA  mit den COBL-Feldern.

  DATA: BEGIN OF fieldname,
          fix(8) VALUE 'IN_COBL-',
          var    LIKE dd03p-fieldname,
        END OF fieldname.
  FIELD-SYMBOLS: <cobl_field>.

* Die Namensungleichheiten zwischen BBSEG und COBL wurden schon in
* RFBIBL02 ausgeglichen

  fieldname-var = i_fieldname.
  ASSIGN (fieldname) TO <cobl_field>.
  <cobl_field> = i_fieldvalue.
ENDFORM.                               " FILL_IN_COBL
*&---------------------------------------------------------------"KJV -*
*&      Form  DYNPRO_SENDEN_FTAB                                 "KJV
*&---------------------------------------------------------------"KJV -*
*       text                                                     "KJV
*----------------------------------------------------------------"KJV -*
*  -->    ANEP-Daten aus FTAB in FT übertragen                   "KJV
*  <--  p2        text                                           "KJV
* JVA: Additional screen for assets wipe-ups
*----------------------------------------------------------------"KJV -*
FORM dynpro_senden_ftab.                                         "KJV
  DATA: BEGIN OF ftab_fields OCCURS 0,                         "KJV
          fnam LIKE ftab-fnam,                                "KJV
        END OF ftab_fields .                                   "KJV
  CHECK dynnr = 305.                                           "KJV
  REFRESH ftab_fields.                                         "KJV
  CLEAR ft.                                                    "KJV
  ft-program  = rep_name_ab.                                   "KJV
  ft-dynpro   = '0275'.                                        "KJV
  ft-dynbegin = 'X'.                                           "KJV
  APPEND ft.                                                   "KJV
  LOOP AT ftab.                                                "KJV
    LOOP AT ftab_fields WHERE fnam = ftab-fnam.                "KJV
    ENDLOOP.                                                   "KJV
    IF sy-subrc = 0. "field found --> new dynpro               "KJV
      ft-program  = rep_name_ab.                               "KJV
      ft-dynpro   = '0275'.                                    "KJV
      ft-dynbegin = 'X'.                                        "KJV
      CLEAR: ft-fnam, ft-fval.                                 "KJV
      APPEND ft.                                               "KJV
      REFRESH ftab_fields.                                     "KJV
    ENDIF.                                                     "KJV
    ft = ftab.                                                 "KJV
    APPEND ft.                                                 "KJV
    ftab_fields-fnam = ftab-fnam.                             "KJV
    APPEND ftab_fields.                                        "KJV
  ENDLOOP.                                                     "KJV
ENDFORM.                    " DYNPRO_SENDEN_FTAB                "KJV

*&---------------------------------------------------------------------*
*&      Form  isis_daten
*&---------------------------------------------------------------------*
*       falls FTISIS Daten enthält ISIS FB aufrufen und                *
*       anschließend FT füllen
*----------------------------------------------------------------------*
FORM isis_daten.

  CHECK tfill_ftisis > 0.
  DATA: BEGIN OF ft_bdc OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF ft_bdc.

*-----Falls Versicherungsdaten da sind, muss auf das Versicherungsdynpro
*-----gesprungen werden. Dazu auf dem Kontierungsblock-Dynpro das
*-----Ankreuzfeld setzen.

  CLEAR ft.
  ft-fnam = 'DKACB-XINSUR' .
  ft-fval = 'X'.
  APPEND ft.

  CALL FUNCTION 'ISIS_FILL_BDCDATA'
    EXPORTING
      i_form     = 'X'
    TABLES
      t_copadata = ftisis
      t_bdcdata  = ft_bdc.                                  "#EC *

  LOOP AT ft_bdc.
    CLEAR ft.
    ft = ft_bdc.
    APPEND ft.
  ENDLOOP.

*--------- Rücksprung zum Kontierungsblockbild -------------------------
  IF sy-subrc = 0.
    CLEAR ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    APPEND ft.
  ENDIF.


ENDFORM.                    " isis_daten
*&---------------------------------------------------------------------*
*&      Form  FCODE_PBBP
*&---------------------------------------------------------------------*
*       Falls Transaktion FBV1 und "vollständig parken" gewählt
*       wurde mit =PBBP abschliessen.
*----------------------------------------------------------------------*
FORM fcode_pbbp ##CALLED.

  IF tfill_ftvv NE 0.
*   Dynpro für Vermögensverwaltung wurde gesendet.
*   Rücksprung auf das vorige Standad-Dynpro
    CLEAR ft.
    ft-program  = rep_name.
    ft-dynpro   = dynnr.
    ft-dynbegin = 'X'.
    APPEND ft.
    IF dynnr = 300 OR dynnr = 305.
      PERFORM leeres_cobl_to_ft.
    ENDIF.
  ENDIF.
  IF tfill_xfttax NE 0 AND xmwst NE 'X'.
    PERFORM tax_dynpro.
*   back to standard-dynpro
    DESCRIBE TABLE ft LINES index.
  ENDIF.

  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '=PBBP'.
  index = index + 1.
  INSERT ft INDEX index.
ENDFORM.                                                    " FCODE_PBBP
*&---------------------------------------------------------------------*
*&      Form  generic_kontl_data
*&---------------------------------------------------------------------*
* Created by Note 499049
*-----------------------------------------------------------------------
* Support handling of field KONTL which can be used by several IS.
*
* We decode KONTL at this point by calling the appropriate
* decode function which has to be provided by the industry solution
*----------------------------------------------------------------------*
FORM generic_kontl_data .

  CHECK tfill_ft_generic_kontl > 0.

  DATA: BEGIN OF ft_bdc OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF ft_bdc.
  DATA: ld_kontl      TYPE kontl_fi,
        ld_kontt      TYPE kontt_fi,
        ld_program    TYPE repid,
        ld_screen     TYPE dynnr,
        ld_leave_code TYPE tcode.

  READ TABLE ft_generic_kontl WITH KEY fnam = 'BSEG-KONTL'.
  IF sy-subrc = 0.
    ld_kontl = ft_generic_kontl-fval.
  ENDIF.

  READ TABLE ft_generic_kontl WITH KEY fnam = 'BSEG-KONTT'.
  IF sy-subrc = 0.
    ld_kontt = ft_generic_kontl-fval.
  ENDIF.

*-----------------------------------------------------------------------
* As of now, only IS Insurance uses field KONTL; therefore
* we don't call a BADI or BTE here.
* Please also refer to similar source code in function modules
* FI_DOC_TO_ACCFI_TRANSFORM
* FI_DOC_TO_ACC_TRANSFORM
* in which a similar decoding is performed....
*-----------------------------------------------------------------------
  CLEAR ft.

  CASE ld_kontt.
    WHEN 'VV' OR 'VX'.    " account assignment type of IS insurance

* The following coding moved to routine dynpro_senden_ftk. Note 604733
* set 'details' flag
*    ft-fnam = 'DKACB-XINSUR'.
*    ft-fval = 'X'.
*    append ft.

* call appropriate decode function
      CALL FUNCTION 'FI_DECODE_KONTL'
        EXPORTING
          i_kontt      = ld_kontt
          i_kontl      = ld_kontl
        IMPORTING
          i_program    = ld_program
          i_screen     = ld_screen
          i_leave_code = ld_leave_code
        TABLES
          t_bdcft      = ft_bdc.                            "#EC *

* call appropriate program and screen
      CLEAR ft.
      ft-program  = ld_program.
      ft-dynpro   = ld_screen.
      ft-dynbegin = 'X'.
      APPEND ft.

* transfer decoded fields to ft structure
      LOOP AT ft_bdc.
        CLEAR ft.
        ft = ft_bdc.
        APPEND ft.
      ENDLOOP.

* leave screen
      CLEAR ft.
      IF NOT ld_leave_code IS INITIAL.
        ft-fnam = 'BDC_OKCODE'.
        ft-fval = ld_leave_code.
        APPEND ft.
      ENDIF.

    WHEN OTHERS.
*  call whatever kind of decode function here, similar to example above
  ENDCASE.

*--------- Back to COBL screen SAPLKACB 0002

  IF sy-subrc = 0.
    CLEAR ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    APPEND ft.
  ENDIF.

ENDFORM.                    " generic_kontl_data.
*&---------------------------------------------------------------------*
*&      Form  tax_exchange_rate
*&---------------------------------------------------------------------*
*      Created by Note  564235
*----------------------------------------------------------------------*

FORM tax_exchange_rate.

  DATA: ld_data(10) TYPE c.

* ----- Provide the exchange rate for tax calculations if necessary----
* check not txkrs is initial.                               "N2054102
  IF NOT txkrs IS INITIAL.                                  "N2054102
    WRITE txkrs TO ld_data USING EDIT MASK '==EXCRT'.
    CLEAR ft.
    ft-fnam(14)    = 'RTAX1-KURSF'.
    ft-fval = ld_data.
    APPEND ft.
  ENDIF.                                                    "N2054102

*begin of note 2054102
  CLEAR ld_data.
  IF NOT ctxkrs IS INITIAL.
    WRITE ctxkrs TO ld_data USING EDIT MASK '==EXCRT'.
    CLEAR ft.
    ft-fnam(14)    = 'RTAX1-CTXKRS'.
    ft-fval = ld_data.
    APPEND ft.
  ENDIF.
* end of note 2054102

ENDFORM.                    " tax_exchange_rate


FORM tax_calcdate.
  DATA: ld_data(10) TYPE c.

* ----- Provide the tax calculation date for tax calculations if necessary----

  IF NOT txdat IS INITIAL.
    WRITE txdat TO ld_data.
    CLEAR ft.
    ft-fnam(14)    = 'RTAX1-TXDAT'.
    ft-fval = ld_data.
    APPEND ft.
  ENDIF.

ENDFORM.

FORM tax_fulfilldate.
  DATA: lv_data(10) TYPE c.

  IF NOT gv_fulfilldate IS INITIAL.
    CLEAR lv_data.
    WRITE gv_fulfilldate TO lv_data.
    CLEAR ft.
    ft-fnam(16)    = 'BKPF-FULFILLDATE'.
    ft-fval = lv_data.
    APPEND ft.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  iban_data
*&---------------------------------------------------------------------*
*       Dynpro mit Iban Daten aufrufen
*----------------------------------------------------------------------*
FORM iban_data .

  DATA: ld_index  TYPE n,
        ld_index2 TYPE sy-index,
        ld_iban   TYPE iban     ##NEEDED.

  FIELD-SYMBOLS <ftc> TYPE bdcdata.

  READ TABLE ftc WITH KEY fnam = 'BSEC-BANKN' ASSIGNING <ftc>.
  IF sy-subrc NE 0 OR <ftc>-fval IS INITIAL.
* iban without bank account: SAPLIBMA screen 0200

*   okcode fuer IBAN
    CLEAR ft.
    ft-fnam = 'BDC_OKCODE'.
    ft-fval = 'IBAN'.
    APPEND ft.
*   Daten uebertragen
    CLEAR ft.
    ft-program  = rep_name_iban.
    ft-dynpro   = '0200'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam  = 'BDC_OKCODE'.
    ft-fval  = '=ENTR'.
    APPEND ft.
    ft-fnam  = 'BDC_CURSOR'.
    ft-fval  = 'IBAN01'.
    APPEND ft.
    LOOP AT ftiban.
      IF ftiban-fnam NE 'TIBAN-IBAN'.
        IF ftiban-fnam = 'TIBAN-BANKS' OR
           ftiban-fnam = 'TIBAN-BANKL'.
          ft = ftiban.
          APPEND ft.
        ENDIF.
      ELSE.
        CONDENSE ft-fval NO-GAPS.
        ld_iban = ft-fval(34).
* IBAN Daten aufspalten.
* Es werden alle neun Felder gefuellt, um zu verhindern
* dass generierte Daten ausversehen uebernommen werden
        DO 9 TIMES.
          ld_index = sy-index.
          CONCATENATE 'IBAN' '0' ld_index INTO ft-fnam.
          ld_index2 = ( sy-index - 1 ) * 4 .
          IF sy-index LE 8.
            ft-fval = ftiban-fval+ld_index2(4).
          ELSE.
            ft-fval = ftiban-fval+ld_index2(2).
          ENDIF.
          APPEND ft.
        ENDDO.
      ENDIF.
    ENDLOOP.
    CLEAR ft.
    ft-program  = rep_name_iban.
    ft-dynpro   = '0200'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam  = 'BDC_OKCODE'.
    ft-fval  = '=ENTR'.
    APPEND ft.

  ELSE.
* iban with bank account: SAPLIBMA screen 0100

*   okcode fuer IBAN
    CLEAR ft.
    ft-fnam = 'BDC_OKCODE'.
    ft-fval = 'IBAN'.
    APPEND ft.
*   Daten uebertragen
    CLEAR ft.
    ft-program  = rep_name_iban.
    ft-dynpro   = '0100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam  = 'BDC_OKCODE'.
    ft-fval  = '=ENTR'.
    APPEND ft.
    ft-fnam  = 'BDC_CURSOR'.
    ft-fval  = 'IBAN01'.
    APPEND ft.
    LOOP AT ftiban.
      IF ftiban-fnam NE 'TIBAN-IBAN'.
        ft = ftiban.
        APPEND ft.
      ELSE.
        CONDENSE ft-fval NO-GAPS.
        ld_iban = ft-fval(34).
* IBAN Daten aufspalten.
* Es werden alle neun Felder gefuellt, um zu verhindern
* dass generierte Daten ausversehen uebernommen werden
        DO 9 TIMES.
          ld_index = sy-index.
          CONCATENATE 'IBAN' '0' ld_index INTO ft-fnam.
          ld_index2 = ( sy-index - 1 ) * 4 .
          IF sy-index LE 8.
            ft-fval = ftiban-fval+ld_index2(4).
          ELSE.
            ft-fval = ftiban-fval+ld_index2(2).
          ENDIF.
          APPEND ft.
        ENDDO.
      ENDIF.
    ENDLOOP.

  ENDIF.


* Ruecksprung zum CPD Bild
  CLEAR ft.
  ft-program  = rep_name_c.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.

ENDFORM.                    " iban_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_BBSEG_FOR_VZK
*&---------------------------------------------------------------------*
*       VZK in BBSEG => IS-IS (insurance) active
*----------------------------------------------------------------------*
FORM check_bbseg_for_vzk  CHANGING vzk_active TYPE c.

  DATA: lv_descr  TYPE REF TO cl_abap_structdescr,
        lv_length TYPE i.
  FIELD-SYMBOLS: <comp_descr> TYPE abap_compdescr.

  lv_descr ?= cl_abap_structdescr=>describe_by_name( 'BBSEG' ).
  CLEAR vzk_active.

  LOOP AT lv_descr->components ASSIGNING <comp_descr>.
    lv_length = numofchar( <comp_descr>-name ) .
    IF lv_length >= 5.
      SUBTRACT 5 FROM lv_length.
      IF <comp_descr>-name(5) = 'ISCD_' OR
       <comp_descr>-name+lv_length(5) = '_ISCD' .
        vzk_active = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_BBSEG_FOR_VZK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_SIMULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_FT  text
*----------------------------------------------------------------------*
FORM document_simulation.
  DATA:
    ls_ft            LIKE bdcdata,
    l_tfill_xftclear LIKE sy-tfill.

* sy-tfill eq 0
* we assume that we have only on account payments or
* G/L account postings

  DESCRIBE TABLE xftclear LINES l_tfill_xftclear.

  DESCRIBE TABLE ft LINES sy-tfill.
  READ TABLE ft INTO ls_ft INDEX sy-tfill.

  IF l_tfill_xftclear NE 0.
    ls_ft-fval = 'PA'.                                    "Note1855107
    MODIFY ft FROM ls_ft INDEX sy-tfill.

    CLEAR ls_ft.

    ls_ft-program = 'SAPDF05X'.
    ls_ft-dynpro   = '3100'.
    ls_ft-dynbegin    = 'X'.
    APPEND ls_ft TO ft.

    CLEAR ls_ft.

    ls_ft-fnam = 'BDC_OKCODE'.
    ls_ft-fval = 'BS'.
    APPEND ls_ft TO ft.
  ELSE.
    READ TABLE ft INTO ls_ft WITH KEY fval = '/11'.
    ls_ft-fval = 'BS'.
    MODIFY ft FROM ls_ft INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " DOCUMENT_SIMULATION
*&---------------------------------------------------------------------*
*&      Form  MANDATE_DATA
*&---------------------------------------------------------------------*
*       Call mandate screen for CpD (one time) customer and individual
*       payer.
*       Created by note 1923657
*----------------------------------------------------------------------*
FORM mandate_data.

  IF gv_mndid_ext IS INITIAL.                               "N1970773
    CALL FUNCTION 'SEPA_CUSTOMIZING_READ'
      EXPORTING
        i_anwnd           = 'F'
        i_parameter_id    = 'MNDID_EXT'
      IMPORTING
        e_parameter_value = gv_mndid_ext.
  ENDIF.
  IF gv_crdid_ext IS INITIAL.
    CALL FUNCTION 'SEPA_CUSTOMIZING_READ'
      EXPORTING
        i_anwnd           = 'F'
        i_parameter_id    = 'CRDID_EXT'
      IMPORTING
        e_parameter_value = gv_crdid_ext.
  ENDIF.                                                    "N1970773
  READ TABLE ftmndid INDEX 1.
* okcode  for mandate
  CLEAR ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = 'MNDT'.
  APPEND ft.
* mandate reference in 1st popup screen
  IF gv_mndid_ext NE gc_off OR gv_crdid_ext NE gc_off       "N1970773
  OR ftmndid-fval IS NOT INITIAL.                           "N1970773
    CLEAR ft.
    ft-program  = rep_name_mndt.
    ft-dynpro   = '1100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    IF gv_mndid_ext NE gc_off                               "N1970773
    OR ftmndid-fval IS NOT INITIAL.                         "N1970773
      CLEAR ft.
      ft-fnam = 'RFSEPA_INTKEY-MNDID'.
      ft-fval = ftmndid-fval.
      APPEND ft.
    ENDIF.                                                  "N1970773
    CLEAR ft.
    ft-fnam  = 'BDC_OKCODE'.
    ft-fval  = '=ENTR'.
    APPEND ft.
  ENDIF.                                                    "N1970773
* mandate reference screen in order to derive mandate data.
  CLEAR ft.
  ft-program  = rep_name_mndt.
  ft-dynpro   = '0210'.
  ft-dynbegin = 'X'.
  APPEND ft.
  CLEAR ft.
  ft-fnam  = 'BDC_OKCODE'.
  ft-fval  = '=ENTR'.
  APPEND ft.
* back to CpD screen
  CLEAR ft.
  ft-program  = rep_name_c.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.

ENDFORM.                    " MANDATE_DATA
* Begin of note 2142438
*&---------------------------------------------------------------------*
*&      Form  DYNPRO_SENDEN_FTTXT
*&---------------------------------------------------------------------*
*       Call Text Screen for CUP and CIG
*----------------------------------------------------------------------*
FORM dynpro_senden_fttxt .

  STATICS: BEGIN OF st_txttab OCCURS 0.
             INCLUDE STRUCTURE theadvb.
STATICS END OF st_txttab.
  STATICS: xalready_run TYPE flag.

  DATA: BEGIN OF lt_ttxern OCCURS 0 ##NEEDED.
          INCLUDE STRUCTURE ttxern.
  DATA: END OF lt_ttxern            .
  DATA: BEGIN OF lt_ttxit OCCURS 0  ##NEEDED.
          INCLUDE STRUCTURE ttxit.
  DATA: END OF lt_ttxit.

  DATA: BEGIN OF lt_fttxt OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA:   pos(2) TYPE n.
  DATA: END OF lt_fttxt.

  DATA: ld_max_len  TYPE i      VALUE 11  ##NEEDED,
        ld_fnam_txt TYPE char15.

* read customizing if not already done
  IF xalready_run NE 'X'.
    xalready_run = 'X'.

    SELECT * FROM ttxern
             INTO lt_ttxern
             WHERE tdobject = 'BELEG'
               AND txtgr    = '*'.
      SELECT * FROM ttxit
             INTO lt_ttxit
             WHERE tdobject = lt_ttxern-tdobject
               AND tdid     = lt_ttxern-tdid.
        CHECK lt_ttxit-tdspras = sy-langu.
        MOVE-CORRESPONDING lt_ttxit  TO st_txttab.
        MOVE-CORRESPONDING lt_ttxern TO st_txttab.
        APPEND st_txttab.
      ENDSELECT.
    ENDSELECT.
  ENDIF.

* read position of CIG/CUP from st_txttab into ld_cig_pos and ld_cip_pos

  LOOP AT fttxt.
    MOVE-CORRESPONDING fttxt TO lt_fttxt.
    READ TABLE st_txttab WITH KEY tdid = fttxt-fnam+5(4).
    IF sy-subrc = 0.
      lt_fttxt-pos = sy-tabix.
      APPEND lt_fttxt.
    ENDIF.
  ENDLOOP.
  SORT lt_fttxt BY pos.

* create BI data
* send okcode for text screen
  IF xtbsl-koart NE  'S'.
    CLEAR ft.
    ft-fnam = 'BDC_OKCODE'.
    ft-fval = '=TEXT'.
    APPEND ft.
  ENDIF.

  PERFORM dynpro_senden_wt.                                 "N2481799

* maybe it is necessary to send the coding block
*  IF xtbsl-koart = 'S'.
*    perform dynpro_senden_ftk.
*  ENDIF.

  CLEAR ft.
  ft-program  = rep_name_text.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  APPEND ft.

  CLEAR ft.                                                 "N2481799
  ft-fnam = 'BDC_OKCODE'.                                   "N2481799
  ft-fval = '=BACK'.                                        "N2481799
  APPEND ft.                                                "N2481799

* check which one is smaller, is it gt 11, send ok_code /23, determine new position
* replace position in template
  LOOP AT lt_fttxt.
    ld_fnam_txt = 'RTEXT-LTEXT(YY)'.
    IF lt_fttxt-pos > '11'.
* send okcode /23 new page

    ENDIF.
    REPLACE 'YY' WITH lt_fttxt-pos INTO ld_fnam_txt.
    CLEAR ft.
    ft-fnam = ld_fnam_txt.
    ft-fval = lt_fttxt-fval.
    APPEND ft.

  ENDLOOP.

ENDFORM.
* end of note 2142438

* Begin of note 2481799
*&---------------------------------------------------------------------*
*&      Form  DYNPRO_SENDEN_WT
*&---------------------------------------------------------------------*
*       send popup for withholding tax data
*----------------------------------------------------------------------*
FORM dynpro_senden_wt.
  DATA :l_lifnr TYPE lifnr.                                 "878993
  DATA :l_kunnr TYPE kunnr.                                 "878993
  DATA :h_lfb1 LIKE lfb1 OCCURS 1 WITH HEADER LINE.         "878993
  DATA :h_knb1 LIKE knb1 OCCURS 1 WITH HEADER LINE.         "878993
  DATA: cmp_str  TYPE string.                               "870828
  cmp_str = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   "870828
  DATA: h_wt_acco LIKE with_item-wt_acco.


  CHECK g_wt_filled IS INITIAL.

  READ TABLE ft WITH KEY fnam = 'BKPF-AWTYP' fval = 'IBKPF'
    TRANSPORTING NO FIELDS.                                 "2746002
  IF sy-subrc = 0.                                          "2746002
    READ TABLE ft WITH KEY fnam = 'BSEG-WRBTR'
      TRANSPORTING NO FIELDS.                               "2746002
    CHECK sy-subrc = 0.                                     "2746002
  ENDIF.                                                    "2746002

  g_wt_filled = 'X'.
  IF tcode NE 'FB05'                                       "note 526316
     AND ( xtbsl-koart EQ 'K' OR xtbsl-koart EQ 'D' ).      "642392
*  if tfill_ftw ne 0.
    CALL FUNCTION 'FI_CHECK_EXTENDED_WT'
      EXPORTING
        i_bukrs              = bukrs
      EXCEPTIONS
        component_not_active = 1
        not_found            = 2
        OTHERS               = 3.
    IF sy-subrc = 0.
*   h_wt_acco = konto(10).                                  "508747
      IF xtbsl-koart = 'K'.                          "start of 878993
        l_lifnr = konto.
        CALL FUNCTION 'FI_WT_READ_LFB1'
          EXPORTING
            i_lifnr   = l_lifnr
            i_bukrs   = bukrs
          TABLES
            t_lfb1    = h_lfb1
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc = 0.
          READ TABLE h_lfb1 WITH KEY lifnr = h_lfb1-lifnr
                                     bukrs = bukrs.
          IF NOT h_lfb1-lnrze IS INITIAL.
            konto = h_lfb1-lnrze.
          ENDIF.
        ENDIF.
      ELSEIF xtbsl-koart = 'D' .
        l_kunnr = konto.
        CALL FUNCTION 'FI_WT_READ_KNB1'
          EXPORTING
            i_kunnr   = l_kunnr
            i_bukrs   = bukrs
          TABLES
            t_knb1    = h_knb1
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc = 0.
          READ TABLE h_knb1 WITH KEY kunnr = h_knb1-kunnr
                                     bukrs = bukrs.
          IF NOT h_knb1-knrze IS INITIAL.
            konto = h_knb1-knrze.
          ENDIF.
        ENDIF.
      ENDIF.                                           "end of 878993

      IF konto NA cmp_str.                                  "870828
        SHIFT konto LEFT DELETING LEADING '0'.              "642392
      ENDIF.                                                "870828

      IF konto(1) = '='.                                    "673286
        IF konto CA '.'.                                    "673286
          SHIFT konto UP TO '.'.                            "673286
        ENDIF.                                              "673286
        SHIFT konto LEFT.                                   "673286
      ENDIF.                                                "673286

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'           "508747
        EXPORTING                                           "508747
          input  = konto(10)           "673286 "624989 "508747
        IMPORTING                                           "508747
          output = h_wt_acco.                        "508747
      TRANSLATE h_wt_acco TO UPPER CASE.                 "#EC TRANSLANG
      CALL FUNCTION 'FI_WT_FIPI_FILL_FT_TAB'
        EXPORTING
          i_dynnr            = dynnr
          i_index            = index
          i_koart            = xtbsl-koart
          i_wt_acco          = h_wt_acco
          i_bukrs            = bukrs
          i_budat            = budat_wt
          i_bschl            = xtbsl-bschl
          i_umskz            = umskz
*       IMPORTING
*         e_index            = index
        TABLES
          i_ftw              = ftw
          i_ft               = ft
        EXCEPTIONS
          dynpro_not_correct = 1
          OTHERS             = 2  ##FM_SUBRC_OK.
*
*--- Dynpro setzen für Fußzeile
*         clear ft.
*         ft-program  = rep_name.
*         ft-dynpro   = dynnr.
*         ft-dynbegin = 'X'.
*         append ft.
*         if dynnr = '0300' or
*            dynnr = '0301' or
*            dynnr = '0302' or
*            dynnr = '0312'.
*            clear ft.
*            ft-fnam = 'BDC_CURSOR'.
*            ft-fval = fnam_konto.
*            append ft.
*         endif.
*         describe table ft lines index.
*
*    endif.
    ENDIF.
  ENDIF.                                                     "note 526316
ENDFORM.
* end of note 2142438
