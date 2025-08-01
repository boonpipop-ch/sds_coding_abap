*&---------------------------------------------------------------------*
*& Include ZSDSMMI0010
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM WE03_AUSGABE                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM we03_ausgabe USING lgortsplit.
*----------------Drucken WE-Version 3 Überschrift---------------------*
  if lgortsplit is initial.
     ON CHANGE OF MKPF-MBLNR OR MSEG-BWART "OR MSEG-EBELN
                             OR NAST-KSCHL OR MSEG-WERKS.
     CLEAR XKOPFDR.
     endon.
  else.
     ON CHANGE OF MKPF-MBLNR OR MSEG-BWART "OR MSEG-EBELN
                             OR NAST-KSCHL OR MSEG-WERKS
                             OR MSEG-LGORT.
     CLEAR XKOPFDR.
     endon.
  endif.
  IF XKOPFDR IS INITIAL.
    XKOPFDR = X.
    IF NEW_PAGE = X.
      CALL FUNCTION 'CONTROL_FORM'
           EXPORTING
                COMMAND = 'NEW-PAGE'.
    ENDIF.
    IF EKKO-BSART = 'UB'.
      AM07M-LITXT = TEXT-101.
      AM07M-LIBZG = EKKO-RESWK.
    ELSE.
      AM07M-LITXT = TEXT-100.
      AM07M-LIBZG = EKKO-LIFNR.
      IF NOT EKKO-LLIEF IS INITIAL.
        AM07M-LIBZ2 = EKKO-LLIEF.
      ELSE.
        AM07M-LIBZ2 = EKKO-LIFNR.
      ENDIF.
    ENDIF.
    IF T156-SHKZG = H.
      AM07M-HDLNE = TEXT-020.
    ELSE.
      AM07M-HDLNE = TEXT-010.
    ENDIF.
    IF XPSTY       IS INITIAL.         "Lagermaterial ?
      IF MSEG-XBLVS IS INITIAL.
        IF T156-SHKZG = H AND
           NOT MSEG-GRUND IS INITIAL.
          IF NOT T159P-BACOD IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3BACOKOPF'
                      WINDOW  = 'RUEKOPF'.
            NEW_PAGE = X.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3KOPF'
                      WINDOW  = 'KOPF'.     "'RUEKOPF'.
            NEW_PAGE = X.
          ENDIF.
        ELSE.
          IF NOT T159P-BACOD IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3BACOKOPF'
                      WINDOW  = 'KOPF'.
            NEW_PAGE = X.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3KOPF'
                      WINDOW  = 'KOPF'.
            NEW_PAGE = X.
          ENDIF.
        ENDIF.
      ELSE.
        IF T156-SHKZG = H AND
           NOT MSEG-GRUND IS INITIAL.
          IF NOT T159P-BACOD IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3BACOLVSKOPF'
                      WINDOW  = 'RUEKOPF'.
            NEW_PAGE = X.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3LVSKOPF'  "RL-Material
                      WINDOW  = 'RUEKOPF'.
            NEW_PAGE = X.
          ENDIF.
        ELSE.
          IF NOT T159P-BACOD IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3BACOLVSKOPF'
                      WINDOW  = 'KOPF'.
            NEW_PAGE = X.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'WE3LVSKOPF'  "RL-Material
                      WINDOW  = 'KOPF'.
            NEW_PAGE = X.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF T156-SHKZG = H AND
         NOT MSEG-GRUND IS INITIAL.
        IF NOT T159P-BACOD IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'WE3BACOKOPF1'
                    WINDOW  = 'RUEKOPF'.
          NEW_PAGE = X.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'WE3KOPF1'
                    WINDOW  = 'RUEKOPF'.
          NEW_PAGE = X.
        ENDIF.
      ELSE.
        IF NOT T159P-BACOD IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'WE3BACOKOPF1'
                    WINDOW  = 'KOPF'.
          NEW_PAGE = X.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'WE3KOPF1'
                    WINDOW  = 'KOPF'.
          NEW_PAGE = X.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*------------- Drucken WE-Version 3 Position --------------------------*

**perform w3_lieferplan.                  "Routine Lieferplan
  CLEAR T157E.
  IF T156-SHKZG = H AND
     NOT MSEG-GRUND IS INITIAL.
    SELECT SINGLE * FROM T157E WHERE BWART = MSEG-BWART
                               AND   GRUND = MSEG-GRUND
                               AND   SPRAS = LANGUAGE.
  ENDIF.
  IF MSEG-EBELN IS INITIAL.
    IF NOT MABDR-MAKTX IS INITIAL.
      EKPO-TXZ01 = MABDR-MAKTX.
    ENDIF.
  ENDIF.
  IF MABDR-WERTU IS INITIAL AND
     MABDR-MENGU = X   OR              "Bestandsfuehrung = UNBW oder
     XPSTY       IS INITIAL.           "Lagermaterial ?
* User-Exit über Erweiterung MBCF0005
    CALL CUSTOMER-FUNCTION '001'
         EXPORTING
              I_MKPF  = MKPF
              I_MSEG  = MSEG
              I_EKKO  = EKKO
              I_EKPO  = EKPO
              I_NAST  = NAST
              I_TNAPR = TNAPR
         TABLES
              I_EKKN  = XEKKN
         CHANGING
              C_AM07M = AM07M
         EXCEPTIONS
              OTHERS  = 0.
    MSEG-DMBTR = EKPO-MENGE * EKPO-NETPR.   "ADD 3SDS007
    IF MSEG-XBLVS IS INITIAL.
      PERFORM W3_LAGERMATERIAL.        "Routine Lagermaterial
    ELSE.
      PERFORM W3_LVSMATERIAL.          "Routine RL-Material
    ENDIF.
  ELSE.
    PERFORM W3_VERBRAUCHSMATERIAL.     "Routine Verbrauchsmaterial
  ENDIF.

*-------------- Drucken WE-Version 3 Seitenfuss ----------------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT  = 'WE3FUSS'
            WINDOW   = 'FUSS'
            FUNCTION = 'APPEND'.
ENDFORM.

*--------------Unteroutinen für Positionsdruck -----------------------*
FORM W3_LAGERMATERIAL.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'WE3LGMAT'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM W3_LVSMATERIAL                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM W3_LVSMATERIAL.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'WE3LVSMAT'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM W3_VERBRAUCHSMATERIAL                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM W3_VERBRAUCHSMATERIAL.
  CLEAR: AM07M-KONTIERUNG.
  AM07M-RSTYP = XPSTY.
  CASE XPSTY.
    WHEN XFERT.                        "Fertigungsauftrag
      IF X_KONT1 IS INITIAL.
        AM07M-KONTIERUNG = MSEG-AUFNR.
      ELSE.
        AM07M-KONTIERUNG = EKKN-AUFNR.
      ENDIF.
    WHEN XANLAGE.
      MOVE SPACE TO ANLAGE.
      IF X_KONT1 IS INITIAL.
        ANLAGE-ANLN1 = MSEG-ANLN1.
        ANLAGE-ANLN2 = MSEG-ANLN2.
      ELSE.
        ANLAGE-ANLN1 = EKKN-ANLN1.
        ANLAGE-ANLN2 = EKKN-ANLN2.
      ENDIF.
      MOVE SPACE TO AM07M-KONTIERUNG.
      CONDENSE ANLAGE NO-GAPS.
      AM07M-KONTIERUNG = ANLAGE.

    WHEN   XKDE.                     "Kundeneinzelbestand?
      MOVE SPACE TO KUNDE.
      IF X_KONT1 IS INITIAL.
        KUNDE-KDAUF = MSEG-KDAUF.
        KUNDE-KDPOS = MSEG-KDPOS.
      ELSE.
        KUNDE-KDAUF = EKKN-VBELN.
        KUNDE-KDPOS = EKKN-VBELP.
      ENDIF.
      MOVE SPACE TO AM07M-KONTIERUNG.
      CONDENSE KUNDE NO-GAPS.
      AM07M-KONTIERUNG = KUNDE.

    WHEN   XKDANR.                     "Kundenauftrag ?
      MOVE SPACE TO KUNDE.
      IF X_KONT1 IS INITIAL.
        KUNDE-KDAUF = MSEG-KDAUF.
        KUNDE-KDPOS = MSEG-KDPOS.
        KUNDE-KDEIN = MSEG-KDEIN.
      ELSE.
        KUNDE-KDAUF = EKKN-VBELN.
        KUNDE-KDPOS = EKKN-VBELP.
        KUNDE-KDEIN = EKKN-VETEN.
      ENDIF.
      MOVE SPACE TO AM07M-KONTIERUNG.
      CONDENSE KUNDE NO-GAPS.
      AM07M-KONTIERUNG = KUNDE.

    WHEN   XKOSTL.                     "auf Kostenstelle kontiert
      IF X_KONT1 IS INITIAL.
        AM07M-KONTIERUNG = MSEG-KOSTL.
      ELSE.
        AM07M-KONTIERUNG = EKKN-KOSTL.
      ENDIF.

    WHEN   XPROJN.                     "auf Projekt/Netzplan
      IF X_KONT1 IS INITIAL.
        IF MSEG-NPLNR IS INITIAL.
          PERFORM PSP_CONVERT USING MSEG-PS_PSP_PNR.
        ELSE.
          AM07M-KONTIERUNG = MSEG-NPLNR.
          PERFORM NW_VORGANG_LESEN USING MSEG-AUFPL MSEG-APLZL.
          IF NOT N_VORNR IS INITIAL.
            MOVE '/'     TO AM07M-KONTIERUNG+12.
            MOVE N_VORNR TO AM07M-KONTIERUNG+13.
          ENDIF.
        ENDIF.
      ELSE.
        IF EKKN-NPLNR IS INITIAL.
          PERFORM PSP_CONVERT USING EKKN-PS_PSP_PNR.
        ELSE.
          AM07M-KONTIERUNG = EKKN-NPLNR.
          PERFORM NW_VORGANG_LESEN USING EKKN-AUFPL EKKN-APLZL.
          IF NOT N_VORNR IS INITIAL.
            MOVE '/'     TO AM07M-KONTIERUNG+12.
            MOVE N_VORNR TO AM07M-KONTIERUNG+13.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
  CLEAR X_KONT1.
* User-Exit über Erweiterung MBCF0005
  CALL CUSTOMER-FUNCTION '001'
       EXPORTING
            I_MKPF  = MKPF
            I_MSEG  = MSEG
            I_EKKO  = EKKO
            I_EKPO  = EKPO
            I_NAST  = NAST
            I_TNAPR = TNAPR
       TABLES
            I_EKKN  = XEKKN
       CHANGING
            C_AM07M = AM07M
       EXCEPTIONS
            OTHERS  = 0.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'WE3VERBRMAT'.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM WE03_AUSGABE_PDF                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM we03_ausgabe_pdf USING lgortsplit.
*----------------Drucken WE-Version 3 Überschrift---------------------*
  IF lgortsplit IS INITIAL.
    ON CHANGE OF gs_mkpf-mblnr OR gs_mseg-ebeln OR gs_mseg-bwart
                            OR nast-kschl OR gs_mseg-werks.
      CLEAR xkopfdr.
      IF NOT gs_we03_details IS INITIAL.
        APPEND gs_we03_details TO gt_we03_details.
        CLEAR gs_we03_details.
      ENDIF.
    ENDON.
  ELSE.
    ON CHANGE OF gs_mkpf-mblnr OR gs_mseg-ebeln OR gs_mseg-bwart
                            OR nast-kschl OR gs_mseg-werks
                            OR gs_mseg-lgort.
      CLEAR xkopfdr.
      IF NOT gs_we03_details IS INITIAL.
        APPEND gs_we03_details TO gt_we03_details.
        CLEAR gs_we03_details.
      ENDIF.
    ENDON.
  ENDIF.
  IF xkopfdr IS INITIAL.
    xkopfdr = x.
    IF ekko-bsart = 'UB'.
      gs_am07m-litxt = text-101.
      gs_am07m-libzg = ekko-reswk.
    ELSE.
      gs_am07m-litxt = text-100.
      gs_am07m-libzg = ekko-lifnr.
      IF NOT ekko-llief IS INITIAL.
        gs_am07m-libz2 = ekko-llief.
      ELSE.
        gs_am07m-libz2 = ekko-lifnr.
      ENDIF.
    ENDIF.
    IF gs_t156-shkzg = h.                                       "2910987
      gs_am07m-hdlne = text-020.
    ELSE.
      gs_am07m-hdlne = text-010.
    ENDIF.

    MOVE ladr TO gs_we03_details-ladr.
    MOVE gs_am07m TO gs_we03_details-am07m.
    MOVE gs_mkpf TO gs_we03_details-mkpf.
    MOVE gs_mseg TO gs_we03_details-mseg.
* In case not split records clear header storage loc data
    IF lgortsplit IS INITIAL.
      gs_we03_details-mseg-lgort = ' '.
    ENDIF.
    MOVE gs_t001w TO gs_we03_details-t001w.
    MOVE ekko TO gs_we03_details-ekko.
    MOVE t024 TO gs_we03_details-t024.
    MOVE gs_mabdr TO gs_we03_details-mabdr. " this is moved to handle table display in layout
  ENDIF.
*------------- Drucken WE-Version 3 Position --------------------------*
  CLEAR gs_am07m-rstyp.                                         "3012827

  IF xpsty IS NOT INITIAL.                                      "1722124
    PERFORM W3_VERBRAUCHSMATERIAL_PDF.                          "1722124
  ENDIF.                                                        "1722124

  CLEAR t157e.
  IF gs_t156-shkzg = h AND                                      "2910987
     NOT gs_mseg-grund IS INITIAL.
    SELECT SINGLE * FROM t157e WHERE bwart = gs_mseg-bwart
                               AND   grund = gs_mseg-grund
                               AND   spras = language.
  ENDIF.
  IF gs_mseg-ebeln IS INITIAL.
    IF NOT gs_mabdr-maktx IS INITIAL.
      ekpo-txz01 = gs_mabdr-maktx.
    ENDIF.
  ENDIF.
  IF gs_mabdr-wertu IS INITIAL AND
     gs_mabdr-mengu = x   OR              "Bestandsfuehrung = UNBW oder
     xpsty       IS INITIAL.           "Lagermaterial ?
* User-Exit über Erweiterung MBCF0005
    CALL CUSTOMER-FUNCTION '001'
         EXPORTING
              i_mkpf  = gs_mkpf
              i_mseg  = gs_mseg
              i_ekko  = ekko
              i_ekpo  = ekpo
              i_nast  = nast
              i_tnapr = tnapr
         TABLES
              i_ekkn  = xekkn
         CHANGING
              c_am07m = gs_am07m
         EXCEPTIONS
              OTHERS  = 0.
  ENDIF.

  MOVE gs_am07m TO gs_main-am07m.
  MOVE gs_mabdr TO gs_main-mabdr.
  MOVE gs_mseg TO gs_main-mseg.
  MOVE t157e TO gs_main-t157e.
  MOVE ekpo TO gs_main-ekpo.

  APPEND gs_main TO gt_main.
  IF gs_mabdr-wertu IS INITIAL AND
       gs_mabdr-mengu = x   OR              "Bestandsfuehrung = UNBW oder
       gs_am07m-rstyp       IS INITIAL.           "Lagermaterial ?
    IF gs_mseg-xblvs IS INITIAL.
      APPEND LINES OF gt_main TO gs_we03_details-main_lagermaterial. "Routine Lagermaterial
      CLEAR gt_main.
    ELSE.
      APPEND LINES OF gt_main TO gs_we03_details-main_lvsmaterial.    "Routine RL-Material
      CLEAR gt_main.
    ENDIF.
  ELSE.
    APPEND LINES OF gt_main TO gs_we03_details-main_verbrauchsmaterial.     "Routine Verbrauchsmaterial
    CLEAR gt_main.
  ENDIF.
ENDFORM.                    "WE03_AUSGABE_pdf
*---------------------------------------------------------------------*
*       FORM W3_VERBRAUCHSMATERIAL_pdf                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM w3_verbrauchsmaterial_pdf.
  CLEAR: gs_am07m-kontierung.
  gs_am07m-rstyp = xpsty.
  CASE xpsty.
    WHEN xfert.                        "Fertigungsauftrag
      IF x_kont1 IS INITIAL.
        gs_am07m-kontierung = gs_mseg-aufnr.                    "2158392
      ELSE.
        gs_am07m-kontierung = ekkn-aufnr.
      ENDIF.
    WHEN xanlage.
      MOVE space TO anlage.
      IF x_kont1 IS INITIAL.
        anlage-anln1 = gs_mseg-anln1.
        anlage-anln2 = gs_mseg-anln2.
      ELSE.
        anlage-anln1 = ekkn-anln1.
        anlage-anln2 = ekkn-anln2.
      ENDIF.
      MOVE space TO gs_am07m-kontierung.
      CONDENSE anlage NO-GAPS.
      gs_am07m-kontierung = anlage.

    WHEN   xkde.                     "Kundeneinzelbestand?
      MOVE space TO kunde.
      IF x_kont1 IS INITIAL.
        kunde-kdauf = gs_mseg-kdauf.
        kunde-kdpos = gs_mseg-kdpos.
      ELSE.
        kunde-kdauf = ekkn-vbeln.
        kunde-kdpos = ekkn-vbelp.
      ENDIF.
      MOVE space TO gs_am07m-kontierung.
      CONDENSE kunde NO-GAPS.
      gs_am07m-kontierung = kunde.

    WHEN   xkdanr.                     "Kundenauftrag ?
      MOVE space TO kunde.
      IF x_kont1 IS INITIAL.
        kunde-kdauf = gs_mseg-kdauf.
        kunde-kdpos = gs_mseg-kdpos.
        kunde-kdein = gs_mseg-kdein.
      ELSE.
        kunde-kdauf = ekkn-vbeln.
        kunde-kdpos = ekkn-vbelp.
        kunde-kdein = ekkn-veten.
      ENDIF.
      MOVE space TO gs_am07m-kontierung.
      CONDENSE kunde NO-GAPS.
      am07m-kontierung = kunde.

    WHEN   xkostl.                     "auf Kostenstelle kontiert
      IF x_kont1 IS INITIAL.
        gs_am07m-kontierung = gs_mseg-kostl.
      ELSE.
        gs_am07m-kontierung = ekkn-kostl.
      ENDIF.

    WHEN   xprojn OR xnplan.              "auf Projekt/Netzplan "2158392
      IF x_kont1 IS INITIAL.
        IF gs_mseg-nplnr IS INITIAL.
          PERFORM psp_convert_pdf USING gs_mseg-ps_psp_pnr.
        ELSE.
          gs_am07m-kontierung = gs_mseg-nplnr.                  "2158392
          PERFORM nw_vorgang_lesen                              "2158392
            USING gs_mseg-aufpl gs_mseg-aplzl.                  "2158392
          IF NOT n_vornr IS INITIAL.
            MOVE '/'     TO gs_am07m-kontierung+12.
            MOVE n_vornr TO gs_am07m-kontierung+13.
          ENDIF.
        ENDIF.
      ELSE.
        IF ekkn-nplnr IS INITIAL.
          PERFORM psp_convert_pdf USING ekkn-ps_psp_pnr.
        ELSE.
          gs_am07m-kontierung = ekkn-nplnr.
          PERFORM nw_vorgang_lesen USING ekkn-aufpl ekkn-aplzl.
          IF NOT n_vornr IS INITIAL.
            MOVE '/'     TO gs_am07m-kontierung+12.
            MOVE n_vornr TO gs_am07m-kontierung+13.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
  CLEAR x_kont1.
* User-Exit über Erweiterung MBCF0005
  CALL CUSTOMER-FUNCTION '001'
       EXPORTING
            i_mkpf  = gs_mkpf
            i_mseg  = gs_mseg
            i_ekko  = ekko
            i_ekpo  = ekpo
            i_nast  = nast
            i_tnapr = tnapr
       TABLES
            i_ekkn  = xekkn
       CHANGING
            c_am07m = gs_am07m
       EXCEPTIONS
            OTHERS  = 0.
ENDFORM.                    "W3_VERBRAUCHSMATERIAL_PDF
