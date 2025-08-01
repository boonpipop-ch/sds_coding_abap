FUNCTION-POOL zsdsfi08 MESSAGE-ID f8.

*----------------------------------------------------------------------*
* Function module : Z_SDSFI_POSTING_INTF_CLR_PART
* Function  Desc  : COPY FM POSTING_INTERFACE_CLEARING
*                   AND ADD MORE BDC STEPS FOR PARTIAL CLEARING
* WRICEF id       : FIARE03
* Start Date      : 07.06.2024
* Developer       : Apichat C.
* SAP Version     : S/4 HANA
* COPY ALL CODING OF TOP INCLUDE FROM STANDARD INCLUDE LFIPITOP
* and adjust logic with search term CH00
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        : Apichat C.
* Date          : 09.06.2025
* Change Request: 420000473
* Transport no. : F36K917933
* Search term   : CH01
* Description   : Accept button for popup with data contain manual
*                 witholding tax
***********************************************************************


*-----------------------------------------------------------------------
*        Tabellen / Strukturen
*-----------------------------------------------------------------------

*TABLES:  rfipi.                        " Arbeits- Schnittstellenfelder
TABLES: t001,                          " Buchungkreistabelle
        t005,                         " Ländertabelle
        t019w,                        " Window-Auswahl Buchhaltung
        t041a.                        " Ausgleichsvorgänge

TABLES: tstc,                          " SAP-Transaktions-Codes
        ttxd,                         " Struktur des Steuerstandortcode
        tbsl.                         " Buchungsschlüssel

TABLES:  skb1.                         " Sachkontenstamm (Buchungskreis)

TABLES: tcurx,           " Dezimalstellen der Währungen  " QHA950512
        trdir.          " Systemtabelle TRDIR           " QHA950512

TABLES:  t074u.

*------- Feldtabelle gesamt --------------------------------------------
DATA:    BEGIN OF ft OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ft.

*------- Feldtabelle Standard-Dynpros ----------------------------------
DATA:    BEGIN OF fta OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF fta.

*------- Feldtabelle CPD-Dynpro ----------------------------------------
DATA:    BEGIN OF ftc OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftc.

*------- Feldtabelle IBAN-Daten ----------------------------------------
DATA:    BEGIN OF ftiban OCCURS 0.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftiban.

* ------ Feldtabelle Mandatsreferenz -----------------------------------
DATA:    BEGIN OF ftmndid OCCURS 0.                            "N1923657
           INCLUDE STRUCTURE bdcdata.                          "N1923657
DATA:    END OF ftmndid.                                    "N1923657

*------- Feldtabelle COPA-Daten ----------------------------------------
DATA:    BEGIN OF ftcopa OCCURS 0.
           INCLUDE STRUCTURE copadata.
DATA:    END OF ftcopa.

*------- Feldtabelle ISIS-Daten ----------------------------------------
DATA:    BEGIN OF ftisis OCCURS 0.
           INCLUDE STRUCTURE copadata.
DATA:    END OF ftisis.

*------- Fieldtable for IS data, stored in generic string KONTL
DATA:    BEGIN OF ft_generic_kontl OCCURS 0.
           INCLUDE STRUCTURE copadata.
DATA:    END OF ft_generic_kontl.

*------- Feldtabelle Fußzeiledaten -------------------------------------
DATA:    BEGIN OF ftf OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftf.

*------- Feldtabelle Fußzeiledaten (nur für Konten) --------------------
DATA:    BEGIN OF ftfkto OCCURS 4.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftfkto.

*------- Feldtabelle Felder Public Sector ------------------------------
DATA:    BEGIN OF ftps   OCCURS 3.                        "30F
           INCLUDE STRUCTURE bdcdata.                     "30F
DATA:    END OF ftps.                                     "30F

*------- Feldtabelle Kontierungsblock-Dynpro ---------------------------
DATA:    BEGIN OF ftk OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftk.

*------- Feldtabelle Steuern-Dynpro ------------------------------------
DATA:    BEGIN OF ftt OCCURS 10 ##NEEDED.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftt.

*------- Feldtabelle Zusatz-Dynpro der Vermögensverwaltung -------------
DATA:    BEGIN OF ftvv OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftvv.

*------- Feldtabelle Kontierungsbl.Zusatz-Dynpro der Vermögensverwaltung
DATA:    BEGIN OF ftvk OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftvk.

*------- Feldtabelle Anlagen AfA-Anteile --------------------"KJV------
DATA:    BEGIN OF ftab OCCURS 10.                            "KJV
           INCLUDE STRUCTURE bdcdata.                        "KJV
DATA:    END OF ftab.                                        "KJV

*------- Field table for extended withholding tax ----------------------
DATA:    BEGIN OF ftw OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftw.

*------- Feldtabelle Zusatz-Dynpros ------------------------------------
DATA:    BEGIN OF ftz OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftz.

*------- Feldtabelle Betragssplitt------------------------------------
DATA:    BEGIN OF ftsplt OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftsplt.

*------- Feldtabelle Betragssplitt WT ---------------------------------
DATA:    BEGIN OF ftsplt_wt OCCURS 10.
           INCLUDE STRUCTURE bdcdata.
DATA:    END OF ftsplt_wt.

*------- Belegnummerntabelle -------------------------------------------
DATA:    BEGIN OF xbltab OCCURS 2.
           INCLUDE STRUCTURE blntab.
DATA:    END OF xbltab.

*------- Feldtabelle mit Ausgleichsdaten aus Schnittstelle -------------
DATA:    BEGIN OF xftclear OCCURS 50.
           INCLUDE STRUCTURE ftclear.
DATA:    END OF xftclear.

*------- Temporary Storage for data records without SFELD  -------------
DATA:    BEGIN OF yftclear .                                 "31i
           INCLUDE STRUCTURE ftclear.
DATA:    END OF yftclear.

*------- Feldtabelle mit BKPF- und BSEG-Daten aus Schnittstelle --------
DATA:    BEGIN OF xftpost OCCURS 50.
           INCLUDE STRUCTURE ftpost.
DATA:    END OF xftpost.

*------- Feldtabelle mit den Steuern -----------------------------------
DATA:    BEGIN OF xfttax OCCURS 10.
           INCLUDE STRUCTURE fttax.
DATA:    END OF xfttax.

*------- Feldtabelle mit den komprimierten Steuern --------------
DATA: BEGIN OF cxfttax OCCURS 10,
        mwskz  LIKE fttax-mwskz,
        txdat  TYPE txdat,
        bschl  LIKE fttax-bschl,
        txjcd  LIKE fttax-txjcd,
        kschl  LIKE fttax-kschl,
        pfwste LIKE bset-fwste,
        phwste LIKE bset-hwste,
        txkrs  TYPE txkrs_bkpf,                       " Note 564235
        ctxkrs TYPE bkpf-ctxkrs,                            "N2054102
        shkzg  LIKE bset-shkzg,                             "N2481799
      END OF cxfttax.

*------- itab with collected debits and credits for the same tax code
DATA: BEGIN OF cxfttax_taxcode OCCURS 10,                   "2481799
        mwskz  LIKE fttax-mwskz,
        txdat  TYPE txdat,
        txjcd  LIKE fttax-txjcd,
        kschl  LIKE fttax-kschl,
        pfwste LIKE bset-fwste,
        phwste LIKE bset-hwste,
        txkrs  TYPE txkrs_bkpf,                       " Note 564235
        ctxkrs TYPE bkpf-ctxkrs,                            "N2054102
      END OF cxfttax_taxcode.

*------- work itab  --------------
DATA: BEGIN OF cxfttax_tmp OCCURS 10,                       "2481799
        mwskz  LIKE fttax-mwskz,
        txdat  TYPE txdat,
        bschl  LIKE fttax-bschl,
        txjcd  LIKE fttax-txjcd,
        kschl  LIKE fttax-kschl,
        pfwste LIKE bset-fwste,
        phwste LIKE bset-hwste,
        txkrs  TYPE txkrs_bkpf,
        ctxkrs TYPE bkpf-ctxkrs,
        shkzg  LIKE bset-shkzg,
      END OF cxfttax_tmp.

*------- Tabelle XTBSL (Buchungsschlüssel) -----------------------------
DATA:    BEGIN OF xtbsl OCCURS 10.
           INCLUDE STRUCTURE tbsl.
DATA:    END OF xtbsl.

*------- Tabelle XT041A (Ausgleichsvorgänge) ---------------------------
DATA: BEGIN OF xt041a OCCURS 5,
        auglv LIKE t041a-auglv,
      END OF xt041a.

*------- Feldleiste mit den Steuern ------------------------------------
DATA:    BEGIN OF yfttax  ##NEEDED.
           INCLUDE STRUCTURE fttax.
DATA:    END OF yfttax.

*------- Feldleiste mit Textdynprodaten---------------------------------
DATA:    BEGIN OF fttxt OCCURS 2.             "2142438
           INCLUDE STRUCTURE bdcdata.         "2142438
DATA:    END OF fttxt.                                      "2142438
*eject

*------- Hilfsheader          ------------------------------------------

* Header IN_COBL beinhaltet die übergebenen COBL-Felder.
* Wird im FB RKE_FILL_BDCDATA_WITH_CRITERIA benötigt um zu verhindern,
* daß Kontierungsmerkmale auf das Dynpro SAPLKACB/0002 doppelt
* gesendet werden, falls man diese doppelt übergibt (in FTK und FTCOPA)
* Bsp. BBSEG-PRCTR und BBSEG-RKE_PRCTR wird gefüllt.
* (Falls man ein Feld doppelt auf ein Dynpro sendet, wird dieses Feld
* nicht mehr eingabebereit.
DATA in_cobl LIKE cobl.


*-----------------------------------------------------------------------
*        Einzelfelder
*-----------------------------------------------------------------------

*------- Einzelfelder Schnittstelle ------------------------------------
DATA: auglv        LIKE t041a-auglv,      " Ausgleichsvorgang
      augbl        LIKE rf05r-augbl,      " Ausgleichsbelegnummer
      belns        LIKE rf05a-belns,      " Belnr zu storn. Beleg
      bdcimmed     LIKE rfipi-bdcimmed,   " nur BDC: sof. Abspielen
      bdcstrtdt    LIKE tbtcjob-sdlstrtdt,  "nur BDC: Startdatum
      bdcstrttm    LIKE tbtcjob-sdlstrttm,  "nur BDC: Startzeit
      budat        TYPE bsis_view-budat,  " Budat Stornobeleg
      ftfkto_indx  LIKE sy-tabix,  "QHA   " Index merken für FTFKTO
      funct        LIKE rfipi-funct,      " Funktion
      group        LIKE apqi-groupid,     " Mappenname
      gjahr        LIKE rf05r-gjahr,      " Geschäftsjahr
      gjahs        LIKE rf05a-gjahs,      " Gjahr zu storn. Beleg
      holdd        LIKE apqi-startdate,   " Startdateum
      mandt        LIKE sy-mandt,         " Mandant
      mode(1)      TYPE c,                " Anzeigemodus
      monat        TYPE bsis_view-monat,  " Buchungsper Stornobeleg

      msgid        LIKE sy-msgid,         " Message-ID
      msgno        LIKE sy-msgno,         " Message-Nummer
      msgty        LIKE sy-msgty,         " Message-Typ
      msgv1        LIKE sy-msgv1,         " Message-Variable 1
      msgv2        LIKE sy-msgv2,         " Message-Variable 2
      msgv3        LIKE sy-msgv3,         " Message-Variable 3
      msgv4        LIKE sy-msgv4,         " Message-Variable 4

      mwskzs       LIKE skb1-mwskz,       " Steuerkategorie Sako

      queue_id     LIKE apqi-qid,         " BDC Unique Key

      sgfunct      LIKE rfipi-sgfunct,    " Single function
      subrc        LIKE sy-subrc,         " Returncode
      tcode        LIKE sy-tcode,         " Transakt.Code
      update(1)    TYPE c,                " Updatemodus
      usnam        LIKE apqi-userid,      " Username
      stgrd        LIKE uf05a-stgrd,      " Stornogrund
      voidr        LIKE rf05a-voidr,      " Ungültigkeitsgrund
      xbdcc        LIKE rfipi-xbdcc,      " X=BDC bei Error in C
      xkeep        LIKE apqi-qerase,      " X=Mappe halten
      bdc_app_area TYPE bdc_app_area,     " App Area for BI authorization check content: sm30: BDC_APP_AREAS
      xsimu        TYPE c.                " document simulation
*------- Hilfsfelder ---------------------------------------------------
DATA:    anbwa          LIKE bseg-anbwa ##NEEDED.    " Anlagenbewegungsart

DATA: bschl     LIKE bseg-bschl,    " Buchungsschlüssel
      bukrs     LIKE bkpf-bukrs,    " Buchungskreis
      blart     LIKE bkpf-blart,    " Belegart
      budat_int LIKE cobl-budat,    " date in internal format
      budat_wt  TYPE bsis_view-budat.

*DATA:    char20(20)     TYPE c.                                " QHATAX

DATA: defsize       TYPE c,         " N849676 X=Dynpro-Standardgröße
      dezzeichen(1) TYPE c,          " Dezimalzeichen
      dezstellen(1) TYPE n,          " Dezimalstellen
      dynnr         LIKE tstc-dypno.    " Standard-Dynpronummer

DATA:    fixpt          LIKE trdir-fixpt.   "Fixp.arithmetik
DATA:    fnam_konto     LIKE bdcdata-fnam.    "field name      "30E

DATA: glflex_active TYPE xfeld,                        "Note1605537
      group_open(1) TYPE c.             " X=Mappe schon geöffnet

DATA:    index          LIKE sy-tfill   .   " Tabellenindex

DATA: jobcount LIKE tbtco-jobcount, " Jobnummer
      jobname  LIKE tbtco-jobname. " Jobname

DATA: konto LIKE rf05a-newko,   " Kontonummer (17-stellig)
      knrze LIKE bbseg-knrze.   " Abweichende Zentrale

DATA: rcode(1) TYPE c,             " Return Code  " QHA950512
      regul    LIKE rf05a-regul.   " abweich. Regul. in Beleg

DATA:    loopc          LIKE sy-loopc.      " Loop-Zähler

DATA:    mpool          LIKE t019w-mpool    ##NEEDED.   " Modulpoolname

DATA:    runtime        TYPE i.             "Runtime

DATA: tabix_041a             TYPE i,            " Index T041A
      tfill_ftab             TYPE i,            " Anz. Einträge FTAB  "KJV
      tfill_ftc              TYPE i,            " Anz. Einträge in FTC
      tfill_ftiban           TYPE i,            " Anz. Einträge in FTIBAN.
      tfill_ftmndid          TYPE i,            " Anz. Einträge in FTMNDID
      tfill_ftcopa           TYPE i,            " Anz. Einträge in FTCopa
      tfill_ftisis           TYPE i,            " Anz. Einträge in FTISIS
      tfill_ft_generic_kontl TYPE i,     " no. of entries ......
      tfill_ftfkto           TYPE i,            " Anz. Einträge in FTFKTO
      tfill_ftk              TYPE i ##NEEDED,   " Anz. Einträge in FTK
      tfill_xfttax           TYPE i,            " Anz. Einträge in FTTAX
      tfill_ftvk             TYPE i,            " Anz. Einträge in FTVK
      tfill_ftvv             TYPE i,            " Anz. Einträge in FTVV
      tfill_ftw              TYPE i   ##NEEDED, " Anz. Einträge in FTW
      tfill_ftz              TYPE i,            " Anz. Einträge in FTZ
      tfill_041a             TYPE i,            " Anz. Einträge in XT041A
      tfill_ftsplt           TYPE i,            " Anz. Eintrage in FTSPLT
      tfill_fttxt            TYPE i.            " Anz. Einträge in FTTXT

DATA:    umskz          LIKE bseg-umskz.    " Sonderumsatzkennzeichen

DATA:    gv_tax_country TYPE fot_tax_country.

DATA: waers     LIKE bkpf-waers,    " Währung
      waers_old LIKE bkpf-waers,    " Währung      " QHA950512
      winfk     LIKE t019w-winfk,   " Window-Funktion (T019W)
      winnr     LIKE t019w-winnr.   " Window-Nummer

*        speichern von VBUND bei Eingabe auf Kopfebene
DATA:    vbund          LIKE bseg-vbund.                    "QHA941207

DATA:    xmwst          LIKE bkpf-xmwst.    " Steuer rechnen
DATA:    xmwst_set(1)   TYPE c.             " set BKPF-XMWST again
*---- send okcode /17 with FBV1
DATA:    send_ok17      TYPE flag.

*------- Konstanten ----------------------------------------------------
DATA:    rep_name(8)    TYPE c.
DATA:    rep_name_a(8)  TYPE c VALUE 'SAPMF05A'. " Mpool SAPMF05A
DATA:    rep_name_bv(8) TYPE c VALUE 'SAPLF040'. " Mpool SAPLF040 BVorEr
DATA:    rep_name_c(8)  TYPE c VALUE 'SAPLFCPD'. " Mpool SAPLFCPD (CPD)
DATA:    rep_name_iban(8) TYPE c VALUE 'SAPLIBMA'. " Mpool SAPLIBMA
DATA:    rep_name_mndt(19) TYPE c VALUE 'SAPLSEPA_MANDATE_UI'. "Mandate
DATA:    rep_name_k(8)  TYPE c VALUE 'SAPLKACB'. " Mpool SAPLKACB (CoBl)
DATA:    rep_name_t(8)  TYPE c VALUE 'SAPLTAX1'. " Mpool SAPLTAX1 (Taxes
DATA:    rep_name_r(8)  TYPE c VALUE 'SAPMF05R'. " Mpool SAPMF05R (FBRA)
*DATA:    rep_name_v(8)  TYPE c VALUE 'SAPLF014'. " Mpool SAPLF014(VBUND)
DATA:    rep_name_vk(8) TYPE c VALUE 'SAPLFVI8'. " Mpool SAPLFVI8(Vermög
DATA:    rep_name_vv(8) TYPE c VALUE 'SAPLFVI9'. " Mpool SAPLFVI9(Vermög
DATA:    rep_name_ab(8) TYPE c VALUE 'SAPLAINT'. " Mpool Anl.ant. "KJV
DATA:    rep_name_text(8) TYPE c VALUE 'SAPLFTXT'. "Text Screen


DATA: no_date LIKE  sy-datum        VALUE '        ',
      no_time LIKE  sy-uzeit        VALUE '      '.

DATA:    bkpf_bukrs      LIKE bkpf-bukrs.                  "Note 641889
DATA:    txkrs           LIKE fttax-txkrs.                 "note 1257048
DATA:    txdat           TYPE txdat.
DATA:    ctxkrs          LIKE fttax-ctxkrs.                 "N2054102
DATA:    gv_fulfilldate  TYPE bkpf-fulfilldate.

DATA: gl_tax_code_found,                                    "N1899628
      xfttax_count           LIKE xftpost-count,            "N1899628
      count_valid_gl_items   LIKE xftpost-count,            "N1899628
      g_ftpost_count         LIKE xftpost-count,            "N1899628
      g_ftpost_count_max     LIKE xftpost-count,            "N1899628
      jurcode_active,                                       "N1899628
      external_system_aktive,                               "N1899628
      tax_linewise,                                         "N1899628
      g_wt_filled.                                          "N2481799


FIELD-SYMBOLS: <ftpost1> TYPE ftpost,                       "N1899628
               <ftpost2> TYPE ftpost,                       "N1899628
               <ftpost3> TYPE ftpost,                       "N1899628
               <ftpost4> TYPE ftpost,                       "N1899628
               <ftpost5> TYPE ftpost.                       "N1899628

DATA: gv_mndid_ext TYPE sepa_param_value,                   "N1970773
      gv_crdid_ext TYPE sepa_param_value.                   "N1970773
CONSTANTS: gc_off TYPE sepa_param_value VALUE 'DISABLED'.   "N1970773

*----------------QR-BILL------------------------------*
*Note 2949101
TYPE-POOLS: abap.

DATA: gv_qr_act TYPE abap_bool,
      gv_bankn  TYPE ftpost-fval,
      gv_bankl  TYPE ftpost-fval,
      gv_banks  TYPE ftpost-fval,
      gv_bkont  TYPE ftpost-fval,
      gv_bkref  TYPE ftpost-fval,
      gv_bukrs  TYPE bukrs,
      gv_iban   TYPE tiban-iban.

DATA: gv_supplier_account TYPE bseg-lifnr.      "Note 3139261
DATA: lineitem_number        TYPE bseu-vozei,
      document_number        TYPE bkpf-belnr,
      debit_credit_indicator TYPE bseg-shkzg.
