***INCLUDE PSDBCMEM.

DATA: BEGIN OF COMMON PART %_SAPDBPSJ_PSDBCMEM.

CONSTANTS:
*                                      "Objekttypen  (s. RBONRART)
   CON_OBT_IMTP LIKE ELM_PS-OBTYP VALUE 'ID',    "Inv.Prog.
   CON_OBT_EQUI LIKE ELM_PS-OBTYP VALUE 'IE',    "Equipment
   CON_OBT_TECH LIKE ELM_PS-OBTYP VALUE 'IF',    "Techn.Platz
   CON_OBT_IMPR LIKE ELM_PS-OBTYP VALUE 'IP',    "Inv.prog.pos.
   CON_OBT_NETZ LIKE ELM_PS-OBTYP VALUE 'NP',    "Netzplan
   CON_OBT_ACT  LIKE ELM_PS-OBTYP VALUE 'NV',    "Netzplan-Vorgang
   CON_OBT_ELEM LIKE ELM_PS-OBTYP VALUE 'NE',    "Netzplan-Element
   CON_OBT_PLV  LIKE ELM_PS-OBTYP VALUE 'O1',    "Planauftragsvorgangng
   CON_OBT_COMP LIKE ELM_PS-OBTYP VALUE 'OK',    "Materialkomp/Reservrv
   CON_OBT_ORPO LIKE ELM_PS-OBTYP VALUE 'OP',    "Auftragsposition
   CON_OBT_ORDC LIKE ELM_PS-OBTYP VALUE 'OR',    "Auftrag
   CON_OBT_ORDV LIKE ELM_PS-OBTYP VALUE 'OV',    "PP-Vorgang
   CON_OBT_PROJ LIKE ELM_PS-OBTYP VALUE 'PD',    "Projektdefinition
   CON_OBT_PEG  LIKE ELM_PS-OBTYP VALUE 'PG',    "Pegging-Objekt
   CON_OBT_PRPS LIKE ELM_PS-OBTYP VALUE 'PR',    "PSP-Element
   CON_OBT_TEMP LIKE ELM_PS-OBTYP VALUE 'TM',    "temp. Objektnummer
   CON_OBT_VSPD LIKE ELM_PS-OBTYP VALUE 'V1',    "Version Projektdefin.
   CON_OBT_VSPR LIKE ELM_PS-OBTYP VALUE 'V2',    "Version PSP-Elemente
   CON_OBT_VSNP LIKE ELM_PS-OBTYP VALUE 'V3',    "Version Netzplan
   CON_OBT_VSNV LIKE ELM_PS-OBTYP VALUE 'V4',    "Version Netzplanvorg.
   CON_OBT_VSOK LIKE ELM_PS-OBTYP VALUE 'V5',    "Version Reservierung
   CON_OBT_VAOB LIKE ELM_PS-OBTYP VALUE 'V6',    "Version Anordnungsbez.
   CON_OBT_VKAP LIKE ELM_PS-OBTYP VALUE 'V7',    "Version Kapazitaetsb.
   CON_OBT_VSOS LIKE ELM_PS-OBTYP VALUE 'V8',    "Version Auftr.arb.folg
   CON_OBT_VSOP LIKE ELM_PS-OBTYP VALUE 'V9',    "Version Auftragspos.
   CON_OBT_VSOR LIKE ELM_PS-OBTYP VALUE 'VA',    "Version Order             "note 2193016
   CON_OBT_VSoV LIKE ELM_PS-OBTYP VALUE 'VV',    "Version Operation         "note 2193016

   CON_OBT_SDOR LIKE ELM_PS-OBTYP VALUE 'VB',    "Verkaufsbelegpos.
   CON_OBT_VERD LIKE ELM_PS-OBTYP VALUE 'VD',    "Verdichtungsobj.
   CON_OBT_SDHD LIKE ELM_PS-OBTYP VALUE 'VK',    "Verkaufsbelegkopf
   CON_OBT_PCTR LIKE ELM_PS-OBTYP VALUE '0106',  "Profit-Center
   CON_OBT_KSTL LIKE ELM_PS-OBTYP VALUE '0101',  "Kostenstelle
   CON_OBT_SET  LIKE ELM_PS-OBTYP VALUE 'SET',   "Set
   CON_OBT_SINT LIKE ELM_PS-OBTYP VALUE 'SINT'.  "Intervall
CONSTANTS:
  CNDBDIR_MEM_ID(32)        TYPE C VALUE 'SAP         DIRECTORY',
  CJDB_2_PSJ_MEM_ID(32)     TYPE C VALUE 'SAPDBPSJ     CJDB_SEL_FIELDS',
  PSJ_2_CJDB_MEM_ID(32)      TYPE C VALUE 'SAPDBPSJ     PSJ_RESULTS',
  CON_TMPDT_MEM_ID(32)       TYPE C VALUE 'SAPDBPSJ_CN_TMPDT',
  CON_TCNDB_TCNDS_MEM_ID(32) TYPE C VALUE 'SAPDBPSJ_CN_TCNDB',
  CON_SELECTION_MEM_ID(32)   TYPE C VALUE 'SAPDBPSJ_CN_SELECTION',
  CON_VSNMR_TEMP LIKE VSKOPF-VSNMR VALUE 'saptemporary'.

*---------------------------------------------------------------------*
* Flags
*---------------------------------------------------------------------*
* (Erklärung gilt für FLG_XXX = 'X')
*---------------------------------------------------------------------*
DATA:
      FLG_AUTHORITY_FROM_TABLE TYPE C,  "STORE_OBJ_CO_OBJ --> Authority
      GV_NO_AUTHORITY_CHECK    TYPE boole_d ,  "note 2437856
      FLG_CALL_INTERN          TYPE C,
      FLG_CALL_PSMERK_INTERN   TYPE C,  "note 371855
      FLG_CHANGED              TYPE C,
      FLG_CO_OBJ_NO_PRE_READ   TYPE C,  "kein Preread unterhalb ONR00
      FLG_DATA_SELECTED(1)     TYPE C,  "LDB hat Daten zu den Sel.krit.
                                        " gefunden
      FLG_DO_CHECK_DELETE(1)   TYPE C,  "gelöschte oder zur Löschung
                                        " vorgem. Objekte werden nicht
                                        " dargestellt
      FLG_CHECK_DEL_FOR_ELMPS  TYPE C,  "bei nichtkaufmännischer
                                        "Auswertung corep in elm_ps
                                        "setzen für Objekte, die aus CO
                                        "Sicht nicht relevant sind
                                        "(Status loe, loevm...)

      FLG_DO_DB_SELECTION(1)   TYPE C VALUE 'X',
                                        "es wird von der phys. DB sel.
                                        " sinnvoll, wenn andere Sicht
                                        " dargest. werden soll.
      FLG_DO_DB_SELECTION_TMP(1)   TYPE C VALUE 'X',
      FLG_DO_VIEW_CHECK(1)     TYPE C,  "View-Check wird ausgeschaltet,
                                        " nachdem die alternative
                                        " Hierarchie abgearbeitet ist
      FLG_DYN_SEL_INIT(1)      TYPE C,  "Freie Abgr. wurden initial.
      FLG_DYN_SEL_PRPS(1)      TYPE C,  "Freie Abgr. für PRPS aktiv
      FLG_DYN_SEL_PRTE(1)      TYPE C,  "Freie Abgr. für PRTE aktiv
      FLG_DYN_SEL_PSMERK       TYPE C,  "Freie Abgr. für PSMERK aktiv
      flg_put_psmerk           TYPE c, "to show call from put_psmerk
      FLG_FIRST_SEL_VERS(1)    TYPE C,  "erster Durchlauf für Sel.vers.
                                        "(beim 2. muß der Hierarchie-
                                        "aufbau wieder stattfinden)
      FLG_GET_CO_OBJ           TYPE C,  "GET für wenigst. eine Tabelle
                                        " im CO-Obj.
      FLG_GET_AFRU02           TYPE C,  "GET(_LATE) für AFRU02 vorh.
      FLG_GET_MLSTD            TYPE C,  "GET(_LATE) für MLSTD  vorh.
      FLG_GET_PRTE             TYPE C,  "GET(_LATE) für PRTE   vorh.
      FLG_GET_PSMLST           TYPE C,  "GET(_LATE) für PSMLST vorh.
      FLG_GET_JSTO             TYPE C,  "GET für Status vorh.
      FLG_GET_RPSCO            TYPE C,  "GET für RPSCO1 vorh.
      FLG_HIERARCHY_BUILT      TYPE C,  "RSTHIE ist bereits für
                                        " nächsten Aufruf korrekt gef.
      FLG_INDICES_EXIST        TYPE C,  "Sel.umfang wurde ermittelt
*      flg_itab_elem_changed    TYPE c,  "ITAB_ELEM wurde verkürzt
      FLG_JOB_OK               TYPE C,  "Jobeinplanung erfolgreich
      FLG_NO_HIERARCHY         TYPE C,  "Hierarchie wird nicht aufgebaut
      FLG_NO_STOP_INFO         TYPE C,  "space --> I011-Msg angez.
      FLG_NO_PUTS              TYPE C,  "nur PUT bei VSKOPF, ARKOPF
                                        " Performanceoption
      FLG_OBJECTS_ONLY         TYPE C,  "nur Objekte, keine Hierarchie
      FLG_PS_INFO_PROFIL       TYPE C,  "space --> DB-Profil
      FLG_READ_CAPACITIES(1)   TYPE C,  "Beim Lesen der Arb.pl. zum Vorg
                                        " werden Kapaz. mitgelesen
      FLG_READ_DATA_WITH_NO_SEL(1) TYPE C,
                                        "es werden alle Daten zu
                                        " einer Version/Archiv gelesen,
                                        " obwohl keine Sel.krit. einge-
                                        " geben sind.
      FLG_RESB                 TYPE C,  "RESB01 oder RESB02 sind vorh.
      FLG_SAVE_SEL_VERS(1)     TYPE C,  "Selektionsversion autom. sich.
      FLG_SAVE_NO_SEL_VERS(1)  TYPE C,  "Sel.vers. werden NICHT geschr.
*     flg_select_all_fields(1) TYPE c,  "Lesen von Orig.dat. mit
*                                       " SELECT *
      FLG_SEL_VERS(1)          TYPE C,  "Selektionsversion wird bearb.
      FLG_TCNDB_AUTO_MODIF(1)  TYPE C VALUE 'X',
                                        " TCNDB-Felder, für die kein GET
                                        " auf die entspr. Tab. exist.
                                        " werden initialisiert
      FLG_TCNDB_CHANGED(1)     TYPE C,  "im Dynpro SAPLCJDB0100 wurde
                                        " TCNDB geändertB0100 das
      FLG_NEW_PROFILE          TYPE C,
      FLG_PRIPARAMS_VALID      TYPE C,  "Druckparams OK
      FLG_NO_DATABASE          TYPE C,  "Bei PUT ARKOPF wird LDB verl.
      FLG_NO_SELECT            TYPE C,
*      flg_drad_comp            TYPE c,  "Doku zum Component
      FLG_DRAD_EBAN_PRPS       TYPE C,  "Doku vom Banf zum PSP
      FLG_DRAD_EBAN_NETZ       TYPE C,  "Doku vom Banf zum Netzplan
      FLG_DRAD_EBAN_ACT        TYPE C,  "Doku vom Banf zum Vorgang
      FLG_DRAD_EBAN_COMP       TYPE C,  "Doku vom Banf zum Component
      FLG_DRAD_EKPO_PRPS       TYPE C,  "Doku vom Best. zum PSP
      FLG_DRAD_EKPO_NP         TYPE C,  "Doku vom Best. zum Netzplan
                                        "                   Vorgang
      BEGIN OF FLG_LATE OCCURS 0,       "GET .... LATE wurde proz.
        PSDYIP(1) TYPE C,
        PSDYNP(1) TYPE C,
        PSDYNV(1) TYPE C,
        PSDYPD(1) TYPE C,
        PSDYPG(1) TYPE C,
        PSDYPR(1) TYPE C,
        PSDYVB(1) TYPE C,
        PSDYVK(1) TYPE C,
        PSDYOK(1) TYPE C,
        PSDYPEG(1) TYPE C,
      END OF FLG_LATE,
      BEGIN OF FLG_PSDY,                "wird für CHECK bei GET benötigt
        PSDYIP(1) TYPE C,
        PSDYNP(1) TYPE C,
        PSDYNV(1) TYPE C,
        PSDYPD(1) TYPE C,
        PSDYPG(1) TYPE C,
        PSDYPR(1) TYPE C,
        PSDYVB(1) TYPE C,
        PSDYVK(1) TYPE C,
        PSDYOK(1) TYPE C,
        PSDYPEG(1) TYPE C,
      END OF FLG_PSDY.

DATA: CNDBDIR_WA  LIKE CNDBDIR.
DATA: MEM_ID      LIKE PSJ_MEM_ID  VALUE 'SAPDBPSJ'.
* Behandlung der TCNDB-Werte in Abhängigkeit der GETs im Selektionsrep.
DATA: TCNDB_MODIF LIKE TCNDB.
DATA: CN_TCNDB    LIKE TCNDB,
      CN_TCNDS    LIKE TCNDS,
      CN_TMPDT    TYPE C,
      CN_TMPSL    TYPE C,
      TMP_NO_DATA_BASE TYPE C,
      TMPDT_TABIX LIKE SY-TABIX.
DATA: BEGIN OF FLG_SELECTION_TMP.
        INCLUDE STRUCTURE TCNDB.
DATA:   NETZ_PROJ     LIKE TCNDB-NETZ_PSP,  "Netzpläne zur Projektdef.
        AUFK_PSP      LIKE TCNDB-NETZ_PSP,  "Aufträge zum PSP
        NETZ_ACT      LIKE TCNDB-ACT,       "Vorgänge zur Netzpläne
        AUFK_ACT      LIKE TCNDB-ACT,       "Vorgänge zur Aufträge
        AUFK          LIKE TCNDB-NETZ,      "Aufträge
        APL           LIKE TCNDB-NETZ,      "AUFNR zu AUFPL
        AFFL          LIKE TCNDB-NETZ,      "Werkauftragsfolge
        PRTE          LIKE TCNDB-PRPS,      "Terminierungsdaten
        PRHI          LIKE TCNDB-PRPS,      "Terminierungsdaten
        SEL_MODE      TYPE C,               "Selektionsmode
      END OF FLG_SELECTION_TMP.
DATA: CN_SELECTION LIKE FLG_SELECTION_TMP.

*---------------------------------------------------------------------*
* interne Tabellen
*---------------------------------------------------------------------*
DATA: BEGIN OF KEY_ACT01_VORNR OCCURS 0,
        AUFPL LIKE ACT01-AUFPL,
        APLZL LIKE ACT01-APLZL,
        SUMNR LIKE ACT01-SUMNR,
        VORNR LIKE ACT01-VORNR,
        TABIX LIKE SY-TABIX,
      END OF KEY_ACT01_VORNR.
DATA: BEGIN OF KEY_AFAB01 OCCURS 0,
        AUFPL_VOR LIKE AFAB01-AUFPL_VOR,
      END OF KEY_AFAB01.
DATA: BEGIN OF KEY_AFFH01 OCCURS 0,
        AUFPL LIKE AFFH01-AUFPL,
        APLZL LIKE AFFH01-APLZL,
        OBJNR LIKE AFFH01-OBJNR,
        OBJTY LIKE AFFH01-OBJTY,
        OBJID LIKE AFFH01-OBJID,
      END OF KEY_AFFH01.
DATA: BEGIN OF KEY_AFFL  OCCURS 0,
        AUFPL LIKE AFFL-AUFPL,
      END OF KEY_AFFL.
DATA: BEGIN OF KEY_AFIH OCCURS 0,
        AUFNR LIKE AFIH-AUFNR,
      END OF KEY_AFIH.
DATA: BEGIN OF KEY_AFPO OCCURS 0,
        AUFNR LIKE AFPO-AUFNR,
      END OF KEY_AFPO.
DATA: BEGIN OF KEY_AFRU OCCURS 0,
        RUECK LIKE AFRU-RUECK,
        AUFPL LIKE AFRU-AUFPL,
      END OF KEY_AFRU.
DATA: BEGIN OF KEY_EBKN_NP OCCURS 0,
        BANFN LIKE EBAN-BANFN,
        BNFPO LIKE EBAN-BNFPO,
      END OF KEY_EBKN_NP.
DATA: BEGIN OF KEY_EBKN_PR OCCURS 0,
        BANFN LIKE EBAN-BANFN,
        BNFPO LIKE EBAN-BNFPO,
      END OF KEY_EBKN_PR.
DATA: BEGIN OF KEY_EKBE_NP OCCURS 0,
        EBELN      LIKE EKBE-EBELN,
        EBELP      LIKE EKBE-EBELP,
        zekkn      like ekbe-zekkn,
      END OF KEY_EKBE_NP.
DATA: KEY_EKBE_PR LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: KEY_EKET_NP LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: KEY_EKET_PR LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: KEY_EKKN_NP LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: KEY_EKKN_PR LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF KEY_EKKO OCCURS 0,
        EBELN      LIKE EKKO-EBELN,
      END OF KEY_EKKO.
DATA: KEY_EKPO_NP LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: KEY_EKPO_PR LIKE KEY_EKBE_NP OCCURS 0 WITH HEADER LINE.
DATA: WA_DRAD_EKPO_PR LIKE KEY_EKPO_PR OCCURS 0 WITH HEADER LINE.
DATA: WA_DRAD_EKPO_NP LIKE KEY_EKPO_NP OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF KEY_ELEM  OCCURS 0,
        AUFPL LIKE ACT01-AUFPL,
        APLZL LIKE ACT01-APLZL,
        SUMNR LIKE ACT01-SUMNR,
        VORNR LIKE ACT01-VORNR,
        OBJNR LIKE ACT01-OBJNR,
        PROJN LIKE ACT01-PROJN,
        PRCTR LIKE ACT01-PRCTR,
        TABIX LIKE SY-TABIX,
        USED  TYPE C,
      END OF KEY_ELEM.
DATA: BEGIN OF KEY_KBED01 OCCURS 0,
        BEDID  LIKE KBED01-BEDID,
        BEDZL  LIKE KBED01-BEDZL,
        CANUM  LIKE KBED01-CANUM,
        CANUMF LIKE KBED01-CANUMF,
      END OF KEY_KBED01.
DATA: BEGIN OF KEY_KBED04 OCCURS 0,
        BEDID  LIKE KBED04-BEDID,
        BEDZL  LIKE KBED04-BEDZL,
        PLNUM  LIKE KBED04-PLNUM,
        CANUM  LIKE KBED04-CANUM,
        CANUMF LIKE KBED04-CANUMF,
      END OF KEY_KBED04.
DATA: BEGIN OF KEY_KBEZ OCCURS 0,
        BEDID LIKE KBEZ-BEDID,
        BEDZL LIKE KBEZ-BEDZL,
        CANUM LIKE KBEZ-CANUM,
      END OF KEY_KBEZ.
DATA: BEGIN OF KEY_MLSTD OCCURS 0,
        PSPNR LIKE PSMLST-PSPNR,
        AUFPL LIKE MLSTD-AUFPL,
        APLZL LIKE MLSTD-APLZL,
      END OF KEY_MLSTD.
DATA: BEGIN OF KEY_NEPO OCCURS 0,
        AUFNR LIKE AFPO-AUFNR,
      END OF KEY_NEPO.
DATA: BEGIN OF KEY_PLAF OCCURS 0,
        PLNUM LIKE PLAF-PLNUM,
        PSPEL LIKE PLAF-PSPEL,
        RSNUM LIKE PLAF-RSNUM,
        BEDID LIKE PLAF-BEDID,
      END OF KEY_PLAF.
DATA: BEGIN OF KEY_PRTE OCCURS 0,
        POSNR LIKE PRTE-POSNR,
        PSPHI LIKE PRTE-PSPHI,
      END OF KEY_PRTE.
DATA: BEGIN OF KEY_PSMLST OCCURS 0,
        PSPNR LIKE PSMLST-PSPNR,
        AUFPL LIKE MLSTD-AUFPL,
        APLZL LIKE MLSTD-APLZL,
      END OF KEY_PSMLST.
DATA: BEGIN OF KEY_PSTX OCCURS 0,
        PSTXTKY LIKE PSTX-PSTXTKY,
      END OF KEY_PSTX.
DATA: BEGIN OF KEY_PSTX1 OCCURS 0,
        PSTXTKY LIKE PSTX1-PSTXTKY,
      END OF KEY_PSTX1.
DATA: BEGIN OF KEY_RESB01 OCCURS 0,
        RSNUM LIKE RESB01-RSNUM,
        RSPOS LIKE RESB01-RSPOS,
        RSART LIKE RESB01-RSART,
        NO_DISP LIKE RESB01-NO_DISP,
        DBSKZ LIKE RESB01-DBSKZ,
        FLGEX LIKE RESB01-FLGEX,
        AUFPL LIKE RESB01-AUFPL,
        APLZL LIKE RESB01-APLZL,
        OBJNR LIKE RESB01-OBJNR,
        KZBWS LIKE RESB01-KZBWS,
        PSPEL LIKE RESB01-PSPEL,
        XLOEK LIKE RESB01-XLOEK,
      END OF KEY_RESB01.
DATA: KEY_RESB01_BWS LIKE KEY_RESB01 OCCURS 0 WITH HEADER LINE.
DATA: KEY_RESB01_BWS2 LIKE KEY_RESB01 OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF WA_KEY_RESB01,
        RSNUM LIKE RESB01-RSNUM,
        RSPOS LIKE RESB01-RSPOS,
      END OF WA_KEY_RESB01.
DATA: BEGIN OF KEY_VBAK OCCURS 0,
        VBELN      LIKE VBAK-VBELN,
        OBJNR      LIKE VBAK-OBJNR,
        VBTYP      LIKE VBAK-VBTYP,
        WAERK      LIKE VBAK-WAERK,
        PS_PSP_PNR LIKE VBAK-PS_PSP_PNR,
        KOKRS      LIKE VBAK-KOKRS,
        BUKRS_VF   LIKE VBAK-BUKRS_VF,
      END OF KEY_VBAK.
DATA: BEGIN OF KEY_VBAP OCCURS 0,
        VBELN LIKE VBAP-VBELN,
        POSNR LIKE VBAP-POSNR,
        OBJNR LIKE VBAP-OBJNR,
        PS_PSP_PNR LIKE VBAP-PS_PSP_PNR,
        WAERK LIKE VBAP-WAERK,
        ARKTX LIKE VBAP-ARKTX,
      END OF KEY_VBAP.

DATA: BEGIN OF KEY_RESB01_BWS_INDX OCCURS 0,
        OBJNR LIKE RESB01-OBJNR,
        TABIX LIKE SY-TABIX,
      END OF KEY_RESB01_BWS_INDX.
DATA: BEGIN OF KEY_RESB01_INDX OCCURS 0,
        AUFPL LIKE RESB01-AUFPL,
        APLZL LIKE RESB01-APLZL,
        rsnum LIKE resb01-rsnum,
        TABIX LIKE SY-TABIX,
      END OF KEY_RESB01_INDX.

DATA: BEGIN OF INDX_ACT01 OCCURS 0,
        AUFPL LIKE ACT01-AUFPL,
        START LIKE SY-TABIX,
        STOP  LIKE SY-TABIX,
      END OF INDX_ACT01.
DATA: BEGIN OF INDX_LIPS OCCURS 0,
        PSPNR LIKE PROJ-PSPNR,
        VBELN LIKE LIPS-VBELN,
        POSNR LIKE LIPS-POSNR,
        INDEX LIKE SY-TABIX,
      END OF INDX_LIPS.
DATA: BEGIN OF INDX_PRPS_R OCCURS 0,
        PSPHI LIKE PRPS_R-PSPHI,
        START LIKE SY-TABIX,
        STOP  LIKE SY-TABIX,
      END OF INDX_PRPS_R.
DATA: BEGIN OF INDX_PRPS_CO OCCURS 0,
        PSPHI LIKE PRPS_R-PSPHI,
        START LIKE SY-TABIX,
        STOP  LIKE SY-TABIX,
      END OF INDX_PRPS_CO.

DATA: I_ELM_PS           LIKE ELM_PS     OCCURS 0 WITH HEADER LINE.
DATA: I_ELM_PS_ALL       LIKE ELM_PS     OCCURS 0 WITH HEADER LINE.
DATA: I_RSTHIE           LIKE RSTHIE     OCCURS 0 WITH HEADER LINE.
DATA: I_RSTHIE_ALL       LIKE RSTHIE     OCCURS 0 WITH HEADER LINE.
DATA: I_RSTHIE2          LIKE RSTHIE     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PSJ_HIE_INDX  LIKE PSJ_HIE_INDX OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFFL          LIKE AFFL       OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_AFAB                 OCCURS 0.
        INCLUDE STRUCTURE AFAB.
DATA:   AUFNR_VOR LIKE AFKO-AUFNR,
        VORNR_VOR LIKE AFVGD-VORNR,
        LTXA1_VOR LIKE AFVGD-LTXA1,
        AUFNR_NCH LIKE AFKO-AUFNR,
        VORNR_NCH LIKE AFVGD-VORNR,
        LTXA1_NCH LIKE AFVGD-LTXA1,
        AUFPL     LIKE AFKO-AUFPL.
DATA: END OF ITAB_AFAB.
DATA: BEGIN OF ITAB_AFBA                 OCCURS 0,
        AUFPL LIKE AFVGB-AUFPL,
        APLZL LIKE AFVGB-APLZL,
        BANFN LIKE AFVGB-BANFN,
        BNFPO LIKE AFVGB-BNFPO,
        EBELN LIKE M_MEKKE-EBELN,
        EBELP LIKE M_MEKKE-EBELP,
      END OF ITAB_AFBA.
DATA: BEGIN OF ITAB_AFBA_I               OCCURS 0,
        AUFPL LIKE AFVGB-AUFPL,
        APLZL LIKE AFVGB-APLZL,
        BANFN LIKE AFVGB-BANFN,
        BNFPO LIKE AFVGB-BNFPO,
      END OF ITAB_AFBA_I.
DATA: ITAB_AFFH          LIKE AFFH       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFFH01        LIKE AFFH01     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFFHD         LIKE AFFHD      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFIH          LIKE AFIH       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFKO          LIKE AFKO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFPO          LIKE AFPO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFRU          LIKE AFRU       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_AFRU01        LIKE AFRU01     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_ACT01         LIKE ACT01      OCCURS 0 WITH HEADER LINE.
*                                    AUFPL    <--> AUFNR
DATA: BEGIN OF ITAB_AFVG_STD             OCCURS 0,
        AUFPL   LIKE AFVC-AUFPL,
        PLNAL   LIKE AFVC-PLNAL,
        APLZL   LIKE AFVC-APLZL,
        VORNR   LIKE AFVC-VORNR,
      END OF ITAB_AFVG_STD.
DATA: ITAB_AUFK          LIKE AUFK       OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_DRAD_NP              OCCURS 0.
        INCLUDE STRUCTURE DRAD.
DATA:   AUFNR LIKE AUFK-AUFNR,
        VORNR LIKE ACT01-VORNR,
        PRPS_INT LIKE ELM_PS-PRPS_INT.
DATA: END OF ITAB_DRAD_NP.
DATA: ITAB_DRAD_PR     LIKE ITAB_DRAD_NP OCCURS 0 WITH HEADER LINE.
DATA: ITAB_DRAD_FHM    LIKE ITAB_DRAD_NP OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_DRAD_COMP            OCCURS 0.
        INCLUDE STRUCTURE DRAD.
DATA:   RSNUM LIKE RESB01-RSNUM,
        RSPOS LIKE RESB01-RSPOS.
DATA: END OF ITAB_DRAD_COMP.
DATA: BEGIN OF ITAB_DRAD_EBAN_PRPS       OCCURS 0.
        INCLUDE STRUCTURE DRAD.
DATA:   BANFN LIKE EBAN-BANFN,
        BNFPO LIKE EBAN-BNFPO.
DATA: END OF ITAB_DRAD_EBAN_PRPS.
DATA: ITAB_DRAD_EBAN_NETZ LIKE ITAB_DRAD_EBAN_PRPS
                                         OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_DRAD_EKPO_PRPS       OCCURS 0.  "Doku zum
        INCLUDE STRUCTURE DRAD.                     "Einkaufsbeleg
DATA:   EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP.
DATA: END OF ITAB_DRAD_EKPO_PRPS.
DATA: ITAB_DRAD_EKPO_NP LIKE ITAB_DRAD_EKPO_PRPS
                                         OCCURS 0 WITH HEADER LINE.
DATA: ITAB_EBAN_NP     LIKE EBAN         OCCURS 0 WITH HEADER LINE.
DATA: ITAB_EBAN_PR     LIKE EBAN         OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_EKBEL_NP             OCCURS 0,
        RSNUM      LIKE RESB-RSNUM,
        RSPOS      LIKE RESB-RSPOS,
        AUFPL      LIKE AFVGB-AUFPL,
        APLZL      LIKE AFVGB-APLZL,
        NPLNR      LIKE EKKN-NPLNR,
        PS_PSP_PNR LIKE PRPS-PSPNR,
        BANFN      LIKE AFVGB-BANFN,
        BNFPO      LIKE AFVGB-BNFPO,
        EBELN      LIKE M_MEKKE-EBELN,
        EBELP      LIKE M_MEKKE-EBELP,
        zekkn      like ekkn-zekkn,
      END OF ITAB_EKBEL_NP.
DATA: ITAB_EKBEL_PR   LIKE ITAB_EKBEL_NP OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_EKKO                 OCCURS 0.
        INCLUDE STRUCTURE EKKO.
DATA:   AUFPL LIKE ACT01-AUFPL,
        APLZL LIKE ACT01-APLZL,
        PS_PSP_PNR LIKE PRPS-PSPNR.
DATA: END OF ITAB_EKKO.
DATA: ITAB_EVFG          LIKE EVFG       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_EVOF_P        LIKE EVOF_P     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_EV_PARAM      LIKE EVOP       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_FPLA          LIKE FPLA       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_FPLAPS_NP     LIKE FPLA       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_FPLAPS_PR     LIKE FPLA       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_FPLT          LIKE FPLT       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_FPLTPS_NP     LIKE FPLT       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_FPLTPS_PR     LIKE FPLT       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_IMPR          LIKE IMPR       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_IMTP          LIKE IMTP       OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_JEST_VKNT            OCCURS 0,
        OBJNR LIKE JEST-OBJNR,
        INACT LIKE JEST-INACT,
      END OF ITAB_JEST_VKNT.
DATA: ITAB_KBED01        LIKE KBED01     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_KBED04        LIKE KBED04     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_KBEZ          LIKE KBEZ       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_KPER          LIKE KPER       OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_LIKP OCCURS 0.
        INCLUDE STRUCTURE LIKP.
DATA:   PSPNR   LIKE PROJ-PSPNR,
      END OF ITAB_LIKP.
DATA: ITAB_LIPS          LIKE LIPS       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_MLSTD         LIKE MLSTD      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PSMLST        LIKE MLSTD      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PSMERK        LIKE PSMERK     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_NEKO          LIKE AFKO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_NEPO          LIKE AFPO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_NETZ          LIKE AUFK       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_NPTX          LIKE NPTX       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PEGOB         LIKE PEGOB      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PEGQTY        LIKE PEGQTY     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLAB          LIKE PLAB       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLAF          LIKE PLAF       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLFH          LIKE PLFH       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLFL          LIKE PLFL       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLKO          LIKE PLKO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLPO          LIKE PLPO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PLMZ          LIKE PLMZ       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PROJ          LIKE PROJ       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PRPS_R        LIKE PRPS_R     OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PRPS_CO       LIKE PRPS_CO    OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PRPS_CO_MEM   LIKE PRPS_CO    OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PRTE          LIKE PRTE       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PRTX          LIKE PRTX       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PSTX          LIKE PSTX       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PSTX1         LIKE PSTX1      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_PNTX          LIKE PSTX       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_RESB          LIKE RESBD      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_RESB01        LIKE RESBD      OCCURS 0 WITH HEADER LINE.
*DATA: ITAB_REVNA         LIKE REVN       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_RPSCO         LIKE RPSCO      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_RPSqt         LIKE RPSqt      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_RSDB          LIKE RSDB       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_RSTHIE        LIKE RSTHIE     OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_SD_PRO               OCCURS 0,
        PSPNR LIKE PRPS-PSPNR,           "PSP-Nr <-> Projektdefinition
        PSPHI LIKE PRPS-PSPHI,           "für Fakturaelemente
      END OF ITAB_SD_PRO.
DATA: ITAB_STPO          LIKE STPO       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_VBAK          LIKE VBAK       OCCURS 0 WITH HEADER LINE.
DATA: WA_VBAK            LIKE VBAK.
DATA: ITAB_VBAP          LIKE VBAP       OCCURS 0 WITH HEADER LINE.
DATA: WA_VBAP            LIKE VBAP.
DATA: ITAB_VBKD          LIKE VBKD       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_VBUK          LIKE VBUK       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_VBUP          LIKE VBUP       OCCURS 0 WITH HEADER LINE.
DATA: ITAB_VGAUT         LIKE AFVGD      OCCURS 0 WITH HEADER LINE.
DATA: ITAB_VSKOPF        LIKE VSKOPF     OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF STORED_MEM_IDS            OCCURS 0,
        MEM_ID        LIKE MEM_ID,
        TABLENAME(60) TYPE C,
      END OF STORED_MEM_IDS.
DATA: STORE_OBJ_CO_OBJ   LIKE PSJ_AUTH   OCCURS 0 WITH HEADER LINE.
DATA: STORE_OBJ_CO_OBJ_ACT
                         LIKE PSJ_AUTH   OCCURS 0 WITH HEADER LINE.
DATA: STORE_OBJ_RPSCO TYPE TABLE OF JSTO_PRE
                      WITH HEADER LINE.
DATA: STORE_OBJ_SEL_SCHEME TYPE TABLE OF REF_JSTO
                           WITH HEADER LINE.
DATA: BEGIN OF SD_AKT_BEL                OCCURS 0,
        MANDT LIKE VBAP-MANDT,
        VBELN LIKE VBAP-VBELN,
        POSNR LIKE VBAP-POSNR,
        VBTYP LIKE VBAK-VBTYP,
      END OF SD_AKT_BEL.
DATA: BEGIN OF STORED_ITAB_ELEM          OCCURS 0,
        AUFPL             LIKE AFVC-AUFPL,
        FLG_ENTRIES_EXIST TYPE C,
      END OF STORED_ITAB_ELEM.
DATA: GT_VERSIONEN LIKE PSJ_VERSIONEN OCCURS 0,
      g_versionen  like psj_versionen,
      g_work_wo_memory type boole_d.

DATA: S_ITAB_TABIX LIKE SY-TABIX.

* für alternative Hierarchien
DATA: I_RSTHVKEY_ALT     LIKE RSTHVKEY   OCCURS 0 WITH HEADER LINE,
      I_RSTHIE_ALT       LIKE RSTHIE     OCCURS 0 WITH HEADER LINE,
      I_RSTINT_ALT       LIKE RSTINT     OCCURS 0 WITH HEADER LINE,
      I_RSTTYP_ALT       LIKE RSTTYP     OCCURS 0 WITH HEADER LINE,
      I_RSTADD_ALT       LIKE RSTADD     OCCURS 0 WITH HEADER LINE,
      I_RSTTXT_ALT       LIKE RSTTXT     OCCURS 0 WITH HEADER LINE,
      I_V01_TBL_ALT      LIKE V01CKRCO   OCCURS 0 WITH HEADER LINE,
      I_INFO_TBL_ALT     LIKE KKRVDCOOBJ OCCURS 0 WITH HEADER LINE.

* Indexstrukturen
*                                       PSP      <--> Netzvorgänge
DATA: ITAB_AFVCP         LIKE AFVCP      OCCURS 0 WITH HEADER LINE,
*                                       SPSP     <--> SNETZVORGÄNGE
      BEGIN OF ITAB_AFVCP_S OCCURS 0,
        PLNNR LIKE PLPO-PLNNR,
        PLNKN LIKE PLPO-PLNKN,
        PSPNR LIKE PLPO-PSPNR,
      END OF ITAB_AFVCP_S,
*                                       SD-Belege<--> Netzplan
      ITAB_AUKOD         LIKE M_AUKOD    OCCURS 0 WITH HEADER LINE,
*                                       Netzplan <--> Projektdef.
      ITAB_NET_PDEF      LIKE NET_PDEF   OCCURS 0 WITH HEADER LINE,
*                                       Netzplan <--> PSP-Elemente
      ITAB_NET_PSP       LIKE NET_PSP    OCCURS 0 WITH HEADER LINE,
*                                       Netzplan <--> SD-Belege
      ITAB_NET_SD        LIKE NET_SD     OCCURS 0 WITH HEADER LINE,
*                                       Top-Auftrag Auftr.netz <--> Auft
      ITAB_ORDNA         LIKE M_ORDNA    OCCURS 0 WITH HEADER LINE,
      ITAB_ORDNA_TMP     LIKE M_ORDNA    OCCURS 0 WITH HEADER LINE,
*>>> note 1645524
      ITAB_PRPOA         TYPE SORTED TABLE OF M_PRPOA
                           WITH UNIQUE key aufnr
                           WITH NON-UNIQUE SORTED key wbs_key COMPONENTS pronr
                           WITH HEADER LINE,
*                                       PSP      <--> CO-Aufträge
      ITAB_PRPOB         TYPE SORTED TABLE OF M_PRPOB
                           WITH NON-UNIQUE KEY aufnr              "note 1765496
                           WITH NON-UNIQUE SORTED key wbs_key COMPONENTS projn
                           WITH HEADER LINE,
*                                       PSP      <--> Netzpläne
      ITAB_PRPOC         TYPE SORTED TABLE OF M_PRPOB
                           WITH NON-UNIQUE KEY aufnr              "note 1765496
                           WITH NON-UNIQUE SORTED key wbs_key COMPONENTS projn
                           WITH HEADER LINE,
*<<< note 1645524
      BEGIN OF ITAB_PRPOC_S OCCURS 0,
        MANDT LIKE PLKO-MANDT,
        PSPNR LIKE PLKO-PSPNR,
        PLNNR LIKE PLKO-PLNNR,
        PLNAL LIKE PLKO-PLNAL,
      END OF ITAB_PRPOC_S,
*                                       PSP      <--> PP-Aufträge
      ITAB_PRPOD         LIKE M_PRPOD    OCCURS 0 WITH HEADER LINE,
*                                       Teilnetz <--> übgeord.Netz
*     itab_vallc         LIKE vallc      OCCURS 0 WITH HEADER LINE,
      BEGIN OF ITAB_VALLC OCCURS 0,
        MANDT LIKE AFKO-MANDT,
        AUFNR LIKE AFKO-AUFNR,
        AUFNT LIKE AFKO-AUFNT,
        AUFPT LIKE AFKO-AUFPT,
        APLZT LIKE AFKO-APLZT,
      END OF ITAB_VALLC,
*                                       SD-Belege<--> PSP
      ITAB_VMPAA         LIKE VMPA      OCCURS 0 WITH HEADER LINE,
*                                       PSP      <--> SD-Belege
      ITAB_VMPAB         LIKE VMPA      OCCURS 0 WITH HEADER LINE,
*                                       BANF <--> Auftrag
      ITAB_MBANG         LIKE M_MBANG    OCCURS 0 WITH HEADER LINE,
*                                       BANF <--> Netzplan
      ITAB_MBANN         LIKE M_MBANN    OCCURS 0 WITH HEADER LINE,
*                                       EBL  <--> Banf
      ITAB_MEKKE         LIKE M_MEKKE    OCCURS 0 WITH HEADER LINE,
*                                       EBL  <--> Auftrag
      ITAB_MEKKG         LIKE M_MEKKG    OCCURS 0 WITH HEADER LINE,
*                                       EBL  <--> Netzplan
      ITAB_MEKKN         LIKE M_MEKKN    OCCURS 0 WITH HEADER LINE,
*                                      "EBL  <--> PSP
      BEGIN OF ITAB_MEKKP                OCCURS 0,
        EBELN LIKE M_MEKKP-EBELN,
        EBELP LIKE M_MEKKP-EBELP,
        ZEKKN LIKE M_MEKKP-ZEKKN,
        PS_PSP_PNR LIKE M_MEKKP-PS_PSP_PNR,
      END OF ITAB_MEKKP.

DATA: ACTUAL_TIME            LIKE SY-UZEIT,
      ACTUAL_ELEM            LIKE AFVC-AUFPL,
      CN_PROCESS             TYPE C,
      RUN_TIME               TYPE I,
      BEGIN OF SEL_SCHEME_REC OCCURS 0,
        PROJ LIKE TJ48-SELID,
        PRPS LIKE TJ48-SELID,
        AUFK LIKE TJ48-SELID,
        AFVG LIKE TJ48-SELID,
      END OF SEL_SCHEME_REC,

      START_TIME             LIKE SY-UZEIT,
      TIME_OUT               TYPE I,
      TIME_OUT_STRING(5)     TYPE C.

* start of note 1747890
TYPES: BEGIN OF t_pm_order_netzkont,
         aufnr    TYPE aufk-aufnr,
         netzkont TYPE afko-netzkont,
       END OF t_pm_order_netzkont.
DATA: gt_pm_order_netzkont TYPE TABLE OF t_pm_order_netzkont.
DATA: gv_oaa_order_found TYPE boole_d.
* end of note 1747890

*---------------------------------------------------------------------*
* Strings
*---------------------------------------------------------------------*
DATA: BEGIN OF LINES,
        PROJ  LIKE SY-TFILL,
        VBEL  LIKE SY-TFILL,
        IMTP  LIKE SY-TFILL,
        PRPS  LIKE SY-TFILL,
        NETZ  LIKE SY-TFILL,
        ACT   LIKE SY-TFILL,
        KOMP  LIKE SY-TFILL,
      END OF LINES.
DATA: EV_FLG_FOR_RCNVS000(19)    TYPE C
                                 VALUE 'SAPDBPSJ     EV_FLG',
      EV_BUF_VERSION(23)         TYPE C
                                 VALUE 'SAPDBPSJ     EV_BUF_NEW',
      STATUS_BUF_VERSION(23)     TYPE C
                                 VALUE 'SAPDBPSJ     STATUS_BUF',
      RPSCO_BUF_VERSION(22)      TYPE C
                                 VALUE 'SAPDBPSJ     RPSCO_BUF',
      RPSQT_BUF_VERSION(22)      TYPE C
                                 VALUE 'SAPDBPSJ     RPSQT_BUF',
      COMPONENT_BUF_VERSION(26)  TYPE C
                                 VALUE 'SAPDBPSJ     COMPONENT_BUF',
      DB_PROFILE_BUF(23)         TYPE C
                                 VALUE 'SAPDBPSJ     DB_PROFILE',
      PIS_PROFILE_BUF(24)        TYPE C
                                 VALUE 'SAPDBPSJ     PIS_PROFILE',
      STORED_OBJS_BUF(24)        TYPE C
                                 VALUE 'SAPDBPSJ     STORED_OBJS',
      SELECT_FIELDS_BUF(26)      TYPE C
                                 VALUE 'SAPDBPSJ     SELECT_FIELDS',
      AUTHORITYCHECK_BUF(27)     TYPE C
                                 VALUE 'SAPDBPSJ     AUTHORITYCHECK',
      AUTHORITYCHECK_BUF_ACT(27) TYPE C,
      MESSAGE_BUF(20)            TYPE C
                                 VALUE 'SAPDBPSJ     MESSAGE'.

* Handle für Modifikation der TCNDB per FB PS07...
DATA:  PSJ_HANDLE_TCNDB TYPE I.

* Funktionsname für Archiv-/Temporärdatenlese-FB
DATA: PSJ_FUNCNAME LIKE TFDIR-FUNCNAME,
      PSJ_FUNCNAME_TMP LIKE TFDIR-FUNCNAME.

* Zusatztablen für Memeory-daten
DATA: BEGIN OF INDX_TABLE OCCURS 0,
        TABNAME(60) TYPE C,
        VERSION LIKE PSJ_VERSIONEN-VERSION,
        AUFRUF  LIKE PSJ_VERSIONEN-AUFRUF,
        START LIKE SY-TABIX,
        STOP  LIKE SY-TABIX,
      END OF INDX_TABLE.

DATA: END OF COMMON PART %_SAPDBPSJ_PSDBCMEM.
