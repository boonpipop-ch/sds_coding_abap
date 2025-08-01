*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0100
*  Creation Date      : 21.03.2024
*  Author             : B Chiewsarikij
*  Add-on ID          : ZMMF001
*  Description        : Program to print service entry sheet form
*  Purpose            : N/A
*  Copied from        : ZRM11RNDR2
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*&---------------------------------------------------------------------*
*& Report ZSDSMMR0100
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdsmmr0100 NO STANDARD PAGE HEADING MESSAGE-ID me
                   LINE-SIZE 80.

INCLUDE messddir.            "Direktwerte Nachrichten
INCLUDE messdata.            "Tabellendefinition XNAST YNAST
INCLUDE vmsgpart.            "Partner

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: essr,                          "Erfassungsblatt
        ekko,                          "Einkaufsbelegkopf
        lfa1,                          "Lieferantenstamm
        t001w,                         "Werke
        t161n,                         "Nachrichtenschemata
        t161m,                         "Nachrichtenfeinsteuerung
        t185,                          "Bildsteuerung
        t185f,
        t185v,
        tnapr,
        sadr,
        addr1_val,
        ekpo,
        eket,
        rm06p,
        ekpa,
        t024,
        t024e,
        t001,
        t006a,
        t006,
        t166p,
        ttxit,
        esll,
        rm11p,
        esuc,
        ml_esll.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF msfo_variablen,
         varno  LIKE t167v-varno,
         meins  LIKE t167v-meins,
         varbez LIKE t167c-varbez,
         value  LIKE esll-frmval1,
       END OF msfo_variablen.


TYPES: BEGIN OF msfo_formel,
         formelnr  LIKE  t167f-formelnr,
         formel    LIKE  t167f-formel,
         meins     LIKE  t167f-meins,
         formelbez LIKE  t167b-formelbez,
       END OF msfo_formel.

TYPES msfo_tab_variablen TYPE msfo_variablen OCCURS 5.

TYPES:
  BEGIN OF typ_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    netwr TYPE ekpo-netwr,
  END OF typ_ekpo,

  ttyp_ekpo TYPE STANDARD TABLE OF typ_ekpo.

** Datendeklaration zum ALV
TYPE-POOLS: slis.
TYPES: BEGIN OF slis_extab,
         fcode LIKE rsmpe-func,
       END OF slis_extab.
TYPES: slis_t_extab TYPE slis_extab OCCURS 1.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------

*-> internal tables
DATA: gt_ekpo TYPE STANDARD TABLE OF typ_ekpo.
*-> range
RANGES: gr_ebeln FOR essr-ebeln.
*-> work areas
DATA: ls_layout TYPE slis_layout_alv.
** Übergabestruktur an ALV
DATA: BEGIN OF t_merep_outtab_srvdoc OCCURS 0.
        INCLUDE TYPE merep_outtab_srvdoc.
DATA:   fax_number LIKE addr1_val-fax_number,
        extension1 LIKE addr1_val-extension1,
        extension2 LIKE addr1_val-extension2,
        country    LIKE addr1_val-country.
DATA: counter      TYPE i,
      markfield(1).
DATA: END OF t_merep_outtab_srvdoc.
DATA: kappl-erfa      LIKE nast-kappl VALUE 'ES',
      business_object LIKE nast-objtype VALUE 'BUS2091'.

* Erfassungsblätter----------------------------------------------------*
DATA:    BEGIN OF xessr OCCURS 50.
           INCLUDE STRUCTURE essr.
DATA:      name1      LIKE addr1_val-name1,
           fax_number LIKE addr1_val-fax_number,
           extension1 LIKE addr1_val-extension1,
           extension2 LIKE addr1_val-extension2,
           country    LIKE addr1_val-country.
DATA:    END OF xessr.

* Belegköpfe ----------------------------------------------------------*
DATA:    BEGIN OF xkopf OCCURS 50.
           INCLUDE STRUCTURE ekko.
DATA:    END OF xkopf.

* Key zum Lesen Tabelle XKOPF -----------------------------------------*
DATA: BEGIN OF xkopfkey,
        mandt LIKE ekko-mandt,
        ebeln LIKE ekko-ebeln,
      END OF xkopfkey.

DATA: BEGIN OF xtheadkey,
        tdobject LIKE thead-tdobject,
        tdname   LIKE thead-tdname,
        tdid     LIKE thead-tdid,
      END OF xtheadkey.
DATA: BEGIN OF xt166p OCCURS 10.
        INCLUDE STRUCTURE t166p.
DATA: END OF xt166p.

*- Table of the text headers-------------------------------------------*
DATA: BEGIN OF xthead OCCURS 10.
        INCLUDE STRUCTURE thead.
DATA: END OF xthead.

* Anzahl Nachrichten --------------------------------------------------*
DATA: BEGIN OF nat OCCURS 5,
        lblni LIKE essr-lblni,
        anzna TYPE i,
      END OF nat.
DATA: BEGIN OF it166p OCCURS 10.
        INCLUDE STRUCTURE t166p.
DATA: END OF it166p.

DATA: BEGIN OF gt_last_packno OCCURS 0,
        ebeln     LIKE essr-ebeln,
        ebelp     LIKE essr-ebelp,
        packno    LIKE eslh-packno,
        netwr     LIKE essr-netwr,
        qty_order LIKE essr-netwr,
      END OF gt_last_packno.

DATA: selkz,                                   "Selektionsskennzeichen
      selok,                                   "Selektionskz gesetzt
      druok,                                   "ausgebbare Nachricht
      xausg,                                   "Nachricht ausgegeben
      colflag,                                 "Streifenmuster
      xdruvo,                                  "Printing
      canzna(12),                              "Anzahl Nachrichten
      nachrappl    LIKE nast-kappl,               "Applikation
      xobjky       LIKE nast-objky,                  "Objektkey
      exlblni      LIKE essr-lblni,                 "Ausgeschlossener Beleg
      xauth,                                   "Hilfsfeld Berechtigung
      count        TYPE i,                         "Zaehler für Anz. Belege
      fdpos        LIKE sy-fdpos,                     "Stelle
      not_found,
      old_ebeln    LIKE ekko-ebeln,               "letzte Bestellung
      entries      LIKE sy-tfill,                  "Counter of table entries
      xdrflg       LIKE t166p-drflg,                 "Auxilliary field text printing
      gv_total     LIKE essr-lwert,
      gv_line      LIKE ml_esll-extrow,
      gv_qty_order LIKE esuh-sumlimit,
      gv_remain    LIKE esll-netwr.
DATA: gv_packno      LIKE eslh-packno,
      gv_last_packno LIKE eslh-packno,
      gv_last_netwr  LIKE esll-netwr.

DATA: formel TYPE msfo_formel.

DATA: variablen TYPE msfo_tab_variablen WITH HEADER LINE.

*- Hilfsfelder für Sicherungspopup  ----------------------------------*
DATA: answer,                            "Antwort aus Pop-UP
      ansy   VALUE 'J',
      ansn   VALUE 'N',
      ansa   VALUE 'A'.

*- Hilfsfelder für Nachrichtenfunktionsbaustein ----------------------*
DATA: bildtext(40),
      beltext      LIKE dv70a-btext,
      fcode(4),
      nupdat,
      xupdat,
      xnewget,
      retco        LIKE sy-subrc,
      rs_selfield  TYPE slis_selfield.

*- Hide-Felder -------------------------------------------------------*
DATA: BEGIN OF hide,
        lblni LIKE essr-lblni,
        bstyp LIKE ekko-bstyp,
        ebeln LIKE ekko-ebeln,
        spras LIKE ekko-spras,
        lifnr LIKE ekko-lifnr,
        name1 LIKE lfa1-name1,
        telfx LIKE lfa1-telfx,
        teltx LIKE lfa1-teltx,
        telx1 LIKE lfa1-telx1,
        land1 LIKE lfa1-land1,
        ekorg LIKE ekko-ekorg,
        bukrs LIKE ekko-bukrs,
        bsart LIKE ekko-bsart,
        reswk LIKE ekko-reswk,
        zeile LIKE sy-linno,
        seite LIKE sy-pagno,
      END OF hide.

*- Markierbereich ----------------------------------------------------*
DATA: BEGIN OF xmint,
        ebelv LIKE ekko-ebeln,
        ebelb LIKE ekko-ebeln,
      END OF xmint.

*-> variables
DATA : gv_mng      TYPE c,
       gv_gm       TYPE c,
       gv_agm      TYPE c,
       gv_amd      TYPE c,
       gv_sd       TYPE c,
       gv_md       TYPE c,
       gv_pres     TYPE c,
       gv_namemng  TYPE c LENGTH 70,
       gv_datemng  TYPE c LENGTH 10,
       gv_namegm   TYPE c LENGTH 70,
       gv_nameagm  TYPE c LENGTH 70,
       gv_dategm   TYPE c LENGTH 10,
       gv_dateagm  TYPE c LENGTH 10,
       gv_nameamd  TYPE c LENGTH 70,
       gv_dateamd  TYPE c LENGTH 10,
       gv_namesd   TYPE c LENGTH 70,
       gv_datesd   TYPE c LENGTH 10,
       gv_namemd   TYPE c LENGTH 70,
       gv_datemd   TYPE c LENGTH 10,
       gv_namepres TYPE c LENGTH 70,
       gv_datepres TYPE c LENGTH 10,
       gv_namereq  TYPE c LENGTH 70,
       gv_datereq  TYPE c LENGTH 10,
       gv_datum    TYPE sy-datum.


*-> reference
DATA ofield TYPE slis_t_fieldcat_alv WITH HEADER LINE.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
*CONSTANTS:

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECT-OPTIONS:
            s_lblni FOR essr-lblni MEMORY ID lblni MATCHCODE OBJECT essr,
            s_namag FOR essr-sbnamag,
            s_naman FOR essr-sbnaman,
            s_lbldt FOR essr-lbldt,
            s_kzabn FOR essr-kzabn,
            s_ebeln FOR ekko-ebeln MEMORY ID bes MATCHCODE OBJECT mekk,
            s_ekorg FOR ekko-ekorg MEMORY ID eko,
            s_bsart FOR ekko-bsart,
            s_ekgrp FOR ekko-ekgrp,
            s_lifnr FOR ekko-lifnr MATCHCODE OBJECT kred,
            s_bedat FOR ekko-bedat.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_uebli LIKE rm06a-p_nachueb DEFAULT 'X',
              p_detai LIKE rm06a-p_detai NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK nachr WITH FRAME TITLE TEXT-004.
  INCLUDE messselp.
SELECTION-SCREEN END OF BLOCK nachr.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  p_kappl = kappl-erfa.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
AT SELECTION-SCREEN.
*- Fehlermeldung: mit Detailfunktionen muß Belegnummer gesetzt sein ---*
  IF p_detai NE space.
    READ TABLE s_ebeln INDEX 1.
    IF sy-subrc NE 0 AND p_kappl EQ space.
      MESSAGE e259.
    ENDIF.
    IF s_ebeln-low CA '*' AND p_kappl EQ space.
      IF sy-fdpos LE 6.
        MESSAGE e259.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*  F4 help on the selection screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kschl.

  CALL FUNCTION 'HELP_VALUES_KSCHL_PREPARE'
    EXPORTING
      program = sy-cprog
      dynnr   = sy-dynnr
      kvewe   = 'B'
      kappl   = 'ES'
    IMPORTING
      kschl   = p_kschl
    EXCEPTIONS
      OTHERS  = 1.

*----------------------------------------------------------------------*
*  Beginn der Selektion                                                *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*- Im Batch keine Nachrichtendetailfunktionen -------------------------*
  IF sy-batch NE space.
    CLEAR p_detai.
  ENDIF.

*- Je nach Funktionswahl - Status setzen ------------------------------*
  IF p_detai NE space.
    SET PF-STATUS 'LIST'.
  ELSE.
    SET PF-STATUS 'LIS1'.
  ENDIF.
  SET TITLEBAR '001'.
  not_found = 'X'.
  PERFORM aktivitaet_setzen(sapfm06d) USING '76'.

*- Füllen Übergabetabelle für Konditionsschlüssel ---------------------*
  PERFORM ranges_fuellen.

  IF p_detai NE space.
*- Datenselektion über Nachrichten-FBs - wegen Detailfunktionen -------*
    PERFORM datenselektion_detail.
  ELSE.
*- Datenselektion ohne Nachrichten-FBs - Detailfunktionen nicht erlaubt*
    PERFORM datenselektion.
  ENDIF.

  IF NOT sy-batch IS INITIAL.
    CHECK NOT s_lblni IS INITIAL.
    LOOP AT xnast WHERE vstat EQ '0'.
      READ TABLE xessr WITH KEY lblni = xnast-objky.        "558077
      IF sy-subrc = 0.
        MOVE xnast TO nast.
        PERFORM einzelnachricht(rsnast00) USING retco.
      ENDIF.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
*- keine Nachrichten gefunden -----------------------------------------*
  IF not_found NE space.
    MESSAGE s260.
    IF sy-calld NE space.
      LEAVE.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ENDIF.
  CLEAR hide.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.

  ULINE.
  FORMAT COLOR COL_GROUP INTENSIFIED.
  WRITE: / TEXT-f01.
  FORMAT COLOR COL_HEADING INTENSIFIED.
  WRITE: / TEXT-u01.
  WRITE: TEXT-u02.
  ULINE.
*----------------------------------------------------------------------*
*  OK-Code-Eingabe                                                     *
*----------------------------------------------------------------------*
AT USER-COMMAND.

  CASE sy-ucomm.
*- Zurück -------------------------------------------------------------*
    WHEN 'ZURU'.
      PERFORM ende.
      IF answer EQ space.
        IF sy-calld EQ space.
          LEAVE TO TRANSACTION sy-tcode.
        ELSE.
          LEAVE.
        ENDIF.
      ENDIF.
*- Abbrechen ----------------------------------------------------------*
    WHEN 'XIT '.
      PERFORM ende.
      IF answer EQ space.
        IF sy-calld EQ space.
          LEAVE TO TRANSACTION sy-tcode.
        ELSE.
          LEAVE.
        ENDIF.
      ENDIF.
*- Sichern ------------------------------------------------------------*
    WHEN 'BU  '.
      PERFORM ucomm_buch.
*- Beenden ------------------------------------------------------------*
    WHEN 'EN  '.
      PERFORM ende.
      IF answer EQ space.
        IF sy-calld EQ space.
          LEAVE. " transaction 'ME00'.
        ELSE.
          LEAVE.
        ENDIF.
      ENDIF.
*- Ausgeben -----------------------------------------------------------*
    WHEN 'DR  '.
      PERFORM ucomm_druck.
      IF selok NE space AND
         xausg NE space.
        IF xupdat NE space.
          PERFORM buchen.
          MESSAGE s258.
        ELSE.
          MESSAGE s257.
        ENDIF.
        COMMIT WORK.
        IF sy-calld EQ space.
          LEAVE TO TRANSACTION sy-tcode.
        ELSE.
          LEAVE.
        ENDIF.
      ENDIF.
*- Ausgabe anzeigen ---------------------------------------------------*
    WHEN 'DRAZ'.
      PERFORM ucomm_druck.
*- Probedruck----------------------------------------------------------*
    WHEN 'DRPR'.
      PERFORM ucomm_druck.
*- Anzeigen Beleg -----------------------------------------------------*
    WHEN 'HICK'.
      PERFORM ucomm_hick.
*- Detail Nachrichten -------------------------------------------------*
    WHEN 'NANZ'.
      PERFORM ucomm_nanz.
*- Einzelne Zeile markieren -------------------------------------------*
    WHEN 'MARK'.
      PERFORM ucomm_mark.
*- Nochmal Markieren --------------------------------------------------*
    WHEN 'MAKT'.
      PERFORM ucomm_makt.
*- Alle markieren -----------------------------------------------------*
    WHEN 'MALL'.
      PERFORM ucomm_mall.
*- Markierungen löschen -----------------------------------------------*
    WHEN 'MDEL'.
      PERFORM ucomm_mdel.
*- Markieren Intervall ------------------------------------------------*
    WHEN 'MINT'.
      PERFORM ucomm_mint.
*<--Begin of insert by CR 300000314
    WHEN 'PRNT'.
      PERFORM print_report.
*-->End of insert by CR 300000314
  ENDCASE.
*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Kopfdaten ausgeben                                                  *
*----------------------------------------------------------------------*
FORM ausgabe_kopf.

  CLEAR not_found.
*..Hierarchiezeile Bestellung ausgeben
  IF essr-ebeln NE old_ebeln.
    old_ebeln  = essr-ebeln.
    RESERVE 3 LINES.
    FORMAT COLOR COL_GROUP INTENSIFIED OFF.
    WRITE: /  sy-vline,
            4 ekko-ebeln,
           15 ekko-bsart,
           20 ekko-ekgrp,
           24 ekko-lifnr,
           35(34) addr1_val-name1,
           70 ekko-bedat DD/MM/YYYY,
           80 sy-vline.
    CLEAR hide.
    hide-bstyp = ekko-bstyp.
    hide-ekorg = ekko-ekorg.
    hide-bsart = ekko-bsart.
    hide-bukrs = ekko-bukrs.
    hide-reswk = ekko-reswk.
    hide-ebeln = ekko-ebeln.
    hide-spras = ekko-spras.
    hide-lifnr = ekko-lifnr.
    hide-telfx = addr1_val-fax_number.
    hide-teltx = addr1_val-extension1.
    hide-telx1 = addr1_val-extension2.
    hide-name1 = addr1_val-name1.
    hide-land1 = addr1_val-country.
    hide-zeile = sy-linno.
    hide-seite = sy-pagno.
    HIDE hide.
  ENDIF.

*- Erfassungsblatt Zeile ausgeben ------------------------------------*
  IF colflag EQ space.
    FORMAT COLOR COL_NORMAL INTENSIFIED.
    colflag = 'X'.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    colflag = ' '.
  ENDIF.

  RESERVE 2 LINES.
  WRITE: /  sy-vline,
          2 selkz AS CHECKBOX,
          4 essr-lblni,
         15 essr-ebelp,
         21 essr-kzabn AS CHECKBOX INPUT OFF,
         24 essr-txz01,
         70 essr-lbldt DD/MM/YYYY,
         80 sy-vline.

  hide-lblni = essr-lblni.
  hide-zeile = sy-linno.
  hide-seite = sy-pagno.
  HIDE hide.

*- Anzahl Nachrichten zu einem Einkaufsbeleg --------------------------*
  READ TABLE nat WITH KEY essr-lblni BINARY SEARCH.
  WRITE nat-anzna TO canzna.
  CONDENSE canzna.
  WRITE: /  sy-vline,
          24 canzna,
          80 sy-vline.

ENDFORM.                    "AUSGABE_KOPF

*----------------------------------------------------------------------*
*  Beleg merken in NAT - Einkaufsbeleg und Anzahl Nachrichten          *
*----------------------------------------------------------------------*
FORM beleg_merken.

  IF nat-anzna NE 0.
    APPEND nat.
  ENDIF.

ENDFORM.                    "BELEG_MERKEN

*----------------------------------------------------------------------*
*  Nachrichten sichern                                                 *
*----------------------------------------------------------------------*
FORM buchen.

  CALL FUNCTION 'RV_MESSAGES_UPDATE'
    EXPORTING
      msg_objky = xobjky.

ENDFORM.                    "BUCHEN

*----------------------------------------------------------------------*
*  Liste beenden                                                       *
*----------------------------------------------------------------------*
FORM ende.

  CLEAR answer.

* Nachrichten erzeugt -------------------------------------------------*
  IF xupdat NE space.
* Sicherungsabfrage ---------------------------------------------------*
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1 = text-301
        textline2 = text-302
        titel     = text-300
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN ansy.
        PERFORM buchen.
        COMMIT WORK.
        MESSAGE s224.
        CLEAR answer.
      WHEN ansn.
        CLEAR answer.
      WHEN ansa.
    ENDCASE.
  ENDIF.

ENDFORM.                    "ENDE

*----------------------------------------------------------------------*
*  Daten selektieren ohne Nachrichtendetailfunktionen                  *
*----------------------------------------------------------------------*
FORM datenselektion.
*<-- Begin of Insertion CR 300000314
  DATA: lv_index LIKE sy-tabix.
  RANGES: lr_ebeln for ekko-ebeln.

*--> End of Insertion CR 300000314
*- Zurücksetzen Tabelle der Objektkeys --------------------------------*
  REFRESH r_objky.
  CLEAR   r_objky.

*- Select-option übertragen für Belegnummernintervall ----------------*
  LOOP AT s_lblni.
    r_objky-sign = s_lblni-sign.
    r_objky-option = s_lblni-option.
    r_objky-low = s_lblni-low.
    r_objky-high = s_lblni-high.
    APPEND r_objky.
  ENDLOOP.

*- Lesen Nachrichten -------------------------------------------------*
  SELECT * FROM nast APPENDING TABLE xnast
                     WHERE kappl EQ p_kappl
*                     AND   objky IN r_objky  "Comment CR 300000314
                     AND   kschl IN r_kschl.
  SORT xnast BY kappl objky.

  REFRESH nat.
  CLEAR   nat.

*- Analysieren Nachrichten - füllen Belegtabelle ---------------------*
  LOOP AT xnast.
    IF xnast-objky NE nast-objky AND
       nast-objky NE space.
      PERFORM beleg_merken.
      CLEAR nat.
    ENDIF.
    nast-objky = xnast-objky.
    IF xnast-vstat EQ p_vstat AND
       xnast-aktiv EQ space.
      nat-lblni = xnast-objky.
      nat-anzna = nat-anzna + 1.
    ELSE.
      DELETE xnast.
    ENDIF.
  ENDLOOP.
  PERFORM beleg_merken.

  SORT nat BY lblni.
  DESCRIBE TABLE nat LINES count.
*- Kein Einkaufsbeleg zum Ausgeben - raus -----------------------------*
  IF count EQ 0.
    MESSAGE s260.
    IF sy-calld NE space.
      LEAVE.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
    EXIT.
  ENDIF.

*<-- Begin of Insertion CR 300000314
  REFRESH lr_ebeln.
* Get all PO of selected Service entry sheet
  SELECT ebeln INTO lr_ebeln-low FROM essr
                       WHERE lblni IN r_objky
                       AND ebeln   IN s_ebeln.
    lr_ebeln-sign = 'I'.
    lr_ebeln-option = 'EQ'.
    COLLECT lr_ebeln.
    CLEAR lr_ebeln.
  ENDSELECT.
  IF NOT s_ebeln[] IS INITIAL
    AND lr_ebeln[] IS INITIAL.
    EXIT.
  ENDIF.
  CLEAR gr_ebeln.
  REFRESH gr_ebeln.
*--> End of Insertion CR 300000314

*- Füllen Select-Option-Tabelle für Select auf ESSR -------------------*

*- Lesen Erfassungsblaetter-------------------------------------------*
*  SELECT * FROM essr APPENDING TABLE xessr
*    FOR ALL ENTRIES IN nat
*    WHERE lblni EQ nat-lblni AND
*          sbnamag IN s_namag AND
*          sbnaman IN s_naman AND
*          lbldt   IN s_lbldt AND
*          kzabn   IN s_kzabn AND
*          ebeln   IN lr_ebeln.

*<<< Begin of check issue SQL error : T41K914894
  SELECT * FROM ESSR APPENDING TABLE XESSR
                     FOR ALL ENTRIES IN NAT
                     WHERE LBLNI   EQ NAT-LBLNI
                     AND   SBNAMAG IN S_NAMAG
                     AND   SBNAMAN IN S_NAMAN
                     AND   LBLDT   IN S_LBLDT
                     AND   KZABN   IN S_KZABN
                     AND   EBELN   IN S_EBELN.
*>>> End of check issue SQL error : T41K914894

  MOVE lr_ebeln[] TO gr_ebeln[].

  SORT xessr BY mandt ebeln ebelp lblni.

*<-- Begin of Insertion CR 300000314
  IF NOT s_lblni[] IS INITIAL.
    DELETE xessr WHERE lblni NOT IN s_lblni.
    SORT xessr BY mandt ebeln ebelp lblni.
  ENDIF.
*--> End of Insertion CR 300000314

*- Kein Einkaufsbeleg druckfähig - raus -------------------------------*
  DESCRIBE TABLE xessr LINES count.
  IF count EQ 0.
    MESSAGE s260.
    IF sy-calld NE space.
      LEAVE.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
    EXIT.
  ENDIF.

* ALV oder nicht ALV
  DATA: l_value TYPE string40.
  GET PARAMETER ID 'ME_USE_GRID' FIELD l_value.
  IF l_value IS INITIAL.

*- Ausgeben Belege auf der Liste --------------------------------------*
    LOOP AT xessr.
      MOVE xessr TO essr.
      PERFORM lesen_bestellung.
      CHECK: s_ekorg,
             s_bsart,
             s_ekgrp,
             s_lifnr,
             s_bedat.
      PERFORM berechtigungen_kopf(sapfm06d).
      IF sy-subrc NE 0.
        xauth = 'X'.
      ENDIF.
      CHECK sy-subrc EQ 0.
*<-- Begin of Replace CR 300000314
*      PERFORM ausgabe_kopf.
      PERFORM write_report.
*--> End of Replace CR 300000314
    ENDLOOP.

    ULINE.
    CLEAR hide.
    IF xauth NE space.
      PERFORM enaco(sapfm06d) USING 'ME' '235'.
      IF sy-subrc NE 0.
        MESSAGE s235.
      ENDIF.
    ENDIF.

  ELSE.   "mit ALV
*- Ausgeben Belege auf der Liste --------------------------------------*
    LOOP AT xessr.
      essr = xessr.
      PERFORM lesen_bestellung.
      PERFORM berechtigungen_kopf(sapfm06d).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ekko TO xkopf.
        APPEND xkopf.
*        XESSR-NAME1 = ADDR1_VAL-NAME1.
*        XESSR-TELFX = ADDR1_VAL-FAX_NUMBER.
*        XESSR-TELTX = ADDR1_VAL-EXTENSION1.
*        XESSR-TELX1 = ADDR1_VAL-EXTENSION2.
*        XESSR-LAND1 = ADDR1_VAL-COUNTRY.
        MOVE-CORRESPONDING addr1_val TO xessr.
        MODIFY xessr.
      ELSE.
        DELETE xessr.
      ENDIF.
      CLEAR: xessr, ekko, addr1_val, xkopf, essr.
    ENDLOOP.

    PERFORM ausgabe_kopf_alv.
    LEAVE PROGRAM.

  ENDIF.  "mit/ohne ALV

ENDFORM.                    "DATENSELEKTION

*----------------------------------------------------------------------*
*  Daten selektieren mit Nachrichtendetailfunktionen                   *
*----------------------------------------------------------------------*
FORM datenselektion_detail.

*<-- Begin of Insertion CR 300000314
  RANGES: lr_objky FOR nast-objky,
          lr_ebeln FOR essr-ebeln.
*--> End of Insertion CR 300000314

*- Füllen Übergabetabelle für Belegnummernintervall -------------------*
  LOOP AT s_lblni.
    r_objky-sign = s_lblni-sign.
    r_objky-option = s_lblni-option.
    r_objky-low = s_lblni-low.
    r_objky-high = s_lblni-high.
    APPEND r_objky.
  ENDLOOP.

*- Lesen Nachrichten -------------------------------------------------*
  CALL FUNCTION 'RV_MESSAGES_SELECT'
    EXPORTING
      msg_erdat = p_erdat
      msg_eruhr = p_eruhr
      msg_kappl = p_kappl
      msg_vstat = p_vstat
      msg_vsztp = p_vsztp
    TABLES
      s_objky   = lr_objky    "CR 300000314 use blank objky to retrieve all
      s_nacha   = r_nacha
      s_kschl   = r_kschl.

  CALL FUNCTION 'RV_MESSAGES_GET'
    TABLES
      tab_xnast = xnast
      tab_ynast = ynast.

*- Keine Nachrichten gefunden ----------------------------------------*
  READ TABLE xnast INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s230.
    IF sy-calld NE space.
      LEAVE.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ENDIF.

*- Aufbau Belegnummerntabelle aus Nachrichtentabelle -----------------*
  REFRESH s_lblni.
  CLEAR count.
  LOOP AT xnast.
    IF count GT 100.
      MESSAGE s271.
      EXIT.
    ENDIF.
    ON CHANGE OF xnast-objky.
      s_lblni-sign = 'I'.
      s_lblni-option = 'EQ'.
      s_lblni-low = xnast-objky.
      COLLECT s_lblni.
      count = count + 1.
    ENDON.
    nat-lblni = xnast-objky.
    nat-anzna = 1.
    COLLECT nat.
  ENDLOOP.

*  REFRESH r_objky.
  CLEAR exlblni.

*<-- Begin of Insertion CR 300000314
  REFRESH lr_ebeln.
* Get all PO of selected Service entry sheet
  SELECT ebeln FROM essr INTO lr_ebeln-low
                       WHERE lblni IN s_lblni
                        AND  lblni IN r_objky
                        AND  ebeln IN s_ebeln.
    lr_ebeln-sign = 'I'.
    lr_ebeln-option = 'EQ'.
    COLLECT lr_ebeln.
    CLEAR lr_ebeln.
  ENDSELECT.

  REFRESH r_objky.
*--> End of Insertion CR 300000314

* ALV oder nicht ALV
  DATA: l_value TYPE string40.
  GET PARAMETER ID 'ME_USE_GRID' FIELD l_value.
  IF l_value IS INITIAL.

*- Lesen Erfassungsblaetter ------------------------------------------*
    SELECT * FROM essr "WHERE lblni IN s_lblni "CR 300000314 Comment
                       WHERE ebeln IN lr_ebeln "CR 300000314 Insert
                         ORDER BY PRIMARY KEY.

      IF exlblni NE space.
        xobjky = exlblni.
        CALL FUNCTION 'RV_MESSAGES_PURGE'
          EXPORTING
            msg_objky = xobjky.
      ENDIF.
      exlblni = essr-lblni.

      CHECK: s_naman,
             s_namag,
             s_lbldt,
             s_kzabn.

      PERFORM lesen_bestellung.
      CHECK: s_ebeln,
             s_ekorg,
             s_bsart,
             s_ekgrp,
             s_lifnr,
             s_bedat.

      PERFORM berechtigungen_kopf(sapfm06d).
      IF sy-subrc NE 0.
        xauth = 'X'.
      ENDIF.
      CHECK sy-subrc EQ 0.
      IF p_uebli NE space.
*<-- Begin of Replace CR 300000314
*      PERFORM ausgabe_kopf.
        PERFORM write_report.
*--> End of Replace CR 300000314
      ENDIF.
      CLEAR exlblni.

    ENDSELECT.

  ELSE.      "mit ALV
*- Lesen Erfassungsblaetter ------------------------------------------*
    SELECT * FROM essr APPENDING TABLE xessr
                       FOR ALL ENTRIES IN s_lblni
                       WHERE lblni = s_lblni-low
                       AND ebeln IN lr_ebeln. "CR 300000314

    LOOP AT xessr.
      essr = xessr.
      PERFORM lesen_bestellung.
      PERFORM berechtigungen_kopf(sapfm06d).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ekko TO xkopf.
        APPEND xkopf.
*        XESSR-NAME1 = ADDR1_VAL-NAME1.
*        XESSR-TELFX = ADDR1_VAL-FAX_NUMBER.
*        XESSR-TELTX = ADDR1_VAL-EXTENSION1.
*        XESSR-TELX1 = ADDR1_VAL-EXTENSION2.
*        XESSR-LAND1 = ADDR1_VAL-COUNTRY.
        MOVE-CORRESPONDING addr1_val TO xessr.
        MODIFY xessr.
      ELSE.
        DELETE xessr.
      ENDIF.
      CLEAR: xessr, ekko, addr1_val, xkopf, essr.
    ENDLOOP.

    PERFORM ausgabe_kopf_alv.
    LEAVE PROGRAM.

  ENDIF.     "mit/ohne ALV

  ULINE.

  IF exlblni NE space.
    xobjky = exlblni.
    CALL FUNCTION 'RV_MESSAGES_PURGE'
      EXPORTING
        msg_objky = xobjky.
  ENDIF.

  CLEAR hide.

  IF xauth NE space.
    PERFORM enaco(sapfm06d) USING 'ME' '235'.
    IF sy-subrc NE 0.
      MESSAGE s235.
    ENDIF.
  ENDIF.

  CHECK p_uebli EQ space.

  CLEAR t185f.
  CLEAR t185.
  CLEAR t185v.

  t185-panel = 'NUBM'.
  t185-bldgr = 'N0  '.
  t185f-fcode = 'ENT1'.
  t185f-trtyp = 'V'.
  CLEAR bildtext.
  bildtext = text-007.
  CALL FUNCTION 'RV_MESSAGES_MAINTENANCE_MANY'
    EXPORTING
      min_bild     = 'M'
      min_cua_text = bildtext
      min_t185     = t185
      min_t185f    = t185f
      min_t185v    = t185v
    IMPORTING
      mex_fcode    = fcode
      mex_t185f    = t185f
      mex_t185v    = t185v
    TABLES
      mtb_part     = part
      mtb_xnast    = xnast
      mtb_ynast    = ynast.

  CASE t185f-fcode.
    WHEN 'DRCK'.
      CLEAR druok.
      LOOP AT xnast WHERE vstat EQ '0'.
        MOVE xnast TO nast.
        druok = 'X'.
        PERFORM einzelnachricht(rsnast00) USING retco.
      ENDLOOP.
      IF druok EQ space.
        MESSAGE s256 WITH hide-lblni.
      ELSE.
        MESSAGE s257.
      ENDIF.
    WHEN 'BABA'.
      LEAVE. " transaction 'ME00'.
  ENDCASE.
  LEAVE TO TRANSACTION sy-tcode.

ENDFORM.                    "DATENSELEKTION_DETAIL

*----------------------------------------------------------------------*
*  Nachrichten ausgeben                                                *
*----------------------------------------------------------------------*
FORM drucken.

  CLEAR nupdat.
*- bei Nachrichtenänderung neu holen ----------------------------------*
  IF xnewget NE space.
    CALL FUNCTION 'RV_MESSAGES_GET'
      TABLES
        tab_xnast = xnast
        tab_ynast = ynast.
    CLEAR xnewget.
  ENDIF.

  CLEAR druok.
  LOOP AT xnast WHERE objky EQ hide-lblni.
*- Ausgabe anzeigen auch bei bereits gedruckten Belegen ---------------*
    IF sy-ucomm NE 'DRAZ'.
      CHECK xnast-vstat EQ '0'.
    ENDIF.
    MOVE xnast TO nast.
    druok = 'X'.

    CASE sy-ucomm.
      WHEN 'DRPR'.
        IF nast-nacha NE '1'.
          MESSAGE e254.
          EXIT.
        ENDIF.
        PERFORM einzelnachricht_screen(rsnast00) USING retco.
        IF retco EQ 0.
          nupdat = 'X'.
        ENDIF.
      WHEN 'DRAZ'.
*         IF NAST-NACHA EQ '6'.
        IF nast-nacha EQ '5' OR nast-nacha EQ '6'.          "543864
          MESSAGE e255.
          EXIT.
        ENDIF.
        nast-tcode = 'XTST'.
        PERFORM einzelnachricht_screen(rsnast00) USING retco.
      WHEN 'DR  '.
        PERFORM einzelnachricht(rsnast00) USING retco.
        IF retco NE 3.
          xobjky = hide-lblni.
          CALL FUNCTION 'RV_MESSAGES_PURGE'
            EXPORTING
              msg_objky = xobjky.
        ENDIF.
        xausg = 'X'.
        COMMIT WORK.
    ENDCASE.
  ENDLOOP.

  IF druok EQ space.
    MESSAGE i256 WITH hide-lblni.
  ENDIF.

ENDFORM.                    "DRUCKEN

*----------------------------------------------------------------------*
*  Bestellung lesen                                                    *
*----------------------------------------------------------------------*
FORM lesen_bestellung.

  IF essr-ebeln NE ekko-ebeln.
    SELECT SINGLE * FROM ekko WHERE ebeln EQ essr-ebeln.
*- Lieferantennamen besorgen -----------------------------------------*
    CALL FUNCTION 'MM_ADDRESS_GET'
      EXPORTING
        i_ekko    = ekko
      IMPORTING
        e_address = addr1_val
      EXCEPTIONS
        OTHERS    = 1.
  ENDIF.
ENDFORM.                    "LESEN_BESTELLUNG

*----------------------------------------------------------------------*
*  Nachrichten bearbeiten                                              *
*----------------------------------------------------------------------*
FORM nachrichten.

*-- Füllen Bildtext und Nachrichtenapplikation ------------------------*
  CLEAR beltext.
  nachrappl = kappl-erfa.
  beltext = text-001.
  fdpos = STRLEN( beltext ).
  WRITE '....................' TO beltext+fdpos(19).
  WRITE space      TO beltext+20(1).
  WRITE hide-lblni TO beltext+21.
  bildtext = text-007.

  xobjky = hide-lblni.
*-- Partnertabelle für Übergabe an Nachrichtensteuerung füllen --------*
  REFRESH part.
  part-parvw = 'LF'.
  part-parno = hide-lifnr.
  part-name1 = hide-name1.
  part-telfx = hide-telfx.
  part-teltx = hide-teltx.
  part-telx1 = hide-telx1.
  part-land1 = hide-land1.
  part-ed_partart = 'K'.
  part-ed_orgid   = hide-ekorg.
  part-ed_applid  = 'M'.
  APPEND part.

  CLEAR t185f.
  CLEAR t185.
  CLEAR t185v.

  t185-panel = 'NUEB'.
  t185-bldgr = 'N0  '.
  t185f-fcode = 'ENT1'.
  t185f-trtyp = 'V'.

  CLEAR t161n.
  SELECT SINGLE * FROM t161n WHERE kvewe EQ 'B'
                               AND kappl EQ nachrappl.
  CALL FUNCTION 'RV_MESSAGES_MAINTENANCE'
    EXPORTING
      min_btext    = beltext
      min_cua_text = bildtext
      min_kappl    = nachrappl
      min_kalsm    = t161n-kalsm
      min_objky    = xobjky
      min_t185     = t185
      min_t185f    = t185f
      min_t185v    = t185v
      pi_objtype   = business_object
    IMPORTING
      mex_fcode    = fcode
      mex_t185f    = t185f
      mex_t185v    = t185v
      mex_updat    = nupdat
    TABLES
      mtb_part     = part.

  IF nupdat NE space.
    xupdat = 'X'.
    xnewget = 'X'.
  ENDIF.         .

  CLEAR t185f.
  CLEAR t185.
  CLEAR t185v.

ENDFORM.                    "NACHRICHTEN

*----------------------------------------------------------------------*
*  Ranges-Tabellen für Datenselektion füllen                           *
*----------------------------------------------------------------------*
FORM ranges_fuellen.

  IF p_kschl NE space.
    r_kschl-sign   = 'I'.
    r_kschl-option = 'EQ'.
    r_kschl-low    = p_kschl.
    APPEND r_kschl.
  ELSE.
    p_kappl = kappl-erfa.
*- Applikation eingegeben: Zugehörige Konditionsschlüssel füllen ------*
    SELECT * FROM t161m WHERE kvewe EQ 'B'
                          AND kappl EQ p_kappl.
      r_kschl-sign   = 'I'.
      r_kschl-option = 'EQ'.
      r_kschl-low    = t161m-kschl. COLLECT r_kschl.
    ENDSELECT.
  ENDIF.

ENDFORM.                    "RANGES_FUELLEN

*----------------------------------------------------------------------*
*  Nachrichten sichern                                                 *
*----------------------------------------------------------------------*
FORM ucomm_buch.

* Nachrichten erzeugt -------------------------------------------------*
  IF xupdat EQ space.
    CLEAR sy-ucomm.
    MESSAGE s223.
    EXIT.
  ENDIF.

  PERFORM buchen.
  COMMIT WORK.
  MESSAGE s224.
  LEAVE TO TRANSACTION sy-tcode.

ENDFORM.                    "UCOMM_BUCH

*----------------------------------------------------------------------*
*  Nachrichten Ausgeben                                                *
*----------------------------------------------------------------------*
FORM ucomm_druck.

  CLEAR selok.
  CLEAR xausg.
  DO.
    CLEAR: hide, selkz.
    READ LINE sy-index FIELD VALUE selkz.
    IF sy-subrc EQ 0.
      IF selkz EQ 'X' OR selkz EQ 'x'.
        selok = 'X'.
        sy-lisel+1(1) = '*'.
        MODIFY LINE sy-index.
        PERFORM drucken.
        IF nupdat NE space.
          sy-index = sy-index + 1.
          READ LINE sy-index.
          IF sy-ucomm EQ 'DRPR'.
            MOVE text-103 TO sy-lisel+34(30).
          ENDIF.
          MODIFY LINE sy-index LINE FORMAT COLOR COL_POSITIVE.
        ENDIF.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  CLEAR hide.
  IF selok EQ space.
    MESSAGE s201.
    EXIT.
  ENDIF.

ENDFORM.                    "UCOMM_DRUCK

*----------------------------------------------------------------------*
*  Anzeigen Beleg                                                      *
*----------------------------------------------------------------------*
FORM ucomm_hick.

  IF hide-bstyp EQ space.
    MESSAGE s201.
    EXIT.
  ENDIF.
*SET PARAMETER ID 'BES' FIELD HIDE-EBELN.

  IF hide-lblni IS INITIAL.
*   CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
    CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
      EXPORTING
        i_ebeln      = hide-ebeln
        i_enjoy      = 'X'
      EXCEPTIONS
        not_found    = 1
        no_authority = 2
        invalid_call = 3
        OTHERS       = 4.
  ELSE.
    SET PARAMETER ID 'LBL' FIELD hide-lblni.
    CALL TRANSACTION 'ML81N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    "UCOMM_HICK

*----------------------------------------------------------------------*
*  Nachrichten Anzeigen                                                *
*----------------------------------------------------------------------*
FORM ucomm_nanz.

*- Diese Funktion nur bei Nachrichtendetailfunktionen möglich ---------*
  IF p_detai EQ space.
    MESSAGE e143.
  ENDIF.

  CLEAR selok.
  DO.
    CLEAR: hide, selkz.
    READ LINE sy-index FIELD VALUE selkz addr1_val-name1.
    IF sy-subrc EQ 0.
      IF selkz EQ 'X' OR selkz EQ 'x'.
        selok = 'X'.
        sy-lisel+1(1) = '*'.
        MODIFY LINE sy-index.
        PERFORM nachrichten.
        IF nupdat NE space.
          sy-index = sy-index + 1.
          READ LINE sy-index.
          sy-lisel+34(30) = text-100.
          MODIFY LINE sy-index LINE FORMAT COLOR COL_POSITIVE.
        ENDIF.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  IF selok EQ space.
    MESSAGE s201.
  ENDIF.
  CLEAR hide.

ENDFORM.                    "UCOMM_NANZ

*----------------------------------------------------------------------*
*  Markieren Zeile                                                     *
*----------------------------------------------------------------------*
FORM ucomm_mark.

  IF hide-bstyp EQ space.
    MESSAGE s201.
    EXIT.
  ENDIF.

  READ LINE hide-zeile OF PAGE hide-seite INDEX 0.
  IF sy-subrc EQ 0.
    IF sy-lisel+1(1) NE 'X'.
      sy-lisel+1(1) = 'X'.
    ELSE.
      sy-lisel+1(1) = space.
    ENDIF.
    MODIFY LINE hide-zeile OF PAGE hide-seite INDEX 0.
  ENDIF.
  CLEAR hide.

ENDFORM.                    "UCOMM_MARK

*----------------------------------------------------------------------*
*  Markieren Alle                                                      *
*----------------------------------------------------------------------*
FORM ucomm_mall.

  DO.
    CLEAR hide.
    READ LINE sy-index.
    IF sy-subrc EQ 0.
      IF hide-bstyp NE space.
        sy-lisel+1(1) = 'X'.
        MODIFY LINE sy-index.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  CLEAR hide.

ENDFORM.                    "UCOMM_MALL

*----------------------------------------------------------------------*
*  Nochmals markieren                                                  *
*----------------------------------------------------------------------*
FORM ucomm_makt.

  DO.
    CLEAR hide.
    READ LINE sy-index.
    IF sy-subrc EQ 0.
      IF sy-lisel+1(1) = '*'.
        sy-lisel+1(1) = 'X'.
        MODIFY LINE sy-index.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLEAR hide.

ENDFORM.                    "UCOMM_MAKT

*----------------------------------------------------------------------*
*  Markierungen löschen                                                *
*----------------------------------------------------------------------*
FORM ucomm_mdel.

* Abbrechen markieren Block -------------------------------------------*
  IF xmint-ebelv NE space.
    CLEAR xmint.
    CLEAR hide.
    EXIT.
  ENDIF.

* sonst Markierungen löschen ------------------------------------------*
  DO.
    CLEAR hide.
    READ LINE sy-index.
    IF sy-subrc EQ 0.
      IF sy-lisel+1(1) EQ 'X'.
        sy-lisel+1(1) = space.
        MODIFY LINE sy-index.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLEAR hide.

ENDFORM.                    "UCOMM_MDEL

*----------------------------------------------------------------------*
*  Markieren Block                                                     *
*----------------------------------------------------------------------*
FORM ucomm_mint.

  IF hide-bstyp EQ space.
    MESSAGE s201.
    EXIT.
  ENDIF.

* Blockende -----------------------------------------------------------*
  IF xmint-ebelv NE space.
* Blockende und Anfang vertauschen ------------------------------------*
    IF hide-lblni LT xmint-ebelv.
      xmint-ebelb = xmint-ebelv.
      xmint-ebelv = hide-lblni.
    ELSE.
      xmint-ebelb = hide-lblni.
    ENDIF.
* Markieren zwischen Anfang und Ende ----------------------------------*
    DO.
      CLEAR hide.
      READ LINE sy-index.
      IF sy-subrc EQ 0.
        IF sy-lisel+1(1) NE 'X'        AND
           hide-lblni GE xmint-ebelv AND
           hide-lblni LE xmint-ebelb.
          sy-lisel+1(1) = 'X'.
          MODIFY LINE sy-index.
        ENDIF.
      ELSE.
        CLEAR xmint.
        EXIT.
      ENDIF.
    ENDDO.
* Blockanfang ---------------------------------------------------------*
  ELSE.
    xmint-ebelv = hide-lblni.
    MESSAGE s213.
  ENDIF.
  CLEAR hide.

ENDFORM.                    "UCOMM_MINT

*----------------------------------------------------------------------*
*  Write Report  (CR 300000314)                                                  *
*----------------------------------------------------------------------*
FORM write_report.

  CLEAR not_found.
*..Hierarchiezeile Bestellung ausgeben
  IF essr-ebeln NE old_ebeln.
    old_ebeln  = essr-ebeln.
    RESERVE 3 LINES.
    FORMAT COLOR COL_GROUP INTENSIFIED OFF.
    WRITE: /  sy-vline,
            2 selkz AS CHECKBOX,  "CR 300000314
            4 ekko-ebeln,
           15 ekko-bsart,
           20 ekko-ekgrp,
           24 ekko-lifnr,
           35(34) addr1_val-name1,
           70 ekko-bedat DD/MM/YYYY,
           80 sy-vline.
    CLEAR hide.
    hide-bstyp = ekko-bstyp.
    hide-ekorg = ekko-ekorg.
    hide-bsart = ekko-bsart.
    hide-bukrs = ekko-bukrs.
    hide-reswk = ekko-reswk.
    hide-ebeln = ekko-ebeln.
    hide-spras = ekko-spras.
    hide-lifnr = ekko-lifnr.
    hide-telfx = addr1_val-fax_number.
    hide-teltx = addr1_val-extension1.
    hide-telx1 = addr1_val-extension2.
    hide-name1 = addr1_val-name1.
    hide-land1 = addr1_val-country.
    hide-zeile = sy-linno.
    hide-seite = sy-pagno.
    HIDE hide.
  ENDIF.

*- Erfassungsblatt Zeile ausgeben ------------------------------------*
  IF colflag EQ space.
    FORMAT COLOR COL_NORMAL INTENSIFIED.
    colflag = 'X'.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    colflag = ' '.
  ENDIF.

  RESERVE 2 LINES.
  WRITE: /  sy-vline,
*          2 selkz AS CHECKBOX, "CR 300000314 Comment
          4 essr-lblni,
         15 essr-ebelp,
         21 essr-kzabn AS CHECKBOX INPUT OFF,
         24 essr-txz01,
         70 essr-lbldt DD/MM/YYYY,
         80 sy-vline.

  hide-lblni = essr-lblni.
  hide-zeile = sy-linno.
  hide-seite = sy-pagno.
  HIDE hide.

*- Anzahl Nachrichten zu einem Einkaufsbeleg --------------------------*
  READ TABLE nat WITH KEY essr-lblni BINARY SEARCH.
  WRITE nat-anzna TO canzna.
  CONDENSE canzna.
  WRITE: /  sy-vline,
          24 canzna,
          80 sy-vline.

ENDFORM.                    "write_report

*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
*       Print report using SapScript (CR 300000314)
*----------------------------------------------------------------------*
FORM print_report .

  DATA: lt_essr   LIKE TABLE OF xessr.
  DATA: BEGIN OF lt_loop OCCURS 0,
        ebeln LIKE xessr-ebeln,
        lblni LIKE xessr-lblni,
        END OF lt_loop.
  DATA: lw_loop LIKE LINE OF lt_loop.

  DATA: lw_essr   LIKE LINE OF lt_essr,
        lw_ekko   LIKE ekko,
        lw_ekpo   LIKE ekpo,
        lw_rm06p  LIKE rm06p,
        lw_eket   LIKE eket,
        lw_ekpa   LIKE ekpa,
        lw_sadr   LIKE sadr,
        lw_lfa1   LIKE lfa1,
        lw_t024   LIKE t024,
        lw_t024e  LIKE t024e,
        lw_t001   LIKE t001,
        h_parvw   LIKE ekpa-parvw.

* Loop get selected EBELN
  CLEAR selok.
  CLEAR xausg.
  DO.
    CLEAR: hide, selkz.
    READ LINE sy-index FIELD VALUE selkz.
    IF sy-subrc EQ 0.
      IF selkz EQ 'X' OR selkz EQ 'x'.
        selok = 'X'.
        sy-lisel+1(1) = '*'.
        MODIFY LINE sy-index.
*        PERFORM drucken.
        LOOP AT xessr INTO lw_essr
          WHERE ebeln = hide-ebeln.
          APPEND lw_essr TO lt_essr.
          CLEAR lw_essr.
        ENDLOOP.
*        IF nupdat NE space.
*          sy-index = sy-index + 1.
*          READ LINE sy-index.
*          IF sy-ucomm EQ 'DRPR'.
*            MOVE text-103 TO sy-lisel+34(30).
*          ENDIF.
*          MODIFY LINE sy-index LINE FORMAT COLOR COL_POSITIVE.
*        ENDIF.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  CLEAR hide.
  IF selok EQ space.
    MESSAGE s201.
    EXIT.
  ENDIF.

  SORT lt_essr BY ebeln.
  IF lt_essr[] IS NOT INITIAL.
    PERFORM open_form.
* BOI CH14
    SELECT
      ebeln
      ebelp
      netwr
     INTO TABLE gt_ekpo
     FROM ekpo
     FOR ALL ENTRIES IN lt_essr[]
     WHERE ebeln = lt_essr-ebeln.
    IF sy-subrc = 0.
      SORT gt_ekpo BY ebeln ebelp.
    ENDIF.
* EOI CH14
  ENDIF.
  SORT lt_essr BY ebeln lblni.

  LOOP AT lt_essr INTO lw_essr.
    CLEAR lw_loop.
    MOVE-CORRESPONDING lw_essr TO lw_loop.
    APPEND lw_loop TO lt_loop.
  ENDLOOP.
  SORT lt_loop BY ebeln lblni.
  REFRESH gt_last_packno.
  CLEAR gv_line.
  LOOP AT lt_loop INTO lw_loop.
    READ TABLE lt_essr WITH KEY lblni = lw_loop-lblni INTO essr.
    AT NEW ebeln.
      CLEAR: gv_total.
      SELECT SINGLE * INTO ekko
      FROM ekko
      WHERE ebeln = essr-ebeln.
      IF sy-subrc = 0.
*BOI CH04
*       PERFORM f_check_amount_loa  USING ekko-ebeln
*                                         ekko-ernam
*                                         ekko-waers
*                                         gt_ekpo[].
*EOI CH04
        PERFORM start_form.
      ENDIF.
      PERFORM print_header.
      PERFORM printer_header_item.
    ENDAT.
*<------------------------------
    IF nast-aende EQ space.
      xdruvo = '1'.
    ELSE.
      xdruvo = '2'.
    ENDIF.
    SELECT SINGLE * INTO ekpo
      FROM ekpo
      WHERE ebeln = essr-ebeln
      AND   ebelp = essr-ebelp.
    SELECT * INTO lw_eket
      FROM eket
      WHERE ebeln EQ ekpo-ebeln
      AND   ebelp EQ ekpo-ebelp.
      EXIT.
    ENDSELECT.
    IF sy-subrc EQ 0 AND eket-eindt NE 0.
      PERFORM prepare_deliv_date USING eket-eindt  eket-lpein
                                    ekko-spras     lfa1-land1
                                    rm06p-lfdat eket-lpein
                                    rm06p-pritx.
    ENDIF.
    IF ekko-lifnr NE space.
      CLEAR ekpa.
      IF h_parvw NE space.
        SELECT * INTO ekpa
          FROM ekpa
          WHERE ebeln = ekko-ebeln
          AND parvw = h_parvw.
          EXIT.
        ENDSELECT.
      ENDIF.
      IF ekpa-lifn2 NE space.
        SELECT SINGLE * INTO lfa1
          FROM lfa1
          WHERE lifnr = ekpa-lifn2.
      ELSE.
        CALL FUNCTION 'MM_ADDRESS_GET'
          EXPORTING
            i_ekko = ekko
          IMPORTING
            e_sadr = sadr
          EXCEPTIONS
            OTHERS = 1.
        MOVE-CORRESPONDING sadr TO lfa1.
      ENDIF.
    ENDIF.
    SELECT SINGLE * INTO t024
      FROM t024
      WHERE ekgrp EQ ekko-ekgrp.
    SELECT SINGLE * INTO t024e
      FROM t024e
      WHERE ekorg EQ ekko-ekorg.
    SELECT SINGLE * INTO t001
      FROM t001
      WHERE bukrs EQ ekko-bukrs.

    SELECT * FROM t166p INTO TABLE it166p
         WHERE druvo EQ xdruvo
         AND   bstyp EQ 'Q'
         AND   bsart EQ ekko-bsart
         AND   pstyp EQ ekpo-pstyp.
    PERFORM work_address USING ekpo-werks.

    gv_total = gv_total + essr-lwert.

    PERFORM print_line.
*<------------------------------
    CLEAR: ekpo, rm06p, eket.

    AT END OF ebeln.
      PERFORM print_total_amount.
      PERFORM end_form.
    ENDAT.

  ENDLOOP.

  CLEAR: gv_last_packno, gv_last_netwr.
*  LOOP AT lt_essr INTO lw_essr.
*
*  ENDLOOP.

  IF lt_essr[] IS NOT INITIAL.
    PERFORM close_form.
  ENDIF.

ENDFORM.                    " PRINT_REPORT

INCLUDE 1rndr_ausgabe_kopf_alvf01.
*&---------------------------------------------------------------------*
*&      Form  OPEN_FORM
*&---------------------------------------------------------------------*
*       Open Form
*----------------------------------------------------------------------*
FORM open_form .
  CALL FUNCTION 'OPEN_FORM'
   EXPORTING
*   APPLICATION                       = 'TX'
*   ARCHIVE_INDEX                     =
*   ARCHIVE_PARAMS                    =
     device                            = 'PRINTER'
     dialog                            = 'X'
     form                              = 'ZF_MM_SERV_EN2'
     language                          = sy-langu
*   OPTIONS                           =
*   MAIL_SENDER                       =
*   MAIL_RECIPIENT                    =
*   MAIL_APPL_OBJECT                  =
*   RAW_DATA_INTERFACE                = '*'
*   SPONUMIV                          =
* IMPORTING
*   LANGUAGE                          =
*   NEW_ARCHIVE_PARAMS                =
*   RESULT                            =
   EXCEPTIONS
     canceled                          = 1
     device                            = 2
     form                              = 3
     OPTIONS                           = 4
     unclosed                          = 5
     mail_options                      = 6
     archive_error                     = 7
     invalid_fax_number                = 8
     more_params_needed_in_batch       = 9
     spool_error                       = 10
     codepage                          = 11
     OTHERS                            = 12.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " OPEN_FORM
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM
*&---------------------------------------------------------------------*
*       Close Form
*----------------------------------------------------------------------*
FORM close_form .

  CALL FUNCTION 'CLOSE_FORM'
* IMPORTING
*   RESULT                         =
*   RDI_RESULT                     =
* TABLES
*   OTFDATA                        =
   EXCEPTIONS
     unopened                       = 1
     bad_pageformat_for_print       = 2
     send_error                     = 3
     spool_error                    = 4
     codepage                       = 5
     OTHERS                         = 6.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CLOSE_FORM
*&---------------------------------------------------------------------*
*&      Form  START_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_form .
  CALL FUNCTION 'START_FORM'
   EXPORTING
*     ARCHIVE_INDEX          =
     form                   = 'ZF_MM_SERV_EN2'
*     LANGUAGE               = ' '
*     STARTPAGE              = ' '
*     PROGRAM                = ' '
*     MAIL_APPL_OBJECT       =
*   IMPORTING
*     LANGUAGE               =
   EXCEPTIONS
     form                   = 1
     format                 = 2
     unended                = 3
     unopened               = 4
     unused                 = 5
     spool_error            = 6
     codepage               = 7
     OTHERS                 = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " START_FORM
*&---------------------------------------------------------------------*
*&      Form  END_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM end_form .
  CALL FUNCTION 'END_FORM'
*   IMPORTING
*     RESULT                         =
   EXCEPTIONS
     unopened                       = 1
     bad_pageformat_for_print       = 2
     spool_error                    = 3
     codepage                       = 4
     OTHERS                         = 5.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " END_FORM
*&---------------------------------------------------------------------*
*&      Form  prepare_deliv_date
*&---------------------------------------------------------------------*
*       prepare delivery date
*----------------------------------------------------------------------*
FORM prepare_deliv_date  USING p_datumi p_lpeini p_spras p_land1
                            p_datume p_lpeine p_pritxe.

  CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
    EXPORTING
      internal_date      = p_datumi
      internal_period    = p_lpeini
      language           = p_spras
      country            = p_land1
    IMPORTING
      external_date      = p_datume
      external_period    = p_lpeine
      external_printtext = p_pritxe.

ENDFORM.                    " prepare_deliv_date
*&---------------------------------------------------------------------*
*&      Form  WORK_ADDRESS
*&---------------------------------------------------------------------*
*       Get Work Address
*----------------------------------------------------------------------*
FORM work_address  USING p_werks.

  DATA: h_ekko LIKE ekko.
  DATA: lw_t001w LIKE t001w.

  CLEAR sadr.
  CHECK p_werks NE space.
  SELECT SINGLE * INTO lw_t001w
    FROM  t001w
    WHERE  werks = p_werks.
  CHECK sy-subrc EQ 0.
  h_ekko-reswk = p_werks.
  h_ekko-bsakz = 'T'.
  CALL FUNCTION 'MM_ADDRESS_GET'
    EXPORTING
      i_ekko = h_ekko
    IMPORTING
      e_sadr = sadr
    EXCEPTIONS
      OTHERS = 0.

ENDFORM.                    " WORK_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  PRINT_HEADER
*&---------------------------------------------------------------------*
*       Print Header
*----------------------------------------------------------------------*
FORM print_header .

* Head data summary -------------------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER'
      window  = 'HEADER'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

* MAIN window  -------------------------------------------------------*
* Header text---------------------------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER_TEXT'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

ENDFORM.                    " PRINT_HEADER

*&---------------------------------------------------------------------*
*&      Form  printer_header_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM printer_header_item.

* Positionszeilen - Überschrift - 1. Seite ----------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_HEADER'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

* Positionszeilen - Überschrift - Folgeseiten -------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_HEADER'
      type    = 'TOP'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

ENDFORM.                    " PRINT_HEADER_ITEM
*&---------------------------------------------------------------------*
*&      Form  PRINT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_line .

  DATA  BEGIN OF lt_arrangement OCCURS 50.   "Table ARRANGEMENT
          INCLUDE STRUCTURE ml_esll.
  DATA  END   OF lt_arrangement.
  DATA  BEGIN OF lt_achievement OCCURS 50.     "Table ACHIEVEMENT
          INCLUDE STRUCTURE ml_esll.
  DATA  END   OF lt_achievement.

  DATA: lv_line LIKE ml_esll-extrow.

  IF NOT essr-packno IS INITIAL.
    REFRESH: lt_arrangement, lt_achievement.
    CLEAR: lt_arrangement, lt_achievement.
* Read Arrangement
    CALL FUNCTION 'MS_SUBDIVISION_FOR_PRINT'
      EXPORTING
        packno           = essr-packno
      TABLES
        gliederung       = lt_arrangement
      EXCEPTIONS
        packno_not_exist = 01.
    IF sy-subrc = 0.
      LOOP AT lt_arrangement.
        PERFORM print_arrangement TABLES lt_arrangement.
* Achievements read (to the arrangement)
        CALL FUNCTION 'MS_SERVICES_FOR_PRINT'
          EXPORTING
            packno            = lt_arrangement-sub_packno
          TABLES
            leistung          = lt_achievement
          EXCEPTIONS
            no_services_found = 01.
        IF sy-subrc EQ 0.
          PERFORM print_srvpos TABLES lt_achievement.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.                    " PRINT_LINE
*&---------------------------------------------------------------------*
*&      Form  PRINT_ARRANGEMENT
*&---------------------------------------------------------------------*
*       Print Arrangement
*----------------------------------------------------------------------*
FORM print_arrangement TABLES pw_arrangement STRUCTURE ml_esll.

  CHECK pw_arrangement-rang NE 0.
  MOVE pw_arrangement TO ml_esll.
  IF gv_line IS INITIAL.
    gv_line = ml_esll-extrow.
  ELSE.
    gv_line = gv_line + 10.
    ml_esll-extrow = gv_line.
  ENDIF.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'SERVICES'
    EXCEPTIONS
      OTHERS  = 01.
  PERFORM texts USING pw_arrangement-packno pw_arrangement-introw '*'.
  CLEAR ml_esll.

ENDFORM.                    " PRINT_ARRANGEMENT
*&---------------------------------------------------------------------*
*&      Form  TEXTS
*&---------------------------------------------------------------------*
*       Texts spend according to table 166P
*----------------------------------------------------------------------*
FORM texts  USING packno introw id.

  DATA: xname LIKE thead-tdname.

  xname  = packno.
  xname+10 = introw.
  CLEAR xtheadkey.

  REFRESH xt166p.
  CLEAR xt166p.

  LOOP AT it166p.
    MOVE it166p TO xt166p.
    CASE xt166p-tdobject.
      WHEN 'ASMD'.    "Activity Master
        IF ml_esll-srvpos NE space.
          PERFORM text_select USING 'ASMD' ml_esll-srvpos
                                    xt166p-tdid.
          READ TABLE xthead INDEX 1.
          IF sy-subrc EQ 0.
            xt166p-txnam = ml_esll-srvpos.
            APPEND xt166p.
          ENDIF.
        ENDIF.
      WHEN 'ESLL'.
**.....Text for achievement ESLL
        IF xtheadkey-tdobject NE 'ESLL'.
          PERFORM text_select USING 'ESLL'  xname id.
        ENDIF.
        xtheadkey-tdid = xt166p-tdid.
        READ TABLE xthead WITH KEY
                         tdobject      = xtheadkey-tdobject
                         tdname        = xtheadkey-tdname
                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          xt166p-txnam = xname.
          APPEND xt166p.
        ENDIF.
      WHEN OTHERS.
        APPEND xt166p.
    ENDCASE.
  ENDLOOP.
  SORT xt166p BY drflg drpri.

*..no further text with same order permit

  xdrflg = '#'.
  LOOP AT xt166p.
    IF xt166p-drflg EQ xdrflg.
      DELETE xt166p.
    ELSE.
      xdrflg = xt166p-drflg.
    ENDIF.
  ENDLOOP.

*  Full texts spend
  LOOP AT xt166p.
    MOVE xt166p TO t166p.
*..Full text designation
    PERFORM read_ttxit USING xt166p-titdr xt166p-tdobject xt166p-tdid.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_TEXT'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDLOOP.

ENDFORM.                    " TEXTS

*&---------------------------------------------------------------------*
*&      Form  TEXT_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OBJECT     text
*      -->NAME       text
*      -->ID         text
*----------------------------------------------------------------------*
FORM text_select  USING object name id.
*&---------------------------------------------------------------------*
*&      Form  TEXT_SELECT
*&---------------------------------------------------------------------*
*       Text-Header selektieren                                        *
*----------------------------------------------------------------------*

  TABLES: thead.

* Text headers read
  thead-tdobject  = object.
  thead-tdspras   = ekko-spras.
  thead-tdname    = name.
  thead-tdid      = id.
  MOVE-CORRESPONDING thead TO xtheadkey.

  CALL FUNCTION 'SELECT_TEXT'
    EXPORTING
      id         = thead-tdid
      language   = thead-tdspras
      name       = thead-tdname
      object     = thead-tdobject
    IMPORTING
      entries    = entries
    TABLES
      selections = xthead.
  SORT xthead BY tdid.

ENDFORM.                    "TEXT_SELECT

*&---------------------------------------------------------------------*
*&      Form  LESEN_TTXIT
*&---------------------------------------------------------------------*
*       Designation of the text read                                   *
*----------------------------------------------------------------------*
FORM read_ttxit USING titdr object id.

  CLEAR ttxit.
  CASE titdr.
    WHEN 'X'.
      SELECT SINGLE * FROM ttxit WHERE tdspras  EQ ekko-spras
                                 AND   tdobject EQ object
                                 AND   tdid     EQ id.
  ENDCASE.

ENDFORM.                    " LESEN_TTXIT

*&---------------------------------------------------------------------*
*&      Form  PRINT_SRVPOS
*&---------------------------------------------------------------------*
*       Achievements spend                                             *
*----------------------------------------------------------------------*
FORM print_srvpos TABLES lt_archievement STRUCTURE ml_esll.

  DATA: h_brtwr TYPE f.
  DATA: help_brtpr TYPE f.
  DATA: lw_archievement LIKE LINE OF lt_archievement.
  DATA: lt_essr LIKE TABLE OF xessr.
  DATA: lw_essr LIKE LINE OF lt_essr.
  RANGES: lr_lblni FOR essr-lblni.
  DATA: lv_lblni LIKE LINE OF lr_lblni.

  DATA: lw_last_packno LIKE LINE OF gt_last_packno.

  LOOP AT lt_archievement INTO lw_archievement.
    IF lw_archievement-alternat NE space.
      SELECT SINGLE * FROM esll
         WHERE packno EQ lw_archievement-packno
         AND   introw EQ lw_archievement-alt_introw.
    ENDIF.
    MOVE space TO lw_archievement-extgroup.
    MOVE '0'   TO lw_archievement-rang.
    MOVE lw_archievement TO ml_esll.
    MOVE esll-extrow TO ml_esll-alt_introw.
* Prepare price unit --------------------------------------------*
    CLEAR rm11p-prpei.
    IF ml_esll-peinh NE 1.
      rm11p-prpei(1) = '/'.
      WRITE ml_esll-peinh NO-SIGN TO rm11p-prpei+1(6).
      DO.
        IF rm11p-prpei+1(1) NE space.
          rm11p-prpei(1) = '/'.
          EXIT.
        ENDIF.
        SHIFT rm11p-prpei LEFT.
      ENDDO.
    ENDIF.

*    pfeld         = ml_esll-brtwr * 1000.
*    IF ml_esll-menge NE 0.
*       h_brtwr = pfeld / ml_esll-menge * ml_esll-peinh.
*       MOVE h_brtwr TO  ml_esll-brtwr.
*   ENDIF.

* Logic for old proofs without gross price
    IF ml_esll-tbtwr IS INITIAL.
      h_brtwr = ml_esll-brtwr * 1000.
      IF ml_esll-menge NE 0.
        IF ml_esll-peinh NE 0.
          help_brtpr = h_brtwr / ml_esll-menge * ml_esll-peinh.
          ml_esll-tbtwr = help_brtpr.
        ELSE.
          help_brtpr = h_brtwr / ml_esll-menge.
          ml_esll-tbtwr = help_brtpr.
        ENDIF.
      ELSE.
        IF ml_esll-peinh NE 0.
          help_brtpr = h_brtwr * ml_esll-peinh.
          ml_esll-tbtwr = help_brtpr.
        ELSE.
          ml_esll-tbtwr = h_brtwr.
        ENDIF.
      ENDIF.
    ENDIF.


* Read full text quantity unit ----------------------------------------*
    CLEAR t006a.
    SELECT SINGLE * FROM t006a WHERE spras EQ ekko-spras
                               AND   msehi EQ lw_archievement-meins.

* Determine right-of-comma positions ------------------------------------------*
    CLEAR t006.
    SELECT SINGLE * FROM t006 WHERE msehi EQ lw_archievement-meins.

* Determine external plan line and group from the order ------------*
    CLEAR: rm11p-pln_extrow, rm11p-pln_grp.
    IF NOT ml_esll-pln_introw IS INITIAL.
      CALL FUNCTION 'MS_GET_EXTERNAL_ROW'
        EXPORTING
          i_packno  = ml_esll-pln_packno
          i_introw  = ml_esll-pln_introw
        IMPORTING
          e_extrow  = rm11p-pln_extrow
          e_extpath = rm11p-pln_grp.
    ENDIF.

* Determine data with unplanned achievement from contract limits -----------*
    CLEAR: rm11p-knt_extrow, rm11p-knt_group, esuc-ebeln, esuc-ebelp.
    IF NOT ml_esll-knt_introw IS INITIAL.
      CALL FUNCTION 'MS_GET_EXTERNAL_ROW'
        EXPORTING
          i_packno  = ml_esll-knt_packno
          i_introw  = ml_esll-knt_introw
        IMPORTING
          e_extrow  = rm11p-knt_extrow
          e_extpath = rm11p-knt_group
          e_ebeln   = esuc-ebeln
          e_ebelp   = esuc-ebelp.
    ENDIF.

    IF gv_line IS INITIAL.
      gv_line = ml_esll-extrow.
    ELSE.
      gv_line = gv_line + 10.
      ml_esll-extrow = gv_line.
    ENDIF.
*<-- Begin of insert by CR 300000314
*   Calculate Qty Order
    SELECT SINGLE b~packno
      INTO gv_packno
      FROM essr AS a
      JOIN eslh AS b
      ON a~ebeln = b~ebeln
      AND a~ebelp = b~ebelp
      WHERE a~lblni = ml_esll-ebeln.
    READ TABLE gt_last_packno WITH KEY ebeln = essr-ebeln
                                       ebelp = essr-ebelp
                                  INTO lw_last_packno.
    IF lw_last_packno-packno = gv_packno.
      gv_qty_order = lw_last_packno-qty_order - lw_last_packno-netwr.
    ELSE.
      CLEAR gv_qty_order.
      SELECT SINGLE sumlimit
      INTO (gv_qty_order)
      FROM esuh
      WHERE packno = gv_packno.
* Deduct with unselected Service entry sheets
      MOVE s_lblni[] TO lr_lblni[].
      SORT lr_lblni BY low.
      READ TABLE lr_lblni INTO lv_lblni.
      SELECT * INTO TABLE lt_essr
        FROM essr
        WHERE ebeln     EQ essr-ebeln
        AND   ebelp     EQ essr-ebelp
        AND   loekz     EQ space                  "WCH T41K912588
        AND   lblni     LT lv_lblni-low.
      LOOP AT lt_essr INTO lw_essr.
        gv_qty_order = gv_qty_order - lw_essr-netwr.
      ENDLOOP.
    ENDIF.
    READ TABLE gt_last_packno WITH KEY ebeln = essr-ebeln
                                       ebelp = essr-ebelp
                                  INTO lw_last_packno.
    IF sy-subrc = 0.
      lw_last_packno-packno = gv_packno.
      lw_last_packno-netwr = ml_esll-netwr.
      lw_last_packno-qty_order = gv_qty_order.
      MODIFY TABLE gt_last_packno FROM lw_last_packno
                            TRANSPORTING packno netwr qty_order.
    ELSE.
      lw_last_packno-ebeln = essr-ebeln.
      lw_last_packno-ebelp = essr-ebelp.
      lw_last_packno-packno = gv_packno.
      lw_last_packno-netwr = ml_esll-netwr.
      lw_last_packno-qty_order = gv_qty_order.
      APPEND lw_last_packno TO gt_last_packno.
    ENDIF.
    CLEAR lw_last_packno.
    gv_remain = gv_qty_order - ml_esll-netwr.
*--> End of insert by CR 300000314

* Expenditure of the text element SERVICES ----------------------------------*
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'SERVICES'
      EXCEPTIONS
        OTHERS  = 01.

*...Full texts select and spend
    PERFORM texts USING  lw_archievement-packno lw_archievement-introw '*'.
*   Time data spending
    PERFORM print_time.
*   Formula spend
    PERFORM print_formel.

    CLEAR ml_esll.
  ENDLOOP.

ENDFORM.                    "PRINT_SRVPOS

*&---------------------------------------------------------------------*
*&      Form  PRINT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_time.

  CHECK NOT ml_esll-pernr IS INITIAL OR
        NOT ml_esll-persext IS INITIAL OR
        NOT ml_esll-sdate IS INITIAL OR
        NOT ml_esll-begtime IS INITIAL OR
        NOT ml_esll-endtime IS INITIAL.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TIME_DATA'.
*          others        = 1.        "Parameter nicht vorhanden (TODO)

ENDFORM.                    " PRINT_TIME

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORMEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_formel.

  DATA: feld(15).
  FIELD-SYMBOLS: <value>.
  CHECK NOT ml_esll-formelnr IS INITIAL.

  CALL FUNCTION 'MS_READ_AND_CHECK_FORMULA'
    EXPORTING
      i_formelnr = ml_esll-formelnr
      no_errors  = 'X'
    IMPORTING
      e_formel   = formel
    TABLES
      variablen  = variablen
    EXCEPTIONS
      OTHERS     = 0.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'FORMEL_KOPF'.
*          others        = 1.               "wegen TODO entfernt
  MOVE 'ML_ESLL-FRMVAL1' TO feld.
  LOOP AT variablen.
    MOVE sy-tabix TO feld+14(1).
    ASSIGN (feld) TO <value>.
    MOVE <value> TO ml_esll-frmval1.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'FORMEL_BODY'.
*              others        = 1.               "wegen TODO entfernt
  ENDLOOP.

ENDFORM.                    " PRINT_FORMEL

*&---------------------------------------------------------------------*
*&      Form  PRINT_TOTAL_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_total_amount.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TOTAL_AMOUNT'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

ENDFORM.                    " PRINT_TOTAL
**&---------------------------------------------------------------------*
**&      Form  F_CHECK_AMOUNT_LOA
**&---------------------------------------------------------------------*
**       Routine for check amount LOA (Refer logic from  ZFM06PE02)
**----------------------------------------------------------------------*
*FORM f_check_amount_loa USING uv_ebeln TYPE ekko-ebeln
*                              uv_uname TYPE ekko-ernam
*                              uv_waers TYPE ekko-waers
*                              ut_ekpo  TYPE ttyp_ekpo.
*
*
*  "Clear all related LOA variable
*  CLEAR:
*    gv_mng,
*    gv_gm,
*    gv_amd,
*    gv_sd,
*    gv_md,
*    gv_pres,
*    gv_namemng,
*    gv_datemng,
*    gv_namegm,
*    gv_dategm,
*    gv_nameamd,
*    gv_dateamd,
*    gv_namesd,
*    gv_datesd,
*    gv_namemd,
*    gv_datemd,
*    gv_namepres,
*    gv_datepres,
*    gv_namereq,
*    gv_datereq,
*    gv_datum.
*
*  TYPES:
*    BEGIN OF ltyp_ztmm_map_loa,
*      division TYPE ztmm_map_loa-division,
*      process  TYPE ztmm_map_loa-process,
*      amount_l TYPE ztmm_map_loa-amount_l,
*      amount_h TYPE ztmm_map_loa-amount_h,
*      operator TYPE ztmm_map_loa-operator,
*      mng      TYPE ztmm_map_loa-mng,
*      agm_gm   TYPE ztmm_map_loa-agm_gm,
*      amd      TYPE ztmm_map_loa-amd,
*      sd       TYPE ztmm_map_loa-sd,
*      md       TYPE ztmm_map_loa-md,
*      pres     TYPE ztmm_map_loa-pres,
*    END OF ltyp_ztmm_map_loa.
*
*  DATA: lt_ztmm_map_loa TYPE STANDARD TABLE OF ltyp_ztmm_map_loa,
*        lt_ekpo         TYPE ttyp_ekpo,
*        ls_ztmm_map_loa TYPE ltyp_ztmm_map_loa,
*        ls_ekpo         LIKE LINE OF lt_ekpo,
*        lv_exch_rate    TYPE ukursp,
*        lv_exrate       TYPE ztmm_map_loa-amount_l,
*        lv_amount       TYPE ekpo-netwr.
*
*  DATA : BEGIN OF ls_ztmm_log_po,
*    ebeln  TYPE ztmm_log_po-ebeln,
*    runng  TYPE ztmm_log_po-runng,
*    status TYPE ztmm_log_po-status,
*    adsdr  TYPE ztmm_log_po-adsdr,
*    posit  TYPE ztmm_log_po-posit,
*    erdat  TYPE ztmm_log_po-erdat,
*    ertim  TYPE ztmm_log_po-ertim,
*  END OF ls_ztmm_log_po.
*
*  DATA :
*    lt_ztmm_log_po  LIKE TABLE OF ls_ztmm_log_po,
*    ls_tmp          LIKE ls_ztmm_log_po,
*    ls_chk          LIKE ls_ztmm_log_po,
*    lv_ernam1       TYPE ekko-ernam,
*    lv_tabix        TYPE sy-tabix,
*    lv_appfg        TYPE ztmm_status_po-appfg.
*
*  RANGES : lr_amount FOR ztmm_map_loa-amount_l.
*
*  lt_ekpo[] = ut_ekpo[].
*  DELETE lt_ekpo[] WHERE ebeln <> uv_ebeln.
*
*  SELECT ztmm_map_loa~division
*         process
*         amount_l
*         amount_h
*         operator
*         mng
*         agm_gm
*         amd
*         sd
*         md
*         pres
*    FROM ztmm_map_user
*    INNER JOIN ztmm_map_loa
*    ON ztmm_map_user~division EQ ztmm_map_loa~division
*    INTO TABLE lt_ztmm_map_loa
*    WHERE ztmm_map_user~uname    EQ uv_uname
*      AND ( ztmm_map_loa~process EQ 'PO' OR
*            ztmm_map_loa~process EQ 'FG' )
*      AND ztmm_map_user~dfalg    NE 'X'
*      AND ztmm_map_loa~dfalg     NE 'X'.
*
*
*  IF uv_waers NE 'THB'.
*    PERFORM f_get_exchange_rate USING sy-datum
*                                      uv_waers
*                             CHANGING lv_exch_rate.
*
*    lv_exrate = lv_exch_rate / 1000.
*  ENDIF.
*
*  "Summarize Net amount
*  CLEAR : lv_amount.
*  LOOP AT lt_ekpo INTO ls_ekpo.
*    IF uv_waers NE 'THB'.
*      ls_ekpo-netwr = ( ( ls_ekpo-netwr / 100 ) * lv_exrate ).
*    ENDIF.
*
*    ADD ls_ekpo-netwr TO lv_amount.
*  ENDLOOP.
*
*  LOOP AT lt_ztmm_map_loa INTO ls_ztmm_map_loa.
*
*    CLEAR lr_amount.
*    lr_amount-sign   = 'I'.
*    lr_amount-option = ls_ztmm_map_loa-operator.
*    lr_amount-low    = ls_ztmm_map_loa-amount_l.
*    lr_amount-high   = ls_ztmm_map_loa-amount_h.
*    APPEND lr_amount.
*
*    IF lv_amount IN lr_amount.
*      gv_mng  = ls_ztmm_map_loa-mng.
*      gv_gm   = ls_ztmm_map_loa-agm_gm.
*      gv_agm  = ls_ztmm_map_loa-agm_gm.
*      gv_amd  = ls_ztmm_map_loa-amd.
*      gv_sd   = ls_ztmm_map_loa-sd.
*      gv_md   = ls_ztmm_map_loa-md.
*      gv_pres = ls_ztmm_map_loa-pres.
*      EXIT.
*    ENDIF.
*
*    CLEAR : ls_ztmm_map_loa,lr_amount[].
*  ENDLOOP.
*
*  SELECT ebeln
*         runng
*         status
*         adsdr
*         posit
*         erdat
*         ertim
*    FROM ztmm_log_po
*    INTO TABLE lt_ztmm_log_po
*    WHERE ebeln EQ uv_ebeln
*      AND flagr NE 'X'.
*    IF sy-subrc = 0.
*      SORT lt_ztmm_log_po BY ebeln runng.
*    ENDIF.
*
*  SELECT SINGLE ernam
*  FROM ekko
*  INTO lv_ernam1
*  WHERE ebeln EQ uv_ebeln.
*
*  gv_namereq = lv_ernam1.
*
*  READ TABLE lt_ztmm_log_po INTO ls_tmp
*  WITH KEY status = 'COMP'.
*  IF sy-subrc = 0.
*    SELECT SINGLE appfg
*      FROM ztmm_status_po
*      INTO lv_appfg
*      WHERE ebeln EQ ls_tmp-ebeln.
*    LOOP AT lt_ztmm_log_po INTO ls_ztmm_log_po.
*      lv_tabix = sy-tabix.
*      IF lv_appfg+1(1) EQ 'G'.
*        IF     ls_ztmm_log_po-posit EQ 'MGR'.
*          gv_namemng  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AGM'.
*          gv_nameagm   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'GM'.
*          gv_namegm   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AMD'.
*          gv_nameamd  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'SD'.
*          gv_namesd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'MD'.
*          gv_namemd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'PRD'.
*          gv_namepres = ls_ztmm_log_po-adsdr+10(40).
*        ENDIF.
*
*        READ TABLE lt_ztmm_log_po INTO ls_chk
*        WITH KEY ebeln  = ls_ztmm_log_po-ebeln
*                 status = 'COMP'.
*        IF sy-subrc = 0.
*          IF     lv_tabix EQ 1.
*            gv_datereq  = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 2.
*            gv_datemng  = ls_ztmm_log_po-erdat.
*            gv_dategm   = ls_ztmm_log_po-erdat.
*            gv_dateagm  = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 3.
*            gv_dategm   = ls_ztmm_log_po-erdat.
*            gv_dateagm  = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 4.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 5.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 6.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 7.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ENDIF.
*
*        ENDIF.
*      ELSEIF lv_appfg+2(1) EQ 'G'.
*        IF ls_ztmm_log_po-posit EQ 'MGR'.
*          gv_namemng  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'GM'.
*          gv_namegm   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AGM'.
*          gv_nameagm  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AMD'.
*          gv_nameamd  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'SD'.
*          gv_namesd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'MD'.
*          gv_namemd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'PRD'.
*          gv_namepres = ls_ztmm_log_po-adsdr+10(40).
*        ENDIF.
*
*        READ TABLE lt_ztmm_log_po INTO ls_chk
*        WITH KEY ebeln  = ls_ztmm_log_po-ebeln
*                 status = 'COMP'.
*        IF sy-subrc = 0.
*          IF     lv_tabix EQ 1.
*            gv_datereq  = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 2.
*            gv_datemng  = ls_ztmm_log_po-erdat.
*            gv_dategm   = ls_ztmm_log_po-erdat.
*            gv_dateagm  = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 4.
*            gv_dategm   = ls_ztmm_log_po-erdat.
*            gv_dateagm  = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 5.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 6.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 7.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 8.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        IF ls_ztmm_log_po-posit EQ 'MGR'.
*          gv_namemng  = ls_ztmm_log_po-adsdr+10(40).
*          gv_namegm   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'AMD'.
*          gv_nameamd  = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'SD'.
*          gv_namesd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'MD'.
*          gv_namemd   = ls_ztmm_log_po-adsdr+10(40).
*        ELSEIF ls_ztmm_log_po-posit EQ 'PRD'.
*          gv_namepres = ls_ztmm_log_po-adsdr+10(40).
*        ENDIF.
*
*        READ TABLE lt_ztmm_log_po INTO ls_chk
*        WITH KEY ebeln  = ls_ztmm_log_po-ebeln
*                 status = 'COMP'.
*        IF sy-subrc = 0.
*          IF     lv_tabix EQ 1.
*            gv_datereq  = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 2.
*            gv_datemng  = ls_ztmm_log_po-erdat.
*            gv_dategm   = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 3.
*            gv_datesd   = ls_ztmm_log_po-erdat.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 4.
*            gv_dateamd  = ls_ztmm_log_po-erdat.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 5.
*            gv_datemd   = ls_ztmm_log_po-erdat.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ELSEIF lv_tabix EQ 6.
*            gv_datepres = ls_ztmm_log_po-erdat.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*      CLEAR : ls_ztmm_log_po.
*    ENDLOOP.
*
*    IF gv_namegm IS NOT INITIAL.
*      gv_agm = 'X'.
*    ELSE.
*      gv_gm  = 'X'.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_AMOUNT_LOA
**&---------------------------------------------------------------------*
**&      Form  F_GET_EXCHANGE_RATE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_get_exchange_rate USING uv_date  TYPE datum
*                               uv_waers TYPE ekko-waers
*                      CHANGING cv_exchange TYPE ukursp.
*
*  DATA : lv_rate_type  TYPE bapi1093_1-rate_type,
*         lv_from_curr  TYPE bapi1093_1-from_curr,
*         lv_to_currncy TYPE  bapi1093_1-to_currncy,
*         lv_date       TYPE  bapi1093_2-trans_date.
*
*  DATA : ls_exch_rate  TYPE bapi1093_0,
*         ls_return    TYPE  bapiret1.
*
*  CONSTANTS : BEGIN OF lc_exchange,
*    rate_type TYPE c LENGTH 1 VALUE 'M',
*    currncy   TYPE c LENGTH 3 VALUE 'THB',
*  END OF lc_exchange.
*
*  lv_rate_type  = lc_exchange-rate_type.
*  lv_from_curr  = uv_waers.
*  lv_to_currncy = lc_exchange-currncy.
*
*  PERFORM f_first_date USING uv_date
*                    CHANGING lv_date.
*
*  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
*    EXPORTING
*      rate_type  = lv_rate_type
*      from_curr  = lv_from_curr
*      to_currncy = lv_to_currncy
*      date       = lv_date
*    IMPORTING
*      exch_rate  = ls_exch_rate
*      return     = ls_return.
*
*  cv_exchange = ls_exch_rate-exch_rate.
*
*ENDFORM.                    " F_GET_EXCHANGE_RATE
**&---------------------------------------------------------------------*
**&      Form  F_FIRST_DATE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      <--P_LV_DATE  text
**----------------------------------------------------------------------*
*FORM f_first_date USING uv_date_in TYPE datum
*               CHANGING cv_date    TYPE datum.
*
*  DATA : lv_date             TYPE datum,
*         lv_month_begin_date TYPE datum,
*         lv_month_end_date   TYPE datum.
*
*  lv_date = uv_date_in.
*
*  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
*    EXPORTING
*      iv_date             = lv_date
*    IMPORTING
*      ev_month_begin_date = lv_month_begin_date
*      ev_month_end_date   = lv_month_end_date.
*
*  cv_date = lv_month_begin_date.
*
*ENDFORM.                    " F_FIRST_DATE
