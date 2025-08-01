*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0620
*  Creation Date      : 04.11.2024
*  Author             : B.CHIEWSARIKIJ (SDS)
*  Add-on ID          : N/A
*  Description        : Serial List
*  Purpose            : N/A
*  Copied from        : ZRTCHECKL
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0620.
*&---------------------------------------------------------------------*
*& Report ZSDSMMR0620
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE RVADTABL.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: VBCO3,                         "Schlusselfelder Belegkopf
        VBDKL,                         "View auf Lieferkopf
        VBDPL,                         "View auf Lieferposition
        WEZLK,                         "Struktur fur Kopfdaten Zahlliste
        WEZLP,                         "Struktur fur Pos.daten Zahlliste
        TLINE,                         "Textzeile
        MARA,                          "Materialstamm
        MARD,                          "Lagerortdaten
        EBEFU,                         "View auf Bestellpositionen
        KBEFU,                         "View auf Bestellkopf
        LFA1,                          "Lieferantenstamm
        EKPO,                          "Bestellpositionen
        EKKO,                          "Bestellkopf
        EKET,                          "Einteilungen
        VBPA,                          "Partner
        T001W,                         "beliefertes Werk
        MTCOM,                         "Komm.bereich zum Lesen MARA
        VBADR,
        SER01,
        OBJK.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF TYP_SERIAL,
         MATNR TYPE LIPS-MATNR,
         VGBEL TYPE LIPS-VGBEL,  " PO No.
         SERNR TYPE OBJK-SERNR,
       END OF TYP_SERIAL.
TYPES: BEGIN OF TYP_LINE,
         MATNR     TYPE LIPS-MATNR,
         VGBEL     TYPE LIPS-VGBEL,  " PO No.
         SORT      TYPE C,           " 0:Matnr,  1:PO,   3:Serial
         INDEX(5)  TYPE N,           " body index
         TDFORMAT  TYPE TLINE-TDFORMAT,
         TEXT(255) TYPE C,
       END OF TYP_LINE.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA: IT_SERIAL TYPE TABLE OF TYP_SERIAL WITH HEADER LINE,
      IT_LINE   TYPE TABLE OF TYP_LINE   WITH HEADER LINE,
      IT_TLINE  TYPE TABLE OF TLINE      WITH HEADER LINE.

DATA: BEGIN OF TVBDPL OCCURS 0.  " Interne Tabelle Lieferpositionen
        INCLUDE STRUCTURE VBDPL.
DATA: END OF TVBDPL.

DATA: BEGIN OF TEBEFU OCCURS 0.  " Interne Tabelle Bestellpositionen
        INCLUDE STRUCTURE EBEFU.
DATA: END OF TEBEFU.

DATA: BEGIN OF TEKSEL OCCURS 0.  " benotigt fur FBSt Bestellung lesen
        INCLUDE STRUCTURE EKSEL.
DATA: END OF TEKSEL.

DATA: BEGIN OF TEKBNK OCCURS 0.  " benotigt fur FBSt Bestellung lesen
        INCLUDE STRUCTURE EKBNK.
DATA: END OF TEKBNK.

DATA: BEGIN OF TWEZLP OCCURS 0.  " Interne Tabelle der fur Kontroll-
        INCLUDE STRUCTURE WEZLP. " Unterlage rel. Pos.-Informationen
DATA: END OF TWEZLP.

DATA: BEGIN OF XTHEAD OCCURS 10. " Tabelle der Text-IDs pro Position
        INCLUDE STRUCTURE THEAD.
DATA: END OF XTHEAD.

DATA: BEGIN OF TLINETAB OCCURS 10. " Tabelle der Texte einer Text-ID zu
        INCLUDE STRUCTURE TLINE.       " einer Position
DATA: END OF TLINETAB.

DATA: BEGIN OF TABDUMMY OCCURS 1,      "Dummy-Tabelle fur
        FDUMMY,                        "Funktionsbaustein Material_Read
      END OF TABDUMMY.

DATA: RETCODE LIKE SY-SUBRC.
DATA: XSCREEN(1) TYPE C.           "Ausgabe auf Drucker oder Bildschirm
DATA: NOT_FIRST(1) TYPE C.         "zur Steuerung mehrerer Aufkleber bei
"einer Lieferposition
DATA: QUELLE(1) TYPE C,           "Kennung, wie Einstieg erfolgt ist
      XLIEF     VALUE 'L',
      XBEST     VALUE 'B'.
DATA: XEBELN      LIKE EKPO-EBELN.
DATA: COMP_WERK   LIKE T001W-WERKS.
DATA: COMP_LFDAT  LIKE EKET-EINDT.
DATA: H_TABIX     LIKE SY-TABIX,
      H_RESTMENGE LIKE WEZLP-SOLL_MENGE,
      H_IBTYP     LIKE T163D-IBTYP.

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
*SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
* SELECT-OPTIONS:
*SELECTION-SCREEN END OF BLOCK s01.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*---------------------------------------------------------------------*
*       FORM ENTRY_CHECKLIST_LIEF                                     *
*---------------------------------------------------------------------*
*       Einstieg Zahlliste aus Avis oder Grob-WE (Lieferungen)        *
*---------------------------------------------------------------------*
FORM ENTRY_CHECKLIST_LIEF USING RETURN_CODE TYPE SYSUBRC
                                US_SCREEN   TYPE C.
  CLEAR RETCODE.
  RETURN_CODE = 0.
  XSCREEN = US_SCREEN.
* Kennung, das Einstieg uber Lieferbeleg erfolgt (also aus Grob-WE bzw.
* Lieferavis)
  QUELLE = XLIEF.
* Beschaffung der Daten uber SD-Lieferung
  PERFORM GET_DATA_LIEF.
  IF RETCODE NE 0.
    RETURN_CODE = 1.
    EXIT.
  ENDIF.
* Formular offnen
  PERFORM FORM_OPEN USING US_SCREEN WEZLK-LAND1_EM.
  IF RETCODE NE 0.
    RETURN_CODE = 1.
    EXIT.
  ENDIF.
* Textelemente fur Kontrolliste
  PERFORM PRINT_CHECKL.
  IF RETCODE NE 0.
    RETURN_CODE = 1.
    EXIT.
  ENDIF.
* Formular schliesen
  PERFORM FORM_CLOSE.
  IF RETCODE NE 0.
    RETURN_CODE = 1.
  ENDIF.

ENDFORM.                    "ENTRY_CHECKLIST_LIEF
*---------------------------------------------------------------------*
*       FORM FORM_OPEN                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  US_SCREEN                                                     *
*  -->  US_COUNTRY                                                    *
*---------------------------------------------------------------------*
FORM FORM_OPEN USING US_SCREEN  TYPE C
                     US_COUNTRY TYPE LAND1.
*  INCLUDE rvadopfo.
  DATA: LVS_ITCPO        TYPE   ITCPO,
        LVF_DEVICE(30)   TYPE   C,
        LVF_DIALOG(1)    TYPE   C   VALUE ' ',
        LVS_RECIPIENT    LIKE   SWOTOBJID,
        LVS_SENDER       LIKE   SWOTOBJID,
        LVS_SNAST        TYPE   SNAST,
        LVF_PROGRAM      LIKE   SY-REPID,
        LVS_COMM_TYPE    TYPE   AD_COMM,
        LVS_COMM_VALUES  TYPE   SZADR_COMM_VALUES,
        L_TIMESTAMP      TYPE   TIMESTAMP,
        L_TIMESTAMPC(15) TYPE   C.

* reset return code
  RETCODE = 0.

* if there is a communication strategy used ...
  IF NOT NAST-TCODE IS INITIAL AND NAST-NACHA EQ '5'.

*   ... use stratagy to get communication type
    CALL FUNCTION 'ADDR_GET_NEXT_COMM_TYPE'
      EXPORTING
        STRATEGY           = NAST-TCODE
*       ADDRESS_TYPE       =
*       ADDRESS_NUMBER     = VBDKA-ADRNR
*       PERSON_NUMBER      = VBDKA-ADRNP
        ADDRESS_NUMBER     = ADDR_KEY-ADDRNUMBER
        PERSON_NUMBER      = ADDR_KEY-PERSNUMBER
      IMPORTING
        COMM_TYPE          = LVS_COMM_TYPE
        COMM_VALUES        = LVS_COMM_VALUES
*        TABLES
*       STRATEGY_TABLE     =
      EXCEPTIONS
        ADDRESS_NOT_EXIST  = 1
        PERSON_NOT_EXIST   = 2
        NO_COMM_TYPE_FOUND = 3
        INTERNAL_ERROR     = 4
        PARAMETER_ERROR    = 5
        OTHERS             = 6.
    IF SY-SUBRC <> 0.
      RETCODE = SY-SUBRC.
      SYST-MSGTY = 'E'.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.

  ENDIF.

* convert communication data
  MOVE-CORRESPONDING NAST TO LVS_SNAST.
  MOVE SY-REPID           TO LVF_PROGRAM.
  CALL FUNCTION 'CONVERT_COMM_TYPE_DATA'
    EXPORTING
      PI_COMM_TYPE              = LVS_COMM_TYPE
      PI_COMM_VALUES            = LVS_COMM_VALUES
      PI_SCREEN                 = US_SCREEN
*     PI_NEWID                  =
      PI_COUNTRY                = US_COUNTRY
      PI_REPID                  = LVF_PROGRAM
      PI_SNAST                  = LVS_SNAST
    IMPORTING
      PE_ITCPO                  = LVS_ITCPO
      PE_DEVICE                 = LVF_DEVICE
      PE_MAIL_RECIPIENT         = LVS_RECIPIENT
      PE_MAIL_SENDER            = LVS_SENDER
    EXCEPTIONS
      COMM_TYPE_NOT_SUPPORTED   = 1
      RECIPIENT_CREATION_FAILED = 2
      SENDER_CREATION_FAILED    = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
    RETCODE = SY-SUBRC.
    SYST-MSGTY = 'E'.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

  CHECK RETCODE EQ 0.

* if there is no communication type
  IF  LVS_COMM_TYPE IS INITIAL.
*   set device
    CASE NAST-NACHA.
      WHEN '1'.
        LVF_DEVICE = 'PRINTER'.
      WHEN '2'.
        LVF_DEVICE = 'TELEFAX'.
        LVS_ITCPO-TDTELENUM = NAST-TELFX.
        IF NAST-TLAND IS INITIAL.
          LVS_ITCPO-TDTELELAND = US_COUNTRY.
        ELSE.
          LVS_ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
        LVS_ITCPO-TDSENDDATE = NAST-VSDAT.
        LVS_ITCPO-TDSENDTIME = NAST-VSURA.
        LVS_ITCPO-TDFAXUSER  = NAST-USNAM.
      WHEN '3'.
        LVF_DEVICE = 'TELETEX'.
        LVS_ITCPO-TDTELENUM = NAST-TELTX.
        IF NAST-TLAND IS INITIAL.
          LVS_ITCPO-TDTELELAND = US_COUNTRY.
        ELSE.
          LVS_ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
        LVS_ITCPO-TDSENDDATE = NAST-VSDAT.
        LVS_ITCPO-TDSENDTIME = NAST-VSURA.
      WHEN '4'.
        LVF_DEVICE = 'TELEX'.
        LVS_ITCPO-TDTELENUM = NAST-TELX1.
        IF NAST-TLAND IS INITIAL.
          LVS_ITCPO-TDTELELAND = US_COUNTRY.
        ELSE.
          LVS_ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
        LVS_ITCPO-TDSENDDATE = NAST-VSDAT.
        LVS_ITCPO-TDSENDTIME = NAST-VSURA.
      WHEN OTHERS.
        LVF_DEVICE = 'PRINTER'.
    ENDCASE.
  ENDIF.

* fill structure itcpo
  ITCPO = LVS_ITCPO.

* OTF-Output, wenn Browser-Druck
  IF NAST-SORT1 = 'EBPP'.
    LVS_ITCPO-TDGETOTF = 'X'.
  ENDIF.

  " Set Spool TITLE
  GET TIME STAMP FIELD L_TIMESTAMP.
  L_TIMESTAMPC = L_TIMESTAMP.
  CONCATENATE 'PO Receipt Inb.' WEZLK-VBELN INTO LVS_ITCPO-TDCOVTITLE.
  CONCATENATE LVS_ITCPO-TDCOVTITLE L_TIMESTAMPC INTO LVS_ITCPO-TDCOVTITLE SEPARATED BY ' '.
* open form
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
*     APPLICATION    = 'TX'
      ARCHIVE_INDEX  = TOA_DARA
      ARCHIVE_PARAMS = ARC_PARAMS
      DEVICE         = LVF_DEVICE
      DIALOG         = ' '
      FORM           = TNAPR-FONAM
      LANGUAGE       = NAST-SPRAS
      OPTIONS        = LVS_ITCPO
      MAIL_SENDER    = LVS_SENDER
      MAIL_RECIPIENT = LVS_RECIPIENT
*     MAIL_APPL_OBJECT   = ' '
*     RAW_DATA_INTERFACE = '*'
*      IMPORTING
*     LANGUAGE       =
*     NEW_ARCHIVE_PARAMS =
*     RESULT         =
    EXCEPTIONS
      CANCELED       = 1
      DEVICE         = 2
      FORM           = 3
      OPTIONS        = 4
      UNCLOSED       = 5
      MAIL_OPTIONS   = 6
      ARCHIVE_ERROR  = 7
      OTHERS         = 8.

  IF SY-SUBRC NE 0.
    CASE SY-SUBRC.
      WHEN 7.
        RETCODE = SY-SUBRC.
        SYST-MSGID = 'VN'.
        SYST-MSGNO = '096'.
        SYST-MSGTY = 'E'.
        SYST-MSGV1 = NAST-KSCHL.
        SYST-MSGV2 = NAST-KAPPL.
        PERFORM PROTOCOL_UPDATE.
      WHEN OTHERS.
        RETCODE = SY-SUBRC.
        PERFORM PROTOCOL_UPDATE.
    ENDCASE.
  ENDIF.
  SET COUNTRY US_COUNTRY.

ENDFORM.                    "FORM_OPEN
*---------------------------------------------------------------------*
*       FORM FORM_CLOSE                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FORM_CLOSE.
  CALL FUNCTION 'CLOSE_FORM'           "...Ende Formulardruck
    EXCEPTIONS
      OTHERS = 1.
  IF SY-SUBRC NE 0.
    RETCODE = 1.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.
  SET COUNTRY SPACE.

ENDFORM.                    "FORM_CLOSE
*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE.
  CHECK XSCREEN = SPACE.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      MSG_ARBGB = SYST-MSGID
      MSG_NR    = SYST-MSGNO
      MSG_TY    = SYST-MSGTY
      MSG_V1    = SYST-MSGV1
      MSG_V2    = SYST-MSGV2
      MSG_V3    = SYST-MSGV3
      MSG_V4    = SYST-MSGV4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                    "PROTOCOL_UPDATE
*---------------------------------------------------------------------*
*       FORM GET_DATA_LIEF                                            *
*----------------------------------------------------------------------*
* Datenbeschaffung bei Einstieg uber SD-Lieferbeleg (bei Lieferavis und
* Grob-WE)
FORM GET_DATA_LIEF.
* Ermitteln Lieferpositionen uber Belegnummer aus Nachrichtensatz
  VBCO3-VBELN = NAST-OBJKY.
  VBCO3-SPRAS = NAST-SPRAS.

  CALL FUNCTION 'RV_DELIVERY_PRINT_VIEW'
    EXPORTING
      COMWA = VBCO3
    IMPORTING
      KOPF  = VBDKL
    TABLES
      POS   = TVBDPL.

* Ermitteln weiterer Positionsdaten und Aufbau Tabelle TWEZLP
  REFRESH TWEZLP.
  LOOP AT TVBDPL.
    H_TABIX = SY-TABIX.
    VBDPL = TVBDPL.
    CLEAR TWEZLP.
    WEZLP-POSNR      = VBDPL-POSNR.
    WEZLP-MATNR      = VBDPL-MATNR.
    WEZLP-SOLL_MENGE = VBDPL-LFIMG.
    WEZLP-BSTME      = VBDPL-VRKME.
    WEZLP-KURZTEXT   = VBDPL-ARKTX.
    WEZLP-EBELN      = VBDPL-VGBEL.
    WEZLP-EBELP      = VBDPL-VGPOS.
    WEZLP-WERKS      = VBDPL-WERKS.
* Lieferdatum ist zwar Kopfdatum, wird aber wegen spaterer
* MHD-Berechnung auch in die Positionen ubernommen
    WEZLP-LFDAT      = VBDKL-LFDAT.
* Vergleich, ob Werk noch eindeutig (ein Lieferavis kann sich potentiell
* auf mehrere Werke beziehen, ein Grob-WE nicht)
    IF H_TABIX = 1.
      COMP_WERK = VBDPL-WERKS.
    ELSE.
      IF NOT COMP_WERK IS INITIAL AND
         VBDPL-WERKS NE COMP_WERK.
        CLEAR COMP_WERK.
      ENDIF.
    ENDIF.
* Leergutunterpositionen werden nicht gedruckt
    CHECK NOT ( EKPO-UPTYP = '3' AND EKPO-UPVOR IS INITIAL ).

    MOVE WEZLP TO TWEZLP.
    APPEND TWEZLP.
  ENDLOOP.

* Bestucken der Kopfstruktur fur Formular
* dazu zunachst Ermitteln der Adressdaten zu den Partner Lieferant
* und empfangendes Werk
* a) Lieferant
  CLEAR WEZLK.

  WEZLK-LIFNR = VBDKL-LIFNR.

* b) empfangendes Werk

  WEZLK-WERKS = COMP_WERK.
* weitere Kopfdaten
  WEZLK-VBELN = VBDKL-VBELN.
  WEZLK-LFDAT = VBDKL-LFDAT.
  WEZLK-LFUHR = VBDKL-LFUHR.
  WEZLK-VERUR = VBDKL-VERUR.
* Unterscheidung, ob Zahlliste auf Basis Lieferavis oder Grob-WE
  CALL FUNCTION 'ME_CONFIRMATION_DELIVERY_TYPE'
    EXPORTING
      I_FUNC              = '2'
    CHANGING
      C_IBTYP             = H_IBTYP
      C_LFART             = VBDKL-LFART
    EXCEPTIONS
      FUNCTION_NOT_VALID  = 01
      PARAM_VALUE_MISSING = 02
      NO_ITEM_FOUND       = 03.
  IF H_IBTYP = '2'.
    WEZLK-TYPZL_WE = 'L'.              " Lieferavis
  ELSE.
    WEZLK-TYPZL_WE = 'G'.              " Grob-WE
  ENDIF.
* Get Serial Numbers
  SELECT VGBEL OB~MATNR OB~SERNR
    FROM LIPS  AS LS
         INNER JOIN SER01 AS SE ON LS~VBELN = SE~LIEF_NR
                               AND LS~POSNR = SE~POSNR
         INNER JOIN OBJK  AS OB ON SE~OBKNR = OB~OBKNR
    INTO CORRESPONDING FIELDS OF TABLE IT_SERIAL
    WHERE VBELN = WEZLK-VBELN.
  IF SY-SUBRC <> 0.
    RETCODE = SY-SUBRC.
    EXIT.
  ENDIF.

ENDFORM.                    "GET_DATA_LIEF
*---------------------------------------------------------------------*
*       FORM PRINT_CHECKL                                             *
*---------------------------------------------------------------------*
*       Ausgabe MAIN-Fenster fur Kontrolliste (sowohl auf Basis       *
*       Bestellung als auch Lieferung)                                *
*---------------------------------------------------------------------*
FORM PRINT_CHECKL.
  DATA: L_THEAD  TYPE THEAD.

  PERFORM MAKE_MAIN_LINES USING WEZLK-VBELN.
  L_THEAD-TDFORM = 'ZMMFSER'.

  PERFORM START_FORM USING 'ZMMFSER'.
  CALL FUNCTION 'WRITE_FORM_LINES'
    EXPORTING
      FUNCTION = 'SET'
      HEADER   = L_THEAD
      TYPE     = 'BODY'
      WINDOW   = 'MAIN'
    TABLES
      LINES    = IT_TLINE.
  PERFORM END_FORM.

ENDFORM.                    "PRINT_CHECKL
*&---------------------------------------------------------------------*
*&      Form  MAKE_MAIN_LINES
*&---------------------------------------------------------------------*
*       Make lines for main window
*----------------------------------------------------------------------*
*      -->IN_TAB     text
*      -->OUT_TAB    text
*----------------------------------------------------------------------*
FORM MAKE_MAIN_LINES USING P_VBELN  TYPE LIPS-VBELN.
  DATA: L_GTOTAL(5) TYPE C,  " grand total
        L_MTOTAL(5) TYPE C,  " count serial number
        L_INDEX(5)  TYPE C,  " index for sort
        L_MOD(2)    TYPE N,  " for count serial in line
        L_SERNR(7)  TYPE C,  " tmp
        L_ZTRUCKNO  TYPE LIKP-ZTRUCKNO, " Truck No.
        L_TDLINE    TYPE TLINE-TDLINE.

  CONSTANTS: LC_ROWNUM(2) TYPE N VALUE '10'. " 1 gyou ni kaku sernr no kazu
  CONSTANTS: BEGIN OF LC_SORT,
               MATNR TYPE C VALUE 0,  " Material & Qty Line
               VGBEL TYPE C VALUE 1,  " PO No. Line
               SERNR TYPE C VALUE 2,  " Serial Number Line
             END OF LC_SORT.

  CLEAR:   IT_LINE, L_MTOTAL, L_GTOTAL.
  REFRESH: IT_LINE.

  SORT IT_SERIAL BY MATNR VGBEL SERNR.
  LOOP AT IT_SERIAL.
    AT NEW MATNR.
      CLEAR: IT_LINE, L_MTOTAL.
    ENDAT.

    AT NEW VGBEL.
      MOVE-CORRESPONDING IT_SERIAL TO IT_LINE.
      IT_LINE-SORT = LC_SORT-VGBEL.
      IT_LINE-TDFORMAT = 'PO'.
      CONCATENATE ',,PO No. :' IT_LINE-VGBEL INTO IT_LINE-TEXT SEPARATED BY ' '.
      APPEND IT_LINE.
      CLEAR: IT_LINE, L_INDEX, L_MOD.
    ENDAT.

    L_MTOTAL = L_MTOTAL + 1.  " count up
    L_MOD = ( L_MOD + 1 ) MOD LC_ROWNUM.
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        INPUT  = IT_SERIAL-SERNR
      IMPORTING
        OUTPUT = L_SERNR.

    CONCATENATE IT_LINE-TEXT ',,' L_SERNR INTO IT_LINE-TEXT.
    IF L_MOD = 0. " sernr ga lc_rownum ko tamattara
      L_INDEX = L_INDEX + 1.
      MOVE-CORRESPONDING IT_SERIAL TO IT_LINE.
      IT_LINE-SORT = LC_SORT-SERNR.
      IT_LINE-INDEX  = L_INDEX.
      IT_LINE-TDFORMAT = 'SE'.
      APPEND IT_LINE.
      CLEAR IT_LINE.
    ENDIF.

    AT END OF VGBEL.
      IF L_MOD <> 0.
        L_INDEX = L_INDEX + 1.
        MOVE-CORRESPONDING IT_SERIAL TO IT_LINE.
        IT_LINE-SORT   = LC_SORT-SERNR.
        IT_LINE-INDEX    = L_INDEX.
        IT_LINE-TDFORMAT = 'SE'.
        APPEND IT_LINE.
        CLEAR: IT_LINE, L_MOD.
      ENDIF.
    ENDAT.

    AT END OF MATNR.
      IT_LINE-MATNR = IT_SERIAL-MATNR.
      IT_LINE-SORT   = LC_SORT-MATNR.
      IT_LINE-TDFORMAT = 'MQ'.
      CONCATENATE ',,Material :' IT_LINE-MATNR 'Qty :' L_MTOTAL INTO IT_LINE-TEXT SEPARATED BY ',,'.
      APPEND IT_LINE.
      CLEAR IT_LINE.
      L_GTOTAL = L_GTOTAL + L_MTOTAL.
    ENDAT.

  ENDLOOP.

  CLEAR IT_TLINE.
  REFRESH IT_TLINE.
  SORT IT_LINE BY MATNR VGBEL SORT INDEX.
  LOOP AT IT_LINE.
    AT NEW MATNR.
      PERFORM APPEND_TDLINE USING '/:' 'PROTECT'.
      PERFORM APPEND_TDLINE_SPACE.
    ENDAT.

    PERFORM APPEND_TDLINE USING IT_LINE-TDFORMAT IT_LINE-TEXT.

    AT END OF MATNR.
      PERFORM APPEND_TDLINE USING '/:' 'ENDPROTECT'.
    ENDAT.

    AT LAST.
      PERFORM APPEND_TDLINE_SPACE.
      CLEAR L_TDLINE.
      CONCATENATE ',,Grand Total :' L_GTOTAL INTO L_TDLINE SEPARATED BY ',,'.
      PERFORM APPEND_TDLINE USING 'GT' L_TDLINE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " MAKE_MAIN_LINES
*&---------------------------------------------------------------------*
*&      Form  APPEND_TDLINE
*&---------------------------------------------------------------------*
FORM APPEND_TDLINE USING P_TDFORMAT
                         P_TDLINE.
  CLEAR IT_TLINE.
  IT_TLINE-TDFORMAT = P_TDFORMAT.
  IT_TLINE-TDLINE   = P_TDLINE.
  APPEND IT_TLINE.

ENDFORM.                    " APPEND_TDLINE
*&---------------------------------------------------------------------*
*&      Form  APPEND_TDLINE_SPACE
*&---------------------------------------------------------------------*
FORM APPEND_TDLINE_SPACE.
  PERFORM APPEND_TDLINE USING '/' '&SPACE&'.

ENDFORM.                    " APPEND_TDLINE_SPACE
*&---------------------------------------------------------------------*
*&      Form  START_FORM
*&---------------------------------------------------------------------*
*      -->P_FORM   form name
*----------------------------------------------------------------------*
FORM START_FORM  USING P_FORM.
  CALL FUNCTION 'START_FORM'
    EXPORTING
      FORM = P_FORM.

ENDFORM.                    " START_FORM
*&---------------------------------------------------------------------*
*&      Form  END_FORM
*&---------------------------------------------------------------------*
FORM END_FORM .
  CALL FUNCTION 'END_FORM'.

ENDFORM.                    " END_FORM
