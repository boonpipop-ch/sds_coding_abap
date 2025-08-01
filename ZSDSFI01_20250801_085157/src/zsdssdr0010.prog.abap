*&---------------------------------------------------------------------*
*& Report ZSDSSDR0010
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0010.
*----------------------------------------------------------------------*
* data                                                                 *
*----------------------------------------------------------------------*

* constants
CONSTANTS: APPL(2)  VALUE 'V3',
           ACTVT(2) VALUE '04',        " activity for output processing
           TRUE     VALUE 'X',
           FALSE    VALUE ' '.

* tables
TABLES: NASE, VBRK, KNA1, TPART ,
        T052,KNB1,VBRP. "T41K922026 " T41K933597

* Mod Hans
TABLES: VRKPA.

DATA: BEGIN OF LT_VRKPA OCCURS 50.
        INCLUDE STRUCTURE VRKPA.
DATA: END OF LT_VRKPA.

DATA: BEGIN OF LT_VBRK OCCURS 50.
        INCLUDE STRUCTURE VBRK.
DATA: END OF LT_VBRK.
* Mod Hans

* inttabs
DATA: MSGS LIKE MSG0 OCCURS 100 WITH HEADER LINE,
      DISP LIKE NALIV3 OCCURS 100 WITH HEADER LINE.

DATA: BEGIN OF AUTH OCCURS 100,
        VKORG LIKE VBRK-VKORG,
        AFLAG TYPE C,
      END OF AUTH.

DATA: BEGIN OF AUTF OCCURS 100,
        FKART LIKE VBRK-FKART,
        AFLAG TYPE C,
      END OF AUTF.

* fields
DATA: REPID LIKE SY-REPID,
      NODIA TYPE C.

"Add by Wantanee "T41K922026
DATA:    BEGIN OF DYNPFIELDS OCCURS 1.
           INCLUDE STRUCTURE DYNPREAD.
DATA:    END   OF DYNPFIELDS.
DATA:  CHAR1(1)       TYPE C.             " Hilfsfeld
"End add by Wantanee "T41K922026

* range  (for conversion of parameter
RANGES: RG_OBJKY FOR NAST-OBJKY,
        RG_FKTYP FOR VBRK-FKTYP,
        RG_LAND1 FOR VBRK-LAND1,
        RG_SPART FOR VBRK-SPART,
        RG_VKORG FOR VBRK-VKORG,
        RG_VTWEG FOR VBRK-VTWEG.
*        rg_zterm FOR vbrk-zterm. "Add by Wantanee 20151225


*----------------------------------------------------------------------*
* A. selection  screen                                                 *
*----------------------------------------------------------------------*

* restricted F4-help to message (output) types of the application
INITIALIZATION.
  NASE-KAPPL = APPL.

* A1. message
  SELECTION-SCREEN BEGIN OF BLOCK MESSAGE WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS:
      RG_KSCHL FOR NASE-KSCHL MEMORY ID NAC,
      RG_NACHA FOR NASE-NACHA.
    PARAMETERS:
      PM_NSORT LIKE NASE-SORV3 DEFAULT '01' OBLIGATORY,
      PM_VERMO TYPE NA_VERMO_NEW DEFAULT '1' OBLIGATORY,
      PM_VERDI LIKE NASE-VERDI NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK MESSAGE.

  SELECTION-SCREEN SKIP.

* A2. billing
  SELECTION-SCREEN BEGIN OF BLOCK APPLICATION WITH FRAME TITLE TEXT-002.
    SELECT-OPTIONS:
      RG_VBELN FOR VBRK-VBELN MEMORY ID VF,
      RG_ZTERM FOR KNB1-ZTERM MEMORY ID VF, "Add by Wantanee 20151225
                                                            "T41K922026
      RG_FKDAT FOR VBRK-FKDAT MEMORY ID DVO,
      RG_ERNAM FOR VBRK-ERNAM,
      RG_ERDAT FOR VBRK-ERDAT.

    PARAMETERS:
      PM_ALLEL LIKE VBCO7-ALLEL DEFAULT TRUE,
      PM_ALLEA LIKE VBCO7-ALLEA DEFAULT TRUE,
      PM_ALLEB LIKE VBCO7-ALLEB,
      PM_ALLEI LIKE VBCO7-ALLEI,
      PM_ALLEF LIKE VBCO7-ALLEF,
      PM_ALLED LIKE VBCO7-ALLEF,
      PM_ETAXM LIKE VBCO7-ALLEF.
    PARAMETERS:
      PM_VKORG LIKE VBRK-VKORG MEMORY ID VKO,
      PM_VTWEG LIKE VBRK-VTWEG MEMORY ID VTW,
      PM_SPART LIKE VBRK-SPART MEMORY ID SPA.
    SELECT-OPTIONS:
      RG_VKBUR FOR VBRP-VKBUR,  " Add by Wantanee 20190930
      RG_VKGRP FOR VBRP-VKGRP,  " Add by Wantanee 20190930
      RG_KUNAG FOR VBRK-KUNAG MATCHCODE OBJECT DEBI,
      RG_KUNRG FOR VBRK-KUNAG MATCHCODE OBJECT DEBI.
    PARAMETERS:
      PM_LAND1 LIKE VBRK-LAND1 MEMORY ID VLL.
  SELECTION-SCREEN END OF BLOCK APPLICATION.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK SELECTION WITH FRAME TITLE TEXT-003.
    PARAMETERS:
      N_SELECT TYPE NA_SEL_NAST RADIOBUTTON GROUP SLCT,
      F_SELECT TYPE NA_SEL_VBRK RADIOBUTTON GROUP SLCT.
  SELECTION-SCREEN END OF BLOCK SELECTION
  .
* It's difficult to transfer billing numbers to objectkeys in order
* to get position messages too. Therfore only a single value or an
* intervall is allowed.
AT SELECTION-SCREEN ON RG_VBELN.
  LOOP AT RG_VBELN.
    IF ( RG_VBELN = 'E' ) OR
    ( ( RG_VBELN-OPTION <> 'EQ' ) AND ( RG_VBELN-OPTION <> 'BT' ) ).
      MESSAGE E076(VN).
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR RG_ZTERM-LOW.
  PERFORM HELP_ZTERM USING  'rg_zterm-low' '*' KNB1-ZTERM."rg_zterm-low.

*----------------------------------------------------------------------*
* B. main program                                                      *
*----------------------------------------------------------------------*

START-OF-SELECTION.
  DATA: LV_VERMO TYPE NA_VERMO_NEW.

  DATA : LV_MEMORY TYPE C LENGTH 255.

  PERFORM PARAMETER_CONVERT.

  IF PM_ETAXM = TRUE.
    CONCATENATE SY-UNAME '_' SY-DATUM INTO LV_MEMORY.
    FREE MEMORY ID LV_MEMORY.

    PERFORM F_EXPORT_MEMORY USING LV_MEMORY.
  ENDIF.

  IF F_SELECT = TRUE.
    PERFORM MESSAGES_INVOIC.
  ENDIF.
  BREAK WANTANEE.
*    PERFORM f_check_saleoffice_salesgroup. " Add by Wantanee 20190930
  IF PM_VERMO = 3.
    CALL FUNCTION 'WFMC_MESSAGES_SELECT_ALL_FALSE'
      EXPORTING
        PI_APPLICATION = APPL
        PI_VSZTP       = ' '
      TABLES
        RI_MEDIUM      = RG_NACHA
        RI_TYPE        = RG_KSCHL
        RI_OBJECT      = RG_OBJKY
        TX_MESSAGES    = MSGS.
  ELSE.
    IF PM_VERMO = 4.
      LV_VERMO = 3.
    ELSE.
      LV_VERMO = PM_VERMO.
    ENDIF.
    CALL FUNCTION 'WFMC_MESSAGES_SELECT'
      EXPORTING
        PI_APPLICATION = APPL
        PI_PROCESSING  = LV_VERMO
      TABLES
        RI_MEDIUM      = RG_NACHA
        RI_TYPE        = RG_KSCHL
        RI_OBJECT      = RG_OBJKY
        TX_MESSAGES    = MSGS.
  ENDIF.

  IF F_SELECT = FALSE.
    PERFORM MESSAGES_FILTER.
  ENDIF.

  CALL FUNCTION 'WFMC_MESSAGES_EXTEND'
    TABLES
      TX_MESSAGES = MSGS.

  PERFORM F_FILLTER_DATA.
  PERFORM F_MODIFY_MSGS.

  PERFORM DISPLAY_PREPARE.

  IF NOT SY-BATCH IS INITIAL.
    PM_VERDI = TRUE.
  ENDIF.
  CALL FUNCTION 'WFMC_MESSAGES_PROCESS'
    EXPORTING
      PI_DISPLAY_ID = 'NALIV3'
      PI_NO_DIALOG  = PM_VERDI
      PI_VERMO      = PM_VERMO
    TABLES
      TX_MESSAGES   = MSGS
      TX_DISPLAY    = DISP.

  IF PM_ETAXM = TRUE.
    CONCATENATE SY-UNAME '_' SY-DATUM INTO LV_MEMORY.
    FREE MEMORY ID LV_MEMORY.
  ENDIF.
*----------------------------------------------------------------------*
* C. routines (internal)                                               *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM PARAMETER_CONVERT                                        *
*---------------------------------------------------------------------*
*       convert selection parameter                                   *
*---------------------------------------------------------------------*
FORM PARAMETER_CONVERT.

* read billing type (category?)
  REFRESH RG_FKTYP.
  RG_FKTYP-SIGN = 'I'.
  RG_FKTYP-OPTION = 'EQ'.
  IF PM_ALLEL = TRUE.
    RG_FKTYP-LOW = 'L'.
    APPEND RG_FKTYP.
    RG_FKTYP-LOW = 'X'.
    APPEND RG_FKTYP.
  ENDIF.
  IF PM_ALLEA = TRUE.
    RG_FKTYP-LOW = 'A'.
    APPEND RG_FKTYP.
    RG_FKTYP-LOW = 'P'.
    APPEND RG_FKTYP.
*   check double entry
    IF PM_ALLEL = FALSE.
      RG_FKTYP-LOW = 'X'.
      APPEND RG_FKTYP.
    ENDIF.
  ENDIF.
  IF PM_ALLEB = TRUE.
    RG_FKTYP-LOW = 'B'.
    APPEND RG_FKTYP.
    RG_FKTYP-LOW = 'C'.
    APPEND RG_FKTYP.
    RG_FKTYP-LOW = 'K'.
    APPEND RG_FKTYP.
  ENDIF.
  IF PM_ALLEI = TRUE.
    RG_FKTYP-LOW = 'I'.
    APPEND RG_FKTYP.
  ENDIF.
  IF PM_ALLEF = TRUE.
    RG_FKTYP-LOW = 'R'.
    APPEND RG_FKTYP.
  ENDIF.
  IF PM_ALLED = TRUE.
    RG_FKTYP-LOW = 'D'.
    APPEND RG_FKTYP.
  ENDIF.

* read sales organization
  REFRESH RG_VKORG.
  IF NOT PM_VKORG IS INITIAL.
    RG_VKORG-SIGN   = 'I'.
    RG_VKORG-OPTION = 'EQ'.
    RG_VKORG-LOW    = PM_VKORG.
    APPEND RG_VKORG.
  ENDIF.

* read distribution channel
  REFRESH RG_VTWEG.
  IF NOT PM_VTWEG IS INITIAL.
    RG_VTWEG-SIGN   = 'I'.
    RG_VTWEG-OPTION = 'EQ'.
    RG_VTWEG-LOW    = PM_VTWEG.
    APPEND RG_VTWEG.
  ENDIF.

* read division
  REFRESH RG_SPART.
  IF NOT PM_SPART IS INITIAL.
    RG_SPART-SIGN   = 'I'.
    RG_SPART-OPTION = 'EQ'.
    RG_SPART-LOW    = PM_SPART.
    APPEND RG_SPART.
  ENDIF.

* read country
  REFRESH RG_LAND1.
  IF NOT PM_LAND1 IS INITIAL.
    RG_LAND1-SIGN   = 'I'.
    RG_LAND1-OPTION = 'EQ'.
    RG_LAND1-LOW    = PM_LAND1.
    APPEND RG_LAND1.
  ENDIF.

* transfer document numbers as object keys
  REFRESH RG_OBJKY.

  IF PM_ALLEF = 'X'.                   "invoice list
    LOOP AT RG_VBELN.
      CLEAR RG_OBJKY.
      CASE RG_VBELN-OPTION.
        WHEN 'EQ'.
          RG_OBJKY-SIGN   = RG_VBELN-SIGN.
          RG_OBJKY-OPTION = 'CP'.
          RG_OBJKY-LOW    = RG_VBELN-LOW.
          RG_OBJKY-LOW+10 = '*'.
          APPEND RG_OBJKY.
        WHEN 'BT'.
          RG_OBJKY-SIGN   = RG_VBELN-SIGN.
          RG_OBJKY-OPTION = 'BT'.
          RG_OBJKY-LOW    = RG_VBELN-LOW.
          RG_OBJKY-HIGH   = RG_VBELN-HIGH.
          APPEND RG_OBJKY.
          IF NOT RG_VBELN-LOW IS INITIAL.
            RG_OBJKY-LOW+10 = '0000000000000000'.
          ENDIF.
          IF NOT RG_VBELN-HIGH IS INITIAL.
            RG_OBJKY-HIGH+10 = '9999999999999999'.
          ENDIF.
          APPEND RG_OBJKY.
      ENDCASE.
    ENDLOOP.
  ELSE.
    LOOP AT RG_VBELN.
      CLEAR RG_OBJKY.
      MOVE-CORRESPONDING RG_VBELN TO RG_OBJKY.
      APPEND RG_OBJKY.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "PARAMETER_CONVERT


*---------------------------------------------------------------------*
*       FORM MESSAGES_FILTER                                          *
*---------------------------------------------------------------------*
*       filter messages                                               *
*---------------------------------------------------------------------*
FORM MESSAGES_FILTER.

  DATA: FLAG         TYPE C,
        LT_MSGS_SORT TYPE SORTED TABLE OF MSG0
                     WITH NON-UNIQUE KEY OBJKY KSCHL.
  DATA: CHECK_INV TYPE VBRK-VBELN.

  FIELD-SYMBOLS:
        <FS_MSGS>    TYPE MSG0.

  SORT MSGS BY OBJKY KSCHL.

  LT_MSGS_SORT = MSGS[].
  BREAK WANTANEE.
  LOOP AT LT_MSGS_SORT ASSIGNING <FS_MSGS>.

*--- only check new ones due to mass deletion
    CHECK <FS_MSGS>-OBJKY(10) NE VBRK-VBELN.
    FLAG = TRUE.


*    --- select from VBRK
    SELECT SINGLE * FROM VBRK
            WHERE VBELN = <FS_MSGS>-OBJKY(10) AND
                  FKTYP IN RG_FKTYP AND
                  FKDAT IN RG_FKDAT AND
                  ERNAM IN RG_ERNAM AND
                  ERDAT IN RG_ERDAT AND
                  VKORG IN RG_VKORG AND
                  VTWEG IN RG_VTWEG AND
                  SPART IN RG_SPART AND
                  KUNAG IN RG_KUNAG AND
                  KUNRG IN RG_KUNRG AND
                  LAND1 IN RG_LAND1 AND
                  ZTERM IN RG_ZTERM.  "Add by Wantanee 20151225
    IF SY-SUBRC <> 0.
      FLAG = FALSE.
    ENDIF.
*    --- check authorization concerning VKORG
    IF FLAG NE FALSE.
      READ TABLE AUTH WITH KEY VKORG = VBRK-VKORG BINARY SEARCH.
      IF SY-SUBRC <> 0.
        AUTH-VKORG = VBRK-VKORG.
        AUTHORITY-CHECK OBJECT 'V_VBRK_VKO'
          ID 'VKORG' FIELD AUTH-VKORG
          ID 'ACTVT' FIELD ACTVT.
        IF SY-SUBRC = 0.
          AUTH-AFLAG = TRUE.
        ELSE.
          AUTH-AFLAG = FALSE.
          FLAG = FALSE.
        ENDIF.
        INSERT AUTH INDEX SY-TABIX.
      ELSEIF AUTH-AFLAG = FALSE.
        FLAG = FALSE.
      ENDIF.
    ENDIF.
*    --- check authorization concerning FKART
    IF FLAG NE FALSE.
      READ TABLE AUTF WITH KEY FKART = VBRK-FKART BINARY SEARCH.
      IF SY-SUBRC <> 0.
        AUTF-FKART = VBRK-FKART.
        AUTHORITY-CHECK OBJECT 'V_VBRK_FKA'
          ID 'FKART' FIELD AUTF-FKART
          ID 'ACTVT' FIELD ACTVT.
        IF SY-SUBRC = 0.
          AUTF-AFLAG = TRUE.
        ELSE.
          AUTF-AFLAG = FALSE.
          FLAG = FALSE.
        ENDIF.
        INSERT AUTF INDEX SY-TABIX.
      ELSEIF AUTF-AFLAG = FALSE.
        FLAG = FALSE.
      ENDIF.
    ENDIF.
*    --- delete all messages for the key
    IF FLAG = FALSE.
      DELETE LT_MSGS_SORT WHERE OBJKY(10) = <FS_MSGS>-OBJKY(10).
    ENDIF.
*---20190930 case document have sale group and sale office T41K933673
    IF FLAG NE FALSE.
      CLEAR: CHECK_INV.
      IF RG_VKBUR IS NOT INITIAL OR RG_VKGRP IS NOT INITIAL.
        SELECT SINGLE VBELN
          INTO CHECK_INV
          FROM VBRP
          WHERE VBELN EQ <FS_MSGS>-OBJKY(10)
          AND VKBUR IN RG_VKBUR
          AND   VKGRP IN RG_VKGRP.

        IF CHECK_INV IS INITIAL.
          DELETE LT_MSGS_SORT WHERE OBJKY(10) = <FS_MSGS>-OBJKY(10).
        ENDIF.

      ENDIF.
    ENDIF.

*---End 20190930 case document have sale group and sale office T41K933673


  ENDLOOP.

  MSGS[] = LT_MSGS_SORT.

ENDFORM.                    "MESSAGES_FILTER

*---------------------------------------------------------------------*
*       FORM MESSAGES_INVOIC                                          *
*---------------------------------------------------------------------*
*       filter invoices                                               *
*---------------------------------------------------------------------*
FORM MESSAGES_INVOIC.

  DATA: LD_NEXT_VBELN TYPE VBRK-VBELN.
  DATA: LD_TABIX TYPE SY-TABIX.
  DATA: FLAG TYPE C.

  BREAK WANTANEE.

  REFRESH RG_OBJKY.
  IF NOT RG_KUNRG IS INITIAL.
    SELECT VRKPA~* FROM VRKPA
      INNER JOIN VBRK ON VRKPA~VBELN EQ VBRK~VBELN
      INTO TABLE @LT_VRKPA WHERE VRKPA~VBELN IN @RG_VBELN
                             AND VRKPA~FKTYP IN @RG_FKTYP AND
                                 VRKPA~FKDAT IN @RG_FKDAT AND
                                 VBRK~ERNAM  IN @RG_ERNAM AND
                                 VBRK~ERDAT  IN @RG_ERDAT AND
                                 VRKPA~VKORG IN @RG_VKORG AND
                                 VRKPA~VTWEG IN @RG_VTWEG AND
                                 VRKPA~KUNAG IN @RG_KUNAG AND
                                 VRKPA~KUNNR IN @RG_KUNRG.
    "Add by Wantanee 20151225
*                                            zterm IN rg_zterm.

    IF NOT LT_VRKPA[] IS INITIAL.
      SELECT * FROM VBRK INTO TABLE LT_VBRK FOR ALL ENTRIES
         IN LT_VRKPA WHERE VBELN = LT_VRKPA-VBELN.
    ENDIF.
  ELSE.

    SELECT * FROM VBRK INTO TABLE LT_VBRK
            WHERE VBELN IN RG_VBELN AND
                  FKTYP IN RG_FKTYP AND
                  FKDAT IN RG_FKDAT AND
                  ERNAM IN RG_ERNAM AND
                  ERDAT IN RG_ERDAT AND
                  VKORG IN RG_VKORG AND
                  VTWEG IN RG_VTWEG AND
                  SPART IN RG_SPART AND
                  KUNAG IN RG_KUNAG AND
                  KUNRG IN RG_KUNRG AND
                  LAND1 IN RG_LAND1 AND
                  ZTERM IN RG_ZTERM.  "Add by Wantanee 20151225
  ENDIF.

  LOOP AT LT_VBRK.
    FLAG = TRUE.
*--- check authorization concerning VKORG
    READ TABLE AUTH WITH KEY VKORG = LT_VBRK-VKORG BINARY SEARCH.
    IF SY-SUBRC <> 0.
      AUTH-VKORG = LT_VBRK-VKORG.
      AUTHORITY-CHECK OBJECT 'V_VBRK_VKO'
        ID 'VKORG' FIELD AUTH-VKORG
        ID 'ACTVT' FIELD ACTVT.
      IF SY-SUBRC = 0.
        AUTH-AFLAG = TRUE.
      ELSE.
        AUTH-AFLAG = FALSE.
        FLAG = FALSE.
      ENDIF.
      INSERT AUTH INDEX SY-TABIX.
    ELSEIF AUTH-AFLAG = FALSE.
      FLAG = FALSE.
    ENDIF.
*--- check authorization concerning FKART
    IF FLAG NE FALSE.
      READ TABLE AUTF WITH KEY FKART = LT_VBRK-FKART BINARY SEARCH.
      IF SY-SUBRC <> 0.
        AUTF-FKART = LT_VBRK-FKART.
        AUTHORITY-CHECK OBJECT 'V_VBRK_FKA'
          ID 'FKART' FIELD AUTF-FKART
          ID 'ACTVT' FIELD ACTVT.
        IF SY-SUBRC = 0.
          AUTF-AFLAG = TRUE.
        ELSE.
          AUTF-AFLAG = FALSE.
          FLAG = FALSE.
        ENDIF.
        INSERT AUTF INDEX SY-TABIX.
      ELSEIF AUTF-AFLAG = FALSE.
        FLAG = FALSE.
      ENDIF.
    ENDIF.

    IF FLAG NE FALSE.
      CLEAR RG_OBJKY.
*--- Should invoice lists be considered?
      IF PM_ALLEF IS INITIAL.
        IF LD_NEXT_VBELN = LT_VBRK-VBELN.
          READ TABLE RG_OBJKY INDEX LD_TABIX.
          RG_OBJKY-OPTION = 'BT'.
          RG_OBJKY-HIGH = LT_VBRK-VBELN.
          MODIFY RG_OBJKY INDEX LD_TABIX.
        ELSE.
          RG_OBJKY-SIGN   = 'I'.
          RG_OBJKY-OPTION = 'EQ'.
          RG_OBJKY-LOW    = LT_VBRK-VBELN.
          APPEND RG_OBJKY.
          LD_TABIX = SY-TABIX.
        ENDIF.
        LD_NEXT_VBELN = LT_VBRK-VBELN + '1'.
        SHIFT LD_NEXT_VBELN RIGHT.
        OVERLAY LD_NEXT_VBELN WITH '0000000000'.
      ELSE.
*--- if the document is an invoice list
        IF LT_VBRK-FKTYP EQ 'R'.
          RG_OBJKY-SIGN   = 'I'.
          RG_OBJKY-OPTION = 'CP'.
          RG_OBJKY-LOW    = LT_VBRK-VBELN.
          RG_OBJKY-LOW+10 = '*'.
          APPEND RG_OBJKY.
        ELSE.
          RG_OBJKY-SIGN   = 'I'.
          RG_OBJKY-OPTION = 'EQ'.
          RG_OBJKY-LOW    = LT_VBRK-VBELN.
          APPEND RG_OBJKY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF RG_OBJKY[] IS INITIAL.
    RG_OBJKY-SIGN   = 'I'.
    RG_OBJKY-OPTION = 'EQ'.
    CLEAR: RG_OBJKY-LOW, RG_OBJKY-HIGH.
    APPEND RG_OBJKY.
  ENDIF.


ENDFORM.                    "MESSAGES_INVOIC

*---------------------------------------------------------------------*
*       FORM MESSAGES_PREPARE                                         *
*---------------------------------------------------------------------*
*     sort messages and create display table                          *
*---------------------------------------------------------------------*
FORM DISPLAY_PREPARE.

* sort messages
  CASE PM_NSORT.
    WHEN '01'.
      SORT MSGS BY OBJKY KSCHL.
    WHEN '02'.
      SORT MSGS BY KSCHL OBJKY.
    WHEN '03'.
      SORT MSGS BY PARNR KSCHL.
    WHEN '04'.
      SORT MSGS BY SORT1 SORT2 SORT3.
    WHEN '05'.
      SORT MSGS BY KSCHL SORT1 SORT2 SORT3.
    WHEN OTHERS.
      SORT MSGS BY OBJKY.
  ENDCASE.

* create display
  LOOP AT MSGS.
    MSGS-TABIX = SY-TABIX.
    MODIFY MSGS.
    MOVE-CORRESPONDING MSGS TO DISP.
    DISP-VBELN = MSGS-OBJKY+00(10).
    DISP-POSNR = MSGS-OBJKY+10(06).
    DISP-TABIX = SY-TABIX.
    APPEND DISP.
  ENDLOOP.

ENDFORM.                    "DISPLAY_PREPARE
*&---------------------------------------------------------------------*
*&      Form  HELP_ZTERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->H01_FNAME  text
*      -->H01_FAUSW  text
*      -->H01_ZTERM  text
*----------------------------------------------------------------------*
FORM HELP_ZTERM USING
     H01_FNAME LIKE RFCU3-FNAME
     H01_FAUSW TYPE C
     H01_ZTERM LIKE T052-ZTERM.

  DATA: DYNNR LIKE SY-DYNNR,          " aktuelles Dynpro
        ZTERM LIKE T052-ZTERM.        " Inhalt des Feldes H01_FNAME

*------ Inhalt des Feldes H01_FNAME vom Dynpro besorgen ----------------
  CLEAR   DYNPFIELDS.
  REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = H01_FNAME.
  APPEND DYNPFIELDS.
  DYNNR = SY-DYNNR.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = 'SAPMF02D'
      DYNUMB     = DYNNR
    TABLES
      DYNPFIELDS = DYNPFIELDS
    EXCEPTIONS
      OTHERS     = 4.
  IF SY-SUBRC = 0.
    READ TABLE DYNPFIELDS INDEX 1.
    ZTERM = DYNPFIELDS-FIELDVALUE.
    TRANSLATE ZTERM TO UPPER CASE.                       "#EC TRANSLANG
  ENDIF.

*------ Feld H01_FNAME Eingabe- oder Anzeigefeld? ----------------------
  PERFORM FELDSTATUS_ERMITTELN USING H01_FNAME CHAR1.

*------ Zahlungsbedingungen anzeigen -----------------------------------
  CALL FUNCTION 'FI_F4_ZTERM'
    EXPORTING
      I_KOART = 'D'
      I_ZTERM = ZTERM
      I_XSHOW = CHAR1
    IMPORTING
      E_ZTERM = T052-ZTERM.
  IF NOT T052-ZTERM IS INITIAL.
    H01_ZTERM = T052-ZTERM.
    RG_ZTERM-LOW = T052-ZTERM.

  ENDIF.
ENDFORM.                    "HELP_ZTERM
*---------------------------------------------------------------------*
*        FORM FELDSTATUS_ERMITTELN                                    *
*---------------------------------------------------------------------*
*        Feldstatus auf dem Screen zu einem Feld ermitteln            *
*---------------------------------------------------------------------*
*        --> FNAME   Feldname                                         *
*        <-- XSHOW   Kennzeichen: Feld wird nur angezeigt?            *
*---------------------------------------------------------------------*
FORM FELDSTATUS_ERMITTELN USING FNAME XSHOW.
  LOOP AT SCREEN.
    CHECK SCREEN-NAME = FNAME.
    IF SCREEN-INPUT = '0'.
      XSHOW = 'X'.
    ELSE.
      XSHOW = SPACE.
    ENDIF.
    EXIT.
  ENDLOOP.
ENDFORM.                    "FELDSTATUS_ERMITTELN
**---------------------------------------------------------------------*
**---------------------------------------------------------------------*
**        FORM F_CHECK_SALESOFFICE_SALESGROUP                               *
**---------------------------------------------------------------------*
**        Feldstatus auf dem Screen zu einem Feld ermitteln            *
**---------------------------------------------------------------------*
**        --> FNAME   Feldname                                         *
**        <-- XSHOW             *
**---------------------------------------------------------------------*
*FORM f_check_saleoffice_salesgroup.
**  rg_objky
*   IF rg_vkbur IS NOT INITIAL OR rg_vkgrp IS NOT INITIAL.
*
*
*   ENDIF.
*
*ENDFORM.                    "FELDSTATUS_ERMITTELN
**---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_EXPORT_MEMORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_MEMORY  text
*----------------------------------------------------------------------*
FORM F_EXPORT_MEMORY USING LV_MEMORY.

  EXPORT PM_ETAXM TO MEMORY ID LV_MEMORY.

ENDFORM.                    " F_EXPORT_MEMORY
*&---------------------------------------------------------------------*
*& Form F_MODIFY_MSGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_MODIFY_MSGS .
  DATA LS_MSGS LIKE LINE OF MSGS[].
  LS_MSGS-USNAM = SY-UNAME.

  SELECT SINGLE LDEST
    FROM ZSDSSDC028
    INTO LS_MSGS-LDEST
    WHERE UNAME = SY-UNAME.
  IF SY-SUBRC EQ 0.
    MODIFY MSGS FROM LS_MSGS TRANSPORTING USNAM LDEST
                                    WHERE USNAM IS NOT INITIAL.
  ELSE.
    MODIFY MSGS FROM LS_MSGS TRANSPORTING USNAM
                                    WHERE USNAM IS NOT INITIAL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILLTER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_FILLTER_DATA .
*  IF RG_ERNAM[] IS NOT INITIAL.
*    DELETE MSGS WHERE USNAM NOT IN RG_ERNAM[].
*  ENDIF.
*
*  IF RG_ERDAT[] IS NOT INITIAL.
*    DELETE MSGS WHERE ERDAT NOT IN RG_ERDAT[].
*  ENDIF.
ENDFORM.
