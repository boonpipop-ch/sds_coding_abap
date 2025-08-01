class ZCL_SDSCA_DATA_VALIDATION definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods VALIDATE_DATE
    importing
      !IF_INPUT type CLIKE
      !IF_FORMAT type CHAR1 default ' '
    exporting
      !EF_OUTPUT type SY-DATUM
      !EF_INVALID type FLAG .
  methods VALIDATE_NUMBER
    importing
      !IF_INPUT type CLIKE
      !IF_FORMAT type CHAR1 default ' '
    exporting
      !EF_OUTPUT type ANY
      !EF_INVALID type FLAG .
  methods VALIDATE_FLAG
    importing
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type FLAG
      !EF_INVALID type FLAG .
  methods VALIDATE_LOB
    importing
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type CLIKE
      !EF_INVALID type FLAG .
  methods VALIDATE_PROJ_RESPPERS
    importing
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type PROJ-VERNR
      !EF_INVALID type FLAG .
  methods VALIDATE_PROJ_PROFILE
    importing
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type PROJ-PROFL
      !EF_INVALID type FLAG .
  methods VALIDATE_WBSELEM
    importing
      !IF_INPUT type CLIKE
      !IF_CREATE type FLAG default ' '
    exporting
      !EF_OUTPUT type PRPS-POSID
      !EF_INVALID type FLAG
      !EF_PSPID type PROJ-PSPID
      !EF_OBJNR type PRPS-OBJNR .
  methods VALIDATE_PROJECT_TYPE
    importing
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type PRPS-PRART
      !EF_INVALID type FLAG .
  methods VALIDATE_CO_VERSION
    importing
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type TKVS-VERSI
      !EF_INVALID type FLAG .
  methods VALIDATE_PROFIT_CENTER
    importing
      !IF_BUKRS type TKA02-BUKRS optional
      !IF_KOKRS type CEPC-KOKRS default '1000'
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type CEPC-PRCTR
      !EF_INVALID type FLAG .
  methods VALIDATE_COST_CENTER
    importing
      !IF_BUKRS type TKA02-BUKRS optional
      !IF_KOKRS type CSKS-KOKRS default '1000'
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type CSKS-KOSTL
      !EF_INVALID type FLAG .
  methods VALIDATE_COST_ELEMENT
    importing
      !IF_BUKRS type T001-BUKRS optional
      !IF_KTOPL type CSKA-KTOPL default 'RCOA'
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type CSKA-KSTAR
      !EF_INVALID type FLAG .
  methods VALIDATE_DOMAIN_VALUE
    importing
      !IF_DOMNAME type DD07L-DOMNAME
      !IF_INPUT type CLIKE
    exporting
      !EF_OUTPUT type ANY
      !EF_INVALID type FLAG .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF TS_TCJ04,
      VERNR TYPE  TCJ04-VERNR,
    END OF TS_TCJ04 .
  TYPES:
    TT_TCJ04 TYPE SORTED TABLE OF TS_TCJ04
                       WITH UNIQUE KEY VERNR .
  TYPES:
    BEGIN OF TS_TCJ41,
      PROFL TYPE  TCJ41-PROFIDPROJ,
    END OF TS_TCJ41 .
  TYPES:
    TT_TCJ41 TYPE SORTED TABLE OF TS_TCJ41
                       WITH UNIQUE KEY PROFL .
  TYPES:
    BEGIN OF TS_TCJ1,
      PRART TYPE  TCJ1-PRART,
    END OF TS_TCJ1 .
  TYPES:
    TT_TCJ1 TYPE SORTED TABLE OF TS_TCJ1
                       WITH UNIQUE KEY PRART .
  TYPES:
    BEGIN OF TS_TKVS,
      VERSN TYPE  TKVS-VERSI,
    END OF TS_TKVS .
  TYPES:
    TT_TKVS TYPE SORTED TABLE OF TS_TKVS
                       WITH UNIQUE KEY VERSN .
  TYPES:
    BEGIN OF TS_CEPC,
      BUKRS TYPE  TKA02-BUKRS,
      KOKRS TYPE  CEPC-KOKRS,
      PRCTR TYPE  CEPC-PRCTR,
    END OF TS_CEPC .
  TYPES:
    TT_CEPC TYPE SORTED TABLE OF TS_CEPC
                       WITH UNIQUE KEY PRCTR
                                       BUKRS
                                       KOKRS .
  TYPES:
    BEGIN OF TS_CSKS,
      BUKRS TYPE  TKA02-BUKRS,
      KOKRS TYPE  CSKS-KOKRS,
      KOSTL TYPE  CSKS-KOSTL,
    END OF TS_CSKS .
  TYPES:
    TT_CSKS TYPE SORTED TABLE OF TS_CSKS
                       WITH UNIQUE KEY KOSTL
                                       BUKRS
                                       KOKRS .
  TYPES:
    BEGIN OF TS_CSKA,
      BUKRS TYPE  T001-BUKRS,
      KTOPL TYPE  CSKA-KTOPL,
      KSTAR TYPE  CSKA-KSTAR,
    END OF TS_CSKA .
  TYPES:
    TT_CSKA TYPE SORTED TABLE OF TS_CSKA
                       WITH UNIQUE KEY KSTAR
                                       BUKRS
                                       KTOPL .
  TYPES:
    BEGIN OF TS_ZDSMMC033,
      LOB TYPE  ZDSMMC033-LOB,
    END OF TS_ZDSMMC033 .
  TYPES:
    TT_ZDSMMC033 TYPE SORTED TABLE OF TS_ZDSMMC033
                       WITH UNIQUE KEY LOB .

  TYPES:
    BEGIN OF TS_DD07L,
      DOMNAME  TYPE  DD07L-DOMNAME,
      DOMVALUE TYPE  DD07L-DOMVALUE_L,
    END OF TS_DD07L.
  TYPES:
    TT_DD07L TYPE SORTED TABLE OF TS_DD07L
                    WITH UNIQUE KEY DOMNAME
                                    DOMVALUE.

  DATA GT_TCJ04 TYPE TT_TCJ04 .
  DATA GT_TCJ41 TYPE TT_TCJ41 .
  DATA GT_TCJ1 TYPE TT_TCJ1 .
  DATA GT_TKVS TYPE TT_TKVS .
  DATA GT_CEPC TYPE TT_CEPC .
  DATA GT_CSKS TYPE TT_CSKS .
  DATA GT_CSKA TYPE TT_CSKA .
  DATA GT_ZDSMMC033 TYPE TT_ZDSMMC033 .
  DATA GT_DD07L TYPE TT_DD07L .
ENDCLASS.



CLASS ZCL_SDSCA_DATA_VALIDATION IMPLEMENTATION.


METHOD CONSTRUCTOR.
* Initialize Variables
  CLEAR:
    GT_TCJ04,
    GT_TCJ41,
    GT_TCJ1,
    GT_TKVS,
    GT_CEPC,
    GT_CSKS,
    GT_CSKA,
    GT_ZDSMMC033,
    GT_DD07L.
ENDMETHOD.


METHOD VALIDATE_COST_CENTER.

  DATA:
    LS_CSKS  TYPE  TS_CSKS.

  DATA:
    LF_BUKRS TYPE  TS_CSKS-BUKRS,
    LF_KOSTL TYPE  TS_CSKS-KOSTL.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 10.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = IF_INPUT
    IMPORTING
      OUTPUT = LF_KOSTL.

  IF IF_BUKRS IS SUPPLIED.
    LF_BUKRS = IF_BUKRS.
  ENDIF.

  IF LF_BUKRS IS INITIAL.
    READ TABLE GT_CSKS INTO LS_CSKS
                       WITH KEY KOSTL = LF_KOSTL
                                BUKRS = LF_BUKRS
                                KOKRS = IF_KOKRS
                       BINARY SEARCH.
  ELSE.
    READ TABLE GT_CSKS INTO LS_CSKS
                       WITH KEY KOSTL = LF_KOSTL
                                BUKRS = LF_BUKRS
                       BINARY SEARCH.
  ENDIF.
  IF SY-SUBRC NE 0.

    IF LF_BUKRS IS INITIAL.
      SELECT @SPACE AS BUKRS,
             KOKRS,
             KOSTL
        FROM CSKS
       WHERE KOKRS EQ @IF_KOKRS
         AND KOSTL EQ @LF_KOSTL
         AND DATAB LE @SY-DATUM
         AND DATBI GE @SY-DATUM
        INTO @LS_CSKS
          UP TO 1 ROWS.
      ENDSELECT.
    ELSE.
      SELECT B~BUKRS,
             A~KOKRS,
             A~KOSTL
        FROM CSKS AS A
               INNER JOIN TKA02 AS B
                 ON  B~KOKRS = A~KOKRS
                 AND B~GSBER = @SPACE
       WHERE A~KOSTL EQ @LF_KOSTL
         AND B~BUKRS EQ @LF_BUKRS
         AND A~DATAB LE @SY-DATUM
         AND A~DATBI GE @SY-DATUM
        INTO @LS_CSKS
          UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_CSKS INTO TABLE GT_CSKS.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_CSKS-KOSTL.

ENDMETHOD.


METHOD VALIDATE_COST_ELEMENT.

  DATA:
    LS_CSKA  TYPE  TS_CSKA.

  DATA:
    LF_BUKRS TYPE  TS_CSKA-BUKRS,
    LF_KSTAR TYPE  TS_CSKA-KSTAR.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 10.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = IF_INPUT
    IMPORTING
      OUTPUT = LF_KSTAR.

  IF IF_BUKRS IS SUPPLIED.
    LF_BUKRS = IF_BUKRS.
  ENDIF.

  IF LF_BUKRS IS INITIAL.
    READ TABLE GT_CSKA INTO LS_CSKA
                       WITH KEY KSTAR = LF_KSTAR
                                BUKRS = LF_BUKRS
                                KTOPL = IF_KTOPL
                       BINARY SEARCH.
  ELSE.
    READ TABLE GT_CSKA INTO LS_CSKA
                       WITH KEY KSTAR = LF_KSTAR
                                BUKRS = LF_BUKRS
                       BINARY SEARCH.
  ENDIF.
  IF SY-SUBRC NE 0.

    IF LF_BUKRS IS INITIAL.
      SELECT SINGLE @SPACE AS BUKRS,
                    KTOPL,
                    KSTAR
        FROM CSKA
       WHERE KTOPL EQ @IF_KTOPL
         AND KSTAR EQ @LF_KSTAR
        INTO @LS_CSKA.
    ELSE.
      SELECT B~BUKRS,
             A~KTOPL,
             A~KSTAR
        FROM CSKA AS A
               INNER JOIN T001 AS B
                 ON  B~KTOPL = A~KTOPL
       WHERE A~KSTAR EQ @LF_KSTAR
         AND B~BUKRS EQ @LF_BUKRS
        INTO @LS_CSKA
          UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_CSKA INTO TABLE GT_CSKA.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_CSKA-KSTAR.

ENDMETHOD.


METHOD VALIDATE_CO_VERSION.

  DATA:
    LS_TKVS  TYPE  TS_TKVS.

  DATA:
    LF_VERSN  TYPE  TKVS-VERSI.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 3.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = IF_INPUT
    IMPORTING
      OUTPUT = LF_VERSN.

  READ TABLE GT_TKVS INTO LS_TKVS
                      WITH KEY VERSN = LF_VERSN
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    SELECT SINGLE VERSI
      FROM TKVS
     WHERE VERSI EQ @LF_VERSN
      INTO @LS_TKVS.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_TKVS INTO TABLE GT_TKVS.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_TKVS-VERSN.

ENDMETHOD.


METHOD VALIDATE_DATE.

  DATA:
    LF_DATUM  TYPE  SY-DATUM.


* Initialize output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  TRY.
*     Check Format
      CASE IF_FORMAT.
*       -----------------------
*       Format DD.MM.YYYY
*       -----------------------
        WHEN SPACE.
*       Check Length
          IF STRLEN( IF_INPUT ) NE 10.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
          IF NOT IF_INPUT CO '1234567890.'.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
          LF_DATUM = IF_INPUT+6(4) && IF_INPUT+3(2) && IF_INPUT(2).

        WHEN OTHERS.
          EF_INVALID = 'X'.
          RETURN.
      ENDCASE.

      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          DATE                      = LF_DATUM
        EXCEPTIONS
          PLAUSIBILITY_CHECK_FAILED = 1
          OTHERS                    = 2.
      IF SY-SUBRC <> 0.
        EF_INVALID = 'X'.
        RETURN.
      ENDIF.

    CATCH CX_ROOT.
      EF_INVALID = 'X'.
      RETURN.

  ENDTRY.

* Assign Result
  EF_OUTPUT = LF_DATUM.

ENDMETHOD.


METHOD VALIDATE_DOMAIN_VALUE.

  DATA:
    LS_DD07L  TYPE  TS_DD07L.

  DATA:
    LF_DOMVALUE  TYPE  DD07L-DOMVALUE_L.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 10.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  LF_DOMVALUE  = IF_INPUT.

  READ TABLE GT_DD07L INTO LS_DD07L
                      WITH KEY DOMNAME = IF_DOMNAME
                               DOMVALUE = LF_DOMVALUE
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    SELECT DOMNAME,
           DOMVALUE_L AS DOMVALUE
      FROM DD07L
     WHERE DOMNAME    EQ @IF_DOMNAME
       AND AS4LOCAL   EQ 'A'
       AND AS4VERS    EQ '0000'
       AND DOMVALUE_L EQ @LF_DOMVALUE
     ORDER BY VALPOS ASCENDING
      INTO @LS_DD07L
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_DD07L INTO TABLE GT_DD07L.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_DD07L-DOMVALUE.

ENDMETHOD.


METHOD VALIDATE_FLAG.

* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 1 OR
     NOT IF_INPUT CO ' Xx'.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

* Assign Output
  EF_OUTPUT = IF_INPUT.
  TRANSLATE EF_OUTPUT TO UPPER CASE.

ENDMETHOD.


METHOD VALIDATE_LOB.

  DATA:
    LS_ZDSMMC033  TYPE  TS_ZDSMMC033.

  DATA:
    LF_LOB  TYPE  ZDSMMC033-LOB.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 3.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  LF_LOB  = IF_INPUT.

  READ TABLE GT_ZDSMMC033 INTO LS_ZDSMMC033
                      WITH KEY LOB = LF_LOB
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    SELECT SINGLE LOB
      FROM ZDSMMC033
     WHERE LOB EQ @LF_LOB
       AND ZLANG EQ @SY-LANGU
      INTO @LS_ZDSMMC033.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_ZDSMMC033 INTO TABLE GT_ZDSMMC033.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_ZDSMMC033-LOB.

ENDMETHOD.


METHOD VALIDATE_NUMBER.

  DATA:
    LF_INPUT  TYPE  TEXT1000,
    LF_COUNT  TYPE  I,
    LF_OFFSET TYPE  I,
    LF_MINUS  TYPE  FLAG.


* Initialize output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* -----------------------
* Format List
* SPACE = Integer
* G = Fiscal Year (1800-2200)
* M = Month (01-12)
* A = Amount (-XX,XXX.XX)
* -----------------------

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  LF_INPUT = IF_INPUT.

  TRY.
*     Check Format
      CASE IF_FORMAT.
*       -----------------------
*       Format Integer
*       -----------------------
        WHEN SPACE.
          IF NOT LF_INPUT CO '1234567890 '.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.

*       -----------------------
*       Format Fiscal Year
*       -----------------------
        WHEN 'G'.
          IF NOT LF_INPUT CO '1234567890 '.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
          IF LF_INPUT LT 1800 OR
             LF_INPUT GT 2200.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.

*       -----------------------
*       Format Month
*       -----------------------
        WHEN 'M'.
          IF NOT LF_INPUT CO '1234567890 '.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
          IF LF_INPUT LT 1 OR
             LF_INPUT GT 12.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.

*       -----------------------
*       Format Amount
*       -----------------------
        WHEN 'A'.
          IF NOT LF_INPUT CO '1234567890,.- '.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
*         Find Decimal
          CLEAR: LF_COUNT, LF_OFFSET.
          FIND ALL OCCURRENCES OF '.' IN LF_INPUT MATCH COUNT LF_COUNT
                                                  MATCH OFFSET LF_OFFSET.
          IF LF_COUNT GT 1.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
*         Find Grouping after Decimal?
          CLEAR LF_COUNT.
          FIND ALL OCCURRENCES OF ',' IN LF_INPUT+LF_OFFSET MATCH COUNT LF_COUNT.
          IF LF_COUNT GT 0.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.
*         Remove Grouping Char
          REPLACE ALL OCCURRENCES OF ',' IN LF_INPUT WITH ''.
*         Remove Minus Char and mark minus flag
          IF LF_INPUT(1) EQ '-'.
            LF_MINUS = 'X'.
            LF_INPUT = LF_INPUT+1.
          ENDIF.
*         Find Minus Sign
          CLEAR LF_COUNT.
          FIND ALL OCCURRENCES OF '-' IN LF_INPUT MATCH COUNT LF_COUNT.
          IF LF_COUNT GT 0.
            EF_INVALID = 'X'.
            RETURN.
          ENDIF.

        WHEN OTHERS.
          EF_INVALID = 'X'.
          RETURN.
      ENDCASE.

*     Assign Result
      EF_OUTPUT = LF_INPUT.
      IF LF_MINUS = 'X'.
        EF_OUTPUT = EF_OUTPUT * -1.
      ENDIF.

    CATCH CX_ROOT.
      EF_INVALID = 'X'.
      RETURN.

  ENDTRY.

ENDMETHOD.


METHOD VALIDATE_PROFIT_CENTER.

  DATA:
    LS_CEPC  TYPE  TS_CEPC.

  DATA:
    LF_BUKRS TYPE  TS_CEPC-BUKRS,
    LF_PRCTR TYPE  TS_CEPC-PRCTR.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 10.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = IF_INPUT
    IMPORTING
      OUTPUT = LF_PRCTR.

  IF IF_BUKRS IS SUPPLIED.
    LF_BUKRS = IF_BUKRS.
  ENDIF.

  IF LF_BUKRS IS INITIAL.
    READ TABLE GT_CEPC INTO LS_CEPC
                       WITH KEY PRCTR = LF_PRCTR
                                BUKRS = LF_BUKRS
                                KOKRS = IF_KOKRS
                       BINARY SEARCH.
  ELSE.
    READ TABLE GT_CEPC INTO LS_CEPC
                       WITH KEY PRCTR = LF_PRCTR
                                BUKRS = LF_BUKRS
                       BINARY SEARCH.
  ENDIF.
  IF SY-SUBRC NE 0.

    IF LF_BUKRS IS INITIAL.
      SELECT @SPACE AS BUKRS,
             KOKRS,
             PRCTR
        FROM CEPC
       WHERE KOKRS EQ @IF_KOKRS
         AND PRCTR EQ @LF_PRCTR
         AND DATAB LE @SY-DATUM
         AND DATBI GE @SY-DATUM
        INTO @LS_CEPC
          UP TO 1 ROWS.
      ENDSELECT.
    ELSE.
      SELECT B~BUKRS,
             A~KOKRS,
             A~PRCTR
        FROM CEPC AS A
               INNER JOIN TKA02 AS B
                 ON  B~KOKRS = A~KOKRS
                 AND B~GSBER = @SPACE
       WHERE A~PRCTR EQ @LF_PRCTR
         AND B~BUKRS EQ @LF_BUKRS
         AND A~DATAB LE @SY-DATUM
         AND A~DATBI GE @SY-DATUM
        INTO @LS_CEPC
          UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_CEPC INTO TABLE GT_CEPC.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_CEPC-PRCTR.

ENDMETHOD.


METHOD VALIDATE_PROJECT_TYPE.

  DATA:
    LS_TCJ1  TYPE  TS_TCJ1.

  DATA:
    LF_PRART  TYPE  TCJ1-PRART.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 2.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  LF_PRART  = IF_INPUT.

  READ TABLE GT_TCJ1 INTO LS_TCJ1
                      WITH KEY PRART = LF_PRART
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    SELECT SINGLE PRART
      FROM TCJ1
     WHERE PRART EQ @LF_PRART
      INTO @LS_TCJ1.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_TCJ1 INTO TABLE GT_TCJ1.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_TCJ1-PRART.

ENDMETHOD.


METHOD VALIDATE_PROJ_PROFILE.

  DATA:
    LS_TCJ41  TYPE  TS_TCJ41.

  DATA:
    LF_PROFL  TYPE  PROJ-PROFL.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 7.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  LF_PROFL  = IF_INPUT.

  READ TABLE GT_TCJ41 INTO LS_TCJ41
                      WITH KEY PROFL = LF_PROFL
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    SELECT SINGLE PROFIDPROJ
      FROM TCJ41
     WHERE PROFIDPROJ EQ @LF_PROFL
      INTO @LS_TCJ41.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_TCJ41 INTO TABLE GT_TCJ41.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_TCJ41-PROFL.

ENDMETHOD.


METHOD VALIDATE_PROJ_RESPPERS.

  DATA:
    LS_TCJ04  TYPE  TS_TCJ04.

  DATA:
    LF_VERNR  TYPE  PROJ-VERNR.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

  IF STRLEN( IF_INPUT ) GT 8 OR
     NOT IF_INPUT CO '1234567890'.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

  LF_VERNR  = IF_INPUT.

  READ TABLE GT_TCJ04 INTO LS_TCJ04
                      WITH KEY VERNR = LF_VERNR
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    SELECT SINGLE VERNR
      FROM TCJ04
     WHERE VERNR EQ @LF_VERNR
      INTO @LS_TCJ04.
    IF SY-SUBRC NE 0.
      EF_INVALID = 'X'.
      RETURN.
    ENDIF.
    INSERT LS_TCJ04 INTO TABLE GT_TCJ04.
  ENDIF.

* Assign Output
  EF_OUTPUT = LS_TCJ04-VERNR.

ENDMETHOD.


METHOD VALIDATE_WBSELEM.

  DATA:
    LF_POSID TYPE  PRPS-POSID.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  IF STRLEN( IF_INPUT ) GT 24.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

* Convert to Input format
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING
      INPUT  = IF_INPUT
    IMPORTING
      OUTPUT = LF_POSID.

* Read existing
  SELECT SINGLE POSID,
                PSPHI,
                OBJNR
    FROM PRPS
   WHERE POSID EQ @LF_POSID
    INTO @DATA(LS_PRPS).
  IF SY-SUBRC NE 0.
    CLEAR LS_PRPS.
  ENDIF.

* Change with Existing --> Ok
  IF IF_CREATE IS INITIAL AND
     LS_PRPS-POSID IS NOT INITIAL.
    EF_OUTPUT = LF_POSID.
* Create without existing --> Ok
  ELSEIF IF_CREATE EQ 'X' AND
         LS_PRPS-POSID IS INITIAL.
    EF_OUTPUT = LF_POSID.
  ELSE.
    EF_OUTPUT = LS_PRPS-POSID.
    EF_INVALID = 'X'.
  ENDIF.

* Return Project
  IF EF_PSPID IS SUPPLIED.
    WRITE LS_PRPS-PSPHI TO EF_PSPID.
  ENDIF.

* Return Object
  IF EF_OBJNR IS SUPPLIED.
    EF_OBJNR = LS_PRPS-OBJNR.
  ENDIF.

ENDMETHOD.
ENDCLASS.
