*&---------------------------------------------------------------------*
*& Report ZSDSSDR0710
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0710.
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBRK.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF GS_RESULT ##NEEDED,
        VBELN TYPE VBRP-VBELN,
      END OF GS_RESULT,
      GT_RESULT LIKE TABLE OF GS_RESULT WITH EMPTY KEY.

DATA: BEGIN OF GS_POST ##NEEDED,
        VBELN     TYPE VBRP-VBELN,
        POSNR     TYPE VBRP-POSNR,
        AUBEL     TYPE VBRP-AUBEL,
        AUPOS     TYPE VBRP-AUPOS,
        NETWR     TYPE VBRP-NETWR,
        FKDAT_ANA TYPE VBRP-FKDAT_ANA,
        KUNRG_ANA TYPE VBRP-KUNRG_ANA,
      END OF GS_POST,
      GT_POST LIKE TABLE OF GS_POST WITH EMPTY KEY ##NEEDED,

      BEGIN OF GS_BSID ##NEEDED,
        BUKRS TYPE BSEG-BUKRS,
        GJAHR TYPE BSEG-GJAHR,
        BELNR TYPE BSEG-BELNR,
        UMSKZ TYPE BSEG-UMSKZ,
        AUGBL TYPE BSEG-AUGBL,
      END OF GS_BSID,
      GT_BSID LIKE TABLE OF GS_BSID ##NEEDED,

      BEGIN OF GS_VBRK ##NEEDED,
        VBELN TYPE VBRK-VBELN,
        NETWR TYPE VBRK-NETWR,
        MWSBK TYPE VBRK-MWSBK,
        BELNR TYPE VBRK-BELNR,
        GJAHR TYPE VBRK-GJAHR,
        FKDAT TYPE VBRK-FKDAT,
        BUKRS TYPE VBRK-BUKRS,
        WAERK TYPE VBRK-WAERK,
        KUNRG TYPE VBRK-KUNRG,
      END OF GS_VBRK,

      BEGIN OF GS_ZSDSSDT010 ##NEEDED,
        BUKRS TYPE ZSDSSDT010-BUKRS,
        GJAHR TYPE ZSDSSDT010-GJAHR,
        BELNR TYPE ZSDSSDT010-BELNR,
        VBELN TYPE ZSDSSDT010-VBELN,
        POSNR TYPE ZSDSSDT010-POSNR,
        WAERS TYPE ZSDSSDT010-WAERS,
        DMBTR TYPE ZSDSSDT010-DMBTR,
      END OF GS_ZSDSSDT010,
      GT_ZSDSSDT010 LIKE TABLE OF GS_ZSDSSDT010 WITH EMPTY KEY ##NEEDED,

      BEGIN OF GS_ZSDSSDT015 ##NEEDED,
        VBELN TYPE ZSDSSDT015-VBELN,
        POSNR TYPE ZSDSSDT015-POSNR,
        BUKRS TYPE ZSDSSDT015-BUKRS,
        BELNR TYPE ZSDSSDT015-BELNR,
        GJAHR TYPE ZSDSSDT015-GJAHR,
        DMBTR TYPE ZSDSSDT015-DMBTR,
      END OF GS_ZSDSSDT015,
      GT_ZSDSSDT015 LIKE TABLE OF GS_ZSDSSDT015 ##NEEDED,

      BEGIN OF GS_POST_DOC ##NEEDED,
        BUKRS TYPE VBRK-BUKRS,
        BELNR TYPE ZSDSSDT010-BELNR,
        GJAHR TYPE ZSDSSDT010-GJAHR,
        VBELV TYPE VBFA-VBELV,
        POSNV TYPE VBFA-POSNV,
        VBELN TYPE VBFA-VBELN,
        POSNN TYPE VBFA-POSNN,
        KUNRG TYPE VBRK-KUNRG,
        FKDAT TYPE VBRK-FKDAT,
        WAERK TYPE VBRK-WAERK,
        DMBTR TYPE ZSDSSDT010-DMBTR,
        WAERS TYPE ZSDSSDT010-WAERS,
      END OF GS_POST_DOC,
      GT_POST_DOC LIKE TABLE OF GS_POST_DOC ##NEEDED,

      GT_REVERSAL TYPE TABLE OF ZSDSSDT015 ##NEEDED,

      BEGIN OF GS_CLEARING_DOC ##NEEDED,
        BELNR TYPE ZSDSSDT015-BELNR,
        GJAHR TYPE ZSDSSDT015-GJAHR,
      END OF GS_CLEARING_DOC,
      GT_CLEARING_DOC LIKE TABLE OF GS_CLEARING_DOC ##NEEDED.

DATA: GT_FTPOST TYPE TABLE OF FTPOST ##NEEDED,
      GT_FTTAX  TYPE TABLE OF FTTAX ##NEEDED,
      GT_BLNTAB TYPE TABLE OF BLNTAB ##NEEDED,
      GS_FTPOST TYPE FTPOST ##NEEDED,
      GF_COUNT  TYPE I ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA : P_VBELN TYPE VBRK-VBELN.
*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  PARAMETERS P_DATE TYPE SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS: S_VBELN FOR VBRK-VBELN OBLIGATORY.
  PARAMETERS: R_POST RADIOBUTTON GROUP G1 DEFAULT 'X',
              R_RES  RADIOBUTTON GROUP G1 ##NEEDED.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_DATA.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_RESULT IS NOT INITIAL.
    LOOP AT GT_RESULT INTO GS_RESULT.
      PERFORM F_CLEAR_DATA.
      P_VBELN = GS_RESULT-VBELN.
      PERFORM GET_DATA.
      IF GT_REVERSAL[] IS NOT INITIAL OR GT_ZSDSSDT010[] IS NOT INITIAL.
        IF R_POST IS NOT INITIAL.
          IF GT_BSID[] IS NOT INITIAL.
            PERFORM POST_DOC.
          ELSE.
            MESSAGE TEXT-003 TYPE 'E'.
          ENDIF.
        ELSE.
          PERFORM REVERSAL_DOC.
        ENDIF.
      ELSE.
        MESSAGE TEXT-002 TYPE 'E'.
      ENDIF.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
*  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form POST_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM POST_DATA USING LV_VBELN.

  DATA: LF_JOBNAME  TYPE TBTCJOB-JOBNAME,
        LF_JOBCOUNT TYPE TBTCJOB-JOBCOUNT.


  CONCATENATE 'ZSDE017'
               SY-DATUM
               SY-UZEIT
         INTO  LF_JOBNAME.

*     open job
  CALL FUNCTION 'JOB_OPEN' ##FM_SUBRC_OK
    EXPORTING
      JOBNAME          = LF_JOBNAME
    IMPORTING
      JOBCOUNT         = LF_JOBCOUNT
    EXCEPTIONS
      CANT_CREATE_JOB  = 1
      INVALID_JOB_DATA = 2
      JOBNAME_MISSING  = 3
      OTHERS           = 4.

  SUBMIT ZSDSSDR0150                                     "#EC CI_SUBMIT
*        WITH S_VBELN IN LRT_VBELN
    WITH P_VBELN = LV_VBELN
    WITH R_POST  = R_POST
    WITH R_RES   = R_RES
    VIA JOB LF_JOBNAME NUMBER LF_JOBCOUNT
    USER SY-UNAME
    AND RETURN .

*   Schedule and close job.
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      JOBCOUNT  = LF_JOBCOUNT
      JOBNAME   = LF_JOBNAME
      STRTIMMED = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA .
  SELECT VBELN
    FROM VBRK
    WHERE VBELN IN @S_VBELN[]
    INTO TABLE @Gt_RESULT.
ENDFORM.
*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .
  IF R_POST IS NOT INITIAL.
    DO 3 TIMES.
      SELECT SINGLE VBELN
                    NETWR
                    MWSBK
                    BELNR
                    GJAHR
                    FKDAT
                    BUKRS
                    WAERK
                    KUNRG
        FROM VBRK
        INTO GS_VBRK
        WHERE VBELN = P_VBELN.

      IF SY-SUBRC = 0.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

    DO 3 TIMES.
      SELECT VBELN,
             POSNR,
             AUBEL,
             AUPOS,
             NETWR + MWSBP AS NETWR,
             FKDAT_ANA,
             KUNRG_ANA
        FROM VBRP
        INTO TABLE @GT_POST
        WHERE VBELN = @P_VBELN.

      IF SY-SUBRC = 0.
        SELECT A~BUKRS,
               A~GJAHR,
               A~BELNR,
               A~VBELN,
               A~POSNR,
               A~WAERS,
               SUM( A~DMBTR ) AS DMBTR
          FROM ZSDSSDT010 AS A
          INNER JOIN @GT_POST AS B
          ON  A~VBELN = B~AUBEL
          AND A~POSNR = B~AUPOS
          GROUP BY A~VBELN,  A~POSNR, A~BUKRS, A~BELNR, A~GJAHR, A~WAERS
          ORDER BY A~BUKRS, A~BELNR, A~GJAHR, A~VBELN,  A~POSNR, A~WAERS
*          ORDER BY A~VBELN,  A~POSNR, A~BUKRS, A~BELNR, A~GJAHR, A~WAERS
          INTO TABLE @GT_ZSDSSDT010.

        IF GT_ZSDSSDT010[] IS NOT INITIAL.

          SELECT A~BUKRS,
                 A~GJAHR,
                 A~BELNR,
                 A~BUZEI,
                 A~AUGDT,
                 A~AUGBL,
                 A~UMSKZ
            FROM BSEG AS A                              "#EC CI_NOORDER
            INNER JOIN @GT_ZSDSSDT010 AS B
            ON  A~BUKRS = B~BUKRS
            AND A~GJAHR = B~GJAHR
            AND A~BELNR = B~BELNR
            WHERE A~KOART = 'D'
              AND A~BSCHL = '19'
            INTO TABLE @DATA(LT_BSEG).

          IF SY-SUBRC = 0.
            SELECT A~BUKRS,
                   A~GJAHR,
                   A~BELNR,
                   A~BUZEI,
                   A~UMSKZ,
                   A~H_BUDAT
              FROM BSEG AS A                            "#EC CI_NOORDER
              INNER JOIN @LT_BSEG AS B
              ON  A~BUKRS = B~BUKRS
              AND A~BELNR = B~AUGBL
              AND A~H_BUDAT = B~AUGDT
              WHERE A~KOART = 'D'
                AND A~BSCHL = '19'
                AND A~UMSKZ = 'S'
              INTO TABLE @DATA(LT_CLEAR).
          ENDIF.
          LOOP AT LT_BSEG ASSIGNING FIELD-SYMBOL(<LFS_BSEG>).
            APPEND INITIAL LINE TO GT_BSID ASSIGNING FIELD-SYMBOL(<LFS_BSID>).
            <LFS_BSID>-BUKRS = <LFS_BSEG>-BUKRS.
            <LFS_BSID>-GJAHR = <LFS_BSEG>-GJAHR.
            <LFS_BSID>-BELNR = <LFS_BSEG>-BELNR.
            IF <LFS_BSEG>-AUGBL IS NOT INITIAL.
              READ TABLE LT_CLEAR ASSIGNING FIELD-SYMBOL(<LFS_CLEAR>) WITH KEY BELNR   = <LFS_BSEG>-AUGBL
                                                                               H_BUDAT = <LFS_BSEG>-AUGDT.
              IF SY-SUBRC = 0.
                <LFS_BSID>-UMSKZ = <LFS_CLEAR>-UMSKZ.
                <LFS_BSID>-AUGBL = <LFS_BSEG>-AUGBL.
              ENDIF.
            ELSE.
              <LFS_BSID>-UMSKZ = <LFS_BSEG>-UMSKZ.
            ENDIF.

          ENDLOOP.

*          SELECT A~BUKRS,
*                 A~GJAHR,
*                 A~BELNR,
*                 A~UMSKZ
*            FROM BSID_VIEW AS A
*            INNER JOIN @GT_ZSDSSDT010 AS B
*            ON  A~BUKRS = B~BUKRS
*            AND A~GJAHR = B~GJAHR
*            AND A~BELNR = B~BELNR
*            GROUP BY A~BUKRS, A~GJAHR, A~BELNR, A~UMSKZ
*            INTO TABLE @GT_BSID.
        ENDIF.

        SELECT A~VBELN,
               A~POSNR,
               A~BUKRS,
               A~BELNR,
               A~GJAHR,
*               A~WAERS,
               SUM( A~DMBTR ) AS DMBTR
          FROM ZSDSSDT015 AS A
          INNER JOIN @GT_POST AS B
          ON  A~VBELN = B~AUBEL
          AND A~POSNR = B~AUPOS
          WHERE A~BELNR_C = ''
          GROUP BY A~VBELN,  A~POSNR, A~BUKRS, A~BELNR, A~GJAHR, A~WAERS
          ORDER BY A~VBELN,  A~POSNR, A~BUKRS, A~BELNR, A~GJAHR, A~WAERS
          INTO TABLE @GT_ZSDSSDT015.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

  ELSE.
    DO 3 TIMES.
      SELECT *                                          "#EC CI_NOFIELD
        FROM ZSDSSDT015
        INTO TABLE GT_REVERSAL
        WHERE VBELN_B = P_VBELN
          AND BELNR_C = ''.

      IF SY-SUBRC = 0.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.


  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REVERSAL_DOC
*&---------------------------------------------------------------------*
FORM REVERSAL_DOC .
  DATA: LT_BLNTAB   TYPE TABLE OF BLNTAB,
        LT_REVERSAL TYPE TABLE OF ZSDSSDT015.

  DATA: LF_MSGID    TYPE SY-MSGID,
        LF_MSGNO    TYPE SY-MSGNO,
        LF_MSGTY    TYPE SY-MSGTY,
        LF_MSGV1    TYPE SY-MSGV1,
        LF_MSGV2    TYPE SY-MSGV2,
        LF_MSGV3    TYPE SY-MSGV3,
        LF_MSGV4    TYPE SY-MSGV4,
        LF_SUBRC    TYPE SY-SUBRC ##NEEDED,
        LF_MSG(200).

  LT_REVERSAL[] = GT_REVERSAL.
  SORT LT_REVERSAL BY BELNR_S GJAHR_S.
  DELETE ADJACENT DUPLICATES FROM LT_REVERSAL COMPARING BELNR_S GJAHR_S.

  LOOP AT LT_REVERSAL ASSIGNING FIELD-SYMBOL(<L_REVERSAL>).
    "INTO DATA(LS_REVERSAL) ##INTO_OK.
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        I_FUNCTION         = 'C'
      EXCEPTIONS
        CLIENT_INCORRECT   = 1
        FUNCTION_INVALID   = 2
        GROUP_NAME_MISSING = 3
        MODE_INVALID       = 4
        UPDATE_INVALID     = 5
        USER_INVALID       = 6
        OTHERS             = 7.
    IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'POSTING_INTERFACE_REVERSE_DOC'
      EXPORTING
        I_BELNS                  = <L_REVERSAL>-BELNR_S
        I_BUKRS                  = <L_REVERSAL>-BUKRS
        I_GJAHS                  = <L_REVERSAL>-GJAHR_S
        I_TCODE                  = 'FB08'
        I_STGRD                  = '01'
      IMPORTING
        E_MSGID                  = LF_MSGID
        E_MSGNO                  = LF_MSGNO
        E_MSGTY                  = LF_MSGTY
        E_MSGV1                  = LF_MSGV1
        E_MSGV2                  = LF_MSGV2
        E_MSGV3                  = LF_MSGV3
        E_MSGV4                  = LF_MSGV4
        E_SUBRC                  = LF_SUBRC
      TABLES
        T_BLNTAB                 = LT_BLNTAB
      EXCEPTIONS
        TRANSACTION_CODE_INVALID = 1
        NO_AUTHORIZATION         = 2
        OTHERS                   = 3.
    IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXCEPTIONS
        SESSION_NOT_PROCESSABLE = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.

    IF LF_MSGNO = '312' AND LF_MSGNO = '312' AND LF_MSGTY = 'S' ##BOOL_OK.
      WRITE / |Doc. Special: { <L_REVERSAL>-BELNR_S }/Doc. Reverse: { LF_MSGV1 } | ##NO_TEXT. "#EC CI_NOORDER
      LOOP AT GT_REVERSAL ASSIGNING FIELD-SYMBOL(<L_REVERSAL_LOG>) WHERE BELNR_S = <L_REVERSAL>-BELNR_S
                                                                     AND GJAHR_S = <L_REVERSAL>-GJAHR_S.
        <L_REVERSAL_LOG>-BELNR_C = LF_MSGV1.
        <L_REVERSAL_LOG>-GJAHR_C = SY-DATUM(4).
      ENDLOOP.

    ELSEIF LF_MSGTY = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = LF_MSGID
          MSGNR               = LF_MSGNO
          MSGV1               = LF_MSGV1
          MSGV2               = LF_MSGV2
          MSGV3               = LF_MSGV3
          MSGV4               = LF_MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LF_MSG.
      WRITE / |Doc. Special: { <L_REVERSAL>-BELNR_S } Error: { LF_MSG } | ##NO_TEXT. "#EC CI_NOORDER
    ENDIF.

    CLEAR: LT_BLNTAB[], LF_MSGID, LF_MSGNO, LF_MSGTY, LF_MSGV1,
           LF_MSGV2, LF_MSGV3, LF_MSGV4, LF_SUBRC, LF_MSG.
  ENDLOOP.

  IF GT_REVERSAL[] IS NOT INITIAL.
    MODIFY ZSDSSDT015 FROM TABLE GT_REVERSAL[].
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_DOC
*&---------------------------------------------------------------------*
FORM POST_DOC .
  DATA: LF_MODE      TYPE  RFPDO-ALLGAZMD,
        LF_DATE(10),
        LF_WRBTR     TYPE BSEG-WRBTR,
        LF_ZUONR(50),
        LF_TOTAL     TYPE BSEG-WRBTR.
  "LF_MSG(200).

  DATA: LF_SUM TYPE ZSDSSDT015-DMBTR.

  DATA: LF_SUBRC      TYPE SY-SUBRC ##NEEDED,
        LF_MSGID      TYPE SY-MSGID,
        LF_MSGNO      TYPE SY-MSGNO,
        LF_MSGTY      TYPE SY-MSGTY ##NEEDED,
        LF_MSGV1      TYPE SY-MSGV1,
        LF_MSGV2      TYPE SY-MSGV2,
        LF_MSGV3      TYPE SY-MSGV3,
        LF_MSGV4      TYPE SY-MSGV4,
        LF_LINEP(110).

  DATA: LT_ZSDSSDT015 TYPE TABLE OF ZSDSSDT015.

  LF_MODE = 'N'.

  LOOP AT GT_ZSDSSDT010 ASSIGNING FIELD-SYMBOL(<L_ZSDSSDT010>).
    GS_ZSDSSDT010 = <L_ZSDSSDT010>.

    READ TABLE GT_POST ASSIGNING FIELD-SYMBOL(<L_POST>) WITH KEY AUBEL = GS_ZSDSSDT010-VBELN
                                                                 AUPOS = GS_ZSDSSDT010-POSNR.
    IF SY-SUBRC = 0.
      READ TABLE GT_ZSDSSDT015 ASSIGNING FIELD-SYMBOL(<L_ZSDSSDT015>) WITH KEY VBELN = GS_ZSDSSDT010-VBELN"VBELN = <L_POST>-AUBEL
                                                                               POSNR = GS_ZSDSSDT010-POSNR"POSNR = <L_POST>-AUPOS
                                                                               BUKRS = GS_ZSDSSDT010-BUKRS
                                                                               BELNR = GS_ZSDSSDT010-BELNR
                                                                               GJAHR = GS_ZSDSSDT010-GJAHR.
      IF SY-SUBRC = 0.
        LF_WRBTR = <L_ZSDSSDT010>-DMBTR - <L_ZSDSSDT015>-DMBTR.
      ELSE.
        LF_WRBTR = <L_ZSDSSDT010>-DMBTR.
      ENDIF.

*      CLEAR GS_ZSDSSDT015.
*      GS_ZSDSSDT015 = GT_ZSDSSDT015[ VBELN = <L_POST>-AUBEL
*                                     POSNR = <L_POST>-AUPOS ].
*
*      LF_WRBTR = <L_ZSDSSDT010>-DMBTR - GS_ZSDSSDT015-DMBTR.
      IF LF_WRBTR > <L_POST>-NETWR.
        LF_WRBTR = <L_POST>-NETWR.
        CLEAR: <L_POST>-NETWR.
      ELSE.
        <L_POST>-NETWR = <L_POST>-NETWR - LF_WRBTR.
      ENDIF.
      LF_TOTAL = LF_TOTAL + LF_WRBTR.
      IF LF_WRBTR IS NOT INITIAL.
        APPEND VALUE #( VBELN   = GS_ZSDSSDT010-VBELN
                        POSNR   = GS_ZSDSSDT010-POSNR
                        BUKRS   = GS_ZSDSSDT010-BUKRS
                        BELNR   = GS_ZSDSSDT010-BELNR
                        GJAHR   = GS_ZSDSSDT010-GJAHR
                        BELNR_S = ''
                        GJAHR_S = ''
                        DMBTR   = LF_WRBTR
                        WAERS   = GS_ZSDSSDT010-WAERS
                        VBELN_B = P_VBELN
                        BELNR_C = ''
                        GJAHR_C = ''
                        AENAM   = SY-UNAME
                        AEDAT   = SY-DATUM
                        AEZET   = SY-UZEIT )
                  TO LT_ZSDSSDT015.
      ENDIF.

    ENDIF.



    AT END OF BELNR.
      IF LF_TOTAL IS NOT INITIAL.
        "Header
        GS_FTPOST-STYPE = 'K'.
        GS_FTPOST-COUNT =  1.
        WRITE P_DATE TO LF_DATE.
        PERFORM ADD_FIELD USING 'BKPF-BLDAT' LF_DATE. "Document Date
        PERFORM ADD_FIELD USING 'BKPF-BLART' 'DG'.
        PERFORM ADD_FIELD USING 'BKPF-BUKRS' GS_ZSDSSDT010-BUKRS.
        PERFORM ADD_FIELD USING 'BKPF-BUDAT' LF_DATE. "Posting Date
        PERFORM ADD_FIELD USING 'BKPF-WAERS' GS_ZSDSSDT010-WAERS.
        PERFORM ADD_FIELD USING 'BKPF-XBLNR' GS_ZSDSSDT010-VBELN. "Reference
        PERFORM ADD_FIELD USING 'BKPF-BKTXT' P_VBELN. "Doc.Header Text
        PERFORM ADD_FIELD USING 'BKPF-BRNCH' '0000'.

        "Item
        GS_FTPOST-STYPE = 'P'.
        GS_FTPOST-COUNT =  1.
        PERFORM ADD_FIELD USING 'RF05A-NEWBS' '09'.
        PERFORM ADD_FIELD USING 'RF05A-NEWKO' <L_POST>-KUNRG_ANA.
        READ TABLE GT_BSID INTO DATA(LS_BSID) WITH KEY GJAHR = GS_ZSDSSDT010-GJAHR
                                                       BELNR = GS_ZSDSSDT010-BELNR.
        IF SY-SUBRC = 0.
          PERFORM ADD_FIELD USING 'RF05A-NEWUM' LS_BSID-UMSKZ.
          IF LS_BSID-AUGBL IS NOT INITIAL.
            LF_ZUONR = LS_BSID-AUGBL.
          ELSE.
            LF_ZUONR = GS_ZSDSSDT010-BELNR.
          ENDIF.
        ELSE.
          CLEAR: LS_BSID.
        ENDIF.

        PERFORM ADD_FIELD USING 'BSEG-WRBTR' LF_TOTAL.

        IF LS_BSID-UMSKZ = 'S'.
          PERFORM ADD_FIELD USING 'BSEG-MWSKZ' 'O7'.
          PERFORM ADD_FIELD USING 'RF05A-XMWST' 'X'.
        ELSE.
          PERFORM ADD_FIELD USING 'BSEG-MWSKZ' 'OX'.
        ENDIF.

        PERFORM ADD_FIELD USING 'BSEG-ZFBDT' LF_DATE.
        PERFORM ADD_FIELD USING 'BSEG-ZUONR' LF_ZUONR.
*        PERFORM ADD_FIELD USING 'BSEG-ZUONR' GS_ZSDSSDT010-BELNR.

        GS_FTPOST-COUNT =  2.
        PERFORM ADD_FIELD USING 'RF05A-NEWBS' '11'.
        PERFORM ADD_FIELD USING 'RF05A-NEWKO' <L_POST>-KUNRG_ANA.
        PERFORM ADD_FIELD USING 'BSEG-WRBTR' LF_TOTAL.

        IF LS_BSID-UMSKZ = 'S'.
          PERFORM ADD_FIELD USING 'BSEG-MWSKZ' 'O7'.
        ELSE.
          PERFORM ADD_FIELD USING 'BSEG-MWSKZ' 'OX'.
        ENDIF.

        PERFORM ADD_FIELD USING 'BSEG-ZFBDT' LF_DATE.
        PERFORM ADD_FIELD USING 'BSEG-ZUONR' LF_ZUONR.
*        PERFORM ADD_FIELD USING 'BSEG-ZUONR' GS_ZSDSSDT010-BELNR.
*        PERFORM ADD_FIELD USING '' ''.

        CALL FUNCTION 'POSTING_INTERFACE_START' ##FM_SUBRC_OK
          EXPORTING
            I_FUNCTION         = 'C'
            I_MODE             = LF_MODE
          EXCEPTIONS
            CLIENT_INCORRECT   = 1
            FUNCTION_INVALID   = 2
            GROUP_NAME_MISSING = 3
            MODE_INVALID       = 4
            UPDATE_INVALID     = 5
            USER_INVALID       = 6
            OTHERS             = 7.
        IF SY-SUBRC <> 0.
          WRITE / |Doc. Receive: { GS_ZSDSSDT010-BELNR } Error: Cannot post | ##NO_TEXT. "#EC CI_NOORDER
          LOOP AT LT_ZSDSSDT015 INTO DATA(LS_ZSDSSDT015_LOG) WHERE VBELN   = GS_ZSDSSDT010-VBELN ##NEEDED ##INTO_OK
                                                               AND BUKRS   = GS_ZSDSSDT010-BUKRS
                                                               AND BELNR   = GS_ZSDSSDT010-BELNR
                                                               AND GJAHR   = GS_ZSDSSDT010-GJAHR.
            DELETE LT_ZSDSSDT015.

          ENDLOOP.
        ELSE.
          CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT' ##FM_SUBRC_OK
            EXPORTING
              I_TCODE                  = 'FB01'
              I_XSIMU                  = ABAP_FALSE
*             I_SGFUNCT                = ' '
*             I_NO_AUTH                = ' '
            IMPORTING
              E_SUBRC                  = LF_SUBRC
              E_MSGID                  = LF_MSGID
              E_MSGTY                  = LF_MSGTY
              E_MSGNO                  = LF_MSGNO
              E_MSGV1                  = LF_MSGV1
              E_MSGV2                  = LF_MSGV2
              E_MSGV3                  = LF_MSGV3
              E_MSGV4                  = LF_MSGV4
            TABLES
              T_BLNTAB                 = GT_BLNTAB
              T_FTPOST                 = GT_FTPOST
              T_FTTAX                  = GT_FTTAX
            EXCEPTIONS
              ACCOUNT_MISSING          = 1
              COMPANY_CODE_MISSING     = 2
              POSTING_KEY_INVALID      = 3
              POSTING_KEY_MISSING      = 4
              RECORD_TYPE_INVALID      = 5
              TRANSACTION_CODE_INVALID = 6
              AMOUNT_FORMAT_ERROR      = 7
              TOO_MANY_LINE_ITEMS      = 8
              COMPANY_CODE_INVALID     = 9
              SCREEN_NOT_FOUND         = 10
              NO_AUTHORIZATION         = 11
              OTHERS                   = 12.

          READ TABLE GT_BLNTAB INTO DATA(LS_BLNTAB) INDEX 1.
          IF SY-SUBRC = 0.
            WRITE / |Doc. Receive: { GS_ZSDSSDT010-BELNR }/Doc. Special: { LS_BLNTAB-BELNR }/{ LS_BLNTAB-GJAHR } | ##NO_TEXT. "#EC CI_NOORDER
            LOOP AT LT_ZSDSSDT015 ASSIGNING FIELD-SYMBOL(<L_ZSDSSDT015_LOG>) WHERE VBELN   = GS_ZSDSSDT010-VBELN
                                                                               AND BUKRS   = GS_ZSDSSDT010-BUKRS
                                                                               AND BELNR   = GS_ZSDSSDT010-BELNR
                                                                               AND GJAHR   = GS_ZSDSSDT010-GJAHR.
              <L_ZSDSSDT015_LOG>-BELNR_S = LS_BLNTAB-BELNR.
              <L_ZSDSSDT015_LOG>-GJAHR_S = LS_BLNTAB-GJAHR.

              LF_SUM = LF_SUM + <L_ZSDSSDT015_LOG>-DMBTR.
            ENDLOOP.
            GS_CLEARING_DOC-BELNR = LS_BLNTAB-BELNR."<L_ZSDSSDT015_LOG>-BELNR.
            GS_CLEARING_DOC-GJAHR = LS_BLNTAB-GJAHR."<L_ZSDSSDT015_LOG>-GJAHR.
            COLLECT GS_CLEARING_DOC INTO GT_CLEARING_DOC.
          ELSE.
            CALL FUNCTION 'FORMAT_MESSAGE'
              EXPORTING
                ID        = LF_MSGID
                LANG      = SY-LANGU
                NO        = LF_MSGNO
                V1        = LF_MSGV1
                V2        = LF_MSGV2
                V3        = LF_MSGV3
                V4        = LF_MSGV4
              IMPORTING
                MSG       = LF_LINEP
              EXCEPTIONS ##FM_SUBRC_OK
                NOT_FOUND = 1
                OTHERS    = 2.
            IF LF_LINEP IS INITIAL.
              LF_LINEP = 'Process Error' ##NO_TEXT.
            ENDIF.
            WRITE / |Doc. Receive: { GS_ZSDSSDT010-BELNR } Error: { LF_LINEP } | ##NO_TEXT. "#EC CI_NOORDER
            LOOP AT LT_ZSDSSDT015 INTO LS_ZSDSSDT015_LOG WHERE VBELN   = GS_ZSDSSDT010-VBELN ##NEEDED ##INTO_OK
                                                           AND BUKRS   = GS_ZSDSSDT010-BUKRS
                                                           AND BELNR   = GS_ZSDSSDT010-BELNR
                                                           AND GJAHR   = GS_ZSDSSDT010-GJAHR.
              DELETE LT_ZSDSSDT015.

            ENDLOOP.
          ENDIF.

          CALL FUNCTION 'POSTING_INTERFACE_END' ##FM_SUBRC_OK
            EXCEPTIONS
              SESSION_NOT_PROCESSABLE = 1
              OTHERS                  = 2.

        ENDIF.

*        WRITE <L_POST>-FKDAT_ANA TO LF_DATE.
*
*        PERFORM DYNPRO USING: 'X' 'SAPMF05A' '0100',
*                              ' ' 'BKPF-BLDAT' LF_DATE,
*                              ' ' 'BKPF-BLART' 'DG',
*                              ' ' 'BKPF-BUKRS' GS_ZSDSSDT010-BUKRS,
*                              ' ' 'BKPF-BUDAT' LF_DATE,
*                              ' ' 'BKPF-WAERS' GS_ZSDSSDT010-WAERS,
*                              ' ' 'BKPF-XBLNR' GS_ZSDSSDT010-VBELN,
*                              ' ' 'BKPF-BKTXT' P_VBELN,
*                              ' ' 'BKPF-BRNCH' '0000',
*                              ' ' 'RF05A-NEWBS' '09',
*                              ' ' 'RF05A-NEWKO' <L_POST>-KUNRG_ANA,
*                              ' ' 'RF05A-NEWUM' 'N',
*                              ' ' 'BDC_OKCODE' '/00'.
*
*        PERFORM DYNPRO USING: 'X' 'SAPMF05A' '0304',
*                              ' ' 'BSEG-WRBTR' LF_TOTAL,
*                              ' ' 'BSEG-MWSKZ' 'OX',
*                              ' ' 'BSEG-ZFBDT' LF_DATE,
*                              ' ' 'BSEG-ZUONR' GS_ZSDSSDT010-BELNR,
*                              ' ' 'RF05A-NEWBS' '11',
*                              ' ' 'RF05A-NEWKO' <L_POST>-KUNRG_ANA,
*                              ' ' 'BDC_OKCODE' '/00'.
*
*        PERFORM DYNPRO USING: 'X' 'SAPLFWTD' '0100',
*                              ' ' 'BDC_OKCODE' '=GO'.
*
*        PERFORM DYNPRO USING: 'X' 'SAPMF05A' '0301',
*                              ' ' 'BSEG-WRBTR' LF_TOTAL,
*                               ' ' 'BSEG-MWSKZ' 'OX',
*                               ' ' 'BSEG-ZFBDT' LF_DATE,
*                              ' ' 'BSEG-ZUONR' GS_ZSDSSDT010-BELNR,
*                              ' ' 'BDC_OKCODE' '=BS'.
*
*        PERFORM DYNPRO USING: 'X' 'SAPLFWTD' '0100',
*                              ' ' 'BDC_OKCODE' '=GO'.
*
*        PERFORM DYNPRO USING: 'X' 'SAPMF05A' '0700',
*                              ' ' 'BDC_OKCODE' '=BU'.
*
*        PERFORM DYNPRO USING: 'X' 'SAPLFWTD' '0100',
*                              ' ' 'BDC_OKCODE' '=GO'.
*
*        CALL TRANSACTION 'F-02' USING GT_BDCDATA         "#EC CI_CALLTA
*                                MODE LF_MODE
*                                UPDATE 'S'
*                                MESSAGES INTO GT_MESSTAB.
*
*        READ TABLE GT_MESSTAB ASSIGNING FIELD-SYMBOL(<L_MESSTAB>) WITH KEY MSGTYP = 'S'
*                                                                           MSGID  = 'F5'
*                                                                           MSGNR  = '312'.
*        IF SY-SUBRC = 0.
*          WRITE / |Doc. Receive: { GS_ZSDSSDT010-BELNR }/Doc. Special: { <L_MESSTAB>-MSGV1 } | ##NO_TEXT. "#EC CI_NOORDER
*
*          LOOP AT LT_ZSDSSDT015 ASSIGNING FIELD-SYMBOL(<L_ZSDSSDT015_LOG>) WHERE VBELN   = GS_ZSDSSDT010-VBELN
*                                                                             AND BUKRS   = GS_ZSDSSDT010-BUKRS
*                                                                             AND BELNR   = GS_ZSDSSDT010-BELNR
*                                                                             AND GJAHR   = GS_ZSDSSDT010-GJAHR.
*            <L_ZSDSSDT015_LOG>-BELNR_S = <L_MESSTAB>-MSGV1.
*            <L_ZSDSSDT015_LOG>-GJAHR_S = <L_POST>-FKDAT_ANA(4).
*
*          ENDLOOP.
*
*        ELSE.
*          READ TABLE GT_MESSTAB ASSIGNING <L_MESSTAB> INDEX 1."WITH KEY MSGTYP = 'E'.
*          IF SY-SUBRC = 0.
*            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*              EXPORTING
*                MSGID               = <L_MESSTAB>-MSGID
*                MSGNR               = <L_MESSTAB>-MSGNR
*                MSGV1               = <L_MESSTAB>-MSGV1
*                MSGV2               = <L_MESSTAB>-MSGV2
*                MSGV3               = <L_MESSTAB>-MSGV3
*                MSGV4               = <L_MESSTAB>-MSGV4
*              IMPORTING
*                MESSAGE_TEXT_OUTPUT = LF_MSG.
*
*            WRITE / |Doc. Receive: { GS_ZSDSSDT010-BELNR } Error: { LF_MSG } | ##NO_TEXT. "#EC CI_NOORDER
*          ENDIF.
*          LOOP AT LT_ZSDSSDT015 INTO DATA(LS_ZSDSSDT015_LOG) WHERE VBELN   = GS_ZSDSSDT010-VBELN
*                                                               AND BUKRS   = GS_ZSDSSDT010-BUKRS
*                                                               AND BELNR   = GS_ZSDSSDT010-BELNR
*                                                               AND GJAHR   = GS_ZSDSSDT010-GJAHR.
*            DELETE LT_ZSDSSDT015.
*
*          ENDLOOP.
*        ENDIF.

*        CLEAR: LF_DATE, LF_TOTAL, GT_BDCDATA[], GT_MESSTAB[], LF_MSG.
        CLEAR: LF_DATE, LF_TOTAL, GT_FTPOST[], GT_FTTAX[], GT_BLNTAB[], GS_FTPOST, GF_COUNT, LF_ZUONR.
      ENDIF.
    ENDAT.
    CLEAR: LF_WRBTR.
  ENDLOOP.

  IF LT_ZSDSSDT015[] IS NOT INITIAL.
    LOOP AT LT_ZSDSSDT015 INTO LS_ZSDSSDT015_LOG ##INTO_OK.
      IF LS_ZSDSSDT015_LOG-BELNR_S IS INITIAL.
        DELETE LT_ZSDSSDT015.
      ENDIF.
    ENDLOOP.
    MODIFY ZSDSSDT015 FROM TABLE LT_ZSDSSDT015[].
    COMMIT WORK AND WAIT.

    IF LF_SUM = ( GS_VBRK-NETWR + GS_VBRK-MWSBK ) AND GT_CLEARING_DOC[] IS NOT INITIAL.
      PERFORM CLEARING_DOC.
    ENDIF.
  ENDIF.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form DYNPRO
**&---------------------------------------------------------------------*
*FORM DYNPRO  USING P_PROGRAM ##PERF_NO_TYPE
*                   P_FNAM ##PERF_NO_TYPE
*                   P_FVAL ##PERF_NO_TYPE.
*  CLEAR GS_BDCDATA .
*  IF P_PROGRAM = 'X' .
*    GS_BDCDATA-PROGRAM = P_FNAM .
*    GS_BDCDATA-DYNPRO  = P_FVAL .
*    GS_BDCDATA-DYNBEGIN = P_PROGRAM.
*  ELSE.
*    GS_BDCDATA-FNAM = P_FNAM.
*    GS_BDCDATA-FVAL = P_FVAL.
*    CONDENSE GS_BDCDATA-FVAL.
*  ENDIF.
*  APPEND GS_BDCDATA TO GT_BDCDATA.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_FIELD
*&---------------------------------------------------------------------*
FORM ADD_FIELD  USING UF_FNAM ##PERF_NO_TYPE
                      UF_FVAL ##PERF_NO_TYPE.
  APPEND INITIAL LINE TO GT_FTPOST ASSIGNING FIELD-SYMBOL(<L_FTPOST>).
  <L_FTPOST>-STYPE = GS_FTPOST-STYPE.
  <L_FTPOST>-COUNT = GS_FTPOST-COUNT.
  <L_FTPOST>-FNAM = UF_FNAM.
  <L_FTPOST>-FVAL = UF_FVAL.
  CONDENSE: <L_FTPOST>-FNAM, <L_FTPOST>-FVAL.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEARING_DOC
*&---------------------------------------------------------------------*
FORM CLEARING_DOC .

  DATA: LF_MODE TYPE  RFPDO-ALLGAZMD,
        LF_SIM  TYPE CHAR1.

  DATA:
    LF_MSGID    TYPE SY-MSGID,
    LF_MSGNO    TYPE SY-MSGNO,
    LF_MSGTY    TYPE SY-MSGTY ##NEEDED,
    LF_MSGV1    TYPE SY-MSGV1,
    LF_MSGV2    TYPE SY-MSGV2,
    LF_MSGV3    TYPE SY-MSGV3,
    LF_MSGV4    TYPE SY-MSGV4,
    LF_SUBRC    TYPE SY-SUBRC ##NEEDED,
    LF_MSG(200),

    LT_BLNTAB   TYPE STANDARD TABLE OF BLNTAB,
    LT_CLEAR    TYPE STANDARD TABLE OF FTCLEAR,
    LT_POST     TYPE STANDARD TABLE OF FTPOST,
    LT_TAX      TYPE STANDARD TABLE OF FTTAX.

  LF_MODE = 'N'.

  "Header
  LT_POST = VALUE FEB_T_FTPOST( BASE LT_POST (
                    STYPE = 'K'
                    COUNT = 1:
                    FNAM = 'BKPF-BLDAT'
                    FVAL = |{ P_DATE DATE = USER }| ) ),
                    FNAM = 'BKPF-BUDAT'
                    FVAL = |{ P_DATE DATE = USER }| ) ),
                    FNAM = 'BKPF-BLART'
                    FVAL = 'DG' ) ),
                    FNAM = 'BKPF-BUKRS'
                    FVAL = GS_VBRK-BUKRS ) ),
                    FNAM = 'BKPF-WAERS'
                    FVAL = GS_VBRK-WAERK ) ).

  LOOP AT GT_CLEARING_DOC INTO GS_CLEARING_DOC.
    APPEND INITIAL LINE TO LT_CLEAR ASSIGNING FIELD-SYMBOL(<LFS_CLEAR>).
    <LFS_CLEAR>-AGKOA = 'D'.
    <LFS_CLEAR>-AGKON = GS_VBRK-KUNRG. "Customer
    <LFS_CLEAR>-AGBUK = GS_VBRK-BUKRS.           "Company code
    <LFS_CLEAR>-XNOPS = 'X'.           "G/L Indicator
    <LFS_CLEAR>-SELFD = 'BELNR'.
*    <lfs_clear>-agums = uf_clearing-umskz.
    CONCATENATE GS_CLEARING_DOC-BELNR
                GS_CLEARING_DOC-GJAHR
           INTO <LFS_CLEAR>-SELVON.
    <LFS_CLEAR>-SELBIS = GS_CLEARING_DOC-BELNR.
  ENDLOOP.

  APPEND INITIAL LINE TO LT_CLEAR ASSIGNING <LFS_CLEAR>.
  <LFS_CLEAR>-AGKOA = 'D'.
  <LFS_CLEAR>-AGKON = GS_VBRK-KUNRG. "Customer
  <LFS_CLEAR>-AGBUK = GS_VBRK-BUKRS.           "Company code
  <LFS_CLEAR>-XNOPS = 'X'.           "G/L Indicator
  <LFS_CLEAR>-SELFD = 'BELNR'.
*    <lfs_clear>-agums = uf_clearing-umskz.
  CONCATENATE GS_VBRK-BELNR
              GS_VBRK-GJAHR
         INTO <LFS_CLEAR>-SELVON.
  <LFS_CLEAR>-SELBIS = GS_VBRK-BELNR.

  CALL FUNCTION 'POSTING_INTERFACE_START' ##FM_SUBRC_OK
    EXPORTING
      I_FUNCTION         = 'C'
      I_MODE             = LF_MODE
    EXCEPTIONS
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      USER_INVALID       = 6
      OTHERS             = 7.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING' ##FM_SUBRC_OK
    EXPORTING
      I_AUGLV                    = 'UMBUCHNG'
      I_TCODE                    = 'FB05'
      I_SGFUNCT                  = 'C'
*     I_NO_AUTH                  = ' '
      I_XSIMU                    = LF_SIM
    IMPORTING
      E_MSGID                    = LF_MSGID
      E_MSGNO                    = LF_MSGNO
      E_MSGTY                    = LF_MSGTY
      E_MSGV1                    = LF_MSGV1
      E_MSGV2                    = LF_MSGV2
      E_MSGV3                    = LF_MSGV3
      E_MSGV4                    = LF_MSGV4
      E_SUBRC                    = LF_SUBRC
    TABLES
      T_BLNTAB                   = LT_BLNTAB
      T_FTCLEAR                  = LT_CLEAR
      T_FTPOST                   = LT_POST
      T_FTTAX                    = LT_TAX
    EXCEPTIONS
      CLEARING_PROCEDURE_INVALID = 1
      CLEARING_PROCEDURE_MISSING = 2
      TABLE_T041A_EMPTY          = 3
      TRANSACTION_CODE_INVALID   = 4
      AMOUNT_FORMAT_ERROR        = 5
      TOO_MANY_LINE_ITEMS        = 6
      COMPANY_CODE_INVALID       = 7
      SCREEN_NOT_FOUND           = 8
      NO_AUTHORIZATION           = 9
      OTHERS                     = 10.

  READ TABLE LT_BLNTAB INTO DATA(LS_BLNTAB) INDEX 1.
  IF SY-SUBRC = 0.
    WRITE / |Doc. Clearing: { LS_BLNTAB-BELNR }/{ LS_BLNTAB-GJAHR }| ##NO_TEXT.
  ELSE.
*    MESSAGE ID LF_MSGID TYPE LF_MSGTY NUMBER LF_MSGNO
*        WITH LF_MSGV1 LF_MSGV2 LF_MSGV3 LF_MSGV4 INTO LF_MSG.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        ID        = LF_MSGID
        LANG      = SY-LANGU
        NO        = LF_MSGNO
        V1        = LF_MSGV1
        V2        = LF_MSGV2
        V3        = LF_MSGV3
        V4        = LF_MSGV4
      IMPORTING
        MSG       = LF_MSG
      EXCEPTIONS ##FM_SUBRC_OK
        NOT_FOUND = 1
        OTHERS    = 2.
    IF LF_MSG IS INITIAL.
      LF_MSG = 'Process Error' ##NO_TEXT.
    ENDIF.
    WRITE / |Doc. Clearing Error: { LF_MSG }| ##NO_TEXT.
  ENDIF.

  CALL FUNCTION 'POSTING_INTERFACE_END' ##FM_SUBRC_OK
    EXCEPTIONS
      SESSION_NOT_PROCESSABLE = 1
      OTHERS                  = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CLEAR_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CLEAR_DATA .
  CLEAR : P_VBELN ,
          GS_POST,
          GT_POST,
          GS_BSID,
          GT_BSID,
          GS_VBRK,
          GS_ZSDSSDT010,
          GT_ZSDSSDT010,
          GS_ZSDSSDT015,
          GT_ZSDSSDT015,
          GS_POST_DOC,
          GT_POST_DOC,
          GT_REVERSAL,
          GS_CLEARING_DOC,
          GT_FTPOST,
          GT_FTTAX,
          GT_BLNTAB,
          GS_FTPOST,
          GF_COUNT.

ENDFORM.
