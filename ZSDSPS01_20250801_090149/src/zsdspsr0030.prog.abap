*-----------------------------------------------------------------------
*  Program ID         : ZSDSPSR0030
*  Creation Date      : 08.08.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : PSE005
*  Description        : Program to set user status ‘WTEC’ for WBS – FG,
*                       Std. Warranty and Ext. Warranty
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSPSR0030.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  PRPS.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TS_WBS,
    POSID TYPE PRPS-POSID,
  END OF TS_WBS,
  TT_WBS TYPE STANDARD TABLE OF TS_WBS,
  BEGIN OF TS_PRPS,
    PSPNR TYPE PRPS-PSPNR,
    POSID TYPE PRPS-POSID,
    OBJNR TYPE PRPS-OBJNR,
  END OF TS_PRPS,
  TT_PRPS      TYPE SORTED TABLE OF TS_PRPS WITH NON-UNIQUE KEY PSPNR,
  TT_RANGE_WBS TYPE RANGE OF PRPS-POSID.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_WBS  TYPE STANDARD TABLE OF TS_WBS ##NEEDED.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
DATA:
  GRT_WBS_FG        TYPE TT_RANGE_WBS ##NEEDED,
  GRT_WBS_EXT       TYPE TT_RANGE_WBS ##NEEDED,
  GRT_WBS_STD       TYPE TT_RANGE_WBS  ##NEEDED,
  GRT_SYSTEM_STATUS TYPE RANGE OF JEST-STAT ##NEEDED,
  GRT_USER_STATUS   TYPE RANGE OF JEST-STAT ##NEEDED.
*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
DATA:
  GF_DB_PROFILE          TYPE TCNT-PROF_DB ##NEEDED,
  GF_SELECT_USER_STATUS  TYPE PS_STATUS_TEXT ##NEEDED,
  GF_USER_STATUS_PROFILE TYPE J_STSMA ##NEEDED.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF GC_VBTYP,
    QT TYPE VBAK-VBTYP VALUE 'B',
    DO TYPE VBAK-VBTYP VALUE 'J',
  END OF GC_VBTYP,
  GC_WBSTK_COMPLETE TYPE LIKP-WBSTK VALUE 'C'.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECT-OPTIONS:
  S_WBS FOR PRPS-POSID.
PARAMETERS:
  P_TEST AS CHECKBOX DEFAULT 'X'.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_SELECT_GENC.
  PERFORM F_SELECT_WBS_FG  USING GRT_WBS_FG
                           CHANGING GT_WBS.
  PERFORM F_SELECT_WBS_EXTEND_WARRANTY USING GRT_WBS_EXT
                                       CHANGING GT_WBS.
  PERFORM F_SELECT_WBS_STD_WARRANTY USING GRT_WBS_STD
                                    CHANGING GT_WBS.
  IF GT_WBS[] IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_SUBMIT_CNMASSSTATUS USING GT_WBS.

*&---------------------------------------------------------------------*
*& Form F_SELECT_WBS_FG
*&---------------------------------------------------------------------*
FORM F_SELECT_WBS_FG USING URT_WBS TYPE TT_RANGE_WBS
                     CHANGING CT_WBS TYPE TT_WBS.
  DATA: LV_TABIX     TYPE SY-TABIX,
        LV_SUM_RFMNG TYPE VBFA-RFMNG,
        LT_PRPS      TYPE TT_PRPS.
  PERFORM F_SELECT_PRPS USING URT_WBS
                        CHANGING LT_PRPS.
  IF LT_PRPS IS INITIAL.
    RETURN.
  ENDIF.

  SELECT VBELN,
         POSNR,
         KWMENG,
         PS_PSP_PNR,
         A~POSID
   FROM  VBAP INNER JOIN @LT_PRPS AS A
   ON VBAP~PS_PSP_PNR = A~PSPNR
  WHERE VBTYP_ANA = @GC_VBTYP-QT
  INTO TABLE @DATA(LT_QT).
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  SELECT VBFA~VBELV,
         VBFA~POSNV,
         VBFA~VBELN,
         VBFA~POSNN,
         VBFA~RFMNG
  FROM VBFA INNER JOIN LIKP
  ON VBFA~VBELN = LIKP~VBELN
            INNER JOIN @LT_QT AS A
  ON  VBELV = A~VBELN
  AND POSNV = A~POSNR
  WHERE WBSTK = @GC_WBSTK_COMPLETE
  AND   VBTYP_V = @GC_VBTYP-QT
  AND   VBTYP_N = @GC_VBTYP-DO
  INTO TABLE @DATA(LT_DO).
  SORT LT_DO BY VBELV POSNV.

  LOOP AT LT_QT INTO DATA(LS_QT) ##INTO_OK.
    CLEAR LV_SUM_RFMNG.
    READ TABLE LT_DO INTO DATA(LS_DO) WITH KEY VBELV = LS_QT-VBELN
                                               POSNV = LS_QT-POSNR
                                               BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_TABIX = SY-TABIX.
      LOOP AT LT_DO INTO LS_DO FROM LV_TABIX.
        IF  LS_DO-VBELV <> LS_QT-VBELN
        OR LS_DO-POSNV <> LS_QT-POSNR.
          EXIT.
        ENDIF.
        LV_SUM_RFMNG = LV_SUM_RFMNG + LS_DO-RFMNG.
      ENDLOOP.
    ENDIF.
    IF LV_SUM_RFMNG <> LS_QT-KWMENG.
      DELETE LT_QT WHERE POSID = LS_QT-POSID.
    ENDIF.
  ENDLOOP.
  SORT LT_QT BY POSID .
  DELETE ADJACENT DUPLICATES FROM LT_QT COMPARING POSID.
  MOVE-CORRESPONDING LT_QT TO CT_WBS KEEPING TARGET LINES.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_genc
*&---------------------------------------------------------------------*
FORM F_SELECT_GENC .
  DATA: LT_GEN_C   TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = SY-REPID
    IMPORTING
      ET_GEN_C = LT_GEN_C.
  LOOP AT LT_GEN_C ASSIGNING FIELD-SYMBOL(<L_GEN_C>).
    CASE <L_GEN_C>-PARAM.
      WHEN 'DB_PROFILE'.
        GF_DB_PROFILE = <L_GEN_C>-VALUE_LOW.
      WHEN 'SELECT_USER_STATUS'.
        GF_SELECT_USER_STATUS = <L_GEN_C>-VALUE_LOW.
      WHEN 'SYSTEM_STATUS'.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE GRT_SYSTEM_STATUS.
      WHEN 'USER_STATUS'.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE GRT_USER_STATUS.
      WHEN 'USER_STATUS_PROFILE'.
        GF_USER_STATUS_PROFILE = <L_GEN_C>-VALUE_LOW.
      WHEN 'WBS_FG'.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE GRT_WBS_FG.
      WHEN 'WBS_SERV_EXTEND_WARRANTY'.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE GRT_WBS_EXT.
      WHEN 'WBS_SERV_STD_WARRANTY'.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE GRT_WBS_STD.

    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_wbs_extend_warranty
*&---------------------------------------------------------------------*
FORM F_SELECT_WBS_EXTEND_WARRANTY USING URT_WBS TYPE TT_RANGE_WBS
                                  CHANGING CT_WBS TYPE TT_WBS.
  DATA: LT_PRPS      TYPE TT_PRPS.

  PERFORM F_SELECT_PRPS USING URT_WBS

                        CHANGING LT_PRPS.
  IF LT_PRPS IS INITIAL.
    RETURN.
  ENDIF.

  SELECT EXT_POSID,                                     "#EC CI_NOFIELD
         EXT_WRT_END,
         B~POSID
  FROM ZSDSCMT003 AS A INNER JOIN @LT_PRPS AS B
  ON A~EXT_POSID = B~POSID
  WHERE EXT_WRT_END < @SY-DATUM
  AND  EXT_POSID NOT IN ( SELECT EXT_POSID
                           FROM ZSDSCMT003
                           WHERE EXT_POSID IN @GRT_WBS_EXT
                           AND  ( EXT_WRT_END >= @SY-DATUM
                           OR EXT_WRT_END = '00000000' )
                         )
  AND EXT_POSID NOT IN ( SELECT AC_ASSIGNMENT
                         FROM CRMS4D_SERV_I
                         WHERE EXT_POSID IN @GRT_WBS_EXT
                         AND   STAT_LIFECYCLE <> 'C' )
  INTO TABLE @DATA(LT_003).

  SORT LT_003 BY POSID.
  DELETE ADJACENT DUPLICATES FROM LT_003 COMPARING POSID.
  MOVE-CORRESPONDING LT_003 TO CT_WBS KEEPING TARGET LINES.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SUBMIT_CNMASSSTATUS
*&---------------------------------------------------------------------*
FORM F_SUBMIT_CNMASSSTATUS  USING  UT_WBS TYPE TT_WBS.
  DATA: LR_POSID TYPE RANGE OF PRPS_R-POSID,
        LR_STUFE TYPE RANGE OF PRPS_R-STUFE. "default value
  DATA: LF_PROCESSED        TYPE FLAG,
        LS_PRINT_PARAMETERS TYPE PRI_PARAMS.

  INSERT VALUE #( SIGN = 'I'
                  OPTION = 'EQ'
                  LOW = 1     ##NUMBER_OK
                  HIGH = 99 ) ##NUMBER_OK
         INTO TABLE LR_STUFE.

  LOOP AT UT_WBS INTO DATA(LS_WBS).
    INSERT VALUE #( SIGN   =  'I'
                    OPTION =  'EQ'
                    LOW    =  LS_WBS-POSID
                    HIGH   =  '' )
           INTO TABLE LR_POSID    .
  ENDLOOP.

  SET PARAMETER ID 'PDB'  FIELD GF_DB_PROFILE.
  LF_PROCESSED = 'X'.
  EXPORT LV_PROCESSED FROM LF_PROCESSED TO MEMORY ID 'PROCESSED'.

  IF SY-BATCH = ABAP_TRUE.
    SUBMIT RCNMASSSTATUS                                 "#EC CI_SUBMIT
    TO SAP-SPOOL
    SPOOL PARAMETERS LS_PRINT_PARAMETERS
    WITHOUT SPOOL DYNPRO
    WITH CN_PSPNR IN LR_POSID          "wbs
    WITH CN_STUFE IN LR_STUFE          "level
    WITH SYS = ABAP_FALSE              "system status
    WITH USR = ABAP_TRUE               "user status
    WITH PROF = GF_USER_STATUS_PROFILE "user status profile
    WITH UUSR = GF_SELECT_USER_STATUS  "select user status
    WITH SEL = ABAP_FALSE              "show screen for selection
    WITH TEST = P_TEST                "test run
    AND RETURN.
*    ENDIF.
  ELSE.
    SUBMIT RCNMASSSTATUS                                 "#EC CI_SUBMIT
    WITH CN_PSPNR IN LR_POSID          "wbs
    WITH CN_STUFE IN LR_STUFE          "level
    WITH SYS = ABAP_FALSE              "system status
    WITH USR = ABAP_TRUE               "user status
    WITH PROF = GF_USER_STATUS_PROFILE "user status profile
    WITH UUSR = GF_SELECT_USER_STATUS  "select user status
    WITH SEL = ABAP_FALSE              "show screen for selection
    WITH TEST = P_TEST                "test run
    AND RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_WBS_STD_WARRANTY
*&---------------------------------------------------------------------*
FORM F_SELECT_WBS_STD_WARRANTY  USING URT_WBS TYPE TT_RANGE_WBS
                                CHANGING CT_WBS TYPE TT_WBS.
  DATA: LT_PRPS      TYPE TT_PRPS.

  PERFORM F_SELECT_PRPS USING URT_WBS
                        CHANGING LT_PRPS.

  IF LT_PRPS IS INITIAL.
    RETURN.
  ENDIF.

  SELECT STD_POSID,                                     "#EC CI_NOFIELD
         STD_WRT_END,
         B~POSID
  FROM ZSDSCMT003 AS A INNER JOIN @LT_PRPS AS B
  ON A~STD_POSID = B~POSID
  WHERE STD_WRT_END < @SY-DATUM
  AND STD_POSID NOT IN ( SELECT STD_POSID
                           FROM ZSDSCMT003
                           WHERE STD_POSID IN @GRT_WBS_STD
                           AND ( STD_WRT_END >= @SY-DATUM
                             OR STD_WRT_END = '00000000'
                             OR ( WRTLT_FLAG = '1' AND WRTLT = ''  ) )
                        )
  AND STD_POSID NOT IN ( SELECT AC_ASSIGNMENT
                         FROM CRMS4D_SERV_I
                         WHERE STD_POSID IN @GRT_WBS_STD
                         AND   STAT_LIFECYCLE <> 'C' )
  INTO TABLE @DATA(LT_003).

  SORT LT_003 BY POSID.
  DELETE ADJACENT DUPLICATES FROM LT_003 COMPARING POSID.
  MOVE-CORRESPONDING LT_003 TO CT_WBS KEEPING TARGET LINES.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_prps
*&---------------------------------------------------------------------*
FORM F_SELECT_PRPS  USING    URT_WBS TYPE TT_RANGE_WBS
                    CHANGING CT_PRPS TYPE TT_PRPS.
  SELECT PSPNR,
         POSID,
         PRPS~OBJNR
  FROM PRPS
  WHERE POSID IN @URT_WBS
  AND   POSID IN @S_WBS
  AND   OBJNR IN ( SELECT OBJNR
                   FROM JEST
                   WHERE OBJNR = PRPS~OBJNR
                   AND   STAT IN @GRT_SYSTEM_STATUS
                   AND   INACT = '' )
  AND   OBJNR NOT IN ( SELECT OBJNR
                   FROM JEST
                   WHERE OBJNR = PRPS~OBJNR
                   AND   STAT IN @GRT_USER_STATUS
                   AND   INACT = '' )
  INTO TABLE @CT_PRPS.
ENDFORM.
