class ZCL_SDSCM_WARRANTY_UTIL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_INST_INFO,
        EQUNR      TYPE  ZSDSCMT003-EQUNR,
        DTYPE      TYPE  ZSDSDE_CM_DTYPE,
        DATUM      TYPE  SY-DATUM,
        INACT      TYPE  CRM_JCDS-INACT,
        OBJECT_ID  TYPE  CRMS4D_SERV_I-OBJECT_ID,
        NUMBER_INT TYPE  CRMS4D_SERV_I-NUMBER_INT,
      END OF TS_INST_INFO .
  types:
    BEGIN OF TS_REGISTER_INFO,
        EQUNR       TYPE  ZSDSCMT003-EQUNR,
        MATNR       TYPE  ZSDSCMT003-MATNR,
        SERNR       TYPE  ZSDSCMT003-SERNR,
        WRTPK       TYPE  ZSDSCMT003-WRTPK,
        WRTLT_FLAG  TYPE  ZSDSCMT003-WRTLT_FLAG,
        VBELN_VL    TYPE  ZSDSCMT003-VBELN_VL,
        FKDAT       TYPE  ZSDSCMT003-FKDAT,
        WADAT_IST   TYPE  ZSDSCMT003-WADAT_IST,
        VBELN_VA    TYPE  ZSDSCMT003-VBELN_VA,
        KUNNR       TYPE  ZSDSCMT003-KUNNR,
        VKBUR       TYPE  ZSDSCMT003-VKBUR,
        VKGRP       TYPE  ZSDSCMT003-VKGRP,
        GI_WRT_BEG  TYPE  ZSDSCMT003-GI_WRT_BEG,
        GI_WRT_END  TYPE  ZSDSCMT003-GI_WRT_END,
        STD_WRT_BEG TYPE  ZSDSCMT003-STD_WRT_BEG,
        STD_WRT_END TYPE  ZSDSCMT003-STD_WRT_END,
        MAT_POSID   TYPE  ZSDSCMT003-MAT_POSID,
        STD_POSID   TYPE  ZSDSCMT003-STD_POSID,
        EXT_POSID   TYPE  ZSDSCMT003-EXT_POSID,
      END OF TS_REGISTER_INFO .

  constants GC_LOG_OBJECT type BALOBJ_D value 'ZSDSWARRTY' ##NO_TEXT.
  constants GC_LOG_SUBOBJECT type BALSUBOBJ value 'WARRTY_MASTR' ##NO_TEXT.
  constants GC_INSTALL type ZSDSDE_CM_DTYPE value '1' ##NO_TEXT.
  constants GC_COMMISSION type ZSDSDE_CM_DTYPE value '2' ##NO_TEXT.

  class-methods REGISTER_WARRANTY_PRODUCT
    importing
      !IS_INPUT type TS_REGISTER_INFO
      !IF_TEST type FLAG
      !IF_COMMIT type FLAG default 'X'
    exporting
      !ES_RETURN type BAPIRET1 .
  class-methods INST_COMM_WARRANTY_PRODUCT
    importing
      !IS_INPUT type TS_INST_INFO
      !IF_TEST type FLAG default 'X'
      !IF_COMMIT type FLAG default 'X'
    exporting
      !ES_RETURN type BAPIRET1 .
  class-methods UPDATE_WARRANTY_LETTER
    importing
      !IS_INPUT type ZSDSCMT003
      !IF_TEST type FLAG default 'X'
      !IF_COMMIT type FLAG default 'X'
    exporting
      !ES_RETURN type BAPIRET1 .
protected section.
private section.

  class-methods MODIFY_WARRANTY_MASTER
    importing
      !IS_DATA type ZSDSCMT003
      !IS_EXIST type ZSDSCMT003 optional
      !IF_TEST type FLAG default 'X'
      !IF_COMMIT type FLAG default 'X'
    exporting
      !ES_RETURN type BAPIRET1 .
  class-methods ADD_PROCESSING_LOG
    importing
      !IF_EQUNR type ZSDSCMT003-EQUNR
      !IT_MSG type BAL_T_MSG
    exporting
      !ES_RETURN type BAPIRET1 .
ENDCLASS.



CLASS ZCL_SDSCM_WARRANTY_UTIL IMPLEMENTATION.


METHOD ADD_PROCESSING_LOG.

  DATA
    LT_LOG_HANDLE  TYPE  BAL_T_LOGH.

  DATA:
    LS_LOG  TYPE  BAL_S_LOG.

  DATA:
    LF_LOG_HANDLE  TYPE  BALLOGHNDL.


* Initialize Output
  CLEAR: ES_RETURN.

  CLEAR LS_LOG.

  WRITE IF_EQUNR TO LS_LOG-EXTNUMBER LEFT-JUSTIFIED.
  CONDENSE LS_LOG-EXTNUMBER.
  LS_LOG-OBJECT     = GC_LOG_OBJECT.
  LS_LOG-SUBOBJECT  = GC_LOG_SUBOBJECT.
  LS_LOG-ALDATE     = SY-DATUM.
  LS_LOG-ALTIME     = SY-UZEIT.
  LS_LOG-ALUSER     = SY-UNAME.
  LS_LOG-ALTCODE    = SY-TCODE.
  LS_LOG-ALPROG     = SY-CPROG.
  IF SY-BATCH IS INITIAL.
    LS_LOG-ALMODE = 'D'.
  ELSE.
    LS_LOG-ALMODE = 'B'.
  ENDIF.

* Create Log Header
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG                 = LS_LOG
    IMPORTING
      E_LOG_HANDLE            = LF_LOG_HANDLE
    EXCEPTIONS
      LOG_HEADER_INCONSISTENT = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    ES_RETURN-TYPE        = SY-MSGTY.
    ES_RETURN-ID          = SY-MSGID.
    ES_RETURN-NUMBER      = SY-MSGNO.
    ES_RETURN-MESSAGE_V1  = SY-MSGV1.
    ES_RETURN-MESSAGE_V2  = SY-MSGV2.
    ES_RETURN-MESSAGE_V3  = SY-MSGV3.
    ES_RETURN-MESSAGE_V4  = SY-MSGV4.
    MESSAGE ID SY-MSGID
            TYPE SY-MSGTY
            NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

  LOOP AT IT_MSG ASSIGNING FIELD-SYMBOL(<L_MSG>).
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        I_LOG_HANDLE     = LF_LOG_HANDLE
        I_S_MSG          = <L_MSG>
      EXCEPTIONS
        LOG_NOT_FOUND    = 1
        MSG_INCONSISTENT = 2
        LOG_IS_FULL      = 3
        OTHERS           = 4.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  INSERT LF_LOG_HANDLE INTO TABLE LT_LOG_HANDLE.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      I_CLIENT         = SY-MANDT
      I_IN_UPDATE_TASK = 'X'
      I_T_LOG_HANDLE   = LT_LOG_HANDLE
    EXCEPTIONS
      LOG_NOT_FOUND    = 1
      SAVE_NOT_ALLOWED = 2
      NUMBERING_ERROR  = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
    ES_RETURN-TYPE        = SY-MSGTY.
    ES_RETURN-ID          = SY-MSGID.
    ES_RETURN-NUMBER      = SY-MSGNO.
    ES_RETURN-MESSAGE_V1  = SY-MSGV1.
    ES_RETURN-MESSAGE_V2  = SY-MSGV2.
    ES_RETURN-MESSAGE_V3  = SY-MSGV3.
    ES_RETURN-MESSAGE_V4  = SY-MSGV4.
    MESSAGE ID SY-MSGID
            TYPE SY-MSGTY
            NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD INST_COMM_WARRANTY_PRODUCT.

  DATA:
    LS_DATA  TYPE  ZSDSCMT003,
    LS_EXIST TYPE  ZSDSCMT003.

  FIELD-SYMBOLS:
    <L_DATUM>     TYPE  ZSDSCMT003-INSDT,
    <L_OBJECT_ID> TYPE  ZSDSCMT003-INSSVO.


* Initialize output
  CLEAR: ES_RETURN.

  DO 1 TIMES.

*   Lock Data
    CALL FUNCTION 'ENQUEUE_EZSDSCMT003'
      EXPORTING
        MODE_ZSDSCMT003 = 'E'
        MANDT           = SY-MANDT
        EQUNR           = IS_INPUT-EQUNR
      EXCEPTIONS
        FOREIGN_LOCK    = 1
        SYSTEM_FAILURE  = 2
        OTHERS          = 3.
    IF SY-SUBRC <> 0.
      ES_RETURN-TYPE        = SY-MSGTY.
      ES_RETURN-ID          = SY-MSGID.
      ES_RETURN-NUMBER      = SY-MSGNO.
      ES_RETURN-MESSAGE_V1  = SY-MSGV1.
      ES_RETURN-MESSAGE_V2  = SY-MSGV2.
      ES_RETURN-MESSAGE_V3  = SY-MSGV3.
      ES_RETURN-MESSAGE_V4  = SY-MSGV4.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                   ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
              INTO ES_RETURN-MESSAGE.
      EXIT.
    ENDIF.

*   Get Existing Warranty
    SELECT SINGLE *
      FROM ZSDSCMT003 AS A
     WHERE A~EQUNR EQ @IS_INPUT-EQUNR
      INTO @LS_EXIST.
    IF SY-SUBRC NE 0.
      CLEAR LS_EXIST.
    ENDIF.

*   Assign Data
    IF LS_EXIST IS NOT INITIAL.
      LS_DATA = LS_EXIST.
    ELSE.
      CLEAR LS_DATA.
      LS_DATA-EQUNR = IS_INPUT-EQUNR.
    ENDIF.

*   Determine Processing Fields
    CASE IS_INPUT-DTYPE.
      WHEN GC_INSTALL.
        ASSIGN ('LS_DATA-INSDT') TO <L_DATUM>.
        ASSIGN ('LS_DATA-INSSVO') TO <L_OBJECT_ID>.
      WHEN GC_COMMISSION.
        ASSIGN ('LS_DATA-COMDT') TO <L_DATUM>.
        ASSIGN ('LS_DATA-COMSVO') TO <L_OBJECT_ID>.
    ENDCASE.

*   Update Field value
    CASE IS_INPUT-INACT.

*     Complete activated
      WHEN SPACE.
        IF <L_DATUM> LT IS_INPUT-DATUM.
          <L_DATUM>  = IS_INPUT-DATUM.
          <L_OBJECT_ID> = IS_INPUT-OBJECT_ID.
        ELSEIF <L_DATUM> GT IS_INPUT-DATUM .
*         Error: Existing Warranty data &1(&2) newer than updating data &3(&4).
          ES_RETURN-TYPE        = 'E'.
          ES_RETURN-ID          = 'ZSDSCM01'.
          ES_RETURN-NUMBER      = '036'.
          WRITE: <L_DATUM>          TO ES_RETURN-MESSAGE_V1,
                 <L_OBJECT_ID>      TO ES_RETURN-MESSAGE_V2,
                 IS_INPUT-DATUM     TO ES_RETURN-MESSAGE_V3,
                 IS_INPUT-OBJECT_ID TO ES_RETURN-MESSAGE_V4.
          MESSAGE ID ES_RETURN-ID
                  TYPE ES_RETURN-TYPE
                  NUMBER ES_RETURN-NUMBER
                  WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                       ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
                  INTO ES_RETURN-MESSAGE.
          EXIT.
        ENDIF.

*     Complete de-activated
      WHEN 'X'.
        IF <L_OBJECT_ID> IS NOT INITIAL.
          IF <L_OBJECT_ID> EQ IS_INPUT-OBJECT_ID.
            CLEAR: <L_DATUM>,
                   <L_OBJECT_ID>.
          ELSE.
*           The existing data is not from SVO
*           Error: Existing Warranty data &1(&2) is not from SVO &3.
            ES_RETURN-TYPE        = 'E'.
            ES_RETURN-ID          = 'ZSDSCM01'.
            ES_RETURN-NUMBER      = '037'.
            WRITE: <L_DATUM>          TO ES_RETURN-MESSAGE_V1,
                   <L_OBJECT_ID>      TO ES_RETURN-MESSAGE_V2,
                   IS_INPUT-OBJECT_ID TO ES_RETURN-MESSAGE_V3.
            MESSAGE ID ES_RETURN-ID
                    TYPE ES_RETURN-TYPE
                    NUMBER ES_RETURN-NUMBER
                    WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                         ES_RETURN-MESSAGE_V3
                    INTO ES_RETURN-MESSAGE.
            EXIT.
          ENDIF.
        ENDIF.
    ENDCASE.

*   Update Data into table
    MODIFY_WARRANTY_MASTER(
      EXPORTING
        IS_DATA   = LS_DATA
        IS_EXIST  = LS_EXIST
        IF_TEST   = IF_TEST
        IF_COMMIT = IF_COMMIT
      IMPORTING
        ES_RETURN = ES_RETURN ).

  ENDDO.

* Unlock Data
  CALL FUNCTION 'DEQUEUE_EZSDSCMT003'
    EXPORTING
      MODE_ZSDSCMT003 = 'E'
      MANDT           = SY-MANDT
      EQUNR           = IS_INPUT-EQUNR.

ENDMETHOD.


METHOD MODIFY_WARRANTY_MASTER.

*  DATA:
*    LT_MSG TYPE BAL_T_MSG.

  DATA:
    LS_SAVE  TYPE  ZSDSCMT003,
    LS_EXIST TYPE  ZSDSCMT003.
*    LS_RETURN TYPE  BAPIRET1.

  DATA:
    LF_LOCK  TYPE  FLAG.


* Initialize Output
  CLEAR: ES_RETURN.

  DO 1 TIMES.

    CLEAR LF_LOCK.
    IF IS_EXIST IS NOT SUPPLIED.
*     Lock Data
      CALL FUNCTION 'ENQUEUE_EZSDSCMT003'
        EXPORTING
          MODE_ZSDSCMT003 = 'E'
          MANDT           = SY-MANDT
          EQUNR           = IS_DATA-EQUNR
        EXCEPTIONS
          FOREIGN_LOCK    = 1
          SYSTEM_FAILURE  = 2
          OTHERS          = 3.
      IF SY-SUBRC <> 0.
        ES_RETURN-TYPE        = SY-MSGTY.
        ES_RETURN-ID          = SY-MSGID.
        ES_RETURN-NUMBER      = SY-MSGNO.
        ES_RETURN-MESSAGE_V1  = SY-MSGV1.
        ES_RETURN-MESSAGE_V2  = SY-MSGV2.
        ES_RETURN-MESSAGE_V3  = SY-MSGV3.
        ES_RETURN-MESSAGE_V4  = SY-MSGV4.
        MESSAGE ID ES_RETURN-ID
                TYPE ES_RETURN-TYPE
                NUMBER ES_RETURN-NUMBER
                WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                     ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
                INTO ES_RETURN-MESSAGE.
        EXIT.
      ENDIF.
      LF_LOCK = 'X'.

*     Read Existing Data
      SELECT SINGLE *
        FROM ZSDSCMT003
       WHERE EQUNR EQ @IS_DATA-EQUNR
        INTO @LS_EXIST.
      IF SY-SUBRC NE 0.
        CLEAR LS_EXIST.
      ENDIF.
    ELSE.
      LS_EXIST = IS_EXIST.
    ENDIF.

*   Compare any change?
    IF IS_DATA EQ LS_EXIST.
*     Error: No data changed.
      ES_RETURN-TYPE        = 'S'.
      ES_RETURN-ID          = 'ZSDSCM01'.
      ES_RETURN-NUMBER      = '038'.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              INTO ES_RETURN-MESSAGE.
      EXIT.
    ENDIF.

*   Assign Saving Data
    CLEAR LS_SAVE.
    LS_SAVE = IS_DATA.

*   Mark Update Flag
    LS_SAVE-UPDFLG = 'X'.

    IF LS_SAVE-ERDAT IS INITIAL.
*     Fill Create Info
      LS_SAVE-ERDAT = SY-DATUM.
      LS_SAVE-ERZET = SY-UZEIT.
      LS_SAVE-ERNAM = SY-UNAME.
    ELSE.
*     Fill Update Info
      LS_SAVE-AEDAT = SY-DATUM.
      LS_SAVE-AEZET = SY-UZEIT.
      LS_SAVE-AENAM = SY-UNAME.
    ENDIF.

*   Update Database table
    MODIFY ZSDSCMT003 FROM LS_SAVE.
    IF SY-SUBRC NE 0.
*     Error: Error during maintaining data into table ZSDSCMT003.
      ES_RETURN-TYPE        = 'E'.
      ES_RETURN-ID          = 'ZSDSCM01'.
      ES_RETURN-NUMBER      = '033'.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              INTO ES_RETURN-MESSAGE.
      EXIT.
    ENDIF.

  ENDDO.

  IF IF_TEST EQ 'X'.
    ROLLBACK WORK.                                     "#EC CI_ROLLBACK

  ELSE.

**   Call Save SLG1 Log
*    ADD_PROCESSING_LOG(
*      EXPORTING
*        IF_EQUNR  = IS_DATA-EQUNR
*        IT_MSG    = LT_MSG
*      IMPORTING
*        ES_RETURN = LS_RETURN ).

    IF IF_COMMIT EQ 'X'.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF ES_RETURN IS INITIAL.
*   Assign Successful message
    ES_RETURN-TYPE        = 'S'.
    IF IF_TEST EQ 'X'.
*     Message: Test run successfully.
      ES_RETURN-ID          = 'ZSDSCM01'.
      ES_RETURN-NUMBER      = '034'.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              INTO ES_RETURN-MESSAGE.
    ELSE.
*     Message: Warranty master data updated successfully.
      ES_RETURN-ID          = 'ZSDSCM01'.
      ES_RETURN-NUMBER      = '035'.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              INTO ES_RETURN-MESSAGE.
    ENDIF.
  ENDIF.

  IF LF_LOCK EQ 'X'.
    CALL FUNCTION 'DEQUEUE_EZSDSCMT003'
      EXPORTING
        MODE_ZSDSCMT003 = 'E'
        MANDT           = SY-MANDT
        EQUNR           = IS_DATA-EQUNR.
  ENDIF.

ENDMETHOD.


METHOD REGISTER_WARRANTY_PRODUCT.

  DATA:
    LS_DATA  TYPE  ZSDSCMT003,
    LS_EXIST TYPE  ZSDSCMT003.

*  FIELD-SYMBOLS:
*    <L_DATUM>     TYPE  ZSDSCMT003-INSDT,
*    <L_OBJECT_ID> TYPE  ZSDSCMT003-INSSVO.


* Initialize output
  CLEAR: ES_RETURN.

  DO 1 TIMES.

*   Lock Data
    CALL FUNCTION 'ENQUEUE_EZSDSCMT003'
      EXPORTING
        MODE_ZSDSCMT003 = 'E'
        MANDT           = SY-MANDT
        EQUNR           = IS_INPUT-EQUNR
      EXCEPTIONS
        FOREIGN_LOCK    = 1
        SYSTEM_FAILURE  = 2
        OTHERS          = 3.
    IF SY-SUBRC <> 0.
      ES_RETURN-TYPE        = SY-MSGTY.
      ES_RETURN-ID          = SY-MSGID.
      ES_RETURN-NUMBER      = SY-MSGNO.
      ES_RETURN-MESSAGE_V1  = SY-MSGV1.
      ES_RETURN-MESSAGE_V2  = SY-MSGV2.
      ES_RETURN-MESSAGE_V3  = SY-MSGV3.
      ES_RETURN-MESSAGE_V4  = SY-MSGV4.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                   ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
              INTO ES_RETURN-MESSAGE.
      EXIT.
    ENDIF.

*   Get Existing Warranty
    SELECT SINGLE *
      FROM ZSDSCMT003 AS A
     WHERE A~EQUNR EQ @IS_INPUT-EQUNR
      INTO @LS_EXIST.
    IF SY-SUBRC NE 0.
      CLEAR LS_EXIST.
    ENDIF.

*   Assign Data
    IF LS_EXIST IS NOT INITIAL.
      LS_DATA = LS_EXIST.
    ELSE.
      CLEAR LS_DATA.
      LS_DATA-EQUNR = IS_INPUT-EQUNR.
    ENDIF.

*   Fill values
    LS_DATA-MATNR       = IS_INPUT-MATNR.
    LS_DATA-SERNR       = IS_INPUT-SERNR.
    LS_DATA-WRTPK       = IS_INPUT-WRTPK.
    LS_DATA-WRTLT_FLAG  = IS_INPUT-WRTLT_FLAG.
    LS_DATA-VBELN_VL    = IS_INPUT-VBELN_VL.
    LS_DATA-FKDAT       = IS_INPUT-FKDAT.
    LS_DATA-WADAT_IST   = IS_INPUT-WADAT_IST.
    LS_DATA-VBELN_VA    = IS_INPUT-VBELN_VA.
    LS_DATA-KUNNR       = IS_INPUT-KUNNR.
    LS_DATA-VKBUR       = IS_INPUT-VKBUR.
    LS_DATA-VKGRP       = IS_INPUT-VKGRP.
    LS_DATA-GI_WRT_BEG  = IS_INPUT-GI_WRT_BEG.
    LS_DATA-GI_WRT_END  = IS_INPUT-GI_WRT_END.
    LS_DATA-STD_WRT_BEG = IS_INPUT-STD_WRT_BEG.
    LS_DATA-STD_WRT_END = IS_INPUT-STD_WRT_END.
    LS_DATA-MAT_POSID   = IS_INPUT-MAT_POSID.
    LS_DATA-STD_POSID   = IS_INPUT-STD_POSID.
    LS_DATA-EXT_POSID   = IS_INPUT-EXT_POSID.

*   Update Data into table
    MODIFY_WARRANTY_MASTER(
      EXPORTING
        IS_DATA   = LS_DATA
        IS_EXIST  = LS_EXIST
        IF_TEST   = IF_TEST
        IF_COMMIT = SPACE
      IMPORTING
        ES_RETURN = ES_RETURN ).

  ENDDO.

* Unlock Data
  CALL FUNCTION 'DEQUEUE_EZSDSCMT003'
    EXPORTING
      MODE_ZSDSCMT003 = 'E'
      MANDT           = SY-MANDT
      EQUNR           = IS_INPUT-EQUNR.

ENDMETHOD.


  method UPDATE_WARRANTY_LETTER.

  DATA:
    LS_DATA  TYPE  ZSDSCMT003,
    LS_EXIST TYPE  ZSDSCMT003.

* Initialize output
  CLEAR: ES_RETURN.

  DO 1 TIMES.

*   Lock Data
    CALL FUNCTION 'ENQUEUE_EZSDSCMT003'
      EXPORTING
        MODE_ZSDSCMT003 = 'E'
        MANDT           = SY-MANDT
        EQUNR           = IS_INPUT-EQUNR
      EXCEPTIONS
        FOREIGN_LOCK    = 1
        SYSTEM_FAILURE  = 2
        OTHERS          = 3.
    IF SY-SUBRC <> 0.
      ES_RETURN-TYPE        = SY-MSGTY.
      ES_RETURN-ID          = SY-MSGID.
      ES_RETURN-NUMBER      = SY-MSGNO.
      ES_RETURN-MESSAGE_V1  = SY-MSGV1.
      ES_RETURN-MESSAGE_V2  = SY-MSGV2.
      ES_RETURN-MESSAGE_V3  = SY-MSGV3.
      ES_RETURN-MESSAGE_V4  = SY-MSGV4.
      MESSAGE ID ES_RETURN-ID
              TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                   ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
              INTO ES_RETURN-MESSAGE.
      EXIT.
    ENDIF.

*   Get Existing Warranty
    SELECT SINGLE *
      FROM ZSDSCMT003 AS A
     WHERE A~EQUNR EQ @IS_INPUT-EQUNR
      INTO @LS_EXIST.
    IF SY-SUBRC NE 0.
      CLEAR LS_EXIST.
      "Data does not exist
      MESSAGE ID 'CSM_DATA_SUP'
              TYPE 'E'
              NUMBER '103'
              WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                   ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
              INTO ES_RETURN-MESSAGE.
      EXIT.
    ENDIF.

    LS_DATA = IS_INPUT.

*   Update Data into table
    MODIFY_WARRANTY_MASTER(
      EXPORTING
        IS_DATA   = LS_DATA
        IS_EXIST  = LS_EXIST
        IF_TEST   = IF_TEST
        IF_COMMIT = SPACE
      IMPORTING
        ES_RETURN = ES_RETURN ).

  ENDDO.

* Unlock Data
  CALL FUNCTION 'DEQUEUE_EZSDSCMT003'
    EXPORTING
      MODE_ZSDSCMT003 = 'E'
      MANDT           = SY-MANDT
      EQUNR           = IS_INPUT-EQUNR.
  endmethod.
ENDCLASS.
