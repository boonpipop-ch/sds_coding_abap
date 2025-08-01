CLASS ZCL_SDSSD_ORDER_CONFIRMATION DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TS_ADVREC_GROUP,
        ADVGP TYPE  ZSDSDE_ADVGP,
        RQASS TYPE  FLAG,
        ZTERM TYPE  RANGE OF VBKD-ZTERM,
        UMSKZ TYPE  RANGE OF BSEG-UMSKZ,
      END OF TS_ADVREC_GROUP .
    TYPES:
      TT_ADVREC_GROUP  TYPE  SORTED TABLE OF TS_ADVREC_GROUP
                               WITH UNIQUE KEY ADVGP .
    TYPES:
      BEGIN OF TS_SO,
        VBELN TYPE  VBAK-VBELN,
      END OF TS_SO .
    TYPES:
      TT_SO  TYPE  STANDARD TABLE OF TS_SO
                               WITH DEFAULT KEY .
    TYPES:
      BEGIN OF TS_CUST,
        KUNNR TYPE  VBAK-KUNNR,
      END OF TS_CUST .
    TYPES:
      TT_CUST  TYPE  STANDARD TABLE OF TS_CUST
                                 WITH DEFAULT KEY .
    TYPES:
      BEGIN OF TS_SCHEDLINE,
        VBELN TYPE  VBEP-VBELN,
        POSNR TYPE  VBEP-POSNR,
        ETENR TYPE  VBEP-ETENR,
      END OF TS_SCHEDLINE .
    TYPES:
      TT_SCHEDLINE  TYPE  SORTED TABLE OF TS_SCHEDLINE
                                        WITH UNIQUE KEY VBELN
                                                        POSNR
                                                        ETENR .
    TYPES:
      BEGIN OF TS_APPV_INFO,
        UNAME     TYPE  SY-UNAME,
        DATUM     TYPE  SY-DATUM,
        UZEIT     TYPE  SY-UZEIT,
        APPROVER  TYPE  CHAR1,
        APPV_STAT TYPE ZSDSDE_SD_APPV_STAT,
      END OF TS_APPV_INFO .
    TYPES:
      TT_VBELN_RANGE TYPE RANGE OF VBAK-VBELN .
    TYPES:
      TT_POSNR_RANGE TYPE RANGE OF VBEP-POSNR .
    TYPES:
      TT_ETENR_RANGE TYPE RANGE OF VBEP-ETENR .
    TYPES:
      TT_EDATU_RANGE TYPE RANGE OF VBEP-EDATU .
    TYPES:
      TT_VKORG_RANGE TYPE RANGE OF VBAK-VKORG .
    TYPES:
      TT_VTWEG_RANGE TYPE RANGE OF VBAK-VTWEG .
    TYPES:
      TT_KUNNR_RANGE TYPE RANGE OF VBAK-KUNNR .
    TYPES:
      TT_VKBUR_RANGE TYPE RANGE OF VBAK-VKBUR .
    TYPES:
      TT_VKGRP_RANGE TYPE RANGE OF VBAK-VKGRP .
    TYPES:
      TT_AUART_RANGE TYPE RANGE OF VBAK-AUART .
    TYPES:
      TT_SPART_RANGE TYPE RANGE OF VBAK-SPART .
    TYPES:
      TT_MATNR_RANGE TYPE RANGE OF VBAP-MATNR .
    TYPES:
      BEGIN OF TS_ADVREC_DATA,
        CHKBOX TYPE  CHAR1,
        BUKRS  TYPE  ZSDSSDT010-BUKRS,
        UMSKZ  TYPE  BSID_VIEW-UMSKZ,
        BELNR  TYPE  ZSDSSDT010-BELNR,
        GJAHR  TYPE  ZSDSSDT010-GJAHR,
        BUDAT  TYPE  BKPF-BUDAT,
        DMBTR  TYPE  ZSDSSDT010-DMBTR,
        WAERS  TYPE  ZSDSSDT010-WAERS,
      END OF TS_ADVREC_DATA .
    TYPES:
      TT_ADVREC_DATA  TYPE  STANDARD TABLE OF TS_ADVREC_DATA
                                                 WITH DEFAULT KEY .
    TYPES:
      BEGIN OF TS_CONF_DATA,
        VBELN         TYPE  VBEP-VBELN,
        POSNR         TYPE  VBEP-POSNR,
        ETENR         TYPE  VBEP-ETENR,
        EDATU         TYPE  VBEP-EDATU,
        BMENG         TYPE  VBEP-BMENG,
        VRKME         TYPE  VBEP-VRKME,
        LIFSP         TYPE  VBEP-LIFSP,
        UEPOS         TYPE  VBAP-UEPOS,
        KUNNR         TYPE  VBAK-KUNNR,
        KUNNR_NAME    TYPE  ZSDSDE_KUNNR_NAME,
        VTWEG         TYPE  VBAK-VTWEG,
        VTWEG_TXT     TYPE  TVTWT-VTEXT,
        AUART         TYPE  VBAK-AUART,
        AUART_TXT     TYPE  TVAKT-BEZEI,
        VKBUR         TYPE  VBAK-VKBUR,
        VKBUR_TXT     TYPE  TVKBT-BEZEI,
        VKGRP         TYPE  VBAK-VKGRP,
        VKGRP_TXT     TYPE  TVGRT-BEZEI,
        SPART         TYPE  VBAK-SPART,
        SPART_TXT     TYPE  TSPAT-VTEXT,
        ZTERM         TYPE  VBKD-ZTERM,
        MATNR         TYPE  VBAP-MATNR,
        ARKTX         TYPE  VBAP-ARKTX,
        CO_APPV_STAT  TYPE  ZSDSSDT009-CO_APPV_STAT,
        CO_APPV_BY    TYPE  ZSDSSDT009-CO_APPV_BY,
        CO_APPV_DATE  TYPE  ZSDSSDT009-CO_APPV_DATE,
        CO_APPV_TIME  TYPE  ZSDSSDT009-CO_APPV_TIME,
        MGR_APPV_STAT TYPE  ZSDSSDT009-MGR_APPV_STAT,
        MGR_APPV_BY   TYPE  ZSDSSDT009-MGR_APPV_BY,
        MGR_APPV_DATE TYPE  ZSDSSDT009-MGR_APPV_DATE,
        MGR_APPV_TIME TYPE  ZSDSSDT009-MGR_APPV_TIME,
        AMOUNT0       TYPE  VBAP-NETPR,
        AMOUNT        TYPE  VBAP-NETPR,
        AMOUNT_ADV    TYPE  VBAP-NETPR,
        WAERS         TYPE  VBAP-WAERK,
        ADVGP         TYPE  ZSDSDE_ADVGP,
        ADVREC        TYPE  TT_ADVREC_DATA,
      END OF TS_CONF_DATA .
    TYPES:
      TT_CONF_DATA  TYPE  STANDARD TABLE OF TS_CONF_DATA
                                    WITH KEY VBELN
                                             POSNR
                                             ETENR .
    TYPES:
      BEGIN OF TS_ADVGP_ADVREC,
        ADVGP  TYPE  ZSDSDE_ADVGP,
        ADVREC TYPE  TT_ADVREC_DATA,
      END OF TS_ADVGP_ADVREC .
    TYPES:
      TT_ADVGP_ADVREC TYPE SORTED TABLE OF TS_ADVGP_ADVREC
                                  WITH UNIQUE KEY ADVGP .
    TYPES:
      BEGIN OF TS_REMAIN_ADVREC,
        KUNNR        TYPE  KNA1-KUNNR,
        ADVGP_ADVREC TYPE  TT_ADVGP_ADVREC,
      END OF TS_REMAIN_ADVREC .
    TYPES:
      TT_REMAIN_ADVREC  TYPE  SORTED TABLE OF TS_REMAIN_ADVREC
                                                   WITH UNIQUE KEY KUNNR .
    TYPES:
      BEGIN OF TS_APPV_RESULT,
        VBELN TYPE  VBEP-VBELN,
        POSNR TYPE  VBEP-POSNR,
        ETENR TYPE  VBEP-ETENR,
        MSGTY TYPE  MSGTY,
        MSGTX TYPE  TEXT1000,
      END OF TS_APPV_RESULT .
    TYPES:
      TT_APPV_RESULT  TYPE SORTED TABLE OF TS_APPV_RESULT
                                   WITH UNIQUE KEY VBELN
                                                   POSNR
                                                   ETENR .
    TYPES:
      BEGIN OF TS_ADVREC_ASSIGN,
        VBELN TYPE  ZSDSSDT010-VBELN,
        POSNR TYPE  ZSDSSDT010-POSNR,
        ETENR TYPE  ZSDSSDT010-ETENR,
        BUKRS TYPE  ZSDSSDT010-BUKRS,
        BELNR TYPE  ZSDSSDT010-BELNR,
        GJAHR TYPE  ZSDSSDT010-GJAHR,
        DMBTR TYPE  ZSDSSDT010-DMBTR,
        WAERS TYPE  ZSDSSDT010-WAERS,
      END OF TS_ADVREC_ASSIGN .
    TYPES:
      TT_ADVREC_ASSIGN  TYPE  STANDARD TABLE OF TS_ADVREC_ASSIGN
                                     WITH KEY VBELN
                                              POSNR
                                              ETENR
                                              BUKRS
                                              BELNR
                                              GJAHR .

    CONSTANTS GC_MODE_PENDING TYPE CHAR1 VALUE ' ' ##NO_TEXT.
    CONSTANTS GC_MODE_APPROVED TYPE CHAR1 VALUE '1' ##NO_TEXT.
    CONSTANTS GC_MODE_ALL TYPE CHAR1 VALUE '2' ##NO_TEXT.
    CONSTANTS GC_COOPERATOR TYPE CHAR1 VALUE '1' ##NO_TEXT.
    CONSTANTS GC_MANAGER TYPE CHAR1 VALUE '2' ##NO_TEXT.
    CONSTANTS GC_STAT_APPROVE TYPE ZSDSDE_SD_APPV_STAT VALUE 'Y' ##NO_TEXT.
    CONSTANTS GC_STAT_REJECT TYPE ZSDSDE_SD_APPV_STAT VALUE 'N' ##NO_TEXT.

    CLASS-METHODS DETERMINE_ADVGP
      IMPORTING
        !IF_ZTERM       TYPE VBKD-ZTERM
      RETURNING
        VALUE(RF_ADVGP) TYPE ZSDSDE_ADVGP .
    CLASS-METHODS GET_DATA
      IMPORTING
        !IT_SCHEDLINE     TYPE TT_SCHEDLINE OPTIONAL
        !IT_VBELN         TYPE TT_VBELN_RANGE OPTIONAL
        !IT_POSNR         TYPE TT_POSNR_RANGE OPTIONAL
        !IT_ETENR         TYPE TT_ETENR_RANGE OPTIONAL
        !IT_EDATU         TYPE TT_EDATU_RANGE OPTIONAL
        !IT_VKORG         TYPE TT_VKORG_RANGE OPTIONAL
        !IT_VTWEG         TYPE TT_VTWEG_RANGE OPTIONAL
        !IT_KUNNR         TYPE TT_KUNNR_RANGE OPTIONAL
        !IT_VKBUR         TYPE TT_VKBUR_RANGE OPTIONAL
        !IT_VKGRP         TYPE TT_VKGRP_RANGE OPTIONAL
        !IT_AUART         TYPE TT_AUART_RANGE OPTIONAL
        !IT_SPART         TYPE TT_SPART_RANGE OPTIONAL
        !IT_MATNR         TYPE TT_MATNR_RANGE OPTIONAL
        !IF_APPROVER      TYPE CHAR1 DEFAULT '1'
        !IF_MODE          TYPE CHAR1 DEFAULT '2'
        !IF_REMAIN_ADVREC TYPE FLAG DEFAULT ' '
      EXPORTING
        !ET_DATA          TYPE TT_CONF_DATA
        !ET_REMAIN_ADVREC TYPE TT_REMAIN_ADVREC .
    CLASS-METHODS GET_REMAIN_CUST_ADVREC
      IMPORTING
        !IT_KUNNR         TYPE TT_KUNNR_RANGE
      EXPORTING
        !ET_REMAIN_ADVREC TYPE TT_REMAIN_ADVREC .
    CLASS-METHODS PROCESS_APPROVAL
      IMPORTING
        !IT_SCHEDLINE TYPE TT_SCHEDLINE
        !IF_APPROVER  TYPE CHAR1 DEFAULT '1'
        !IF_APPV_STAT TYPE ZSDSDE_SD_APPV_STAT
      EXPORTING
        !ES_RETURN    TYPE BAPIRET1
        !ET_RESULT    TYPE TT_APPV_RESULT .
    CLASS-METHODS CANCEL_APPROVAL
      IMPORTING
        !IT_SCHEDLINE TYPE TT_SCHEDLINE
        !IF_APPROVER  TYPE CHAR1 DEFAULT '1'
      EXPORTING
        !ES_RETURN    TYPE BAPIRET1
        !ET_RESULT    TYPE TT_APPV_RESULT .
    CLASS-METHODS DETERMINE_ADVREC
      IMPORTING
        !IT_SCHEDLINE      TYPE TT_SCHEDLINE
        !IF_AUTO           TYPE FLAG DEFAULT ' '
        !IF_LOCK           TYPE FLAG DEFAULT ' '
      EXPORTING
        !ES_RETURN         TYPE BAPIRET1
        !ET_CONF           TYPE TT_CONF_DATA
        !ET_ADVREC         TYPE TT_ADVREC_DATA
        !EF_DISPLAY_ONLY   TYPE FLAG
        !EF_ASSIGNED_FOUND TYPE FLAG .
    CLASS-METHODS SAVE_ADVREC_ASSIGNMENT
      IMPORTING
        !IT_ADVREC_ASSIGN TYPE TT_ADVREC_ASSIGN
        !IF_COMMIT        TYPE FLAG DEFAULT ' '
      EXPORTING
        !ES_RETURN        TYPE BAPIRET1 .
    CLASS-METHODS LOCK_SO
      IMPORTING
        !IT_SO     TYPE TT_SO
      EXPORTING
        !ES_RETURN TYPE BAPIRET1 .
    CLASS-METHODS UNLOCK_SO .
    CLASS-METHODS MAINTAIN_ADVREC_ASSIGN
      IMPORTING
        !IT_SCHEDLINE TYPE TT_SCHEDLINE
        !IF_AUTO      TYPE FLAG
      EXPORTING
        !ET_CONF      TYPE TT_CONF_DATA
        !ET_ADVREC    TYPE TT_ADVREC_DATA .
protected section.
private section.

  constants GC_MSGTY_SUCCESS type CHAR1 value 'S' ##NO_TEXT.
  constants GC_MSGTY_ERROR type CHAR1 value 'E' ##NO_TEXT.
  constants GC_JOBNAME_PREFIX type TBTCJOB-JOBNAME value 'ZSDSSDR0330' ##NO_TEXT.
  class-data GT_VKORG type TT_VKORG_RANGE .
  class-data GT_LOCKED_SO type TT_SO .
  class-data GT_LOCKED_CUST type TT_CUST .
  class-data GF_BLOCK_STATUS type VBEP-LIFSP value 'Z1' ##NO_TEXT.
  class-data GT_ADVREC_GROUP type TT_ADVREC_GROUP .
  class-data GT_ADVREC_DIFF type ZCL_SDSCA_UTILITIES=>TT_GEN_C .

  class-methods GET_GENC .
  class-methods VALIDATE_FOR_ADVREC
    importing
      !IT_SCHEDLINE type TT_SCHEDLINE
    exporting
      !ET_CONF type TT_CONF_DATA
      !ET_ADVREC type TT_ADVREC_DATA
      !ES_RETURN type BAPIRET1
      !EF_DISPLAY_ONLY type FLAG
      !EF_ASSIGNED_FOUND type FLAG .
  class-methods ASSIGN_RETURN
    importing
      !IF_MSGTY type SY-MSGTY
      !IF_MSGID type SY-MSGID
      !IF_MSGNO type SY-MSGNO
      !IF_MSGV1 type CLIKE optional
      !IF_MSGV2 type CLIKE optional
      !IF_MSGV3 type CLIKE optional
      !IF_MSGV4 type CLIKE optional
    exporting
      !ES_RETURN type BAPIRET1 .
  class-methods EXECUTE_SCHEDLINE_APPR
    importing
      !IS_INFO type TS_APPV_INFO
      !IT_CONF type TT_CONF_DATA
    exporting
      !ES_RETURN type BAPIRET1
      !ET_RESULT type TT_APPV_RESULT .
  class-methods VALIDATE_FOR_APPROVAL
    importing
      !IT_SCHEDLINE type TT_SCHEDLINE
      !IF_APPROVER type CHAR1 default '1'
      !IF_APPV_STAT type ZSDSDE_SD_APPV_STAT
    exporting
      !ET_CONF type TT_CONF_DATA
      !ES_RETURN type BAPIRET1 .
  class-methods VALIDATE_FOR_CANCEL
    importing
      !IT_SCHEDLINE type TT_SCHEDLINE
      !IF_APPROVER type CHAR1 default '1'
    exporting
      !ET_CONF type TT_CONF_DATA
      !ES_RETURN type BAPIRET1 .
  class-methods EXECUTE_CANCEL_APPR
    importing
      !IT_CONF type TT_CONF_DATA
      !IF_APPROVER type CHAR1
    exporting
      !ES_RETURN type BAPIRET1
      !ET_RESULT type TT_APPV_RESULT .
  class-methods INTERFACE_SEND_SCHEDULE_STAT
    importing
      !IT_CONF type TT_CONF_DATA
      !IT_RESULT type TT_APPV_RESULT
      !IF_MODE type CHAR1 .
ENDCLASS.



CLASS ZCL_SDSSD_ORDER_CONFIRMATION IMPLEMENTATION.


METHOD ASSIGN_RETURN.

* Initialize output
  CLEAR ES_RETURN.

  ES_RETURN-TYPE       = IF_MSGTY.
  ES_RETURN-ID         = IF_MSGID.
  ES_RETURN-NUMBER     = IF_MSGNO.
  ES_RETURN-MESSAGE_V1 = IF_MSGV1.
  ES_RETURN-MESSAGE_V2 = IF_MSGV2.
  ES_RETURN-MESSAGE_V3 = IF_MSGV3.
  ES_RETURN-MESSAGE_V4 = IF_MSGV4.
  MESSAGE ID   ES_RETURN-ID
          TYPE ES_RETURN-TYPE
          NUMBER ES_RETURN-NUMBER
          WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
               ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
          INTO ES_RETURN-MESSAGE.

ENDMETHOD.


METHOD CANCEL_APPROVAL.

  DATA:
    LT_SO   TYPE  TT_SO,
    LT_DATA TYPE  TT_CONF_DATA.


* Initialize Output
  CLEAR: ES_RETURN,
         ET_RESULT.

  LT_SO = CORRESPONDING TT_SO( IT_SCHEDLINE ).

* ----------------
* Lock Order
* ----------------
  LOCK_SO(
    EXPORTING
      IT_SO     = LT_SO
    IMPORTING
      ES_RETURN = ES_RETURN ).
  IF ES_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.
  FREE: LT_SO.

* ----------------
* Validate data
* ----------------
  VALIDATE_FOR_CANCEL(
    EXPORTING
      IT_SCHEDLINE = IT_SCHEDLINE
      IF_APPROVER  = IF_APPROVER
    IMPORTING
      ET_CONF      = LT_DATA
      ES_RETURN    = ES_RETURN ).

  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO(  ).
    RETURN.
  ENDIF.

* ----------------
* Execute Cancel Approval
* ----------------
  EXECUTE_CANCEL_APPR(
    EXPORTING
      IT_CONF     = LT_DATA
      IF_APPROVER = IF_APPROVER
    IMPORTING
      ES_RETURN   = ES_RETURN
      ET_RESULT   = ET_RESULT ).
  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO(  ).
    RETURN.
  ENDIF.

* Unlock Data at end of processing
  UNLOCK_SO( ).

*--BOI SDI039 - Send interface manager approve status to Salesforce
* ---------------------
* Send interface approve
* ---------------------
  IF IF_APPROVER = GC_MANAGER.
    INTERFACE_SEND_SCHEDULE_STAT(
      EXPORTING
        IT_CONF   = LT_DATA
        IT_RESULT = ET_RESULT
        IF_MODE   = 'C'  ).  "cancel
  ENDIF.
*--EOD SDI039 - Send interface manager approve status to Salesforce
ENDMETHOD.


METHOD DETERMINE_ADVREC.

  DATA:
    LT_SO     TYPE  TT_SO,
    LT_DATA   TYPE  TT_CONF_DATA,
    LT_ADVREC TYPE  TT_ADVREC_DATA.

  DATA:
    LF_ASSIGN_VAL    TYPE  TS_CONF_DATA-AMOUNT,
    LF_AUTO_ASSIGNED TYPE  FLAG,
    LF_ADVREC_DIFF   TYPE  WERT1,
    LF_AMOUNT_DIFF   TYPE  WERT1.


* Initialize Output
  CLEAR: ES_RETURN,
         ET_CONF,
         ET_ADVREC,
         EF_DISPLAY_ONLY,
         EF_ASSIGNED_FOUND.

  IF IF_LOCK EQ 'X'.
    LT_SO = CORRESPONDING TT_SO( IT_SCHEDLINE ).

*   ----------------
*   Lock Order
*   ----------------
    LOCK_SO(
      EXPORTING
        IT_SO     = LT_SO
      IMPORTING
        ES_RETURN = ES_RETURN ).
    IF ES_RETURN IS NOT INITIAL.
      RETURN.
    ENDIF.
    FREE: LT_SO.
  ENDIF.

* ----------------
* Validate data
* ----------------
  VALIDATE_FOR_ADVREC(
    EXPORTING
      IT_SCHEDLINE      = IT_SCHEDLINE
    IMPORTING
      ET_CONF           = LT_DATA
      ET_ADVREC         = LT_ADVREC
      ES_RETURN         = ES_RETURN
      EF_DISPLAY_ONLY   = EF_DISPLAY_ONLY
      EF_ASSIGNED_FOUND = EF_ASSIGNED_FOUND ).
  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO(  ).
    RETURN.
  ENDIF.

* Unlock Data in display only mode
  IF EF_DISPLAY_ONLY EQ 'X'.
    UNLOCK_SO(  ).
  ELSE.

*   ----------------
*   Auto Assignment
*   ----------------
*    IF IF_AUTO EQ 'X'.      "Del- 420000395
      CLEAR LF_AUTO_ASSIGNED.
*     For all un-assign
      LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>)
                      WHERE AMOUNT GT 0.

*Add+ 420000395 >>>
        CLEAR: LF_ADVREC_DIFF,LF_AMOUNT_DIFF.
        READ TABLE GT_ADVREC_DIFF ASSIGNING FIELD-SYMBOL(<L_GENC>)
                                    WITH KEY PARAM_EXT = <L_DATA>-ADVGP
                                    BINARY SEARCH.
        IF SY-SUBRC = 0.
          LF_ADVREC_DIFF = <L_GENC>-VALUE_LOW.
          LF_AMOUNT_DIFF = <L_DATA>-AMOUNT - <L_DATA>-AMOUNT_ADV.
        ENDIF.
*<<<Add+ 420000395

*       Already Assigned
*Del- 420000395 >>>
*        IF <L_DATA>-AMOUNT EQ <L_DATA>-AMOUNT_ADV.
*          CONTINUE.
*        ENDIF.
*<<<Del-  420000395
*Add+ 420000395 >>>
        IF LF_AMOUNT_DIFF BETWEEN 0 AND LF_ADVREC_DIFF.
          CONTINUE.
        ENDIF.
*<<<Add+ 420000395

*       Assign Value
        LF_ASSIGN_VAL = <L_DATA>-AMOUNT - <L_DATA>-AMOUNT_ADV.

*       Until Totally Assigned
        LOOP AT LT_ADVREC ASSIGNING FIELD-SYMBOL(<L_ADVREC>).

*         Determine assign amount
          IF LF_ASSIGN_VAL LE <L_ADVREC>-DMBTR.
            DATA(LF_DMBTR) = LF_ASSIGN_VAL.
          ELSE.
            LF_DMBTR = <L_ADVREC>-DMBTR.
          ENDIF.

*         Calc value left to be assigned
          LF_ASSIGN_VAL       = LF_ASSIGN_VAL - LF_DMBTR.

*         Add ADV rec assigned
          <L_DATA>-AMOUNT_ADV = <L_DATA>-AMOUNT_ADV + LF_DMBTR.
          INSERT VALUE #( BUKRS = <L_ADVREC>-BUKRS
                          UMSKZ = <L_ADVREC>-UMSKZ
                          BELNR = <L_ADVREC>-BELNR
                          GJAHR = <L_ADVREC>-GJAHR
                          BUDAT = <L_ADVREC>-BUDAT
                          DMBTR = LF_DMBTR
                          WAERS = <L_ADVREC>-WAERS )
                  INTO TABLE <L_DATA>-ADVREC.

*         Deduct ADV Receive assigned
          <L_ADVREC>-DMBTR = <L_ADVREC>-DMBTR - LF_DMBTR.
*         Total assigned, remove entry
          IF <L_ADVREC>-DMBTR EQ 0.
            DELETE LT_ADVREC.
          ENDIF.

*         All assigned?
          IF LF_ASSIGN_VAL LE 0.
            EXIT.
          ENDIF.

        ENDLOOP.
        IF SY-SUBRC EQ 0.
          LF_AUTO_ASSIGNED = 'X'.
        ENDIF.

      ENDLOOP.

      IF LF_AUTO_ASSIGNED EQ 'X'.
*       Message: Automatically determine Advance receive amount processed.
        MESSAGE I019(ZSDSSD01) DISPLAY LIKE 'S'.
      ENDIF.

*    ENDIF.

  ENDIF.

* ----------------
* Assign result
* ----------------
  IF ET_CONF IS SUPPLIED.
    ET_CONF = LT_DATA.
  ENDIF.

  IF ET_ADVREC IS SUPPLIED.
    ET_ADVREC = LT_ADVREC.
  ENDIF.

ENDMETHOD.


METHOD EXECUTE_CANCEL_APPR.

  DATA:
    LT_SAVE       TYPE STANDARD TABLE OF ZSDSSDT009,
    LT_ZSDSSDT009 TYPE STANDARD TABLE OF ZSDSSDT009,
    LT_SCHED      TYPE STANDARD TABLE OF BAPISCHDL,
    LT_SCHEDX     TYPE STANDARD TABLE OF BAPISCHDLX,
    LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2,
    LT_RESULT     TYPE TT_APPV_RESULT.

  DATA:
    LS_HEADX TYPE  BAPISDH1X.

  DATA:
    LF_TEST  TYPE  BAPIFLAG-BAPIFLAG,
    LF_ERROR TYPE  FLAG,
    LF_MSGTX TYPE  TS_APPV_RESULT-MSGTX,
    LF_MSGTY TYPE  TS_APPV_RESULT-MSGTY.


* Initialize Output
  CLEAR: ES_RETURN,
         ET_RESULT.

* Read Existing Status
  SELECT X~VBELN,
         X~POSNR,
         X~ETENR,
         A~CO_APPV_STAT,
         A~CO_APPV_BY,
         A~CO_APPV_DATE,
         A~CO_APPV_TIME,
         A~MGR_APPV_STAT,
         A~MGR_APPV_BY,
         A~MGR_APPV_DATE,
         A~MGR_APPV_TIME
    FROM @IT_CONF AS X ##ITAB_KEY_IN_SELECT
           LEFT OUTER JOIN ZSDSSDT009 AS A
             ON  A~VBELN = X~VBELN
             AND A~POSNR = X~POSNR
             AND A~ETENR = X~ETENR
    ORDER BY X~VBELN ASCENDING,
             X~POSNR ASCENDING,
             X~ETENR ASCENDING
    INTO CORRESPONDING FIELDS OF TABLE @LT_ZSDSSDT009 ##TOO_MANY_ITAB_FIELDS.
  IF SY-SUBRC NE 0.
    CLEAR LT_ZSDSSDT009.
  ENDIF.

* Group Data by Sales Order
  LOOP AT IT_CONF INTO DATA(LS_CONF) ##INTO_OK
                  GROUP BY ( VBELN = LS_CONF-VBELN ) ASCENDING
                  ASSIGNING FIELD-SYMBOL(<L_GROUP>).

    CLEAR: LT_SAVE,
           LS_HEADX,
           LT_SCHED,
           LT_SCHEDX,
           LT_RETURN,
           LF_ERROR,
           LT_RESULT.

*   --------------------------
*   Collect Changing Data
*   --------------------------
    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_CONF>).

*     Collect Changing Schedule line status
      INSERT VALUE #( ITM_NUMBER = <L_CONF>-POSNR
                      SCHED_LINE = <L_CONF>-ETENR
                      REQ_DLV_BL = GF_BLOCK_STATUS
                     )
               INTO TABLE LT_SCHED.
      INSERT VALUE #( ITM_NUMBER = <L_CONF>-POSNR
                      SCHED_LINE = <L_CONF>-ETENR
                      UPDATEFLAG = 'U'
                      REQ_DLV_BL = 'X'
                     )
               INTO TABLE LT_SCHEDX.

*     Collect changing Status in ZSDSSDT009
      READ TABLE LT_ZSDSSDT009 INTO DATA(LS_SAVE)
                               WITH KEY VBELN = <L_CONF>-VBELN
                                        POSNR = <L_CONF>-POSNR
                                        ETENR = <L_CONF>-ETENR
                               BINARY SEARCH.
      IF SY-SUBRC NE 0.
        CLEAR LS_SAVE.
        LS_SAVE-VBELN = <L_CONF>-VBELN.
        LS_SAVE-POSNR = <L_CONF>-POSNR.
        LS_SAVE-ETENR = <L_CONF>-ETENR.
      ENDIF.

      CASE IF_APPROVER.
        WHEN GC_COOPERATOR.
          CLEAR: LS_SAVE-CO_APPV_STAT,
                 LS_SAVE-CO_APPV_BY,
                 LS_SAVE-CO_APPV_DATE,
                 LS_SAVE-CO_APPV_TIME,
                 LS_SAVE-MGR_APPV_STAT,
                 LS_SAVE-MGR_APPV_BY,
                 LS_SAVE-MGR_APPV_DATE,
                 LS_SAVE-MGR_APPV_TIME.
        WHEN GC_MANAGER.
          CLEAR: LS_SAVE-MGR_APPV_STAT,
                 LS_SAVE-MGR_APPV_BY,
                 LS_SAVE-MGR_APPV_DATE,
                 LS_SAVE-MGR_APPV_TIME.
      ENDCASE.

      INSERT LS_SAVE INTO TABLE LT_SAVE.

*     Collect Result Key
      INSERT VALUE #( VBELN = <L_CONF>-VBELN
                      POSNR = <L_CONF>-POSNR
                      ETENR = <L_CONF>-ETENR )
             INTO TABLE LT_RESULT.

    ENDLOOP.

*   --------------------------
*   Updating Approval Status
*   --------------------------
    CLEAR: LF_ERROR,
           LF_MSGTX,
           LF_MSGTY.
    DO 1 TIMES.

*     Update status into ZSDSSDT009
      MODIFY ZSDSSDT009 FROM TABLE LT_SAVE.
      IF SY-SUBRC NE 0.
*       Text-e01: Error during updating status into table ZSDSSDT009.
        LF_MSGTX = TEXT-E01.
        LF_ERROR = 'X'.
        EXIT.
      ENDIF.

*     In case of Manager Approved, re-lock delivery block in schedline
      IF IF_APPROVER EQ GC_MANAGER.

        LS_HEADX-UPDATEFLAG = 'U'.

*       Update Delivery Block in Schedule line
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            SALESDOCUMENT    = <L_GROUP>-VBELN
            ORDER_HEADER_INX = LS_HEADX
            SIMULATION       = LF_TEST
          TABLES
            RETURN           = LT_RETURN
            SCHEDULE_LINES   = LT_SCHED
            SCHEDULE_LINESX  = LT_SCHEDX.

*       Check Result
        LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                          WHERE (  TYPE EQ 'A' OR
                                   TYPE EQ 'X' OR
                                   TYPE EQ 'E'  ).
          LF_ERROR = 'X'.
          LF_MSGTX = <L_RETURN>-MESSAGE.
          EXIT.
        ENDLOOP.

      ENDIF.

    ENDDO.

    IF LF_ERROR IS INITIAL.
      LF_MSGTY = GC_MSGTY_SUCCESS.
*     Text-i02: Cancellation processed successfully.
      LF_MSGTX = TEXT-I02.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      LF_MSGTY = GC_MSGTY_ERROR.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

*   Update Result Message
    MODIFY LT_RESULT FROM VALUE #( MSGTY = LF_MSGTY
                                   MSGTX = LF_MSGTX )
                     TRANSPORTING MSGTY MSGTX
                     WHERE MSGTX IS INITIAL.            "#EC CI_SORTSEQ

*   Assign Result
    INSERT LINES OF LT_RESULT INTO TABLE ET_RESULT.

  ENDLOOP.

ENDMETHOD.


METHOD EXECUTE_SCHEDLINE_APPR.

  DATA:
    LT_SAVE       TYPE STANDARD TABLE OF ZSDSSDT009,
    LT_ZSDSSDT009 TYPE STANDARD TABLE OF ZSDSSDT009,
    LT_SCHED      TYPE STANDARD TABLE OF BAPISCHDL,
    LT_SCHEDX     TYPE STANDARD TABLE OF BAPISCHDLX,
    LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2,
    LT_RESULT     TYPE TT_APPV_RESULT.

  DATA:
    LS_HEADX TYPE  BAPISDH1X.

  DATA:
    LF_TEST  TYPE  BAPIFLAG-BAPIFLAG,
    LF_ERROR TYPE  FLAG,
    LF_MSGTX TYPE  TS_APPV_RESULT-MSGTX,
    LF_MSGTY TYPE  TS_APPV_RESULT-MSGTY.


* Initialize Output
  CLEAR: ES_RETURN,
         ET_RESULT.

* Read Existing Status
  SELECT X~VBELN,
         X~POSNR,
         X~ETENR,
         A~CO_APPV_STAT,
         A~CO_APPV_BY,
         A~CO_APPV_DATE,
         A~CO_APPV_TIME,
         A~MGR_APPV_STAT,
         A~MGR_APPV_BY,
         A~MGR_APPV_DATE,
         A~MGR_APPV_TIME
    FROM @IT_CONF AS X ##ITAB_KEY_IN_SELECT
           LEFT OUTER JOIN ZSDSSDT009 AS A
             ON  A~VBELN = X~VBELN
             AND A~POSNR = X~POSNR
             AND A~ETENR = X~ETENR
    ORDER BY X~VBELN ASCENDING,
             X~POSNR ASCENDING,
             X~ETENR ASCENDING
    INTO CORRESPONDING FIELDS OF TABLE @LT_ZSDSSDT009 ##TOO_MANY_ITAB_FIELDS.
  IF SY-SUBRC NE 0.
    CLEAR LT_ZSDSSDT009.
  ENDIF.

* Group Data by Sales Order
  LOOP AT IT_CONF INTO DATA(LS_CONF) ##INTO_OK
                  GROUP BY ( VBELN = LS_CONF-VBELN ) ASCENDING
                  ASSIGNING FIELD-SYMBOL(<L_GROUP>).

    CLEAR: LT_SAVE,
           LS_HEADX,
           LT_SCHED,
           LT_SCHEDX,
           LT_RETURN,
           LF_ERROR,
           LT_RESULT.

*   --------------------------
*   Collect Changing Data
*   --------------------------
    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_CONF>).

*     Collect Changing Schedule line status
      INSERT VALUE #( ITM_NUMBER = <L_CONF>-POSNR
                      SCHED_LINE = <L_CONF>-ETENR
                      REQ_DLV_BL = SPACE
                     )
               INTO TABLE LT_SCHED.
      INSERT VALUE #( ITM_NUMBER = <L_CONF>-POSNR
                      SCHED_LINE = <L_CONF>-ETENR
                      UPDATEFLAG = 'U'
                      REQ_DLV_BL = 'X'
                     )
               INTO TABLE LT_SCHEDX.

*     Collect changing Status in ZSDSSDT009
      READ TABLE LT_ZSDSSDT009 INTO DATA(LS_SAVE)
                               WITH KEY VBELN = <L_CONF>-VBELN
                                        POSNR = <L_CONF>-POSNR
                                        ETENR = <L_CONF>-ETENR
                               BINARY SEARCH.
      IF SY-SUBRC NE 0.
        CLEAR LS_SAVE.
        LS_SAVE-VBELN = <L_CONF>-VBELN.
        LS_SAVE-POSNR = <L_CONF>-POSNR.
        LS_SAVE-ETENR = <L_CONF>-ETENR.
      ENDIF.

      CASE IS_INFO-APPROVER.
        WHEN GC_COOPERATOR.
          LS_SAVE-CO_APPV_STAT  = IS_INFO-APPV_STAT.
          LS_SAVE-CO_APPV_BY    = IS_INFO-UNAME.
          LS_SAVE-CO_APPV_DATE  = IS_INFO-DATUM.
          LS_SAVE-CO_APPV_TIME  = IS_INFO-UZEIT.
        WHEN GC_MANAGER.
          LS_SAVE-MGR_APPV_STAT = IS_INFO-APPV_STAT.
          LS_SAVE-MGR_APPV_BY   = IS_INFO-UNAME.
          LS_SAVE-MGR_APPV_DATE = IS_INFO-DATUM.
          LS_SAVE-MGR_APPV_TIME = IS_INFO-UZEIT.
      ENDCASE.

      INSERT LS_SAVE INTO TABLE LT_SAVE.

*     Collect Result Key
      INSERT VALUE #( VBELN = <L_CONF>-VBELN
                      POSNR = <L_CONF>-POSNR
                      ETENR = <L_CONF>-ETENR )
             INTO TABLE LT_RESULT.

    ENDLOOP.

*   --------------------------
*   Updating Approval Status
*   --------------------------
    CLEAR: LF_ERROR,
           LF_MSGTX,
           LF_MSGTY.
    DO 1 TIMES.

*     Update status into ZSDSSDT009
      MODIFY ZSDSSDT009 FROM TABLE LT_SAVE.
      IF SY-SUBRC NE 0.
*       Text-e01: Error during updating status into table ZSDSSDT009.
        LF_MSGTX = TEXT-E01.
        LF_ERROR = 'X'.
        EXIT.
      ENDIF.

*     In case of Manager Approved, unlock delivery block in schedline
      IF IS_INFO-APPROVER EQ GC_MANAGER AND
         IS_INFO-APPV_STAT EQ GC_STAT_APPROVE.

        LS_HEADX-UPDATEFLAG = 'U'.

*       Update Delivery Block in Schedule line
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            SALESDOCUMENT    = <L_GROUP>-VBELN
            ORDER_HEADER_INX = LS_HEADX
            SIMULATION       = LF_TEST
          TABLES
            RETURN           = LT_RETURN
            SCHEDULE_LINES   = LT_SCHED
            SCHEDULE_LINESX  = LT_SCHEDX.

*       Check Result
        LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                          WHERE (  TYPE EQ 'A' OR
                                   TYPE EQ 'X' OR
                                   TYPE EQ 'E'  ).
          LF_ERROR = 'X'.
          LF_MSGTX = <L_RETURN>-MESSAGE.
          EXIT.
        ENDLOOP.

      ENDIF.

    ENDDO.

    IF LF_ERROR IS INITIAL.
      LF_MSGTY = GC_MSGTY_SUCCESS.
*     Text-i01: Approval processed successfully.
      LF_MSGTX = TEXT-I01.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      LF_MSGTY = GC_MSGTY_ERROR.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

*   Update Result Message
    MODIFY LT_RESULT FROM VALUE #( MSGTY = LF_MSGTY
                                   MSGTX = LF_MSGTX )
                     TRANSPORTING MSGTY MSGTX
                     WHERE MSGTX IS INITIAL.            "#EC CI_SORTSEQ

*   Assign Result
    INSERT LINES OF LT_RESULT INTO TABLE ET_RESULT.

  ENDLOOP.

ENDMETHOD.


METHOD GET_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_ORDER_CONFIRMATION
*  Creation Date      : 24.06.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : SDE011
*  Description        : This is method to get order confirmation data
*  Purpose            : To approve/unlock sales order schedule lines
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  TYPES: BEGIN OF TS_AUART,
           AUART TYPE  VBAK-AUART,
         END OF TS_AUART.
  TYPES: TT_AUART TYPE SORTED TABLE OF TS_AUART
                       WITH UNIQUE KEY AUART.

  DATA:
    LT_LIFSP    TYPE  RANGE OF VBEP-LIFSP,
    LT_CO_STAT  TYPE  RANGE OF ZSDSSDT009-CO_APPV_STAT         ##NEEDED,
    LT_MGR_STAT TYPE  RANGE OF ZSDSSDT009-MGR_APPV_STAT        ##NEEDED.

  DATA:
    LS_DATA   TYPE  TS_CONF_DATA,
    LS_ADVREC TYPE  TS_ADVREC_DATA.

  DATA:
    LF_COND   TYPE  STRING,
    LF_ACTVT  TYPE  ACTIV_AUTH VALUE '02',
    LF_NATION TYPE  ADRC-NATION VALUE ' ',
    LF_AMOUNT TYPE  TS_CONF_DATA-AMOUNT,
    LF_DIFF   TYPE  TS_CONF_DATA-AMOUNT.


* Initialize Output
  CLEAR: ET_DATA,
         ET_REMAIN_ADVREC.

* Assign Status Conditions
  CLEAR: LF_COND,
         LT_LIFSP,
         LT_CO_STAT,
         LT_MGR_STAT.
  CASE IF_APPROVER.
*   -----------------------
*   CO Operator approval
*   -----------------------
    WHEN GC_COOPERATOR.
      CASE IF_MODE.
        WHEN GC_MODE_PENDING.
          LF_COND = '( D~CO_APPV_STAT IS NULL OR D~CO_APPV_STAT IS INITIAL )'.
          APPEND VALUE #( SIGN = 'E'
                          OPTION = 'EQ'
                          LOW = ' ' )
                  TO LT_LIFSP.
        WHEN GC_MODE_APPROVED.
          LF_COND = 'D~CO_APPV_STAT IS NOT INITIAL AND D~MGR_APPV_STAT IS INITIAL'.
        WHEN GC_MODE_ALL.
          LF_COND = 'D~CO_APPV_STAT IN @LT_CO_STAT AND ( D~MGR_APPV_STAT IS NULL OR D~MGR_APPV_STAT IS INITIAL )'.
      ENDCASE.

*   -----------------------
*   Manager approval
*   -----------------------
    WHEN GC_MANAGER.
*     Only Already approved by CO Operator
      LF_COND = 'D~CO_APPV_STAT IN @LT_CO_STAT AND D~MGR_APPV_STAT IN @LT_MGR_STAT'.
      CASE IF_MODE.
        WHEN GC_MODE_PENDING.
          APPEND VALUE #( SIGN = 'I'
                          OPTION = 'EQ'
                          LOW    = 'Y' )
                 TO LT_CO_STAT.
          APPEND VALUE #( SIGN = 'I'
                          OPTION = 'EQ'
                          LOW    = ' ' )
                 TO LT_MGR_STAT.
          APPEND VALUE #( SIGN = 'E'
                          OPTION = 'EQ'
                          LOW = ' ' )
                  TO LT_LIFSP.
        WHEN GC_MODE_APPROVED.
          APPEND VALUE #( SIGN = 'I'
                          OPTION = 'EQ'
                          LOW    = 'Y' )
                 TO LT_CO_STAT.
          APPEND VALUE #( SIGN = 'E'
                          OPTION = 'EQ'
                          LOW    = ' ' )
                 TO LT_MGR_STAT.
        WHEN GC_MODE_ALL.
          CLEAR: LT_MGR_STAT.
      ENDCASE.

  ENDCASE.

* --------------------------
* 2 Cases of Selection
* - Specified Schedline key
* - Selection Criteria
* --------------------------
  IF IT_SCHEDLINE IS SUPPLIED AND
     IT_SCHEDLINE IS NOT INITIAL.
    SELECT A~VBELN,
           A~POSNR,
           A~ETENR,
           A~EDATU,
           A~BMENG,
           A~VRKME,
           A~LIFSP,
           B~KUNNR,
           CONCAT_WITH_SPACE( L~NAME1,L~NAME2,1 ) AS KUNNR_NAME,
           B~VTWEG,
           E~VTEXT AS VTWEG_TXT,
           B~AUART,
           I~BEZEI AS AUART_TXT,
           B~VKBUR,
           G~BEZEI AS VKBUR_TXT,
           B~VKGRP,
           H~BEZEI AS VKGRP_TXT,
           B~SPART,
           F~VTEXT AS SPART_TXT,
           M~ZTERM AS ZTERM_I,
           N~ZTERM AS ZTERM_H,
           C~MATNR,
           C~ARKTX,
           D~CO_APPV_STAT,
           D~CO_APPV_BY,
           D~CO_APPV_DATE,
           D~CO_APPV_TIME,
           D~MGR_APPV_STAT,
           D~MGR_APPV_BY,
           D~MGR_APPV_DATE,
           D~MGR_APPV_TIME,
*          Price Detail
           C~UEPOS,
           C~NETWR,
           C~WAERK AS WAERS,
           C~KBMENG,
           C~KWMENG,
           C~NETPR,
           C~KPEIN,
           C~KMEIN,
           C~CMPRE,
           C~MWSBP
      FROM @IT_SCHEDLINE AS X
            INNER JOIN VBEP AS A
              ON  A~VBELN = X~VBELN
              AND A~POSNR = X~POSNR
              AND A~ETENR = X~ETENR
            INNER JOIN VBAK AS B
              ON  B~VBELN = A~VBELN
            INNER JOIN VBAP AS C
              ON  C~VBELN = A~VBELN
              AND C~POSNR = A~POSNR
*            INNER JOIN KNA1 AS J
*              ON  J~KUNNR = B~KUNNR
              INNER JOIN VBPA AS J
              ON  A~VBELN = J~VBELN
              AND J~PARVW = 'AG'
            INNER JOIN ADRC AS L
              ON  L~ADDRNUMBER = J~ADRNR
              AND L~NATION     = @LF_NATION
            LEFT OUTER JOIN ZSDSSDT009 AS D
              ON  D~VBELN = A~VBELN
              AND D~POSNR = A~POSNR
              AND D~ETENR = A~ETENR
            LEFT OUTER JOIN TVTWT AS E
              ON  E~VTWEG = B~VTWEG
              AND E~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TSPAT AS F
              ON  F~SPART = B~SPART
              AND F~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TVKBT AS G
              ON  G~VKBUR = B~VKBUR
              AND G~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TVGRT AS H
              ON  H~VKGRP = B~VKGRP
              AND H~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TVAKT AS I                 "#EC CI_BUFFJOIN
              ON  I~AUART = B~AUART
              AND I~SPRAS = @SY-LANGU
            LEFT OUTER JOIN VBKD AS M
              ON  M~VBELN = C~VBELN
              AND M~POSNR = C~POSNR
            LEFT OUTER JOIN VBKD AS N
              ON  N~VBELN = C~VBELN
              AND N~POSNR = '000000'
     WHERE B~VKORG IN @GT_VKORG
       AND L~DATE_FROM LE @SY-DATUM
       AND L~DATE_TO   GE @SY-DATUM
       AND (LF_COND)
      ORDER BY A~VBELN ASCENDING,
               A~POSNR ASCENDING,
               A~ETENR ASCENDING
      INTO TABLE @DATA(LT_SCHED).
  ELSE.
    SELECT A~VBELN,
           A~POSNR,
           A~ETENR,
           A~EDATU,
           A~BMENG,
           A~VRKME,
           A~LIFSP,
           B~KUNNR,
           CONCAT_WITH_SPACE( L~NAME1,L~NAME2,1 ) AS KUNNR_NAME,
           B~VTWEG,
           E~VTEXT AS VTWEG_TXT,
           B~AUART,
           I~BEZEI AS AUART_TXT,
           B~VKBUR,
           G~BEZEI AS VKBUR_TXT,
           B~VKGRP,
           H~BEZEI AS VKGRP_TXT,
           B~SPART,
           F~VTEXT AS SPART_TXT,
           M~ZTERM AS ZTERM_I,
           N~ZTERM AS ZTERM_H,
           C~MATNR,
           C~ARKTX,
           D~CO_APPV_STAT,
           D~CO_APPV_BY,
           D~CO_APPV_DATE,
           D~CO_APPV_TIME,
           D~MGR_APPV_STAT,
           D~MGR_APPV_BY,
           D~MGR_APPV_DATE,
           D~MGR_APPV_TIME,
*          Price Detail
           C~UEPOS,
           C~NETWR,
           C~WAERK AS WAERS,
           C~KBMENG,
           C~KWMENG,
           C~NETPR,
           C~KPEIN,
           C~KMEIN,
           C~CMPRE,
           C~MWSBP
      FROM VBEP AS A
            INNER JOIN VBAK AS B
              ON  B~VBELN = A~VBELN
            INNER JOIN VBAP AS C
              ON  C~VBELN = A~VBELN
              AND C~POSNR = A~POSNR
*            INNER JOIN KNA1 AS J
*              ON  J~KUNNR = B~KUNNR
              INNER JOIN VBPA AS J
              ON  A~VBELN = J~VBELN
              AND J~PARVW = 'AG'
            INNER JOIN ADRC AS L
              ON  L~ADDRNUMBER = J~ADRNR
              AND L~NATION     = @LF_NATION
            LEFT OUTER JOIN ZSDSSDT009 AS D
              ON  D~VBELN = A~VBELN
              AND D~POSNR = A~POSNR
              AND D~ETENR = A~ETENR
            LEFT OUTER JOIN TVTWT AS E
              ON  E~VTWEG = B~VTWEG
              AND E~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TSPAT AS F
              ON  F~SPART = B~SPART
              AND F~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TVKBT AS G
              ON  G~VKBUR = B~VKBUR
              AND G~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TVGRT AS H
              ON  H~VKGRP = B~VKGRP
              AND H~SPRAS = @SY-LANGU
            LEFT OUTER JOIN TVAKT AS I                 "#EC CI_BUFFJOIN
              ON  I~AUART = B~AUART
              AND I~SPRAS = @SY-LANGU
            LEFT OUTER JOIN VBKD AS M
              ON  M~VBELN = C~VBELN
              AND M~POSNR = C~POSNR
            LEFT OUTER JOIN VBKD AS N
              ON  N~VBELN = C~VBELN
              AND N~POSNR = '000000'
     WHERE A~VBELN IN @IT_VBELN
       AND A~POSNR IN @IT_POSNR
       AND A~ETENR IN @IT_ETENR
       AND A~EDATU IN @IT_EDATU
       AND A~BMENG NE 0
       AND A~LIFSP IN @LT_LIFSP
       AND B~VKORG IN @GT_VKORG
       AND B~VTWEG IN @IT_VTWEG
       AND B~KUNNR IN @IT_KUNNR
       AND B~VKBUR IN @IT_VKBUR
       AND B~VKGRP IN @IT_VKGRP
       AND B~AUART IN @IT_AUART
       AND B~SPART IN @IT_SPART
       AND C~MATNR IN @IT_MATNR
       AND L~DATE_FROM LE @SY-DATUM
       AND L~DATE_TO   GE @SY-DATUM
       AND (LF_COND)
      ORDER BY A~VBELN ASCENDING,
               A~POSNR ASCENDING,
               A~ETENR ASCENDING
      INTO TABLE @LT_SCHED.
  ENDIF.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Filtering Out by Authorization
  DATA(LT_AUART) = CORRESPONDING TT_AUART( LT_SCHED DISCARDING DUPLICATES
                                     MAPPING AUART = AUART ).
  LOOP AT LT_AUART ASSIGNING FIELD-SYMBOL(<L_AUART>).
    AUTHORITY-CHECK OBJECT 'V_VBAK_AAT'
     ID 'AUART' FIELD <L_AUART>-AUART
     ID 'ACTVT' FIELD LF_ACTVT.
    IF SY-SUBRC <> 0.
      DELETE LT_SCHED WHERE AUART EQ <L_AUART>-AUART.
    ENDIF.
  ENDLOOP.

* -------------------------------
* Get Advance Receive Assigned
* -------------------------------
  SELECT A~VBELN,
         A~POSNR,
         A~ETENR,
         A~BUKRS,
         A~BELNR,
         A~GJAHR,
         B~BUDAT,
         A~DMBTR,
         A~WAERS
    FROM @LT_SCHED AS X  ##ITAB_KEY_IN_SELECT
           INNER JOIN ZSDSSDT010 AS A
             ON  A~VBELN = X~VBELN
             AND A~POSNR = X~POSNR
             AND A~ETENR = X~ETENR
           LEFT OUTER JOIN BKPF AS B
             ON  B~BUKRS = A~BUKRS
             AND B~BELNR = A~BELNR
             AND B~GJAHR = A~GJAHR
    ORDER BY A~VBELN ASCENDING,
             A~POSNR ASCENDING,
             A~ETENR ASCENDING,
             A~BUKRS ASCENDING,
             A~BELNR ASCENDING,
             A~GJAHR ASCENDING
    INTO TABLE @DATA(LT_ADV).
  IF SY-SUBRC NE 0.
    CLEAR LT_ADV.
  ENDIF.

* -------------------------------
* Assign result
* -------------------------------
  LOOP AT LT_SCHED ASSIGNING FIELD-SYMBOL(<L_SCHED>).
    CLEAR LS_DATA.
    MOVE-CORRESPONDING <L_SCHED> TO LS_DATA.
    IF <L_SCHED>-ZTERM_I IS NOT INITIAL.
      LS_DATA-ZTERM = <L_SCHED>-ZTERM_I.
    ELSE.
      LS_DATA-ZTERM = <L_SCHED>-ZTERM_H.
    ENDIF.
    LS_DATA-ADVGP = DETERMINE_ADVGP( LS_DATA-ZTERM ).

*   Calculate item amount
    IF <L_SCHED>-BMENG EQ <L_SCHED>-KWMENG.
      LS_DATA-AMOUNT  = <L_SCHED>-NETWR + <L_SCHED>-MWSBP.
    ELSE.
      LS_DATA-AMOUNT  = <L_SCHED>-CMPRE * <L_SCHED>-BMENG.
*     Adjust Amount from rounding
      SELECT VBELN,
             POSNR,
             ETENR,
             BMENG
        FROM VBEP
       WHERE VBELN EQ @<L_SCHED>-VBELN
         AND POSNR EQ @<L_SCHED>-POSNR
       ORDER BY EDATU DESCENDING,
                ETENR DESCENDING
        INTO TABLE @DATA(LT_VBEP).
      IF SY-SUBRC NE 0.
        CLEAR LT_VBEP.
      ENDIF.
*     Only for last Schedule line
      READ TABLE LT_VBEP ASSIGNING FIELD-SYMBOL(<L_VBEP>)
                         INDEX 1.
      IF SY-SUBRC EQ 0 AND
         <L_VBEP>-ETENR EQ <L_SCHED>-ETENR.
        CLEAR LF_AMOUNT.
        LOOP AT LT_VBEP ASSIGNING <L_VBEP>.
          LF_AMOUNT = LF_AMOUNT + ( <L_VBEP>-BMENG * <L_SCHED>-CMPRE ).
        ENDLOOP.
*       Calc diff
        LF_DIFF = ( <L_SCHED>-NETWR + <L_SCHED>-MWSBP ) - LF_AMOUNT.
*       Add diff
        LS_DATA-AMOUNT  = LS_DATA-AMOUNT + LF_DIFF.
      ENDIF.

    ENDIF.
    IF <L_SCHED>-KWMENG NE 0.
      LS_DATA-AMOUNT0 = ( <L_SCHED>-NETWR / <L_SCHED>-KWMENG ) * <L_SCHED>-BMENG.
    ENDIF.
    LS_DATA-WAERS   = <L_SCHED>-WAERS.

*   Convert Currency here.... If LS_DATA-WAERS is not T001-WAERS

    LOOP AT LT_ADV ASSIGNING FIELD-SYMBOL(<L_ADV>)
                   WHERE VBELN EQ <L_SCHED>-VBELN
                     AND POSNR EQ <L_SCHED>-POSNR
                     AND ETENR EQ <L_SCHED>-ETENR.
      CLEAR LS_ADVREC.
      MOVE-CORRESPONDING <L_ADV> TO LS_ADVREC.
      LS_DATA-AMOUNT_ADV = LS_DATA-AMOUNT_ADV + <L_ADV>-DMBTR.
      INSERT LS_ADVREC INTO TABLE LS_DATA-ADVREC.
    ENDLOOP.
    INSERT LS_DATA INTO TABLE ET_DATA.
  ENDLOOP.

* -------------------------------
* Get Remainging Adv. Recipt for Customers
* -------------------------------
  IF IF_REMAIN_ADVREC EQ 'X'.
*   Get List of Remaining Customer Advance Receive
    DATA(LT_KUNNR) = CORRESPONDING TT_KUNNR_RANGE( ET_DATA
                                                   MAPPING LOW = KUNNR ).
    SORT LT_KUNNR BY LOW ASCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_KUNNR COMPARING LOW.
    MODIFY LT_KUNNR FROM VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = SPACE ) TRANSPORTING SIGN OPTION
                    WHERE LOW IS NOT INITIAL.

*   Read Customer Remaining Advance Receive Amount
    GET_REMAIN_CUST_ADVREC(
      EXPORTING
        IT_KUNNR         = LT_KUNNR
      IMPORTING
        ET_REMAIN_ADVREC = ET_REMAIN_ADVREC ).
  ENDIF.

ENDMETHOD.


METHOD GET_REMAIN_CUST_ADVREC.
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*  19.06.2025  F36K919712  Zulkiff B.   Deposit Amount incorrect
*-----------------------------------------------------------------------

  CONSTANTS:
    LC_BSCHL  TYPE  BSEG-BSCHL VALUE '19'.

  DATA:
    LT_REMAIN TYPE TT_REMAIN_ADVREC.

  DATA:
    LR_UMSKZ  TYPE  RANGE OF BSID_VIEW-UMSKZ.

  DATA:
    LF_BOOK_AMT TYPE  BSEG-DMBTR,
    LF_AMOUNT   TYPE  BSEG-DMBTR.


* Initialize Output
  CLEAR: ET_REMAIN_ADVREC.

* Get Constants
  GET_GENC( ).

* Only when Advance Receive Setting found
  IF GT_ADVREC_GROUP IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR LR_UMSKZ.
  LOOP AT GT_ADVREC_GROUP ASSIGNING FIELD-SYMBOL(<L_ADVREC_GROUP>).
    APPEND LINES OF <L_ADVREC_GROUP>-UMSKZ TO LR_UMSKZ.
  ENDLOOP.
  SORT LR_UMSKZ BY SIGN
                   OPTION
                   LOW
                   HIGH.
  DELETE ADJACENT DUPLICATES FROM LR_UMSKZ COMPARING ALL FIELDS.
  IF LR_UMSKZ IS INITIAL.
    RETURN.
  ENDIF.

* Get Open Advance Receive items
  SELECT A~KUNNR,
         A~BUKRS,
         A~BELNR,
         A~GJAHR,
         A~BUZEI,
         A~UMSKZ,
         A~BSCHL,
         A~ZUONR,
         A~BUDAT,
         A~ZTERM,
         CASE A~SHKZG
           WHEN 'S' THEN CAST( A~DMBTR * -1 AS CURR( 23,2 ) )  ##NUMBER_OK
           WHEN 'H' THEN A~DMBTR
         END AS DMBTR,
         CASE A~SHKZG
           WHEN 'S' THEN CAST( A~MWSTS * -1 AS CURR( 23,2 ) )  ##NUMBER_OK
           WHEN 'H' THEN A~MWSTS
         END AS MWSTS,
         B~WAERS,
         A~XREF2        "Add+ 420000631
    FROM BSID_VIEW AS A
           INNER JOIN T001 AS B                        "#EC CI_BUFFJOIN
             ON  B~BUKRS = A~BUKRS
   WHERE KUNNR IN @IT_KUNNR
     AND UMSKZ IN @LR_UMSKZ
     AND BSCHL EQ @LC_BSCHL
   ORDER BY A~KUNNR ASCENDING,
            A~BUKRS ASCENDING,
            A~BELNR ASCENDING,
            A~GJAHR ASCENDING,
            A~BUZEI ASCENDING
    INTO TABLE @DATA(LT_ADVREC).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Read Amount already booked into schedule line
  SELECT A~VBELN,
         A~POSNR,
         A~ETENR,
         A~BUKRS,
         A~BELNR,
         A~GJAHR,
         A~DMBTR,
         A~WAERS
    FROM @LT_ADVREC AS X
           INNER JOIN ZSDSSDT010 AS A
             ON  A~BUKRS = X~BUKRS
             AND A~BELNR = X~BELNR
             AND A~GJAHR = X~GJAHR
   ORDER BY A~BUKRS ASCENDING,
            A~BELNR ASCENDING,
            A~GJAHR ASCENDING,
            A~VBELN ASCENDING,
            A~POSNR ASCENDING,
            A~ETENR ASCENDING
    INTO TABLE @DATA(LT_MAP_SCHED).
  IF SY-SUBRC NE 0.
    CLEAR LT_MAP_SCHED.
  ENDIF.

*Add+ 420000631 >>>
  SELECT A~VBELN,
         A~POSNR,
         A~ETENR,
         A~BUKRS,
         A~BELNR,
         A~GJAHR,
         A~DMBTR,
         A~WAERS
    FROM @LT_ADVREC AS X
           INNER JOIN ZSDSSDT010 AS A
             ON  A~BUKRS = X~BUKRS
             AND A~BELNR = X~XREF2
             AND A~GJAHR = X~GJAHR
   ORDER BY A~BUKRS ASCENDING,
            A~BELNR ASCENDING,
            A~GJAHR ASCENDING,
            A~VBELN ASCENDING,
            A~POSNR ASCENDING,
            A~ETENR ASCENDING
   INTO TABLE @DATA(LT_MAP_SCHED_S).
  IF SY-SUBRC = 0.
    LOOP AT LT_MAP_SCHED_S INTO DATA(LS_MAP_SCHED_S).
      DATA(LV_TABIX) = SY-TABIX.

      SELECT VBELV, POSNV, VBELN, POSNN, VBTYP_N, ERDAT,ERZET
        FROM VBFA
       WHERE VBELV = @LS_MAP_SCHED_S-VBELN
         AND POSNV = @LS_MAP_SCHED_S-POSNR
         AND VBTYP_N IN ('M','N')
        INTO TABLE @DATA(LT_VBFA).
      IF SY-SUBRC = 0.
        SORT LT_VBFA BY ERDAT DESCENDING ERZET DESCENDING.
        READ TABLE LT_VBFA INTO DATA(LS_VBFA) INDEX 1.
        IF SY-SUBRC = 0.
          IF LS_VBFA-VBTYP_N = 'M'.
            DELETE LT_MAP_SCHED_S INDEX LV_TABIX.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*<<<Add+ 420000631

* Calculate Remaining
  CLEAR LT_REMAIN.
  LOOP AT GT_ADVREC_GROUP ASSIGNING <L_ADVREC_GROUP>.

    LOOP AT LT_ADVREC ASSIGNING FIELD-SYMBOL(<L_ADVREC>)
                      WHERE UMSKZ IN <L_ADVREC_GROUP>-UMSKZ.

      READ TABLE LT_REMAIN ASSIGNING FIELD-SYMBOL(<L_REMAIN>)
                           WITH KEY KUNNR = <L_ADVREC>-KUNNR
                           BINARY SEARCH.
      IF SY-SUBRC NE 0.
        INSERT VALUE #( KUNNR = <L_ADVREC>-KUNNR )
                       INTO TABLE LT_REMAIN
                       ASSIGNING <L_REMAIN>.
      ENDIF.

      READ TABLE <L_REMAIN>-ADVGP_ADVREC ASSIGNING FIELD-SYMBOL(<L_ADVGP_ADVREC>)
                                         WITH KEY ADVGP = <L_ADVREC_GROUP>-ADVGP
                                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
        INSERT VALUE #( ADVGP = <L_ADVREC_GROUP>-ADVGP )
                       INTO TABLE <L_REMAIN>-ADVGP_ADVREC
                       ASSIGNING <L_ADVGP_ADVREC>.
      ENDIF.

      READ TABLE <L_ADVGP_ADVREC>-ADVREC ASSIGNING FIELD-SYMBOL(<L_REMAIN_ADVREC>)
                                         WITH KEY BUKRS = <L_ADVREC>-BUKRS
                                                  BELNR = <L_ADVREC>-BELNR
                                                  GJAHR = <L_ADVREC>-GJAHR
                                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
*       Sum Book amount for Advrec Doc
        CLEAR LF_BOOK_AMT.
        LOOP AT LT_MAP_SCHED ASSIGNING FIELD-SYMBOL(<L_MAP_SCHED>)
                             WHERE BUKRS EQ <L_ADVREC>-BUKRS
                               AND BELNR EQ <L_ADVREC>-BELNR
                               AND GJAHR EQ <L_ADVREC>-GJAHR.
          LF_BOOK_AMT = LF_BOOK_AMT + <L_MAP_SCHED>-DMBTR.
        ENDLOOP.
*Add+ 420000631 >>>
        IF <L_ADVREC>-UMSKZ = 'S' AND <L_ADVREC>-XREF2 <> SPACE.
          LOOP AT LT_MAP_SCHED_S ASSIGNING FIELD-SYMBOL(<L_MAP_SCHED_S>)
                               WHERE BUKRS EQ <L_ADVREC>-BUKRS
                                 AND BELNR EQ <L_ADVREC>-XREF2
                                 AND GJAHR EQ <L_ADVREC>-GJAHR.
            LF_BOOK_AMT = LF_BOOK_AMT + <L_MAP_SCHED_S>-DMBTR.
          ENDLOOP.
        ENDIF.
*<<<Add+ 420000631
        LF_AMOUNT = <L_ADVREC>-DMBTR + <L_ADVREC>-MWSTS - LF_BOOK_AMT.
        INSERT VALUE #( BUKRS = <L_ADVREC>-BUKRS
                        UMSKZ = <L_ADVREC>-UMSKZ
                        BELNR = <L_ADVREC>-BELNR
                        GJAHR = <L_ADVREC>-GJAHR
                        BUDAT = <L_ADVREC>-BUDAT
                        DMBTR = LF_AMOUNT
                        WAERS = <L_ADVREC>-WAERS )
               INTO TABLE <L_ADVGP_ADVREC>-ADVREC.
      ELSE.
        <L_REMAIN_ADVREC>-DMBTR = <L_REMAIN_ADVREC>-DMBTR + <L_ADVREC>-DMBTR + <L_ADVREC>-MWSTS.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

* Final Check
  LOOP AT LT_REMAIN ASSIGNING <L_REMAIN>.

    LOOP AT <L_REMAIN>-ADVGP_ADVREC ASSIGNING <L_ADVGP_ADVREC>.
      DELETE <L_ADVGP_ADVREC>-ADVREC WHERE DMBTR LE 0.
      IF <L_ADVGP_ADVREC>-ADVREC IS INITIAL.
        DELETE <L_REMAIN>-ADVGP_ADVREC.
        CONTINUE.
      ENDIF.
*     Sort for FIFO Deduction
      SORT <L_ADVGP_ADVREC>-ADVREC BY BUKRS ASCENDING
                                      BUDAT ASCENDING
                                      GJAHR ASCENDING
                                      BELNR ASCENDING.
    ENDLOOP.
  ENDLOOP.

* Assign Result
  ET_REMAIN_ADVREC = LT_REMAIN.

ENDMETHOD.


METHOD LOCK_SO.

  DATA:
    LT_CUST  TYPE  TT_CUST.


* Initialize Output
  CLEAR ES_RETURN.

* Lock SO
  LOOP AT IT_SO ASSIGNING FIELD-SYMBOL(<L_SO>).
    CALL FUNCTION 'ENQUEUE_EVVBAKE'
      EXPORTING
        MODE_VBAK      = 'E'
        MANDT          = SY-MANDT
        VBELN          = <L_SO>-VBELN
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
*     Error Sales order &1 is currently locked by user &2.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '005'
          IF_MSGV1  = <L_SO>-VBELN
          IF_MSGV2  = SY-MSGV1
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.
    INSERT <L_SO> INTO TABLE GT_LOCKED_SO.
  ENDLOOP.
  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO( ).
  ENDIF.

* Get Related Customer of SO
  SELECT DISTINCT A~KUNNR
    FROM @IT_SO AS X ##ITAB_KEY_IN_SELECT
           INNER JOIN VBAK AS A
             ON  A~VBELN = X~VBELN
    INTO TABLE @LT_CUST.
  IF SY-SUBRC NE 0.
    CLEAR LT_CUST.
  ENDIF.

* Lock Related Customers
  LOOP AT LT_CUST ASSIGNING FIELD-SYMBOL(<L_CUST>).
    CALL FUNCTION 'ENQUEUE_EZSDSSDS044'
      EXPORTING
        MODE_ZSDSSDS044 = 'E'
        MANDT           = SY-MANDT
        KUNNR           = <L_CUST>-KUNNR
      EXCEPTIONS
        FOREIGN_LOCK    = 1
        SYSTEM_FAILURE  = 2
        OTHERS          = 3.
    IF SY-SUBRC <> 0.
*     Error Customer &1 is currently being processed by user &2.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '006'
          IF_MSGV1  = <L_CUST>-KUNNR
          IF_MSGV2  = SY-MSGV1
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.
    INSERT <L_CUST> INTO TABLE GT_LOCKED_CUST.
  ENDLOOP.
  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO( ).
  ENDIF.

ENDMETHOD.


METHOD MAINTAIN_ADVREC_ASSIGN.

  DATA:
    LT_CONF   TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA,
    LT_ADVREC TYPE  ZCL_SDSSD_ORDER_CONFIRMATION=>TT_ADVREC_DATA.

  DATA:
    LS_RETURN  TYPE  BAPIRET1.

  DATA:
    LF_CANCEL         TYPE  FLAG,
    LF_DISPLAY_ONLY   TYPE  FLAG,
    LF_ASSIGNED_FOUND TYPE FLAG.


* Initialize Output
  CLEAR: ET_CONF,
         ET_ADVREC.

* Start Assign Advance Receive for Items (Lock processing)
  ZCL_SDSSD_ORDER_CONFIRMATION=>DETERMINE_ADVREC(
    EXPORTING
      IT_SCHEDLINE      = IT_SCHEDLINE
      IF_AUTO           = IF_AUTO
      IF_LOCK           = 'X'
    IMPORTING
      ES_RETURN         = LS_RETURN
      ET_CONF           = LT_CONF
      ET_ADVREC         = LT_ADVREC
      EF_DISPLAY_ONLY   = LF_DISPLAY_ONLY
      EF_ASSIGNED_FOUND = LF_ASSIGNED_FOUND ).
  IF LS_RETURN IS NOT INITIAL.
*   Show Error
    MESSAGE ID LS_RETURN-ID
            TYPE 'S'
            NUMBER LS_RETURN-NUMBER DISPLAY LIKE LS_RETURN-TYPE
            WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    RETURN.
  ENDIF.

* Call screen to modify data
  CALL FUNCTION 'Z_SDSSD_ASSIGN_ADVREC'
    EXPORTING
      IF_DISPLAY_ONLY = LF_DISPLAY_ONLY
      IF_DELETE_ALLOW = LF_ASSIGNED_FOUND
    CHANGING
      CT_DATA         = LT_CONF
      CT_ADVREC       = LT_ADVREC
    EXCEPTIONS
      USER_CANCEL     = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    LF_CANCEL = 'X'.
  ENDIF.

  IF LF_DISPLAY_ONLY IS INITIAL AND
     LF_CANCEL EQ 'X'.
*   Message: Operation is cancelled by user.
    MESSAGE S032(ZSDSCA01).
  ENDIF.

* Unlock SO
  ZCL_SDSSD_ORDER_CONFIRMATION=>UNLOCK_SO( ).

* Assign Result
  IF LF_CANCEL IS INITIAL AND
     LF_DISPLAY_ONLY IS INITIAL.
    IF ET_CONF IS SUPPLIED.
      ET_CONF = LT_CONF.
    ENDIF.

    IF ET_ADVREC IS SUPPLIED.
      ET_ADVREC = LT_ADVREC.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD PROCESS_APPROVAL.

  DATA:
    LT_SO   TYPE  TT_SO,
    LT_DATA TYPE  TT_CONF_DATA.

  DATA:
    LS_INFO  TYPE  TS_APPV_INFO.


* Initialize Output
  CLEAR: ES_RETURN,
         ET_RESULT.

  LT_SO = CORRESPONDING TT_SO( IT_SCHEDLINE ).

* ----------------
* Lock Order
* ----------------
  LOCK_SO(
    EXPORTING
      IT_SO     = LT_SO
    IMPORTING
      ES_RETURN = ES_RETURN ).
  IF ES_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.
  FREE: LT_SO.

* ----------------
* Validate data
* ----------------
  VALIDATE_FOR_APPROVAL(
    EXPORTING
      IT_SCHEDLINE = IT_SCHEDLINE
      IF_APPROVER  = IF_APPROVER
      IF_APPV_STAT = IF_APPV_STAT
    IMPORTING
      ET_CONF      = LT_DATA
      ES_RETURN    = ES_RETURN ).

  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO(  ).
    RETURN.
  ENDIF.

* ----------------
* Execute Approval
* ----------------
  CLEAR LS_INFO.
  LS_INFO-UNAME     = SY-UNAME.
  LS_INFO-DATUM     = SY-DATUM.
  LS_INFO-UZEIT     = SY-UZEIT.
  LS_INFO-APPROVER  = IF_APPROVER.
  LS_INFO-APPV_STAT = IF_APPV_STAT.

  EXECUTE_SCHEDLINE_APPR(
    EXPORTING
      IS_INFO   = LS_INFO
      IT_CONF   = LT_DATA
    IMPORTING
      ES_RETURN = ES_RETURN
      ET_RESULT = ET_RESULT ).
  IF ES_RETURN IS NOT INITIAL.
    UNLOCK_SO(  ).
    RETURN.
  ENDIF.

* Unlock Data at end of processing
  UNLOCK_SO( ).

*--BOI SDI039 - Send interface manager approve status to Salesforce
* ---------------------
* Send interface approve
* ---------------------
  IF IF_APPROVER = GC_MANAGER.
    INTERFACE_SEND_SCHEDULE_STAT(
      EXPORTING
        IT_CONF   = LT_DATA
        IT_RESULT = ET_RESULT
        IF_MODE   = 'A'  ) . "approve
  ENDIF.
*--EOD SDI039 - Send interface manager approve status to Salesforce
ENDMETHOD.


METHOD SAVE_ADVREC_ASSIGNMENT.

  DATA:
    LT_SAVE      TYPE  STANDARD TABLE OF ZSDSSDT010,
    LT_SCHEDLINE TYPE  TT_SCHEDLINE.

  DATA:
    LS_SAVE  TYPE  ZSDSSDT010.

  DATA:
    LF_AENAM TYPE  ZSDSSDT010-AENAM,
    LF_AEDAT TYPE  ZSDSSDT010-AEDAT,
    LF_AEZET TYPE  ZSDSSDT010-AEZET.


* Initialize Output
  CLEAR: ES_RETURN.

* Assign Processing Log info
  LF_AENAM = SY-UNAME.
  LF_AEDAT = SY-DATUM.
  LF_AEZET = SY-UZEIT.
  LOOP AT IT_ADVREC_ASSIGN ASSIGNING FIELD-SYMBOL(<L_ADVREC_ASSIGN>).

    IF <L_ADVREC_ASSIGN>-DMBTR IS NOT INITIAL.
      CLEAR LS_SAVE.
      MOVE-CORRESPONDING <L_ADVREC_ASSIGN> TO LS_SAVE.
      LS_SAVE-AENAM = LF_AENAM.
      LS_SAVE-AEDAT = LF_AEDAT.
      LS_SAVE-AEZET = LF_AEZET.
      INSERT LS_SAVE INTO TABLE LT_SAVE.
    ENDIF.

    COLLECT VALUE TS_SCHEDLINE( VBELN = <L_ADVREC_ASSIGN>-VBELN
                                POSNR = <L_ADVREC_ASSIGN>-POSNR
                                ETENR = <L_ADVREC_ASSIGN>-ETENR )
            INTO LT_SCHEDLINE.
  ENDLOOP.

* Delete existing assignment for each Schedline
  LOOP AT LT_SCHEDLINE ASSIGNING FIELD-SYMBOL(<L_SCHEDLINE>).
    DELETE FROM ZSDSSDT010 WHERE VBELN EQ <L_SCHEDLINE>-VBELN
                             AND POSNR EQ <L_SCHEDLINE>-POSNR
                             AND ETENR EQ <L_SCHEDLINE>-ETENR.
  ENDLOOP.

  IF LT_SAVE IS NOT INITIAL.
*   Save Data into DB
    MODIFY ZSDSSDT010 FROM TABLE LT_SAVE.
    IF SY-SUBRC NE 0.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
*     Error: Error during save entries into table ZSDSSDT010.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '020'
        IMPORTING
          ES_RETURN = ES_RETURN ).
      RETURN.
    ENDIF.
  ENDIF.

  IF IF_COMMIT EQ 'X'.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDMETHOD.


METHOD UNLOCK_SO.

  LOOP AT GT_LOCKED_SO ASSIGNING FIELD-SYMBOL(<L_SO>).
    CALL FUNCTION 'DEQUEUE_EVVBAKE'
      EXPORTING
        MODE_VBAK = 'E'
        MANDT     = SY-MANDT
        VBELN     = <L_SO>-VBELN.
  ENDLOOP.

  LOOP AT GT_LOCKED_CUST ASSIGNING FIELD-SYMBOL(<L_CUST>).
    CALL FUNCTION 'DEQUEUE_EZSDSSDS044'
      EXPORTING
        MODE_ZSDSSDS044 = 'E'
        MANDT           = SY-MANDT
        KUNNR           = <L_CUST>-KUNNR.
  ENDLOOP.

  CLEAR: GT_LOCKED_SO,
         GT_LOCKED_CUST.

ENDMETHOD.


METHOD VALIDATE_FOR_ADVREC.

  DATA:
    LT_DATA          TYPE  TT_CONF_DATA,
    LT_REMAIN_ADVREC TYPE  TT_REMAIN_ADVREC,
    LT_ADVGP         TYPE  STANDARD TABLE OF ZSDSDE_ADVGP.

  DATA:
    LF_NOT_REQ  TYPE  FLAG.


* Initialize Output
  CLEAR: ES_RETURN,
         ET_CONF,
         ET_ADVREC,
         EF_DISPLAY_ONLY,
         EF_ASSIGNED_FOUND.

* ----------------
* Get Refresh data for Specified Key
* ----------------
  GET_DATA(
    EXPORTING
      IT_SCHEDLINE     = IT_SCHEDLINE
      IF_REMAIN_ADVREC = 'X'
    IMPORTING
      ET_DATA          = LT_DATA
      ET_REMAIN_ADVREC = LT_REMAIN_ADVREC ).

* Sort for processing
  SORT LT_DATA BY VBELN ASCENDING
                  POSNR ASCENDING
                  ETENR ASCENDING.

* ----------------
* Validate Data
* ----------------
  IF LINES( LT_REMAIN_ADVREC ) GT 1.
*   Error: Please select entries for same customer code/AdvRec Group.
    ASSIGN_RETURN(
      EXPORTING
        IF_MSGTY  = 'E'
        IF_MSGID  = 'ZSDSSD01'
        IF_MSGNO  = '016'
      IMPORTING
        ES_RETURN = ES_RETURN ).
    RETURN.
  ENDIF.

* Check Only same Advance Receive Group
  LT_ADVGP = VALUE #( FOR GROUPS LF_ADVGP OF <L_LINE> IN LT_DATA
                      GROUP BY <L_LINE>-ADVGP WITHOUT MEMBERS ( LF_ADVGP ) ).
  IF LINES( LT_ADVGP ) GT 1.
*   Error: Please select entries for same customer code/AdvRec Group.
    ASSIGN_RETURN(
      EXPORTING
        IF_MSGTY  = 'E'
        IF_MSGID  = 'ZSDSSD01'
        IF_MSGNO  = '016'
      IMPORTING
        ES_RETURN = ES_RETURN ).
    RETURN.
  ENDIF.

  DATA(LF_LINES) = LINES( IT_SCHEDLINE ).
  LOOP AT IT_SCHEDLINE ASSIGNING FIELD-SYMBOL(<L_SCHEDLINE>).

    READ TABLE LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>)
                       WITH KEY VBELN = <L_SCHEDLINE>-VBELN
                                POSNR = <L_SCHEDLINE>-POSNR
                                ETENR = <L_SCHEDLINE>-ETENR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Error: Invalid Schedline &1 &2 &3. Please refresh data and try again.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '007'
          IF_MSGV1  = <L_SCHEDLINE>-VBELN
          IF_MSGV2  = <L_SCHEDLINE>-POSNR
          IF_MSGV3  = <L_SCHEDLINE>-ETENR
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.

    CLEAR LF_NOT_REQ.
    IF <L_DATA>-AMOUNT IS NOT INITIAL.
      READ TABLE LT_REMAIN_ADVREC TRANSPORTING NO FIELDS
                                  WITH KEY KUNNR = <L_DATA>-KUNNR
                                  BINARY SEARCH.
      IF SY-SUBRC NE 0.
*       No Adv reamaining
        LF_NOT_REQ = 'X'.
      ENDIF.
    ELSE.
      LF_NOT_REQ = 'X'.
    ENDIF.

    IF LF_NOT_REQ EQ 'X'.
*     Error: Schedule line &1 &2 &3 is not needed for Adv.Receive assignment.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '015'
          IF_MSGV1  = <L_SCHEDLINE>-VBELN
          IF_MSGV2  = <L_SCHEDLINE>-POSNR
          IF_MSGV3  = <L_SCHEDLINE>-ETENR
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.

*   Only when more than 1
    IF LF_LINES GT 1.
      IF <L_DATA>-MGR_APPV_STAT EQ GC_STAT_APPROVE.
*       Error: Schedule line &1 &2 &3 was already approved by Manager.
        ASSIGN_RETURN(
          EXPORTING
            IF_MSGTY  = 'E'
            IF_MSGID  = 'ZSDSSD01'
            IF_MSGNO  = '012'
            IF_MSGV1  = <L_SCHEDLINE>-VBELN
            IF_MSGV2  = <L_SCHEDLINE>-POSNR
            IF_MSGV3  = <L_SCHEDLINE>-ETENR
          IMPORTING
            ES_RETURN = ES_RETURN ).
        EXIT.
      ENDIF.
      IF <L_DATA>-CO_APPV_STAT EQ GC_STAT_APPROVE.
*       Error: Schedule line &1 &2 &3 was already &4.
*       Text-a01: approved
        ASSIGN_RETURN(
          EXPORTING
            IF_MSGTY  = 'E'
            IF_MSGID  = 'ZSDSSD01'
            IF_MSGNO  = '009'
            IF_MSGV1  = <L_SCHEDLINE>-VBELN
            IF_MSGV2  = <L_SCHEDLINE>-POSNR
            IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IF_MSGV4  = TEXT-A01
          IMPORTING
            ES_RETURN = ES_RETURN ).
        EXIT.
      ENDIF.
    ENDIF.

    IF <L_DATA>-CO_APPV_STAT EQ GC_STAT_APPROVE.
      EF_DISPLAY_ONLY = 'X'.
    ENDIF.

*   Assignment found
    IF <L_DATA>-ADVREC IS NOT INITIAL.
      EF_ASSIGNED_FOUND = 'X'.
    ENDIF.

  ENDLOOP.
  IF ES_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

* Assign Data output
  IF ET_CONF IS SUPPLIED.
    ET_CONF = LT_DATA.
  ENDIF.

  IF ET_ADVREC IS SUPPLIED.
    READ TABLE LT_REMAIN_ADVREC ASSIGNING FIELD-SYMBOL(<L_REMAIN_ADVREC>)
                                INDEX 1.
    IF SY-SUBRC EQ 0.
      READ TABLE <L_REMAIN_ADVREC>-ADVGP_ADVREC ASSIGNING FIELD-SYMBOL(<L_ADVGP_ADVREC>)
                                                WITH KEY ADVGP = LT_ADVGP[ 1 ]
                                                BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        ET_ADVREC = <L_ADVGP_ADVREC>-ADVREC.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_FOR_APPROVAL.

  DATA:
    LT_DATA          TYPE  TT_CONF_DATA,
    LT_REMAIN_ADVREC TYPE  TT_REMAIN_ADVREC.

  DATA:
    LF_APPV_TXT    TYPE  TEXT50,
    LF_RQASS       TYPE  FLAG,
    LF_ADVREC_DIFF TYPE  WERT1,
    LF_AMOUNT_DIFF TYPE  WERT1.


* Initialize Output
  CLEAR: ES_RETURN.

  CASE IF_APPV_STAT.
    WHEN GC_STAT_APPROVE.
*     Text-a01: approved
      LF_APPV_TXT = TEXT-A01.
    WHEN GC_STAT_REJECT.
*     Text-a02: rejected
      LF_APPV_TXT = TEXT-A02.
  ENDCASE.

* ----------------
* Get Refresh data for Specified Key
* ----------------
  GET_DATA(
    EXPORTING
      IT_SCHEDLINE     = IT_SCHEDLINE
      IF_APPROVER      = IF_APPROVER
      IF_REMAIN_ADVREC = 'X'
    IMPORTING
      ET_DATA          = LT_DATA
      ET_REMAIN_ADVREC = LT_REMAIN_ADVREC ).

* Sort for processing
  SORT LT_DATA BY VBELN ASCENDING
                  POSNR ASCENDING
                  ETENR ASCENDING.

* ----------------
* Validate Data
* ----------------
  LOOP AT IT_SCHEDLINE ASSIGNING FIELD-SYMBOL(<L_SCHEDLINE>).

    READ TABLE LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>)
                       WITH KEY VBELN = <L_SCHEDLINE>-VBELN
                                POSNR = <L_SCHEDLINE>-POSNR
                                ETENR = <L_SCHEDLINE>-ETENR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Error: Invalid Schedline &1 &2 &3. Please refresh data and try again.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '007'
          IF_MSGV1  = <L_SCHEDLINE>-VBELN
          IF_MSGV2  = <L_SCHEDLINE>-POSNR
          IF_MSGV3  = <L_SCHEDLINE>-ETENR
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.

*   Check Status
    IF <L_DATA>-LIFSP IS INITIAL.
*     Error: Schedule line &1 &2 &3 is not in delivery block status.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '008'
          IF_MSGV1  = <L_SCHEDLINE>-VBELN
          IF_MSGV2  = <L_SCHEDLINE>-POSNR
          IF_MSGV3  = <L_SCHEDLINE>-ETENR
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.

    CASE IF_APPROVER.
      WHEN GC_COOPERATOR.
        IF <L_DATA>-MGR_APPV_STAT EQ GC_STAT_APPROVE.
*         Error: Schedule line &1 &2 &3 was already approved by Manager.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '012'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.
        IF <L_DATA>-CO_APPV_STAT EQ IF_APPV_STAT.
*         Error: Schedule line &1 &2 &3 was already &4.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '009'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
              IF_MSGV4  = LF_APPV_TXT
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.

      WHEN GC_MANAGER.
        IF <L_DATA>-CO_APPV_STAT NE GC_STAT_APPROVE.
*         Error: Schedule line &1 &2 &3 is not yet approved by Co-Operator.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '010'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.
        IF <L_DATA>-MGR_APPV_STAT EQ IF_APPV_STAT.
*         Error: Schedule line &1 &2 &3 was already &4.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '009'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
              IF_MSGV4  = LF_APPV_TXT
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.
    ENDCASE.

*   Only when Approved
    IF IF_APPV_STAT EQ GC_STAT_APPROVE.

      CLEAR LF_RQASS.
      READ TABLE GT_ADVREC_GROUP ASSIGNING FIELD-SYMBOL(<L_ADVREC_GROUP>)
                                 WITH KEY ADVGP = <L_DATA>-ADVGP
                                 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LF_RQASS = <L_ADVREC_GROUP>-RQASS.
      ENDIF.

*     Only Validate When Assignment is required
      IF LF_RQASS EQ 'X'.
*Add+ 420000395 >>>
        CLEAR: LF_ADVREC_DIFF,LF_AMOUNT_DIFF.
        READ TABLE GT_ADVREC_DIFF ASSIGNING FIELD-SYMBOL(<L_GENC>)
                                    WITH KEY PARAM_EXT = <L_DATA>-ADVGP
                                    BINARY SEARCH.
        IF SY-SUBRC = 0.
          LF_ADVREC_DIFF = <L_GENC>-VALUE_LOW.
          LF_AMOUNT_DIFF = <L_DATA>-AMOUNT - <L_DATA>-AMOUNT_ADV.
        ENDIF.
*<<< 420000395
*       Wrong AdvRec Assigned
        IF <L_DATA>-AMOUNT_ADV IS NOT INITIAL AND
*           <L_DATA>-AMOUNT_ADV NE <L_DATA>-AMOUNT.         "Del- 420000395
           NOT ( LF_AMOUNT_DIFF BETWEEN 0 AND LF_ADVREC_DIFF ). "Add+ 420000395
*         Error: Sched. line &1 &2 &3 was not yet assigned advance receive amount.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '011'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.

*       Check AdvRec
        READ TABLE LT_REMAIN_ADVREC ASSIGNING FIELD-SYMBOL(<L_REMAIN_ADVREC>)
                                    WITH KEY KUNNR = <L_DATA>-KUNNR
                                    BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Not Yet Assign Remaining Advance receive
          IF <L_DATA>-AMOUNT IS NOT INITIAL AND
             <L_DATA>-AMOUNT_ADV IS INITIAL.
*           Error: Sched. line &1 &2 &3 was not yet assigned advance receive amount.
            ASSIGN_RETURN(
              EXPORTING
                IF_MSGTY  = 'E'
                IF_MSGID  = 'ZSDSSD01'
                IF_MSGNO  = '011'
                IF_MSGV1  = <L_SCHEDLINE>-VBELN
                IF_MSGV2  = <L_SCHEDLINE>-POSNR
                IF_MSGV3  = <L_SCHEDLINE>-ETENR
              IMPORTING
                ES_RETURN = ES_RETURN ).
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

* Assign Data output
  IF ET_CONF IS SUPPLIED.
    ET_CONF = LT_DATA.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_FOR_CANCEL.

  DATA:
    LT_DATA          TYPE  TT_CONF_DATA.


* Initialize Output
  CLEAR: ES_RETURN.

* ----------------
* Get Refresh data for Specified Key
* ----------------
  GET_DATA(
    EXPORTING
      IT_SCHEDLINE = IT_SCHEDLINE
      IF_APPROVER  = IF_APPROVER
    IMPORTING
      ET_DATA      = LT_DATA ).

* Sort for processing
  SORT LT_DATA BY VBELN ASCENDING
                  POSNR ASCENDING
                  ETENR ASCENDING.

* ----------------
* Validate Data
* ----------------
  LOOP AT IT_SCHEDLINE ASSIGNING FIELD-SYMBOL(<L_SCHEDLINE>).

    READ TABLE LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>)
                       WITH KEY VBELN = <L_SCHEDLINE>-VBELN
                                POSNR = <L_SCHEDLINE>-POSNR
                                ETENR = <L_SCHEDLINE>-ETENR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Error: Invalid Schedline &1 &2 &3. Please refresh data and try again.
      ASSIGN_RETURN(
        EXPORTING
          IF_MSGTY  = 'E'
          IF_MSGID  = 'ZSDSSD01'
          IF_MSGNO  = '007'
          IF_MSGV1  = <L_SCHEDLINE>-VBELN
          IF_MSGV2  = <L_SCHEDLINE>-POSNR
          IF_MSGV3  = <L_SCHEDLINE>-ETENR
        IMPORTING
          ES_RETURN = ES_RETURN ).
      EXIT.
    ENDIF.

    CASE IF_APPROVER.
      WHEN GC_COOPERATOR.
        IF <L_DATA>-MGR_APPV_STAT EQ GC_STAT_APPROVE.
*         Error: Schedule line &1 &2 &3 was already approved by Manager.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '012'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.
        IF <L_DATA>-CO_APPV_STAT IS INITIAL.
*         Error: Schedule line &1 &2 &3 is not yet approved.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '013'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.

      WHEN GC_MANAGER.
        IF <L_DATA>-MGR_APPV_STAT IS INITIAL.
*         Error: Schedule line &1 &2 &3 is not yet approved.
          ASSIGN_RETURN(
            EXPORTING
              IF_MSGTY  = 'E'
              IF_MSGID  = 'ZSDSSD01'
              IF_MSGNO  = '013'
              IF_MSGV1  = <L_SCHEDLINE>-VBELN
              IF_MSGV2  = <L_SCHEDLINE>-POSNR
              IF_MSGV3  = <L_SCHEDLINE>-ETENR
            IMPORTING
              ES_RETURN = ES_RETURN ).
          EXIT.
        ENDIF.
    ENDCASE.

  ENDLOOP.

* Assign Data output
  IF ET_CONF IS SUPPLIED.
    ET_CONF = LT_DATA.
  ENDIF.

ENDMETHOD.


METHOD DETERMINE_ADVGP.

* Initialize Output
  CLEAR RF_ADVGP.

* Get Constant
  GET_GENC( ).

* Find Advrec Group from Payment term
  LOOP AT GT_ADVREC_GROUP ASSIGNING FIELD-SYMBOL(<L_ADVREC_GROUP>).
    IF IF_ZTERM IN <L_ADVREC_GROUP>-ZTERM.
      RF_ADVGP = <L_ADVREC_GROUP>-ADVGP.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD GET_GENC.

  CONSTANTS:
    LC_ADVREC_UMSKZ TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_SPGL_IND',
    LC_ADVREC_ZTERM TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_PAYMTERM',
    LC_ADVREC_RQASS TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_REQUIRED',
    LC_ADVREC_DIFF  TYPE  ZSDSDE_PARAM_NAME VALUE 'ADVREC_DIFF'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC        TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C,
    LT_RANGE_PARAM TYPE RANGE OF ZSDSCAC001-PARAM.

  DATA:
    LS_ADVREC_GROUP  TYPE  TS_ADVREC_GROUP.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSSD_ORDER_CONFIRMATION'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_ADVREC_GROUP.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read
  LF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Special GL Indicator for Advance Receive
*     ------------------------------------
      WHEN LC_ADVREC_UMSKZ.
        CLEAR LS_ADVREC_GROUP.
        LS_ADVREC_GROUP-ADVGP = <L_GENC>-PARAM_EXT.
        READ TABLE GT_ADVREC_GROUP ASSIGNING FIELD-SYMBOL(<L_ADVREC_GROUP>)
                                   WITH KEY ADVGP = LS_ADVREC_GROUP-ADVGP
                                   BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_ADVREC_GROUP INTO TABLE GT_ADVREC_GROUP
                                 ASSIGNING <L_ADVREC_GROUP>.
        ENDIF.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE <L_ADVREC_GROUP>-UMSKZ.

*     ------------------------------------
*     Payment Term for Advance Receive
*     ------------------------------------
      WHEN LC_ADVREC_ZTERM.
        CLEAR LS_ADVREC_GROUP.
        LS_ADVREC_GROUP-ADVGP = <L_GENC>-PARAM_EXT.
        READ TABLE GT_ADVREC_GROUP ASSIGNING <L_ADVREC_GROUP>
                                   WITH KEY ADVGP = LS_ADVREC_GROUP-ADVGP
                                   BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_ADVREC_GROUP INTO TABLE GT_ADVREC_GROUP
                                 ASSIGNING <L_ADVREC_GROUP>.
        ENDIF.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE <L_ADVREC_GROUP>-ZTERM.

*     ------------------------------------
*     Require assignment flag for Advance Receive
*     ------------------------------------
      WHEN LC_ADVREC_RQASS.
        CLEAR LS_ADVREC_GROUP.
        LS_ADVREC_GROUP-ADVGP = <L_GENC>-PARAM_EXT.
        READ TABLE GT_ADVREC_GROUP ASSIGNING <L_ADVREC_GROUP>
                                   WITH KEY ADVGP = LS_ADVREC_GROUP-ADVGP
                                   BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_ADVREC_GROUP INTO TABLE GT_ADVREC_GROUP
                                 ASSIGNING <L_ADVREC_GROUP>.
        ENDIF.
        <L_ADVREC_GROUP>-RQASS = <L_GENC>-VALUE_LOW.

    ENDCASE.

  ENDLOOP.

  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LC_ADVREC_DIFF ) TO LT_RANGE_PARAM.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID  = LF_REPID
      IRT_PARAM = LT_RANGE_PARAM
    IMPORTING
      ET_GEN_C  = GT_ADVREC_DIFF.

ENDMETHOD.


  METHOD INTERFACE_SEND_SCHEDULE_STAT.
    DATA: LC_REPID_SUBMIT TYPE SY-REPID VALUE 'ZSDSSDR0330'.
    DATA: LT_DATA TYPE STANDARD TABLE OF ZSDSSDS102,
          LS_DATA TYPE ZSDSSDS102.
    DATA: LF_JOBNAME TYPE TBTCJOB-JOBNAME,
          LF_JOBNUM  TYPE TBTCJOB-JOBCOUNT,
          LF_IMPKEY  TYPE ZSDSSDT020-ID,
          LF_APPR    TYPE FLAG,
          LF_CANC    TYPE FLAG.
    STATICS: LRT_AUART_SEND_INTERFACE TYPE RANGE OF VBAK-AUART.

    IF LRT_AUART_SEND_INTERFACE[] IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = LC_REPID_SUBMIT
          IF_PARAM = 'ORDER_TYPE'
        IMPORTING
          ET_RANGE = LRT_AUART_SEND_INTERFACE.
    ENDIF.
    READ TABLE IT_CONF  ASSIGNING FIELD-SYMBOL(<L_CONF>) INDEX 1.
    IF SY-SUBRC = 0.
      IF <L_CONF>-AUART NOT IN LRT_AUART_SEND_INTERFACE.
        RETURN.
      ENDIF.
    ENDIF.

* ===============filter only succes data =============
    LOOP AT IT_CONF ASSIGNING <L_CONF>.
      READ TABLE IT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                           WITH KEY VBELN = <L_CONF>-VBELN
                                    POSNR = <L_CONF>-POSNR
                                    ETENR = <L_CONF>-ETENR.
      IF SY-SUBRC = 0.
        IF <L_RESULT>-MSGTY = GC_MSGTY_SUCCESS.
          LS_DATA-VBELN = <L_CONF>-VBELN.
          LS_DATA-POSNR = <L_CONF>-POSNR.
          LS_DATA-ETENR = <L_CONF>-ETENR.
          INSERT LS_DATA INTO TABLE LT_DATA.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF LT_DATA IS INITIAL.
      RETURN.
    ENDIF.
    SORT LT_DATA BY VBELN POSNR ETENR.

* ===============prepare parameter and export data for submit program =============
    CASE IF_MODE.
      WHEN 'A'. "approve
        LF_APPR = ABAP_TRUE.
        LF_CANC = ABAP_FALSE.
      WHEN 'C'. "cancel
        LF_APPR = ABAP_FALSE.
        LF_CANC = ABAP_TRUE.
    ENDCASE.

    READ TABLE LT_DATA INTO LS_DATA INDEX 1.
    LF_IMPKEY = SY-DATUM && SY-UZEIT && '-'
                && LS_DATA-VBELN && LS_DATA-POSNR && LS_DATA-ETENR.

* Export data to datable for import in program ZSDSSDR0330
    EXPORT INPUT = LT_DATA TO DATABASE ZSDSSDT020(IP) ID LF_IMPKEY.



* ===============call interface by submit in backgroud =============
* Generate JOB to call Interface
    CONCATENATE GC_JOBNAME_PREFIX
                SY-DATUM
                SY-UZEIT
           INTO LF_JOBNAME
           SEPARATED BY '_'.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LF_JOBNAME
      IMPORTING
        JOBCOUNT         = LF_JOBNUM
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

* Call interface program
    SUBMIT ZSDSSDR0330                                   "#EC CI_SUBMIT
      WITH RB_APPR EQ LF_APPR
      WITH RB_CANC EQ LF_CANC
      WITH P_TEST  EQ ABAP_FALSE
      WITH P_IMPKEY EQ LF_IMPKEY
      VIA JOB LF_JOBNAME NUMBER LF_JOBNUM
      AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LF_JOBNUM
        JOBNAME              = LF_JOBNAME
        STRTIMMED            = 'X'
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        OTHERS               = 8.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
