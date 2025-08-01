CLASS ZCL_SDSPS_PROJECT_STRUC_SERV DEFINITION
  PUBLIC
  INHERITING FROM ZCL_SDSCA_REST_SERVICE
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TS_PROJDEF_DATA,
        MODE     TYPE CHAR1,
        PROJDEF  TYPE ZCL_SDSPS_PROJECT=>TS_PROJECTDEF,
        PROJDEFX TYPE ZCL_SDSPS_PROJECT=>TS_PROJECTDEFX,
      END OF TS_PROJDEF_DATA .
    TYPES:
      BEGIN OF TS_WBS_DATA,
        PSPID   TYPE PROJ-PSPID,
        MODE    TYPE CHAR1,
        WBS     TYPE ZCL_SDSPS_PROJECT=>TS_WBSELEM,
        WBSX    TYPE ZCL_SDSPS_PROJECT=>TS_WBSELEMX,
        WBS_REQ TYPE ZSDSPSS003,
      END OF TS_WBS_DATA .
    TYPES:
      TT_WBS_DATA TYPE STANDARD TABLE OF TS_WBS_DATA .
    TYPES:
      BEGIN OF TS_MAP_PROFL,
        PROFL      TYPE  PROJ-PROFL,
        COND_PSPID TYPE  RANGE OF PROJ-PSPID,
      END OF TS_MAP_PROFL .
    TYPES:
      TT_MAP_PROFL TYPE SORTED TABLE OF TS_MAP_PROFL
                                   WITH UNIQUE KEY PROFL .
    TYPES:
      BEGIN OF TS_SEQ_APUP,
        ITMTY TYPE ZSDSDE_PS_ITMTY,
        ACTTY TYPE RANGE OF ZSDSDE_PS_ACTTY,
      END OF TS_SEQ_APUP .
    TYPES:
      TT_SEQ_APUP TYPE SORTED TABLE OF TS_SEQ_APUP
                                WITH UNIQUE KEY ITMTY .
    TYPES:
      BEGIN OF TS_MAP_PRART,
        PRART      TYPE  PRPS-PRART,
        COND_POSID TYPE  RANGE OF PRPS-POSID,
      END OF TS_MAP_PRART .
    TYPES:
      TT_MAP_PRART TYPE SORTED TABLE OF TS_MAP_PRART
                                   WITH UNIQUE KEY PRART .
    TYPES:
      BEGIN OF TS_LOB_PRJTY,
        PRJTY TYPE  ZSDSDE_PRJTY,
        LOB   TYPE  PRPS-USR00,
      END OF TS_LOB_PRJTY .
    TYPES:
      TT_LOB_PRJTY TYPE SORTED TABLE OF TS_LOB_PRJTY
                                 WITH UNIQUE KEY PRJTY .

    TYPES:
      BEGIN OF TS_LOB_ITMTY,
        ITMTY TYPE  ZSDSDE_PS_ITMTY,
        LOB   TYPE  PRPS-USR00,
      END OF TS_LOB_ITMTY .
    TYPES:
      TT_LOB_ITMTY TYPE SORTED TABLE OF TS_LOB_ITMTY
                                 WITH UNIQUE KEY ITMTY .

    TYPES:
      BEGIN OF TS_LOB_ACTTY,
        ITMTY TYPE  ZSDSDE_PS_ITMTY,
        ACTTY TYPE  ZSDSDE_PS_ACTTY,
        LOB   TYPE  PRPS-USR00,
      END OF TS_LOB_ACTTY .
    TYPES:
      TT_LOB_ACTTY TYPE SORTED TABLE OF TS_LOB_ACTTY
                                 WITH UNIQUE KEY ITMTY
                                                 ACTTY .
    TYPES:
      BEGIN OF TS_ITMTY_PRCTR,
        ITMTY TYPE  ZSDSDE_PS_ITMTY,
        ACTTY TYPE  RANGE OF ZSDSDE_PS_ACTTY,
      END OF TS_ITMTY_PRCTR .
    TYPES:
      TT_ITMTY_PRCTR  TYPE  SORTED TABLE OF TS_ITMTY_PRCTR
                              WITH UNIQUE KEY ITMTY .
    TYPES:
      BEGIN OF TS_NEW_POSID,
        LOW   TYPE  PRPS-POSID,
        HIGH  TYPE  PRPS-POSID,
        POSID TYPE  PRPS-POSID,
      END OF TS_NEW_POSID .
    TYPES:
      TT_NEW_POSID TYPE SORTED TABLE OF TS_NEW_POSID
                                 WITH UNIQUE KEY LOW
                                                 HIGH .

    CONSTANTS GC_CREATE TYPE CHAR1 VALUE 'C' ##NO_TEXT.
    CONSTANTS GC_CHANGE TYPE CHAR1 VALUE 'U' ##NO_TEXT.
    CONSTANTS GC_OVERHAUL TYPE CHAR1 VALUE '2' ##NO_TEXT.

    METHODS INITIALIZE_DATA
        REDEFINITION .
    METHODS PROCESS_DATA
        REDEFINITION .
protected section.
private section.

  data GREF_VALIDATE type ref to ZCL_SDSCA_DATA_VALIDATION .
  data GT_MAP_PROFL type TT_MAP_PROFL .
  data GT_SEQ_APUP type TT_SEQ_APUP .
  data GT_MAP_PRART type TT_MAP_PRART .
  data GT_LOB_PRJTY type TT_LOB_PRJTY .
  data GT_LOB_ITMTY type TT_LOB_ITMTY .
  data GT_LOB_ACTTY type TT_LOB_ACTTY .
  data GT_ITMTY_PRCTR type TT_ITMTY_PRCTR .
  data GF_DEFT_PRCTR type PRPS-PRCTR .
  data GT_NEW_POSID type TT_NEW_POSID .

  methods WAIT_PROJECT_UNLOCK
    importing
      !IF_PSPID type PROJ-PSPID .
  methods ASSIGN_WBS_RESULT
    importing
      !IT_WBS type TT_WBS_DATA
      !IF_MODE type TS_WBS_DATA-MODE
      !IT_RETURN type BAPIRET2_TAB
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods ASSIGN_VERNR
    importing
      !IF_VKGRP type VKGRP
    exporting
      !EF_VERNR type PROJ-VERNR .
  methods ASSIGN_PROJECT_RESULT
    importing
      !IS_REQUEST type ZSDSPSS002
      !IS_PROJDEF type TS_PROJDEF_DATA
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods GET_NEW_POSID
    importing
      !IF_LOW type PRPS-POSID
      !IF_HIGH type PRPS-POSID
    exporting
      !EF_POSID type PRPS-POSID .
  methods ASSIGN_LOB
    importing
      !IF_PRJTY type ZSDSDE_PRJTY
      !IF_ITMTY type ZSDSDE_PS_ITMTY
      !IF_ACTTY type ZSDSDE_PS_ACTTY
    exporting
      !EF_USR00 type PRPS-USR00 .
  methods ASSIGN_PROFIT_CENTER
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_POSID type PRPS-POSID
      !IF_KOKRS type CSKS-KOKRS default '1000'
      !IF_AKSTL type PRPS-AKSTL
      !IF_VKORG type VBAK-VKORG default '1000'
      !IF_AUART type VBAK-AUART default 'ZO01'
      !IF_PROCESS_TYPE type CRMS4D_SERV_H-PROCESS_TYPE default 'ZRP7'
      !IF_VKBUR type VBAK-VKBUR
      !IF_VKGRP type VBAK-VKGRP
    exporting
      !EF_PRCTR type PRPS-PRCTR .
  methods ASSIGN_WBS_POST1
    importing
      !IF_POSID type PRPS-POSID
      !IF_STUFE type PRPS-STUFE
      !IF_POST1 type ZSDSDE_POST1
    exporting
      !EF_POST1 type ZSDSDE_POST1 .
  methods GENERATE_WBS_LV1_3
    importing
      !IS_PROJDEF type ZCL_SDSPS_PROJECT=>TS_PROJECTDEF
      !IS_WBS type TS_WBS_DATA
    changing
      !CT_WBS type TT_WBS_DATA .
  methods IS_FROM_SFDC
    importing
      !IF_PSPID type PROJ-PSPID
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  methods MAINTAIN_PROJECT_STRUCTURE
    importing
      !IS_REQUEST type ZSDSPSS002
    exporting
      !ES_RESPONSE type ZSDSPSS002 .
  methods ASSIGN_MESSAGE
    importing
      !IF_STATUS type ZSDSDE_REST_STATUS
      !IF_MSGID type SY-MSGID default 'ZSDSPS01'
      !IF_MSGNO type SY-MSGNO
      !IF_MSGV1 type CLIKE optional
      !IF_MSGV2 type CLIKE optional
      !IF_MSGV3 type CLIKE optional
      !IF_MSGV4 type CLIKE optional
      !IF_MESSAGE type CLIKE optional
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods ASSIGN_PRART
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_PRART type PRPS-PRART
      !IF_POSID type PRPS-POSID
    exporting
      !EF_PRART type PRPS-PRART .
  methods ASSIGN_STUFE
    importing
      !IF_POSID type PRPS-POSID
      !IF_STUFE type PRPS-STUFE
    exporting
      !EF_STUFE type PRPS-STUFE
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods GET_GENC .
  methods GET_NEW_POSID_C3
    importing
      !IF_POSID type PRPS-POSID
      !IF_USR01 type PRPS-USR01
      !IF_USR03 type PRPS-USR03
    exporting
      !EF_POSID type PRPS-POSID
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods GET_NEW_POSID_P4
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_ITMTY type ZSDSDE_PS_ITMTY
      !IF_ACTTY type ZSDSDE_PS_ACTTY
      !IF_APUP type ZSDSDE_PS_APUP
    exporting
      !EF_POSID type PRPS-POSID
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods GET_NEW_PSPID
    importing
      !IF_BUKRS type T001-BUKRS default '1000'
      !IF_PRJTY type ZSDSDE_PRJTY
    exporting
      !EF_PSPID type PROJ-PSPID .
  methods GET_PROJDEF_FROM_REFID
    importing
      !IF_REFID type PROJ-USR00
    returning
      value(RF_PSPID) type PROJ-PSPID .
  methods GET_WBSELEM_FROM_EMEMO
    importing
      !IF_REFID type PRPS-USR01
      !IF_EMEMO type PRPS-USR03
    returning
      value(RF_POSID) type PRPS-POSID .
  methods GET_WBSELEM_FROM_REFID
    importing
      !IF_PSPID type PROJ-PSPID optional
      !IF_REFID type PRPS-USR01
    returning
      value(RF_POSID) type PRPS-POSID .
  methods VALIDATE_AND_ASSIGN_POSID
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_POSID type PRPS-POSID
      !IF_USR01 type PRPS-USR01
      !IF_USR03 type PRPS-USR03
      !IF_ITMTY type ZSDSDE_PS_ITMTY
      !IF_ACTTY type ZSDSDE_PS_ACTTY
      !IF_APUP type ZSDSDE_PS_APUP
    exporting
      !EF_POSID type PRPS-POSID
      !EF_MODE type TS_WBS_DATA-MODE
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods VALIDATE_AND_ASSIGN_PSPID
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_USR00 type PROJ-USR00
      !IF_PRJTY type ZSDSDE_PRJTY
    exporting
      !EF_PSPID type PROJ-PSPID
      !EF_MODE type TS_PROJDEF_DATA-MODE
    changing
      !CS_RESPONSE type ZSDSPSS002 .
  methods VALIDATE_REQUEST
    importing
      !IS_REQUEST type ZSDSPSS002
    exporting
      !ES_PROJDEF type TS_PROJDEF_DATA
      !ET_WBS type TT_WBS_DATA
    changing
      !CS_RESPONSE type ZSDSPSS002 .
ENDCLASS.



CLASS ZCL_SDSPS_PROJECT_STRUC_SERV IMPLEMENTATION.


METHOD ASSIGN_LOB.

* Initialize Output
  CLEAR: EF_USR00.

* Read From Project Type
  READ TABLE GT_LOB_PRJTY ASSIGNING FIELD-SYMBOL(<L_LOB_PRJTY>)
                          WITH KEY PRJTY = IF_PRJTY
                          BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    EF_USR00 = <L_LOB_PRJTY>-LOB.
    RETURN.
  ENDIF.

* Read From Item Type
  READ TABLE GT_LOB_ITMTY ASSIGNING FIELD-SYMBOL(<L_LOB_ITMTY>)
                          WITH KEY ITMTY = IF_ITMTY
                          BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    EF_USR00 = <L_LOB_ITMTY>-LOB.
    RETURN.
  ENDIF.

* Read From Activity Type
  READ TABLE GT_LOB_ACTTY ASSIGNING FIELD-SYMBOL(<L_LOB_ACTTY>)
                          WITH KEY ITMTY = IF_ITMTY
                                   ACTTY = IF_ACTTY
                          BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    EF_USR00 = <L_LOB_ACTTY>-LOB.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_MESSAGE.

* Update Status at header section
  CS_RESPONSE-RESP_STATUS = IF_STATUS.

  IF IF_MESSAGE IS NOT INITIAL.
    CS_RESPONSE-RESP_MESSAGE = IF_MESSAGE.
  ELSE.
    MESSAGE ID IF_MSGID TYPE 'I'
            NUMBER IF_MSGNO
            WITH IF_MSGV1 IF_MSGV2 IF_MSGV3 IF_MSGV4
            INTO CS_RESPONSE-RESP_MESSAGE.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_PRART.

* Initialize Output
  CLEAR: EF_PRART.

  IF IS_FROM_SFDC( IF_PSPID ).

    EF_PRART = IF_PRART.

  ELSE.

    LOOP AT GT_MAP_PRART ASSIGNING FIELD-SYMBOL(<L_MAP_PRART>).

      IF IF_POSID IN <L_MAP_PRART>-COND_POSID.
        EF_PRART = <L_MAP_PRART>-PRART.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDMETHOD.


METHOD ASSIGN_PROFIT_CENTER.

  DATA:
    LS_SD_COND TYPE  ZCL_SDSSD_SALESORDER_ENH=>TS_DETER_PRCTR,
    LS_CM_COND TYPE  ZCL_SDSCM_ENHANCEMENT=>TS_DETER_PRCTR.

  DATA:
    LF_ITMTY TYPE ZSDSDE_PS_ITMTY,
    LF_ACTTY TYPE ZSDSDE_PS_ACTTY.

* Initialize Output
  CLEAR EF_PRCTR.

  IF IS_FROM_SFDC( IF_PSPID ).

*   -------------------
*   Overhaul Project
*   -------------------
    IF IF_POSID+2(1) EQ GC_OVERHAUL.

*     Only LV 4
      IF STRLEN( IF_POSID ) GE 22 ##NUMBER_OK.
*       Determine Profit center
        CLEAR LS_CM_COND.
        LS_CM_COND-SALES_ORG_SD    = IF_VKORG.
        LS_CM_COND-PROCESS_TYPE    = IF_PROCESS_TYPE.
        LS_CM_COND-SALES_OFFICE_SD = IF_VKBUR.
        LS_CM_COND-SALES_GROUP_SD  = IF_VKGRP.
        EF_PRCTR =  ZCL_SDSCM_ENHANCEMENT=>DETERMINE_PROFIT_CENTER( LS_CM_COND ).
      ENDIF.

*   -------------------
*   Non Overhaul Project
*   -------------------
    ELSE.
      IF STRLEN( IF_POSID ) GE 15 ##NUMBER_OK.
        LF_ITMTY = IF_POSID+13(2).
      ENDIF.
      IF STRLEN( IF_POSID ) GE 18 ##NUMBER_OK.
        LF_ACTTY = IF_POSID+16(2).
      ENDIF.
*     Check Item Type needs mapping?
      READ TABLE GT_ITMTY_PRCTR ASSIGNING FIELD-SYMBOL(<L_ITMTY_PRCTR>)
                                WITH KEY ITMTY = LF_ITMTY
                                BINARY SEARCH.
      IF SY-SUBRC EQ 0 AND
         LF_ACTTY IN <L_ITMTY_PRCTR>-ACTTY.
*       Determine Profit center
        CLEAR LS_SD_COND.
        LS_SD_COND-VKORG = IF_VKORG.
        LS_SD_COND-AUART = IF_AUART.
        LS_SD_COND-VKBUR = IF_VKBUR.
        LS_SD_COND-VKGRP = IF_VKGRP.
        EF_PRCTR =  ZCL_SDSSD_SALESORDER_ENH=>DETERMINE_PROFIT_CENTER( LS_SD_COND ).
      ELSE.
        EF_PRCTR = GF_DEFT_PRCTR.
      ENDIF.
    ENDIF.
  ELSE.
*   Determine From Request Cost Ceter
    SELECT PRCTR
      FROM CSKS
     WHERE KOKRS EQ @IF_KOKRS
       AND KOSTL EQ @IF_AKSTL
       AND DATBI GE @SY-DATUM
       AND DATAB LE @SY-DATUM
     ORDER BY DATBI ASCENDING
      INTO @EF_PRCTR
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD ASSIGN_PROJECT_RESULT.

* Update Response Project data
  CS_RESPONSE-PSPID = IS_PROJDEF-PROJDEF-PSPID.
  CS_RESPONSE-POST1 = IS_PROJDEF-PROJDEF-POST1.
  ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_DATE_TIME_TO_ISO(
    EXPORTING
      IF_DATUM = IS_PROJDEF-PROJDEF-PLFAZ
    IMPORTING
      EF_OUTPUT = CS_RESPONSE-PLFAZ ).
  ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_DATE_TIME_TO_ISO(
    EXPORTING
      IF_DATUM = IS_PROJDEF-PROJDEF-PLSEZ
    IMPORTING
      EF_OUTPUT = CS_RESPONSE-PLSEZ ).
  CS_RESPONSE-USR00 = IS_PROJDEF-PROJDEF-USR00.
  CS_RESPONSE-USR01 = IS_PROJDEF-PROJDEF-USR01.
  CS_RESPONSE-USR02 = IS_PROJDEF-PROJDEF-USR02.
  CS_RESPONSE-USR03 = IS_PROJDEF-PROJDEF-USR03.
  CS_RESPONSE-USR04 = IS_PROJDEF-PROJDEF-USR04.
  CS_RESPONSE-USE04 = IS_REQUEST-USE04.
  CS_RESPONSE-USR10 = IS_PROJDEF-PROJDEF-USR10.
  CS_RESPONSE-VKBUR = IS_REQUEST-VKBUR.
  CS_RESPONSE-VKGRP = IS_REQUEST-VKGRP.
  CS_RESPONSE-PRJTY = IS_REQUEST-PRJTY.
  CS_RESPONSE-RESP_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_SUCCESS.

  CASE IS_PROJDEF-MODE.
    WHEN GC_CREATE.
*     Text-i01: Project definition has been created.
      CS_RESPONSE-RESP_MESSAGE = TEXT-I01.
    WHEN GC_CHANGE.
*     Text-i02: Project definition has been changed.
      CS_RESPONSE-RESP_MESSAGE = TEXT-I02.
  ENDCASE.

ENDMETHOD.


METHOD ASSIGN_STUFE.

  DATA:
    LS_WBSLV  TYPE  ZCL_SDSPS_PROJECT=>TS_WBSLV.

  DATA:
    LF_INVALID TYPE  FLAG,
    LF_TEXT    TYPE  TEXT50.


* Initialize Output
  CLEAR EF_STUFE.

* Get WBS Levels
  ZCL_SDSPS_PROJECT=>GET_WBSELEM_LEVELS(
    EXPORTING
      IF_POSID   = IF_POSID
    IMPORTING
      ES_WBSLV   = LS_WBSLV
      EF_INVALID = LF_INVALID ).

  IF LF_INVALID IS NOT INITIAL.
*   Error: Cannot determine WBS Level for &1.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO  = '017'
        IF_MSGV1  = IF_POSID
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

  IF IF_STUFE IS NOT INITIAL AND
     LS_WBSLV-STUFE NE IF_STUFE.
    LF_TEXT = IF_STUFE.
*   Error: WBSlevel &1 mismatches with WBS &2.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO  = '018'
        IF_MSGV1  = LF_TEXT
        IF_MSGV2  = IF_POSID
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

* Assign Result
  EF_STUFE = LS_WBSLV-STUFE.

ENDMETHOD.


METHOD ASSIGN_VERNR.

* Initialize Output
  CLEAR: EF_VERNR.

  SELECT SINGLE VERNR
    FROM ZSDSPSC003
   WHERE VKGRP EQ @IF_VKGRP
     AND ZDEL_FLG EQ @SPACE
    INTO @EF_VERNR.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_WBS_POST1.

* Initialize Output
  CLEAR EF_POST1.

  CASE IF_STUFE.

    WHEN 1.
      IF IF_POST1 IS INITIAL.
*       Use Project Description
        SELECT SINGLE POST1
          FROM PROJ
         WHERE PSPID EQ @IF_POSID
          INTO @EF_POST1.
        IF SY-SUBRC NE 0.
          RETURN.
        ENDIF.
      ELSE.
        EF_POST1 = IF_POST1.
      ENDIF.

    WHEN 2.
*     Get Item Type Desc
      SELECT SINGLE DESCP
        FROM ZSDSPSC001
       WHERE PRJTY EQ @IF_POSID+2(1)
         AND ITMTY EQ @IF_POSID+13(2)
        INTO @DATA(LF_DESCP).
      IF SY-SUBRC NE 0.
        RETURN.
      ENDIF.
      EF_POST1 = LF_DESCP.

    WHEN 3.
*     Get Activity Type Desc
      SELECT SINGLE DESCP
        FROM ZSDSPSC002
       WHERE PRJTY EQ @IF_POSID+2(1)
         AND ITMTY EQ @IF_POSID+13(2)
         AND ACTTY EQ @IF_POSID+16(2)
        INTO @LF_DESCP.
      IF SY-SUBRC NE 0.
        RETURN.
      ENDIF.
      EF_POST1 = LF_DESCP.

  ENDCASE.

ENDMETHOD.


METHOD ASSIGN_WBS_RESULT.

  DATA:
    LS_RETURN   TYPE  BAPIRET2,
    LS_WBS_RESP TYPE ZSDSPSS003.


  CLEAR LS_RETURN.

* ----------------------
* Determine Result Message
* ----------------------
* Success
  IF IT_RETURN IS INITIAL.
    CASE IF_MODE.
      WHEN GC_CREATE.
*       Message: WBS Element has been created.
        LS_RETURN-TYPE   = 'S'.
        LS_RETURN-ID     = 'ZSDSPS01'.
        LS_RETURN-NUMBER = '022'.
      WHEN GC_CHANGE.
*       Message: WBS Element has been changed.
        LS_RETURN-TYPE   = 'S'.
        LS_RETURN-ID     = 'ZSDSPS01'.
        LS_RETURN-NUMBER = '023'.
    ENDCASE.
* Error
  ELSE.
*   Get last Error message
    LOOP AT IT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                      WHERE ( TYPE EQ 'X' OR
                              TYPE EQ 'A' OR
                              TYPE EQ 'E' )   ##NEEDED.
    ENDLOOP.
    IF SY-SUBRC NE 0.
*     Get 1st Message
      READ TABLE IT_RETURN ASSIGNING <L_RETURN>
                           INDEX 1.
    ENDIF.
    IF SY-SUBRC EQ 0.
      LS_RETURN = <L_RETURN>.
    ENDIF.
  ENDIF.

* Assign WBS Result
  LOOP AT IT_WBS ASSIGNING FIELD-SYMBOL(<L_WBS>)
                 WHERE MODE EQ IF_MODE
                   AND WBS_REQ IS NOT INITIAL. "Not Auto Gen data
    CLEAR LS_WBS_RESP.
    IF LS_RETURN-TYPE = 'S'.
      LS_WBS_RESP-POSID  = <L_WBS>-WBS-POSID.
      LS_WBS_RESP-POST1  = <L_WBS>-WBS-POST1.
      ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_DATE_TIME_TO_ISO(
        EXPORTING
          IF_DATUM  = <L_WBS>-WBS-PSTRT
        IMPORTING
          EF_OUTPUT = LS_WBS_RESP-PSTRT ).
      ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_DATE_TIME_TO_ISO(
        EXPORTING
          IF_DATUM  = <L_WBS>-WBS-PENDE
        IMPORTING
          EF_OUTPUT = LS_WBS_RESP-PENDE ).
      LS_WBS_RESP-PRART = <L_WBS>-WBS-PRART.
      LS_WBS_RESP-STUFE = <L_WBS>-WBS-STUFE.
      LS_WBS_RESP-USR01 = <L_WBS>-WBS-USR01.
      LS_WBS_RESP-USR03 = <L_WBS>-WBS-USR03.
      LS_WBS_RESP-FKSTL = <L_WBS>-WBS-FKSTL.
      LS_WBS_RESP-AKSTL = <L_WBS>-WBS-AKSTL.
      LS_WBS_RESP-ITMTY = <L_WBS>-WBS_REQ-ITMTY.
      LS_WBS_RESP-ACTTY = <L_WBS>-WBS_REQ-ACTTY.
      LS_WBS_RESP-APUP  = <L_WBS>-WBS_REQ-APUP.
      LS_WBS_RESP-RESP_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_SUCCESS.
    ELSE.
      LS_WBS_RESP = <L_WBS>-WBS_REQ.
      LS_WBS_RESP-RESP_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
    ENDIF.
    IF LS_RETURN-MESSAGE IS NOT INITIAL.
      LS_WBS_RESP-RESP_MESSAGE = LS_RETURN-MESSAGE.
    ELSE.
      MESSAGE ID LS_RETURN-ID TYPE 'I'
              NUMBER LS_RETURN-NUMBER
              WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                   LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4
              INTO LS_WBS_RESP-RESP_MESSAGE.
    ENDIF.
    INSERT LS_WBS_RESP INTO TABLE CS_RESPONSE-WBSELEM.
  ENDLOOP.

ENDMETHOD.


METHOD GENERATE_WBS_LV1_3.

  DATA:
    LS_WBS   TYPE  TS_WBS_DATA,
    LS_WBSLV TYPE  ZCL_SDSPS_PROJECT=>TS_WBSLV.

  DATA:
    LF_INVALID TYPE  FLAG,
    LF_STUFE   TYPE  PRPS-STUFE.


* Get WBS Levels
  ZCL_SDSPS_PROJECT=>GET_WBSELEM_LEVELS(
    EXPORTING
      IF_POSID   = IS_WBS-WBS-POSID
    IMPORTING
      ES_WBSLV   = LS_WBSLV
      EF_INVALID = LF_INVALID ).
* Ignore if WBS is not level 4
  IF LF_INVALID IS NOT INITIAL OR
     LS_WBSLV-STUFE NE 4.
    RETURN.
  ENDIF.

* Add 3 Levels
  DO 3 TIMES.

    LF_STUFE = SY-INDEX.

    CLEAR LS_WBS.
    LS_WBS-PSPID = IS_WBS-PSPID.

    LS_WBS-WBS-STUFE = LF_STUFE.
    LS_WBS-WBSX-STUFE = 'X'.

    CASE LF_STUFE.
      WHEN 1.
        LS_WBS-WBS-POSID = LS_WBSLV-POSID_LV1.
      WHEN 2.
        LS_WBS-WBS-POSID = LS_WBSLV-POSID_LV2.
      WHEN 3.
        LS_WBS-WBS-POSID = LS_WBSLV-POSID_LV3.
    ENDCASE.

*   Check Already Exist in List
    READ TABLE CT_WBS TRANSPORTING NO FIELDS
                      WITH KEY WBS-POSID = LS_WBS-WBS-POSID.
    IF SY-SUBRC EQ 0.
*     WBS is in list, ignore it
      CONTINUE.
    ENDIF.

*   Check Mode
    SELECT SINGLE POSID
      FROM PRPS
     WHERE POSID EQ @LS_WBS-WBS-POSID
      INTO @DATA(LF_POSID) ##NEEDED.
    IF SY-SUBRC EQ 0.
      LS_WBS-MODE = GC_CHANGE.
    ELSE.
      LS_WBS-MODE = GC_CREATE.
    ENDIF.

*   Assign Project Type
    LS_WBS-WBS-PRART = IS_WBS-WBS-PRART.
    LS_WBS-WBSX-PRART = 'X'.

    ASSIGN_WBS_POST1(
      EXPORTING
        IF_POSID = LS_WBS-WBS-POSID
        IF_STUFE = LS_WBS-WBS-STUFE
        IF_POST1 = IS_PROJDEF-POST1
      IMPORTING
        EF_POST1 = LS_WBS-WBS-POST1 ).
    LS_WBS-WBSX-POST1 = 'X'.

*   Add New WBS into List
    INSERT LS_WBS INTO TABLE CT_WBS.

  ENDDO.

ENDMETHOD.


METHOD GET_GENC.

  CONSTANTS:
    LC_MAP_PROFL   TYPE  ZSDSDE_PARAM_NAME VALUE 'PROJECT_PROFILE_MAP',
    LC_SEQ_APUP    TYPE  ZSDSDE_PARAM_NAME VALUE 'SEQUENCE_FROM_APUP',
    LC_MAP_PRART   TYPE  ZSDSDE_PARAM_NAME VALUE 'WBS_PROJECT_TYPE_MAP',
    LC_LOB_PRJTY   TYPE  ZSDSDE_PARAM_NAME VALUE 'LOB_PROJECT_TYPE',
    LC_LOB_ITMTY   TYPE  ZSDSDE_PARAM_NAME VALUE 'LOB_ITEM_TYPE',
    LC_LOB_ACTTY   TYPE  ZSDSDE_PARAM_NAME VALUE 'LOB_ACTIVITY_TYPE',
    LC_ITMTY_PRCTR TYPE  ZSDSDE_PARAM_NAME VALUE 'ITEM_TYPE_MAP_PRCTR',
    LC_DEFT_PRCTR  TYPE  ZSDSDE_PARAM_NAME VALUE 'DEFAULT_PRCTR'.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LS_MAP_PROFL   TYPE  TS_MAP_PROFL,
    LS_SEQ_APUP    TYPE  TS_SEQ_APUP,
    LS_MAP_PRART   TYPE  TS_MAP_PRART,
    LS_ITMTY_PRCTR TYPE  TS_ITMTY_PRCTR.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSPS_PROJECT_STRUC_SERV'.


* Initialize Output
  CLEAR: GT_MAP_PROFL,
         GT_SEQ_APUP,
         GT_MAP_PRART,
         GT_LOB_PRJTY,
         GT_LOB_ITMTY,
         GT_LOB_ACTTY,
         GT_ITMTY_PRCTR,
         GF_DEFT_PRCTR.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.


* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Project Profile Mapping
*     ------------------------------------
      WHEN LC_MAP_PROFL.
        CLEAR LS_MAP_PROFL.
        LS_MAP_PROFL-PROFL = <L_GENC>-PARAM_EXT.
        READ TABLE GT_MAP_PROFL ASSIGNING FIELD-SYMBOL(<L_MAP_PROFL>)
                                WITH KEY PROFL = LS_MAP_PROFL-PROFL
                                BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_MAP_PROFL INTO TABLE GT_MAP_PROFL
                              ASSIGNING <L_MAP_PROFL>.
        ENDIF.

        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE <L_MAP_PROFL>-COND_PSPID.

*     ------------------------------------
*     WBS Sequence no from APUP
*     ------------------------------------
      WHEN LC_SEQ_APUP.
        CLEAR LS_SEQ_APUP.
        LS_SEQ_APUP-ITMTY = <L_GENC>-PARAM_EXT.
        READ TABLE GT_SEQ_APUP ASSIGNING FIELD-SYMBOL(<L_SEQ_APUP>)
                                WITH KEY ITMTY = LS_SEQ_APUP-ITMTY
                                BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_SEQ_APUP INTO TABLE GT_SEQ_APUP
                              ASSIGNING <L_SEQ_APUP>.
        ENDIF.

        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE <L_SEQ_APUP>-ACTTY.

*     ------------------------------------
*     WBS Project Type Mapping
*     ------------------------------------
      WHEN LC_MAP_PRART.
        CLEAR LS_MAP_PRART.
        LS_MAP_PRART-PRART = <L_GENC>-PARAM_EXT.
        READ TABLE GT_MAP_PRART ASSIGNING FIELD-SYMBOL(<L_MAP_PRART>)
                                WITH KEY PRART = LS_MAP_PRART-PRART
                                BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_MAP_PRART INTO TABLE GT_MAP_PRART
                              ASSIGNING <L_MAP_PRART>.
        ENDIF.

        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE <L_MAP_PRART>-COND_POSID.

*     ------------------------------------
*     LOB From Project Type
*     ------------------------------------
      WHEN LC_LOB_PRJTY.
        INSERT VALUE #( PRJTY = <L_GENC>-PARAM_EXT
                        LOB   = <L_GENC>-VALUE_LOW )
               INTO TABLE GT_LOB_PRJTY.

*     ------------------------------------
*     LOB From Item Type
*     ------------------------------------
      WHEN LC_LOB_ITMTY.
        INSERT VALUE #( ITMTY = <L_GENC>-PARAM_EXT
                        LOB   = <L_GENC>-VALUE_LOW )
               INTO TABLE GT_LOB_ITMTY.

*     ------------------------------------
*     LOB From Activity
*     ------------------------------------
      WHEN LC_LOB_ACTTY.
        INSERT VALUE #( ITMTY = <L_GENC>-PARAM_EXT
                        ACTTY = <L_GENC>-VALUE_LOW
                        LOB   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_LOB_ACTTY.

*     ------------------------------------
*     Item Type needs Profit Center mapping
*     ------------------------------------
      WHEN LC_ITMTY_PRCTR.
        CLEAR LS_ITMTY_PRCTR.
        LS_ITMTY_PRCTR-ITMTY = <L_GENC>-PARAM_EXT.
        READ TABLE GT_ITMTY_PRCTR ASSIGNING FIELD-SYMBOL(<L_ITMTY_PRCTR>)
                                  WITH KEY ITMTY = LS_ITMTY_PRCTR-ITMTY
                                  BINARY SEARCH.
        IF SY-SUBRC NE 0.
          INSERT LS_ITMTY_PRCTR INTO TABLE GT_ITMTY_PRCTR
                                ASSIGNING <L_ITMTY_PRCTR>.
        ENDIF.

        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE <L_ITMTY_PRCTR>-ACTTY.

*     ------------------------------------
*     Default Profit Center
*     ------------------------------------
      WHEN LC_DEFT_PRCTR.
        GF_DEFT_PRCTR = <L_GENC>-VALUE_LOW.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD GET_NEW_POSID.

  DATA:
    LF_NUM2   TYPE  N LENGTH 2,
    LF_NUM3   TYPE  N LENGTH 3,
    LF_POSID TYPE  PRPS-POSID.


* Get Current Highest
  READ TABLE GT_NEW_POSID ASSIGNING FIELD-SYMBOL(<L_NEW_POSID>)
                          WITH KEY LOW = IF_LOW
                                   HIGH = IF_HIGH
                          BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LF_POSID = <L_NEW_POSID>-POSID.

    CASE LF_POSID(1).
      WHEN 'P'.
*       Add next number
        LF_NUM3 = LF_POSID+19(3) + 1.
        LF_POSID+19(3) = LF_NUM3.
      WHEN 'C'.
*       Add next number
        LF_NUM2 = LF_POSID+21(2) + 1.
        LF_POSID+21(2) = LF_NUM2.
    ENDCASE.
*   Update New number
    <L_NEW_POSID>-POSID = LF_POSID.
  ELSE.
    CLEAR LF_POSID.
    SELECT MAX( POSID ) AS POSID
      FROM PRPS
     WHERE POSID BETWEEN @IF_LOW
                     AND @IF_HIGH
      INTO @LF_POSID.
    IF SY-SUBRC NE 0 OR
       LF_POSID IS INITIAL.
      LF_POSID = IF_LOW.
    ELSE.
      CASE LF_POSID(1).
        WHEN 'P'.
*         Add next number
          LF_NUM3 = LF_POSID+19(3) + 1.
          LF_POSID+19(3) = LF_NUM3.
        WHEN 'C'.
*         Add next number
          LF_NUM2 = LF_POSID+21(2) + 1.
          LF_POSID+21(2) = LF_NUM2.
      ENDCASE.
    ENDIF.
*   Insert Buffer
    INSERT VALUE #( LOW = IF_LOW
                    HIGH = IF_HIGH
                    POSID = LF_POSID )
           INTO TABLE GT_NEW_POSID.
  ENDIF.

* Assign Output
  EF_POSID = LF_POSID.

ENDMETHOD.


METHOD GET_NEW_POSID_C3.

  DATA:
    LF_POSID_LOW  TYPE  PRPS-POSID,
    LF_POSID_HIGH TYPE  PRPS-POSID.


* Initialize Output
  CLEAR EF_POSID.

* WBS Lv2 is Required
  IF IF_POSID IS INITIAL.
*   Error: Cannot find WBS Level 2 from REFID &1.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO    = '014'
        IF_MSGV1    = IF_USR01
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

* eMemo is Required
  IF IF_USR03 IS INITIAL.
*   Error: eMemo is missing for REFID &1.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO    = '030'
        IF_MSGV1    = IF_USR01
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

  DATA(LF_POSID) = GET_WBSELEM_FROM_EMEMO( IF_REFID = IF_USR01
                                           IF_EMEMO = IF_USR03 ).
  IF LF_POSID IS NOT INITIAL.
*   Error: WBS Element already exists for REFID &1 eMemo &2.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO    = '015'
        IF_MSGV1    = IF_USR01
        IF_MSGV2    = IF_USR03
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

  LF_POSID_LOW  = IF_POSID && '-01'.
  LF_POSID_HIGH = IF_POSID && '-99'.

  GET_NEW_POSID(
    EXPORTING
      IF_LOW   = LF_POSID_LOW
      IF_HIGH  = LF_POSID_HIGH
    IMPORTING
      EF_POSID = LF_POSID ).

* Assign result
  EF_POSID = LF_POSID.

ENDMETHOD.


METHOD GET_NEW_POSID_P4.

  DATA:
    LF_USE_APUP   TYPE  FLAG,
    LF_POSID_LOW  TYPE  PRPS-POSID,
    LF_POSID_HIGH TYPE  PRPS-POSID,
    LF_POSID      TYPE  PRPS-POSID.


* Initialize Output
  CLEAR EF_POSID.

* Item Type is required
  IF IF_ITMTY IS INITIAL.
*   Error: ItemType is required.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO  = '019'
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

* Activity Type is required
  IF IF_ACTTY IS INITIAL.
*   Error: Activity is required.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO  = '020'
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

  CLEAR LF_USE_APUP.
  LOOP AT GT_SEQ_APUP ASSIGNING FIELD-SYMBOL(<L_SEQ_APUP>)
                      WHERE ITMTY EQ IF_ITMTY.
    IF IF_ACTTY IN <L_SEQ_APUP>-ACTTY.
      LF_USE_APUP = 'X'.
    ENDIF.
  ENDLOOP.

  IF LF_USE_APUP EQ 'X'.

*   Activity Type is required
    IF IF_APUP IS INITIAL.
*     Error: APUP is required.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO  = '021'
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.

    LF_POSID = IF_PSPID && '-' && IF_ITMTY && '-' && IF_ACTTY && '-'  && IF_APUP.

*   WBS Must not exists
    SELECT SINGLE POSID
      FROM PRPS
     WHERE POSID EQ @LF_POSID
      INTO @LF_POSID.
    IF SY-SUBRC EQ 0.
*     Error: WBS Element &1 already exists.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO  = '013'
          IF_MSGV1  = LF_POSID
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.

  ELSE.
    LF_POSID_LOW  = IF_PSPID && '-' && IF_ITMTY && '-' && IF_ACTTY && '-001'.
    LF_POSID_HIGH = IF_PSPID && '-' && IF_ITMTY && '-' && IF_ACTTY && '-999'.

    GET_NEW_POSID(
      EXPORTING
        IF_LOW  = LF_POSID_LOW
        IF_HIGH = LF_POSID_HIGH
      IMPORTING
        EF_POSID = LF_POSID ).

  ENDIF.

* Assign result
  EF_POSID = LF_POSID.

ENDMETHOD.


METHOD GET_NEW_PSPID.

  DATA
    LT_RETURN TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LF_GJAHR      TYPE  GJAHR,
    LF_NUM        TYPE  N LENGTH 6,
    LF_PSPID_LOW  TYPE  PROJ-PSPID,
    LF_PSPID_HIGH TYPE  PROJ-PSPID,
    LF_PSPID      TYPE  PROJ-PSPID.


* Initialize Output
  CLEAR: EF_PSPID.

* Get Current Fiscal Year
  CALL FUNCTION 'BAPI_PRF_GET_FISCAL_YEAR'
    EXPORTING
      IV_COMPANY_CODE = IF_BUKRS
      IV_DATE         = SY-DATUM
    IMPORTING
      EV_FISCAL_YEAR  = LF_GJAHR
    TABLES
      RETURN          = LT_RETURN.
  IF LT_RETURN IS NOT INITIAL.
*   Error cannot find fiscal year
    RETURN.
  ENDIF.

* Assign Low Value
  LF_PSPID_LOW = 'P-' && IF_PRJTY && '-' && LF_GJAHR+2(2) && '000001'.
  LF_PSPID_HIGH = 'P-' && IF_PRJTY && '-' && LF_GJAHR+2(2) && '999999'.

* Get Current Highest
  CLEAR LF_PSPID.
  SELECT MAX( PSPID ) AS PSPID
    FROM PROJ
   WHERE PSPID BETWEEN @LF_PSPID_LOW
                   AND @LF_PSPID_HIGH
    INTO @LF_PSPID.
  IF SY-SUBRC NE 0 OR
     LF_PSPID IS INITIAL.
    LF_PSPID = LF_PSPID_LOW.
  ELSE.
*   Add next number
    LF_NUM = LF_PSPID+6(6) + 1.
    LF_PSPID+6(6) = LF_NUM.
  ENDIF.

* Assign result
  EF_PSPID = LF_PSPID.

ENDMETHOD.


METHOD GET_PROJDEF_FROM_REFID.

* Initiailize Output
  CLEAR: RF_PSPID.

  SELECT PSPID
    FROM PROJ
   WHERE USR00 EQ @IF_REFID
    ORDER BY PSPID ASCENDING
    INTO @RF_PSPID
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD GET_WBSELEM_FROM_EMEMO.

* Initiailize Output
  CLEAR: RF_POSID.

  SELECT POSID
    FROM PRPS
   WHERE USR01 EQ @IF_REFID
     AND USR03 EQ @IF_EMEMO
    ORDER BY POSID ASCENDING
    INTO @RF_POSID
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD GET_WBSELEM_FROM_REFID.

  DATA:
    LR_STUFE  TYPE  RANGE OF PRPS-STUFE.


* Initiailize Output
  CLEAR: RF_POSID.

* Determine Level for USR01 Key
  CASE IF_PSPID(1).
    WHEN 'P'.
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW    = 4 )
             INTO TABLE LR_STUFE.
    WHEN 'C'.
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW    = 2 )
             INTO TABLE LR_STUFE.
  ENDCASE.

  SELECT A~POSID
    FROM PRPS AS A
           INNER JOIN PROJ AS B
             ON  B~PSPNR = A~PSPHI
   WHERE A~USR01 EQ @IF_REFID
     AND A~STUFE IN @LR_STUFE
     AND B~PSPID EQ @IF_PSPID
    ORDER BY A~POSID ASCENDING
    INTO @RF_POSID
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD INITIALIZE_DATA.
* Initialize Variables
  CLEAR: GT_NEW_POSID.

  CREATE OBJECT GREF_VALIDATE.

  GET_GENC( ).
ENDMETHOD.


METHOD IS_FROM_SFDC.

  CLEAR RF_RESULT.

* Check If data is from Salesforce
* - Check Project starting with P
  IF IF_PSPID(1) EQ 'P'.
    RF_RESULT = ABAP_TRUE.
  ENDIF.

ENDMETHOD.


METHOD MAINTAIN_PROJECT_STRUCTURE.

  DATA:
    LT_WBS        TYPE  TT_WBS_DATA,
    LT_WBS_CREATE TYPE  ZCL_SDSPS_PROJECT=>TT_WBSELEM,
    LT_WBS_CHANGE TYPE  ZCL_SDSPS_PROJECT=>TT_WBS_UPD,
    LT_RETURN     TYPE  BAPIRET2_TAB.

  DATA:
    LS_PROJDEF TYPE  TS_PROJDEF_DATA,
    LS_RETURN  TYPE  BAPIRETURN1.


* Initialize Output
  CLEAR: ES_RESPONSE.

* -------------------
* Validate Request
* -------------------
  VALIDATE_REQUEST(
    EXPORTING
      IS_REQUEST  = IS_REQUEST
    IMPORTING
      ES_PROJDEF  = LS_PROJDEF
      ET_WBS      = LT_WBS
    CHANGING
      CS_RESPONSE = ES_RESPONSE ).
  IF ES_RESPONSE-RESP_STATUS IS NOT INITIAL.
    RETURN.
  ENDIF.

  CASE LS_PROJDEF-MODE.
*   -------------------
*   Create Project
*   -------------------
    WHEN GC_CREATE.
      ZCL_SDSPS_PROJECT=>CREATE_PROJECTDEF( EXPORTING IS_PROJECTDEF = LS_PROJDEF-PROJDEF
                                                      IF_TEST       = SPACE
                                            IMPORTING ES_RETURN     = LS_RETURN ).

*   -------------------
*   Change Project
*   -------------------
    WHEN GC_CHANGE.
      ZCL_SDSPS_PROJECT=>CHANGE_PROJECTDEF( EXPORTING IS_PROJECTDEF  = LS_PROJDEF-PROJDEF
                                                      IS_PROJECTDEFX = LS_PROJDEF-PROJDEFX
                                                      IF_TEST        = SPACE
                                            IMPORTING ES_RETURN      = LS_RETURN ).

  ENDCASE.
* Project Processing error
  IF LS_RETURN IS NOT INITIAL.
*   Error from project processing
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGID    = LS_RETURN-ID
        IF_MSGNO    = LS_RETURN-NUMBER
        IF_MSGV1    = LS_RETURN-MESSAGE_V1
        IF_MSGV2    = LS_RETURN-MESSAGE_V2
        IF_MSGV3    = LS_RETURN-MESSAGE_V3
        IF_MSGV4    = LS_RETURN-MESSAGE_V4
        IF_MESSAGE  = LS_RETURN-MESSAGE
      CHANGING
        CS_RESPONSE = ES_RESPONSE ).
    RETURN.
  ENDIF.

* Update Project Result
  ASSIGN_PROJECT_RESULT(
    EXPORTING
      IS_REQUEST  = IS_REQUEST
      IS_PROJDEF  = LS_PROJDEF
    CHANGING
      CS_RESPONSE = ES_RESPONSE ).

* -------------------
* Assign WBS Create/Change
* -------------------
  CLEAR: LT_WBS_CREATE,
         LT_WBS_CHANGE.
  LOOP AT LT_WBS ASSIGNING FIELD-SYMBOL(<L_WBS>).
    CASE <L_WBS>-MODE.
      WHEN GC_CREATE.
        INSERT <L_WBS>-WBS INTO TABLE LT_WBS_CREATE.
      WHEN GC_CHANGE.
        INSERT VALUE #( WBSELEM  = <L_WBS>-WBS
                        WBSELEMX = <L_WBS>-WBSX )
               INTO TABLE LT_WBS_CHANGE.
    ENDCASE.
  ENDLOOP.

* -------------------
* Create WBS
* -------------------
  IF LT_WBS_CREATE IS NOT INITIAL.

*   Wait Project Unlock to further process
    WAIT_PROJECT_UNLOCK( LS_PROJDEF-PROJDEF-PSPID ).

    ZCL_SDSPS_PROJECT=>CREATE_WBSELEM( EXPORTING IF_PSPID          = LS_PROJDEF-PROJDEF-PSPID
                                                 IT_WBSELEM        = LT_WBS_CREATE
                                                 IF_TEST           = ' '
                                                 IF_SKIP_CHK_PSPID = ' '
                                       IMPORTING ET_RETURN         = LT_RETURN ).
    ASSIGN_WBS_RESULT(
      EXPORTING
        IT_WBS      = LT_WBS
        IF_MODE     = GC_CREATE
        IT_RETURN   = LT_RETURN
      CHANGING
        CS_RESPONSE = ES_RESPONSE ).

  ENDIF.

* -------------------
* Change WBS
* -------------------
  IF LT_WBS_CHANGE IS NOT INITIAL.

*   Wait Project Unlock to further process
    WAIT_PROJECT_UNLOCK( LS_PROJDEF-PROJDEF-PSPID ).

    ZCL_SDSPS_PROJECT=>CHANGE_WBSELEM( EXPORTING IF_PSPID   = LS_PROJDEF-PROJDEF-PSPID
                                                 IT_WBSELEM = LT_WBS_CHANGE
                                                 IF_TEST    = ' '
                                       IMPORTING ET_RETURN  = LT_RETURN ).
    ASSIGN_WBS_RESULT(
      EXPORTING
        IT_WBS      = LT_WBS
        IF_MODE     = GC_CHANGE
        IT_RETURN   = LT_RETURN
      CHANGING
        CS_RESPONSE = ES_RESPONSE ).
  ENDIF.

ENDMETHOD.


METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSPS_PROJECT_STRUC_SERV
*  Creation Date      : 21.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : PSI006, PSI007, PSI008 and PSI009
*  Description        : This is a Processing class of REST interface
*                       PSI006_1 to maintain project structure
*  Purpose            : To maintain Project Structure
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA:
    LS_REQUEST  TYPE  ZSDSPSS002.

  FIELD-SYMBOLS:
    <L_RESPONSE>  TYPE  ZSDSPSS002.


* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE,
         EF_HTTP_ERROR.

  LS_REQUEST = IREF_REQUEST_DATA->*.

  ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
  IF SY-SUBRC NE 0.
*   Critical error
    RETURN.
  ENDIF.

* Lock for processing
* - Only 1 session for interface can be executed at a time
  IF NOT LOCK_PROCESSING( ).
*   Error: The interface is currently processing other data. Please try again later.
    EF_STATUS     = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
    MESSAGE E028(ZSDSCA01) INTO EF_MESSAGE.
    EF_HTTP_ERROR = 'X'.
    RETURN.
  ENDIF.

* Maintain Project Structure
  MAINTAIN_PROJECT_STRUCTURE(
    EXPORTING
      IS_REQUEST = LS_REQUEST
    IMPORTING
      ES_RESPONSE = <L_RESPONSE> ).

* Unlock Processing
  UNLOCK_PROCESSING( ).

* Assign Log status from Response structure
  IF <L_RESPONSE>-RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*   Error at header level, Nothing success => HTTP error
    EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
    EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
*    EF_HTTP_ERROR = 'X'.
  ELSE.
    LOOP AT <L_RESPONSE>-WBSELEM ASSIGNING FIELD-SYMBOL(<L_WBSELEM>)
                                 WHERE RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*     Error at item level, Something sucess => Must not HTTP error
      EF_STATUS  = <L_WBSELEM>-RESP_STATUS.
      EF_MESSAGE = <L_WBSELEM>-RESP_MESSAGE.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
      EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_AND_ASSIGN_POSID.

* Initialize Output
  CLEAR: EF_POSID,
         EF_MODE.

* --------------------------------------
* Unique Design:-
* - Project P Lv 4 --> USR01 is unique
*                      USR02 is SF Project Code
* - Project C Lv 2 --> USR01 is unique (Unique within LV2)
* - Project C Lv 3 --> USR01 is its higher(lv2) USR01 value
*                      USR03 is unique
* --------------------------------------

* ------------------------
* Check E-Memo (USR03)
* ------------------------
* Only for Project C
  IF IF_POSID(1) EQ 'C' AND
     IF_USR03 IS NOT INITIAL.
    DATA(LF_POSID) = GET_WBSELEM_FROM_EMEMO( IF_REFID = IF_USR01
                                             IF_EMEMO = IF_USR03 ).

*   WBS Element mismatched
    IF IF_POSID IS NOT INITIAL AND
       LF_POSID IS NOT INITIAL AND
       IF_POSID NE LF_POSID.
*     Error: WBS Element &1 is not matched with REFID &2 eMemo &3.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO    = '016'
          IF_MSGV1    = IF_POSID
          IF_MSGV2    = IF_USR01
          IF_MSGV3    = IF_USR03
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.
*   eMemo is used
    IF IF_POSID IS INITIAL AND
       LF_POSID IS NOT INITIAL.
*     Error: WBS Element already exists for REFID &1 eMemo &2.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO    = '015'
          IF_MSGV1    = IF_USR01
          IF_MSGV2    = IF_USR03
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.
  ENDIF.

* ------------------------
* Check Reference ID
* ------------------------
* Reference ID is required
  IF IF_USR01 IS INITIAL.
*   REFID can blank for K2 WBS Lv1
    IF NOT ( IF_POSID IS NOT INITIAL AND
             IF_POSID(1) EQ 'C' AND
             STRLEN( IF_POSID ) EQ 11 ) ##NUMBER_OK.
*     Error: Specify REFID to process
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO    = '009'
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.
  ELSE.
*   Get WBS from REFID
    LF_POSID = GET_WBSELEM_FROM_REFID( IF_REFID = IF_USR01
                                       IF_PSPID = IF_PSPID ).

*   Only for P LV4 or C LV2
    IF ( IF_PSPID(1) EQ 'P' ) OR
       ( IF_PSPID(1) EQ 'C' AND IF_USR03 IS INITIAL ).
*     WBS Element mismatched
      IF IF_POSID IS NOT INITIAL AND
         LF_POSID IS NOT INITIAL AND
         IF_POSID NE LF_POSID.
*       Error: WBS Element &1 is not matched with REFID &2.
        ASSIGN_MESSAGE(
          EXPORTING
            IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
            IF_MSGNO    = '010'
            IF_MSGV1    = IF_POSID
            IF_MSGV2    = IF_USR01
          CHANGING
            CS_RESPONSE = CS_RESPONSE ).
        RETURN.
      ENDIF.

*     REFID is used
      IF IF_USR01 IS NOT INITIAL AND
         IF_POSID IS INITIAL AND
         LF_POSID IS NOT INITIAL.
*       Error: Reference ID &1 is already used for wbs element &2.
        ASSIGN_MESSAGE(
          EXPORTING
            IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
            IF_MSGNO    = '011'
            IF_MSGV1    = IF_USR01
            IF_MSGV2    = LF_POSID
          CHANGING
            CS_RESPONSE = CS_RESPONSE ).
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

* Generate new WBS Element
  IF IF_POSID IS INITIAL.

    EF_MODE = GC_CREATE.

    IF IS_FROM_SFDC( IF_PSPID ).
*     Generate WBS Level 4
      GET_NEW_POSID_P4(
        EXPORTING
          IF_PSPID    = IF_PSPID
          IF_ITMTY    = IF_ITMTY
          IF_ACTTY    = IF_ACTTY
          IF_APUP     = IF_APUP
        IMPORTING
          EF_POSID    = EF_POSID
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      IF CS_RESPONSE-RESP_STATUS IS NOT INITIAL.
        RETURN.
      ENDIF.

    ELSE.
*     Generate WBS Level 3
      GET_NEW_POSID_C3(
        EXPORTING
          IF_POSID    = LF_POSID
          IF_USR01    = IF_USR01
          IF_USR03    = IF_USR03
        IMPORTING
          EF_POSID    = EF_POSID
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      IF CS_RESPONSE-RESP_STATUS IS NOT INITIAL.
        RETURN.
      ENDIF.

    ENDIF.

*   Error WBS Element not found
    IF EF_POSID IS INITIAL.
*     Error: Cannot generate new WBS Element number.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO    = '012'
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.

  ELSE.
    EF_POSID = IF_POSID.

*   Validate POSID
    SELECT SINGLE USR01,
                  USR03
      FROM PRPS
     WHERE POSID EQ @IF_POSID
      INTO @DATA(LS_PRPS) ##NEEDED.
    IF SY-SUBRC EQ 0.
      EF_MODE = GC_CHANGE.
    ELSE.
      EF_MODE = GC_CREATE.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD VALIDATE_AND_ASSIGN_PSPID.

  DATA:
    LF_INVALID TYPE  FLAG.


* Initialize Output
  CLEAR: EF_PSPID,
         EF_MODE.

* Project Def or Ref ID must specified
  IF IF_PSPID IS INITIAL AND
     IF_USR00 IS INITIAL.
*   Error: Specify ProjectDefinition or SFProjectCode to process
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO  = '003'
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

* -----------------------------
* When SF ID is Specified
* -----------------------------
  IF IF_USR00 IS NOT INITIAL.

    DATA(LF_PSPID) = GET_PROJDEF_FROM_REFID( IF_REFID = IF_USR00 ).

*   Project Def mismatched
    IF IF_PSPID IS NOT INITIAL AND
       LF_PSPID IS NOT INITIAL AND
       IF_PSPID NE LF_PSPID.
*     Error: ProjectDefinition &1 is not matched with SFProjectCode &2.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO  = '004'
          IF_MSGV1  = IF_PSPID
          IF_MSGV2  = IF_USR00
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.

*   SFProjectCode is used
    IF IF_USR00 IS NOT INITIAL AND
       IF_PSPID IS INITIAL AND
       LF_PSPID IS NOT INITIAL.
*     Error: Reference ID &1 is already used for project &2.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO  = '005'
          IF_MSGV1  = IF_USR00
          IF_MSGV2  = LF_PSPID
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.

*   Generate new project def
    IF IF_PSPID IS INITIAL.

      EF_MODE = GC_CREATE.

      GREF_VALIDATE->VALIDATE_DOMAIN_VALUE( EXPORTING IF_DOMNAME = 'ZSDSDM_PRJTY'
                                                      IF_INPUT = IF_PRJTY
                                            IMPORTING EF_INVALID = LF_INVALID ).
      IF LF_INVALID EQ 'X' OR
         IF_PRJTY IS INITIAL.
*       Error: Invalid ProjectType &1.
        ASSIGN_MESSAGE(
          EXPORTING
            IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
            IF_MSGNO  = '006'
            IF_MSGV1  = IF_PRJTY
          CHANGING
            CS_RESPONSE = CS_RESPONSE ).
        RETURN.
      ENDIF.
*     Get New Project Def number
      GET_NEW_PSPID(
        EXPORTING
          IF_PRJTY = IF_PRJTY
        IMPORTING
          EF_PSPID = EF_PSPID ).
*     Error Project Def not found
      IF EF_PSPID IS INITIAL.
*       Error: Cannot generate new Project definition number.
        ASSIGN_MESSAGE(
          EXPORTING
            IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
            IF_MSGNO  = '007'
          CHANGING
            CS_RESPONSE = CS_RESPONSE ).
        RETURN.
      ENDIF.
    ELSE.
      EF_MODE  = GC_CHANGE.
      EF_PSPID = IF_PSPID.
    ENDIF.

* -----------------------------
* When SF ID is not Specified
* -----------------------------
  ELSE.
    EF_PSPID = IF_PSPID.

*   Validate PSPID
    SELECT SINGLE USR00
      FROM PROJ
     WHERE PSPID EQ @EF_PSPID
      INTO @DATA(LF_USR00).
    IF SY-SUBRC EQ 0.
      EF_MODE = GC_CHANGE.
      IF IF_USR00 NE LF_USR00.
*       Error: ProjectDefinition &1 is not matched with SFProjectCode &2.
        ASSIGN_MESSAGE(
          EXPORTING
            IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
            IF_MSGNO  = '004'
            IF_MSGV1  = IF_PSPID
            IF_MSGV2  = IF_USR00
          CHANGING
            CS_RESPONSE = CS_RESPONSE ).
        RETURN.
      ENDIF.

    ELSE.
      EF_MODE = GC_CREATE.
*     Error if from SFDC
      IF IS_FROM_SFDC( IF_PSPID ).
*       Error: Project definition &1 is not valid.
        ASSIGN_MESSAGE(
          EXPORTING
            IF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
            IF_MSGNO  = '008'
            IF_MSGV1  = IF_PSPID
          CHANGING
            CS_RESPONSE = CS_RESPONSE ).
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD VALIDATE_REQUEST.

  DATA:
    LS_WBS     TYPE  TS_WBS_DATA.


* Initialize Output
  CLEAR: ES_PROJDEF,
         ET_WBS.

* ***********************************
* Validate Project Definition Fields
* ***********************************
  VALIDATE_AND_ASSIGN_PSPID(
    EXPORTING
      IF_PSPID    = IS_REQUEST-PSPID
      IF_USR00    = IS_REQUEST-USR00
      IF_PRJTY    = IS_REQUEST-PRJTY
    IMPORTING
      EF_PSPID    = ES_PROJDEF-PROJDEF-PSPID
      EF_MODE     = ES_PROJDEF-MODE
    CHANGING
      CS_RESPONSE = CS_RESPONSE ).
  IF CS_RESPONSE-RESP_STATUS IS NOT INITIAL.
    RETURN.
  ENDIF.
  ES_PROJDEF-PROJDEFX-PSPID = 'X'.

  ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_ISO_TO_DATE_TIME(
    EXPORTING
      IF_INPUT         = IS_REQUEST-PLFAZ
    IMPORTING
      EF_DATUM         = ES_PROJDEF-PROJDEF-PLFAZ
    EXCEPTIONS
      CONVERSION_ERROR = 1
      OTHERS           = 2 ).
  IF SY-SUBRC NE 0.
*   Error: Invalid date value &1.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO    = '001'
        IF_MSGV1    = IS_REQUEST-PLFAZ
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.
  ES_PROJDEF-PROJDEFX-PLFAZ = IS_REQUEST-UPDFLG-PLFAZ.

  ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_ISO_TO_DATE_TIME(
    EXPORTING
      IF_INPUT         = IS_REQUEST-PLSEZ
    IMPORTING
      EF_DATUM         = ES_PROJDEF-PROJDEF-PLSEZ
    EXCEPTIONS
      CONVERSION_ERROR = 1
      OTHERS           = 2 ).
  IF SY-SUBRC NE 0.
*   Error: Invalid date value &1.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO    = '001'
        IF_MSGV1    = IS_REQUEST-PLSEZ
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.
  ES_PROJDEF-PROJDEFX-PLSEZ = IS_REQUEST-UPDFLG-PLSEZ.

  ES_PROJDEF-PROJDEF-POST1  = IS_REQUEST-POST1.
  ES_PROJDEF-PROJDEFX-POST1 = IS_REQUEST-UPDFLG-POST1.

  ES_PROJDEF-PROJDEF-USR00  = IS_REQUEST-USR00.
  ES_PROJDEF-PROJDEFX-USR00 = IS_REQUEST-UPDFLG-USR00.

  ES_PROJDEF-PROJDEF-USR01  = IS_REQUEST-USR01.
  ES_PROJDEF-PROJDEFX-USR01 = IS_REQUEST-UPDFLG-USR01.

  ES_PROJDEF-PROJDEF-USR02  = IS_REQUEST-USR02.
  ES_PROJDEF-PROJDEFX-USR02 = IS_REQUEST-UPDFLG-USR02.

  ES_PROJDEF-PROJDEF-USR03  = IS_REQUEST-USR03.
  ES_PROJDEF-PROJDEFX-USR03 = IS_REQUEST-UPDFLG-USR03.

  ES_PROJDEF-PROJDEF-USR04  = IS_REQUEST-USR04.
  ES_PROJDEF-PROJDEFX-USR04 = IS_REQUEST-UPDFLG-USR04.

  ES_PROJDEF-PROJDEF-USR10  = IS_REQUEST-USR10.
  ES_PROJDEF-PROJDEFX-USR10 = IS_REQUEST-UPDFLG-USR10.

* Determine Responsible Person from Sales Group
  ASSIGN_VERNR(
    EXPORTING
      IF_VKGRP = IS_REQUEST-VKGRP
    IMPORTING
      EF_VERNR = ES_PROJDEF-PROJDEF-VERNR ).
  ES_PROJDEF-PROJDEFX-VERNR = IS_REQUEST-UPDFLG-VKGRP.

* If from SFDC, assign High? value based on No. of storeys
  IF IS_FROM_SFDC( ES_PROJDEF-PROJDEF-PSPID ) AND
     ES_PROJDEF-PROJDEF-USR04 GT 8.
    ES_PROJDEF-PROJDEF-USR11  = 'X'.
    ES_PROJDEF-PROJDEFX-USR11 = 'X'.
  ENDIF.

* Project Profile from GenC Setting
  LOOP AT GT_MAP_PROFL ASSIGNING FIELD-SYMBOL(<L_MAP_PROFL>).
    IF ES_PROJDEF-PROJDEF-PSPID IN <L_MAP_PROFL>-COND_PSPID.
      ES_PROJDEF-PROJDEF-PROFL  = <L_MAP_PROFL>-PROFL.
      ES_PROJDEF-PROJDEFX-PROFL = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF ES_PROJDEF-PROJDEF-PROFL IS INITIAL.
*   Error: Cannot determine project profile.
    ASSIGN_MESSAGE(
      EXPORTING
        IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
        IF_MSGNO    = '002'
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    RETURN.
  ENDIF.

* ***********************************
* Validate WBS Element Fields
* ***********************************
  LOOP AT IS_REQUEST-WBSELEM ASSIGNING FIELD-SYMBOL(<L_WBSELEM>).

    CLEAR LS_WBS.

    LS_WBS-PSPID = ES_PROJDEF-PROJDEF-PSPID.
    LS_WBS-WBS_REQ = <L_WBSELEM>.

    VALIDATE_AND_ASSIGN_POSID(
      EXPORTING
        IF_PSPID    = LS_WBS-PSPID
        IF_POSID    = <L_WBSELEM>-POSID
        IF_USR01    = <L_WBSELEM>-USR01
        IF_USR03    = <L_WBSELEM>-USR03
        IF_ITMTY    = <L_WBSELEM>-ITMTY
        IF_ACTTY    = <L_WBSELEM>-ACTTY
        IF_APUP     = <L_WBSELEM>-APUP
      IMPORTING
        EF_POSID    = LS_WBS-WBS-POSID
        EF_MODE     = LS_WBS-MODE
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    IF CS_RESPONSE-RESP_STATUS IS NOT INITIAL.
      EXIT.
    ENDIF.
    LS_WBS-WBSX-POSID = 'X'.

    LS_WBS-WBS-POST1  = <L_WBSELEM>-POST1.
    LS_WBS-WBSX-POST1 = <L_WBSELEM>-UPDFLG-POST1.

    ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_ISO_TO_DATE_TIME(
      EXPORTING
        IF_INPUT         = <L_WBSELEM>-PSTRT
      IMPORTING
        EF_DATUM         = LS_WBS-WBS-PSTRT
      EXCEPTIONS
        CONVERSION_ERROR = 1
        OTHERS           = 2 ).
    IF SY-SUBRC NE 0.
*     Error: Invalid date value &1.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO    = '001'
          IF_MSGV1    = <L_WBSELEM>-PSTRT
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.
    LS_WBS-WBSX-PSTRT = <L_WBSELEM>-UPDFLG-PSTRT.

    ZCL_SDSCA_REST_INTF_UTILITY=>CONVERT_ISO_TO_DATE_TIME(
      EXPORTING
        IF_INPUT         = <L_WBSELEM>-PENDE
      IMPORTING
        EF_DATUM         = LS_WBS-WBS-PENDE
      EXCEPTIONS
        CONVERSION_ERROR = 1
        OTHERS           = 2 ).
    IF SY-SUBRC NE 0.
*     Error: Invalid date value &1.
      ASSIGN_MESSAGE(
        EXPORTING
          IF_STATUS   = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR
          IF_MSGNO    = '001'
          IF_MSGV1    = <L_WBSELEM>-PENDE
        CHANGING
          CS_RESPONSE = CS_RESPONSE ).
      RETURN.
    ENDIF.
    LS_WBS-WBSX-PENDE = <L_WBSELEM>-UPDFLG-PENDE.

    LS_WBS-WBS-USR01  = <L_WBSELEM>-USR01.
    LS_WBS-WBSX-USR01 = <L_WBSELEM>-UPDFLG-USR01.

    LS_WBS-WBS-USR02  = IS_REQUEST-USR00.
    LS_WBS-WBSX-USR02 = IS_REQUEST-UPDFLG-USR00.

    LS_WBS-WBS-USR03  = <L_WBSELEM>-USR03.
    LS_WBS-WBSX-USR03 = <L_WBSELEM>-UPDFLG-USR03.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_WBSELEM>-FKSTL
      IMPORTING
        OUTPUT = LS_WBS-WBS-FKSTL.
    LS_WBS-WBSX-FKSTL = <L_WBSELEM>-UPDFLG-FKSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <L_WBSELEM>-AKSTL
      IMPORTING
        OUTPUT = LS_WBS-WBS-AKSTL.
    LS_WBS-WBSX-AKSTL = <L_WBSELEM>-UPDFLG-AKSTL.

    ASSIGN_PRART(
      EXPORTING
        IF_PSPID = LS_WBS-PSPID
        IF_PRART = <L_WBSELEM>-PRART
        IF_POSID = LS_WBS-WBS-POSID
      IMPORTING
        EF_PRART = LS_WBS-WBS-PRART ).
    LS_WBS-WBSX-PRART = 'X'.

    ASSIGN_STUFE(
      EXPORTING
        IF_POSID    = LS_WBS-WBS-POSID
        IF_STUFE    = <L_WBSELEM>-STUFE
      IMPORTING
        EF_STUFE    = LS_WBS-WBS-STUFE
      CHANGING
        CS_RESPONSE = CS_RESPONSE ).
    IF CS_RESPONSE-RESP_STATUS IS NOT INITIAL.
      EXIT.
    ENDIF.
    LS_WBS-WBSX-STUFE = 'X'.

*   Only for Project P
    IF LS_WBS-PSPID(1) EQ 'P'.
      ASSIGN_LOB(
        EXPORTING
          IF_PRJTY = LS_WBS-PSPID+2(1)
          IF_ITMTY = <L_WBSELEM>-ITMTY
          IF_ACTTY = <L_WBSELEM>-ACTTY
        IMPORTING
          EF_USR00 = LS_WBS-WBS-USR00 ).
      IF LS_WBS-WBS-USR00 IS NOT INITIAL.
        LS_WBS-WBSX-USR00 = 'X'.
      ENDIF.
    ENDIF.

    ASSIGN_PROFIT_CENTER(
      EXPORTING
        IF_PSPID = LS_WBS-PSPID
        IF_POSID = LS_WBS-WBS-POSID
        IF_AKSTL = LS_WBS-WBS-AKSTL
        IF_VKBUR = IS_REQUEST-VKBUR
        IF_VKGRP = IS_REQUEST-VKGRP
      IMPORTING
        EF_PRCTR = LS_WBS-WBS-PRCTR ).
    IF LS_WBS-WBS-PRCTR IS NOT INITIAL.
      LS_WBS-WBSX-PRCTR = 'X'.
    ENDIF.

*   Auto Generate WBS LV1 - 3 entries for SFDC
    IF IS_FROM_SFDC( LS_WBS-PSPID ).
      GENERATE_WBS_LV1_3(
        EXPORTING
          IS_PROJDEF = ES_PROJDEF-PROJDEF
          IS_WBS     = LS_WBS
        CHANGING
          CT_WBS     = ET_WBS ).
    ENDIF.

    INSERT LS_WBS INTO TABLE ET_WBS.

  ENDLOOP.
  IF CS_RESPONSE-RESP_STATUS IS NOT INITIAL.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD WAIT_PROJECT_UNLOCK.

  DATA:
    LF_WAIT_TIMES  TYPE  I VALUE 30.


  DO LF_WAIT_TIMES TIMES.
    CALL FUNCTION 'ENQUEUE_EC_PROJ'
      EXPORTING
        MODE_PROJ_ENQ  = 'E'
        MANDT          = SY-MANDT
        TYP            = 'P'
        PSPID          = IF_PSPID
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EC_PROJ'
      EXPORTING
        MODE_PROJ_ENQ = 'E'
        MANDT         = SY-MANDT
        TYP           = 'P'
        PSPID         = IF_PSPID.
    EXIT.
  ENDDO.

ENDMETHOD.
ENDCLASS.
