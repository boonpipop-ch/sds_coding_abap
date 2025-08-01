class ZCL_SDSCA_UTIL_SDS definition
  public
  final
  create public .

public section.

  data:
    BEGIN OF GS_DATA_LOA,
           RUNNG TYPE ZSDSMMT001-RUNNG,
           STATU TYPE ZSDSMMT001-STATU,
           POSIT TYPE ZSDSMMT001-POSIT,
           FLAGD TYPE ZSDSMMT001-FLAGD,
           REMAK TYPE ZSDSMMT001-REMAK,
           ACTBY TYPE ZSDSMMT001-ACTBY,
           ERDAT TYPE ZSDSMMT001-ERDAT,
           ERTIM TYPE ZSDSMMT001-ERTIM,
           LVDES TYPE ZSDSCAC006-LVDES,
         END OF GS_DATA_LOA .
  class-data IS_SDS type FLAG .
  class-data INITIAL_FLAG type FLAG .
  class-data USER_PARAM type XUVALUE18 .

  methods GET_CUST_NAME
    importing
      !I_CUST_NO type KUNNR
      value(I_NATION) type AD_NATION optional
      value(I_ADDRNUMBER) type AD_ADDRNUM optional
    exporting
      value(E_NAME1) type AD_NAME1
      value(E_NAME2) type AD_NAME2
      value(E_NAME3) type AD_NAME3
      value(E_NAME4) type AD_NAME4
      value(E_NAME_ALL) type STRING .
  methods GET_SALE_NAME
    importing
      !I_SALE_NO type PERSNO
    exporting
      value(E_SALE_NAEM) type ANY
      value(E_NAME_FIRST) type ANY
      value(E_NAME_LAST) type ANY .
  methods GET_ADDRESS
    importing
      !I_ADDRESS_NO type AD_ADDRNUM
      value(I_NATION) type AD_NATION optional
    exporting
      value(E_ADRC) type ADRC
      value(E_ADDRESS_CONCAT_LINE1) type STRING
      value(E_ADDRESS_CONCAT_LINE2) type STRING
      value(E_ADDRESS_CONCAT_LINE3) type STRING
      value(E_ADDRESS_CONCAT_LINE4) type STRING
      value(E_TAXID) type CHAR255 .
  methods GET_TEXT
    importing
      !I_ID type THEAD-TDID
      !I_NAME type ANY
      !I_OBJECT type THEAD-TDOBJECT
      !I_LANGUAGE type THEAD-TDSPRAS
    returning
      value(R_TEXT) type STRING .
  methods SEND_OTF_FILE_TO_AL11
    importing
      !IT_OTF type TSFOTF
      !I_PATH type STRING
      !I_DATA_AL11 type FLAG
    exporting
      !E_MESSAGE type CHAR50
      !E_STATUS type CHAR1 .
  class-methods GET_CONFIG_001
    importing
      !I_REPID type ANY
      value(I_SINGLE_VALUE_FLAG) type FLAG optional
      value(I_PARAM) type ANY optional
      value(I_PARAM_EXT) type ANY optional
      value(I_SEQUENCE) type ANY optional
    changing
      value(CR_RETURN) type ANY TABLE optional
      value(C_RETURN) type ANY optional
      value(C_RETURN_HIGH) type ANY optional .
  methods SEND_FILE_TO_AL11
    importing
      !IT_DATA type ANY TABLE
      value(I_BINARY) type FLAG optional
      !I_PATH type ANY
    exporting
      !E_MESSAGE type ANY
      !E_STATUS type CHAR1 .
  methods CALL_AMOUNT_TAX
    importing
      !I_INPUT type ANY
    returning
      value(R_RETURN) type MWSTS .
  methods GET_PAYMENT_DATE
    importing
      !I_DATE type SY-DATUM
      !I_TERM type T052-ZTERM
    returning
      value(R_DATE) type SY-DATUM .
  methods GET_VEND_NAME
    importing
      !I_VEND_NO type LIFNR
      value(I_NATION) type AD_NATION optional
    exporting
      value(E_NAME1) type AD_NAME1
      value(E_NAME2) type AD_NAME2
      value(E_NAME3) type AD_NAME3
      value(E_NAME4) type AD_NAME4
      value(E_NAME_ALL) type STRING .
  methods GET_SDS_ADDRESS
    importing
      value(I_BRANCH) type ANY optional
    exporting
      value(E_ADDRESS_TH) type ANY
      !E_ADDRESS_EN type ANY .
  methods GET_WBS_DESC
    importing
      !I_WBS type PRPS-PSPNR
    returning
      value(R_WBS_DESC) type PRPS-POST1 .
  methods GET_LOA_PR
    importing
      !I_DOC_NO type ANY
      value(I_RUNNING) type FLAG optional
    returning
      value(R) type ZSDSCAS009 .
  methods GET_LOA_PO
    importing
      !I_DOC_NO type ANY
      value(I_RUNNING) type FLAG optional
    returning
      value(R) type ZSDSCAS009 .
  methods SEND_MAIL
    importing
      !I_SUBJECT type SO_OBJ_DES
      !IT_HTML_CON type SOLI_TAB
      !IT_ATTACH_FILE type ZSDSCAS019_TT
      !I_SENDER_EMAIL type ANY
      !I_SENDER_NAME type ANY
      !IT_RECEIVER type ANY TABLE
      !IT_CC type ANY TABLE .
  methods GET_WBS_LV3_FROM_HIGHER_LV
    importing
      !I_DATA type PS_POSID
    returning
      value(R_RETURN) type PS_POSID .
  methods GET_BRANCH
    importing
      !I_KUNNR type KUNNR
      !I_LANGU type SY-LANGU
    returning
      value(R_DATA) type STRING .
  methods GET_WBS_LV_FOR_PROJECT
    importing
      !I_DATA type PS_POSID
    returning
      value(R_RETURN) type PS_POSID .
  methods GET_EMPCODE_FROM_COSTCENTER
    importing
      !I_COSCENTER type KOSTL
    returning
      value(R_EMP) type PERNR_D .
  class-methods CHECK_SDS
    importing
      !LV_MANDT type SY-MANDT
    exporting
      !LV_SDS type CHAR01 .
  class-methods GET_USER_PARAMETER
    importing
      !I_PARAM type MEMORYID
    returning
      value(R_VALUE) type XUVALUE .
  class-methods SEND_FILE_SERVER
    importing
      !IT_DATA type STRING_TABLE
      !I_PATH type ANY
      !I_FILE_NAME type ANY
    returning
      value(R_RETURN) type CHAR1 .
protected section.
private section.

  constants:
    BEGIN OF GC_CON,
      I  TYPE C LENGTH 1 VALUE 'I',
      E  TYPE C LENGTH 1 VALUE 'E',
      S  TYPE C LENGTH 1 VALUE 'S',
      EQ TYPE C LENGTH 2 VALUE 'EQ',
    END OF GC_CON .
ENDCLASS.



CLASS ZCL_SDSCA_UTIL_SDS IMPLEMENTATION.


  METHOD GET_ADDRESS.

*    DATA : BEGIN OF LS_DATA,
*             HOUSE_NUM1 TYPE ADRC-HOUSE_NUM1,
*             STREET     TYPE ADRC-STREET,
*             LOCATION   TYPE ADRC-LOCATION,
*             CITY2      TYPE ADRC-CITY2,
*             CITY1      TYPE ADRC-CITY1,
*             POST_CODE1 TYPE ADRC-POST_CODE1,
*           END OF LS_DATA.

    DATA : LV_STRING TYPE STRING.

    DATA : LV_TIMES TYPE I.

    SELECT SINGLE *
      FROM ADRC
      INTO E_ADRC
      WHERE ADDRNUMBER EQ I_ADDRESS_NO
        AND NATION     EQ I_NATION
        AND DATE_FROM  LE SY-DATUM
        AND DATE_TO    GE SY-DATUM.

    DO 7 TIMES.
      CLEAR : LV_STRING.

      IF SY-INDEX EQ 1.
        LV_STRING = E_ADRC-HOUSE_NUM1.
      ELSEIF SY-INDEX EQ 2.
        LV_STRING = E_ADRC-STREET.
      ELSEIF SY-INDEX EQ 3.
        LV_STRING = E_ADRC-STR_SUPPL1.
      ELSEIF SY-INDEX EQ 4.
        LV_STRING = E_ADRC-STR_SUPPL2.
      ELSEIF SY-INDEX EQ 5.
        LV_STRING = E_ADRC-STR_SUPPL3.
      ELSEIF SY-INDEX EQ 6.
        LV_STRING = E_ADRC-LOCATION.
      ELSEIF SY-INDEX EQ 7.
        LV_STRING = E_ADRC-CITY2.
*      ELSEIF SY-INDEX EQ 8.
*        LV_STRING = E_ADRC-CITY1.
*      ELSEIF SY-INDEX EQ 9.
*        LV_STRING = E_ADRC-POST_CODE1.
      ENDIF.

      IF LV_TIMES LE 3.
        IF LV_STRING IS NOT INITIAL.
          ADD 1 TO LV_TIMES.
          IF E_ADDRESS_CONCAT_LINE1 IS INITIAL.
            E_ADDRESS_CONCAT_LINE1 = LV_STRING.
          ELSE.
            CONCATENATE E_ADDRESS_CONCAT_LINE1 LV_STRING INTO E_ADDRESS_CONCAT_LINE1 SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ELSEIF LV_TIMES LE 6.
        IF LV_STRING IS NOT INITIAL.
          ADD 1 TO LV_TIMES.
          IF E_ADDRESS_CONCAT_LINE2 IS INITIAL.
            E_ADDRESS_CONCAT_LINE2 = LV_STRING.
          ELSE.
            CONCATENATE E_ADDRESS_CONCAT_LINE2 LV_STRING INTO E_ADDRESS_CONCAT_LINE2 SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ELSE.
        IF LV_STRING IS NOT INITIAL.
          IF E_ADDRESS_CONCAT_LINE3 IS INITIAL.
            E_ADDRESS_CONCAT_LINE3 = LV_STRING.
          ELSE.
            CONCATENATE E_ADDRESS_CONCAT_LINE3 LV_STRING INTO E_ADDRESS_CONCAT_LINE3 SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    IF     E_ADDRESS_CONCAT_LINE2 IS INITIAL.
      CONCATENATE E_ADRC-CITY1 E_ADRC-POST_CODE1 INTO E_ADDRESS_CONCAT_LINE2 SEPARATED BY SPACE.
    ELSEIF E_ADDRESS_CONCAT_LINE3 IS INITIAL.
      CONCATENATE E_ADRC-CITY1 E_ADRC-POST_CODE1 INTO E_ADDRESS_CONCAT_LINE3 SEPARATED BY SPACE.
    ELSEIF E_ADDRESS_CONCAT_LINE4 IS INITIAL.
      CONCATENATE E_ADRC-CITY1 E_ADRC-POST_CODE1 INTO E_ADDRESS_CONCAT_LINE4 SEPARATED BY SPACE.
    ELSEIF E_ADDRESS_CONCAT_LINE4 IS NOT INITIAL.
      CONCATENATE E_ADDRESS_CONCAT_LINE4 E_ADRC-CITY1 E_ADRC-POST_CODE1 INTO E_ADDRESS_CONCAT_LINE4 SEPARATED BY SPACE.
    ENDIF.

*    DO 3 TIMES.
*      CLEAR : LV_STRING.
*      IF SY-INDEX EQ 1.
*        LV_STRING = E_ADRC-CITY2.
*      ELSEIF SY-INDEX EQ 2.
*        LV_STRING = E_ADRC-CITY1.
*      ELSEIF SY-INDEX EQ 3.
*        LV_STRING = E_ADRC-POST_CODE1.
*      ENDIF.
*
*    ENDDO.

    E_TAXID = E_ADRC-TAXJURCODE.

  ENDMETHOD.


  METHOD GET_CUST_NAME.

    IF I_CUST_NO CP 'OT*' AND I_ADDRNUMBER IS NOT INITIAL.
      "ONE TIME CASE
      SELECT SINGLE ADRC~NAME1,
                    ADRC~NAME2,
                    ADRC~NAME3,
                    ADRC~NAME4,
                  ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME_ALL
      FROM ADRC
      WHERE ADDRNUMBER = @I_ADDRNUMBER
        AND NATION EQ @I_NATION
      INTO (@E_NAME1,@E_NAME2,@E_NAME3,@E_NAME4,@E_NAME_ALL).
      IF SY-SUBRC NE 0.
        SELECT SINGLE ADRC~NAME1,
                      ADRC~NAME2,
                      ADRC~NAME3,
                      ADRC~NAME4,
                    ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME_ALL
        FROM ADRC
        WHERE ADDRNUMBER = @I_ADDRNUMBER
        INTO (@E_NAME1,@E_NAME2,@E_NAME3,@E_NAME4,@E_NAME_ALL).
      ENDIF.
    ELSE.
      "CUSTOMER MASTER
      SELECT SINGLE ADRC~NAME1,
                    ADRC~NAME2,
                    ADRC~NAME3,
                    ADRC~NAME4,
                  ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME_ALL
        FROM KNA1
        INNER JOIN ADRC ON KNA1~ADRNR EQ ADRC~ADDRNUMBER
        WHERE KUNNR  EQ @I_CUST_NO
          AND NATION EQ @I_NATION
        INTO (@E_NAME1,@E_NAME2,@E_NAME3,@E_NAME4,@E_NAME_ALL).
      IF SY-SUBRC NE 0.
        SELECT SINGLE ADRC~NAME1,
                    ADRC~NAME2,
                    ADRC~NAME3,
                    ADRC~NAME4,
                  ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME_ALL
        FROM KNA1
        INNER JOIN ADRC ON KNA1~ADRNR EQ ADRC~ADDRNUMBER
        WHERE KUNNR  EQ @I_CUST_NO
        INTO (@E_NAME1,@E_NAME2,@E_NAME3,@E_NAME4,@E_NAME_ALL).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_SALE_NAME.
    DATA : LV_VORNA TYPE PA0002-VORNA,
           LV_NACHN TYPE PA0002-NACHN.

    SELECT SINGLE VORNA
                  NACHN
      FROM PA0002
      INTO (LV_VORNA,LV_NACHN)
      WHERE PERNR EQ I_SALE_NO.
    IF SY-SUBRC EQ 0.
      CONCATENATE LV_VORNA LV_NACHN INTO E_SALE_NAEM SEPARATED BY SPACE.
      E_NAME_FIRST = LV_VORNA.
      E_NAME_LAST  = LV_NACHN.
    ENDIF.
  ENDMETHOD.


  METHOD GET_TEXT.

    DATA : LT_LINES TYPE TABLE OF TLINE,
           LS_LINES TYPE TLINE.

    DATA : LV_NAME TYPE THEAD-TDNAME.

    LV_NAME = I_NAME.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        CLIENT                  = SY-MANDT
        ID                      = I_ID
        LANGUAGE                = I_LANGUAGE
        NAME                    = LV_NAME
        OBJECT                  = I_OBJECT
      TABLES
        LINES                   = LT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF LT_LINES[] IS NOT INITIAL.
      LOOP AT LT_LINES INTO LS_LINES.
        IF R_TEXT IS NOT INITIAL.
          CONCATENATE R_TEXT LS_LINES-TDLINE INTO R_TEXT SEPARATED BY SPACE.
        ELSE.
          R_TEXT = LS_LINES-TDLINE.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  method CALL_AMOUNT_TAX.





  endmethod.


  METHOD GET_CONFIG_001.

    DATA : LR_REPID     TYPE RANGE OF ZSDSCAC001-REPID,
           LR_PARAM     TYPE RANGE OF ZSDSCAC001-PARAM,
           LR_PARAM_EXT TYPE RANGE OF ZSDSCAC001-PARAM_EXT,
           LR_SEQUENCE  TYPE RANGE OF ZSDSCAC001-SEQUENCE.

    DATA LR_DATA TYPE REF TO DATA.
    FIELD-SYMBOLS: <LFS_STRUCT> TYPE ANY,
                   <LFS_DATA>   TYPE ANY.

    IF I_REPID IS NOT INITIAL.
      LR_REPID  =  VALUE #( ( SIGN = GC_CON-I OPTION = GC_CON-EQ LOW = I_REPID ) ).
    ENDIF.

    IF I_PARAM IS NOT INITIAL.
      LR_PARAM =  VALUE #( ( SIGN = GC_CON-I OPTION = GC_CON-EQ LOW = I_PARAM ) ).
    ENDIF.

    IF I_PARAM_EXT IS NOT INITIAL.
      LR_PARAM_EXT =  VALUE #( ( SIGN = GC_CON-I OPTION = GC_CON-EQ LOW = I_PARAM_EXT ) ).
    ENDIF.

    IF I_SEQUENCE IS NOT INITIAL.
      LR_SEQUENCE =  VALUE #( ( SIGN = GC_CON-I OPTION = GC_CON-EQ LOW = I_SEQUENCE ) ).
    ENDIF.

    IF I_SINGLE_VALUE_FLAG EQ ABAP_TRUE.
      SELECT SINGLE
             VALUE_LOW,
             VALUE_HIGH
        FROM ZSDSCAC001
        INTO (@C_RETURN,@C_RETURN_HIGH)
        WHERE REPID     IN @lr_REPID[]
          AND PARAM     IN @lr_PARAM[]
          AND PARAM_EXT IN @lr_PARAM_EXT[]
          AND SEQUENCE  IN @lr_SEQUENCE[].
    ELSE.
      SELECT PARAM_SIGN,
             PARAM_OPTION,
             VALUE_LOW,
             VALUE_HIGH
        FROM ZSDSCAC001
        INTO TABLE @DATA(LT_CONFIG)
        WHERE REPID     IN @lr_REPID[]
          AND PARAM     IN @lr_PARAM[]
          AND PARAM_EXT IN @lr_PARAM_EXT[]
          AND SEQUENCE  IN @lr_SEQUENCE[].

      DATA LS_CONFIG LIKE LINE OF LT_CONFIG.

      CREATE DATA LR_DATA LIKE LINE OF CR_RETURN.
      ASSIGN LR_DATA->* TO <LFS_STRUCT>.

      LOOP AT LT_CONFIG INTO LS_CONFIG.
        DO.
          ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LFS_STRUCT> TO <LFS_DATA>.
          IF SY-SUBRC NE 0.
            EXIT.
          ENDIF.
          IF     SY-INDEX EQ 1.
            <LFS_DATA> = LS_CONFIG-PARAM_SIGN.
          ELSEIF SY-INDEX EQ 2.
            <LFS_DATA> = LS_CONFIG-PARAM_OPTION.
          ELSEIF SY-INDEX EQ 3.
            <LFS_DATA> = LS_CONFIG-VALUE_LOW.
          ELSEIF SY-INDEX EQ 4.
            <LFS_DATA> = LS_CONFIG-VALUE_HIGH.
          ENDIF.
        ENDDO.
        INSERT <LFS_STRUCT> INTO TABLE CR_RETURN.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD GET_LOA_PO.
    DATA : LS_DATA LIKE GS_DATA_LOA,
           LT_DATA LIKE TABLE OF LS_DATA.

    CONSTANTS : BEGIN OF LC_CON,
                  WAI TYPE C LENGTH 3 VALUE 'WAI',
                END OF LC_CON.

    SELECT RUNNG
           STATU
           POSIT
           FLAGD
           REMAK
           ERNAM
           ERDAT
           ERZET
           SHDES
      FROM ZSDSMMT002
      INNER JOIN ZSDSCAC006 ON ZSDSMMT002~POSIT EQ ZSDSCAC006~LVLOA
      INTO TABLE LT_DATA
      WHERE EBELN EQ I_DOC_NO
        AND FLAGD NE ABAP_TRUE
        AND STATU NE LC_CON-WAI.
    IF SY-SUBRC = 0.
      SORT LT_DATA BY RUNNG.
    ENDIF.

    LOOP AT LT_DATA INTO LS_DATA.
      IF I_RUNNING EQ ABAP_TRUE.
        R = LCL_DATA=>SET_LOA_RUN( I_DATA = LS_DATA
                                   I_LOA  = R ).
      ELSE.
        R = LCL_DATA=>SET_LOA( I_DATA = LS_DATA
                               I_LOA  = R ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_LOA_PR.
    DATA : LS_DATA LIKE GS_DATA_LOA,
           LT_DATA LIKE TABLE OF LS_DATA.

    CONSTANTS : BEGIN OF LC_CON,
                  WAI TYPE C LENGTH 3 VALUE 'WAI',
                END OF LC_CON.

    SELECT ZSDSMMT001~RUNNG
           ZSDSMMT001~STATU
           ZSDSMMT001~POSIT
           ZSDSMMT001~FLAGD
           ZSDSMMT001~REMAK
           ZSDSMMT001~ACTBY
           ZSDSMMT001~ERDAT
           ZSDSMMT001~ERTIM
           ZSDSCAC006~SHDES
      FROM ZSDSMMT006
      INNER JOIN ZSDSMMT001 ON ZSDSMMT006~WEBNO EQ ZSDSMMT001~BANFN
      INNER JOIN ZSDSCAC006 ON ZSDSMMT001~POSIT EQ ZSDSCAC006~LVLOA
      INTO TABLE LT_DATA
      WHERE ZSDSMMT006~BANFN EQ I_DOC_NO
        AND ZSDSMMT001~FLAGD NE ABAP_TRUE
        AND ZSDSMMT001~STATU NE LC_CON-WAI.
    IF SY-SUBRC = 0.
      SORT LT_DATA BY RUNNG.
    ENDIF.

    LOOP AT LT_DATA INTO LS_DATA.
      IF I_RUNNING EQ ABAP_TRUE.
        R = LCL_DATA=>SET_LOA_RUN( I_DATA = LS_DATA
                                   I_LOA  = R ).
      ELSE.
        R = LCL_DATA=>SET_LOA( I_DATA = LS_DATA
                               I_LOA  = R ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_PAYMENT_DATE.
    SELECT SINGLE ZTAG1
      FROM T052
      INTO @DATA(LV_ZTAG1)
      WHERE ZTERM EQ @I_TERM.
    IF SY-SUBRC EQ 0.
      R_DATE = I_DATE + LV_ZTAG1.
    ENDIF.
  ENDMETHOD.


  METHOD GET_SDS_ADDRESS.

    CONSTANTS : LC_COM_CODE TYPE C LENGTH 4  VALUE '1000',
                LC_TEL      TYPE C LENGTH 4  VALUE 'TEL.',
                LC_I        TYPE C LENGTH 1  VALUE 'I',
                LC_COL      TYPE C LENGTH 1  VALUE ':',
                LC_HEAD     TYPE C LENGTH 11 VALUE 'Head Office',
                LC_COMMA    TYPE C LENGTH 1  VALUE ',',
                LC_THAI     TYPE C LENGTH 8  VALUE 'THAILAND',
                LC_COOL     TYPE C LENGTH 50 VALUE 'Cool Line 1271'.

    CONSTANTS LC_HEADER_OFFICE TYPE C LENGTH 4 VALUE '0000'.

    DATA : I_ADDRESS_NO TYPE AD_ADDRNUM,
           I_NATION     TYPE AD_NATION,
           E_ADRC       TYPE ADRC.

    DATA : BEGIN OF LS_DATA,
             ADRNR TYPE J_1BBRANCH-ADRNR,
             NAME  TYPE J_1BBRANCHT-NAME,
           END OF LS_DATA.

    DATA : LV_POST_CODE TYPE CHAR50,
           LV_STREET    TYPE CHAR255,
           LV_CITY2     TYPE CHAR255,
           LV_CITY1     TYPE CHAR255,
           LV_LOCATION  TYPE CHAR255.

    DATA : LV_BRANCH_TH TYPE CHAR50,
           LV_BRANCH_EN TYPE CHAR50.

    IF I_BRANCH IS INITIAL.
      I_BRANCH = LC_HEADER_OFFICE.
    ENDIF.

    SELECT SINGLE J_1BBRANCH~ADRNR,
                  J_1BBRANCHT~NAME
      FROM J_1BBRANCH
      INNER JOIN J_1BBRANCHT ON J_1BBRANCH~BUKRS       EQ J_1BBRANCHT~BUKRS   AND
                                J_1BBRANCH~BRANCH      EQ J_1BBRANCHT~BRANCH  AND
                                J_1BBRANCHT~BUPLA_TYPE EQ @SPACE              AND
                                J_1BBRANCHT~LANGUAGE   EQ @SY-LANGU
      WHERE J_1BBRANCH~BUKRS  EQ @LC_COM_CODE
        AND J_1BBRANCH~BRANCH EQ @I_BRANCH
      INTO @LS_DATA.

    I_NATION     = SPACE.
    I_ADDRESS_NO = LS_DATA-ADRNR.

    GET_ADDRESS( EXPORTING I_ADDRESS_NO = I_ADDRESS_NO
                           I_NATION     = I_NATION
                 IMPORTING E_ADRC       = E_ADRC ).

    CONCATENATE E_ADRC-POST_CODE1 LC_COMMA INTO LV_POST_CODE.

    IF I_BRANCH EQ LC_HEADER_OFFICE.
      LV_BRANCH_TH = LS_DATA-NAME.
      LV_BRANCH_EN = LC_HEAD.
    ELSE.
      SPLIT LS_DATA-NAME AT SPACE INTO LV_BRANCH_TH
                                       LV_BRANCH_EN.

      REPLACE ALL OCCURRENCES OF '(' IN LV_BRANCH_EN WITH SPACE.
      REPLACE ALL OCCURRENCES OF ')' IN LV_BRANCH_EN WITH SPACE.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_BRANCH_EN WITH SPACE.
    ENDIF.

    CONCATENATE LV_BRANCH_TH
                LC_COL
                E_ADRC-STREET
                E_ADRC-STR_SUPPL3
                E_ADRC-LOCATION
                E_ADRC-CITY2
                E_ADRC-CITY1
                LV_POST_CODE
                LC_TEL
                E_ADRC-TEL_NUMBER
                LC_COOL
           INTO E_ADDRESS_TH SEPARATED BY SPACE.

    CLEAR : E_ADRC.

    I_NATION     = LC_I.
    I_ADDRESS_NO = LS_DATA-ADRNR.

    GET_ADDRESS( EXPORTING I_ADDRESS_NO = I_ADDRESS_NO
                           I_NATION     = I_NATION
                 IMPORTING E_ADRC       = E_ADRC ).

    LV_STREET    = E_ADRC-STREET."| { E_ADRC-STREET } | & |{ LC_COMMA }|.
    LV_LOCATION  = E_ADRC-LOCATION."| { E_ADRC-LOCATION } | & |{ LC_COMMA }|.
    LV_CITY2     = |{ E_ADRC-CITY2 }|." & |{ LC_COMMA }|.
    LV_CITY1     = |{ E_ADRC-CITY1 }| & |{ LC_COMMA }|.
    LV_POST_CODE = |{ E_ADRC-POST_CODE1 }| & |{ LC_COMMA }|.



    CONCATENATE LV_BRANCH_EN
                LC_COL
                LV_STREET
                LV_LOCATION
                LV_CITY2
                LV_CITY1
                LV_POST_CODE
                LC_THAI
           INTO E_ADDRESS_EN SEPARATED BY SPACE.

    TRANSLATE E_ADDRESS_EN TO UPPER CASE.

*    IF I_BRANCH IS NOT INITIAL.
*    E_ADDRESS_TH = 'สำนักงานใหญ่ : 22 ซอยอ่อนนุช 55/1 แขวงประเวศ เขตประเวศ กรุงเทพฯ 10250, TEL.02-838-3200 FAX. 02-721-6680'.
*    E_ADDRESS_EN = 'Head Office : 22 SOI ONNUCH 55/1, PRAVET SUBDISTRICT, PRAVET DISTRICT, BANGKOK 10250, THAILAND'.
*
*
*  ELSE.
*
*
*  ENDIF.

  ENDMETHOD.


  METHOD GET_VEND_NAME.
    SELECT SINGLE ADRC~NAME1,
                  ADRC~NAME2,
                  ADRC~NAME3,
                  ADRC~NAME4,
                ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME_ALL
      FROM LFA1
      INNER JOIN ADRC ON LFA1~ADRNR EQ ADRC~ADDRNUMBER
      WHERE LIFNR  EQ @I_vend_NO
        AND NATION EQ @I_NATION
      INTO (@E_NAME1,@E_NAME2,@E_NAME3,@E_NAME4,@E_NAME_ALL).
    IF SY-SUBRC NE 0.
      SELECT SINGLE ADRC~NAME1,
                    ADRC~NAME2,
                    ADRC~NAME3,
                    ADRC~NAME4,
                  ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME_ALL
      FROM LFA1
      INNER JOIN ADRC ON LFA1~ADRNR EQ ADRC~ADDRNUMBER
      WHERE KUNNR  EQ @I_vend_NO
      INTO (@E_NAME1,@E_NAME2,@E_NAME3,@E_NAME4,@E_NAME_ALL).
    ENDIF.
  ENDMETHOD.


  METHOD GET_WBS_DESC.

    SELECT SINGLE POST1
      FROM PRPS
      INTO R_WBS_DESC
      WHERE PSPNR EQ I_WBS.

  ENDMETHOD.


  METHOD SEND_FILE_TO_AL11.

    FIELD-SYMBOLS <LFS_DATA> TYPE ANY.

    IF I_BINARY EQ ABAP_TRUE.
      OPEN DATASET I_PATH FOR OUTPUT IN BINARY MODE.
    ELSE.
      OPEN DATASET I_PATH FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    ENDIF.
    IF SY-SUBRC EQ 0.
      LOOP AT IT_DATA ASSIGNING <LFS_DATA>.
        TRANSFER <LFS_DATA> TO I_PATH.
      ENDLOOP.
      CLOSE DATASET I_PATH.
      E_STATUS  = GC_CON-S.
      E_MESSAGE = TEXT-S01.
    ELSE.
      E_STATUS  = GC_CON-E.
      E_MESSAGE = TEXT-E01.
    ENDIF.

  ENDMETHOD.


  method GET_WBS_LV3_FROM_HIGHER_LV.
    DATA : LV_WBS TYPE PS_POSNR.

    DATA : BEGIN OF LS_TAB_CHECK,
             DATA TYPE CHAR255,
           END OF LS_TAB_CHECK.
    DATA : LT_TAB_CHECK LIKE TABLE OF LS_TAB_CHECK.

    DATA : LT_TAB_WBS LIKE TABLE OF LS_TAB_CHECK,
           LS_TAB_WBS LIKE LINE OF LT_TAB_WBS.

    DATA : LV_CHECK TYPE C.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
      EXPORTING
        INPUT     = I_DATA
      IMPORTING
        OUTPUT    = LV_WBS
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT POSID
      FROM PRPS AS A
      WHERE PSPHI EQ ( SELECT PSPHI
                         FROM PRPS
                        WHERE PSPNR EQ @LV_WBS )
        AND STUFE EQ 3
      INTO TABLE @DATA(lt_LV3).

    DATA : LS_LV3 LIKE LINE OF LT_LV3.

    IF LT_LV3 IS NOT INITIAL.
      SPLIT I_DATA AT '-' INTO TABLE LT_TAB_WBS.

      LOOP AT LT_LV3 INTO LS_LV3.
        SPLIT LS_LV3 AT '-' INTO TABLE LT_TAB_CHECK.
        CLEAR : LV_CHECK.
        LOOP AT LT_TAB_CHECK INTO LS_TAB_CHECK.
          READ TABLE LT_TAB_WBS INTO LS_TAB_WBS INDEX SY-TABIX.
          IF LS_TAB_CHECK-DATA NE LS_TAB_WBS-DATA.
            LV_CHECK = ABAP_TRUE.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF LV_CHECK EQ SPACE.
          R_RETURN = LS_LV3.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  endmethod.


  METHOD SEND_OTF_FILE_TO_AL11.

    IF I_DATA_AL11 EQ ABAP_TRUE.
      LCL_DATA=>SEND_TO_DATA_SPIDER_PATH( EXPORTING IT_OTF    = IT_OTF
                                                    I_PATH    = I_PATH
                                          IMPORTING E_MESSAGE = E_MESSAGE
                                                    E_STATUS  = E_STATUS ).

    ELSE.
      LCL_DATA=>SEND_TO_LOCAL_PATH( EXPORTING IT_OTF    = IT_OTF
                                              I_PATH    = I_PATH
                                    IMPORTING E_MESSAGE = E_MESSAGE
                                              E_STATUS  = E_STATUS ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_BRANCH.

    DATA : LV_TEXT_TH TYPE STRING,
           LV_TEXT_EN TYPE STRING.

    SELECT SINGLE
      FITHA_PBUPL_D~J_1TPBUPL,
      FITHA_PBUPL_D_T~DESCRIPTION
      FROM FITHA_PBUPL_D
      INNER JOIN FITHA_PBUPL_D_T ON FITHA_PBUPL_D~KUNNR EQ FITHA_PBUPL_D_T~KUNNR AND
                                    FITHA_PBUPL_D_T~SPRAS EQ 'E'
      WHERE FITHA_PBUPL_D~KUNNR          EQ @I_KUNNR
        AND FITHA_PBUPL_D~DEFAULT_BRANCH EQ @ABAP_TRUE
        AND FITHA_PBUPL_D~J_1TPBUPL <> 'NVAT'
      INTO @DATA(LS_BRANCH).

    IF LS_BRANCH IS NOT INITIAL.
      SPLIT LS_BRANCH-DESCRIPTION AT '/' INTO LV_TEXT_EN
                                              LV_TEXT_TH.

      IF I_LANGU EQ GC_CON-E.
        R_DATA = LV_TEXT_EN.
      ELSE.
        R_DATA = LV_TEXT_TH.
      ENDIF.

      IF LS_BRANCH-J_1TPBUPL IS NOT INITIAL
      AND LS_BRANCH-J_1TPBUPL <> '00000'.
        R_DATA = |{ R_DATA } สาขาที่  : { LS_BRANCH-J_1TPBUPL }|.
      ENDIF.

      CONDENSE R_DATA.
    ENDIF.
  ENDMETHOD.


  METHOD GET_EMPCODE_FROM_COSTCENTER.

    SELECT SINGLE PERNR
      FROM ZSDSFIC030
      INTO R_EMP
      WHERE KOSTL EQ I_COSCENTER.

  ENDMETHOD.


  METHOD GET_WBS_LV_FOR_PROJECT.
    DATA : LV_WBS TYPE PS_POSNR.

    DATA : BEGIN OF LS_TAB_CHECK,
             DATA TYPE CHAR255,
           END OF LS_TAB_CHECK.
    DATA : LT_TAB_CHECK LIKE TABLE OF LS_TAB_CHECK.

    DATA : LT_TAB_WBS LIKE TABLE OF LS_TAB_CHECK,
           LS_TAB_WBS LIKE LINE OF LT_TAB_WBS.

    DATA : LV_CHECK TYPE C.

    DATA : LV_TABIX TYPE SY-TABIX.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
      EXPORTING
        INPUT     = I_DATA
      IMPORTING
        OUTPUT    = LV_WBS
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT POSID
      FROM PRPS AS A
      WHERE PSPHI EQ ( SELECT PSPHI
                         FROM PRPS
                        WHERE PSPNR EQ @LV_WBS )
        AND STUFE EQ 4
      INTO TABLE @DATA(LT_LV4).

    DATA : LS_LV4 LIKE LINE OF LT_LV4.

    IF LT_LV4 IS NOT INITIAL.
      SPLIT I_DATA AT '-' INTO TABLE LT_TAB_WBS.
      DO.
        CLEAR : LV_CHECK.
        READ TABLE LT_LV4 INTO LS_LV4 INDEX 1.
        IF SY-SUBRC NE 0.
          EXIT.
        ELSE.
          DELETE LT_LV4 INDEX 1.
        ENDIF.

        SPLIT LS_LV4 AT '-' INTO TABLE LT_TAB_CHECK.

        LOOP AT LT_TAB_WBS INTO LS_TAB_WBS.
          LV_TABIX = SY-TABIX.
          READ TABLE LT_TAB_CHECK INTO lS_TAB_CHECK INDEX LV_TABIX.
          IF LS_TAB_WBS NE lS_TAB_CHECK.
            LV_CHECK = ABAP_TRUE.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF SY-SUBRC NE 0.
          CONTINUE.
        ENDIF.
        IF LV_CHECK IS INITIAL.
          EXIT.
        ENDIF.
      ENDDO.
      IF LV_CHECK EQ SPACE.
        R_RETURN = LS_LV4.
        EXIT.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method SEND_MAIL.
    DATA : LV_MONTH_TEXT TYPE C LENGTH 10.

    DATA : LS_DOCUMENT_DATA       TYPE  SODOCCHGI1,
           LV_PUT_IN_OUTBOX       TYPE  SONV-FLAG,
           LV_SENDER_ADDRESS      TYPE  SOEXTRECI1-RECEIVER,
           LV_SENDER_ADDRESS_TYPE TYPE  SOEXTRECI1-ADR_TYP,
           LV_COMMIT_WORK         TYPE  SONV-FLAG.

    DATA : LV_SENT_TO_ALL   TYPE  SONV-FLAG,
           LV_NEW_OBJECT_ID TYPE  SOFOLENTI1-OBJECT_ID,
           LV_SENDER_ID     TYPE  SOUDK.

    DATA : I_TEXT    TYPE TABLE OF SOLI,
           LV_NUM    TYPE C LENGTH 15,
           TAB_LINE1 TYPE SO_BD_NUM,
           TAB_LINE2 TYPE	SO_BD_NUM,
           LV_FIELEC TYPE SOOD-OBJDES.

    DATA : LO_DOC_BCS TYPE REF TO CL_DOCUMENT_BCS,
           LO_BCS     TYPE REF TO CL_BCS.

    DATA : LO_SENDER TYPE REF TO CL_CAM_ADDRESS_BCS.

    DATA : LO_RECIPIENT TYPE REF TO CL_CAM_ADDRESS_BCS.

    DATA : LT_SOLIX TYPE SOLIX_TAB,
           LS_SOLIX LIKE LINE OF LT_SOLIX.

    DATA : LT_SOLIX_ATTH TYPE SOLIX_TAB,
           LS_SOLIX_ATTH LIKE LINE OF LT_SOLIX.

    DATA : LT_SOLI TYPE SOLI_TAB,
           LS_SOLI LIKE LINE OF LT_SOLI.

    DATA : LT_SOLI_ATTH TYPE SOLI_TAB,
           LS_SOLI_ATTH LIKE LINE OF LT_SOLI.

    DATA : LV_SUB TYPE STRING.

    DATA : LV_LINE TYPE SO_TEXT255.

*    DATA : LV_CC TYPE ADR6-SMTP_ADDR.

    DATA : LV_CC    TYPE ADR6-SMTP_ADDR, " VALUE 'jakarin_s@tripetch-it.co.th',
           LV_EMAIL TYPE ADR6-SMTP_ADDR. " VALUE 'jakarin_s@tripetch-it.co.th'.

    DATA : LV_SENDER TYPE ADR6-SMTP_ADDR, " VALUE 'jakarin_s@tripetch-it.co.th',
           LV_SNAME  TYPE ADR6-SMTP_ADDR. " VALUE 'jakarin_s@tripetch-it.co.th'.

    DATA : LS_ATTACH_FILE TYPE ZSDSCAS019.

    FIELD-SYMBOLS <LFS_ANY> TYPE ANY.

    CONSTANTS : C_CRET(2) TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.

    CONSTANTS : BEGIN OF LC_CON,
                  HTM TYPE C LENGTH 3 VALUE 'HTM',
                  PDF TYPE C LENGTH 3 VALUE 'PDF',
                END OF LC_CON.

    CLEAR : LO_BCS,LO_DOC_BCS .

    LT_SOLI = IT_HTML_CON.

    LV_SUB = I_SUBJECT.
    TRY.
        LO_DOC_BCS = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                            I_TYPE    = LC_CON-HTM
                            I_TEXT    = LT_SOLI
                            I_SUBJECT = I_SUBJECT ).
      CATCH CX_DOCUMENT_BCS.
    ENDTRY.
    TRY.
        LO_BCS = CL_BCS=>CREATE_PERSISTENT( ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_DOCUMENT( I_DOCUMENT = LO_DOC_BCS ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_MESSAGE_SUBJECT( IP_SUBJECT = LV_SUB ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LV_SENDER = I_SENDER_EMAIL.
        LV_SNAME  = I_SENDER_NAME.
        LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = LV_SENDER
                                                                 I_ADDRESS_NAME   = LV_SNAME ).
      CATCH CX_ADDRESS_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_SENDER( EXPORTING I_SENDER = LO_SENDER ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.

    LOOP AT IT_RECEIVER ASSIGNING <LFS_ANY>.
      LV_EMAIL = <LFS_ANY>.
      TRY.
          LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING =  LV_EMAIL ).
        CATCH CX_ADDRESS_BCS.
      ENDTRY.
      TRY.
          LO_BCS->ADD_RECIPIENT( I_RECIPIENT = LO_RECIPIENT ).
        CATCH CX_SEND_REQ_BCS.
      ENDTRY.
    ENDLOOP.
    IF <LFS_ANY> IS ASSIGNED.
      UNASSIGN <LFS_ANY>.
    ENDIF.

*--------------------------------------------------------------------*
* Set UTF-8 For Excel
*--------------------------------------------------------------------*
*      DATA: BINARY_CONTENT TYPE SOLIX_TAB,
*            TEXT_STRING    TYPE STRING.
*
*      DATA: XS_CONTENT   TYPE XSTRING,
*            APP_TYPE(50) .
*
*      DATA LENGTH TYPE I.
*
*
*
*      PERFORM F_GEN_DATA_EXCEL TABLES LT_SOLIX_ATTH
*                                USING LV_LINE
*                                      C_CRET.
*
*      LV_FIELEC = TEXT-101.
    TRY.
        LOOP AT IT_ATTACH_FILE INTO LS_ATTACH_FILE.
          LV_FIELEC     = LS_ATTACH_FILE-FILENAME.
          LT_SOLIX_ATTH = LS_ATTACH_FILE-DATAF.
          LO_DOC_BCS->ADD_ATTACHMENT( I_ATTACHMENT_TYPE    = LC_CON-PDF
                                      I_ATTACHMENT_SUBJECT = LV_FIELEC
                                      I_ATT_CONTENT_HEX    = LT_SOLIX_ATTH ).
        ENDLOOP.
      CATCH CX_DOCUMENT_BCS.
    ENDTRY.
*--------------------------------------------------------------------*
* Set Send Excel
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* CC
*--------------------------------------------------------------------*
    IF IT_CC[] IS NOT INITIAL.
      LOOP AT IT_CC ASSIGNING <LFS_ANY>.
        LV_CC = <LFS_ANY>.
        TRY.
            LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = LV_CC  ).
          CATCH CX_ADDRESS_BCS.
        ENDTRY.
        TRY.
            LO_BCS->ADD_RECIPIENT( I_RECIPIENT = LO_RECIPIENT
                                   I_COPY      = ABAP_TRUE ).
          CATCH CX_SEND_REQ_BCS.
        ENDTRY.
      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------*
*Call Function send mail
*--------------------------------------------------------------------*
    TRY.
        LO_BCS->SEND( ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.

*--------------------------------------------------------------------*
* Commit Work And Wait
*--------------------------------------------------------------------*
    LCL_DATA=>COMMIT( ).
  endmethod.


  METHOD CHECK_SDS.
    DATA: BEGIN OF LS_GEN_C,
            REPID        TYPE  ZSDSCAC001-REPID,
            PARAM        TYPE  ZSDSCAC001-PARAM,
            PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
            SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
            PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
            PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
            VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
            VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
            VDESC        TYPE  ZSDSCAC001-VDESC,
          END OF LS_GEN_C .
    DATA: LT_GEN_C  LIKE STANDARD TABLE OF LS_GEN_C .

    DATA : IV_REPID TYPE PROGRAMM,
           LT_PARAM TYPE RANGE OF ZSDSCAC001-PARAM,
           LT_EXT   TYPE RANGE OF ZSDSCAC001-PARAM_EXT.

    DATA : LS_PARAM LIKE LINE OF LT_PARAM,
           LS_EXT   LIKE LINE OF LT_EXT.

    CONSTANTS : BEGIN OF LC_CON,
                  SIGN    TYPE C LENGTH 1  VALUE 'I',
                  OPTION  TYPE C LENGTH 2  VALUE 'EQ',
                  VALUE   TYPE C LENGTH 10 VALUE 'SDS_CLIENT',
                  PROGRAM TYPE C LENGTH 14 VALUE 'SDS_PROGRAM',
                END OF LC_CON.

    CLEAR : LT_GEN_C,LT_PARAM,LS_PARAM.

    IV_REPID = LC_CON-PROGRAM.

    LS_PARAM-SIGN   = LC_CON-SIGN.
    LS_PARAM-OPTION = LC_CON-OPTION.
    LS_PARAM-LOW    = LC_CON-VALUE.
    APPEND LS_PARAM TO LT_PARAM.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C( EXPORTING IF_REPID  = IV_REPID
                                              IRT_PARAM = LT_PARAM
                                              IRT_EXT   = LT_EXT
                                    IMPORTING ET_GEN_C  = LT_GEN_C ).

    IF LT_GEN_C IS NOT INITIAL.
      LOOP AT LT_GEN_C INTO LS_GEN_C WHERE VALUE_LOW EQ SY-MANDT.
        LV_SDS = ABAP_TRUE.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        LV_SDS = ABAP_FALSE.
      ENDIF.
    ELSE.
      LV_SDS = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  METHOD GET_USER_PARAMETER.

    SELECT SINGLE PARVA
        FROM USR05
        WHERE BNAME EQ @SY-UNAME
          AND PARID EQ @I_PARAM
      INTO @R_VALUE.


  ENDMETHOD.


  METHOD SEND_FILE_SERVER.

    DATA: LV_URL              TYPE STRING,
          LV_METHOD           TYPE STRING,
          LV_BODY_TEXT        TYPE STRING,
          LV_BODY_BIN         TYPE XSTRING,
          LV_LEN_api          TYPE I,
          LV_LEN_BIN          TYPE I,
          LV_RETURN_BODY_TEXT TYPE STRING,
          LV_RETURN_BODY_BIN  TYPE XSTRING,
          LV_MESSAGE          TYPE STRING.

    DATA : LV_STATUS TYPE C.

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA : BEGIN OF LS_BINARY,
             LINE(512) TYPE C,
           END OF LS_BINARY.

    DATA : LT_BINARY LIKE TABLE OF LS_BINARY,
           LV_LEN    TYPE I.

*    DATA : LT_STRING TYPE TABLE OF STRING,
*           LS_STRING TYPE STRING.

    DATA : LV_FILE_SEND TYPE XSTRING.

    DATA : LV_FILE_NAME TYPE STRING.

    DATA : LV_FILE_PATH TYPE STRING.

    DATA : lt_CHAR16384 TYPE TABLE OF CHAR16384,
           ls_CHAR16384 TYPE CHAR16384.

    LOOP AT IT_DATA INTO DATA(LS_DATA).
      LS_CHAR16384 = LS_DATA.
      APPEND LS_CHAR16384 TO LT_CHAR16384.
    ENDLOOP.

    IF LT_CHAR16384 IS NOT INITIAL.
      CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
        IMPORTING
          OUTPUT_LENGTH = LV_LEN
        TABLES
          TEXT_TAB      = LT_CHAR16384
          BINARY_TAB    = LT_BINARY
        EXCEPTIONS
          FAILED        = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          INPUT_LENGTH = LV_LEN
        IMPORTING
          BUFFER       = LV_FILE_SEND
        TABLES
          BINARY_TAB   = LT_BINARY
        EXCEPTIONS
          FAILED       = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      LV_FILE_NAME = I_FILE_NAME.
      LV_FILE_PATH = I_PATH.

      LV_METHOD    = 'POST'.
      LV_URL       = LCL_DATA=>ENDPOINT( ).
      LT_HEADER    = LCL_DATA=>HEADER_API( ).

      LV_BODY_TEXT = LCL_DATA=>BODY_API( I_NAME = LV_FILE_NAME
                                         I_PATH = LV_FILE_PATH
                                         I_FILE = LV_FILE_SEND ).
      LV_LEN_api   = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).


      CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
        EXPORTING
          I_URL              = LV_URL
          I_METHOD           = LV_METHOD
          I_HEADER           = LT_HEADER
          I_BODY_TEXT        = LV_BODY_TEXT
          I_BODY_BIN         = LV_BODY_BIN
          I_LEN              = LV_LEN_api
          I_LEN_BIN          = LV_LEN_BIN
        IMPORTING
*         E_RETURN           = LT_DATA_API
          E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
          E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
          E_MESSAGE          = LV_MESSAGE
          E_STATUS           = LV_STATUS.
      IF LV_STATUS EQ 'S'.
        R_RETURN = 'S'.
      ELSE.
        R_RETURN = 'E'.
      ENDIF.
    ELSE.
      R_RETURN = 'E'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
