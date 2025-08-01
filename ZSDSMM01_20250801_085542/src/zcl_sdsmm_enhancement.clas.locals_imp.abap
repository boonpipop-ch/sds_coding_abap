*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0650_CLASS
*&---------------------------------------------------------------------*
CLASS LCL_UTIL DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      CONVERT_ALPHA_IN  IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      CONVERT_ALPHA_OUT IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY.
ENDCLASS.
CLASS LCL_UTIL IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_IN.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_OUT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_outPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
ENDCLASS.
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      SAVE IMPORTING I_DATA TYPE TY_T_MSEG,
      CALL_API IMPORTING I_DATA  TYPE ANY
                         I_MBLNR TYPE MKPF-MBLNR
                         I_MJAHR TYPE MKPF-MJAHR,
      ENDPOINT RETURNING VALUE(R) TYPE STRING,
      HEADER RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY IMPORTING I_DATA   TYPE ANY
           RETURNING VALUE(R) TYPE STRING,
      BODY_LEN IMPORTING I_DATA   TYPE STRING
               RETURNING VALUE(R) TYPE I,
      GEN_DATA_FORMAT IMPORTING I_DATA   TYPE SY-DATUM
                      RETURNING VALUE(R) TYPE STRING,
      CHECK_RESERVATION IMPORTING IT_DATA  TYPE GOITEM_T
                        RETURNING VALUE(R) TYPE CHAR255,
      SEND_DATA_AGAIN IMPORTING I_YEAR TYPE ZSDSMMT027-MJAHR
                                I_DOC  TYPE ZSDSMMT027-MBLNR.
    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.

    CONSTANTS : BEGIN OF LC_CON,
                  S   TYPE C LENGTH 1 VALUE 'S',
                  E   TYPE C LENGTH 1 VALUE 'E',
                  DEV TYPE C LENGTH 3 VALUE 'F36',
                  QAS TYPE C LENGTH 3 VALUE 'F46',
                  PRD TYPE C LENGTH 3 VALUE 'F56',
                END OF LC_CON.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD SAVE.
    DATA : BEGIN OF LS_TRANSFER,
             _TRANSFER_KEY       TYPE STRING,
             _RESER_KEY          TYPE STRING,
             _MAT_DOC_NO         TYPE STRING,
             _MAT_DOC_ITEM       TYPE STRING,
             _SAP_CREATED_DATE   TYPE STRING,
             _QUANTITY           TYPE I,
             _TYPE               TYPE STRING,
             _PRODUCT_CODE       TYPE STRING,
             _FROM_LOCATION      TYPE STRING,
             _TO_LOCATION        TYPE STRING,
             _REF_SAP_RESER_NO   TYPE STRING,
             _REF_SAP_RESER_ITEM TYPE STRING,
           END OF LS_TRANSFER.
    DATA : LT_TRANSFER_ITEM LIKE TABLE OF LS_TRANSFER.

    DATA : BEGIN OF LS_BODY,
             _REF_SAP_RESER_NO TYPE STRING,
             _MAT_DOC_NO       TYPE STRING,
             _TRANS_FER_ITEM   LIKE LT_TRANSFER_ITEM,
           END OF LS_BODY.

    DATA LS_TMP LIKE LINE OF I_DATA.

    DATA LT_TMP LIKE I_DATA.

    LT_TMP = I_DATA.

    SORT LT_TMP BY SHKZG.
    DELETE LT_TMP WHERE SHKZG EQ 'S'.

    SORT LT_TMP BY MBLNR MJAHR ZEILE.

    DATA : BEGIN OF LS_RSNUM,
             RSNUM TYPE MSEG-RSNUM,
           END OF LS_RSNUM.
    DATA : LT_RSNUM LIKE HASHED TABLE OF LS_RSNUM WITH UNIQUE KEY RSNUM.

    LT_RSNUM =  CORRESPONDING #( LT_TMP  DISCARDING DUPLICATES ).

    SELECT COUNT( * )
      FROM @LT_RSNUM AS A
      INNER JOIN ZSDSMMT028 ON A~RSNUM EQ ZSDSMMT028~RSNUM AND
                               ZSDSMMT028~STATU EQ 'SAV'.
    IF SY-SUBRC NE 0.
      LOOP AT LT_TMP INTO DATA(LS_DATA) WHERE RSNUM IS NOT INITIAL.
        MOVE-CORRESPONDING LS_DATA TO LS_TMP.

        LS_BODY-_REF_SAP_RESER_NO = LS_TMP-RSNUM.
        LS_BODY-_MAT_DOC_NO       = LS_TMP-MBLNR.

        CONCATENATE LS_TMP-MBLNR '/' LS_TMP-ZEILE INTO LS_TRANSFER-_TRANSFER_KEY.
        CONCATENATE LS_TMP-RSNUM '/' LS_TMP-RSPOS INTO LS_TRANSFER-_RESER_KEY.
        LS_TRANSFER-_MAT_DOC_NO       = LS_TMP-MBLNR.
        LS_TRANSFER-_MAT_DOC_ITEM     = LS_TMP-ZEILE.
        LS_TRANSFER-_SAP_CREATED_DATE = GEN_DATA_FORMAT( SY-DATUM ).
        LS_TRANSFER-_QUANTITY         = LS_TMP-MENGE.
        IF LS_TMP-XAUTO EQ ABAP_TRUE.
          LS_TRANSFER-_TYPE             = 'Cancelled'.
        ELSE.
          LS_TRANSFER-_TYPE             = 'Transferred'.
        ENDIF.
        LS_TRANSFER-_PRODUCT_CODE       = LS_TMP-MATNR.
        LS_TRANSFER-_FROM_LOCATION      = LS_TMP-LGORT.
        LS_TRANSFER-_TO_LOCATION        = LS_TMP-UMLGO.
        LS_TRANSFER-_REF_SAP_RESER_NO   = LS_TMP-RSNUM.
        LS_TRANSFER-_REF_SAP_RESER_ITEM = LS_TMP-RSPOS.
        APPEND LS_TRANSFER TO LS_BODY-_TRANS_FER_ITEM.
        AT END OF MBLNR.
          CALL_API( I_DATA  = LS_BODY
                    I_MBLNR = LS_TMP-MBLNR
                    I_MJAHR = LS_TMP-MJAHR ).
        ENDAT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD ENDPOINT.
    CONSTANTS : BEGIN OF LC_API,
                  REPID TYPE C LENGTH 22 VALUE 'ZSDSMMR0650',
                  PARAM TYPE C LENGTH 22 VALUE 'ENDPOINT_SFDC_RES',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                END OF LC_API.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PARAM
                                                      I_PARAM_EXT         = LC_API-DEV
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PARAM
                                                      I_PARAM_EXT         = LC_API-QAS
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PARAM
                                                      I_PARAM_EXT         = LC_API-PRD
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD HEADER.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    DATA : LV_TOKEN TYPE STRING.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

  ENDMETHOD.
  METHOD BODY.
    CALL METHOD /UI2/CL_JSON=>SERIALIZE
      EXPORTING
        DATA        = I_DATA
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
      RECEIVING
        R_JSON      = R
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.
  METHOD BODY_LEN.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD CALL_API.
    DATA: LV_URL              TYPE STRING,
          LV_METHOD           TYPE STRING,
          LV_BODY_TEXT        TYPE STRING,
          LV_BODY_BIN         TYPE XSTRING,
          LV_LEN              TYPE I,
          LV_LEN_BIN          TYPE I,
          LV_RETURN_BODY_TEXT TYPE STRING,
          LV_RETURN_BODY_BIN  TYPE XSTRING,
          LV_MESSAGE          TYPE STRING,
          LV_STATUS           TYPE C.

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: BEGIN OF LS_TRANSFER_ITEM,
            _Transfer_Key TYPE STRING,
            _Status       TYPE STRING,
          END OF LS_TRANSFER_ITEM.

    DATA: BEGIN OF LS_DATA,
            _Ref_Sap_Reser_No TYPE STRING,
            _Mat_Doc_No       TYPE STRING,
            _Response_Status  TYPE STRING,
            _Response_Message TYPE STRING,
            _Response         LIKE TABLE OF LS_TRANSFER_ITEM,
          END OF LS_DATA.

    DATA : LV_TOKEN TYPE STRING.

    DATA : LS_ZSDSMMT027 TYPE ZSDSMMT027.

    DATA : LV_CHECK TYPE C.

    LV_METHOD    = 'POST'.
    LV_URL       = LCL_DATA=>ENDPOINT( ).
    IF LV_URL IS NOT INITIAL.
      LT_HEADER    = LCL_DATA=>HEADER( ).
      LV_BODY_TEXT = LCL_DATA=>BODY( I_DATA ).
      LV_LEN       = LCL_DATA=>BODY_LEN( LV_BODY_TEXT ).

      CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
        EXPORTING
          I_URL              = LV_URL
          I_METHOD           = LV_METHOD
          I_HEADER           = LT_HEADER
          I_BODY_TEXT        = LV_BODY_TEXT
          I_BODY_BIN         = LV_BODY_BIN
          I_LEN              = LV_LEN
          I_LEN_BIN          = LV_LEN_BIN
        IMPORTING
          E_RETURN           = LS_DATA
          E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
          E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
          E_MESSAGE          = LV_MESSAGE
          E_STATUS           = LV_STATUS.
      LS_ZSDSMMT027-MBLNR = I_MBLNR.
      LS_ZSDSMMT027-MJAHR = I_MJAHR.
      IF LV_STATUS EQ 'S'.
        IF LS_DATA-_Response_Status EQ 'Success' OR
           LS_DATA-_Response_Status EQ 'SUCCESS' OR
           LS_DATA-_Response_Status EQ 'S'.
          LS_ZSDSMMT027-METYP = 'S'.
          LS_ZSDSMMT027-MESSG = LS_DATA-_Response_MESSAGE.
        ELSE.
          MESSAGE I001(ZSDSMM01) WITH TEXT-E04.
          LS_ZSDSMMT027-METYP = 'E'.
          LS_ZSDSMMT027-MESSG = LS_DATA-_Response_MESSAGE.
*          LCL_DATA=>SEND_DATA_AGAIN( I_YEAR = LS_ZSDSMMT027-MBLNR
*                                     I_DOC  = LS_ZSDSMMT027-MJAHR ).
        ENDIF.
      ELSE.
        MESSAGE I000(ZSDSMM01) WITH TEXT-E04.
        LS_ZSDSMMT027-METYP = 'E'.
        LS_ZSDSMMT027-MESSG = TEXT-E01.
        LCL_DATA=>SEND_DATA_AGAIN( I_YEAR = LS_ZSDSMMT027-MJAHR
                                   I_DOC  = LS_ZSDSMMT027-MBLNR ).
      ENDIF.

      SELECT SINGLE ERNAM,
                    ERDAT,
                    ERZET
        FROM ZSDSMMT027
        WHERE MBLNR EQ @LS_ZSDSMMT027-MBLNR
          AND MJAHR EQ @LS_ZSDSMMT027-MJAHR
        INTO @DATA(LS_CHECK_INSERT).
      IF SY-SUBRC EQ 0.
        LS_ZSDSMMT027-ERNAM = LS_CHECK_INSERT-ERNAM.
        LS_ZSDSMMT027-ERDAT = LS_CHECK_INSERT-ERDAT.
        LS_ZSDSMMT027-ERZET = LS_CHECK_INSERT-ERZET.
      ELSE.
        LS_ZSDSMMT027-ERNAM = SY-UNAME.
        LS_ZSDSMMT027-ERDAT = SY-DATUM.
        LS_ZSDSMMT027-ERZET = SY-UZEIT.
      ENDIF.

      LS_ZSDSMMT027-AENAM = SY-UNAME.
      LS_ZSDSMMT027-AEDAT = SY-DATUM.
      LS_ZSDSMMT027-AEZET = SY-UZEIT.

      MODIFY ZSDSMMT027 FROM LS_ZSDSMMT027.
    ELSE.
      LS_ZSDSMMT027-METYP = 'E'.
      LS_ZSDSMMT027-MESSG = TEXT-E02.
    ENDIF.

  ENDMETHOD.
  METHOD GEN_DATA_FORMAT.
    CONCATENATE I_DATA+0(4) '-'
                I_DATA+4(2) '-'
                I_DATA+6(2)
           INTO R.
  ENDMETHOD.
  METHOD CHECK_RESERVATION.
    DATA : BEGIN OF LS_RESER,
             RSNUM TYPE MSEG-RSNUM,
           END OF LS_RESER.
    DATA : LT_RESER LIKE HASHED TABLE OF LS_RESER WITH UNIQUE KEY RSNUM.

    LT_RESER =  CORRESPONDING #( IT_DATA  DISCARDING DUPLICATES ).

    DESCRIBE TABLE LT_RESER LINES DATA(LV_LINE).

    IF LV_LINE GT 1.
      R = TEXT-E03.
    ELSE.
      CLEAR R.
    ENDIF.

  ENDMETHOD.
  METHOD SEND_DATA_AGAIN.
    DATA : S_MJAHR TYPE RANGE OF MKPF-MJAHR,
           S_MBLNR TYPE RANGE OF MKPF-MBLNR.

    DATA : LV_JOBNAME  TYPE TBTCO-JOBNAME,
           LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

    DATA: LV_DATA TYPE SY-DATUM,
          LV_TIME TYPE SY-UZEIT.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.
    DATA : LS_SELTAB           TYPE TABLE OF RSPARAMS.

    S_MJAHR =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_YEAR ) ).
    S_MBLNR =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_DOC ) ).

    CONCATENATE 'RESER' '_' SY-UNAME '_' SY-UZEIT INTO LV_JOBNAME.

    LV_DATA = SY-DATUM.
    LV_TIME = SY-UZEIT + 120.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LV_JOBNAME
        SDLSTRTDT        = LV_DATA
        SDLSTRTTM        = LV_TIME
      IMPORTING
        JOBCOUNT         = LV_JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
*       immediately            = 'X'
        NO_DIALOG              = 'X'
        USER                   = SY-UNAME
      IMPORTING
        OUT_PARAMETERS         = LS_PRINT_PARAMETERS
      EXCEPTIONS
        ARCHIVE_INFO_NOT_FOUND = 1
        INVALID_PRINT_PARAMS   = 2
        INVALID_ARCHIVE_PARAMS = 3
        OTHERS                 = 4.

    SUBMIT ZSDSMMR0650 TO SAP-SPOOL AND RETURN
                                   WITH SELECTION-TABLE LS_SELTAB
                                                   USER SY-UNAME
                                       SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                   WITHOUT SPOOL DYNPRO
                                                VIA JOB LV_JOBNAME
                                                 NUMBER LV_JOBCOUNT
                            USING SELECTION-SCREEN  1000
                            WITH S_MJAHR    IN S_MJAHR
                            WITH S_MBLNR    IN S_MBLNR
                            WITH P_AUTO EQ ABAP_TRUE.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LV_JOBCOUNT
        JOBNAME              = LV_JOBNAME
        SDLSTRTDT            = LV_DATA
        SDLSTRTTM            = LV_TIME
*       STRTIMMED            = ABAP_TRUE
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        OTHERS               = 8.

  ENDMETHOD.
ENDCLASS.
