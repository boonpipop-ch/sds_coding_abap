class ZCL_SDSCA_GEN_DATA_FILE definition
  public
  final
  create public .

public section.

  class-methods GEN_DATA_FIEL
    importing
      value(I_HEADER) type ANY TABLE optional
      value(I_ITEM) type ANY TABLE optional
      value(I_FOOTER) type ANY TABLE optional
      value(I_SEPARATOR) type ANY optional
      value(I_START_END_VALUE) type ANY optional
      value(I_FIX_LEN) type FLAG optional
      value(I_GROUP_BY_DETAIL_FILED1) type FLAG optional
      value(I_LEN) type ANY optional
    returning
      value(R_RETURN) type EREC_T_STRING .
  methods FTP_FILE_PUT
    importing
      value(I_WINDOW_PATH) type ANY optional
      !I_AL11_PATH type ANY
      !I_FILE_NAME type ANY
      value(I_USER) type ANY optional
      value(I_PASS) type ANY optional
      value(I_IP) type ANY optional
      value(I_PORT) type ANY optional
      value(IT_DATA) type EVE_TT_STRING optional
      value(I_DATA_SPIDER) type CHAR1 optional
    returning
      value(R) type CHAR1 .
  methods FTP_FILE_GET
    importing
      !I_WINDOW_PATH type ANY
      !I_AL11_PATH type ANY
      !I_FILE_NAME type ANY
      !I_USER type ANY
      !I_PASS type ANY
      !I_IP type ANY
      !I_PORT type ANY
      !I_MODE type CHAR1
    returning
      value(R) type EVE_TT_STRING .
  methods FTP_FILE_MGET
    importing
      !I_WINDOW_PATH type ANY
      !I_AL11_PATH type ANY
      !I_FILE_NAME type ANY
      !I_USER type ANY
      !I_PASS type ANY
      !I_IP type ANY
      !I_PORT type ANY
      !I_MODE type CHAR1
    returning
      value(R) type ZSDSCAS008_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCA_GEN_DATA_FILE IMPLEMENTATION.


  METHOD GEN_DATA_FIEL.

    IF I_FIX_LEN EQ ABAP_TRUE.
      R_RETURN = LCL_DATA=>GEN_FILE_FIX_LEN( EXPORTING I_HEADER          = I_HEADER
                                                       I_ITEM            = I_ITEM
                                                       I_FOOTER          = I_FOOTER
                                                       I_START_END_VALUE = I_START_END_VALUE
                                                       I_len             = I_len ).
    ELSEIF I_GROUP_BY_DETAIL_FILED1 EQ ABAP_TRUE.
      R_RETURN = LCL_DATA=>GEN_FILEGROUP( EXPORTING I_HEADER          = I_HEADER
                                                    I_ITEM            = I_ITEM
                                                    I_FOOTER          = I_FOOTER
                                                    I_SEPARATOR       = I_SEPARATOR
                                                    I_START_END_VALUE = I_START_END_VALUE ).

    ELSE.
      R_RETURN = LCL_DATA=>GEN_FILE( EXPORTING I_HEADER          = I_HEADER
                                               I_ITEM            = I_ITEM
                                               I_FOOTER          = I_FOOTER
                                               I_SEPARATOR       = I_SEPARATOR
                                               I_START_END_VALUE = I_START_END_VALUE ).
    ENDIF.

  ENDMETHOD.


  METHOD FTP_FILE_GET.
    TYPES: BEGIN OF LY_CMDOUT,
             LINE(100) TYPE C,
           END OF LY_CMDOUT.

    TYPES: BEGIN OF LY_FILES,
             FILENAME TYPE CHAR128,
           END OF LY_FILES.

    DATA: lv_CMD(40) TYPE C,
          lv_HDL     TYPE I,
          lv_KEY     TYPE I VALUE 26101957,
          lv_SLEN    TYPE I,
          lt_CMDOUT  TYPE STANDARD TABLE OF LY_CMDOUT,
          ls_CMDOUT  TYPE LY_CMDOUT.

    DATA: lv_ENCODE_PWD TYPE C LENGTH 30,
          lv_MESSAGE    TYPE STRING.

    DATA : LV_HOST TYPE C LENGTH 255.

    DATA : LS_STRING LIKE LINE OF R.

    DATA : LT_FILES TYPE TABLE OF LY_FILES,
           LS_FILES TYPE LY_FILES.

    CONSTANTS: LC_DEST TYPE RFCDES-RFCDEST VALUE 'SAPFTPA',
               LC_SAP  TYPE ESEFILEPATH VALUE '/tmp',
               LC_E    TYPE C VALUE 'E',
               LC_S    TYPE C VALUE 'S',
               LC_N    TYPE C VALUE 'N',
               LC_B    TYPE C VALUE 'B',
               LC_U    TYPE C VALUE 'U',
               LC_SL   TYPE C VALUE '/',
               LC_CD   TYPE C LENGTH 2 VALUE 'cd',
               LC_LCD  TYPE C LENGTH 3 VALUE 'lcd',
               LC_GET  TYPE C LENGTH 3 VALUE 'get',
               LC_DEL  TYPE C LENGTH 3 VALUE 'del',
               LC_ESZZ TYPE C LENGTH 4 VALUE '8600'.

    SET EXTENDED CHECK OFF.
    lv_SLEN = STRLEN( I_PASS ).

    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        SOURCE      = I_PASS
        SOURCELEN   = lv_SLEN
        KEY         = lv_KEY
      IMPORTING
        DESTINATION = lv_ENCODE_PWD.

    CONCATENATE I_IP I_PORT INTO LV_HOST SEPARATED BY SPACE.

    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
        USER            = I_USER
        PASSWORD        = LV_ENCODE_PWD
        HOST            = LV_HOST
        RFC_DESTINATION = LC_DEST
      IMPORTING
        HANDLE          = LV_HDL
      EXCEPTIONS
        NOT_CONNECTED   = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ENDIF.

    CONCATENATE LC_CD I_WINDOW_PATH INTO LV_CMD SEPARATED BY SPACE.
    REFRESH LT_CMDOUT.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        HANDLE        = LV_HDL
        COMMAND       = LV_CMD
        COMPRESS      = LC_N
      TABLES
        DATA          = LT_CMDOUT
      EXCEPTIONS
        COMMAND_ERROR = 1
        TCPIP_ERROR   = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ENDIF.

    CONCATENATE LC_LCD I_AL11_PATH INTO LV_CMD SEPARATED BY SPACE.
    REFRESH LT_CMDOUT.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        HANDLE        = LV_HDL
        COMMAND       = LV_CMD
        COMPRESS      = LC_N
      TABLES
        DATA          = LT_CMDOUT
      EXCEPTIONS
        COMMAND_ERROR = 1
        TCPIP_ERROR   = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ENDIF.

    CONCATENATE LC_GET I_FILE_NAME INTO LV_CMD SEPARATED BY SPACE.
    REFRESH LT_CMDOUT.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        HANDLE        = LV_HDL
        COMMAND       = LV_CMD
        COMPRESS      = LC_N
      TABLES
        DATA          = LT_CMDOUT
      EXCEPTIONS
        COMMAND_ERROR = 1
        TCPIP_ERROR   = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ELSE.
      CONCATENATE LC_SAP LC_SL I_FILE_NAME INTO LS_FILES.
      IF     I_MODE EQ LC_B.
        OPEN DATASET LS_FILES FOR INPUT IN LEGACY TEXT MODE BIG ENDIAN CODE PAGE LC_ESZZ .
      ELSEIF I_MODE EQ LC_U.
        OPEN DATASET LS_FILES FOR INPUT IN TEXT MODE ENCODING UTF-8.
      ENDIF.
      DO.
        READ DATASET LS_FILES INTO LS_STRING.
        IF SY-SUBRC EQ 0.
          APPEND LS_STRING TO R.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      DELETE DATASET LS_FILES.
    ENDIF.

    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        HANDLE = LV_HDL.
  ENDMETHOD.


  METHOD FTP_FILE_MGET.
    TYPES: BEGIN OF LY_CMDOUT,
             LINE(100) TYPE C,
           END OF LY_CMDOUT.

    TYPES: BEGIN OF LY_FILES,
             FILENAME TYPE CHAR128,
           END OF LY_FILES.

    DATA: lv_CMD(40) TYPE C,
          lv_HDL     TYPE I,
          lv_KEY     TYPE I VALUE 26101957,
          lv_SLEN    TYPE I,
          lt_CMDOUT  TYPE STANDARD TABLE OF LY_CMDOUT,
          ls_CMDOUT  TYPE LY_CMDOUT.

    DATA: lv_ENCODE_PWD TYPE C LENGTH 30,
          lv_MESSAGE    TYPE STRING.

    DATA : LV_HOST TYPE C LENGTH 255.

    DATA : LS_STRING LIKE LINE OF R.

    DATA : LS_TMP TYPE STRING.

    DATA : LT_FILES TYPE TABLE OF LY_FILES,
           LS_FILES TYPE LY_FILES.

    DATA : LT_FILE TYPE EVE_TT_STRING,
           LS_FILE LIKE LINE OF LT_FILE.

    CONSTANTS: LC_DEST TYPE RFCDES-RFCDEST VALUE 'SAPFTPA',
               LC_SAP  TYPE ESEFILEPATH VALUE '/tmp',
               LC_E    TYPE C VALUE 'E',
               LC_S    TYPE C VALUE 'S',
               LC_N    TYPE C VALUE 'N',
               LC_B    TYPE C VALUE 'B',
               LC_U    TYPE C VALUE 'U',
               LC_SL   TYPE C VALUE '/',
               LC_CD   TYPE C LENGTH 2 VALUE 'cd',
               LC_LCD  TYPE C LENGTH 3 VALUE 'lcd',
               LC_MGET TYPE C LENGTH 4 VALUE 'mget',
               LC_DEL  TYPE C LENGTH 3 VALUE 'del',
               LC_ESZZ TYPE C LENGTH 4 VALUE '8600'.

    SET EXTENDED CHECK OFF.
    lv_SLEN = STRLEN( I_PASS ).

    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        SOURCE      = I_PASS
        SOURCELEN   = lv_SLEN
        KEY         = lv_KEY
      IMPORTING
        DESTINATION = lv_ENCODE_PWD.

    CONCATENATE I_IP I_PORT INTO LV_HOST SEPARATED BY SPACE.

    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
        USER            = I_USER
        PASSWORD        = LV_ENCODE_PWD
        HOST            = LV_HOST
        RFC_DESTINATION = LC_DEST
      IMPORTING
        HANDLE          = LV_HDL
      EXCEPTIONS
        NOT_CONNECTED   = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ENDIF.

    CONCATENATE LC_CD I_WINDOW_PATH INTO LV_CMD SEPARATED BY SPACE.
    REFRESH LT_CMDOUT.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        HANDLE        = LV_HDL
        COMMAND       = LV_CMD
        COMPRESS      = LC_N
      TABLES
        DATA          = LT_CMDOUT
      EXCEPTIONS
        COMMAND_ERROR = 1
        TCPIP_ERROR   = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ENDIF.

    CONCATENATE LC_LCD I_AL11_PATH INTO LV_CMD SEPARATED BY SPACE.
    REFRESH LT_CMDOUT.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        HANDLE        = LV_HDL
        COMMAND       = LV_CMD
        COMPRESS      = LC_N
      TABLES
        DATA          = LT_CMDOUT
      EXCEPTIONS
        COMMAND_ERROR = 1
        TCPIP_ERROR   = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ENDIF.

    CONCATENATE LC_MGET I_FILE_NAME INTO LV_CMD SEPARATED BY SPACE.
    REFRESH LT_CMDOUT.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        HANDLE        = LV_HDL
        COMMAND       = LV_CMD
        COMPRESS      = LC_N
      TABLES
        DATA          = LT_CMDOUT
      EXCEPTIONS
        COMMAND_ERROR = 1
        TCPIP_ERROR   = 2.
    IF SY-SUBRC <> 0.
*      LS_STRING = LC_E.
*      APPEND LS_STRING TO R.
      RETURN.
    ELSE.

      LT_FILE = LCL_DATA=>GET_PATH( I_PATH = LC_SAP
                                    I_FILE = I_FILE_NAME ).

      LOOP AT LT_FILE INTO LS_FILE.
*        CONCATENATE LC_SAP LC_SL I_FILE_NAME INTO LS_FILES.
        IF     I_MODE EQ LC_B.
          OPEN DATASET LS_FILE FOR INPUT IN LEGACY TEXT MODE BIG ENDIAN CODE PAGE LC_ESZZ .
        ELSEIF I_MODE EQ LC_U.
          OPEN DATASET LS_FILE FOR INPUT IN TEXT MODE ENCODING UTF-8.
        ENDIF.
        DO.
          READ DATASET LS_FILE INTO LS_TMP.
          IF SY-SUBRC EQ 0.
            LS_STRING-FILE = I_FILE_NAME.
            LS_STRING-DATA = LS_TMP.
            APPEND LS_STRING TO R.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        DELETE DATASET LS_FILE.
      ENDLOOP.
    ENDIF.


    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        HANDLE = LV_HDL.
  ENDMETHOD.


  METHOD FTP_FILE_PUT.
    TYPES: BEGIN OF LY_CMDOUT,
             LINE(100) TYPE C,
           END OF LY_CMDOUT.

    DATA: lv_CMD(200) TYPE C,
          lv_HDL      TYPE I,
          lv_KEY      TYPE I VALUE 26101957,
          lv_SLEN     TYPE I,
          lt_CMDOUT   TYPE STANDARD TABLE OF LY_CMDOUT,
          ls_CMDOUT   TYPE LY_CMDOUT.

    DATA: lv_ENCODE_PWD TYPE C LENGTH 30,
          lv_MESSAGE    TYPE STRING.

    DATA : LV_HOST TYPE C LENGTH 255.

    DATA : LS_DATA LIKE LINE OF IT_DATA.

    DATA : LV_PATH TYPE STRING.

    DATA : LV_MSG TYPE C LENGTH 200.

    DATA : LV_CHK TYPE C.

    CONSTANTS: LC_DEST TYPE RFCDES-RFCDEST VALUE 'SAPFTPA',
               LC_SAP  TYPE ESEFILEPATH VALUE '/tmp',
               LC_E    TYPE C VALUE 'E',
               LC_S    TYPE C VALUE 'S',
               LC_N    TYPE C VALUE 'N',
               LC_SL   TYPE C VALUE '/',
               LC_CD   TYPE C LENGTH 2 VALUE 'cd',
               LC_LCD  TYPE C LENGTH 3 VALUE 'lcd',
               LC_PUT  TYPE C LENGTH 3 VALUE 'put'.

    IF I_DATA_SPIDER NE ABAP_TRUE.
*    SET EXTENDED CHECK OFF.
      lv_SLEN = STRLEN( I_PASS ).

      CALL FUNCTION 'HTTP_SCRAMBLE'
        EXPORTING
          SOURCE      = I_PASS
          SOURCELEN   = lv_SLEN
          KEY         = lv_KEY
        IMPORTING
          DESTINATION = lv_ENCODE_PWD.

      CONCATENATE I_IP I_PORT INTO LV_HOST SEPARATED BY SPACE.

      CALL FUNCTION 'FTP_CONNECT'
        EXPORTING
          USER            = I_USER
          PASSWORD        = LV_ENCODE_PWD
          HOST            = LV_HOST
          RFC_DESTINATION = LC_DEST
        IMPORTING
          HANDLE          = LV_HDL
        EXCEPTIONS
          NOT_CONNECTED   = 1
          OTHERS          = 2.
      IF SY-SUBRC <> 0.
        R = LC_E.
        RETURN.
      ENDIF.

      CONCATENATE LC_CD I_WINDOW_PATH INTO LV_CMD SEPARATED BY SPACE.
      REFRESH LT_CMDOUT.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          HANDLE        = LV_HDL
          COMMAND       = LV_CMD
          COMPRESS      = LC_N
        TABLES
          DATA          = LT_CMDOUT
        EXCEPTIONS
          COMMAND_ERROR = 1
          TCPIP_ERROR   = 2.
      IF SY-SUBRC <> 0.
        R = LC_E.
        RETURN.
      ENDIF.

      CONCATENATE LC_LCD I_AL11_PATH INTO LV_CMD SEPARATED BY SPACE.
      REFRESH LT_CMDOUT.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          HANDLE        = LV_HDL
          COMMAND       = LV_CMD
          COMPRESS      = LC_N
        TABLES
          DATA          = LT_CMDOUT
        EXCEPTIONS
          COMMAND_ERROR = 1
          TCPIP_ERROR   = 2.
      IF SY-SUBRC <> 0.
        R = LC_E.
        RETURN.
      ENDIF.

      IF IT_DATA IS NOT INITIAL.
        CONCATENATE I_AL11_PATH LC_SL I_FILE_NAME INTO LV_PATH.
        OPEN DATASET LV_PATH FOR OUTPUT MESSAGE LV_MSG
                             IN TEXT MODE ENCODING UTF-8.
        IF SY-SUBRC EQ 0.
          LV_CHK = ABAP_TRUE.
          LOOP AT IT_DATA INTO LS_DATA.
            TRANSFER LS_DATA TO LV_PATH.
          ENDLOOP.
          CLOSE DATASET LV_PATH.
        ELSE.
          LV_CHK = ABAP_FALSE.
        ENDIF.
      ENDIF.

      CONCATENATE LC_PUT I_FILE_NAME INTO LV_CMD SEPARATED BY SPACE.
      REFRESH LT_CMDOUT.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          HANDLE        = LV_HDL
          COMMAND       = LV_CMD
          COMPRESS      = LC_N
        TABLES
          DATA          = LT_CMDOUT
        EXCEPTIONS
          COMMAND_ERROR = 1
          TCPIP_ERROR   = 2.
      IF SY-SUBRC <> 0.
        R = LC_E.
        IF LV_CHK = ABAP_TRUE.
          DELETE DATASET LV_PATH.
        ENDIF.
        RETURN.
      ELSE.
        R = LC_S.
      ENDIF.

      IF LV_CHK = ABAP_TRUE.
        DELETE DATASET LV_PATH.
      ENDIF.

      CALL FUNCTION 'FTP_DISCONNECT'
        EXPORTING
          HANDLE = LV_HDL.
    ELSE.
      IF IT_DATA IS NOT INITIAL.
        CONCATENATE I_AL11_PATH LC_SL I_FILE_NAME INTO LV_PATH.
        OPEN DATASET LV_PATH FOR OUTPUT MESSAGE LV_MSG
                             IN TEXT MODE ENCODING UTF-8.
        IF SY-SUBRC EQ 0.
          LV_CHK = ABAP_TRUE.
          LOOP AT IT_DATA INTO LS_DATA.
            TRANSFER LS_DATA TO LV_PATH.
          ENDLOOP.
          R = LC_S.
          CLOSE DATASET LV_PATH.
        ELSE.
          LV_CHK = ABAP_FALSE.
        ENDIF.
      ELSE.
        CONCATENATE I_AL11_PATH LC_SL I_FILE_NAME INTO LV_PATH.
        OPEN DATASET LV_PATH FOR OUTPUT MESSAGE LV_MSG
                             IN TEXT MODE ENCODING UTF-8.
        IF SY-SUBRC EQ 0.
          LV_CHK = ABAP_TRUE.
*          TRANSFER LS_DATA TO LV_PATH.
          R = LC_S.
          CLOSE DATASET LV_PATH.
        ELSE.
          LV_CHK = ABAP_FALSE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
