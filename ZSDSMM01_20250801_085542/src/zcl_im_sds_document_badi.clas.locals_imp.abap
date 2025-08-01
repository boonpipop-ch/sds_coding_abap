*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      EXPORT_MB1A_TO_WMS IMPORTING I_XMKPF  TYPE TY_T_MKPF
                                   I_XMSEG  TYPE TY_T_MSEG
                                   I_XVM07M TYPE TY_T_VM07M,
      CHECK_SDS        IMPORTING LV_MANDT TYPE SY-MANDT
                       RETURNING VALUE(R) LIKE ABAP_TRUE.

    CLASS-DATA : LCL_GEN_FILE TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    CONSTANTS : BEGIN OF GC_CON,
                  AG     TYPE C LENGTH 2 VALUE 'AG',
                  ZERO   TYPE C LENGTH 1 VALUE '0',
                  SDS    TYPE C LENGTH 3 VALUE 'SDS',
                  SVC_IN TYPE C LENGTH 6 VALUE 'SVC_IN',
                END OF GC_CON.

ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD EXPORT_MB1A_TO_WMS.
    TYPES: BEGIN OF TYP_ITAB_WMS_TXT,
             ZFLAG(1)           TYPE  C,
             ZCUST_ID(3)        TYPE  C,
             ZDOC_NO(10)        TYPE  C,
             ZDOC_DATE(8)       TYPE  C,
             ZDOC_CRE_DATE(8)   TYPE  C,
             ZLINE_ITEM(4)      TYPE  C,
             ZMATERIAL(18)      TYPE  C,
             ZMAT_DESC(40)      TYPE  C,
             ZQTY(8)            TYPE  C,
             ZSUPPLIER_CODE(10) TYPE  C,
             ZSUPPLIER_NAME(40) TYPE  C,
             ZDELIVER_TO(60)    TYPE  C,
             ZREMARK(30)        TYPE  C,
             ZREMARK2(255)      TYPE  C,
             ZCHECK(1)          TYPE  C,
             ZWH_CODE(10)       TYPE  C,
           END OF TYP_ITAB_WMS_TXT.
    DATA: LT_ITAB_WMS_TXT     TYPE STANDARD TABLE OF TYP_ITAB_WMS_TXT.
    DATA: LS_ITAB_WMS_TXT TYPE TYP_ITAB_WMS_TXT.
    DATA: LS_LIKP TYPE LIKP,
          LS_LIPS TYPE LIPSVB.
    DATA: LV_QTY              TYPE I.
    DATA: LV_TEXT_EXPORT(506)    TYPE C.
    DATA: G_SPACE TYPE STRING,
          L_POS   TYPE I.
    DATA: MESTYP1   TYPE  EDIDC-MESTYP.
    DATA: LV_PATH TYPE STRING.
    DATA: LV_FILENAME TYPE RLGRAP-FILENAME.

    DATA: BEGIN OF LS_MAKT_1,
            MATNR TYPE MAKT-MATNR,
            MAKTX TYPE MAKT-MAKTX,
          END OF LS_MAKT_1.
    DATA: LT_MAKT_1 LIKE TABLE OF LS_MAKT_1.

    DATA: BEGIN OF LS_CAUFV,
            AUFNR TYPE CAUFV-AUFNR,
            KTEXT TYPE CAUFV-KTEXT,
            VAPLZ TYPE CAUFV-VAPLZ,
            PARNR TYPE IHPA-PARNR,
            ADRNR TYPE IHPA-ADRNR,
          END OF LS_CAUFV.
    DATA: LT_CAUFV LIKE TABLE OF LS_CAUFV.

    DATA: BEGIN OF LS_WORK_CENTER,
            OBJID TYPE CRHD-OBJID,    "object no
            ARBPL TYPE CRHD-ARBPL,    "Work Center
            KTEXT TYPE CRTX-KTEXT,    "name work center
          END OF LS_WORK_CENTER.
    DATA: LT_WORK_CENTER LIKE TABLE OF LS_WORK_CENTER.
    DATA: LT_RESB1 TYPE STANDARD TABLE OF RESB.
    DATA: LS_RESB1 TYPE RESB.
    DATA : IS_MSEG_1  TYPE  MSEG.
    DATA : IS_MKPF TYPE MKPF.

    DATA: LV_ADRNR TYPE KNA1-ADRNR.
    DATA: LV_MBLNR TYPE MSEG-MBLNR.
    DATA: LV_SPACE TYPE STRING.

    DATA: LT_FILE TYPE EREC_T_STRING.

    DATA: LV_STATUS TYPE CHAR1.
*-------------------------------------------------------------
* Select Data=
**------------------------------------------------------------
*    IF (  SY-UNAME NE 'RFC_EPGW' AND
*          SY-UNAME NE 'SDSBATCH' AND  "Edit by Wantanee T41K923859
*          SY-UNAME NE 'TAKADA.K' AND
*          SY-UNAME NE 'SFDC' ). "Add by Wantanee T41K923986
      IF I_XMSEG IS NOT INITIAL.
        SELECT MATNR
               MAKTX
          FROM MAKT
          INTO TABLE LT_MAKT_1
          FOR ALL ENTRIES IN I_XMSEG
          WHERE MATNR EQ I_XMSEG-MATNR
            AND SPRAS EQ SY-LANGU.

        SELECT A~AUFNR A~KTEXT A~VAPLZ
               B~PARNR B~ADRNR
        INTO TABLE LT_CAUFV
        FROM CAUFV AS A INNER JOIN IHPA AS B
                        ON ( A~OBJNR EQ B~OBJNR
                        AND  B~PARVW EQ GC_CON-AG )

        FOR ALL ENTRIES IN I_XMSEG
        WHERE A~AUFNR EQ I_XMSEG-AUFNR.

        IF NOT LT_CAUFV IS INITIAL.
          SELECT A~OBJID A~ARBPL B~KTEXT
           INTO TABLE  LT_WORK_CENTER
           FROM CRHD AS A INNER JOIN CRTX AS B
                          ON ( A~OBJID EQ B~OBJID ).
        ENDIF.
      ENDIF.

      LOOP AT I_XMSEG INTO IS_MSEG_1 WHERE ( BWART EQ '905' ) OR
                                           ( BWART EQ '907' ).
        CLEAR: LV_ADRNR,LV_MBLNR.
        IF IS_MSEG_1-LGORT+0(2) EQ '21'.
          LS_ITAB_WMS_TXT-ZFLAG    = GC_CON-ZERO.
          LS_ITAB_WMS_TXT-ZCUST_ID = GC_CON-SDS.
          LS_ITAB_WMS_TXT-ZDOC_NO  = IS_MSEG_1-MBLNR.
          LV_MBLNR = IS_MSEG_1-MBLNR.

          READ TABLE I_XMKPF INTO IS_MKPF INDEX 1.
          IF SY-SUBRC EQ 0.
            LS_ITAB_WMS_TXT-ZDOC_DATE = IS_MKPF-BUDAT.
          ENDIF.


          LS_ITAB_WMS_TXT-ZDOC_CRE_DATE = SY-DATUM.
          LS_ITAB_WMS_TXT-ZLINE_ITEM = IS_MSEG_1-ZEILE.
          LS_ITAB_WMS_TXT-ZMATERIAL = IS_MSEG_1-MATNR.

          READ TABLE LT_MAKT_1 INTO LS_MAKT_1
                             WITH KEY MATNR = IS_MSEG_1-MATNR.
          IF SY-SUBRC = 0.
            LS_ITAB_WMS_TXT-ZMAT_DESC = LS_MAKT_1-MAKTX.
          ENDIF.

          LV_QTY = IS_MSEG_1-MENGE.
          LS_ITAB_WMS_TXT-ZQTY = LV_QTY.
          CONDENSE LS_ITAB_WMS_TXT-ZQTY NO-GAPS.
          READ TABLE LT_CAUFV INTO LS_CAUFV
                           WITH KEY AUFNR = IS_MSEG_1-AUFNR.
          IF SY-SUBRC = 0.
            LS_ITAB_WMS_TXT-ZSUPPLIER_CODE = LS_CAUFV-VAPLZ.


            READ TABLE LT_WORK_CENTER INTO LS_WORK_CENTER
                                WITH KEY ARBPL = LS_CAUFV-VAPLZ.
            IF SY-SUBRC EQ 0.

              LS_ITAB_WMS_TXT-ZSUPPLIER_NAME = LS_WORK_CENTER-KTEXT.
            ENDIF.
            LS_ITAB_WMS_TXT-ZDELIVER_TO = GC_CON-SVC_IN.

            CONCATENATE  LS_CAUFV-AUFNR LS_CAUFV-KTEXT
                  INTO LS_ITAB_WMS_TXT-ZREMARK2 SEPARATED BY SPACE.
          ENDIF.
          CONCATENATE IS_MSEG_1-RSNUM IS_MSEG_1-RSPOS
                  INTO LS_ITAB_WMS_TXT-ZREMARK SEPARATED BY SPACE.

          LS_ITAB_WMS_TXT-ZWH_CODE = IS_MSEG_1-LGORT.

          IF IS_MSEG_1-LGORT+0(2) EQ '21'.
            APPEND LS_ITAB_WMS_TXT TO LT_ITAB_WMS_TXT.
          ENDIF.

        ENDIF.
      ENDLOOP.
*
      IF NOT LT_ITAB_WMS_TXT IS INITIAL.
        LT_FILE = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( I_ITEM    = LT_ITAB_WMS_TXT
                                                          I_FIX_LEN = ABAP_TRUE ).
        IF LCL_GEN_FILE IS NOT BOUND.
          CREATE OBJECT LCL_GEN_FILE.
        ENDIF.

        CONCATENATE 'SVR' LV_MBLNR '_' SY-DATUM SY-UZEIT '.txt'
                     INTO LV_FILENAME.

        LV_STATUS = LCL_GEN_FILE->FTP_FILE_PUT( I_WINDOW_PATH = 'SONY_LOGISTIC_FG/DEV/OUT'
                                                I_AL11_PATH   = '/tmp'
                                                I_FILE_NAME   = LV_FILENAME
                                                I_USER        = 'ds'
                                                I_PASS        = 'ds=20240521'
                                                I_IP          = '172.31.136.249'
                                                I_PORT        = '21'
                                                IT_DATA       = LT_FILE ).
        IF LV_STATUS EQ 'S'.
          " SUCCESS FTP FILE
        ELSE.
          " CANNOT FTP FILE
        ENDIF.
      ENDIF.
*    ENDIF.
  ENDMETHOD.
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
        R = ABAP_TRUE.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        R = ABAP_FALSE.
      ENDIF.
    ELSE.
      R = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
