*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    CONSTANTS : BEGIN OF LC_CON,
                  XVBEP TYPE C LENGTH 17 VALUE '(SAPMV45A)XVBEP[]',
                END OF LC_CON.
    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      UPDATE_CONFIRM_QTY IMPORTING I_DATA TYPE TAB_XYVBAP
                                   I_HEAD TYPE VBAK.


ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD UPDATE_CONFIRM_QTY.
    FIELD-SYMBOLS : <LFT_XVBEP> TYPE TT_VBEPVB,
                    <LFS_XVBEP> TYPE VBEPVB.

    DATA : LT_VBEP TYPE TT_VBEPVB.

    DATA : LT_DATA_CHECK LIKE I_DATA.

    ASSIGN (LC_CON-XVBEP) TO <LFT_XVBEP>.

    DATA: LV_JOBNAME  TYPE TBTCO-JOBNAME,
          LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

    DATA : LS_SELTAB    TYPE TABLE OF RSPARAMS.

    DATA: LV_DATA TYPE SY-DATUM,
          LV_TIME TYPE SY-UZEIT.

    DATA : S_VBELN TYPE RANGE OF VBAP-VBELN.
    DATA : S_POSNR TYPE RANGE OF VBAP-POSNR.

    DATA : S_POSNR_TMP TYPE RANGE OF VBAP-POSNR.

    DATA : LCL_SALES TYPE REF TO ZCL_SDSSD_SALES_DOCUMENT.

    DATA : LT_SO TYPE ZSDSSDS127_TT,
           LS_SO TYPE ZSDSSDS127.

    DATA :BEGIN OF LS_MAT,
            MATNR TYPE VBAP-MATNR,
          END OF LS_MAT.
    "DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATNR.
    DATA : LT_MAT LIKE TABLE OF LS_MAT WITH EMPTY KEY.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.

    IF I_HEAD-VBTYP NE 'C'.
      RETURN.
    ENDIF.

    IF <LFT_XVBEP> IS ASSIGNED.
      LT_VBEP = <LFT_XVBEP>.
      DELETE LT_VBEP WHERE BMENG NE 0.
    ENDIF.

    IF LT_VBEP IS NOT INITIAL.
*      LT_MAT =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).
      CLEAR : LT_MAT.
      LOOP AT I_DATA INTO DATA(LS_DATA_TMP) WHERE GBSTA NE 'C' AND
                                                  KOWRR NE ABAP_TRUE. " no BOM.
        READ TABLE LT_VBEP
        WITH KEY VBELN = LS_DATA_TMP-VBELN
                 POSNR = LS_DATA_TMP-POSNR TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
          CONTINUE.
        ENDIF.

        LS_MAT-MATNR = LS_DATA_TMP-MATNR.
        APPEND LS_MAT TO LT_MAT.
        APPEND LS_DATA_TMP TO LT_DATA_CHECK.
      ENDLOOP.
      IF LCL_SALES IS NOT BOUND.
        CREATE OBJECT LCL_SALES.
      ENDIF.

      SORT LT_MAT BY MATNR.
      DELETE ADJACENT DUPLICATES FROM LT_MAT COMPARING MATNR.

      SELECT ZSDSSDC030~MATNR
        FROM @LT_MAT AS A
        INNER JOIN ZSDSSDC030 ON A~MATNR EQ ZSDSSDC030~MATNR
        WHERE DAY_FIRST LE @SY-DATUM
          AND DAY_LAST  GE @SY-DATUM
          AND CONTF     EQ @ABAP_TRUE
          AND FLAGD     EQ @ABAP_FALSE
        INTO TABLE @DATA(LT_CONTROL).

      LOOP AT LT_DATA_CHECK INTO DATA(LS_DATA) WHERE VBELN EQ I_HEAD-VBELN.
        READ TABLE LT_CONTROL
        WITH KEY MATNR = LS_DATA-MATNR TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.
          S_POSNR_TMP =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = LS_DATA-POSNR ) ).
          APPEND LINES OF S_POSNR_TMP TO S_POSNR.
        ENDIF.
      ENDLOOP.

      IF S_POSNR[] IS NOT INITIAL.
        S_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_HEAD-VBELN ) ).

        SORT S_POSNR BY LOW.
        DELETE ADJACENT DUPLICATES FROM S_POSNR COMPARING LOW.

        CONCATENATE 'CANCEL_CONF_QTY' '_' SY-DATUM '_' SY-UZEIT INTO LV_JOBNAME.

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
            MESSAGE E208(00) WITH 'Error'.
        ENDCASE.

        CALL FUNCTION 'GET_PRINT_PARAMETERS'
          EXPORTING
*           immediately            = 'X'
            NO_DIALOG              = 'X'
            USER                   = SY-UNAME
          IMPORTING
            OUT_PARAMETERS         = LS_PRINT_PARAMETERS
          EXCEPTIONS
            ARCHIVE_INFO_NOT_FOUND = 1
            INVALID_PRINT_PARAMS   = 2
            INVALID_ARCHIVE_PARAMS = 3
            OTHERS                 = 4.

        SUBMIT ZSDSSDR0580 TO SAP-SPOOL AND RETURN
                                       WITH SELECTION-TABLE LS_SELTAB
                                                       USER SY-UNAME
                                           SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                       WITHOUT SPOOL DYNPRO
                                                    VIA JOB LV_JOBNAME
                                                     NUMBER LV_JOBCOUNT
                               USING SELECTION-SCREEN  1000
                                WITH S_VBELN IN S_VBELN[]
                                WITH S_POSNR IN S_POSNR[]
                                WITH P_AUTO  EQ ABAP_TRUE
                                WITH P_ZERO  EQ abap_TRUE.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            JOBCOUNT             = LV_JOBCOUNT
            JOBNAME              = LV_JOBNAME
            SDLSTRTDT            = LV_DATA
            SDLSTRTTM            = LV_TIME
*           STRTIMMED            = ABAP_TRUE
          EXCEPTIONS
            CANT_START_IMMEDIATE = 1
            INVALID_STARTDATE    = 2
            JOBNAME_MISSING      = 3
            JOB_CLOSE_FAILED     = 4
            JOB_NOSTEPS          = 5
            JOB_NOTEX            = 6
            LOCK_FAILED          = 7
            OTHERS               = 8.


*      DATA(RT_RETURN) = LCL_SALES->CANCEL_CONFIRM_QTY( LT_SO ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
