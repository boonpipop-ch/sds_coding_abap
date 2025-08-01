*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      UPDATE_CONFIRM_QTY IMPORTING I_DATA TYPE ZSDSSDS015_TT
                                   I_HEAD TYPE ZSDSSDS048,
      CHECK_CO_CONFIRM_QTY IMPORTING I_DATA   TYPE CHAR20
                           RETURNING VALUE(R) TYPE CHAR50.

ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD UPDATE_CONFIRM_QTY.

    DATA : LT_DATA_CHECK LIKE I_DATA.


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
            MATERIALNUMBER TYPE VBAP-MATNR,
          END OF LS_MAT.
    DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATERIALNUMBER.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.

    LT_MAT =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

    IF LCL_SALES IS NOT BOUND.
      CREATE OBJECT LCL_SALES.
    ENDIF.

    SELECT ZSDSSDC030~MATNR
      FROM @LT_MAT AS A
      INNER JOIN ZSDSSDC030 ON A~MATERIALNUMBER EQ ZSDSSDC030~MATNR
      WHERE DAY_FIRST LE @SY-DATUM
        AND DAY_LAST  GE @SY-DATUM
        AND CONTF     EQ @ABAP_TRUE
        AND FLAGD     EQ @ABAP_FALSE
      INTO TABLE @DATA(LT_CONTROL).

    LOOP AT I_DATA INTO DATA(LS_DATA).
      READ TABLE LT_CONTROL
      WITH KEY MATNR = LS_DATA-MATERIALNUMBER TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.
        S_POSNR_TMP =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = LS_DATA-SAPITEMNUMBER ) ).
        APPEND LINES OF S_POSNR_TMP TO S_POSNR.
      ENDIF.
    ENDLOOP.

    IF S_POSNR[] IS NOT INITIAL.
      S_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_HEAD-SAPSALESORDERNO ) ).

      SORT S_POSNR BY LOW.
      DELETE ADJACENT DUPLICATES FROM S_POSNR COMPARING LOW.

      CONCATENATE 'CANCEL_CONF_QTY' '_' I_HEAD-SAPSALESORDERNO '_' SY-UZEIT INTO LV_JOBNAME.

*      LV_DATA = SY-DATUM.
*      LV_TIME = SY-UZEIT + 120.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = LV_JOBNAME
*         SDLSTRTDT        = LV_DATA
*         SDLSTRTTM        = LV_TIME
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
*         immediately            = 'X'
          NO_DIALOG              = 'X'
          USER                   = SY-UNAME
        IMPORTING
          OUT_PARAMETERS         = LS_PRINT_PARAMETERS
        EXCEPTIONS
          ARCHIVE_INFO_NOT_FOUND = 1
          INVALID_PRINT_PARAMS   = 2
          INVALID_ARCHIVE_PARAMS = 3
          OTHERS                 = 4.

      IF LS_PRINT_PARAMETERS-PDEST IS INITIAL.
        LS_PRINT_PARAMETERS-PDEST = 'SDLC'.
      ENDIF.

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
*         SDLSTRTDT            = LV_DATA
*         SDLSTRTTM            = LV_TIME
          STRTIMMED            = ABAP_TRUE
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
  ENDMETHOD.
  METHOD CHECK_CO_CONFIRM_QTY.
    SELECT COUNT( * )
      FROM VBEP AS A
      WHERE A~WMENG NE A~BMENG OR
            A~BMENG EQ 0.
    IF SY-SUBRC EQ 0.
      R = TEXT-E65.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
