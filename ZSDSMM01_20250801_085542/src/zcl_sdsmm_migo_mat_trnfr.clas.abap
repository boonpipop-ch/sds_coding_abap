class ZCL_SDSMM_MIGO_MAT_TRNFR definition
  public
  final
  create public .

public section.

  class-methods INIT_VALUE .
  class-methods SET_BKTXT
    importing
      !IF_REF_MBLNR type MKPF-MBLNR
      !IF_REF_MJAHR type MKPF-MJAHR
      !IF_CANCEL_FG type FLAG
    exporting
      value(EF_BKTXT) type MKPF-BKTXT .
  class-methods CHANGE_BKTXT_OF_REFDOC
    importing
      !IF_CURRENT_MKPF type MKPF .
protected section.
PRIVATE SECTION.

  CLASS-DATA GF_REF_MBLNR TYPE MKPF-MBLNR .
  CLASS-DATA GF_REF_MJAHR TYPE MKPF-MJAHR .
  CLASS-DATA GRT_BWART TYPE FIP_T_BWART_RANGE .
  CLASS-DATA GRT_WERKS TYPE RANGE_T_WERKS .
  CLASS-DATA GC_REPID TYPE PROGRAMM VALUE 'ZCL_SDSMM_MIGO_MAT_TRNFR'.
ENDCLASS.



CLASS ZCL_SDSMM_MIGO_MAT_TRNFR IMPLEMENTATION.


  METHOD CHANGE_BKTXT_OF_REFDOC.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_MIGO_MAT_TRNFR
*  Creation Date      : 09.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : MMR004
*  Description        : SDS_D04_FS_ZMMR004
*                       MIGO: Add Material Document to header text for Material Transfer
*  Purpose            : Add Material Document to header text for Material Transfer
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    IF GRT_BWART[] IS INITIAL
    OR GRT_WERKS[] IS INITIAL
    OR GF_REF_MBLNR IS INITIAL
    OR GF_REF_MJAHR IS INITIAL
    OR IF_CURRENT_MKPF-BKTXT <> GF_REF_MBLNR.
      RETURN.
    ENDIF.


    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
    INTO TABLE @DATA(LT_MKPF)
    FROM MKPF
    WHERE MBLNR = @GF_REF_MBLNR
    AND   MJAHR = @GF_REF_MJAHR.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
    INTO TABLE @DATA(LT_MSEG)
    FROM MSEG
    WHERE MBLNR = @GF_REF_MBLNR
    AND   MJAHR = @GF_REF_MJAHR.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    READ TABLE LT_MKPF ASSIGNING FIELD-SYMBOL(<L_MKPF>) INDEX 1. "#EC CI_NOORDER
    IF SY-SUBRC = 0.
      <L_MKPF>-BKTXT = IF_CURRENT_MKPF-MBLNR.
    ENDIF.

    CALL FUNCTION 'MB_CHANGE_DOCUMENT' IN UPDATE TASK
      TABLES
        ZMKPF = LT_MKPF
        ZMSEG = LT_MSEG.


  ENDMETHOD.


  METHOD INIT_VALUE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_MIGO_MAT_TRNFR
*  Creation Date      : 09.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : MMR004
*  Description        : SDS_D04_FS_ZMMR004
*                       MIGO: Add Material Document to header text for Material Transfer
*  Purpose            : Initialize value
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR: GF_REF_MBLNR,
           GF_REF_MJAHR.

  ENDMETHOD.


  METHOD SET_BKTXT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_MIGO_MAT_TRNFR
*  Creation Date      : 09.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : MMR004
*  Description        : SDS_D04_FS_ZMMR004
*                       MIGO: Add Material Document to header text for Material Transfer
*  Purpose            : Add Material Document to header text for Material Transfer
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR: GF_REF_MBLNR,
           GF_REF_MJAHR.

    IF GRT_BWART[] IS INITIAL.
      ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                      IF_PARAM = 'BWART'
                                            IMPORTING ET_RANGE = GRT_BWART ).
    ENDIF.

    IF GRT_WERKS[] IS INITIAL.
      ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                      IF_PARAM = 'WERKS'
                                            IMPORTING ET_RANGE = GRT_WERKS ).
    ENDIF.

    IF GRT_BWART[] IS INITIAL
    OR GRT_WERKS[] IS INITIAL.
      RETURN.
    ENDIF.
    SELECT BWART, WERKS                                 "#EC CI_NOORDER
    INTO @DATA(LS_MSEG)
    UP TO 1 ROWS
    FROM MSEG
    WHERE MBLNR = @IF_REF_MBLNR
    AND   MJAHR = @IF_REF_MJAHR.
    ENDSELECT.


    IF LS_MSEG-BWART IN GRT_BWART
    AND LS_MSEG-WERKS IN GRT_WERKS.
      IF IF_CANCEL_FG = ABAP_FALSE.
        EF_BKTXT = IF_REF_MBLNR .
        GF_REF_MBLNR = IF_REF_MBLNR.
        GF_REF_MJAHR = IF_REF_MJAHR.
      ELSE.
        CLEAR: EF_BKTXT,
               GF_REF_MBLNR,
               GF_REF_MJAHR.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
