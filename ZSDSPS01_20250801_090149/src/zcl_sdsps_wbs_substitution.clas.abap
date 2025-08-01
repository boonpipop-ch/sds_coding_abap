CLASS ZCL_SDSPS_WBS_SUBSTITUTION DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES TT_BUKRS_RANGE TYPE RANGE OF VBRK-BUKRS.
    TYPES:
      BEGIN OF TS_SRVGL_MAP,
        HKONT TYPE BSEG-HKONT,
        ITMTY TYPE ZSDSPSC002-ITMTY,
        ACTTY TYPE ZSDSPSC002-ACTTY,
      END OF TS_SRVGL_MAP .
    TYPES:
      TT_SRVGL_MAP TYPE SORTED TABLE OF TS_SRVGL_MAP
                                 WITH UNIQUE KEY HKONT .
    TYPES:
      BEGIN OF TS_WRTGL_MAP,
        HKONT TYPE BSEG-HKONT,
        ITMTY TYPE ZSDSPSC002-ITMTY,
        ACTTY TYPE ZSDSPSC002-ACTTY,
      END OF TS_WRTGL_MAP .
    TYPES:
      TT_WRTGL_MAP TYPE SORTED TABLE OF TS_WRTGL_MAP
                                 WITH UNIQUE KEY HKONT .

    CLASS-METHODS SUBSTITUTE_WBS
      IMPORTING
        !IS_COBL  TYPE COBL
      CHANGING
        !CF_PSPNR TYPE PRPS-PSPNR .
    CLASS-METHODS GET_PRODH_MAP
      IMPORTING
        !IF_PRODH  TYPE COBL-ZZ1_PRODH
      EXPORTING
        !EF_SUFFIX TYPE ZSDSPSC004-SUFFIX .
    CLASS-METHODS IS_SDS
      IMPORTING
        !IF_BUKRS        TYPE VBRK-BUKRS
      RETURNING
        VALUE(RF_RESULT) TYPE ABAP_BOOLEAN .
protected section.
private section.

  class-data GT_SRVGL_MAP type TT_SRVGL_MAP .
  class-data GT_WRTGL_MAP type TT_WRTGL_MAP .
  class-data GR_ACTIVE_BUKRS type TT_BUKRS_RANGE .

  class-methods GET_CONSTANTS .
ENDCLASS.



CLASS ZCL_SDSPS_WBS_SUBSTITUTION IMPLEMENTATION.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_WBS_SUBSTITUTION / GET_CONSTANTS
*  Creation Date      : 26.07.2024
*  Author             : Wuthichai L. (Eviden)
*  Add-on ID          : PSE004
*  Description        : To get GENC constant from table ZSDSCAC001
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CONSTANTS:
    LC_ACTIVE_BUKRS TYPE ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_BUKRS',
    LC_SRVGL_MAP    TYPE ZSDSDE_PARAM_NAME VALUE 'SERVICE_GL_MAP',
    LC_WRTGL_MAP    TYPE ZSDSDE_PARAM_NAME VALUE 'WARRANTY_GL_MAP'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSPS_WBS_SUBSTITUTION'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_ACTIVE_BUKRS,
         GT_SRVGL_MAP,
         GT_WRTGL_MAP.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Active Company code
*     ------------------------------------
      WHEN LC_ACTIVE_BUKRS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_ACTIVE_BUKRS.

*     ------------------------------------
*     Service GL Mapping
*     ------------------------------------
      WHEN LC_SRVGL_MAP.
        INSERT VALUE #( HKONT = <L_GENC>-PARAM_EXT
                        ITMTY = <L_GENC>-VALUE_LOW
                        ACTTY = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_SRVGL_MAP.

*     ------------------------------------
*     Warranty GL Mapping
*     ------------------------------------
      WHEN LC_WRTGL_MAP.
        INSERT VALUE #( HKONT = <L_GENC>-PARAM_EXT
                        ITMTY = <L_GENC>-VALUE_LOW
                        ACTTY = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_WRTGL_MAP.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD GET_PRODH_MAP.

  DATA:
    LS_PRODHS  TYPE  PRODHS.

  DATA:
    LF_PRODH  TYPE  ZSDSPSC004-PRODH.


* Initialize Output
  CLEAR: EF_SUFFIX.

  LS_PRODHS = IF_PRODH.

* Check up to 3 Levels
  WHILE LS_PRODHS IS NOT INITIAL.

    CLEAR LF_PRODH.

    LF_PRODH = LS_PRODHS.

*   Read Suffix Mapping
    SELECT SINGLE SUFFIX
      FROM ZSDSPSC004
     WHERE PRODH EQ @LF_PRODH
      INTO @EF_SUFFIX.
    IF SY-SUBRC EQ 0.
      EXIT.
    ENDIF.

    IF LS_PRODHS-PRODH3 IS NOT INITIAL.
      CLEAR LS_PRODHS-PRODH3.
    ELSEIF LS_PRODHS-PRODH2 IS NOT INITIAL.
      CLEAR LS_PRODHS-PRODH2.
    ELSE.
      CLEAR LS_PRODHS-PRODH1.
    ENDIF.

  ENDWHILE.

ENDMETHOD.


METHOD IS_SDS.

* Initialize Output
  CLEAR: RF_RESULT.

* Get Constants setting
  GET_CONSTANTS( ).

  IF GR_ACTIVE_BUKRS IS NOT INITIAL AND
     IF_BUKRS IN  GR_ACTIVE_BUKRS.
    RF_RESULT = 'X'.
  ENDIF.

ENDMETHOD.


METHOD SUBSTITUTE_WBS.
*-----------------------------------------------------------------------
*  Program ID         : SUBSTITUTE_WBS
*  Creation Date      : 31.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : WBS Substitution for Service and Warranty
*  Purpose            : To substitute WBS in posting FI document
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
    LF_TEMP   TYPE  PRPS-POSID,
    LF_SUFFIX TYPE  ZSDSPSC004-SUFFIX,
    LF_PSPNR  TYPE  PRPS-PSPNR.


* Get Constants
  GET_CONSTANTS( ).

* Get WBS Detail
  SELECT SINGLE POSID,
                POST1,
                PSPHI
    FROM PRPS
   WHERE PSPNR EQ @CF_PSPNR
    INTO @DATA(LS_PRPS).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* ----------------------
* For GL of Services
* - Substitute based on Item/Act type mapped and Product hierachy
* ----------------------
  READ TABLE GT_SRVGL_MAP ASSIGNING FIELD-SYMBOL(<L_SRVGL_MAP>)
                          WITH KEY HKONT = IS_COBL-HKONT
                          BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GET_PRODH_MAP(
      EXPORTING
        IF_PRODH  = IS_COBL-ZZ1_PRODH
      IMPORTING
        EF_SUFFIX = LF_SUFFIX ).
    IF LF_SUFFIX IS INITIAL.
      RETURN.
    ENDIF.

*   Format final WBS
    WRITE LS_PRPS-PSPHI TO LF_TEMP.
    CONCATENATE LF_TEMP <L_SRVGL_MAP>-ITMTY <L_SRVGL_MAP>-ACTTY LF_SUFFIX
           INTO LF_TEMP
      SEPARATED BY '-'.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
      EXPORTING
        INPUT     = LF_TEMP
      IMPORTING
        OUTPUT    = LF_PSPNR
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.
    IF SY-SUBRC EQ 0.
      CF_PSPNR = LF_PSPNR.
    ENDIF.

*   End Processing
    RETURN.
  ENDIF.

* ----------------------
* For GL of Warranty
* - Substitute based on Item/Act type mapped with same WBS desc
* ----------------------
  READ TABLE GT_WRTGL_MAP ASSIGNING FIELD-SYMBOL(<L_WRTGL_MAP>)
                          WITH KEY HKONT = IS_COBL-HKONT
                          BINARY SEARCH.
  IF SY-SUBRC EQ 0.
*   Assign WBS Prefix for searching
    WRITE LS_PRPS-PSPHI TO LF_TEMP.
    CONCATENATE LF_TEMP <L_WRTGL_MAP>-ITMTY <L_WRTGL_MAP>-ACTTY '%'
           INTO LF_TEMP
      SEPARATED BY '-'.

*   Find Substitute WBS
    SELECT PSPNR,
           POSID,
           POST1
      FROM PRPS
     WHERE PSPHI  EQ  @LS_PRPS-PSPHI
       AND POST1  EQ  @LS_PRPS-POST1
       AND POSID LIKE @LF_TEMP
     ORDER BY PRIMARY KEY
      INTO @DATA(LS_PRPS_SUB)
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

*   Assign Result
    CF_PSPNR = LS_PRPS_SUB-PSPNR.

*   End Processing
    RETURN.
  ENDIF.

ENDMETHOD.
ENDCLASS.
