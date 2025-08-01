class ZCL_SDSPS_BUDGET definition
  public
  final
  create public .

public section.

  methods GET_WBS_LV
    importing
      !I_DATA type PS_POSID
      !I_LV type PS_STUFE
    returning
      value(R_RETURN) type PS_POSID .
  methods GET_ASSET_CLASS
    importing
      !I_DATA type PS_POSID
    returning
      value(R_RETURN) type MEVAK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSPS_BUDGET IMPLEMENTATION.


  METHOD GET_ASSET_CLASS.

    DATA : BEGIN OF LS_TAB_CHECK,
             DATA TYPE CHAR255,
           END OF LS_TAB_CHECK.
    DATA : LT_TAB_CHECK LIKE TABLE OF LS_TAB_CHECK.

    DATA : LV_LINE TYPE I.

    DATA : LV_WBS_LV1 LIKE I_DATA.

    LV_WBS_LV1 = GET_WBS_LV( I_DATA = I_DATA
                             I_LV   = 1 ).

    IF LV_WBS_LV1 IS NOT INITIAL.
      SPLIT LV_WBS_LV1 AT '-' INTO TABLE LT_TAB_CHECK.

      DESCRIBE TABLE LT_TAB_CHECK LINES LV_LINE.

      READ TABLE LT_TAB_CHECK INTO LS_TAB_CHECK INDEX LV_LINE.
      IF SY-SUBRC EQ 0.
        R_RETURN = LS_TAB_CHECK-DATA.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_WBS_LV.
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
        AND STUFE EQ @I_LV
      INTO TABLE @DATA(LT_LV).

    IF LT_LV IS NOT INITIAL.
      SPLIT I_DATA AT '-' INTO TABLE LT_TAB_WBS.

      LOOP AT LT_LV INTO DATA(LS_LV).
        SPLIT LS_LV AT '-' INTO TABLE LT_TAB_CHECK.
        CLEAR : LV_CHECK.
        LOOP AT LT_TAB_CHECK INTO LS_TAB_CHECK.
          READ TABLE LT_TAB_WBS INTO LS_TAB_WBS INDEX SY-TABIX.
          IF LS_TAB_CHECK-DATA NE LS_TAB_WBS-DATA.
            LV_CHECK = ABAP_TRUE.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF LV_CHECK EQ SPACE.
          R_RETURN = LS_LV.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
