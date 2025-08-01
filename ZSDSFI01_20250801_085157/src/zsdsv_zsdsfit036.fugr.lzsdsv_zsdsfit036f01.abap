*----------------------------------------------------------------------*
***INCLUDE LZSDSV_ZSDSFIT036F01.
*----------------------------------------------------------------------*
  INCLUDE LSVIMTDT.
  INCLUDE ZDSCAI0010.

  FORM VALIDATE_AND_ASSIGN ##CALLED.

    " Validate Customer
    IF ZSDSV_ZSDSFIT036-KUNNR IS NOT INITIAL.
      ZCL_SDSFI_CUSTOMER_CREDIT=>VALIDATE_PARTNER(
        EXPORTING
          IF_PARTNER = ZSDSV_ZSDSFIT036-KUNNR
        IMPORTING
          EF_INVALID = DATA(LF_INVALID)
          EF_MSGTX   = DATA(LF_MSGTX)
      ).
      IF LF_INVALID EQ ABAP_TRUE.
        MESSAGE E000(ZSDSCA01) WITH LF_MSGTX.
        RETURN.
      ENDIF.
    ENDIF.

    " Validate WBS (Project)
    IF ZSDSV_ZSDSFIT036-PSPHI IS NOT INITIAL.
      ZCL_SDSFI_CUSTOMER_CREDIT=>VALIDATE_PROJECT(
        EXPORTING
          IF_PSPNR   = ZSDSV_ZSDSFIT036-PSPHI
        IMPORTING
          EF_INVALID = LF_INVALID
          EF_MSGTX   = LF_MSGTX
      ).

      IF LF_INVALID EQ ABAP_TRUE.
        MESSAGE E000(ZSDSCA01) WITH LF_MSGTX.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT KUNNR,
           PSPHI,
           SEQ,
           STARTDATE,                                       "+420000174
           ENDDATE,                                         "+420000174
           ZDEL_FLG                                         "+420000174
      FROM ZSDSFIT036 INTO TABLE @DATA(LT_WBS_CRD)
      WHERE KUNNR = @ZSDSV_ZSDSFIT036-KUNNR
        AND PSPHI = @ZSDSV_ZSDSFIT036-PSPHI.

    MOVE-CORRESPONDING TOTAL[] TO LT_WBS_CRD KEEPING TARGET LINES.

    SORT LT_WBS_CRD BY KUNNR
                       PSPHI
                       SEQ DESCENDING.

*<-- Start of Insertion 420000174 21.01.2025 (Check period overlap)
    IF ZSDSV_ZSDSFIT036-ZDEL_FLG IS INITIAL.
      LOOP AT LT_WBS_CRD TRANSPORTING NO FIELDS
                         WHERE KUNNR = ZSDSV_ZSDSFIT036-KUNNR
                           AND PSPHI = ZSDSV_ZSDSFIT036-PSPHI
                           AND STARTDATE LE ZSDSV_ZSDSFIT036-ENDDATE
                           AND ENDDATE GE ZSDSV_ZSDSFIT036-STARTDATE
                           AND ZDEL_FLG EQ SPACE.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        DATA: LF_TEXT TYPE TEXT50.
*       Error: Period must not overlap within Partner &1 Project Def &2.
        WRITE ZSDSV_ZSDSFIT036-PSPHI TO LF_TEXT.
        MESSAGE E020(ZSDSFI01) WITH ZSDSV_ZSDSFIT036-KUNNR LF_TEXT.
        RETURN.
      ENDIF.
    ENDIF.
*--> End of Insertion 420000174 21.01.2025

    READ TABLE LT_WBS_CRD
    ASSIGNING FIELD-SYMBOL(<L_WBS_CRD>)
    WITH KEY KUNNR = ZSDSV_ZSDSFIT036-KUNNR
             PSPHI = ZSDSV_ZSDSFIT036-PSPHI
    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      ZSDSV_ZSDSFIT036-SEQ = <L_WBS_CRD>-SEQ + 1.
    ELSE.
      ZSDSV_ZSDSFIT036-SEQ = 1.
    ENDIF.

  ENDFORM.
