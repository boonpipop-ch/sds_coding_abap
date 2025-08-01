FUNCTION Z_SDSCA_GET_ADDRESS3.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(P_DOCNO) TYPE  BELNR_D
*"     REFERENCE(P_GJAHR) TYPE  GJAHR OPTIONAL
*"     REFERENCE(P_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(P_CUSTOMER) TYPE  KUNNR
*"     REFERENCE(P_NATION) TYPE  ADRC-NATION
*"     REFERENCE(P_FLAG) TYPE  C
*"  EXPORTING
*"     REFERENCE(WA_ADRC) TYPE  ADRC
*"     REFERENCE(WA_BSEC) TYPE  BSEC
*"     REFERENCE(V_ATEXT) TYPE  ANREX
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------

  DATA: LV_XCPDD TYPE XCPDD,
        LV_SPRAS TYPE SPRAS.
  DATA LV_ADRNR  TYPE KNA1-ADRNR.

*>>> Start modify ICR 300000091 by CHAOWALITC 24/03/2009
  DATA: BEGIN OF WS_BSAK,
          BUZEI LIKE BSAK-BUZEI,
          XCPDD LIKE BSAK-XCPDD.
  DATA END OF WS_BSAK.
*>>> End modify ICR 300000091 by CHAOWALITC 24/03/2009

  CLEAR: LV_XCPDD,
         LV_SPRAS,
         LV_ADRNR.

*>>> Start modify ICR 300000091 by CHAOWALITC 24/03/2009
*  SELECT SINGLE xcpdd
*  INTO lv_xcpdd
*  FROM bsak
*  WHERE belnr EQ p_docno
*    AND bukrs EQ p_bukrs
*    AND gjahr EQ p_gjahr.

  IF P_FLAG EQ 'V'.

*    --- Find one item has individual set value
    SELECT SINGLE BUZEI XCPDD
    INTO WS_BSAK
    FROM BSAK
    WHERE BELNR EQ P_DOCNO
      AND BUKRS EQ P_BUKRS
      AND GJAHR EQ P_GJAHR
      AND XCPDD NE SPACE
*        AND qbshb ne 0.  "Add by Wantanee 20120525
      AND QSSHB NE 0.  "Add by Wantanee 20120605

  ELSE.
*    --- Find one item has individual set value
    SELECT SINGLE BUZEI XCPDD
    INTO WS_BSAK
    FROM BSAD
    WHERE BELNR EQ P_DOCNO
      AND BUKRS EQ P_BUKRS
      AND GJAHR EQ P_GJAHR
      AND XCPDD NE SPACE.
*        AND qbshb ne 0.  "Add by Wantanee 20120525
*        AND qsshb ne 0.  "Add by Wantanee 20120605
  ENDIF.

*>>> End modify ICR 300000091 by CHAOWALITC 24/03/2009

*>>> Start modify ICR 300000091 by CHAOWALITC 24/03/2009
* IF lv_xcpdd EQ 'X'.
  IF WS_BSAK-XCPDD EQ 'X'.
*>>> End modify ICR 300000091 by CHAOWALITC 24/03/2009.

    PERFORM GET_ADDRESS_ONETIME USING P_DOCNO
                                      P_BUKRS
                                      P_GJAHR
*>>> Start modify ICR 300000091 by CHAOWALITC 24/03/2009
                                      WS_BSAK-BUZEI
*>>> End modify ICR 300000091 by CHAOWALITC 24/03/2009
                                CHANGING WA_BSEC .
    IF WA_BSEC-ANRED NE SPACE.
      IF WA_BSEC-SPRAS EQ 'TH'.
        LV_SPRAS = '2'.
      ELSE.
        LV_SPRAS = 'E'.
      ENDIF.
      SELECT SINGLE ATEXT
      INTO V_ATEXT
      FROM T522T
      WHERE ANRED EQ WA_BSEC-ANRED
        AND SPRSL EQ LV_SPRAS.
    ENDIF.
  ELSE.
*  get address from adrc
    IF P_FLAG = 'C'.

      IF P_CUSTOMER EQ 'OT01'.  "T41K922384 Edit by Wantnaee 20160404
        PERFORM GET_ADDRESS_ONETIME USING P_DOCNO
                                    P_BUKRS
                                    P_GJAHR
                                    WS_BSAK-BUZEI
                              CHANGING WA_BSEC .

      ELSE.

        SELECT SINGLE ADRNR
        FROM KNA1
        INTO LV_ADRNR
        WHERE KUNNR = P_CUSTOMER.
        IF SY-SUBRC = 0.
          PERFORM GET_ADDRESS USING LV_ADRNR
                                    P_NATION
                              CHANGING WA_ADRC.
        ELSE.
          RAISE NO_DATA_FOUND.
        ENDIF.
      ENDIF.
    ELSEIF P_FLAG = 'V'.
      SELECT SINGLE ADRNR
      FROM LFA1
      INTO LV_ADRNR
      WHERE LIFNR = P_CUSTOMER.
      IF SY-SUBRC = 0.
        PERFORM GET_ADDRESS USING LV_ADRNR
                                  P_NATION
                            CHANGING WA_ADRC.
      ELSE.
        RAISE NO_DATA_FOUND.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
