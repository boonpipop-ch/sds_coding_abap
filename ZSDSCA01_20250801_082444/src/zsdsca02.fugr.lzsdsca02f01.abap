*----------------------------------------------------------------------*
***INCLUDE LZSDSCA02F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ADRNR  text
*      -->P_P_NATION  text
*      <--P_WA_ADRC  text
*----------------------------------------------------------------------*
*  Modification Log
*    1) Changed on    :  25/05/2012 (DD/MM/YYYY)
*       Changed by    :  Wantanee Prateep na thalang
*       Change-No     :
*       Transport-No  : T41K913383,T41K913385
*       Description   : Edit case ONE-TIME show addr. wrong
*&
*&
*&---------------------------------------------------------------------*
FORM GET_ADDRESS  USING    P_LV_ADRNR
                           P_NATION
                  CHANGING WA_ADRC.

  SELECT SINGLE *
    FROM ADRC
    INTO WA_ADRC
    WHERE ADDRNUMBER = P_LV_ADRNR
      AND NATION = P_NATION.

  IF SY-SUBRC NE 0.
    SELECT SINGLE *
      FROM ADRC
      INTO WA_ADRC
      WHERE ADDRNUMBER = P_LV_ADRNR.
    IF SY-SUBRC NE 0.
      RAISE NO_DATA_FOUND.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  GET_ADDRESS_ONETIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DOCNO  text
*      <--P_WA_BSEC  text
*----------------------------------------------------------------------*
FORM GET_ADDRESS_ONETIME  USING    P_DOCNO
                                   P_BUKRS
                                   P_GJAHR
                                   P_BUZEI
                          CHANGING WA_BSEC.



  SELECT SINGLE *
  INTO WA_BSEC
  FROM BSEC
  WHERE BELNR EQ P_DOCNO
    AND BUKRS EQ P_BUKRS
    AND GJAHR EQ P_GJAHR
*>>> Start modify ICR    by CHAOWALITC 24/03/2009
    AND BUZEI EQ P_BUZEI
    AND J_1KFTBUS NE SPACE.  "Add by Wantanee 20120525

    IF SY-SUBRC NE 0.
         SELECT SINGLE *
          INTO WA_BSEC
          FROM BSEC
          WHERE BELNR EQ P_DOCNO
            AND BUKRS EQ P_BUKRS
            AND GJAHR EQ P_GJAHR
*        >>> Start modify ICR    by CHAOWALITC 24/03/2009
*            AND buzei EQ p_buzei
            AND J_1KFTBUS NE SPACE.  "Add by Wantanee 20120525

    ENDIF.

*>>> End modify ICR    by CHAOWALITC 24/03/2009

ENDFORM.                    " GET_ADDRESS_ONETIME
