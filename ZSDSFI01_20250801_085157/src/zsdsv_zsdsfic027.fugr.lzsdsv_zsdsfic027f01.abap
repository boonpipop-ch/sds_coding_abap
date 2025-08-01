*&---------------------------------------------------------------------*
*& Include          LZSDSV_ZSDSFIC027F01
*&---------------------------------------------------------------------*
  INCLUDE LSVIMTDT.
  INCLUDE ZDSCAI0010.


*&---------------------------------------------------------------------*
*& Form F_ASSIGN_SEQNO
*&---------------------------------------------------------------------*
  FORM F_ASSIGN_SEQNO .

*    IF ZSDSV_ZSDSFIC027-FR_KUNNR IS NOT INITIAL
*    AND ZSDSV_ZSDSFIC027-TO_KUNNR IS NOT INITIAL
*    AND ZSDSV_ZSDSFIC027-FR_KUNNR GT ZSDSV_ZSDSFIC027-TO_KUNNR.
*      MESSAGE E000(ZSDSCA01) WITH '"From customer" must be less than "To customer"' .
*      RETURN.
*    ENDIF.
    IF ZSDSV_ZSDSFIC027-SEQNO IS NOT INITIAL
    AND ( STATUS-ACTION <> 'A' AND STATUS-ACTION <> 'C' ). "add or copy
      RETURN.
    ENDIF.
    SELECT PIC_PERNR,
           SEQNO
     FROM ZSDSFIC027
     INTO TABLE @DATA(LT_DATA)
     WHERE PIC_PERNR = @ZSDSV_ZSDSFIC027-PIC_PERNR.

    MOVE-CORRESPONDING TOTAL[] TO LT_DATA KEEPING TARGET LINES.

    SORT LT_DATA BY PIC_PERNR
                    SEQNO DESCENDING.

    READ TABLE LT_DATA
    ASSIGNING FIELD-SYMBOL(<L_WBS_CRD>)
    WITH KEY PIC_PERNR = ZSDSV_ZSDSFIC027-PIC_PERNR
                         BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      ZSDSV_ZSDSFIC027-SEQNO = <L_WBS_CRD>-SEQNO + 1.
    ELSE.
      ZSDSV_ZSDSFIC027-SEQNO = 1.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DATA
*&---------------------------------------------------------------------*
  FORM F_VALIDATE_DATA .

    DATA: LS_CHECK TYPE ZSDSV_ZSDSFIC027,
          LT_CHECK TYPE TABLE OF ZSDSV_ZSDSFIC027,
          LF_FOUND TYPE FLAG.

    PERFORM F_ASSIGN_SEQNO.
    MOVE-CORRESPONDING TOTAL[] TO LT_CHECK[].
*=====================customer ===============
    PERFORM F_VALIDATE_FROM_TO_FIELD USING ZSDSV_ZSDSFIC027-FR_KUNNR
                                           ZSDSV_ZSDSFIC027-TO_KUNNR
                                           'Customer'.

*=====================company code ===============
    IF ZSDSV_ZSDSFIC027-BUKRS IS INITIAL.
      MESSAGE E019(ZSDSFI01) WITH 'Company code'.
    ENDIF.



*=================SD based ==============
    IF ZSDSV_ZSDSFIC027-VKORG IS NOT INITIAL.
      "---VTWEG
      PERFORM F_VALIDATE_FROM_TO_FIELD USING ZSDSV_ZSDSFIC027-FR_VTWEG
                                             ZSDSV_ZSDSFIC027-TO_VTWEG
                                             'Distr Channels'.
      "---SPART
      PERFORM F_VALIDATE_FROM_TO_FIELD USING ZSDSV_ZSDSFIC027-FR_SPART
                                             ZSDSV_ZSDSFIC027-TO_SPART
                                             'Division'.
      "---VKBUR -> no check since KNVV canbe blank value
*      IF ZSDSV_ZSDSFIC027-VKBUR IS INITIAL.
*        MESSAGE E019(ZSDSFI01) WITH 'Sales office'.
*      ENDIF.

      "---VKGRP
      PERFORM F_VALIDATE_FROM_TO_CAN_BLANK USING ZSDSV_ZSDSFIC027-FR_VKGRP
                                                       ZSDSV_ZSDSFIC027-TO_VKGRP
                                                       'Sales Group'.
    ELSE.
      CLEAR: ZSDSV_ZSDSFIC027-FR_VTWEG,
             ZSDSV_ZSDSFIC027-TO_VTWEG,
             ZSDSV_ZSDSFIC027-FR_SPART,
             ZSDSV_ZSDSFIC027-TO_SPART,
             ZSDSV_ZSDSFIC027-FR_VKGRP,
             ZSDSV_ZSDSFIC027-TO_VKGRP.
    ENDIF.


*=================BUSAB
    PERFORM F_VALIDATE_FROM_TO_FIELD USING ZSDSV_ZSDSFIC027-FR_BUSAB
                                           ZSDSV_ZSDSFIC027-TO_BUSAB
                                           'Accounting Clerks'.

*=================Overlap ==============
    LOOP AT LT_CHECK INTO LS_CHECK  WHERE ( PIC_PERNR <> ZSDSV_ZSDSFIC027-PIC_PERNR OR SEQNO <> ZSDSV_ZSDSFIC027-SEQNO ) "NOT CURRENT LINE
                                            AND FR_KUNNR <= ZSDSV_ZSDSFIC027-TO_KUNNR AND TO_KUNNR >= ZSDSV_ZSDSFIC027-FR_KUNNR
                                            AND VKORG = ZSDSV_ZSDSFIC027-VKORG
                                            AND BUKRS = ZSDSV_ZSDSFIC027-BUKRS
                                            AND FR_VTWEG <= ZSDSV_ZSDSFIC027-TO_VTWEG AND TO_VTWEG >= ZSDSV_ZSDSFIC027-FR_VTWEG
                                            AND FR_SPART <= ZSDSV_ZSDSFIC027-TO_SPART AND TO_SPART >= ZSDSV_ZSDSFIC027-FR_SPART
                                            AND VKBUR = ZSDSV_ZSDSFIC027-VKBUR
                                            AND FR_VKGRP <= ZSDSV_ZSDSFIC027-TO_VKGRP AND TO_VKGRP >= ZSDSV_ZSDSFIC027-FR_VKGRP
                                            AND FR_BUSAB <= ZSDSV_ZSDSFIC027-TO_BUSAB AND TO_BUSAB >= ZSDSV_ZSDSFIC027-FR_BUSAB.
      LF_FOUND = ABAP_TRUE.
      EXIT.
    ENDLOOP.
    IF LF_FOUND = ABAP_TRUE.
      VIM_ABORT_SAVING = 'X'.
      "PIC &1, Seqno &2 : Overlap customer with PIC &3 Seqno &4
      MESSAGE E015(ZSDSFI01) WITH ZSDSV_ZSDSFIC027-PIC_PERNR ZSDSV_ZSDSFIC027-SEQNO LS_CHECK-PIC_PERNR LS_CHECK-SEQNO.
      RETURN.
    ENDIF.


    "call as standard calling
    PERFORM (COMPL_FORMNAME) IN PROGRAM (SY-REPID) USING <TABLE1>
                                IF FOUND.
  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_from_to_field
*&---------------------------------------------------------------------*
  FORM F_VALIDATE_FROM_TO_FIELD  USING    US_FROM_VALUE TYPE ANY
                                          US_TO_VALUE TYPE ANY
                                          US_FIELDNAME TYPE ANY.

    IF US_FROM_VALUE IS INITIAL
    AND US_TO_VALUE IS INITIAL.
      VIM_ABORT_SAVING = 'X'.
      "From <us_fieldname> / To <us_fieldname> must be inputted
      MESSAGE E016(ZSDSFI01) WITH US_FIELDNAME.
    ENDIF.

    IF US_FROM_VALUE IS NOT INITIAL
    AND US_TO_VALUE IS NOT INITIAL
    AND US_FROM_VALUE GT US_TO_VALUE.
      VIM_ABORT_SAVING = 'X'.
      "From <us_fieldname> must be less than <To us_fieldname>
      MESSAGE E014(ZSDSFI01) WITH US_FIELDNAME.
      RETURN.
    ENDIF.

    IF US_FROM_VALUE IS INITIAL.
      US_FROM_VALUE = US_TO_VALUE.
    ENDIF.

    IF US_TO_VALUE IS INITIAL.
      US_TO_VALUE = US_FROM_VALUE.
    ENDIF.
  ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_FROM_TO_CAN_BLANK
*&---------------------------------------------------------------------*
*  for field that can be blank value
*&---------------------------------------------------------------------*
  FORM F_VALIDATE_FROM_TO_CAN_BLANK  USING    US_FROM_VALUE TYPE ANY
                                              US_TO_VALUE TYPE ANY
                                              US_FIELDNAME TYPE ANY.


    IF US_FROM_VALUE IS NOT INITIAL
    AND US_TO_VALUE IS NOT INITIAL
    AND US_FROM_VALUE GT US_TO_VALUE.
      VIM_ABORT_SAVING = 'X'.
      "From <us_fieldname> must be less than <To us_fieldname>
      MESSAGE E014(ZSDSFI01) WITH US_FIELDNAME.
      RETURN.
    ENDIF.


    IF US_TO_VALUE IS INITIAL.
      US_TO_VALUE = US_FROM_VALUE.
    ENDIF.
  ENDFORM.
