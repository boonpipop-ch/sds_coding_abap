*----------------------------------------------------------------------*
***INCLUDE LZSDSV_ZSDSFIC029F01.
*----------------------------------------------------------------------*
  INCLUDE LSVIMTDT.
  INCLUDE ZDSCAI0010.


*&---------------------------------------------------------------------*
*& Form F_ASSIGN_SEQNO
*&---------------------------------------------------------------------*
  FORM F_ASSIGN_SEQNO .
    IF FUNCTION  <>   'KOPF'
    AND ZSDSV_ZSDSFIC029-SEQNO IS NOT INITIAL.
      RETURN.
    ENDIF.
    SELECT APPROVER,
           APPV_TYPE,
           SEQNO
     FROM ZSDSFIC029
     INTO TABLE @DATA(LT_DATA)
     WHERE APPROVER = @ZSDSV_ZSDSFIC029-APPROVER
     AND   APPV_TYPE = @ZSDSV_ZSDSFIC029-APPV_TYPE.

    MOVE-CORRESPONDING TOTAL[] TO LT_DATA KEEPING TARGET LINES.

    SORT LT_DATA BY APPROVER APPV_TYPE
                    SEQNO DESCENDING.

    READ TABLE LT_DATA
    ASSIGNING FIELD-SYMBOL(<L_WBS_CRD>)
    WITH KEY APPROVER = ZSDSV_ZSDSFIC029-APPROVER
             APPV_TYPE = ZSDSV_ZSDSFIC029-APPV_TYPE BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      ZSDSV_ZSDSFIC029-SEQNO = <L_WBS_CRD>-SEQNO + 1.
    ELSE.
      ZSDSV_ZSDSFIC029-SEQNO = 1.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_DATA
*&---------------------------------------------------------------------*
  FORM F_VALIDATE_DATA .

    DATA: LS_CHECK TYPE ZSDSV_ZSDSFIC029,
          LT_CHECK TYPE TABLE OF ZSDSV_ZSDSFIC029,
          LF_FOUND TYPE FLAG.
    IF ZSDSV_ZSDSFIC029-APPV_TYPE IS INITIAL.
      "Approve Type must be inputted
      VIM_ABORT_SAVING = 'X'.
      MESSAGE E017(ZSDSFI01) WITH 'Approve Type'.
    ENDIF.
    PERFORM F_ASSIGN_SEQNO .
    MOVE-CORRESPONDING TOTAL[] TO LT_CHECK[].
    CASE ZSDSV_ZSDSFIC029-APPV_TYPE .
*     ------------- over credit
      WHEN '1'. "Over Credit
        IF ZSDSV_ZSDSFIC029-AUART IS INITIAL.
          "Sale Document Type must be inputted
          VIM_ABORT_SAVING = 'X'.
          MESSAGE E017(ZSDSFI01) WITH 'Sales Document Type'.
        ENDIF.
        CLEAR: ZSDSV_ZSDSFIC029-VKORG,
               ZSDSV_ZSDSFIC029-VTWEG,
               ZSDSV_ZSDSFIC029-SPART.
        LOOP AT LT_CHECK INTO LS_CHECK  WHERE ( APPROVER <> ZSDSV_ZSDSFIC029-APPROVER
                                                OR SEQNO <>  ZSDSV_ZSDSFIC029-SEQNO ) "NOT CURRENT LINE
                                                AND APPV_TYPE = 1
                                                AND AUART = ZSDSV_ZSDSFIC029-AUART.
          LF_FOUND = ABAP_TRUE.
          EXIT.
        ENDLOOP.
        IF LF_FOUND = ABAP_TRUE.
          VIM_ABORT_SAVING = 'X'.
          "Duplicate data with Approver &1, Approver type &2, Seqno &3
          MESSAGE E018(ZSDSFI01) WITH LS_CHECK-APPROVER LS_CHECK-APPV_TYPE LS_CHECK-SEQNO.
          RETURN.
        ENDIF.
*     ------------- overdue
      WHEN '2'.

        IF ZSDSV_ZSDSFIC029-AUART IS INITIAL.
          "Sale Document Type must be inputted
          VIM_ABORT_SAVING = 'X'.
          MESSAGE E017(ZSDSFI01) WITH 'Sales Document Type'.
        ENDIF.
        IF ZSDSV_ZSDSFIC029-VKORG IS INITIAL.
          "Sales Org must be inputted
          VIM_ABORT_SAVING = 'X'.
          MESSAGE E017(ZSDSFI01) WITH 'Sales Org'.
        ENDIF.
        IF ZSDSV_ZSDSFIC029-VTWEG IS INITIAL.
          "Distribution Channel must be inputted
          VIM_ABORT_SAVING = 'X'.
          MESSAGE E017(ZSDSFI01) WITH 'Distribution Channel'.
        ENDIF.
        IF ZSDSV_ZSDSFIC029-SPART IS INITIAL.
          "Division must be inputted
          VIM_ABORT_SAVING = 'X'.
          MESSAGE E017(ZSDSFI01) WITH 'Division'.
        ENDIF.
        LOOP AT LT_CHECK INTO LS_CHECK  WHERE ( APPROVER <> ZSDSV_ZSDSFIC029-APPROVER
                                                OR SEQNO <>  ZSDSV_ZSDSFIC029-SEQNO ) "NOT CURRENT LINE
                                                AND APPV_TYPE = 2
                                                AND AUART = ZSDSV_ZSDSFIC029-AUART
                                                AND VKORG = ZSDSV_ZSDSFIC029-VKORG
                                                AND VTWEG = ZSDSV_ZSDSFIC029-VTWEG
                                                AND SPART = ZSDSV_ZSDSFIC029-SPART.
          LF_FOUND = ABAP_TRUE.
          EXIT.
        ENDLOOP.
        IF LF_FOUND = ABAP_TRUE.
          VIM_ABORT_SAVING = 'X'.
          "Duplicate data with Approver &1, Approver type &2, Seqno &3
          MESSAGE E018(ZSDSFI01) WITH LS_CHECK-APPROVER LS_CHECK-APPV_TYPE LS_CHECK-SEQNO.
          RETURN.
        ENDIF.
    ENDCASE.


    "call as standard calling
    PERFORM (COMPL_FORMNAME) IN PROGRAM (SY-REPID) USING <TABLE1>
                                IF FOUND.
  ENDFORM.
