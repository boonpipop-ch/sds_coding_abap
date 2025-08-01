class ZCL_SDSFI_REPORT definition
  public
  final
  create public .

public section.

  methods GP_REPORT
    importing
      value(IR_RLDNR) type ANY TABLE optional
      value(IR_RBUKRS) type ANY TABLE optional
      value(IR_GJAHR) type ANY TABLE optional
      value(IR_BELNR) type ANY TABLE optional
      value(IR_BUDAT) type ANY TABLE optional
      value(IR_POSID) type ANY TABLE optional
      value(IR_PRCTR) type ANY TABLE optional
      value(IR_AUFNR) type ANY TABLE optional
      value(IR_MATNR) type ANY TABLE optional
      value(IR_FKART) type ANY TABLE optional
      value(IR_VKORG) type ANY TABLE optional
      value(IR_VTWEG) type ANY TABLE optional
      value(IR_VKGRP) type ANY TABLE optional
      value(IR_VKBUR) type ANY TABLE optional
      value(IR_PRODH) type ANY TABLE optional
      value(IR_RACCT) type ANY TABLE optional
    returning
      value(R_RESULT) type ZSDSFIS183_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSFI_REPORT IMPLEMENTATION.


  METHOD GP_REPORT.

    SELECT *
      FROM ZSDSVC_GET_SALES_REPORT
      WHERE RLDNR    IN @IR_RLDNR[]
        AND RBUKRS   IN @IR_RBUKRS[]
        AND GJAHR    IN @IR_GJAHR[]
        AND BELNR    IN @IR_BELNR[]
        AND BUDAT    IN @IR_BUDAT[]
        AND PS_POSID IN @IR_POSID[]
        AND PRCTR    IN @IR_PRCTR[]
        AND AUFNR    IN @IR_AUFNR[]
        AND MATNR    IN @IR_MATNR[]
        AND FKART    IN @IR_FKART[]
        AND VKORG    IN @IR_VKORG[]
        AND VTWEG    IN @IR_VTWEG[]
        AND VKGRP    IN @IR_VKGRP[]
        AND VKBUR    IN @IR_VKBUR[]
        AND PRODH    IN @IR_PRODH[]
        AND RACCT    IN @IR_RACCT[]
      INTO CORRESPONDING FIELDS OF TABLE @R_RESULT.

    SELECT COUNT( * )
      FROM ZSDSFIT056.
    IF SY-SUBRC EQ 0.
      SELECT B~*
        FROM ZSDSFIT056 AS A
        INNER JOIN ZSDSVC_GET_SALES_REPORT_ALL AS B ON A~RLDNR  EQ B~RLDNR  AND
                                                       A~RBUKRS EQ B~RBUKRS AND
                                                       A~GJAHR  EQ B~GJAHR  AND
                                                       A~BELNR  EQ B~BELNR  AND
                                                       A~DOCLN  EQ B~DOCLN
        WHERE B~RLDNR    IN @IR_RLDNR[]
          AND B~RBUKRS   IN @IR_RBUKRS[]
          AND B~GJAHR    IN @IR_GJAHR[]
          AND B~BELNR    IN @IR_BELNR[]
          AND B~BUDAT    IN @IR_BUDAT[]
          AND B~PS_POSID IN @IR_POSID[]
          AND B~PRCTR    IN @IR_PRCTR[]
          AND B~AUFNR    IN @IR_AUFNR[]
          AND B~MATNR    IN @IR_MATNR[]
          AND B~FKART    IN @IR_FKART[]
          AND B~VKORG    IN @IR_VKORG[]
          AND B~VTWEG    IN @IR_VTWEG[]
          AND B~VKGRP    IN @IR_VKGRP[]
          AND B~VKBUR    IN @IR_VKBUR[]
          AND B~PRODH    IN @IR_PRODH[]
          AND B~RACCT    IN @IR_RACCT[]
        APPENDING CORRESPONDING FIELDS OF TABLE @R_RESULT.
    ENDIF.

    LCL_DATA=>GET_ADDTIONAL_DATA( CHANGING CT_RESULT = R_RESULT ).
  ENDMETHOD.
ENDCLASS.
