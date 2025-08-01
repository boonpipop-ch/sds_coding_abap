*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.

  IF ZSDSFIT052-ERNAM IS INITIAL.
    ZSDSFIT052-ERNAM = SY-UNAME.
    ZSDSFIT052-ERDAT = SY-DATUM.
    ZSDSFIT052-ERZET = SY-UZEIT.
    ZSDSFIT052-AENAM = SY-UNAME.
    ZSDSFIT052-AEDAT = SY-DATUM.
    ZSDSFIT052-AEZET = SY-UZEIT.
  ELSE.
    ZSDSFIT052-AENAM = SY-UNAME.
    ZSDSFIT052-AEDAT = SY-DATUM.
    ZSDSFIT052-AEZET = SY-UZEIT.
  ENDIF.

ENDMODULE.
