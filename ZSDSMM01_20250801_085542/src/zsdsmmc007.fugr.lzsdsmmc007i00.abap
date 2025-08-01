*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.

  IF ZSDSMMC007-ERNAM IS INITIAL.
    ZSDSMMC007-ERNAM = SY-UNAME.
    ZSDSMMC007-ERDAT = SY-DATUM.
    ZSDSMMC007-ERZET = SY-UZEIT.
  ENDIF.
  ZSDSMMC007-AENAM = SY-UNAME.
  ZSDSMMC007-AEDAT = SY-DATUM.
  ZSDSMMC007-AEZET = SY-UZEIT.

ENDMODULE.
