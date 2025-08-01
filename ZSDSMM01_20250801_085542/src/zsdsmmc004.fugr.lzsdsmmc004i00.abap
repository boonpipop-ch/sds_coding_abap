*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.
  IF ZSDSMMC004-ERNAM IS INITIAL.
    ZSDSMMC004-ERNAM = SY-UNAME.
    ZSDSMMC004-ERDAT = SY-DATUM.
    ZSDSMMC004-ERZET = SY-UZEIT.
    ZSDSMMC004-AENAM = SY-UNAME.
    ZSDSMMC004-AEDAT = SY-DATUM.
    ZSDSMMC004-AEZET = SY-UZEIT.
  ELSE.
    ZSDSMMC004-AENAM = SY-UNAME.
    ZSDSMMC004-AEDAT = SY-DATUM.
    ZSDSMMC004-AEZET = SY-UZEIT.
  ENDIF.
ENDMODULE.
