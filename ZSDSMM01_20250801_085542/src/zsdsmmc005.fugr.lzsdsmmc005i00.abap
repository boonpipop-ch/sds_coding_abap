*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.
  IF ZSDSMMC005-ERNAM IS INITIAL.
    ZSDSMMC005-ERNAM = SY-UNAME.
    ZSDSMMC005-ERDAT = SY-DATUM.
    ZSDSMMC005-ERZET = SY-UZEIT.
    ZSDSMMC005-AENAM = SY-UNAME.
    ZSDSMMC005-AEDAT = SY-DATUM.
    ZSDSMMC005-AEZET = SY-UZEIT.
  ELSE.
    ZSDSMMC005-AENAM = SY-UNAME.
    ZSDSMMC005-AEDAT = SY-DATUM.
    ZSDSMMC005-AEZET = SY-UZEIT.
  ENDIF.
ENDMODULE.
