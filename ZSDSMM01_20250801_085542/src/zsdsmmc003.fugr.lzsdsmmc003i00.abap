*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.

  IF ZSDSMMC003-ERNAM IS INITIAL.
    ZSDSMMC003-ERNAM = SY-UNAME.
    ZSDSMMC003-ERDAT = SY-DATUM.
    ZSDSMMC003-ERZET = SY-UZEIT.
    ZSDSMMC003-AENAM = SY-UNAME.
    ZSDSMMC003-AEDAT = SY-DATUM.
    ZSDSMMC003-AEZET = SY-UZEIT.
  ELSE.
    ZSDSMMC003-AENAM = SY-UNAME.
    ZSDSMMC003-AEDAT = SY-DATUM.
    ZSDSMMC003-AEZET = SY-UZEIT.
  ENDIF.
ENDMODULE.
