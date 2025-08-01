*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATA INPUT.

  IF ZSDSMMC008-ERNAM IS INITIAL.
    ZSDSMMC008-ERNAM = SY-UNAME.
    ZSDSMMC008-ERDAT = SY-DATUM.
    ZSDSMMC008-ERZET = SY-UZEIT.
    ZSDSMMC008-AENAM = SY-UNAME.
    ZSDSMMC008-AEDAT = SY-DATUM.
    ZSDSMMC008-AEZET = SY-UZEIT.
  ELSE.
    ZSDSMMC008-AENAM = SY-UNAME.
    ZSDSMMC008-AEDAT = SY-DATUM.
    ZSDSMMC008-AEZET = SY-UZEIT.
  ENDIF.
ENDMODULE.
