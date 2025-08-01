*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE update_date INPUT.
  IF ZSDSMMC006-ERNAM IS INITIAL.
    ZSDSMMC006-ERNAM = SY-UNAME.
    ZSDSMMC006-ERDAT = SY-DATUM.
    ZSDSMMC006-ERZET = SY-UZEIT.
    ZSDSMMC006-AENAM = SY-UNAME.
    ZSDSMMC006-AEDAT = SY-DATUM.
    ZSDSMMC006-AEZET = SY-UZEIT.
  ELSE.
    ZSDSMMC006-AENAM = SY-UNAME.
    ZSDSMMC006-AEDAT = SY-DATUM.
    ZSDSMMC006-AEZET = SY-UZEIT.
  ENDIF.

ENDMODULE.
