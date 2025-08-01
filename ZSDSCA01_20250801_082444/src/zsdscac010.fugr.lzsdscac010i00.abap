*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules
MODULE UPDATE_DATE INPUT.
  IF ZSDSCAC010-ERNAM IS INITIAL.
    ZSDSCAC010-ERNAM = SY-UNAME.
    ZSDSCAC010-ERDAT = SY-DATUM.
    ZSDSCAC010-ERZET = SY-UZEIT.
  ENDIF.
  ZSDSCAC010-AENAM = SY-UNAME.
  ZSDSCAC010-AEDAT = SY-DATUM.
  ZSDSCAC010-AEZET = SY-UZEIT.
ENDMODULE.
