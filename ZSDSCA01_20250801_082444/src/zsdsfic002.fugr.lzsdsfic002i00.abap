*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.
  IF ZSDSFIC002-ERNAM IS INITIAL.
    ZSDSFIC002-ERNAM = SY-UNAME.
    ZSDSFIC002-ERDAT = SY-DATUM.
  ENDIF.
  ZSDSFIC002-AENAM = SY-UNAME.
  ZSDSFIC002-AEDAT = SY-DATUM.
ENDMODULE.
