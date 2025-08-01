*---------------------------------------------------------------------*
*    view related PAI modules
*---------------------------------------------------------------------*

INCLUDE LSVIMITX . "base table related PAI modules

MODULE UPDATE_DATE INPUT.

  IF ZSDSFIC001-CREATED_DATE IS INITIAL.
    ZSDSFIC001-CREATED_DATE = SY-DATUM.
    ZSDSFIC001-CREATED_TIME = SY-UZEIT.
    ZSDSFIC001-CREATED_BY   = SY-UNAME.
  ENDIF.
  ZSDSFIC001-UPDATED_DATE = SY-DATUM.
  ZSDSFIC001-UPDATED_TIME = SY-UZEIT.
  ZSDSFIC001-UPDATED_BY   = SY-UNAME.




ENDMODULE.
