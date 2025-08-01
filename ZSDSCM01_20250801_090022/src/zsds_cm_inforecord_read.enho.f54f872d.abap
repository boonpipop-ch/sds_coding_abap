"Name: \FU:CRMS4_INFORECORD_READ\SE:BEGIN\EI
ENHANCEMENT 0 ZSDS_CM_INFORECORD_READ.
*<-- Start of Insertion CMI008 07.11.2024 (Fix Performance Issue )
  IF ZCL_SDSCM_ENHANCEMENT=>IS_SDS( IF_WERKS = IV_PLANT ) EQ 'X'.
    CALL FUNCTION 'Z_SDSCM_CRMS4_INFORECORD_READ'
      EXPORTING
        IV_MATNR             = IV_MATNR
        IV_PLANT             = IV_PLANT
        IV_PURGRP            = IV_PURGRP
        IV_PURORG            = IV_PURORG
        IV_VALID_FROM        = IV_VALID_FROM
        IV_VALID_TO          = IV_VALID_TO
        IV_SERVICE_PERFORMER = IV_SERVICE_PERFORMER
      IMPORTING
        E_NETPR              = E_NETPR
        E_WAERS              = E_WAERS
        E_EPEIN              = E_EPEIN
      EXCEPTIONS
        NO_INFOREC_FOUND     = 1
        OTHERS               = 2.
    IF SY-SUBRC NE 0.
      IF SY-SUBRC EQ 1.
        RAISE NO_INFOREC_FOUND.
      ENDIF.
    ENDIF.
    RETURN.
  ENDIF.
*--> End of Insertion CMI008 07.11.2024
ENDENHANCEMENT.
