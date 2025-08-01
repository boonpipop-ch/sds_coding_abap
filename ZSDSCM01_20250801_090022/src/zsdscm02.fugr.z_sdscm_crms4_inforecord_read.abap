*-----------------------------------------------------------------------
*  Program ID         : Z_SDSCM_CRMS4_INFORECORD_READ
*  Creation Date      : 07.11.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is coped function module to fix SAP performance
*                       issue.
*  Purpose            : To fix performance issue
*  Copied from        : CRMS4_INFORECORD_READ
*  Restriction        : To change is under CMI008 comment
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
FUNCTION Z_SDSCM_CRMS4_INFORECORD_READ.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_MATNR) TYPE  MATNR
*"     REFERENCE(IV_PLANT) TYPE  EWERK
*"     REFERENCE(IV_PURGRP) TYPE  BKGRP OPTIONAL
*"     REFERENCE(IV_PURORG) TYPE  EKORG OPTIONAL
*"     REFERENCE(IV_VALID_FROM) TYPE  DATUM
*"     REFERENCE(IV_VALID_TO) TYPE  DATUM
*"     REFERENCE(IV_SERVICE_PERFORMER) TYPE  BU_PARTNER OPTIONAL
*"  EXPORTING
*"     VALUE(E_NETPR) TYPE  BAPICUREXT
*"     VALUE(E_WAERS) TYPE  WAERS
*"     VALUE(E_EPEIN) TYPE  PEINH
*"  EXCEPTIONS
*"      NO_INFOREC_FOUND
*"----------------------------------------------------------------------

**********************************************************************************************
** Read price from source list -> EORD based on MATNR, WERKS and Validity
** with following conditions
**    1. IF VRTYP is marked as K or L read from EKPO/EKKO based on EBELN/EBELP and validaty
**    2. Else Read FROM EINE(M_V_Inforecord_Sos_B) based on MATNR, WERKS, LIFNR, EKORG
**    and validity.
**    Pick the one that is marked as fixed with the flag FLIFN, if not read the highest price
** Read price from m_v_purchasing_sos_b based on MATNR, WERKS and Validity
** with following conditions
**     1. READ the entry if EBELN/EBELP is not empty, Read from EKPO/EKKO based on validity
**     2. Else read from EINE(M_V_PURCHASING_SOS) based on matnr, werks, lifnr, ekorg, validity
** If multiple records found pick the one with the highest price.

  DATA: LT_SOURCELIST  TYPE STANDARD TABLE OF T_SOURCELIST,
        LT_EKKO        TYPE STANDARD TABLE OF T_EKKO,
        LV_EKORG       TYPE EKORG,
        LV_EKGRP       TYPE EKGRP,
        LV_NETPR_PUNIT TYPE NETPR,
        LV_PRICEPUNIT1 TYPE NETPR,
        LT_EINE        TYPE STANDARD TABLE OF T_EINE,
        LT_CONDREC     TYPE STANDARD TABLE OF T_COND_REC,
        LT_KONPH       TYPE STANDARD TABLE OF T_KONPH,
        LV_VALID_FROM  TYPE DATUM,
        LV_VALID_TO    TYPE DATUM,
        LT_SUPPLIER    TYPE STANDARD TABLE OF I_CONTINGENTWORKER.


  FIELD-SYMBOLS : <LS_EKKO> TYPE T_EKKO,
                  <LS_EINE> TYPE T_EINE.
  IF IV_PURGRP IS INITIAL.
    LV_EKGRP = '%'.
  ELSE.
    LV_EKGRP = IV_PURGRP.
  ENDIF.
  IF IV_PURORG IS INITIAL.
    LV_EKORG = '%'.
  ELSE.
    LV_EKORG = IV_PURORG.
  ENDIF.
  IF IV_VALID_FROM IS INITIAL.
    LV_VALID_FROM = SY-DATUM.
  ELSE.
    LV_VALID_FROM = IV_VALID_FROM.
  ENDIF.
  IF IV_VALID_TO IS INITIAL.
    LV_VALID_TO = SY-DATUM.
  ELSE.
    LV_VALID_TO = IV_VALID_TO.
  ENDIF.

  "Get list of supplier based on the service performer
  IF IV_SERVICE_PERFORMER IS NOT INITIAL.
    SELECT SUPPLIER FROM I_CONTINGENTWORKER WHERE BUSINESSPARTNER = @IV_SERVICE_PERFORMER INTO TABLE @LT_SUPPLIER.
  ENDIF.

* fetch records from Purchasing Source List
  IF LT_SUPPLIER IS INITIAL.
    SELECT MATNR, WERKS, ZEORD, LIFNR, FLIFN, EBELN, EBELP, EKORG, VRTYP "netpr, waers, peinh
                               FROM EORD AS E
                               WHERE E~MATNR = @IV_MATNR AND
                                     E~WERKS = @IV_PLANT AND
                                     E~VDATU <= @LV_VALID_FROM AND
                                     E~BDATU  >= @LV_VALID_TO AND
                                     E~NOTKZ = ' ' AND
                                     E~EKORG LIKE @LV_EKORG INTO CORRESPONDING FIELDS OF TABLE @LT_SOURCELIST.
  ELSE.
    SELECT MATNR, WERKS, ZEORD, LIFNR, FLIFN, EBELN, EBELP, EKORG, VRTYP "netpr, waers, peinh                      " Select based on the supplier as well
                                FROM EORD AS E
                                FOR ALL ENTRIES IN @LT_SUPPLIER
                                WHERE E~MATNR = @IV_MATNR AND
                                      E~WERKS = @IV_PLANT AND
                                      E~VDATU <= @LV_VALID_FROM AND
                                      E~BDATU  >= @LV_VALID_TO AND
                                      E~NOTKZ = ' ' AND
                                      E~EKORG LIKE @LV_EKORG AND
                                      E~LIFNR = @LT_SUPPLIER-SUPPLIER INTO CORRESPONDING FIELDS OF TABLE @LT_SOURCELIST.
  ENDIF.

  IF SY-SUBRC IS INITIAL.

    READ TABLE LT_SOURCELIST WITH KEY FLIFN = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC IS INITIAL.
      DELETE LT_SOURCELIST WHERE  FLIFN EQ ABAP_FALSE.
    ENDIF.

  ELSE.
*     Purchasing Sources of Supply Basic
    IF LT_SUPPLIER IS INITIAL.
      SELECT MATNR, WERKS,LIFNR,EBELN, EBELP, BSTYP,EKORG,EKGRP FROM M_V_PURCHASING_SOS_B WITH PRIVILEGED ACCESS
                              WHERE   MATNR = @IV_MATNR AND
                                      WERKS = @IV_PLANT AND
                                      VDATU <= @LV_VALID_FROM AND
                                      BDATU  >= @LV_VALID_TO AND
                                      EKORG LIKE @LV_EKORG AND
                                      EKGRP LIKE @LV_EKGRP
                                      INTO CORRESPONDING FIELDS OF TABLE @LT_SOURCELIST.
    ELSE.
      SELECT MATNR, WERKS,LIFNR,EBELN, EBELP, BSTYP,EKORG,EKGRP FROM M_V_PURCHASING_SOS_B WITH PRIVILEGED ACCESS      " Select based on the supplier as well
                            FOR ALL ENTRIES IN @LT_SUPPLIER
                            WHERE   LIFNR = @LT_SUPPLIER-SUPPLIER AND
                                    MATNR = @IV_MATNR AND
                                    WERKS = @IV_PLANT AND
                                    VDATU <= @LV_VALID_FROM AND
                                    BDATU  >= @LV_VALID_TO AND
                                    EKORG LIKE @LV_EKORG AND
                                    EKGRP LIKE @LV_EKGRP
                                    INTO CORRESPONDING FIELDS OF TABLE @LT_SOURCELIST.
    ENDIF.
  ENDIF.

  IF LT_SOURCELIST IS NOT INITIAL.
*    get the price from Purchasing agreement information

    SELECT E~EBELN ,EKORG, EKGRP,WAERS,EBELP ,WERKS , NETPR ,PEINH FROM EKKO AS E
                                         INNER JOIN EKPO AS P
                                         ON E~EBELN = P~EBELN
                                         FOR ALL ENTRIES IN @LT_SOURCELIST
                                         WHERE E~EBELN = @LT_SOURCELIST-EBELN AND
                                               EKORG LIKE @LV_EKORG AND
                                               EKGRP LIKE @LV_EKGRP AND
                                               EBELP = @LT_SOURCELIST-EBELP AND
                                               WERKS = @IV_PLANT AND
                                               ( E~BSTYP = @LT_SOURCELIST-BSTYP OR E~BSTYP = @LT_SOURCELIST-VRTYP ) AND
                                               E~LOEKZ = ' ' AND P~LOEKZ = ' ' AND KDATB <= @LV_VALID_FROM
                                                AND KDATE >= @LV_VALID_TO
                                INTO CORRESPONDING FIELDS OF TABLE @LT_EKKO.
    IF SY-SUBRC IS NOT INITIAL.
*<-- Start of Insertion CMI008 07.11.2024 (Change to internal table join)
      SELECT A~KAPPL, A~KSCHL, A~LIFNR, A~MATNR, A~EKORG, A~WERKS, A~ESOKZ, A~DATBI, A~DATAB, A~KNUMH
        FROM @LT_SOURCELIST AS X
               INNER JOIN A017 AS A
                 ON  A~LIFNR = X~LIFNR
                 AND A~MATNR = X~MATNR
                 AND A~EKORG = X~EKORG
                 AND A~WERKS = X~WERKS
        WHERE A~DATBI >= @LV_VALID_TO       " Validity end date of the condition record
          AND A~DATAB <= @LV_VALID_FROM     " Validity start date of the condition record
        INTO CORRESPONDING FIELDS OF TABLE @LT_CONDREC.
*--> End of Insertion CMI008 07.11.2024
*<-- Start of Deletion CMI008 07.11.2024 (Remove Performance Issue code)
*      SELECT kappl, kschl, lifnr, matnr, ekorg, werks, esokz, datbi, datab, knumh  FROM a017 FOR ALL ENTRIES IN @lt_sourcelist
*        WHERE lifnr = @lt_sourcelist-lifnr AND
*              matnr = @lt_sourcelist-matnr AND
*              ekorg = @lt_sourcelist-ekorg AND
*              werks = @lt_sourcelist-werks AND
*              datbi >= @lv_valid_to AND   " Validity end date of the condition record
*              datab <= @lv_valid_from     " Validity start date of the condition record
*        INTO CORRESPONDING FIELDS OF TABLE @lt_condrec.
*--> End of Deletion CMI008 07.11.2024
*      TEST-SEAM get_eine.
*  get the price from the inforecords
*        SELECT a~infnr, a~matnr, a~lifnr, e~ekorg, e~werks, e~ekgrp, e~waers, e~netpr, e~peinh
*                     FROM  eina AS a
*                     INNER JOIN eine AS e
*                     ON a~infnr = e~infnr
*          FOR ALL ENTRIES IN @lt_sourcelist
*          WHERE a~matnr = @lt_sourcelist-matnr
*            AND a~lifnr = @lt_sourcelist-lifnr
*            AND e~ekorg LIKE @lv_ekorg
*            AND e~werks = @lt_sourcelist-werks
*            AND e~ekgrp LIKE @lv_ekgrp
*          INTO CORRESPONDING FIELDS OF TABLE @lt_eine.
*      END-TEST-SEAM.
    ELSE.
*        purchasing contract found
      SELECT KAPPL, KSCHL, EVRTN, EVRTP, DATBI, DATAB, KNUMH FROM A016 FOR ALL ENTRIES IN @LT_EKKO
        WHERE EVRTN = @LT_EKKO-EBELN AND
              EVRTP = @LT_EKKO-EBELP AND
              DATBI >= @LV_VALID_TO AND " Validity end date of the condition record
              DATAB <= @LV_VALID_FROM   " Validity start date of the condition record
        INTO CORRESPONDING FIELDS OF TABLE @LT_CONDREC.

    ENDIF.
  ELSE.
    RAISE NO_INFOREC_FOUND.

  ENDIF.
  IF LT_CONDREC IS NOT INITIAL.
    SELECT A~KNUMH, A~KAPPL, A~KSCHL, DATAB, DATBI, KOPOS, KONWA, KBETR, KPEIN, KMEIN
           FROM       KONH AS A
           INNER JOIN KONP AS B
           ON         A~KNUMH = B~KNUMH
      FOR ALL ENTRIES IN @LT_CONDREC
      WHERE A~KNUMH = @LT_CONDREC-KNUMH AND
            A~KAPPL = @LT_CONDREC-KAPPL AND
            A~KSCHL = @LT_CONDREC-KSCHL AND
            LOEVM_KO = @ABAP_FALSE       AND
            DATBI >= @LV_VALID_TO       AND " Validity end date of the condition record
            DATAB <= @LV_VALID_FROM         " Validity start date of the condition record
      INTO CORRESPONDING FIELDS OF TABLE @LT_KONPH.
    IF SY-SUBRC IS INITIAL.

      IF LT_KONPH IS NOT INITIAL.
        CLEAR LV_NETPR_PUNIT.
        LOOP AT LT_KONPH ASSIGNING FIELD-SYMBOL(<LS_KONPH>).
          IF LV_NETPR_PUNIT < <LS_KONPH>-KBETR.
            LV_NETPR_PUNIT = <LS_KONPH>-KBETR.
            E_NETPR = LV_NETPR_PUNIT.
            E_WAERS = <LS_KONPH>-KONWA.
            E_EPEIN = <LS_KONPH>-KPEIN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      RAISE NO_INFOREC_FOUND.

    ENDIF.
  ELSE.
    RAISE NO_INFOREC_FOUND.
  ENDIF.

*  IF lt_ekko IS NOT INITIAL.
*    CLEAR lv_netpr_punit.
*    LOOP AT lt_ekko ASSIGNING <ls_ekko>.
*      IF <ls_ekko>-peinh IS NOT INITIAL.
*        lv_pricepunit1 = <ls_ekko>-netpr / <ls_ekko>-peinh.
*      ENDIF.
*      IF lv_netpr_punit LT lv_pricepunit1.
*        lv_netpr_punit = <ls_ekko>-netpr / <ls_ekko>-peinh.
*        e_netpr = lv_netpr_punit.
*        e_waers = <ls_ekko>-waers.
*        e_epein = <ls_ekko>-peinh.
*      ENDIF.
*    ENDLOOP.
*  ELSEIF  lt_eine IS NOT INITIAL.
*    CLEAR lv_netpr_punit.
*    LOOP AT lt_eine ASSIGNING <ls_eine>.
*      IF <ls_eine>-peinh IS NOT INITIAL.
*        lv_pricepunit1 = <ls_eine>-netpr / <ls_eine>-peinh.
*      ENDIF.
*      IF lv_netpr_punit LT lv_pricepunit1.
*        lv_netpr_punit = lv_pricepunit1.
*        e_netpr = lv_netpr_punit.
*        e_waers = <ls_eine>-waers.
*        e_epein = <ls_eine>-peinh.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    RAISE no_inforec_found.
*  ENDIF.

ENDFUNCTION.
