*&---------------------------------------------------------------------*
*& Include          ZSDSSDI0020_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZSDSFII0020_CLASS
*&---------------------------------------------------------------------*
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR,
      START_PROCESS.
    CLASS-METHODS :
      GET_DATA,
      GET_ADDTIONAL_DATA,
      SHOW_REPORT,
      SET_LAYOUT_OUTPUT,
      BUILD_FCAT,
      SET_SORT,
      SET_ALV_GRID,
      HTML_TOP_OF_PAGE,
      CONVERT_ALPHA_IN  IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      CONVERT_ALPHA_OUT IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      GET_MAT_LASTED,
      MAP_DATA,
      READ_TEXT IMPORTING I_ID     TYPE ANY
                          I_OBJECT TYPE ANY
                          I_VBELN  TYPE ANY
                RETURNING VALUE(R) TYPE CHAR200,
      GET_CUSTOMER IMPORTING I_KUNNR     TYPE KNA1-KUNNR
                             I_ADRNR     TYPE KNA1-ADRNR
                   CHANGING  C_NAME_ENG  TYPE ANY
                             C_ADD1      TYPE ANY
                             C_ADD2      TYPE ANY
                             C_CITY2     TYPE ANY
                             C_CITY1     TYPE ANY
                             C_POST_CODE TYPE ANY,
      GET_SALEEMP IMPORTING I_PERNR  TYPE PA0001-PERNR
                  RETURNING VALUE(R) TYPE CHAR200,
      GET_IO IMPORTING I_AUFNR  TYPE AUFK-AUFNR
             RETURNING VALUE(R) TYPE AUFK-KTEXT,
      GET_SHIPMENT IMPORTING I_VBELN TYPE LIKP-VBELN
                   CHANGING  C_TKNUM TYPE ANY
                             C_EXTI1 TYPE ANY
                             C_EXTI2 TYPE ANY
                             C_VSART TYPE VTTK-VSART,
      MAT_DOC_LASTED IMPORTING I_VBELN  TYPE ANY
                     RETURNING VALUE(R) TYPE VBFA-VBELN,
      APPEND_FIELDCAT IMPORTING I_FIELD      TYPE ANY
                                I_REFTABLE   TYPE ANY
                                I_REFFIELD   TYPE ANY
                                I_COLTXT     TYPE ANY
                                I_DOSUM      TYPE ANY
                                I_CFIELDNAME TYPE ANY
                                I_NO_ZERO    TYPE ANY
                                I_COLOR      TYPE ANY,
      GET_WAIT_INV_SER
      .
    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD GET_DATA.
    IF LO IS INITIAL.
      CREATE OBJECT LO.
    ENDIF.

    LO->START_PROCESS( ).
  ENDMETHOD.
  METHOD START_PROCESS.
*GET DATA FOR SERIAL

    IF R_WAIT EQ 'X'.
      GET_WAIT_INV_SER( ).
    ELSE.
      IF NOT S_SERNR IS INITIAL OR NOT S_MATNR IS INITIAL.
        SELECT F~LIEF_NR F~POSNR
        INTO TABLE GT_LIEF_NR
        FROM OBJK AS A INNER JOIN SER01 AS F
                       ON ( A~OBKNR = F~OBKNR )
        WHERE A~MATNR IN S_MATNR
          AND A~SERNR IN S_SERNR
          AND A~TASER EQ 'SER01'
          AND F~VBTYP NE '7'.

        IF NOT GT_LIEF_NR IS INITIAL.
          SELECT A~VBELN, B~POSNR, A~ERDAT, A~KODAT, A~LDDAT, A~LFDAT, A~WADAT, A~ROUTE, A~LSTEL,
           B~MATNR, B~MEINS, B~BRGEW, B~GEWEI, B~VOLUM, B~VOLEH,
           B~SERAIL, B~PRODH, B~VTWEG, B~VKBUR, B~VKGRP, B~VGBEL, B~VGPOS,
           B~AUFNR, D~KUNNR,  D~ADRNR, F~KUNNR, F~ADRNR, G~PERNR,
           A~KOSTK,
           B~LFIMG,
           B~WERKS,
*           B~PIKMG
           VF~RFMNG,
           VK~AUART,
           B~LGORT,
           VF~POSNV, VF~VBELN, VF~POSNN, VF~VBTYP_N , A~VBTYP , "Add by Wantanee 20120314
           VK~ZZPOB,
           VK~PS_PSP_PNR
            INTO TABLE @GT_SERIAL
            FROM LIKP AS A INNER JOIN LIPS AS B
                           ON ( A~VBELN = B~VBELN )
                           LEFT JOIN VBPA AS D
                           ON ( A~VBELN = D~VBELN
                           AND  D~PARVW = 'AG'
                           AND  D~POSNR = '' )
                           LEFT JOIN VBPA AS F
                           ON ( A~VBELN = F~VBELN
                           AND  F~PARVW = 'WE'
                           AND  F~POSNR = '' )
                           LEFT JOIN VBPA AS G
                           ON ( A~VBELN = G~VBELN
                           AND  G~PARVW = 'VE'
                           AND  G~POSNR = '' )
                           LEFT JOIN SER01 AS SE
                           ON ( A~VBELN = SE~LIEF_NR
                           AND  B~POSNR = SE~POSNR )
*                                   AND   se~vbtyp = 'J' )
                           LEFT JOIN OBJK AS OB
                           ON ( SE~OBKNR = OB~OBKNR
                           AND  B~MATNR = OB~MATNR )
*                           INNER JOIN VBUK AS H
*                           ON ( H~VBELN = A~VBELN )
                           LEFT JOIN VBFA AS VF
                           ON ( VF~VBELV = A~VBELN
                           AND  VF~POSNV = B~POSNR )
                           LEFT JOIN VBAK AS VK
                           ON ( VK~VBELN = B~VGBEL )
             FOR ALL ENTRIES IN @GT_LIEF_NR
             WHERE  A~VBELN EQ @GT_LIEF_NR-LIEF_NR
              AND A~ERDAT IN @S_ERDAT
              AND   A~KODAT IN @S_KODAT
              AND   A~LFDAT IN @S_LFDAT
              AND   B~VTWEG IN @S_VTWEG
              AND   B~VKBUR IN @S_VKBUR
              AND   B~VKGRP IN @S_VKGRP
              AND   OB~MATNR IN @S_MATNR
              AND   OB~SERNR IN @S_SERNR
              AND   A~VBELN IN @S_VBELN
              AND   B~WERKS IN @S_WERKS
              AND   VK~AUART IN @S_AUART
              AND   A~WADAT IN @S_WADAT                       "Add CH6
              AND   B~SERAIL EQ 'Z002'
              AND   VK~KUNNR IN @S_KUNNR
              AND   B~PRODH  IN @S_PRODH.
*              AND   A~VBTYP EQ 'J'.
*                      OR    se~vbtyp EQ 'J' .
*                      AND   h~fkstk NE 'C'.

        ENDIF.
      ELSE.

        SELECT A~VBELN, B~POSNR, A~ERDAT, A~KODAT, A~LDDAT, A~LFDAT, A~WADAT, A~ROUTE, A~LSTEL,
              B~MATNR, B~MEINS, B~BRGEW, B~GEWEI, B~VOLUM, B~VOLEH,
              B~SERAIL, B~PRODH, B~VTWEG, B~VKBUR, B~VKGRP, B~VGBEL, B~VGPOS,
              B~AUFNR, D~KUNNR,  D~ADRNR, F~KUNNR, F~ADRNR, G~PERNR,
              A~KOSTK,
              B~LFIMG,
              B~WERKS,
              VF~RFMNG,
              VK~AUART,
              B~LGORT,
              VF~POSNV, VF~VBELN, VF~POSNN, VF~VBTYP_N , A~VBTYP ,"Add by Wantanee 20120314
              VK~ZZPOB,
              VK~PS_PSP_PNR
       INTO TABLE @GT_SERIAL
       FROM LIKP AS A INNER JOIN LIPS AS B
                      ON ( A~VBELN = B~VBELN )
                      LEFT JOIN VBPA AS D
                      ON ( A~VBELN = D~VBELN
                      AND  D~PARVW = 'AG'
                      AND  D~POSNR = '' )
                      LEFT JOIN VBPA AS F
                      ON ( A~VBELN = F~VBELN
                      AND  F~PARVW = 'WE'
                      AND  F~POSNR = '' )
                      LEFT JOIN VBPA AS G
                      ON ( A~VBELN = G~VBELN
                      AND  G~PARVW = 'VE'
                      AND  G~POSNR = '' )
                      LEFT JOIN SER01 AS SE
                      ON ( A~VBELN = SE~LIEF_NR
                      AND  B~POSNR = SE~POSNR )
*                           AND   se~vbtyp = 'J' )
                      LEFT JOIN OBJK AS OB
                      ON ( SE~OBKNR = OB~OBKNR
                      AND  B~MATNR = OB~MATNR )
*                      INNER JOIN VBUK AS H
*                      ON ( H~VBELN = A~VBELN )
                      LEFT JOIN VBFA AS VF
                      ON ( VF~VBELV = A~VBELN
                      AND  VF~POSNV = B~POSNR )
                      LEFT JOIN VBAK AS VK
                      ON ( VK~VBELN = B~VGBEL )
        WHERE  A~ERDAT IN @S_ERDAT
         AND   A~KODAT IN @S_KODAT
         AND   A~LFDAT IN @S_LFDAT
         AND   B~VTWEG IN @S_VTWEG
         AND   B~VKBUR IN @S_VKBUR
         AND   B~VKGRP IN @S_VKGRP
         AND   B~MATNR IN @S_MATNR
         AND   OB~SERNR IN @S_SERNR
         AND   A~VBELN IN @S_VBELN
         AND   B~WERKS IN @S_WERKS
         AND   A~WADAT IN @S_WADAT                            "Add CH6
         AND   VK~AUART IN @S_AUART
         AND   B~SERAIL EQ 'Z002'
         AND   VK~KUNNR IN @S_KUNNR
         AND   B~PRODH  IN @S_PRODH.
*              OR    se~vbtyp EQ 'J' .
*              AND   h~fkstk NE 'C'.

      ENDIF.


    ENDIF.

    "PERFORM get_mat_lasted. " changed by kab 03.08.2015

    MOVE GT_SERIAL TO GT_DELISR.
    SORT GT_DELISR BY VBELN.
    DELETE ADJACENT DUPLICATES FROM GT_DELISR.

    LOOP AT GT_SERIAL INTO WA_SERIAL.
      IF WA_SERIAL-VGBEL NE ''.
        MOVE-CORRESPONDING WA_SERIAL TO WA_SALE_SER.
        APPEND WA_SALE_SER TO GT_SALE_SER.
      ENDIF.
    ENDLOOP.
    SORT GT_SALE_SER BY VGBEL VGPOS.
    DELETE ADJACENT DUPLICATES FROM GT_SALE_SER.



    IF NOT GT_DELISR IS INITIAL.  "Add by Wantanee 20120314
      SELECT A~LIEF_NR A~POSNR B~OBKNR B~MATNR B~SERNR
      INTO TABLE GT_SER01
      FROM SER01 AS A INNER JOIN OBJK AS B
                      ON ( A~OBKNR = B~OBKNR )
      FOR ALL ENTRIES IN GT_DELISR
      WHERE LIEF_NR EQ GT_DELISR-VBELN
        AND B~SERNR IN S_SERNR.
*BOI CH07
      IF SY-SUBRC = 0.
        SORT GT_SER01 BY LIEF_NR POSNR MATNR.
      ENDIF.
*EOI CH07


      SELECT A~VBELN A~TKNUM B~EXTI1 B~EXTI2 B~VSART
      INTO TABLE GT_SHIPMENT
      FROM VTTP AS A INNER JOIN VTTK AS B
                     ON ( A~TKNUM = B~TKNUM )
      FOR ALL ENTRIES IN GT_DELISR
      WHERE A~VBELN  EQ GT_DELISR-VBELN.
    ENDIF.

    IF NOT GT_SALE_SER IS INITIAL.
      SELECT VBELV POSNV VBELN
      INTO TABLE GT_CHECK_INV_S
      FROM VBFA
      FOR ALL ENTRIES IN GT_SALE_SER
      WHERE VBELV EQ GT_SALE_SER-VGBEL
        AND POSNV EQ GT_SALE_SER-VGPOS
        AND ( VBTYP_N EQ 'M'
        OR  VBTYP_N EQ 'N'
        OR  VBTYP_N EQ 'O'
        OR  VBTYP_N EQ 'P' ).
*BOI CH07
      IF SY-SUBRC = 0.
        SORT GT_CHECK_INV_S BY VBELV POSNV.
      ENDIF.
*EOI CH07
    ENDIF.

*get data for noserial
    IF R_WAIT EQ 'X'.
      SELECT A~VBELN, B~POSNR, A~ERDAT, A~KODAT, A~LDDAT, A~LFDAT, A~WADAT, A~ROUTE, A~LSTEL,
                  B~MATNR, B~MEINS, B~BRGEW, B~GEWEI, B~VOLUM, B~VOLEH,
                  B~SERAIL, B~PRODH, B~VTWEG, B~VKBUR, B~VKGRP, B~VGBEL, B~VGPOS,
                  B~AUFNR, D~KUNNR,  D~ADRNR, F~KUNNR, F~ADRNR, G~PERNR,
                  A~KOSTK,
                  B~LFIMG,
                  B~WERKS,
                  VF~RFMNG,
                  VK~AUART,
                  B~LGORT,
                  VF~POSNV, VF~VBELN, VF~POSNN, VF~VBTYP_N, A~VBTYP,
                  VK~ZZPOB,  "Add by Wantanee 20120314
                  VK~PS_PSP_PNR
           INTO TABLE @GT_NOSERIAL
           FROM LIKP AS A INNER JOIN LIPS AS B
                          ON ( A~VBELN = B~VBELN )
                          LEFT JOIN VBPA AS D
                          ON ( A~VBELN = D~VBELN
                          AND  D~PARVW = 'AG'
                          AND  D~POSNR = '' )
                          LEFT JOIN VBPA AS F
                          ON ( A~VBELN = F~VBELN
                          AND  F~PARVW = 'WE'
                          AND  F~POSNR = '' )
                          LEFT JOIN VBPA AS G
                          ON ( A~VBELN = G~VBELN
                          AND  G~PARVW = 'VE'
                          AND  G~POSNR = '' )
*                          INNER JOIN VBUK AS H
*                          ON ( H~VBELN = A~VBELN )
                          LEFT OUTER JOIN VBFA AS VF
                          ON ( ( VF~VBELV = A~VBELN AND  VF~POSNV = B~POSNR ) OR
                               ( VF~VBELN = A~VBELN AND  VF~POSNN = B~POSNR )
                              )
                          INNER JOIN VBAK AS VK
                          ON ( VK~VBELN = B~VGBEL )
            WHERE  A~ERDAT IN @S_ERDAT
             AND   A~KODAT IN @S_KODAT
             AND   A~LFDAT IN @S_LFDAT
             AND   B~VTWEG IN @S_VTWEG
             AND   B~VKBUR IN @S_VKBUR
             AND   B~VKGRP IN @S_VKGRP
             AND   B~MATNR IN @S_MATNR
             AND   A~VBELN IN @S_VBELN
             AND   B~WERKS IN @S_WERKS
             AND   VK~AUART IN @S_AUART
             AND   A~WADAT IN @S_WADAT                        "Add CH6
             AND   B~SERAIL EQ ''
             AND   B~KOWRR EQ ''
             AND   A~FKSTK NE 'C'
             AND   VK~KUNNR IN @S_KUNNR
             AND   B~PRODH  IN @S_PRODH.

    ELSE.
      SELECT A~VBELN, B~POSNR, A~ERDAT, A~KODAT, A~LDDAT, A~LFDAT, A~WADAT, A~ROUTE, A~LSTEL,
                  B~MATNR, B~MEINS, B~BRGEW, B~GEWEI, B~VOLUM, B~VOLEH,
                  B~SERAIL, B~PRODH, B~VTWEG, B~VKBUR, B~VKGRP, B~VGBEL, B~VGPOS,
                  B~AUFNR, D~KUNNR,  D~ADRNR, F~KUNNR, F~ADRNR, G~PERNR,
                  A~KOSTK,
                  B~LFIMG,
                  B~WERKS,
                  VF~RFMNG,
                  VK~AUART,
                  B~LGORT,
                  VF~POSNV, VF~VBELN, VF~POSNN, VF~VBTYP_N, A~VBTYP,
                  VK~ZZPOB,
                  VK~PS_PSP_PNR
           INTO TABLE @GT_NOSERIAL
           FROM LIKP AS A INNER JOIN LIPS AS B
                          ON ( A~VBELN = B~VBELN )
                          LEFT JOIN VBPA AS D
                          ON ( A~VBELN = D~VBELN
                          AND  D~PARVW = 'AG'
                          AND  D~POSNR = '' )
                          LEFT JOIN VBPA AS F
                          ON ( A~VBELN = F~VBELN
                          AND  F~PARVW = 'WE'
                          AND  F~POSNR = '' )
                          LEFT JOIN VBPA AS G
                          ON ( A~VBELN = G~VBELN
                          AND  G~PARVW = 'VE'
                          AND  G~POSNR = '' )
*                          INNER JOIN VBUK AS H
*                          ON ( H~VBELN = A~VBELN )
                          LEFT JOIN VBFA AS VF
                          ON ( ( VF~VBELV = A~VBELN AND  VF~POSNV = B~POSNR ) OR
                               ( VF~VBELN = A~VBELN AND  VF~POSNN = B~POSNR )
                              )
                          INNER JOIN VBAK AS VK
                          ON ( VK~VBELN = B~VGBEL )
            WHERE  A~ERDAT IN @S_ERDAT
             AND   A~KODAT IN @S_KODAT
             AND   A~LFDAT IN @S_LFDAT
             AND   B~VTWEG IN @S_VTWEG
             AND   B~VKBUR IN @S_VKBUR
             AND   B~VKGRP IN @S_VKGRP
             AND   B~MATNR IN @S_MATNR
             AND   A~VBELN IN @S_VBELN
             AND   B~WERKS IN @S_WERKS
             AND   VK~AUART IN @S_AUART
             AND   A~WADAT IN @S_WADAT                        "Add CH6
             AND   B~SERAIL EQ ''
             AND   B~KOWRR EQ ''
             AND   VK~KUNNR IN @S_KUNNR
             AND   B~PRODH  IN @S_PRODH.
*               AND   h~fkstk NE 'C'.
    ENDIF.

    GET_MAT_LASTED( ). "changed by kab 03.08.2015


    MOVE GT_NOSERIAL TO GT_DELINSR.
    SORT GT_DELINSR BY VBELN.
    DELETE ADJACENT DUPLICATES FROM GT_DELINSR.

    LOOP AT GT_NOSERIAL INTO WA_NOSERIAL.
      IF WA_NOSERIAL-VGBEL NE ''.
        MOVE-CORRESPONDING WA_NOSERIAL TO WA_SALE_NOSER.
        APPEND WA_SALE_NOSER TO GT_SALE_NOSER.
      ENDIF.
    ENDLOOP.
    SORT GT_SALE_NOSER BY  VGBEL VGPOS.
    DELETE ADJACENT DUPLICATES FROM GT_SALE_NOSER.

    IF NOT GT_DELINSR IS INITIAL. "Add by Wantanee 20120314
      SELECT A~VBELN A~TKNUM B~EXTI1 B~EXTI2 B~VSART
      INTO TABLE GT_SHIPMENTNSR
      FROM VTTP AS A INNER JOIN VTTK AS B
                     ON ( A~TKNUM = B~TKNUM )
      FOR ALL ENTRIES IN GT_DELINSR
      WHERE A~VBELN  EQ GT_DELINSR-VBELN.
    ENDIF.

    IF NOT GT_SALE_NOSER IS INITIAL.
      SELECT VBELV POSNV VBELN
      INTO TABLE GT_CHECK_INV_NS
      FROM VBFA
      FOR ALL ENTRIES IN GT_SALE_NOSER
      WHERE VBELV EQ GT_SALE_NOSER-VGBEL
        AND POSNV EQ GT_SALE_NOSER-VGPOS
        AND ( VBTYP_N EQ 'M'
        OR  VBTYP_N EQ 'N'
        OR  VBTYP_N EQ 'O'
        OR  VBTYP_N EQ 'P' ).
*BOI CH07
      IF SY-SUBRC = 0.
        SORT GT_CHECK_INV_NS BY VBELV
                                POSNV.
      ENDIF.
*EOI CH07
    ENDIF.


    SELECT VKBUR BEZEI
    INTO TABLE GT_TVKBT
    FROM TVKBT
    WHERE SPRAS EQ 'E'.

    SELECT VKGRP BEZEI
    INTO TABLE GT_TVGRT
    FROM TVGRT
    WHERE SPRAS EQ 'E'.

    SELECT VTWEG VTEXT
    INTO TABLE GT_TVTWT
    FROM TVTWT
    WHERE SPRAS EQ 'E'.

    SELECT LSTEL VTEXT
    INTO TABLE GT_TVLAT
    FROM TVLAT
    WHERE SPRAS EQ 'E'.

    SELECT TBNAM FDNAM STATU BEZEI
    INTO TABLE GT_TVBST
    FROM TVBST
    WHERE TBNAM = 'VBUK'
      AND FDNAM = 'KOSTK'
      AND SPRAS EQ 'E'.

  ENDMETHOD.
  METHOD GET_WAIT_INV_SER.
    SELECT A~VBELN, B~POSNR, A~ERDAT, A~KODAT, A~LDDAT, A~LFDAT, A~WADAT, A~ROUTE, A~LSTEL,
           B~MATNR,  B~MEINS, B~BRGEW, B~GEWEI, B~VOLUM, B~VOLEH,
           B~SERAIL, B~PRODH, B~VTWEG, B~VKBUR, B~VKGRP, B~VGBEL, B~VGPOS,
           B~AUFNR, D~KUNNR, D~ADRNR, F~KUNNR, F~ADRNR, G~PERNR,
           A~KOSTK,
           B~LFIMG,
           B~WERKS,
*             B~PIKMG
           VF~RFMNG,
           VK~AUART,
           B~LGORT,
           VF~POSNV, VF~VBELN, VF~POSNN, VF~VBTYP_N , A~VBTYP,
           VK~ZZPOB
    INTO TABLE @GT_SERIAL
    FROM LIKP AS A INNER JOIN LIPS AS B
                   ON ( A~VBELN = B~VBELN )
                   LEFT JOIN VBPA AS D
                   ON ( A~VBELN = D~VBELN
                   AND  D~PARVW = 'AG'
                   AND  D~POSNR = '' )
                   LEFT JOIN VBPA AS F
                   ON ( A~VBELN = F~VBELN
                   AND  F~PARVW = 'WE'
                   AND  F~POSNR = '' )
                   LEFT JOIN VBPA AS G
                   ON ( A~VBELN = G~VBELN
                   AND  G~PARVW = 'VE'
                   AND  G~POSNR = '' )
                   LEFT JOIN VBFA AS VF
                   ON ( VF~VBELV = A~VBELN
                   AND  VF~POSNV = B~POSNR )
                   LEFT JOIN VBAK AS VK
                   ON ( VK~VBELN = B~VGBEL )
     WHERE  A~ERDAT IN @S_ERDAT
      AND   A~KODAT IN @S_KODAT
      AND   A~LFDAT IN @S_LFDAT
      AND   B~VTWEG IN @S_VTWEG
      AND   B~VKBUR IN @S_VKBUR
      AND   B~VKGRP IN @S_VKGRP
      AND   B~MATNR IN @S_MATNR
      AND   A~VBELN IN @S_VBELN
      AND   B~WERKS IN @S_WERKS
      AND   VK~AUART IN @S_AUART
      AND   A~WADAT IN @S_WADAT                             "Add CH6
      AND   B~SERAIL EQ 'Z002'
      AND   A~FKSTK NE 'C'
      AND   VK~KUNNR IN @S_KUNNR
      AND   B~PRODH  IN @S_PRODH.

  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    DATA : BEGIN OF LS_VBFA,
             VBELV  TYPE VBFA-VBELV,
             POSNV  TYPE VBFA-POSNV,
             VBELN  TYPE VBFA-VBELN,
             POSNN  TYPE VBFA-POSNN,
             NETWR  TYPE VBAP-NETWR,
             KWMENG TYPE VBAP-KWMENG,
           END OF LS_VBFA.
    DATA LT_VBFA LIKE TABLE OF LS_VBFA.

    DATA : BEGIN OF LS_MARA,
             MATNR TYPE MARA-MATNR,
             VOLUM TYPE MARA-VOLUM,
           END OF LS_MARA.
    DATA LT_MARA LIKE TABLE OF LS_MARA.

    DATA : BEGIN OF LS_VBAK,
             VBELN TYPE VBAK-VBELN,
             VKGRP TYPE VBAK-VKGRP,
             BEZEI TYPE TVGRT-BEZEI,
           END OF LS_VBAK.
    DATA LT_VBAK LIKE TABLE OF LS_VBAK.

    DATA : BEGIN OF LS_EQUI,
             MATNR TYPE EQUI-MATNR,
             SERNR TYPE EQUI-SERNR,
             GWLDT TYPE BGMKOBJ-GWLDT,
             GWLEN TYPE BGMKOBJ-GWLEN,
           END OF LS_EQUI.
    DATA LT_EQUI LIKE TABLE OF LS_EQUI.

    DATA LS_ITAB TYPE TYP_ITAB.

    DATA LV_TABIX TYPE SY-TABIX.

    SELECT VBFA~VBELV
           VBFA~POSNV
           VBFA~VBELN
           VBFA~POSNN
           VBAP~NETWR
           VBAP~KWMENG
      FROM VBFA
      INNER JOIN VBAP ON VBFA~VBELV EQ VBAP~VBELN AND
                         VBFA~POSNV EQ VBAP~POSNR
      INTO TABLE LT_VBFA
      FOR ALL ENTRIES IN GT_ITAB
      WHERE VBFA~VBELN   EQ GT_ITAB-LIEF_NR
        AND VBFA~POSNN   EQ GT_ITAB-POSNR
        AND VBFA~VBTYP_N EQ 'J'
        AND VBFA~VBTYP_V EQ 'C'.

    SELECT MATNR
           VOLUM
      FROM MARA
      INTO TABLE LT_MARA
      FOR ALL ENTRIES IN GT_ITAB
      WHERE MATNR EQ GT_ITAB-MATNR.


    SELECT VBAK~VBELN
           VBAK~VKGRP
           TVGRT~BEZEI
      FROM VBAK
      INNER JOIN TVGRT ON VBAK~VKGRP  EQ TVGRT~VKGRP AND
                          TVGRT~SPRAS EQ SY-LANGU
      INTO TABLE LT_VBAK
      FOR ALL ENTRIES IN GT_ITAB
      WHERE VBELN EQ GT_ITAB-VBELN.

    DATA : BEGIN OF LS_PO,
             VBELN TYPE VBKD-VBELN,

           END OF LS_PO.
    DATA : LT_PO LIKE HASHED TABLE OF LS_PO WITH UNIQUE KEY VBELN.

    LT_PO =  CORRESPONDING #( GT_ITAB  DISCARDING DUPLICATES ).

    SELECT VBKD~BSTKD,
              A~VBELN,
           VBKD~POSNR
      FROM  @LT_PO AS A
      INNER JOIN VBKD ON A~VBELN EQ VBKD~VBELN
                     AND VBKD~POSNR EQ '000000'
      INTO TABLE @DATA(LT_TMP).

    IF R_ALL EQ 'X'.


      SELECT EQUI~MATNR
             EQUI~SERNR
             BGMKOBJ~GWLDT
             BGMKOBJ~GWLEN
        FROM EQUI
        INNER JOIN BGMKOBJ ON EQUI~OBJNR    EQ BGMKOBJ~J_OBJNR AND
                              BGMKOBJ~GAART EQ '1'
        INTO TABLE LT_EQUI
        FOR ALL ENTRIES IN GT_ITAB
        WHERE EQUI~MATNR EQ GT_ITAB-MATNR
          AND EQUI~SERNR EQ GT_ITAB-SERNR.
    ENDIF.


    LOOP AT GT_ITAB INTO LS_ITAB.
      LV_TABIX = SY-TABIX.
      READ TABLE LT_VBFA INTO LS_VBFA
      WITH KEY VBELN = LS_ITAB-LIEF_NR
               POSNN = LS_ITAB-POSNR.
      IF SY-SUBRC = 0.
        IF LS_ITAB-SERNR IS NOT INITIAL.
          LS_ITAB-NETWR = LS_VBFA-NETWR / LS_VBFA-KWMENG.
        ELSE.
          LS_ITAB-NETWR = LS_VBFA-NETWR.
        ENDIF.

        LS_ITAB-VGPOS = LS_VBFA-POSNV.
      ENDIF.

      READ TABLE LT_MARA INTO LS_MARA
      WITH KEY MATNR = LS_ITAB-MATNR.
      IF SY-SUBRC = 0.
        LS_ITAB-VOLUM = LS_ITAB-DI_QTY * LS_MARA-VOLUM.
      ENDIF.

      READ TABLE LT_VBAK INTO LS_VBAK
      WITH KEY VBELN = LS_ITAB-VBELN.
      IF SY-SUBRC = 0.
        LS_ITAB-SALESGROUP = LS_VBAK-BEZEI.
        LS_ITAB-VKGRP      = LS_VBAK-VKGRP.
      ENDIF.

      READ TABLE LT_TMP INTO DATA(LS_TMP)
     WITH KEY VBELN = LS_ITAB-VBELN.
      IF SY-SUBRC = 0.
        LS_ITAB-BSTKD = LS_TMP-BSTKD.
      ENDIF.

      IF LS_ITAB-AUART EQ 'ZR01'.
        LS_ITAB-DI_ZR01 = LS_ITAB-DI_QTY * -1.
      ELSE.
        LS_ITAB-DI_ZR01 = LS_ITAB-DI_QTY.
      ENDIF.

      IF R_ALL EQ 'X'.
        READ TABLE LT_EQUI INTO LS_EQUI
        WITH KEY MATNR = LS_ITAB-MATNR
                 SERNR = LS_ITAB-SERNR.
        IF SY-SUBRC = 0.
          LS_ITAB-GWLDT = LS_EQUI-GWLDT.
          LS_ITAB-GWLEN = LS_EQUI-GWLEN.
        ELSE.
          LS_ITAB-GWLDT = LS_ITAB-LFDAT.
          LS_ITAB-GWLEN = LS_ITAB-LFDAT + 540.
        ENDIF.
      ENDIF.

      MODIFY GT_ITAB FROM LS_ITAB INDEX LV_TABIX
                           TRANSPORTING NETWR VOLUM SALESGROUP VKGRP GWLDT GWLEN VGPOS BSTKD DI_ZR01.
      CLEAR : LS_ITAB,LS_VBFA,LS_MARA.
    ENDLOOP.
    SORT GT_ITAB BY LIEF_NR POSNR MATNR SERNR.
    DELETE ADJACENT DUPLICATES FROM GT_ITAB.
  ENDMETHOD.
  METHOD SHOW_REPORT.
    SET_LAYOUT_OUTPUT( ).
    BUILD_FCAT( ).
    SET_SORT( ).
    SET_ALV_GRID( ).
  ENDMETHOD.
  METHOD SET_LAYOUT_OUTPUT.
    GS_LAYOUT-ZEBRA             = GC_X.
    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
  ENDMETHOD.
  METHOD BUILD_FCAT.

    APPEND_FIELDCAT( I_FIELD      = 'LIEF_NR'
                     I_REFTABLE   = 'SER01'
                     I_REFFIELD   = 'LIEF_NR'
                     I_COLTXT     = 'Delivery No.'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'POSNR'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'POSNR'
                     I_COLTXT     = 'Delivery No.'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'AUART'
                     I_REFTABLE   = 'VBAK'
                     I_REFFIELD   = 'AUART'
                     I_COLTXT     = 'SO Type'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VBELN'
                     I_REFTABLE   = 'VBAK'
                     I_REFFIELD   = 'VBELN'
                     I_COLTXT     = 'Sale Order'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VGPOS'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'VGPOS'
                     I_COLTXT     = 'Sales Order Line'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'MATNR'
                     I_REFTABLE   = 'OBJK'
                     I_REFFIELD   = 'MATNR'
                     I_COLTXT     = 'Material'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'SERNR'
                     I_REFTABLE   = 'OBJK'
                     I_REFFIELD   = 'SERNR'
                     I_COLTXT     = 'Serial'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'DI_ZR01'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'DELIVERY QTY'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'PK_QTY'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'PICKING QTY'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'LGORT'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'LGORT'
                     I_COLTXT     = 'SLoc'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'KUNNR_SOLDTO'
                     I_REFTABLE   = 'KNA1'
                     I_REFFIELD   = 'KUNNR'
                     I_COLTXT     = 'Customer code soldto'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'NAME_SOLDTO'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Customer name  soldto'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'KUNNR_SHIPTO'
                     I_REFTABLE   = 'KNA1'
                     I_REFFIELD   = 'KUNNR'
                     I_COLTXT     = 'Customer code shipto'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'NAME_SHIPTO'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Customer name  shipto'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'CLASS'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Class'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'CATEGORY'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Category'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'KODAT'
                     I_REFTABLE   = 'LIKP'
                     I_REFFIELD   = 'KODAT'
                     I_COLTXT     = 'Pick Date'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'LDDAT'
                     I_REFTABLE   = 'LIKP'
                     I_REFFIELD   = 'LDDAT'
                     I_COLTXT     = 'Load Date'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'LFDAT'
                     I_REFTABLE   = 'LIKP'
                     I_REFFIELD   = 'LFDAT'
                     I_COLTXT     = 'deliv.date'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'WADAT'
                     I_REFTABLE   = 'LIKP'
                     I_REFFIELD   = 'WADAT'
                     I_COLTXT     = 'GI Date'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'KOSTK'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Picking Status'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VTWEG_TXT'
                     I_REFTABLE   = 'TVTWT'
                     I_REFFIELD   = 'TVTWT'
                     I_COLTXT     = 'Chanel'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VKBUR_TXT'
                     I_REFTABLE   = 'TVKBT'
                     I_REFFIELD   = 'BEZEI'
                     I_COLTXT     = 'Sale office'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'SALE_CODE'
                     I_REFTABLE   = 'VBPA'
                     I_REFFIELD   = 'PARNR'
                     I_COLTXT     = 'Sale code'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'SALE_NAME'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Sale name'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'AUFNR'
                     I_REFTABLE   = 'AUFK'
                     I_REFFIELD   = 'AUFNR'
                     I_COLTXT     = 'Project Code'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'KTEXT'
                     I_REFTABLE   = 'AUFK'
                     I_REFFIELD   = 'KTEXT'
                     I_COLTXT     = 'Project name'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'SHIPTO_ADD1'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Ship to address'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'ROUTE'
                     I_REFTABLE   = 'LIKP'
                     I_REFFIELD   = 'ROUTE'
                     I_COLTXT     = 'Route'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'SHIPTO_LOC'
                     I_REFTABLE   = 'ADRC'
                     I_REFFIELD   = 'CITY'
                     I_COLTXT     = 'Location ship-to'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'BRGEW'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'BRGEW'
                     I_COLTXT     = 'Total Wght'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'GEWEI'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'GEWEI'
                     I_COLTXT     = 'WUn'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VOLUM'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'VOLUM'
                     I_COLTXT     = 'Volum'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VOLEH'
                     I_REFTABLE   = 'LIPS'
                     I_REFFIELD   = 'VOLEH'
                     I_COLTXT     = 'VUn'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'LSTEL_TXT'
                     I_REFTABLE   = 'TVLAT'
                     I_REFFIELD   = 'VTEXT'
                     I_COLTXT     = 'LoadPt'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'TKNUM'
                     I_REFTABLE   = 'VTTP'
                     I_REFFIELD   = 'TKNUM'
                     I_COLTXT     = 'Shipment'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'EXTI1'
                     I_REFTABLE   = 'VTTK'
                     I_REFFIELD   = 'EXTI1'
                     I_COLTXT     = 'External ID1'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'EXTI2'
                     I_REFTABLE   = 'VTTK'
                     I_REFFIELD   = 'EXTI2'
                     I_COLTXT     = 'Truck drive'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'VSART'
                     I_REFTABLE   = 'VTTK'
                     I_REFFIELD   = 'VSART'
                     I_COLTXT     = 'Shipping Type'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'INV_REK'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Invoice remark'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'REQ_REK'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Request remark'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'MAT_DOC'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Mat Doc.'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'MAT_DOC_L'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Goods Receipt No.'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'INV_NO'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Invoice no.'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'NETWR'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Sales Amount'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'SALESGROUP'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Sales Group'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'ZZPOB'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'POB'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'ZZPOB'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'POB'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'PS_PSP_PNR'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'WBS'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'POST1'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'WBS Description'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'LAND_REK'
                     I_REFTABLE   = ''
                     I_REFFIELD   = ''
                     I_COLTXT     = 'Land No. Remark'
                     I_DOSUM      = SPACE
                     I_CFIELDNAME = SPACE
                     I_NO_ZERO    = SPACE
                     I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'PROJ_REK'
                      I_REFTABLE   = ''
                      I_REFFIELD   = ''
                      I_COLTXT     = 'Project Remark'
                      I_DOSUM      = SPACE
                      I_CFIELDNAME = SPACE
                      I_NO_ZERO    = SPACE
                      I_COLOR      = SPACE ).

    APPEND_FIELDCAT( I_FIELD      = 'BSTKD'
                      I_REFTABLE   = ''
                      I_REFFIELD   = ''
                      I_COLTXT     = 'Customer Reference'
                      I_DOSUM      = SPACE
                      I_CFIELDNAME = SPACE
                      I_NO_ZERO    = SPACE
                      I_COLOR      = SPACE ).

    IF R_ALL EQ 'X'.
      APPEND_FIELDCAT( I_FIELD      = 'GWLDT'
                       I_REFTABLE   = ''
                       I_REFFIELD   = ''
                       I_COLTXT     = 'Warranty Start'
                       I_DOSUM      = SPACE
                       I_CFIELDNAME = SPACE
                       I_NO_ZERO    = SPACE
                       I_COLOR      = SPACE ).

      APPEND_FIELDCAT( I_FIELD      = 'GWLEN'
                       I_REFTABLE   = ''
                       I_REFFIELD   = ''
                       I_COLTXT     = 'Warranty End'
                       I_DOSUM      = SPACE
                       I_CFIELDNAME = SPACE
                       I_NO_ZERO    = SPACE
                       I_COLOR      = SPACE ).


    ENDIF.
  ENDMETHOD.
  METHOD SET_SORT.
**  CLEAR gs_sort.
**  gs_sort-fieldname = 'LIFNR'.
**  gs_sort-spos = '1'.
**  gs_sort-up = 'X'.
***  gs_sort-subtot = 'X'.
**  APPEND gs_sort TO gt_sort.
  ENDMETHOD.
  METHOD SET_ALV_GRID.
*SAPLKKBL
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
        "I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
        I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE  = ' '
*       i_html_height_top       = 12
*       I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME        =
*       I_BACKGROUND_ID         = ' '
*       I_GRID_TITLE            =
*       I_GRID_SETTINGS         =
        IS_LAYOUT               = GS_LAYOUT
        IT_FIELDCAT             = GT_FCAT
*       IT_EXCLUDING            =
*       IT_SPECIAL_GROUPS       =
        IT_SORT                 = GT_SORT
*       IT_FILTER               =
*       IS_SEL_HIDE             =
        I_DEFAULT               = GC_X
        I_SAVE                  = GC_A
*       IS_VARIANT              =
*       IT_EVENTS               =
*       IT_EVENT_EXIT           =
*       IS_PRINT                =
*       IS_REPREP_ID            =
*       I_SCREEN_START_COLUMN   = 0
*       I_SCREEN_START_LINE     = 0
*       I_SCREEN_END_COLUMN     = 0
*       I_SCREEN_END_LINE       = 0
*       I_HTML_HEIGHT_TOP       = 0
*       I_HTML_HEIGHT_END       = 0
*       IT_ALV_GRAPHICS         =
*       IT_HYPERLINK            =
*       IT_ADD_FIELDCAT         =
*       IT_EXCEPT_QINFO         =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER =
*       ES_EXIT_CAUSED_BY_USER  =
      TABLES
        T_OUTTAB                = GT_ITAB
      EXCEPTIONS
        PROGRAM_ERROR           = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.



  METHOD HTML_TOP_OF_PAGE.
*  DATA: text TYPE sdydo_text_element.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 100.
*  text =  'Company Code Data'.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'HEADING'.
*
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
*
*  text = 'User Name : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-uname.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 50.
*
*
*  text = 'Date : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-datum.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 50.
*
*  text = 'Time : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-uzeit.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
  ENDMETHOD.
  METHOD CONVERT_ALPHA_IN.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_OUT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_outPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
  METHOD GET_MAT_LASTED.
    DATA : LC_GET_MAT_DOC TYPE TABLE OF TYP_GET_MAT_DOC.

    IF GT_SERIAL[] IS NOT INITIAL.
      CLEAR LC_GET_MAT_DOC.
      SELECT VBELV
             POSNV
             VBELN
             POSNN
             VBTYP_N
             BWART
        FROM VBFA
        INTO CORRESPONDING FIELDS OF TABLE LC_GET_MAT_DOC
        FOR ALL ENTRIES IN GT_SERIAL
        WHERE VBELV   = GT_SERIAL-VBELN
          AND POSNV   = GT_SERIAL-POSNV
          AND VBELN   = GT_SERIAL-VBELN_VF
          AND POSNN   = GT_SERIAL-POSNN
          AND VBTYP_N = GT_SERIAL-VBTYP_N.

      APPEND LINES OF LC_GET_MAT_DOC TO GT_GET_MAT_DOC.
    ENDIF.

    IF GT_NOSERIAL[] IS NOT INITIAL.
      CLEAR LC_GET_MAT_DOC.
      SELECT VBELV
            POSNV
            VBELN
            POSNN
            VBTYP_N
            BWART
       FROM VBFA
       INTO CORRESPONDING FIELDS OF TABLE LC_GET_MAT_DOC
       FOR ALL ENTRIES IN GT_NOSERIAL
       WHERE VBELV   = GT_NOSERIAL-VBELN
         AND POSNV   = GT_NOSERIAL-POSNV
         AND VBELN   = GT_NOSERIAL-VBELN_VF
         AND POSNN   = GT_NOSERIAL-POSNN
         AND VBTYP_N = GT_NOSERIAL-VBTYP_N.

      APPEND LINES OF LC_GET_MAT_DOC TO GT_GET_MAT_DOC.
    ENDIF.
  ENDMETHOD.
  METHOD MAP_DATA.
    DATA: LV_SALESERNAME(100) TYPE C,
          LV_SALENAME(30)     TYPE C.
    DATA: LV_COUNTQTY TYPE I.
    DATA: LV_FKSAK TYPE VBUK-FKSAK.
    DATA: LV_CHECK TYPE C.
    DATA: LT_SERIAL_TMP TYPE STANDARD TABLE OF TYP_SERIAL.
    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.
    CLEAR: GT_ITAB.

    SORT GT_SERIAL.
    DELETE ADJACENT DUPLICATES FROM GT_SERIAL.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    LT_SERIAL_TMP[] = GT_SERIAL[].
*    DELETE LT_SERIAL_TMP WHERE VBTYP_N <> 'Q'
*                           AND VBTYP_N <> 'R'.
*    SORT GT_ITAB BY VBTYP.
    LOOP AT LT_SERIAL_TMP INTO WA_SERIAL WHERE VBTYP NE '7'.

      CLEAR: WA_ITAB,LV_COUNTQTY,LV_FKSAK,LV_CHECK.

      WA_ITAB-MAT_DOC_L = MAT_DOC_LASTED( WA_SERIAL-VBELN ).

      READ TABLE GT_SERIAL INTO WA_SERIAL_TMP WITH KEY VBELN = WA_SERIAL-VBELN
                                                       POSNR = WA_SERIAL-POSNV
                                              BINARY SEARCH.                        "CH07++


      READ TABLE GT_SERIAL INTO WA_CHK_SMAT_DOC WITH KEY VBELN = WA_SERIAL-VBELN
                                                         POSNR = WA_SERIAL-POSNV
                                                         VBTYP_N = 'R'
                                                BINARY SEARCH.                      "CH07++
      IF SY-SUBRC EQ 0.
        WA_ITAB-MAT_DOC = WA_CHK_SMAT_DOC-VBELN_VF.
      ENDIF.
      IF R_WAIT EQ 'X'.
        READ TABLE GT_CHECK_INV_S INTO WA_CHECK_INV_S WITH KEY VBELV = WA_SERIAL-VGBEL
                                                               POSNV = WA_SERIAL-VGPOS
                                                      BINARY SEARCH.                "CH07++

        IF SY-SUBRC EQ 0.
          LV_CHECK = 'X'.
        ELSE.
          LV_CHECK = ''.
        ENDIF.
      ELSE.
        READ TABLE GT_CHECK_INV_S INTO WA_CHECK_INV_S WITH KEY VBELV = WA_SERIAL-VGBEL
                                                              POSNV = WA_SERIAL-VGPOS
                                                      BINARY SEARCH.                "CH07++
        IF SY-SUBRC EQ 0.
          WA_ITAB-INV_NO = WA_CHECK_INV_S-VBELN.
        ENDIF.
        LV_CHECK = ''.
      ENDIF.

      IF LV_CHECK NE 'X'.
        WA_ITAB-LIEF_NR  = WA_SERIAL-VBELN.
        WA_ITAB-POSNR  = WA_SERIAL-POSNR.  "Add by Wantanee 20130218
        WA_ITAB-VBELN = WA_SERIAL-VGBEL.
        WA_ITAB-MATNR = WA_SERIAL-MATNR.

        WA_ITAB-KUNNR_SOLDTO = WA_SERIAL-SOLD_TO.
        WA_ITAB-KUNNR_SHIPTO = WA_SERIAL-SHIP_TO.
        WA_ITAB-CLASS = WA_SERIAL-PRODH+5(5).
        WA_ITAB-CATEGORY = WA_SERIAL-PRODH+0(5).
        WA_ITAB-ERDAT = WA_SERIAL-ERDAT.
        WA_ITAB-KODAT = WA_SERIAL-KODAT.
        WA_ITAB-LDDAT = WA_SERIAL-LDDAT.
        WA_ITAB-LFDAT = WA_SERIAL-LFDAT.
        WA_ITAB-WADAT = WA_SERIAL-WADAT.
        WA_ITAB-AUFNR = WA_SERIAL-AUFNR.
        WA_ITAB-ROUTE  = WA_SERIAL-ROUTE.
        WA_ITAB-BRGEW  = WA_SERIAL-BRGEW.
        WA_ITAB-GEWEI  = WA_SERIAL-GEWEI.
        WA_ITAB-VOLUM  = WA_SERIAL-VOLUM.
        WA_ITAB-VOLEH  = WA_SERIAL-VOLEH.
        WA_ITAB-SALE_CODE  = WA_SERIAL-SALE_CODE.
        WA_ITAB-AUART  = WA_SERIAL-AUART.
        WA_ITAB-LGORT  = WA_SERIAL-LGORT.
        WA_ITAB-ZZPOB  = WA_SERIAL-ZZPOB.

        WA_ITAB-REQ_REK = READ_TEXT( I_ID     = 'ZH10'
                                     I_OBJECT = 'VBBK'
                                     I_VBELN  = WA_ITAB-LIEF_NR ).

        WA_ITAB-INV_REK = READ_TEXT( I_ID     = 'ZH09'
                                     I_OBJECT = 'VBBK'
                                     I_VBELN  = WA_ITAB-LIEF_NR ).

        GET_CUSTOMER( EXPORTING I_KUNNR     = WA_SERIAL-SOLD_TO
                                I_ADRNR     = WA_SERIAL-ADRNR_SOLD_TO
                       CHANGING C_NAME_ENG  = WA_ITAB-NAME_SOLDTO
                                C_ADD1      = WA_ITAB-SOLDTO_ADD1
                                C_ADD2      = WA_ITAB-SOLDTO_ADD2
                                C_CITY2     = WA_ITAB-SOLDTO_CITY2
                                C_CITY1     = WA_ITAB-SOLDTO_LOC
                                C_POST_CODE = WA_ITAB-SOLDTO_POST_CODE ).

        GET_CUSTOMER( EXPORTING I_KUNNR     = WA_SERIAL-SHIP_TO
                                I_ADRNR     = WA_SERIAL-ADRNR_SHIP_TO
                       CHANGING C_NAME_ENG  = WA_ITAB-NAME_SHIPTO
                                C_ADD1      = WA_ITAB-SHIPTO_ADD1
                                C_ADD2      = WA_ITAB-SHIPTO_ADD2
                                C_CITY2     = WA_ITAB-SHIPTO_CITY2
                                C_CITY1     = WA_ITAB-SHIPTO_LOC
                                C_POST_CODE = WA_ITAB-SHIPTO_POST_CODE ).

        WA_ITAB-SALE_NAME = GET_SALEEMP( WA_SERIAL-SALE_CODE ).
        WA_ITAB-KTEXT    = GET_IO( WA_SERIAL-AUFNR ).

        WA_ITAB-LAND_REK = READ_TEXT( I_ID     = 'ZH11'
                                       I_OBJECT = 'VBBK'
                                       I_VBELN  = WA_ITAB-LIEF_NR ).

        WA_ITAB-PROJ_REK = READ_TEXT( I_ID     = 'ZH06'
                                      I_OBJECT = 'VBBK'
                                      I_VBELN  = WA_ITAB-LIEF_NR ).


*       WA_ITAB-BSTKD = GS_TMP-BSTKD.
*        PERFORM GET_IO USING WA_SERIAL-AUFNR
*                       CHANGING WA_ITAB-KTEXT.

        READ TABLE GT_TVKBT INTO WA_TVKBT WITH KEY VKBUR = WA_SERIAL-VKBUR.
        IF SY-SUBRC = 0.
          WA_ITAB-VKBUR_TXT = WA_TVKBT-BEZEI.
        ENDIF.
        READ TABLE GT_TVTWT INTO WA_TVTWT WITH KEY VTWEG = WA_SERIAL-VTWEG.
        IF SY-SUBRC = 0.
          WA_ITAB-VTWEG_TXT = WA_TVTWT-VTEXT.
        ENDIF.
        READ TABLE GT_TVLAT INTO WA_TVLAT WITH KEY LSTEL = WA_SERIAL-LSTEL.
        IF SY-SUBRC = 0.
          WA_ITAB-LSTEL_TXT  = WA_TVLAT-VTEXT.
        ENDIF.

        GET_SHIPMENT( EXPORTING I_VBELN = WA_SERIAL-VBELN
                       CHANGING C_TKNUM = WA_ITAB-TKNUM
                                C_EXTI1 = WA_ITAB-EXTI1
                                C_EXTI2 = WA_ITAB-EXTI2
                                C_VSART = WA_ITAB-VSART
                    ).

*        READ TABLE GT_SER01 TRANSPORTING NO FIELDS
*                            WITH KEY LIEF_NR = WA_SERIAL-VBELN
*                                     POSNR   = WA_SERIAL-POSNR
*                                     MATNR   = WA_SERIAL-MATNR
*                            BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          LOOP AT GT_SER01 INTO WA_SER01 FROM SY-TABIX.
*            IF WA_SER01-LIEF_NR <> WA_SERIAL-VBELN AND
*               WA_SER01-POSNR   <> WA_SERIAL-POSNR AND
*               WA_SER01-MATNR   <> WA_SERIAL-MATNR.
*              EXIT.
*            ENDIF.
*
*            WA_ITAB-SERNR = WA_SER01-SERNR.
*            WA_ITAB-DI_QTY  = 1.
*            WA_ITAB-PK_QTY  = 1.
*            LV_COUNTQTY = LV_COUNTQTY + 1.
*
*            WA_ITAB-KOSTK = 'C - Fully picked'.
*            IF S_KOSTK IS INITIAL OR S_KOSTK EQ 'C'.
*              READ TABLE GT_ITAB INTO GW_ITAB WITH KEY LIEF_NR = WA_ITAB-LIEF_NR
*                                                       POSNR =   WA_ITAB-POSNR
*                                                       VBELN =   WA_ITAB-VBELN
*                                                       MATNR =   WA_ITAB-MATNR
*                                                       SERNR =   WA_ITAB-SERNR.
*              IF SY-SUBRC NE 0 .
*                APPEND WA_ITAB TO GT_ITAB.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
        WA_ITAB-PS_PSP_PNR = WA_SERIAL-PS_PSP_PNR.
        WA_ITAB-POST1      = LCL_UTIL->GET_WBS_DESC( EXPORTING I_WBS = WA_SERIAL-PS_PSP_PNR ).

        LOOP AT GT_SER01 INTO WA_SER01 WHERE
                               LIEF_NR = WA_SERIAL-VBELN AND
                               POSNR   = WA_SERIAL-POSNR AND
                               MATNR   = WA_SERIAL-MATNR.


          WA_ITAB-SERNR = WA_SER01-SERNR.
          WA_ITAB-DI_QTY  = 1.
          WA_ITAB-PK_QTY  = 1.
          LV_COUNTQTY = LV_COUNTQTY + 1.

          WA_ITAB-KOSTK = 'C - Fully picked'.
          IF S_KOSTK IS INITIAL OR S_KOSTK EQ 'C'.
            READ TABLE GT_ITAB INTO GW_ITAB WITH KEY LIEF_NR = WA_ITAB-LIEF_NR
                                                     POSNR =   WA_ITAB-POSNR
                                                     VBELN =   WA_ITAB-VBELN
                                                     MATNR =   WA_ITAB-MATNR
                                                     SERNR =   WA_ITAB-SERNR.
            IF SY-SUBRC NE 0 .
              APPEND WA_ITAB TO GT_ITAB.
            ENDIF.
          ENDIF.
        ENDLOOP.



        IF WA_SER01 IS INITIAL.
          IF ( S_KOSTK IS INITIAL ) OR ( S_KOSTK EQ 'A' ).
            WA_ITAB-KOSTK = 'A - Not picked'.
            WA_ITAB-DI_QTY  = WA_SERIAL-LFIMG.
            WA_ITAB-PK_QTY  = WA_SERIAL-RFMNG.
            APPEND WA_ITAB TO GT_ITAB.

          ENDIF.

        ELSE.
          IF LV_COUNTQTY NE WA_SERIAL-LFIMG.
            IF ( S_KOSTK IS INITIAL ) OR ( S_KOSTK EQ 'A' ).
              WA_ITAB-SERNR = ''.
              WA_ITAB-KOSTK = 'A - Not picked'.
              WA_ITAB-DI_QTY  = WA_SERIAL-LFIMG - LV_COUNTQTY.
              WA_ITAB-PK_QTY  = WA_SERIAL-LFIMG - LV_COUNTQTY.
              WA_ITAB-VBTYP   = WA_SERIAL-VBTYP.
              APPEND WA_ITAB TO GT_ITAB.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.



    ENDLOOP.
*Insert data noserial
    SORT GT_NOSERIAL.
    DELETE ADJACENT DUPLICATES FROM GT_NOSERIAL.
    LOOP AT GT_NOSERIAL INTO WA_NOSERIAL WHERE VBTYP NE '7'.
      CLEAR: WA_ITAB,LV_CHECK.

      WA_ITAB-MAT_DOC_L = MAT_DOC_LASTED( WA_NOSERIAL-VBELN ).

      READ TABLE GT_NOSERIAL INTO WA_CHK_NSMAT_DOC WITH KEY VBELN = WA_NOSERIAL-VBELN
                                                         POSNR = WA_NOSERIAL-POSNV
                                                         VBTYP_N = 'R'.
      IF SY-SUBRC EQ 0.
        WA_ITAB-MAT_DOC = WA_CHK_NSMAT_DOC-VBELN_VF.
      ENDIF.


      IF R_WAIT EQ 'X'.
        READ TABLE GT_CHECK_INV_NS INTO WA_CHECK_INV_NS WITH KEY VBELV = WA_NOSERIAL-VGBEL
                                                                 POSNV = WA_NOSERIAL-VGPOS
                                                                 BINARY SEARCH.     "CH07++

        IF SY-SUBRC EQ 0.
          LV_CHECK = 'X'.
        ELSE.
          LV_CHECK = ''.
        ENDIF.
      ELSE.
        LV_CHECK = ''.
        READ TABLE GT_CHECK_INV_NS INTO WA_CHECK_INV_NS WITH KEY VBELV = WA_NOSERIAL-VGBEL
                                                                   POSNV = WA_NOSERIAL-VGPOS
                                                                   BINARY SEARCH.     "CH07++
        IF SY-SUBRC EQ 0.
          WA_ITAB-INV_NO = WA_CHECK_INV_NS-VBELN.
        ENDIF.
      ENDIF.

      IF LV_CHECK NE 'X'.
        WA_ITAB-LIEF_NR  = WA_NOSERIAL-VBELN.
        WA_ITAB-POSNR  = WA_NOSERIAL-POSNR.  "Add by Wantanee 20130218
        WA_ITAB-VBELN = WA_NOSERIAL-VGBEL.
        WA_ITAB-MATNR = WA_NOSERIAL-MATNR.
        WA_ITAB-SERNR = ''.
        WA_ITAB-DI_QTY  = WA_NOSERIAL-LFIMG.
        WA_ITAB-PK_QTY  = WA_NOSERIAL-RFMNG.
        WA_ITAB-KUNNR_SOLDTO = WA_NOSERIAL-SOLD_TO.
        WA_ITAB-KUNNR_SHIPTO = WA_NOSERIAL-SHIP_TO.
        WA_ITAB-CLASS = WA_NOSERIAL-PRODH+5(5).
        WA_ITAB-CATEGORY = WA_NOSERIAL-PRODH+0(5).
        WA_ITAB-ERDAT = WA_NOSERIAL-ERDAT.
        WA_ITAB-KODAT = WA_NOSERIAL-KODAT.
        WA_ITAB-LDDAT = WA_NOSERIAL-LDDAT.
        WA_ITAB-LFDAT = WA_NOSERIAL-LFDAT.
        WA_ITAB-WADAT = WA_NOSERIAL-WADAT.
        WA_ITAB-AUFNR = WA_NOSERIAL-AUFNR.
        WA_ITAB-ROUTE  = WA_NOSERIAL-ROUTE.
        WA_ITAB-BRGEW  = WA_NOSERIAL-BRGEW.
        WA_ITAB-GEWEI  = WA_NOSERIAL-GEWEI.
        WA_ITAB-VOLUM  = WA_NOSERIAL-VOLUM.
        WA_ITAB-VOLEH  = WA_NOSERIAL-VOLEH.
        WA_ITAB-SALE_CODE  = WA_NOSERIAL-SALE_CODE.
        WA_ITAB-AUART  =  WA_NOSERIAL-AUART.
        WA_ITAB-LGORT  =  WA_NOSERIAL-LGORT.
        WA_ITAB-ZZPOB  =  WA_NOSERIAL-ZZPOB.

        WA_ITAB-REQ_REK = READ_TEXT( I_ID     = 'ZH10'
                                     I_OBJECT = 'VBBK'
                                     I_VBELN  = WA_ITAB-LIEF_NR ).

        WA_ITAB-INV_REK = READ_TEXT( I_ID     = 'ZH09'
                                     I_OBJECT = 'VBBK'
                                     I_VBELN  = WA_ITAB-LIEF_NR ).

        GET_CUSTOMER( EXPORTING I_KUNNR     = WA_NOSERIAL-SOLD_TO
                                I_ADRNR     = WA_NOSERIAL-ADRNR_SOLD_TO
                       CHANGING C_NAME_ENG  = WA_ITAB-NAME_SOLDTO
                                C_ADD1      = WA_ITAB-SOLDTO_ADD1
                                C_ADD2      = WA_ITAB-SOLDTO_ADD2
                                C_CITY2     = WA_ITAB-SOLDTO_CITY2
                                C_CITY1     = WA_ITAB-SOLDTO_LOC
                                C_POST_CODE = WA_ITAB-SOLDTO_POST_CODE ).

        GET_CUSTOMER( EXPORTING I_KUNNR     = WA_NOSERIAL-SHIP_TO
                                I_ADRNR     = WA_NOSERIAL-ADRNR_SHIP_TO
                       CHANGING C_NAME_ENG  = WA_ITAB-NAME_SHIPTO
                                C_ADD1      = WA_ITAB-SHIPTO_ADD1
                                C_ADD2      = WA_ITAB-SHIPTO_ADD2
                                C_CITY2     = WA_ITAB-SHIPTO_CITY2
                                C_CITY1     = WA_ITAB-SHIPTO_LOC
                                C_POST_CODE = WA_ITAB-SHIPTO_POST_CODE ).

        WA_ITAB-SALE_NAME = GET_SALEEMP( WA_NOSERIAL-SALE_CODE ).
        WA_ITAB-KTEXT    = GET_IO( WA_NOSERIAL-AUFNR ).

        WA_ITAB-PS_PSP_PNR = WA_NOSERIAL-PS_PSP_PNR.
        WA_ITAB-POST1      = LCL_UTIL->GET_WBS_DESC( EXPORTING I_WBS = WA_NOSERIAL-PS_PSP_PNR ).

        WA_ITAB-LAND_REK = READ_TEXT( I_ID     = 'ZH11'
                                     I_OBJECT = 'VBBK'
                                     I_VBELN  = WA_ITAB-LIEF_NR ).

        WA_ITAB-PROJ_REK = READ_TEXT( I_ID     = 'ZH06'
                                     I_OBJECT = 'VBBK'
                                     I_VBELN  = WA_ITAB-LIEF_NR ).


        READ TABLE GT_TVKBT INTO WA_TVKBT WITH KEY VKBUR = WA_NOSERIAL-VKBUR.
        IF SY-SUBRC = 0.
          WA_ITAB-VKBUR_TXT = WA_TVKBT-BEZEI.
        ENDIF.
        READ TABLE GT_TVTWT INTO WA_TVTWT WITH KEY VTWEG = WA_NOSERIAL-VTWEG.
        IF SY-SUBRC = 0.
          WA_ITAB-VTWEG_TXT = WA_TVTWT-VTEXT.
        ENDIF.
        READ TABLE GT_TVLAT INTO WA_TVLAT WITH KEY LSTEL = WA_NOSERIAL-LSTEL.
        IF SY-SUBRC = 0.
          WA_ITAB-LSTEL_TXT  = WA_TVLAT-VTEXT.
        ENDIF.
        READ TABLE GT_TVBST INTO WA_TVBST WITH KEY STATU = WA_NOSERIAL-KOSTK.
        IF SY-SUBRC = 0.
          CONCATENATE WA_NOSERIAL-KOSTK WA_TVBST-BEZEI INTO WA_ITAB-KOSTK SEPARATED BY ' - '.

        ENDIF.
        GET_SHIPMENT( EXPORTING I_VBELN = WA_NOSERIAL-VBELN
                       CHANGING C_TKNUM = WA_ITAB-TKNUM
                                C_EXTI1 = WA_ITAB-EXTI1
                                C_EXTI2 = WA_ITAB-EXTI2
                                C_VSART = WA_ITAB-VSART
                    ).

        IF ( S_KOSTK IS INITIAL ) OR ( S_KOSTK EQ 'C' ).
          WA_ITAB-VBTYP = WA_NOSERIAL-VBTYP.
          APPEND WA_ITAB TO GT_ITAB.
        ENDIF.
      ENDIF.
    ENDLOOP.
*
*    SORT GT_ITAB BY VBTYP.
*    DELETE GT_ITAB WHERE VBTYP EQ '7'.
  ENDMETHOD.
  METHOD READ_TEXT.
    DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
    DATA: WA_LINES LIKE LINE OF IT_LINES.
    DATA: V_NAME TYPE THEAD-TDNAME.

    CLEAR:  IT_LINES[].

    V_NAME = I_VBELN.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = I_ID
        LANGUAGE                = SY-LANGU
        NAME                    = V_NAME
        OBJECT                  = I_OBJECT
      TABLES
        LINES                   = IT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    IF SY-SUBRC = 0.
      LOOP AT IT_LINES INTO WA_LINES.
        CONCATENATE R  WA_LINES-TDLINE INTO R SEPARATED BY SPACE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD GET_CUSTOMER.
    DATA: P_NAME_THA(200) TYPE C.
*    CLEAR:P_NAME_THA,P_NAME_ENG,P_ADD1,P_ADD2,P_CITY2,P_CITY1,P_POST_CODE.


    SELECT ADDRNUMBER NAME1 NAME2 NATION
           STREET CITY1 STR_SUPPL3 LOCATION
           CITY2  POST_CODE1
    INTO TABLE GT_ADRC
    FROM ADRC
    WHERE ADDRNUMBER = I_ADRNR.
*           AND nation = 'I'.
    IF I_KUNNR NE 'OT01'.
      READ TABLE GT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = I_ADRNR
                                               NATION = 'I'.
      IF SY-SUBRC EQ 0.
        CONCATENATE WA_ADRC-NAME1 WA_ADRC-NAME2 INTO C_NAME_ENG.
      ENDIF.
    ELSE.
      READ TABLE GT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = I_ADRNR
                                              NATION = ''.
      IF SY-SUBRC EQ 0.
        CONCATENATE WA_ADRC-NAME1 WA_ADRC-NAME2 INTO C_NAME_ENG.

      ENDIF.
    ENDIF.
    READ TABLE GT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = I_ADRNR
                                             NATION = ''.
    IF SY-SUBRC EQ 0.

      C_ADD1 = WA_ADRC-STREET.
      CONCATENATE WA_ADRC-STR_SUPPL3 WA_ADRC-LOCATION INTO C_ADD2.
      C_CITY2 = WA_ADRC-CITY2.
      C_CITY1 = WA_ADRC-CITY1.
      C_POST_CODE = WA_ADRC-POST_CODE1.

    ENDIF.

  ENDMETHOD.
  METHOD GET_SALEEMP.
    DATA: LV_NAME    TYPE PA0001-ENAME,
          LV_SERNAME TYPE  PA0001-ENAME.
    CLEAR: GT_PA0001,LV_NAME.

    SELECT SINGLE ENAME
    INTO (LV_NAME)
    FROM PA0001
    WHERE PERNR EQ I_PERNR.

    SPLIT LV_NAME AT SPACE INTO:  LV_SERNAME LV_NAME .
    CONCATENATE LV_NAME LV_SERNAME INTO R SEPARATED BY SPACE.

  ENDMETHOD.
  METHOD GET_IO.
    SELECT SINGLE KTEXT
      INTO (R)
      FROM AUFK
      WHERE AUFNR EQ I_AUFNR.
  ENDMETHOD.
  METHOD GET_SHIPMENT.
    LOOP AT GT_SHIPMENT INTO WA_SHIPMENT WHERE VBELN = I_VBELN.
      IF  C_TKNUM EQ ''.
        C_TKNUM = WA_SHIPMENT-TKNUM.
      ELSE.
        CONCATENATE C_TKNUM WA_SHIPMENT-TKNUM INTO C_TKNUM SEPARATED BY ','.
      ENDIF.

      IF  C_EXTI1 EQ ''.
        C_EXTI1 = WA_SHIPMENT-EXTI1.
      ELSE.
        CONCATENATE C_EXTI1 WA_SHIPMENT-EXTI1 INTO C_EXTI1 SEPARATED BY ','.

      ENDIF.

      IF  C_EXTI2 EQ ''.
        C_EXTI2 = WA_SHIPMENT-EXTI2.
      ELSE.
        CONCATENATE C_EXTI2 WA_SHIPMENT-EXTI2 INTO C_EXTI2 SEPARATED BY ','.

      ENDIF.

      C_VSART = WA_SHIPMENT-VSART.
    ENDLOOP.

  ENDMETHOD.
  METHOD MAT_DOC_LASTED.
    CONSTANTS : BEGIN OF LC_VBTYP_N,
                  R TYPE VBFA-VBTYP_N VALUE 'R',
                  H TYPE VBFA-VBTYP_N VALUE 'h',
                END OF LC_VBTYP_N.

    DATA : WA_GET_MAT_DOC  TYPE TYP_GET_MAT_DOC.

    SORT GT_GET_MAT_DOC BY VBELN.

    LOOP AT GT_GET_MAT_DOC INTO WA_GET_MAT_DOC WHERE VBELV = I_VBELN
                                                 AND ( VBTYP_N = LC_VBTYP_N-R OR VBTYP_N = LC_VBTYP_N-H )
                                                 AND BWART IS NOT INITIAL.
      IF WA_GET_MAT_DOC-VBTYP_N = LC_VBTYP_N-H.
        CLEAR R.
      ELSE.
        R = WA_GET_MAT_DOC-VBELN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD APPEND_FIELDCAT.
    DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
          V_COLTXT_LENGTH TYPE I.

    ADD 1 TO V_POS.

    WA_INFIELDCAT-FIELDNAME      = I_FIELD.
    WA_INFIELDCAT-REF_TABNAME    = I_REFTABLE.
    WA_INFIELDCAT-REF_FIELDNAME  = I_REFFIELD.
    WA_INFIELDCAT-COL_POS        = V_POS.
    WA_INFIELDCAT-DO_SUM         = I_DOSUM.
    IF NOT I_NO_ZERO IS INITIAL.
      WA_INFIELDCAT-NO_ZERO = I_NO_ZERO.
    ENDIF.
    IF NOT I_CFIELDNAME IS INITIAL.
      WA_INFIELDCAT-CFIELDNAME = I_CFIELDNAME.
    ENDIF.
*
    IF     I_COLOR = '1'.
      WA_INFIELDCAT-EMPHASIZE = 'C110'.
    ELSEIF I_COLOR = '2' .
      WA_INFIELDCAT-EMPHASIZE = 'C310'.
    ELSEIF I_COLOR = '3' .
      WA_INFIELDCAT-EMPHASIZE = 'C510'.
    ELSEIF I_COLOR = '4' .
      WA_INFIELDCAT-EMPHASIZE = 'C710'.
    ELSEIF I_COLOR = '5' .
      WA_INFIELDCAT-EMPHASIZE = 'C200'.
    ELSEIF I_COLOR = '6' .
      WA_INFIELDCAT-EMPHASIZE = 'C400'.
    ELSEIF I_COLOR = '7' .
      WA_INFIELDCAT-EMPHASIZE = 'C500'.
    ENDIF.

    IF NOT I_COLTXT IS INITIAL.
      V_COLTXT_LENGTH = STRLEN( I_COLTXT ).

      IF V_COLTXT_LENGTH > 20.
        WA_INFIELDCAT-DDICTXT   = 'L'. "Long text
        WA_INFIELDCAT-SELTEXT_L = I_COLTXT.
      ELSEIF V_COLTXT_LENGTH > 10.
        WA_INFIELDCAT-DDICTXT   = 'M'. "Medium Text
        WA_INFIELDCAT-SELTEXT_M = I_COLTXT.
      ELSE.
        WA_INFIELDCAT-DDICTXT   = 'S'. "Short Text
        WA_INFIELDCAT-SELTEXT_S = I_COLTXT.
      ENDIF.
      WA_INFIELDCAT-REPTEXT_DDIC = I_COLTXT.
    ENDIF.
    APPEND WA_INFIELDCAT TO GT_FCAT[].

  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS DEFINITION.
*Handling double click
  PUBLIC SECTION.
    METHODS:
    HANDLE_DOUBLE_CLICK
    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS. "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD. "handle_double_click
ENDCLASS. "lcl_event_receiver IMPLEMENTATION
