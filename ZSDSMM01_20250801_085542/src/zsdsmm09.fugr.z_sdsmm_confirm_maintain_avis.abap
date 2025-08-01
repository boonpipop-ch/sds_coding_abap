FUNCTION Z_SDSMM_CONFIRM_MAINTAIN_AVIS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(T_EKES) TYPE  MMPR_UEKES OPTIONAL
*"----------------------------------------------------------------------
 DATA: it_ekes  TYPE TABLE OF ekes WITH HEADER LINE,
        l_paqty  TYPE ekes-dabmg.

 DATA: gy_ekes TYPE TABLE OF ekes WITH HEADER LINE.
 DATA: lt_ekes1 TYPE TABLE OF UEKES WITH HEADER LINE.

  SORT t_ekes BY ebeln ebelp.

   lt_ekes1[] = T_EKES  .

  LOOP AT lt_ekes1.

    AT END OF ebelp.

      CLEAR: l_paqty, it_ekes.
      REFRESH it_ekes.

      SELECT SUM( menge ) INTO l_paqty
        FROM ekes
*        WHERE ebeln = t_ekes-ebeln
*        AND   ebelp = t_ekes-ebelp
        WHERE ebeln = lt_ekes1-ebeln
        AND   ebelp = lt_ekes1-ebelp
        AND   ebtyp = 'PA'.

      CHECK l_paqty > 0.

      SELECT * FROM ekes
        INTO CORRESPONDING FIELDS OF TABLE it_ekes
*        WHERE ebeln  = t_ekes-ebeln
*        AND   ebelp  = t_ekes-ebelp
        WHERE ebeln  = lt_ekes1-ebeln
        AND   ebelp  = lt_ekes1-ebelp

        AND   ebtyp  = 'AB'
        ORDER BY PRIMARY KEY.

      LOOP AT it_ekes.
        IF it_ekes-menge <= l_paqty.
          it_ekes-dabmg = it_ekes-menge.
          l_paqty = l_paqty - it_ekes-menge.
        ELSE.
          it_ekes-dabmg = l_paqty.
          l_paqty = 0.
        ENDIF.

        UPDATE ekes FROM it_ekes.

        IF l_paqty <= 0.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDAT.
  ENDLOOP.




ENDFUNCTION.
