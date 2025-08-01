*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0090
*  Creation Date      : 21.03.2024
*  Author             : B Chiewsarikij
*  Add-on ID          : ZFIAPE003
*  Description        : Print cheque
*  Purpose            : N/A
*  Copied from        : ZR_FI_PAYMENT_CHEQUE (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*&---------------------------------------------------------------------*
*& Report ZSDSFIR0090
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdsfir0090.
TYPE-POOLS: slis.
INCLUDE: <list>.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: payr,toa_dara,arc_params.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*TYPES:

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA: BEGIN OF tb_payr OCCURS 0,
        zbukr     TYPE  payr-zbukr,
        hbkid     TYPE  payr-hbkid,
        hktid     TYPE  payr-hktid,
        chect     TYPE  payr-chect,
        vblnr     TYPE  payr-vblnr,
        gjahr     TYPE  payr-gjahr,
        zaldt     TYPE  payr-zaldt,
        waers     TYPE  payr-waers,
        rwbtr     TYPE  payr-rwbtr,
        znme1     TYPE  payr-znme1,
        prius     TYPE  payr-prius,
        voidd     TYPE  payr-voidd,
        sgtxt(35) TYPE  c,
        lotno     TYPE  pcec-stapl,
      END OF tb_payr.

DATA: BEGIN OF tb_bsak OCCURS 0,
        augbl TYPE  bsak-augbl,
        gjahr TYPE  bsak-gjahr,
        sgtxt TYPE  bsak-sgtxt,
      END OF tb_bsak.


DATA: BEGIN OF tb_pcec OCCURS 0,
        zbukr   TYPE  pcec-zbukr,
        hbkid   TYPE  pcec-hbkid,
        hktid   TYPE  pcec-hktid,
        stapl   TYPE  pcec-stapl,
        checf   TYPE  pcec-checf,
        chect   TYPE  pcec-chect,
        fstap   TYPE  pcec-fstap,
        checl   TYPE  pcec-checl,
        count   TYPE  i,
        maxch   TYPE  char8, "pcec-chect,
        minch   TYPE  char8, "pcec-chect,
        counton TYPE  i,
        maxon   TYPE  char8, "pcec-chect,
        minon   TYPE  char8, "pcec-chect,
      END OF tb_pcec,

      tb_pcec_pos LIKE tb_pcec OCCURS 0,
      tb_payr1    LIKE tb_payr OCCURS 0,
      tb_payr2    LIKE tb_payr OCCURS 0,
      tb_payr_pos LIKE tb_payr OCCURS 0,
      wa_payr     LIKE tb_payr,
      hd_payr     LIKE tb_payr,
      wa_payr2    LIKE tb_payr,
      wa_payr_pos LIKE tb_payr,
      wa_pcec     LIKE tb_pcec,
      wa_pcec_pos LIKE tb_pcec,
      wa_bsak     LIKE tb_bsak.


DATA: it_field_cat TYPE slis_t_fieldcat_alv,
      wa_field_cat TYPE slis_fieldcat_alv,
      it_events    TYPE slis_t_event,
      wa_events    TYPE slis_alv_event,
      it_header    TYPE slis_t_listheader,
      wa_header    TYPE slis_listheader,
      it_layout    TYPE slis_layout_alv,
      it_sort      TYPE slis_t_sortinfo_alv,
      wa_sort      TYPE slis_sortinfo_alv.

DATA: v_pos  TYPE i.
DATA: sf_fm_name             TYPE rs38l_fnam,
      gwa_control_parameters TYPE ssfctrlop,
      gwa_output_options     TYPE ssfcompop.

DATA: gv_formname  TYPE tdsfname,
      gv_fm_name   TYPE rs38l_fnam,
      gv_tabix     TYPE syst-tabix,
      gv_last_item TYPE syst-tabix.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
*CONSTANTS:

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECT-OPTIONS: s_zbukr FOR payr-zbukr DEFAULT '1000' OBLIGATORY,
                s_gjahr FOR payr-gjahr   DEFAULT sy-datum MODIF ID prc, "Add by Wantanee 20140709
                s_hbkid FOR payr-hbkid,
                s_hktid FOR payr-hktid,
                s_chect FOR payr-chect,
                s_ladat FOR payr-zaldt MODIF ID pos,
                s_laufd FOR payr-laufd MODIF ID cre,
                s_prius FOR payr-prius.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
* Add
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 01(20) TEXT-003 MODIF ID rad.
    SELECTION-SCREEN POSITION 30.
    PARAMETERS: p_pos RADIOBUTTON GROUP rad1 USER-COMMAND ucomm
                MODIF ID rad  DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
* Print
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 01(20) TEXT-004 MODIF ID rad.
    SELECTION-SCREEN POSITION 30.
    PARAMETERS: p_cre RADIOBUTTON GROUP rad1 MODIF ID rad.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  IF NOT p_pos IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'POS'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 EQ 'CRE'.
        screen-active = 0.
        MODIFY SCREEN.
        FREE: s_laufd.
        CLEAR: s_laufd.
      ENDIF.
    ENDLOOP.
  ELSEIF NOT p_cre IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'CRE'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 EQ 'POS'.
        screen-active = 0.
        MODIFY SCREEN.
        FREE: s_ladat.
        CLEAR: s_ladat.
      ENDIF.
    ENDLOOP.
  ENDIF.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM: get_data,
           match_data,
           print_report.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT a~zbukr a~hbkid a~hktid a~chect a~vblnr a~gjahr a~zaldt a~waers a~rwbtr a~znme1 a~prius a~voidd "b~sgtxt
    INTO TABLE tb_payr
    FROM payr AS a "inner join bsak as b on ( a~vblnr = b~augbl and
*                                             a~gjahr = b~gjahr )
    WHERE a~zbukr IN s_zbukr AND
          a~gjahr IN s_gjahr AND "Add by Wantanee 20140709
          a~hbkid IN s_hbkid AND
          a~hktid IN s_hktid AND
          a~chect IN s_chect AND
          a~zaldt IN s_ladat AND
          a~laufd IN s_laufd AND
          a~prius IN s_prius

*          b~SHKZG = 'H'
    ORDER BY a~hbkid a~hktid a~chect.

  SELECT augbl gjahr sgtxt INTO TABLE tb_bsak FROM bsak
    FOR ALL ENTRIES IN tb_payr
    WHERE augbl = tb_payr-vblnr AND
          gjahr = tb_payr-gjahr AND
          shkzg = 'H' AND
          sgtxt <> space.

  SORT tb_payr BY hbkid hktid chect sgtxt.
  DELETE ADJACENT DUPLICATES FROM tb_payr COMPARING ALL FIELDS.


  SELECT zbukr hbkid hktid stapl checf chect fstap checl INTO TABLE tb_pcec
    FROM pcec FOR ALL ENTRIES IN tb_payr
    WHERE zbukr EQ tb_payr-zbukr AND
          hbkid EQ tb_payr-hbkid AND
          hktid EQ tb_payr-hktid AND
          checf LE tb_payr-chect AND
          chect GE tb_payr-chect.

ENDFORM. " get_data
*&---------------------------------------------------------------------*
*& Form build_field_cat
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM build_field_cat .
  CLEAR wa_field_cat.

  PERFORM append_fieldcat USING 'ZBUKR' space space 'CoCD'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'HBKID' space space 'House Bk'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'HKTID' space space 'Acct ID'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'CHECT' space space 'Check number'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'VBLNR' space space 'Payment'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'ZALDT' space space 'Payment Date'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'WAERS' space space 'CrCy'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'RWBTR' space space 'Amount paid(FC)'
                                'X' space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'ZNME1' space space 'Name of the payee'
                                space  space  space space it_field_cat[].
  PERFORM append_fieldcat USING 'PRIUS' space space 'Prepared By'
                                space  space  space space it_field_cat[].

ENDFORM. " build_field_cat

*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM display_data .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = it_field_cat
*     it_events          = it_events
      it_sort            = it_sort
      is_layout          = it_layout
    TABLES
      t_outtab           = tb_payr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_events
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM get_events .

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      et_events = it_events.

  READ TABLE it_events INTO wa_events
  WITH KEY name = slis_ev_top_of_page.
  IF sy-subrc = 0.
    wa_events-form = 'TOP_OF_PAGE'.
    MODIFY it_events FROM wa_events INDEX sy-tabix.
  ENDIF.

ENDFORM. " get_events

*&---------------------------------------------------------------------*
*& Form top_of_page
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM top_of_page.

  wa_header-typ = 'H'.
  wa_header-info = 'Material Data'.
  APPEND wa_header TO it_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header.

ENDFORM. "top_of_page
*&---------------------------------------------------------------------*
*&      Form  MATCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM match_data .
  DATA: min_num   LIKE wa_pcec-minon,
        number1   TYPE i,
        number2   TYPE i,
        number3   TYPE i,
        nummax    TYPE i,
        a_count   TYPE i,
        b_count   TYPE i,
        num_chect TYPE i,
        num_minch TYPE i,
        num_maxch TYPE i.

  IF p_cre IS NOT INITIAL.
    LOOP AT tb_payr INTO wa_payr.
      wa_payr-rwbtr = abs( wa_payr-rwbtr ).
      LOOP AT tb_pcec INTO wa_pcec.
        CLEAR: a_count, b_count, number1, number2, number3, nummax.
        a_count = strlen( wa_payr-chect ).
        b_count = strlen( wa_pcec-chect ).
        number1 = wa_payr-chect.
        number2 = wa_pcec-checf.
        number3 = wa_pcec-chect.
        IF number2 LE number1 AND
           number3 GE number1 AND
           a_count EQ b_count.
          MOVE: wa_pcec-stapl TO wa_payr-lotno.
          wa_pcec-count = wa_pcec-count + 1.

          num_minch = wa_pcec-minch.
          num_maxch = wa_pcec-maxch.
          num_chect = wa_payr-chect.

          IF number1 > num_maxch.
            wa_pcec-maxch = number1.
          ENDIF.

          IF wa_pcec-minch IS INITIAL.
            wa_pcec-minch = number1.
          ELSEIF num_chect < num_minch.
            wa_pcec-minch = number1.
          ENDIF.

          nummax = wa_pcec-maxch.
          IF nummax NE wa_pcec-chect.
            wa_pcec-minon = wa_pcec-maxch + 1.
          ELSEIF nummax EQ wa_pcec-chect.
            wa_pcec-minon = wa_pcec-maxch.
          ENDIF.
          wa_pcec-maxon = wa_pcec-chect.
          wa_pcec-counton = wa_pcec-chect - wa_pcec-maxch.
          MODIFY tb_pcec FROM wa_pcec INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      READ TABLE tb_bsak INTO wa_bsak WITH KEY augbl = wa_payr-vblnr
                                               gjahr = wa_payr-gjahr.
      IF sy-subrc EQ 0.
        wa_payr-sgtxt = wa_bsak-sgtxt.
      ENDIF.

      IF wa_payr-voidd IS NOT INITIAL.
        CLEAR: wa_payr-rwbtr.
      ENDIF.

      MODIFY tb_payr FROM wa_payr." index sy-tabix.
    ENDLOOP.

  ELSEIF p_pos IS NOT INITIAL.
    PERFORM posting_onhand.
    LOOP AT tb_payr INTO wa_payr.
      wa_payr-rwbtr = abs( wa_payr-rwbtr ).
      LOOP AT tb_pcec INTO wa_pcec.
        CLEAR: a_count, b_count, number1, number2, number3, nummax.
        a_count = strlen( wa_payr-chect ).
        b_count = strlen( wa_pcec-chect ).
        number1 = wa_payr-chect.
        number2 = wa_pcec-checf.
        number3 = wa_pcec-chect.
        IF number2 LE number1 AND
           number3 GE number1 AND
           a_count EQ b_count.
          MOVE: wa_pcec-stapl TO wa_payr-lotno.
          wa_pcec-count = wa_pcec-count + 1.
          IF number1 > wa_pcec-maxch.
            wa_pcec-maxch = number1.
          ENDIF.
          IF wa_pcec-minch IS INITIAL.
            wa_pcec-minch = number1.
          ELSEIF wa_payr-chect < wa_pcec-minch.
            wa_pcec-minch = number1.
          ENDIF.
          MODIFY tb_pcec FROM wa_pcec INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      READ TABLE tb_bsak INTO wa_bsak WITH KEY augbl = wa_payr-vblnr
                                               gjahr = wa_payr-gjahr.
      IF sy-subrc EQ 0.
        wa_payr-sgtxt = wa_bsak-sgtxt.
      ENDIF.

      IF wa_payr-voidd IS NOT INITIAL.
        CLEAR: wa_payr-rwbtr.
      ENDIF.

      MODIFY tb_payr FROM wa_payr." index sy-tabix.
    ENDLOOP.

    LOOP AT tb_payr_pos INTO wa_payr.
      wa_payr-rwbtr = abs( wa_payr-rwbtr ).
      LOOP AT tb_pcec INTO wa_pcec.
        CLEAR: a_count, b_count, number1, number2, number3, nummax.
        a_count = strlen( wa_payr-chect ).
        b_count = strlen( wa_pcec-chect ).
        number1 = wa_payr-chect.
        number2 = wa_pcec-checf.
        number3 = wa_pcec-chect.
        IF number2 LE number1 AND
           number3 GE number1 AND
           a_count EQ b_count.
          MOVE: wa_pcec-stapl TO wa_payr-lotno.
*        wa_pcec-counton = wa_pcec-count + 1.
          IF number1 > wa_pcec-maxon.
            wa_pcec-maxon = number1.
          ENDIF.

          nummax = wa_pcec-maxch.
          IF nummax NE wa_pcec-chect.
            wa_pcec-minon = wa_pcec-maxch + 1.
          ELSEIF nummax EQ wa_pcec-chect.
            wa_pcec-minon = wa_pcec-maxch.
          ENDIF.
          wa_pcec-maxon = wa_pcec-chect.
          wa_pcec-counton = wa_pcec-chect - wa_pcec-maxch.
          MODIFY tb_pcec FROM wa_pcec.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    DELETE tb_pcec WHERE count EQ 0.
  ENDIF.
ENDFORM.                    " MATCH_DATA

FORM append_fieldcat  USING  p_field      "Field name
                             p_reftable   "Reference Table name
                             p_reffield   "Reference Field name
                             p_coltxt     "Col Text(for specify)
                             p_dosum      "Sum total
                             p_cfieldname "currency
                             p_no_zero    "no zero
                             p_color      "color
                             p_it_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: wa_infieldcat   TYPE slis_fieldcat_alv,
        v_coltxt_length TYPE i.

  ADD 1 TO v_pos.

  wa_infieldcat-fieldname      = p_field.
  wa_infieldcat-ref_tabname    = p_reftable.
  wa_infieldcat-ref_fieldname  = p_reffield.
  wa_infieldcat-col_pos        = v_pos.
  wa_infieldcat-do_sum         = p_dosum.

  IF NOT p_no_zero IS INITIAL.
    wa_infieldcat-no_zero = p_no_zero.
  ENDIF.

  IF NOT p_cfieldname IS INITIAL.
    wa_infieldcat-cfieldname = p_cfieldname.
  ENDIF.
*
  IF p_color = '1'.
    wa_infieldcat-emphasize = 'C110'.
  ELSEIF p_color = '2' .
    wa_infieldcat-emphasize = 'C310'.
  ELSEIF p_color = '3' .
    wa_infieldcat-emphasize = 'C510'.
  ELSEIF p_color = '4' .
    wa_infieldcat-emphasize = 'C710'.
  ELSEIF p_color = '5' .
    wa_infieldcat-emphasize = 'C200'.
  ELSEIF p_color = '6' .
    wa_infieldcat-emphasize = 'C400'.
  ELSEIF p_color = '7' .
    wa_infieldcat-emphasize = 'C500'.
  ENDIF.

  IF NOT p_coltxt IS INITIAL.
    v_coltxt_length = strlen( p_coltxt ).

    IF v_coltxt_length > 20.
      wa_infieldcat-ddictxt   = 'L'. "long text
      wa_infieldcat-seltext_l = p_coltxt.
    ELSEIF v_coltxt_length > 10.
      wa_infieldcat-ddictxt   = 'M'. "medium text
      wa_infieldcat-seltext_m = p_coltxt.
    ELSE.
      wa_infieldcat-ddictxt   = 'S'. "short text
      wa_infieldcat-seltext_s = p_coltxt.
    ENDIF.

    wa_infieldcat-reptext_ddic = p_coltxt.
  ENDIF.
  APPEND wa_infieldcat TO p_it_fieldcat.

ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LAYOUT  text
*----------------------------------------------------------------------*
FORM build_layout  USING p_gtyp_layout TYPE slis_layout_alv.
  p_gtyp_layout-zebra = 'X'.
  p_gtyp_layout-colwidth_optimize = 'X'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort .
  wa_sort-spos = space.
  wa_sort-fieldname = 'ZALDT'.
  wa_sort-tabname = space.
  wa_sort-up = space.
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort .
  CLEAR wa_sort.
ENDFORM.                    " BUILD_SORT

FORM print_form .

  CONSTANTS: lc_formname TYPE TDSFNAME VALUE 'ZSDSFI004'.

  DATA: lv_option  TYPE ssfcompop,
        lv_control TYPE ssfctrlop.
  DATA: gwa_first  LIKE tb_payr.

  CLEAR: gwa_control_parameters.
  gwa_control_parameters-no_dialog = ' '.
  gwa_control_parameters-preview = 'X'.
  gwa_control_parameters-no_open = ' '.
  gwa_control_parameters-no_close = ' '.

  CLEAR: gwa_output_options.
  gwa_output_options-tddest        = 'LOCL'.
  gwa_output_options-tdnewid       = 'X'.
  gwa_output_options-tdimmed       = 'X'.

  CLEAR: gv_tabix, gv_last_item.
  DESCRIBE TABLE tb_payr1 LINES gv_last_item.
  READ TABLE tb_payr1 INDEX 1 INTO gwa_first.

  LOOP AT tb_payr1 INTO hd_payr.
    gv_tabix = sy-tabix.
    IF gv_tabix EQ gv_last_item.
      IF hd_payr-hbkid NE gwa_first-hbkid.
        gwa_control_parameters-no_open = 'X'.
        gwa_control_parameters-no_close = ' '.
      ELSE.
        IF gv_tabix EQ gv_last_item.
          gwa_control_parameters-no_close = ' '.
        ELSE.
          gwa_control_parameters-no_close = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      IF gv_tabix NE 1.
        gwa_control_parameters-no_open = 'X'.
        gwa_control_parameters-no_close = 'X'.
      ELSE.
        gwa_control_parameters-no_close = 'X'.
      ENDIF.
    ENDIF.


    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = lc_formname
      IMPORTING
        fm_name            = sf_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

* Function module /1BCDWB/SF00000182
    CALL FUNCTION sf_fm_name
      EXPORTING
        archive_index      = toa_dara
        archive_parameters = arc_params
        control_parameters = gwa_control_parameters
        output_options     = gwa_output_options
        user_settings      = ' '
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM init_data_frm TABLES gt_payr gt_pcec
                   CHANGING low high wb_payr txthdd01 txthdd02 txthdd03
                            txthdd04 txthdd05 txthdd06 txthdd07.
  LOOP AT tb_pcec INTO wa_pcec.
    DATA: b_format TYPE char8.
    MOVE: wa_pcec-minch TO b_format.
    CONDENSE b_format.
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        input  = b_format
      IMPORTING
        output = b_format.
    MOVE: b_format TO wa_pcec-minch.

    MOVE: wa_pcec-maxch TO b_format.
    CONDENSE b_format.
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        input  = b_format
      IMPORTING
        output = b_format.
    MOVE: b_format TO wa_pcec-maxch.

    MOVE: wa_pcec-minon TO b_format.
    CONDENSE b_format.
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        input  = b_format
      IMPORTING
        output = b_format.
    MOVE: b_format TO wa_pcec-minon.

    MOVE: wa_pcec-maxon TO b_format.
    CONDENSE b_format.
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        input  = b_format
      IMPORTING
        output = b_format.
    MOVE: b_format TO wa_pcec-maxon.
    MODIFY tb_pcec FROM wa_pcec INDEX sy-tabix.
  ENDLOOP.

  SORT tb_payr BY chect vblnr.
  DELETE tb_pcec WHERE maxch IS INITIAL AND
                       minch IS INITIAL AND
                       maxon IS INITIAL AND
                       minon IS INITIAL.

*  if p_pos is not initial.
*    loop at tb_pcec into wa_pcec.
*      loop at tb_payr_pos into wa_payr_pos where chect >= wa_pcec-checf and
*                                                 chect <= wa_pcec-chect.
*        wa_pcec-counton = wa_pcec-counton + 1.
*        if wa_pcec-maxon is initial.
*           wa_pcec-maxon = wa_payr_pos-chect.
*        elseif wa_payr_pos-chect gt wa_pcec-maxon.
*           wa_pcec-maxon = wa_payr_pos-chect.
*        endif.
*
*        if wa_pcec-minon is initial.
*           wa_pcec-minon = wa_payr_pos-chect.
*        elseif wa_payr_pos-chect lt wa_pcec-minon.
*           wa_pcec-minon = wa_payr_pos-chect.
*        endif.
*
*        wa_payr_pos-lotno = wa_pcec-stapl.
*        modify tb_payr_pos from wa_payr_pos.
*      endloop.
*      modify tb_pcec from wa_pcec.
*      clear: wa_pcec, tb_pcec.
*    endloop.
*  endif.

  gt_payr[]  = tb_payr[].
  gt_pcec[]  = tb_pcec[].
  wb_payr    = hd_payr.

  IF s_ladat[] IS NOT INITIAL.
    low   = s_ladat-low.
    IF s_ladat-high EQ '00000000'.
      high = s_ladat-low.
    ELSE.
      high = s_ladat-high.
    ENDIF.
  ELSE.
    low   = s_laufd-low.
    IF s_laufd-high EQ '00000000'.
      high = s_laufd-low.
    ELSE.
      high = s_laufd-high.
    ENDIF.
  ENDIF.

  PERFORM get_header CHANGING txthdd01 txthdd02 txthdd03
                              txthdd04 txthdd05 txthdd06
                              txthdd07.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_report .
  FREE: tb_payr1.

  tb_payr1[] = tb_payr[].
*  sort tb_payr1 by hbkid hktid.

  DELETE ADJACENT DUPLICATES FROM tb_payr1 COMPARING hbkid hktid.


  PERFORM print_form.

ENDFORM.                    " PRINT_REPORT
*&---------------------------------------------------------------------*
*&      Form  POSTING_ONHAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_onhand .

*  select zbukr hbkid hktid chect vblnr zaldt waers rwbtr znme1 prius
  SELECT zbukr hbkid hktid chect vblnr gjahr zaldt waers rwbtr znme1 prius voidd
    FROM payr INTO TABLE tb_payr_pos
    FOR ALL ENTRIES IN tb_pcec
    WHERE zbukr EQ tb_pcec-zbukr AND
          hbkid EQ tb_pcec-hbkid AND
          hktid EQ tb_pcec-hktid AND
          zaldt GT s_ladat-low.

*          laufd eq tb_pcec-laufd.
*    where zbukr in s_zbukr and
*          hbkid in s_hbkid and
*          hktid in s_hktid and
*          zaldt gt s_ladat-low and
**          zaldt gt s_ladat and
*          laufd in s_laufd.

*  select zbukr hbkid hktid stapl checf chect fstap checl into table tb_pcec_pos
*    from pcec for all entries in tb_payr
*    where zbukr eq tb_payr-zbukr and
*          hbkid eq tb_payr-hbkid and
*          hktid eq tb_payr-hktid and
*          checf le tb_payr-chect and
*          chect ge tb_payr-chect.
ENDFORM.                    " POSTING_ONHAND
*&---------------------------------------------------------------------*
*&      Form  GET_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_header CHANGING txthdd01 txthdd02 txthdd03 txthdd04 txthdd05 txthdd06 txthdd07.
  CLEAR: txthdd01, txthdd02, txthdd03, txthdd04, txthdd05, txthdd06, txthdd07.
  IF s_zbukr-high IS NOT INITIAL.
    CONCATENATE s_zbukr-low 'to' s_zbukr-high INTO txthdd01 SEPARATED BY space.
  ELSE.
    txthdd01 = s_zbukr-low.
  ENDIF.

  IF s_hbkid-high IS NOT INITIAL.
    CONCATENATE s_hbkid-low 'to' s_hbkid-high INTO txthdd02 SEPARATED BY space.
  ELSE.
    txthdd02 = s_hbkid-low.
  ENDIF.

  IF s_hktid-high IS NOT INITIAL.
    CONCATENATE s_hktid-low 'to' s_hktid-high INTO txthdd03 SEPARATED BY space.
  ELSE.
    txthdd03 = s_hktid-low.
  ENDIF.

  IF s_chect-high IS NOT INITIAL.
    CONCATENATE s_chect-low 'to' s_chect-high INTO txthdd04 SEPARATED BY space.
  ELSE.
    txthdd04 = s_chect-low.
  ENDIF.

  IF s_ladat-high IS NOT INITIAL.
*    concatenate s_ladat-low 'to' s_ladat-high into txthdd05 separated by space.
    CONCATENATE: s_ladat-low+6(2) '.' s_ladat-low+4(2) '.' s_ladat-low+0(4) INTO txthdd05,
                 txthdd05 'to' s_ladat-high+6(2) INTO txthdd05 SEPARATED BY space,
                 txthdd05 '.' s_ladat-high+4(2) '.' s_ladat-high+0(4) INTO txthdd05.
  ELSE.
    CONCATENATE s_ladat-low+6(2) '.' s_ladat-low+4(2) '.' s_ladat-low+0(4) INTO txthdd05.
*    txthdd05 = s_ladat-low.
  ENDIF.

  IF s_laufd-high IS NOT INITIAL.
*    concatenate s_laufd-low 'to' s_laufd-high into txthdd06 separated by space.
    CONCATENATE: s_laufd-low+6(2) '.' s_laufd-low+4(2) '.' s_laufd-low+0(4) INTO txthdd06,
                 txthdd06 'to' s_laufd-high+6(2) INTO txthdd06 SEPARATED BY space,
                 txthdd06 '.' s_laufd-high+4(2) '.' s_laufd-high+0(4) INTO txthdd06.
  ELSE.
    CONCATENATE s_laufd-low+6(2) s_laufd-low+4(2) s_laufd-low+0(4) INTO txthdd06.
*    txthdd06 = s_laufd-low.
  ENDIF.

  IF s_prius-high IS NOT INITIAL.
    CONCATENATE s_prius-low 'to' s_prius-high INTO txthdd07 SEPARATED BY space.
  ELSE.
    txthdd07 = s_prius-low.
  ENDIF.


ENDFORM.                    " GET_HEADER
