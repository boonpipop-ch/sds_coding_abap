FUNCTION Z_SDSCA_IDWT_DISPATCHER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_GLOB) TYPE  IDWTGLOB
*"     REFERENCE(I_FIELDS) TYPE  TY_SVAL OPTIONAL
*"     VALUE(IO_WRAPPER) TYPE REF TO  CL_SRF_WRAPPER OPTIONAL
*"  CHANGING
*"     REFERENCE(C_COMPCD) TYPE  TY_IDWTCOMPCD
*"     REFERENCE(C_PARTNER) TYPE  TY_IDWTPARTNER
*"     REFERENCE(C_FIDOC) TYPE  TY_IDWTFIDOC
*"     REFERENCE(C_ERROR) TYPE  TY_IDWTERROR OPTIONAL
*"----------------------------------------------------------------------
  DATA: H_PAR1           LIKE SY-MSGV1,
        WA_DMETOOL       TYPE IDWTINTDME,
        IT_DMETOOL       TYPE IDWTINTDME OCCURS 0,
        WA_FIDOC         TYPE IDWTFIDOC,
        WA_PARTNER       TYPE IDWTPARTNER,
        WA_COMPCD        TYPE IDWTCOMPCD,
        WA_HEADER2       TYPE NORMTITEL,
        WA_DATE_FROM(10) TYPE C,
        WA_DATE_TO(10)   TYPE C,
        H_TABIX          LIKE SY-TABIX,
        H_TREETYPE       TYPE DMEE_TREETYPE VALUE 'WTRE',
        IT_FILE          LIKE FPM_FILE OCCURS 0 WITH HEADER LINE,
        H_EXCEPTION,
        H_REPID          LIKE SY-REPID,
        T_INFO1          TYPE TAX_ALV_INFO,
        T_INFO2          TYPE TAX_ALV_INFO,
        T_INFO3          TYPE TAX_ALV_INFO,
        T_INFO4          TYPE TAX_ALV_INFO,
        H_LINES          TYPE I,
        L_FILENAME       TYPE STRING,
        L_PAYMFILE       LIKE REGUT-FSNAM.

*****************Note 902399****************
*  DATA: BEGIN OF dme_par.
*          INCLUDE STRUCTURE idwtintall.
*  DATA: idwtadd TYPE idwtadd.
*          INCLUDE STRUCTURE idwtintdmep.
*  DATA  END OF dme_par.
*****************Note 902399****************

  DATA: IT_CODE(72)       TYPE C OCCURS 0 WITH HEADER LINE,
        H_SUBPOOLNAME(88) TYPE C,
        H_SORTFIELD(70)   TYPE C,
        IT_SORT_FIELDS    LIKE DMEE_TREE_SORT OCCURS 0,
        WA_SORT_FIELDS    LIKE DMEE_TREE_SORT,
        WA_PCFILE         LIKE FPM_FILE-LINE,
        IT_PCFILE         LIKE WA_PCFILE OCCURS 0,
        WA_WT_NATXT       TYPE NATXT.

  CLASS CL_EXITHANDLER DEFINITION LOAD."declaration
  DATA: BADIOBJ    TYPE REF TO IF_EX_IDWTREP_ADDFUNCINT,
        EXTBADIOBJ TYPE REF TO IF_EX_WTAXREPORT_MODIFY.
  CONSTANTS: C_MESSAGECLASS TYPE ARBGB VALUE 'ID_WT'.

* BoI - Interface for new BADI ALL_DME_FIELDS_MODIFY  "Enterprise
*Note 902399
  DATA:
LT_FIDOC TYPE TABLE OF IDWTFIDOC WITH KEY BUKRS BELNR GJAHR BUZEI WITHT
 INITIAL SIZE 0.

* EoI - Interface for new BADI ALL_DME_FIELDS_MODIFY  "Enterprise

* Data declaration part for additional field IDWTADD (countries)
  DATA: WA_FIELDS TYPE SVAL.
  DATA: FIELDNAME(60) TYPE C.
  FIELD-SYMBOLS: <FS> TYPE ANY.
  DATA: T_COUNTRY_FIELDS  TYPE STANDARD TABLE OF FIELDINFO,
        WA_COUNTRY_FIELDS TYPE FIELDINFO.
  DATA: WA_FIDOC2 TYPE IDWTFIDOC.


* Declaration for BADI call
  DATA EXIT TYPE REF TO IF_EX_IDWTREP_ADDFUNCINT.

  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    CHANGING
      INSTANCE = EXIT.
* BoI - Interface for new BADI ALL_DME_FIELDS_MODIFY  "Enterprise

* sort tables.
*  sort c_compcd by bukrs WT_PERIOD.
*  sort c_partner by bukrs partnerno WT_PERIOD witht WT_WITHCD.
*  sort c_fidoc by bukrs partnerno WT_PERIOD BELNR GJAHR BUZEI.

* Check available additional country specific fields.
  CALL FUNCTION 'DDIC_DATA_DESCRIPTION_SCAN'
    EXPORTING
      I_STRUCTURE      = 'IDWTADD'
*     I_BYPASSING_BUFFER       =
*     I_CONTEXT        =
    TABLES
      T_FIELDINFO      = T_COUNTRY_FIELDS
*     T_FIELDINFO2     =
    EXCEPTIONS
      TABLE_NOT_EXISTS = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Update additional field to FI document line item
  LOOP AT C_FIDOC INTO WA_FIDOC2.
*   Fill up additional country fields from add.selection screen
    LOOP AT I_FIELDS INTO WA_FIELDS.
*     Check existing fields for country specific.
      READ TABLE T_COUNTRY_FIELDS INTO WA_COUNTRY_FIELDS
           WITH KEY FIELDNAME = WA_FIELDS-FIELDNAME.
      IF SY-SUBRC NE 0.
        CONTINUE.                 "Skip non existing field in IDWTADD
      ENDIF.

*     Assign field name/value to corresponding fields
      MOVE 'WA_FIDOC2-' TO FIELDNAME.
      MOVE WA_FIELDS-FIELDNAME TO FIELDNAME+10.
      ASSIGN (FIELDNAME) TO <FS>.
      IF SY-SUBRC EQ 0.
        MOVE WA_FIELDS-VALUE TO <FS>.
      ENDIF.
    ENDLOOP.
    MODIFY C_FIDOC FROM WA_FIDOC2.
  ENDLOOP.


* preparation

*  MOVE-CORRESPONDING i_glob TO dme_par.   'Note 902399

*--------------------------------------------------------------------*
*              creating files using DME-Tool                         *
*--------------------------------------------------------------------*
* Note 784587
  IF I_GLOB-WT_SC_FILE_ACT = 'X' OR I_GLOB-WT_SC_FILE_ACT = 'x' .
* BoI - Insert for DMEE modification                     "Note 640939
*Note 902399
*   lt_fidoc[] = c_fidoc[].
*
*   IF i_glob-WT_SC_FILE_NULL = 'X'.
**       DELETE c_fidoc WHERE WT_QBSHH EQ 0.
*       DELETE lt_fidoc WHERE WT_QBSHH EQ 0.
*   ENDIF.
*
*DESCRIBE TABLE lt_fidoc LINES SY-TFILL.
*
*IF SY-TFILL EQ 0.
*  MESSAGE i702(7q).
*  EXIT.
*ENDIF.
*Note 902399
* Note 784587

    DATA: LS_FIDOC(22)   TYPE C,
          LS_PARTNER(22) TYPE C,
          LS_COMPCD(22)  TYPE C,
          WA_RFDT        TYPE RFDT.


    WA_RFDT-AEDAT = SY-DATUM.
    WA_RFDT-USERA = SY-UNAME.
    WA_RFDT-PGMID = SY-REPID.

    CONCATENATE 'fidoc---' SY-DATUM SY-UZEIT INTO LS_FIDOC.
    CONCATENATE 'partner-' SY-DATUM SY-UZEIT INTO LS_PARTNER.
    CONCATENATE 'compcd--' SY-DATUM SY-UZEIT INTO LS_COMPCD.
*Note 902399
*    EXPORT c_fidoc TO DATABASE RFDT(WT) ID ls_fidoc FROM wa_rfdt.
*
*    Refresh c_fidoc.
*Note 902399
* Note 784587

    CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
      CHANGING
        INSTANCE = EXIT.
    CALL METHOD EXIT->MODIFY_DMEE
      EXPORTING
        FLT_VAL   = WA_IDWTGLOB-INTCA
        I_FIELDS  = I_FIELDS[]
        I_GLOB    = WA_IDWTGLOB
      CHANGING
        C_COMPCD  = C_COMPCD[]
        C_PARTNER = C_PARTNER[]
*       c_fidoc   = lt_fidoc[]
        C_FIDOC   = C_FIDOC[]   "Note 902399
        C_ERROR   = C_ERROR[]
      EXCEPTIONS
        OTHERS    = 1.
* EoI - Insert for DMEE modification
*Note 902399


    LT_FIDOC[] = C_FIDOC[].

    EXPORT C_FIDOC TO DATABASE RFDT(WT) ID LS_FIDOC FROM WA_RFDT.

*   Refresh c_fidoc.

*    EXPORT c_fidoc TO DATABASE RFDT(WT) ID ls_fidoc FROM wa_rfdt.

    FREE C_FIDOC.

*    IMPORT lt_fidoc FROM DATABASE RFDT(WT) ID ls_fidoc.



    IF I_GLOB-WT_SC_FILE_NULL = 'X'.
*       DELETE c_fidoc WHERE WT_QBSHH EQ 0.
      DELETE LT_FIDOC WHERE WT_QBSHH EQ 0.
    ENDIF.

    DESCRIBE TABLE LT_FIDOC LINES SY-TFILL.

    IF SY-TFILL EQ 0.
      MESSAGE I702(7Q).
      EXIT.
    ENDIF.

***************************Call to function module********************
*Note 902399
*CALL FUNCTION 'FI_WT_DMEE_FILE'
*  EXPORTING
*    H_TREETYPE       = h_treetype
*    I_GLOB           = i_glob
*    C_COMPCD         = c_compcd[]
*    C_PARTNER        = c_partner[]
*  TABLES
**   C_ERROR          =
*    IT_FILE          = it_file
*    I_FIELDS         = i_fields[]
*    C_FIDOC          = lt_fidoc[]
    .
*Note 902399
***************************End of call********************************

***************************Call to function module********************
*Note 902399 --MODIFIED

    CALL FUNCTION 'FI_WT_DMEE_FILE'
      EXPORTING
        H_TREETYPE = H_TREETYPE
        I_GLOB     = I_GLOB
        C_COMPCD   = C_COMPCD[]
      TABLES
*       C_ERROR    =
        IT_FILE    = IT_FILE
        I_FIELDS   = I_FIELDS[]
        C_PARTNER  = C_PARTNER[]
        C_FIDOC    = LT_FIDOC[].


*Note 902399 -- MODIFIED
***************************End of call********************************



******************Commented by Note 902399 ****************************
*    h_tabix = 0.                                         "Note 802033
** prepare the internal table for DME tool.
*    LOOP AT lt_fidoc INTO wa_fidoc.                      "Note 640939
*      wa_dmetool-idwtfidoc = wa_fidoc.
*      h_tabix = h_tabix + 1.                             "Note 802033
*      READ TABLE c_partner INTO wa_partner
*                 WITH KEY bukrs     = wa_fidoc-bukrs
*                          partnerno = wa_fidoc-partnerno
*                          wt_period = wa_fidoc-wt_period
*                          witht     = wa_fidoc-witht
*                          wt_withcd = wa_fidoc-wt_withcd.
*      IF sy-subrc = 0.
*        wa_dmetool-idwtpartner = wa_partner.
*      ENDIF.
** BoD - Business place assignment not function           "Enterprise
**      READ TABLE c_compcd INTO wa_compcd
**                 WITH KEY bukrs = wa_fidoc-bukrs.
** EoD - Business place assignment not function           "Enterprise
** BoI - Business place assignment not function           "Enterprise
*      READ TABLE c_compcd INTO wa_compcd
*                 WITH KEY bukrs = wa_fidoc-bukrs
*                          bupla = wa_fidoc-bupla.
** EoI - Business place assignment not function           "Enterprise
*      IF sy-subrc = 0.
*        wa_dmetool-idwtcompcd = wa_compcd.
*      ENDIF.
*
*      APPEND wa_dmetool TO it_dmetool.
*
**give DME-Tool the first item.
*      IF h_tabix = 1.
*
*        CALL FUNCTION 'DMEE_START'
*          EXPORTING
*            i_tree_type       = h_treetype
*            i_tree_id         = i_glob-wt_og_file_id
*            item              = wa_dmetool
*            param             = dme_par
**           UPARAM            =
*          TABLES
**           FILE_OUTPUT       =
*            sort_fields       = it_sort_fields
*          EXCEPTIONS
*            OTHERS            = 1.
*
*        IF sy-subrc NE 0.
*
*          PERFORM exception_set TABLES c_error[]
*                                USING '2'
*                                      i_glob
*                             CHANGING h_exception.
*
*        ENDIF.
*      ENDIF.
*      DELETE lt_fidoc.   "Note 784587
*    ENDLOOP.
* Note 802033

*    EXPORT c_partner TO DATABASE RFDT(WT) ID ls_partner FROM wa_rfdt.
*    EXPORT c_compcd TO DATABASE RFDT(WT) ID ls_compcd FROM wa_rfdt.
*
*    REFRESH  c_partner.
*    REFRESH  c_compcd.

* Note 802033

**sort table by the criteria defined in the file format.
*    IF NOT ( it_sort_fields IS INITIAL ).
*      it_code = 'REPORT TEMPDATA.'.                         "#EC *
*      APPEND it_code.
*      it_code = 'data: it_defsort type table of idwtintdme.'."#EC *
*      APPEND it_code.
*    it_code = 'FORM CREATE_SORT using it_sort like it_defsort[].'."#EC
**
*      APPEND it_code.
*      it_code = 'SORT IT_SORT BY '.                         "#EC *
*      APPEND it_code.
*
*      LOOP AT it_sort_fields INTO wa_sort_fields.
*        CONCATENATE wa_sort_fields-sort_tab '-' wa_sort_fields-sort_fld
*        ' ' INTO h_sortfield.
*        it_code = h_sortfield.
*        APPEND it_code.
*      ENDLOOP.
*
*      it_code = '.'.                                        "#EC *
*      APPEND it_code.
*      it_code = 'ENDFORM.'.                                 "#EC *
*      APPEND it_code.
*
**     Generate coding
*      GENERATE SUBROUTINE POOL it_code NAME h_subpoolname.
*      IF sy-subrc NE 0.
*        CLEAR: h_par1.
*        h_par1 = i_glob-wt_sc_file_name.
*        CALL FUNCTION 'FI_WT_HANDLE_ERRORS'
*          EXPORTING
*            i_glob           = i_glob
*            i_msgid          = c_messageclass
*            i_msgty          = 'I'
*            i_msgnr          = '039'
*            i_par1           = h_par1
**            I_PAR2           =
**            I_PAR3           =
**            I_PAR4           =
**            I_BUKRS          =
**            I_LIFNR          =
**            I_WITHT          =
**            I_WITHCD         =
**            I_BELNR          =
**            I_BUZEI          =
**            I_CTNUMBER       =
*          CHANGING
*            c_error          = c_error[]
*                  .
*
*      ENDIF.
*      PERFORM create_sort IN PROGRAM (h_subpoolname) USING it_dmetool[]
*.
*    ENDIF.
*
** BOI - Move additional country fields to DME and FI doc "Enterprise
**   Move value from additional parameter to user-param for DME file
*    LOOP AT i_fields INTO wa_fields.
**   Assign DME target field name/value
*      MOVE 'DME_PAR-IDWTADD-' TO fieldname.
*      MOVE wa_fields-fieldname TO fieldname+16.
*      ASSIGN (fieldname) TO <fs>.
*      IF sy-subrc EQ 0.
*        MOVE wa_fields-value TO <fs>.
*      ENDIF.
*    ENDLOOP.
*
** EOI - Move additional country fields to DME and FI doc "Enterprise
*
** BOI - new BADI CALL ALL_DME_FIELDS_MODIFY              "Enterprise
*    CALL METHOD exit->all_dme_fields_modify
*      EXPORTING
*        flt_val         = i_glob-intca
*        i_glob          = i_glob
*        i_fields        = i_fields
*        i_compcd        = c_compcd
*        i_partner       = c_partner
**     I_ERROR    =
*      CHANGING
*        i_fidoc         = c_fidoc
*        c_dmetools      = it_dmetool
*        c_dme_add_param = dme_par-idwtadd.
*
** EOI - new BADI CALL ALL_DME_FIELDS_MODIFY              "Enterprise
*
**give DME-Tool all of the items
*    CLEAR: wa_dmetool.
*    LOOP AT it_dmetool INTO wa_dmetool.
*      CALL FUNCTION 'DMEE_PUT_ITEM'
*           EXPORTING
*                item     = wa_dmetool
*                param    = dme_par
**         TABLES
**              ITEM_TAB =
*           EXCEPTIONS
*                OTHERS   = 1.
*
*      IF sy-subrc NE 0.
*        PERFORM exception_set TABLES c_error[]
*                               USING '2'
*                                     i_glob
*                            CHANGING h_exception.
*      ENDIF.
** Note 784587
*    delete it_dmetool.
*
*    ENDLOOP.
** Note 784587
*
*    IMPORT c_fidoc FROM DATABASE RFDT(WT) ID ls_fidoc.
*    IMPORT c_partner FROM DATABASE RFDT(WT) ID ls_partner.
*    IMPORT c_compcd FROM DATABASE RFDT(WT) ID ls_compcd.
*
** Note 784587
*
** At the end of calling DME-Tool
*    CALL FUNCTION 'DMEE_END'
*      TABLES
*        file_output = it_file
*      EXCEPTIONS
*        OTHERS      = 1.
*    IF sy-subrc NE 0.
*      PERFORM exception_set TABLES c_error[]
*                           USING '2'
*                                  i_glob
*                         CHANGING h_exception.
*    ENDIF.
************* Commented by NOte 902399 *****************************
*Note 902399
*  Refresh lt_fidoc[].

    FREE LT_FIDOC[].

    IMPORT C_FIDOC FROM DATABASE RFDT(WT) ID LS_FIDOC.

*Note 902399

* store files.
    CASE I_GLOB-WT_OG_FILE.
* to store the file on the application server.
      WHEN '2'.
        L_PAYMFILE = I_GLOB-WT_SC_FILE_NAME.
        CALL FUNCTION 'FI_PAYM_FILE_OPEN'
          EXPORTING
            I_TEMSE_NAME = SPACE
            I_FILE_NAME  = L_PAYMFILE.

        CALL FUNCTION 'FI_PAYM_FILE_WRITE'
          TABLES
            T_LINES = IT_FILE.

        CALL FUNCTION 'FI_FILE_CLOSE'.

*          OPEN DATASET i_glob-wt_sc_file_name FOR OUTPUT IN TEXT MODE
*                                              ENCODING NON-UNICODE
*                                             IGNORING CONVERSION ERRORS
        .
*
*          LOOP AT it_file.
*            TRANSFER it_file TO i_glob-wt_sc_file_name.
*          ENDLOOP.
*          CLOSE DATASET i_glob-wt_sc_file_name.

        IF SY-SUBRC = 0.
          MESSAGE S010(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME.
        ELSE.
          PERFORM EXCEPTION_SET TABLES   C_ERROR[]
                                USING    '2'
                                         I_GLOB
                                CHANGING H_EXCEPTION.
        ENDIF.
***eof issue due to code correction for France solved****

*      WHEN '1'.
*        LOOP AT it_file.
*          IF i_glob-intca = 'FR'.
*            MOVE it_file-line TO wa_pcfile.
*            APPEND wa_pcfile TO it_pcfile_fr.
*          ELSE.
*            MOVE it_file-line TO wa_pcfile.
*            APPEND wa_pcfile TO it_pcfile.
*          ENDIF.
*        ENDLOOP.
*
** store the file on PC.
*        l_filename = i_glob-wt_sc_file_name.
*        IF i_glob-intca = 'FR'.
*          CALL FUNCTION 'GUI_DOWNLOAD'
*             EXPORTING
**                   BIN_FILESIZE                  =
*                filename                      = l_filename
*                filetype                      = 'ASC'      "UNICODE
**                   APPEND                        = ' '
**                   WRITE_FIELD_SEPARATOR         = ' '
**                   HEADER                        = '00'
*                TRUNC_TRAILING_BLANKS         = ' '
*                TRUNC_TRAILING_BLANKS_EOL     = ' '
**                   WRITE_LF                      = ' '
**                   COL_SELECT                    = ' '
**                   COL_SELECT_MASK               = ' '
**                IMPORTING
**                   FILELENGTH                    =
*             TABLES
*                data_tab                      = it_pcfile_fr
*             EXCEPTIONS
*                file_write_error              = 1
*                no_batch                      = 2
*                gui_refuse_filetransfer       = 3
*                invalid_type                  = 4
*                no_authority                  = 5
*                unknown_error                 = 6
*                header_not_allowed            = 7
*                separator_not_allowed         = 8
*                filesize_not_allowed          = 9
*                header_too_long               = 10
*                dp_error_create               = 11
*                dp_error_send                 = 12
*                dp_error_write                = 13
*                unknown_dp_error              = 14
*                access_denied                 = 15
*                dp_out_of_memory              = 16
*                disk_full                     = 17
*                dp_timeout                    = 18
*                file_not_found                = 19
*                dataprovider_exception        = 20
*                control_flush_error           = 21
*               OTHERS                        = 22.
*        ELSE.
*          CALL FUNCTION 'GUI_DOWNLOAD'
*            EXPORTING
**           BIN_FILESIZE                  =
*              filename                      = l_filename
*              filetype                      = 'ASC'      "UNICODE
**           APPEND                        = ' '
**           WRITE_FIELD_SEPARATOR         = ' '
**           HEADER                        = '00'
**           TRUNC_TRAILING_BLANKS         = ' '
**           WRITE_LF                      = 'X'
**           COL_SELECT                    = ' '
**           COL_SELECT_MASK               = ' '
**         IMPORTING
**           FILELENGTH                    =
*            TABLES
*              data_tab                      = it_pcfile
*           EXCEPTIONS
*             file_write_error              = 1
*             no_batch                      = 2
*             gui_refuse_filetransfer       = 3
*             invalid_type                  = 4
*             no_authority                  = 5
*             unknown_error                 = 6
*             header_not_allowed            = 7
*             separator_not_allowed         = 8
*             filesize_not_allowed          = 9
*             header_too_long               = 10
*             dp_error_create               = 11
*             dp_error_send                 = 12
*             dp_error_write                = 13
*             unknown_dp_error              = 14
*             access_denied                 = 15
*             dp_out_of_memory              = 16
*             disk_full                     = 17
*             dp_timeout                    = 18
*             file_not_found                = 19
*             dataprovider_exception        = 20
*             control_flush_error           = 21
*             OTHERS                        = 22.
*        ENDIF.
*
**        CALL FUNCTION 'GUI_DOWNLOAD'
**            EXPORTING
**                 BIN_FILESIZE            = ' '
**                 CODEPAGE                = ' '
**                  filename                = l_filename
**                 FILETYPE                = 'TEXT'    "UNICODE
**                  filetype                = 'ASC'     "UNICODE
**                 MODE                    = ' '
**                 WK1_N_FORMAT            = ' '
**                 WK1_N_SIZE              = ' '
**                 WK1_T_FORMAT            = ' '
**                 WK1_T_SIZE              = ' '
**                 COL_SELECT              = ' '
**                 COL_SELECTMASK          = ' '
**                 NO_AUTH_CHECK           = ' '
**          IMPORTING
**                 FILELENGTH              =
**             TABLES
**                  data_tab                = it_pcfile
**                 FIELDNAMES              =
**             EXCEPTIONS
**                  file_open_error         = 1
**                  file_write_error        = 2
**                  invalid_filesize        = 3
**                  invalid_type            = 4
**                  no_batch                = 5
**                  unknown_error           = 6
**                  invalid_table_width     = 7
**                  gui_refuse_filetransfer = 8
**                  customer_error          = 9
**                  OTHERS                  = 10
**                  .
*        IF sy-subrc <> 0.
*          PERFORM exception_set TABLES c_error[]
*                               USING '2'
*                                      i_glob
*                             CHANGING h_exception.
*        ELSE.
*          MESSAGE s010(id_wt) WITH i_glob-wt_sc_file_name.
*        ENDIF.
*
*    ENDCASE.
      WHEN '1'.
        LOOP AT IT_FILE.
          MOVE IT_FILE-LINE TO WA_PCFILE.
          APPEND WA_PCFILE TO IT_PCFILE.
        ENDLOOP.

* store the file on PC.
        L_FILENAME = I_GLOB-WT_SC_FILE_NAME.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
*           BIN_FILESIZE            =
            FILENAME                = L_FILENAME
            FILETYPE                = 'ASC'      "UNICODE
*           APPEND                  = ' '
*           WRITE_FIELD_SEPARATOR   = ' '
*           HEADER                  = '00'
*           TRUNC_TRAILING_BLANKS   = ' '
*           WRITE_LF                = 'X'
*           COL_SELECT              = ' '
*           COL_SELECT_MASK         = ' '
*   IMPORTING
*           FILELENGTH              =
          TABLES
            DATA_TAB                = IT_PCFILE
          EXCEPTIONS
            FILE_WRITE_ERROR        = 1
            NO_BATCH                = 2
            GUI_REFUSE_FILETRANSFER = 3
            INVALID_TYPE            = 4
            NO_AUTHORITY            = 5
            UNKNOWN_ERROR           = 6
            HEADER_NOT_ALLOWED      = 7
            SEPARATOR_NOT_ALLOWED   = 8
            FILESIZE_NOT_ALLOWED    = 9
            HEADER_TOO_LONG         = 10
            DP_ERROR_CREATE         = 11
            DP_ERROR_SEND           = 12
            DP_ERROR_WRITE          = 13
            UNKNOWN_DP_ERROR        = 14
            ACCESS_DENIED           = 15
            DP_OUT_OF_MEMORY        = 16
            DISK_FULL               = 17
            DP_TIMEOUT              = 18
            FILE_NOT_FOUND          = 19
            DATAPROVIDER_EXCEPTION  = 20
            CONTROL_FLUSH_ERROR     = 21
            OTHERS                  = 22.


*        CALL FUNCTION 'GUI_DOWNLOAD'
*            EXPORTING
*                 BIN_FILESIZE            = ' '
*                 CODEPAGE                = ' '
*                  filename                = l_filename
*                 FILETYPE                = 'TEXT'    "UNICODE
*                  filetype                = 'ASC'     "UNICODE
*                 MODE                    = ' '
*                 WK1_N_FORMAT            = ' '
*                 WK1_N_SIZE              = ' '
*                 WK1_T_FORMAT            = ' '
*                 WK1_T_SIZE              = ' '
*                 COL_SELECT              = ' '
*                 COL_SELECTMASK          = ' '
*                 NO_AUTH_CHECK           = ' '
*          IMPORTING
*                 FILELENGTH              =
*             TABLES
*                  data_tab                = it_pcfile
*                 FIELDNAMES              =
*             EXCEPTIONS
*                  file_open_error         = 1
*                  file_write_error        = 2
*                  invalid_filesize        = 3
*                  invalid_type            = 4
*                  no_batch                = 5
*                  unknown_error           = 6
*                  invalid_table_width     = 7
*                  gui_refuse_filetransfer = 8
*                  customer_error          = 9
*                  OTHERS                  = 10
*                  .
        IF SY-SUBRC <> 0.
          PERFORM EXCEPTION_SET TABLES C_ERROR[]
                               USING '2'
                                      I_GLOB
                             CHANGING H_EXCEPTION.
        ELSE.
          MESSAGE S010(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME.
        ENDIF.

    ENDCASE.


  ENDIF.

*-------------------------------------------------------------------*
*           call smartforms or Adobe forms to print certificates
*-------------------------------------------------------------------*
* call the smartform 1 or Adobe form 1
  IF ( I_GLOB-WT_SC_FORM_ACT1 = 'X' OR
     I_GLOB-WT_SC_FORM_ACT1 = 'x' ) AND
     NOT ( I_GLOB-WT_OG_FORM_NM1 IS INITIAL ).

    IF I_GLOB-WT_OG_FORM_TYP1 = ''. "'X'.
      PERFORM ADOBEFORM_CALL USING '1'
                                  I_GLOB
                                  C_COMPCD[]
                                  C_PARTNER[]
                                  C_FIDOC[]
                                  C_ERROR[]
                                  IO_WRAPPER
                         CHANGING H_EXCEPTION.
    ELSE.
      PERFORM SMARTFORM_CALL USING '1'
                                  I_GLOB
                                  C_COMPCD[]
                                  C_PARTNER[]
                                  C_FIDOC[]
                                  C_ERROR[]
                         CHANGING H_EXCEPTION.
    ENDIF.

  ENDIF.

*call the smartform 2 or Adobe form 2
  IF ( I_GLOB-WT_SC_FORM_ACT2 = 'X' OR
     I_GLOB-WT_SC_FORM_ACT2 = 'x' ) AND
*     not ( I_glob-wt_og_form_nm1 is initial ).
     NOT ( I_GLOB-WT_OG_FORM_NM2 IS INITIAL ). "Note 823761


    IF I_GLOB-WT_OG_FORM_TYP2 = ''."'X'.
      PERFORM ADOBEFORM_CALL USING  '2'
                                    I_GLOB
                                    C_COMPCD[]
                                    C_PARTNER[]
                                    C_FIDOC[]
                                    C_ERROR[]
                                    IO_WRAPPER
                           CHANGING H_EXCEPTION.

    ELSE.
      PERFORM SMARTFORM_CALL USING  '2'
                                    I_GLOB
                                    C_COMPCD[]
                                    C_PARTNER[]
                                    C_FIDOC[]
                                    C_ERROR[]
                           CHANGING H_EXCEPTION.
    ENDIF.
  ENDIF.

*update the tables, which is relevant for storing the cert. No. etc.

  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE      "factory method call
    CHANGING
      INSTANCE = BADIOBJ.

  IF SY-SUBRC <> 0.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024(ID_WT) WITH 'UPDATES_AFTER_OUTPUT' RAISING ERROR_BADI
       .
    ELSE.
      MESSAGE E024 WITH 'UPDATES_AFTER_OUTPUT'
                   INTO WA_WT_NATXT.
      PERFORM FILL_IDWTERROR TABLES C_ERROR[]
                             USING  '24' WA_WT_NATXT.

    ENDIF.

  ENDIF.

* call method for SAP internal usage
  CALL METHOD BADIOBJ->UPDATES_AFTER_OUTPUT
    EXPORTING
      FLT_VAL   = I_GLOB-INTCA
      I_FIELDS  = I_FIELDS[]
      I_GLOB    = I_GLOB
      I_COMPCD  = C_COMPCD[]
      I_PARTNER = C_PARTNER[]
      I_FIDOC   = C_FIDOC[]
    CHANGING
      C_ERROR   = C_ERROR[].

  IF SY-SUBRC <> 0.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024(ID_WT) WITH 'UPDATES_AFTER_OUTPUT' RAISING ERROR_BADI
   .
    ELSE.
      MESSAGE E024 WITH 'UPDATES_AFTER_OUTPUT'
                   INTO WA_WT_NATXT.
      PERFORM FILL_IDWTERROR TABLES C_ERROR[]
                             USING  '24' WA_WT_NATXT
                             .

    ENDIF.


  ENDIF.

* call the BADI for customer and partner
  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE      "factory method call
    CHANGING
      INSTANCE = EXTBADIOBJ.

  IF SY-SUBRC <> 0.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024(ID_WT) WITH 'ACITIVITIES_AFTER_OUTPUT'
              RAISING ERROR_BADI.
    ELSE.
      MESSAGE E024(ID_WT) WITH 'ACITIVITIES_AFTER_OUTPUT'
                 INTO WA_WT_NATXT.
      PERFORM FILL_IDWTERROR TABLES C_ERROR[]
                           USING  '24' WA_WT_NATXT.

    ENDIF.
  ENDIF.



  CALL METHOD EXTBADIOBJ->ACTIVITIES_AFTER_OUTPUT
    EXPORTING
      FLT_VAL   = I_GLOB-INTCA
      I_FIELDS  = I_FIELDS[]
      I_GLOB    = I_GLOB
      I_COMPCD  = C_COMPCD[]
      I_PARTNER = C_PARTNER[]
      I_FIDOC   = C_FIDOC[]
    CHANGING
      C_ERROR   = C_ERROR[].

  IF SY-SUBRC <> 0.
    IF WA_IDWTGLOB-WT_SC_LIST_ACT4 IS INITIAL.
      MESSAGE E024(ID_WT) WITH 'ACITIVITIES_AFTER_OUTPUT'
              RAISING     ERROR_BADI.
    ELSE.
      MESSAGE E024 WITH 'ACITIVITIES_AFTER_OUTPUT'
                 INTO WA_WT_NATXT.
      PERFORM FILL_IDWTERROR TABLES C_ERROR[]
                             USING  '24' WA_WT_NATXT.

    ENDIF.

  ENDIF.



*--------------------------------------------------------------------*
*      call ALV-Tool to create screen output                         *
*--------------------------------------------------------------------*
* fill the error table
  CASE H_EXCEPTION.
    WHEN 1.
      IF I_GLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE I011(ID_WT) WITH WA_FORMNAME RAISING ERROR_FORM.
      ELSE.
        CLEAR WA_WT_NATXT.
        MESSAGE I011(ID_WT) WITH WA_FORMNAME INTO
                     WA_WT_NATXT.

        PERFORM FILL_IDWTERROR TABLES C_ERROR[]
                            USING  '11' WA_WT_NATXT.

      ENDIF.


    WHEN 2.
      IF I_GLOB-WT_SC_LIST_ACT4 IS INITIAL.
        MESSAGE I008(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME
                RAISING ERROR_FILE.
      ELSE.
        CLEAR WA_WT_NATXT.
        MESSAGE I008(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME INTO
                     WA_WT_NATXT.

        PERFORM FILL_IDWTERROR TABLES C_ERROR[]
                            USING  '008' WA_WT_NATXT.

      ENDIF.
  ENDCASE.

* prepare the list output
  H_REPID = SY-REPID.

  PERFORM LISTINFO_FILL USING    I_GLOB
                                 BADIOBJ
                        CHANGING T_INFO1
                                 T_INFO2
                                 T_INFO3
                                 T_INFO4.
  CLEAR WA_COMPCD.
  DESCRIBE TABLE C_COMPCD LINES H_LINES.
  IF H_LINES = 1.
    READ TABLE C_COMPCD INTO WA_COMPCD INDEX 1.
  ENDIF.

* the second header title will group the second header + the reporting
* period .

  WRITE I_GLOB-WT_SC_REP_FROM TO WA_DATE_FROM DD/MM/YYYY.
  WRITE I_GLOB-WT_SC_REP_TO   TO WA_DATE_TO   DD/MM/YYYY.

  CONCATENATE I_GLOB-WT_OG_TITEL2(40) TEXT-001 WA_DATE_FROM
  TEXT-002 WA_DATE_TO INTO WA_HEADER2 SEPARATED BY SPACE .

  CALL FUNCTION 'FI_WT_ALV_INTERFACE'
    EXPORTING
      IM_METHOD          = I_GLOB-DISP_METHOD
      IM_CALLING_PROGRAM = I_GLOB-PGMNAME
      IM_HEADER_1        = I_GLOB-WT_OG_TITEL1
      IM_HEADER_2        = WA_HEADER2
      IM_BATCH_HEADING   = I_GLOB-WT_SC_BATCH_HEAD
      IM_BH_BUKRS        = WA_COMPCD-BUKRS
      IM_INFO_CC_DATA    = T_INFO1
      IM_INFO_PARTNER    = T_INFO2
      IM_INFO_FI_DOC     = T_INFO3
      IM_INFO_ERROR      = T_INFO4
    TABLES
      TB_CC_DATA         = C_COMPCD
      TB_PARTNER         = C_PARTNER
      TB_FI_DOC          = C_FIDOC
      TB_ERROR           = C_ERROR
    EXCEPTIONS
      OTHERS             = 1.

  IF SY-SUBRC NE 0.
    H_EXCEPTION = 3.
  ENDIF.

*-----------------------------------------------------------------*
* clear the dummy-data inside internal tables after screen output.*
*-----------------------------------------------------------------*
  CASE I_GLOB-DISP_METHOD.
    WHEN '1'.
      REFRESH C_COMPCD.
    WHEN '2'.
      REFRESH C_PARTNER.
    WHEN '3'.
      REFRESH C_FIDOC.
    WHEN '4'.
      REFRESH C_ERROR.
  ENDCASE.


*--------------------------------------------------------------------*
*      raise the corresponding exception                             *
*--------------------------------------------------------------------*
  CASE H_EXCEPTION.
    WHEN 1.
      RAISE ERROR_FORM.
    WHEN 2.
      RAISE ERROR_FILE.
    WHEN 3.
      MESSAGE I031(ID_WT) RAISING ERROR_SCREEN_OUTPUT.
    WHEN 4.
      RAISE ERROR_MULTI.

  ENDCASE.
ENDFUNCTION.
