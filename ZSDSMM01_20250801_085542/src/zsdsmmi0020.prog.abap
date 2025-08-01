*&---------------------------------------------------------------------*
*& Report ZSDSMMI0020
*  Creation Date      : 13.06.2024
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZMMI031
*  Description        : Report and Export interface Equipment master.
*  Purpose            :
*  Copied from        : ZR_CS_SF_IF10
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMI0020.
* -- EQUI
  DATA: BEGIN OF gy_equi,
        equnr           TYPE equi-equnr,      " Equipment Number
        matnr           TYPE equi-matnr,      " Material
        sernr           TYPE equi-sernr,      " Serial number
        erdat           TYPE equi-erdat,      " Created Date
        aedat           TYPE equi-aedat,      " Changed Date
        objnr           TYPE equi-objnr,      " Object number
*        zzinstall_date  TYPE equi-zzinstall_date,
*        zzcommissioning TYPE equi-zzcommissioning,
        END OF gy_equi.
  DATA:gt_equi LIKE TABLE OF gy_equi.
* -- BGMKOBJ
  DATA: BEGIN OF gy_bgmkobj,
        gwldt TYPE bgmkobj-gwldt,   " Guarantee date
        gwlen TYPE bgmkobj-gwlen,   " Date on which the warranty ends
        lvorm TYPE bgmkobj-lvorm,   " Deletion Indicator
        END OF gy_bgmkobj.
** -- EQUI_EDIT
  DATA: BEGIN OF gy_equi_edit,
        equnr           TYPE equi-equnr,      " Equipment Number
        matnr           TYPE equi-matnr,      " Material
        sernr           TYPE equi-sernr,      " Serial number
        erdat           TYPE equi-erdat,      " Created Date
        aedat           TYPE equi-aedat,      " Changed Date
        gwldt           TYPE bgmkobj-gwldt,   " Guarantee date
        gwlen           TYPE bgmkobj-gwlen,   " Date on which the warranty ends
        lvorm           TYPE bgmkobj-lvorm,   " Deletion Indicator
        datum           TYPE ser01-datum,     " Ship Date
*        zzinstall_date  TYPE equi-zzinstall_date,
*        zzcommissioning TYPE equi-zzcommissioning,
        END OF gy_equi_edit.
  DATA:gt_equi_edit LIKE TABLE OF gy_equi_edit.
*  -- SER01
  DATA: BEGIN OF gy_ser01,
        equnr TYPE equi-equnr,
        datum TYPE ser01-datum,
        uzeit TYPE ser01-uzeit,
        vbtyp TYPE ser01-vbtyp,
  END OF gy_ser01.
  DATA:gt_ser01 LIKE TABLE OF gy_ser01.
*  -- MODEL
  DATA: BEGIN OF gy_model,
        matnr  TYPE equi-matnr,      " Material
        sernr  TYPE equi-sernr,      " Serial number
        datum  TYPE ser01-datum,     " DATE(Ship Date)
        gwldt  TYPE equi-gwldt,      " Guarantee date
        gwlen  TYPE equi-gwlen,      " Date on which the warranty ends
        lvorm  TYPE bgmkobj-lvorm,   " Deletion Indicator
        mfrpn  TYPE mara-mfrpn,
        serge  TYPE equi-sernr,
*        name1  TYPE ztcs_regis_warty-name1,
*        addr1  TYPE ztcs_regis_warty-addr1,
*        addr2  TYPE ztcs_regis_warty-addr2,
*        addr3  TYPE ztcs_regis_warty-addr3,
*        provic TYPE ztcs_regis_warty-provic,
*        zipcd  TYPE ztcs_regis_warty-zipcd,
*        phone  TYPE ztcs_regis_warty-phone,
        statu  TYPE tj02t-txt30,
        grdat  TYPE sy-datum,
        insdt  TYPE sy-datum,
        comdt  TYPE sy-datum,
        venst  TYPE sy-datum,
        vened  TYPE sy-datum,
        END OF gy_model.
  DATA: gt_model LIKE TABLE OF gy_model.
*  -- Download File
  TYPES:
    BEGIN OF gty_rec_dwld,
      text          TYPE text1024,                       "For Download
    END   OF gty_rec_dwld.
*
  DATA: gy_dwld TYPE gty_rec_dwld.
  DATA: gt_dwld  LIKE TABLE OF gy_dwld.
*  *-- WORK --**
  DATA: gw_ufile TYPE rlgrap-filename.
  DATA: gw_date  TYPE sy-datum.
  DATA: CT_RESULT TYPE TABLE OF string.
  DATA: CS_STRING TYPE string.
*  ----------------------------------------------------------------------*
*  -- Constants
*  ----------------------------------------------------------------------*
  CONSTANTS:
    gc_on           TYPE c                VALUE 'X',
    gc_module01     TYPE msgv1            VALUE 'WS_UPLOAD',
    gc_module02     TYPE msgv1            VALUE 'WS_DOWNLOAD',
    gc_module03     TYPE msgv1            VALUE 'GUI_DOWNLOAD',
    gc_filetype     TYPE char10           VALUE 'DAT',
    gc_mode         TYPE c                VALUE 'O',
    gc_in(3)        TYPE c                VALUE 'FCU',
    gc_out(3)       TYPE c                VALUE 'CDU',
    gc_acc(3)       TYPE c                VALUE 'ACC',
    gc_spics(16)    TYPE c                VALUE 'ZSPIC',
    gc_uke(16)      TYPE c                VALUE 'ZUKEHARAI',
    gc_dat(3)       TYPE c                VALUE 'DAT'.
*
  CONSTANTS:
    gc_hdr_key(12)            TYPE c VALUE 'EquipmentKey',
    gc_hdr_matnr(14)          TYPE c VALUE 'MaterialNumber',
    gc_hdr_sernr(12)          TYPE c VALUE 'SerialNumber',
    gc_hdr_datum(12)          TYPE c VALUE 'DeliveryDate',
    gc_hdr_gwldt(13)          TYPE c VALUE 'GuaranteeDate',
    gc_hdr_gwlen(11)          TYPE c VALUE 'WarrantyEnd',
    gc_hdr_lvorm(17)          TYPE c VALUE 'DeletionIndicator',
    gc_hdr_mat_manu(15)       TYPE c VALUE 'MaterialManufac',
    gc_hdr_mat_seri(13)       TYPE c VALUE 'SerialManufac',
    gc_hdr_mat_endu(11)       TYPE c VALUE 'EnduserName',
    gc_hdr_mat_addr(14)       TYPE c VALUE 'Address',
    gc_hdr_mat_subd(14)       TYPE c VALUE 'SubDistrict',
    gc_hdr_mat_dist(14)       TYPE c VALUE 'District',
    gc_hdr_mat_prov(14)       TYPE c VALUE 'Provice',
    gc_hdr_mat_zipc(14)       TYPE c VALUE 'ZipCode',
    gc_hdr_mat_usep(12)       TYPE c VALUE 'EndUserPhone',
    gc_hdr_system_s(12)        TYPE c VALUE 'SystemStatus',
    gc_hdr_gr_date(7)          TYPE c VALUE 'GR_Date',
    gc_hdr_install_dt(11)      TYPE c VALUE 'InstallDate',
    gc_hdr_commsioning_dt(17)  TYPE c VALUE 'CommissioningDate',
    gc_hdr_ven_warty_start(19) TYPE c VALUE 'VendorWarrantyStart',
    gc_hdr_ven_warty_end(17)   TYPE c VALUE 'VendorWarrantyEnd'.
*  ----------------------------------------------------------------------*
*  -- SELECTION-SCREEN
*  ----------------------------------------------------------------------*
*
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
*  *-- Local dounload
  PARAMETERS r_lc    RADIOBUTTON GROUP gr1
  DEFAULT 'X' USER-COMMAND ucom.
*  *-- Unix Dounload
  PARAMETERS r_unix    RADIOBUTTON GROUP gr1.
*
  SELECTION-SCREEN END OF BLOCK b2.
*  *-- Local File Path
  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
*
  PARAMETERS p_file  TYPE dsvasdocid
                       MODIF ID saa.
  SELECTION-SCREEN END OF BLOCK b3.
*  *-- Unix File Path
*  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
*  PARAMETERS p_upath TYPE tvarv_val
*                      MODIF ID sab.
*  PARAMETERS p_ufile TYPE tvarv_val
*                      MODIF ID sab.
*  SELECTION-SCREEN END OF BLOCK b4.
*  *--Add Header
  SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME.
  PARAMETERS r_hdr_on     RADIOBUTTON GROUP gr3
  DEFAULT 'X' USER-COMMAND ucom.
  PARAMETERS r_hdr_of     RADIOBUTTON GROUP gr3.
  SELECTION-SCREEN END OF BLOCK b7.
*  *--Select Options
  SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME.
*
  PARAMETERS p_all AS CHECKBOX.
  SELECT-OPTIONS s_equnr FOR gy_equi-equnr.
  SELECT-OPTIONS s_matnr FOR gy_equi-matnr.
  SELECT-OPTIONS s_sernr FOR gy_equi-sernr.
  SELECT-OPTIONS s_exdat FOR gy_equi-erdat.
  PARAMETERS     p_refdt TYPE sy-datum.
  PARAMETERS     p_spmon(2) TYPE n.
  SELECTION-SCREEN END OF BLOCK b8.

*  PARAMETERS p_ztab AS CHECKBOX.
*  PARAMETERS p_letr AS CHECKBOX.
  PARAMETERS p_iq03 AS CHECKBOX.
*
*  -----------------------------------------------------------------------
*  INITIALIZATION.
*  -----------------------------------------------------------------------
  INITIALIZATION.
*  Extraction date
    s_exdat-sign ='I'.
    s_exdat-option = 'EQ'.
    s_exdat-low = sy-datum - 1.
    APPEND s_exdat.
*  Reference Date
    p_refdt = sy-datum.
*  -----------------------------------------------------------------------
*  At Selection-Screen
*  -----------------------------------------------------------------------
*
  AT SELECTION-SCREEN OUTPUT.
    PERFORM sub_modify_screen.
*
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
    PERFORM sub_get_filename CHANGING p_file.
*  ----------------------------------------------------------------------*
*  -- START-OF-SELECTION
*  ----------------------------------------------------------------------*
  START-OF-SELECTION.
*
    DATA: lw_upath_length TYPE i.
*
    IF r_unix = gc_on.
*      IF p_upath IS INITIAL OR p_ufile IS INITIAL.
*        MESSAGE e000(z_sd) WITH 'file path or name is empty'.
*      ENDIF.
*      lw_upath_length = STRLEN( p_upath ) - 1.
*      IF p_upath+lw_upath_length(1) <> '/'.
*        MESSAGE e000(z_sd) WITH 'wrong path name'.
*      ENDIF.
    ELSE.
      IF p_file IS INITIAL.
        MESSAGE e000(z_sd) WITH 'local filename is empty'.
      ENDIF.
    ENDIF.
*
    PERFORM sub_get_data_form.
*
    PERFORM f_get_addtional_data.
    PERFORM sub_output_data_form.
*
    IF r_lc = gc_on.
      PERFORM sub_down_file_form.
    ELSE.
*      PERFORM sub_down_unix_form.
      PERFORM f_sub_down_server.
    ENDIF.
*
*  &---------------------------------------------------------------------*
*  &      Form  SUB_MODIFY_SCREEN
*  &---------------------------------------------------------------------*
  FORM sub_modify_screen.
*
    LOOP AT SCREEN.
*
      IF r_lc = gc_on.
        IF screen-group1 = 'SAB'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
*        CLEAR p_ufile.
*        CLEAR p_upath.
      ELSE.
        IF screen-group1 = 'SAA'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
        CLEAR p_file.
      ENDIF.
*
    ENDLOOP.
*
  ENDFORM.                    " SUB_MODIFY_SCREEN
*  &---------------------------------------------------------------------*
*  &      Form  SUB_GET_FILENAME
*  &---------------------------------------------------------------------*
  FORM sub_get_filename CHANGING c_name.
*
    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        def_filename     = ' '
        def_path         = 'C:\TEMP\'
        mask             = ',*.*,*.*.'
        mode             = 'O'
      IMPORTING
        filename         = c_name
      EXCEPTIONS
        inv_winsys       = 01
        no_batch         = 02
        selection_cancel = 03
        selection_error  = 04.
*
  ENDFORM.                    " sub_GET_FILENAME
*  &---------------------------------------------------------------------*
*  &      FORM SUB_GET_DATA_FORM
*  &---------------------------------------------------------------------*
  FORM sub_get_data_form .
*
    DATA  :l_m_month(3) TYPE c.
    DATA  :l_datum      TYPE sy-datum.
    DATA  :l_vbtyp      TYPE ser01-vbtyp.

    DATA: BEGIN OF ls_venw,
        matnr TYPE equi-matnr,
        sernr TYPE equi-sernr,
        gwldt TYPE bgmkobj-gwldt,   " Guarantee date
        gwlen TYPE bgmkobj-gwlen,   " Date on which the warranty ends
        prdha TYPE mara-prdha,
        END OF ls_venw.
    DATA : lt_venw LIKE TABLE OF ls_venw.

    CLEAR :gw_date,
           l_m_month,
           l_vbtyp.
*
    l_m_month = p_spmon * -1.
*
    CALL FUNCTION 'MONTH_PLUS_DETERMINE'
      EXPORTING
        months  = l_m_month     " P_SPMON - 18
        olddate = p_refdt
      IMPORTING
        newdate = gw_date.
* ----- CREATE EQUI GET -----
*    IF p_ztab NE 'X' AND
*       p_letr NE 'X'.
      SELECT equnr                " Equipment Number
             matnr                " Material Number
             sernr                " Serial number
             erdat                " Date on Which Record Was Created
             aedat                " Changed On
             objnr                " Object number
*             zzinstall_date
*             zzcommissioning
        FROM equi
        INTO TABLE gt_equi
       WHERE equnr IN s_equnr     " Equipment Number
         AND matnr IN s_matnr     " Material Number
         AND sernr IN s_sernr     " Serial number
         AND erdat IN s_exdat.    " Date on Which Record Was Created
* ----- CREATE BGMKOBJ GET -----
      LOOP AT gt_equi INTO gy_equi.
        CLEAR :gy_bgmkobj.
        CLEAR :l_datum.
*
        SELECT SINGLE
               gwldt              " Guarantee date
               gwlen              " Date on which the warranty ends
               lvorm              " Deletion Indicator
          FROM bgmkobj
          INTO gy_bgmkobj
         WHERE j_objnr = gy_equi-objnr
           AND gaart   = '1'.     " Warranty type
* ----- CREATE SER01 GET -----
        PERFORM  sub_ser01_get
          USING  gy_equi-equnr
        CHANGING l_datum
                 l_vbtyp.
* ----- CREATE EDIT -----
        IF p_refdt <= gy_bgmkobj-gwlen.                  " Date on which the warranty ends
          gy_equi_edit-equnr           = gy_equi-equnr.          " Equipment Number
          gy_equi_edit-matnr           = gy_equi-matnr.          " Material
          gy_equi_edit-sernr           = gy_equi-sernr.          " Serial number
          gy_equi_edit-erdat           = gy_equi-erdat.          " Created Date
          gy_equi_edit-aedat           = gy_equi-aedat.          " Changed Date
          gy_equi_edit-gwldt           = gy_bgmkobj-gwldt.       " Guarantee date
          gy_equi_edit-gwlen           = gy_bgmkobj-gwlen.       " Date on which the warranty ends
          gy_equi_edit-lvorm           = gy_bgmkobj-lvorm.       " Deletion Indicator
          gy_equi_edit-datum           = l_datum.                 " Ship Date
*          gy_equi_edit-zzinstall_date  = gy_equi-zzinstall_date.
*          gy_equi_edit-zzcommissioning = gy_equi-zzcommissioning.
          APPEND gy_equi_edit TO gt_equi_edit.
*
        ELSE.
          IF p_all IS INITIAL AND l_vbtyp <> 'J'.         " Compensation
            CONTINUE.
          ENDIF.
          IF gw_date <= l_datum.
            gy_equi_edit-equnr           = gy_equi-equnr.        " Equipment Number
            gy_equi_edit-matnr           = gy_equi-matnr.        " Material
            gy_equi_edit-sernr           = gy_equi-sernr.        " Serial number
            gy_equi_edit-erdat           = gy_equi-erdat.        " Created Date
            gy_equi_edit-aedat           = gy_equi-aedat.        " Changed Date
            gy_equi_edit-gwldt           = gy_bgmkobj-gwldt.     " Guarantee date
            gy_equi_edit-gwlen           = gy_bgmkobj-gwlen.     " Date on which the warranty ends
            gy_equi_edit-lvorm           = gy_bgmkobj-lvorm.     " Deletion Indicator
            gy_equi_edit-datum           = l_datum.               " Ship Date
*            gy_equi_edit-zzinstall_date  = gy_equi-zzinstall_date.
*            gy_equi_edit-zzcommissioning = gy_equi-zzcommissioning.
            APPEND gy_equi_edit TO gt_equi_edit.
          ELSE.
            IF p_all IS INITIAL.
              CONTINUE.                                    " Compensation
            ELSE.
              gy_equi_edit-equnr           = gy_equi-equnr.        " Equipment Number
              gy_equi_edit-matnr           = gy_equi-matnr.        " Material
              gy_equi_edit-sernr           = gy_equi-sernr.        " Serial number
              gy_equi_edit-erdat           = gy_equi-erdat.        " Created Date
              gy_equi_edit-aedat           = gy_equi-aedat.        " Changed Date
              gy_equi_edit-gwldt           = gy_bgmkobj-gwldt.     " Guarantee date
              gy_equi_edit-gwlen           = gy_bgmkobj-gwlen.     " Date on which the warranty ends
              gy_equi_edit-lvorm           = gy_bgmkobj-lvorm.     " Deletion Indicator
              gy_equi_edit-datum           = l_datum.               " Ship Date
*              gy_equi_edit-zzinstall_date  = gy_equi-zzinstall_date.
*              gy_equi_edit-zzcommissioning = gy_equi-zzcommissioning.
              APPEND gy_equi_edit TO gt_equi_edit.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
*    ENDIF.
* ----- CHANGE EQUI GET -----
    REFRESH :gt_equi.
    CLEAR :  gy_equi,
             gy_bgmkobj.
*
*    IF p_ztab NE 'X' AND
*       p_letr NE 'X'.
      SELECT equnr                   " Equipment Number
             matnr                   " Material Number
             sernr                   " Serial number
             erdat                   " Date on Which Record Was Created
             aedat                   " Changed On
             objnr                   " Object number
*             zzinstall_date
*             zzcommissioning
        FROM equi
        INTO TABLE gt_equi
       WHERE equnr IN s_equnr        " Equipment Number
         AND matnr IN s_matnr        " Material Number
         AND sernr IN s_sernr        " Serial number
         AND erdat NOT IN s_exdat    " Date on Which Record Was Created
         AND aedat IN s_exdat.       " Changed On
*    ENDIF.

*    IF p_iq03 NE 'X'.
*      PERFORM f_get_data_change_from_z.
*    ENDIF.
* ----- CHANGE BGMKOBJ GET -----
    LOOP AT gt_equi INTO gy_equi.
      CLEAR :gy_bgmkobj.
      CLEAR :l_datum.
      CLEAR :l_vbtyp.
*
      SELECT SINGLE
             gwldt                 " Guarantee date
             gwlen                 " Date on which the warranty ends
             lvorm                 " Deletion Indicator
        FROM bgmkobj
        INTO gy_bgmkobj
       WHERE j_objnr = gy_equi-objnr
         AND gaart   = '1'.
* ----- CHANGE SER01 GET -----
      PERFORM  sub_ser01_get
        USING  gy_equi-equnr
      CHANGING l_datum
               l_vbtyp.
* ----- CHANGE EDIT -----
      gy_equi_edit-equnr           = gy_equi-equnr.      " Equipment Number
      gy_equi_edit-matnr           = gy_equi-matnr.      " Material
      gy_equi_edit-sernr           = gy_equi-sernr.      " Serial number
      gy_equi_edit-erdat           = gy_equi-erdat.      " Created Date
      gy_equi_edit-aedat           = gy_equi-aedat.      " Changed Date
      gy_equi_edit-gwldt           = gy_bgmkobj-gwldt.   " Guarantee date
      gy_equi_edit-gwlen           = gy_bgmkobj-gwlen.   " Date on which the warranty ends
      gy_equi_edit-lvorm           = gy_bgmkobj-lvorm.   " Deletion Indicator
      gy_equi_edit-datum           = l_datum.             " Ship Date
*      gy_equi_edit-zzinstall_date  = gy_equi-zzinstall_date.
*      gy_equi_edit-zzcommissioning = gy_equi-zzcommissioning.
      APPEND gy_equi_edit TO gt_equi_edit.
    ENDLOOP.
*

    IF gt_equi_edit[] IS NOT INITIAL.
      SELECT equi~matnr
             equi~sernr
             bgmkobj~gwldt
             bgmkobj~gwlen
             mara~prdha
        FROM equi
        INNER JOIN mara    ON mara~matnr      EQ equi~matnr
        INNER JOIN bgmkobj ON equi~objnr      EQ bgmkobj~j_objnr  AND
                              bgmkobj~gaart   EQ '2'
        INTO TABLE lt_venw
        FOR ALL ENTRIES IN gt_equi_edit
        WHERE equi~matnr EQ gt_equi_edit-matnr
          AND equi~sernr EQ gt_equi_edit-sernr.
    ENDIF.

    LOOP AT gt_equi_edit INTO gy_equi_edit.
      gy_model-matnr = gy_equi_edit-matnr.
      gy_model-sernr = gy_equi_edit-sernr.
      gy_model-datum = gy_equi_edit-datum.
      gy_model-gwldt = gy_equi_edit-gwldt.
      gy_model-gwlen = gy_equi_edit-gwlen.
      gy_model-lvorm = gy_equi_edit-lvorm.
*      gy_model-insdt = gy_equi_edit-zzinstall_date.
*      gy_model-comdt = gy_equi_edit-zzcommissioning.

      READ TABLE lt_venw INTO ls_venw
      WITH KEY matnr = gy_equi_edit-matnr
               sernr = gy_equi_edit-sernr.
      IF sy-subrc EQ 0.
        IF ls_venw-prdha+0(2) EQ 'AH' OR
           ls_venw-prdha+0(2) EQ 'CH'.
          gy_model-venst = ls_venw-gwldt.
          gy_model-vened = ls_venw-gwlen.
        ENDIF.
      ENDIF.
      APPEND gy_model TO gt_model.
    ENDLOOP.
*
  ENDFORM.                           "SUB_GET_DATA_FORM
*
*  &---------------------------------------------------------------------*
*  &      FORM SUB_SER01_GET
*  &---------------------------------------------------------------------*
  FORM sub_ser01_get USING    l_equi_equnr LIKE equi-equnr
                     CHANGING l_datum_s    LIKE sy-datum
                              l_vbtyp_s    LIKE ser01-vbtyp.
*
    CLEAR :gy_ser01,
           l_datum_s,
           l_vbtyp_s.

    REFRESH :gt_ser01.
*
    SELECT b~equnr
           c~datum
           c~uzeit
           c~vbtyp
      INTO TABLE gt_ser01
      FROM equi AS a INNER JOIN objk AS b
                     ON ( a~equnr EQ b~equnr )
                     INNER JOIN ser01 AS c
                     ON ( b~obknr EQ c~obknr )
             WHERE a~equnr EQ l_equi_equnr
                   AND ( c~vbtyp EQ 'J' OR c~vbtyp EQ 'T' ).
*
    SORT gt_ser01 BY equnr ASCENDING
                     datum DESCENDING
                     uzeit DESCENDING.
*
    READ TABLE gt_ser01 INTO gy_ser01 INDEX '1'.
*
    IF sy-subrc = '0'.
      l_vbtyp_s = gy_ser01-vbtyp.
      IF l_vbtyp_s = 'J'.
        l_datum_s = gy_ser01-datum.
      ENDIF.
    ENDIF.
*
  ENDFORM.                    "SUB_SER01_GET
*
*  &---------------------------------------------------------------------*
*  &      Form  SUB_DOWN_FILE_FORM
*  &---------------------------------------------------------------------*
  FORM sub_down_file_form .
*
    DATA lw_file TYPE string.
    "DATA lw_file TYPE RLGRAP-FILENAME.
*
    lw_file = p_file.
*
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lw_file
        filetype                = gc_filetype
        codepage                = '4110'
*
      TABLES
        data_tab                = gt_dwld
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
      MESSAGE e001(z_bc) WITH gc_module03 sy-subrc p_file.
    ENDIF.

  ENDFORM.                    " SUB_DOWN_FILE_FORM

**&---------------------------------------------------------------------*
**&      Form  F_SUB_DOWN_SERVER
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
 FORM f_sub_down_server.


  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.

   DATA: LV_PATH_FILE TYPE string.

   ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = 'ZSDSMMI0020' "LC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
                                                  I_PARAM             = sy-sysid "LC_CON-SEPARATOR
*                                                  I_PARAM_EXT      =
                                                  CHANGING  C_RETURN  = LV_PATH_FILE ).

*  IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/SFDC/ZMMI031'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/SFDC/ZMMI031'.
*  ELSE.
**     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
*  ENDIF.


  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.
  CONCATENATE 'EquipmentMasterInsert_' SY-DATUM sy-timlo '.csv' INTO LV_PATH.
  LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
                                      I_AL11_PATH   = LV_PATH_FILE "'/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'
                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                    " I_USER        = 'ds'
                                     "I_PASS        = 'ds=20240521'
                                    " I_IP          = '172.31.136.250'
                                    " I_PORT        = '21'
                                      I_DATA_SPIDER = 'X'
                                     IT_DATA       = CT_RESULT ).

  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.
ENDFORM.
**  &---------------------------------------------------------------------*
**  &      Form  SUB_DOWN_UNIX_FORM
**  &---------------------------------------------------------------------*
*  FORM sub_down_unix_form .
*    DATA:
*      lw_subrc(10) TYPE c.
**  -- Unix File Path
**
*    CLEAR gw_ufile.
**
*    CONCATENATE p_upath
*                p_ufile
*           INTO gw_ufile.
**
**  -- Open
*    OPEN DATASET gw_ufile FOR OUTPUT IN TEXT MODE ENCODING UTF-8 WITH WINDOWS LINEFEED.
*    IF sy-subrc <> 0.
*      MESSAGE e006(z_bc) WITH gw_ufile sy-subrc.
*    ENDIF.
**  -- Transfer
*    LOOP AT gt_dwld INTO gy_dwld.
*      TRANSFER gy_dwld TO gw_ufile.
*      IF sy-subrc <> 0.
*        lw_subrc = sy-subrc.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
**  -- Close
*    CLOSE DATASET gw_ufile.
*    IF sy-subrc <> 0.
*      MESSAGE e007(z_bc) WITH gw_ufile sy-subrc.
*    ENDIF.
**  -- Trasfer Err
*    IF lw_subrc <> 0.
*      CONDENSE lw_subrc.
*      MESSAGE e008(z_bc) WITH gw_ufile lw_subrc.
*    ELSE.
*      MESSAGE s429(ds).
*    ENDIF.
**
*  ENDFORM.                    " sub_DOWN_UNIX_FORM
*  &---------------------------------------------------------------------*
*  &      Form  sub_add_quote
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->CW_TSV_LINE  text
*  ----------------------------------------------------------------------*
  FORM sub_add_quote
    CHANGING cw_tsv_line TYPE gty_rec_dwld-text.

    DATA:
*          LC_TAB TYPE CHAR1 VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
          lc_tab TYPE char1 VALUE ',',
          lw_new_seprator TYPE string.

    CONCATENATE '"' lc_tab '"'
      INTO lw_new_seprator.

    REPLACE ALL OCCURRENCES OF '"' IN cw_tsv_line WITH '""'.
    REPLACE ALL OCCURRENCES OF lc_tab IN cw_tsv_line WITH lw_new_seprator.
    CONCATENATE '"' cw_tsv_line '"'
      INTO cw_tsv_line.


  ENDFORM.                    "sub_add_quote
*  &---------------------------------------------------------------------*
*  &      Form  sub_output_data_form
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
  FORM sub_output_data_form.

*    DATA: LC_TAB TYPE CHAR1 VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
    DATA: lc_tab TYPE char1 VALUE ','.
    DATA: lw_key(36) TYPE c.

    CLEAR gy_dwld.
    REFRESH gt_dwld.
*
    "add header
    IF r_hdr_on = 'X'.
      CONCATENATE
           gc_hdr_key
           gc_hdr_matnr
           gc_hdr_sernr
           gc_hdr_datum
           gc_hdr_gwldt
           gc_hdr_gwlen
           gc_hdr_lvorm
           gc_hdr_mat_manu
           gc_hdr_mat_seri
           gc_hdr_mat_endu
           gc_hdr_mat_addr
           gc_hdr_mat_subd
           gc_hdr_mat_dist
           gc_hdr_mat_prov
           gc_hdr_mat_zipc
           gc_hdr_mat_usep
           gc_hdr_system_s
           gc_hdr_gr_date
           gc_hdr_install_dt
           gc_hdr_commsioning_dt
           gc_hdr_ven_warty_start
           gc_hdr_ven_warty_end
*
        INTO gy_dwld
        SEPARATED BY lc_tab.
*
      PERFORM sub_add_quote CHANGING gy_dwld-text.
*
      APPEND gy_dwld TO gt_dwld.
    ENDIF.

    LOOP AT gt_model INTO gy_model.
*
      CALL FUNCTION 'CONVERSION_EXIT_GERNR_OUTPUT'
        EXPORTING
          input  = gy_model-sernr
        IMPORTING
          output = gy_model-sernr.

      CLEAR:lw_key.
      CONCATENATE gy_model-matnr gy_model-sernr INTO lw_key.

      CONCATENATE
          lw_key
          gy_model-matnr
          gy_model-sernr
          gy_model-datum
          gy_model-gwldt
          gy_model-gwlen
          gy_model-lvorm
          gy_model-mfrpn
          gy_model-serge
*          gy_model-name1
*          gy_model-addr1
*          gy_model-addr2
*          gy_model-addr3
*          gy_model-provic
*          gy_model-zipcd
*          gy_model-phone
          gy_model-statu
          gy_model-grdat
          gy_model-insdt
          gy_model-comdt
          gy_model-venst
          gy_model-vened
*
                INTO gy_dwld
        SEPARATED BY lc_tab.
*
      PERFORM sub_add_quote CHANGING gy_dwld-text.
*
      APPEND gy_dwld TO gt_dwld.
*
    ENDLOOP.
    CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = gt_dwld
                                                             I_SEPARATOR = '","'
                                                             I_START_END_VALUE = '"').

*   PERFORM F_EXPORT_TO_SERVER. "Use sub_output_data_form

  ENDFORM.                    " sub_GET_DATA_FORM
*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDTIONAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM f_get_addtional_data .
    DATA : BEGIN OF ls_mat,
      matnr TYPE mara-matnr,
      sernr TYPE equi-sernr,
      typbz TYPE equi-typbz,
      serge TYPE equi-sernr,
    END OF ls_mat.
    DATA lt_mat LIKE TABLE OF ls_mat.

    DATA : BEGIN OF ls_mara,
      matnr TYPE mara-matnr,
      mfrpn TYPE mara-mfrpn,
    END OF ls_mara.
    DATA lt_mara LIKE TABLE OF ls_mara.

    DATA : BEGIN OF ls_ztcs_regis_warty,
      matnr  TYPE mara-matnr,
      sernr  TYPE equi-sernr,
*      name1  TYPE ztcs_regis_warty-name1,
*      addr1  TYPE ztcs_regis_warty-addr1,
*      addr2  TYPE ztcs_regis_warty-addr2,
*      addr3  TYPE ztcs_regis_warty-addr3,
*      provic TYPE ztcs_regis_warty-provic,
*      zipcd  TYPE ztcs_regis_warty-zipcd,
*      phone  TYPE ztcs_regis_warty-phone,
*      docnr  TYPE ztcs_regis_warty-docnr,
    END OF ls_ztcs_regis_warty.
    DATA lt_ztcs_regis_warty LIKE TABLE OF ls_ztcs_regis_warty.

    DATA : BEGIN OF ls_addr,
*      docnr TYPE ztcs_regis_warty-docnr,
      provt TYPE c LENGTH 70,
      distt TYPE c LENGTH 70,
      subdt TYPE c LENGTH 70,
    END OF ls_addr.
    DATA lt_addr LIKE TABLE OF ls_addr.

    DATA : BEGIN OF ls_ser03,
      obknr TYPE ser03-obknr,
      matnr TYPE equi-matnr,
      sernr TYPE equi-sernr,
      mblnr TYPE ser03-mblnr,
      datum TYPE ser03-datum,
*      txt30 TYPE tj02t-txt30,
*      stat  TYPE jest-stat,
    END OF ls_ser03.
    DATA : lt_ser03 LIKE TABLE OF ls_ser03.

    DATA : BEGIN OF ls_jest,
      objnr TYPE equi-objnr,
      matnr TYPE equi-matnr,
      sernr TYPE equi-sernr,
      txt30 TYPE tj02t-txt30,
      stat  TYPE jest-stat,
    END OF ls_jest.
    DATA : lt_jest LIKE TABLE OF ls_jest.

    DATA ls_tmp LIKE ls_ser03.

    FIELD-SYMBOLS <lfs_model> LIKE LINE OF gt_model.

    IF gt_model[] IS NOT INITIAL.
      SELECT ser03~obknr
             equi~matnr
             equi~sernr
             ser03~mblnr
             ser03~datum
*             tj02t~txt04
*             jest~stat
        FROM equi
        INNER JOIN objk  ON equi~equnr  EQ objk~equnr
        INNER JOIN jest  ON equi~objnr  EQ jest~objnr AND
                            jest~inact  EQ space
        INNER JOIN tj02t ON jest~stat   EQ tj02t~istat AND
                            tj02t~spras EQ sy-langu
        INNER JOIN ser03 ON objk~obknr  EQ ser03~obknr AND
                            ser03~blart EQ 'WE'
        INTO TABLE lt_ser03
        FOR ALL ENTRIES IN gt_model
        WHERE equi~matnr EQ gt_model-matnr
          AND equi~sernr EQ gt_model-sernr.

      SORT lt_ser03 BY obknr DESCENDING.

      SELECT equi~objnr
             equi~matnr
             equi~sernr
             tj02t~txt04
             jest~stat
        FROM equi
*        INNER JOIN objk  ON equi~equnr  EQ objk~equnr
        INNER JOIN jest  ON equi~objnr  EQ jest~objnr AND
                            jest~inact  EQ space
        INNER JOIN tj02t ON jest~stat   EQ tj02t~istat AND
                            tj02t~spras EQ sy-langu
        INTO TABLE lt_jest
        FOR ALL ENTRIES IN gt_model
        WHERE equi~matnr EQ gt_model-matnr
          AND equi~sernr EQ gt_model-sernr.

      SORT lt_jest BY objnr DESCENDING stat DESCENDING.

      SELECT  matnr
              sernr
              typbz
              serge
      FROM equi
      INTO TABLE lt_mat
      FOR ALL ENTRIES IN gt_model
      WHERE matnr EQ gt_model-matnr
        AND sernr EQ gt_model-sernr.

      SELECT matnr
             mfrpn
        FROM mara
        INTO TABLE lt_mara
        FOR ALL ENTRIES IN gt_model
        WHERE matnr EQ gt_model-matnr.

*      SELECT matnr
*             sernr
*             name1
*             addr1
*             addr2
*             addr3
*             provic
*             zipcd
*             phone
*             docnr
*        FROM ztcs_regis_warty
*        INTO TABLE lt_ztcs_regis_warty
*        FOR ALL ENTRIES IN gt_model
*        WHERE matnr EQ gt_model-matnr
*          AND sernr EQ gt_model-sernr.

*      IF lt_ztcs_regis_warty IS NOT INITIAL.
*        SELECT docnr
*               ztprov~destt
*               ztdist~destt
*               ztsubd~destt
*          FROM ztcs_addr_warty
*          INNER JOIN ztprov ON ztcs_addr_warty~provc EQ ztprov~provc
*          INNER JOIN ztdist ON ztcs_addr_warty~distc EQ ztdist~distc AND
*                               ztdist~provc          EQ ztprov~provc
*          INNER JOIN ztsubd ON ztcs_addr_warty~subdc EQ ztsubd~subdc AND
*                               ztsubd~provc          EQ ztdist~provc  AND
*                               ztsubd~distc          EQ ztdist~distc
*          INTO TABLE lt_addr
*          FOR ALL ENTRIES IN lt_ztcs_regis_warty
*          WHERE docnr EQ lt_ztcs_regis_warty-docnr.
*      ENDIF.

      LOOP AT gt_model ASSIGNING <lfs_model>.
        READ TABLE lt_mat INTO ls_mat
        WITH KEY matnr = <lfs_model>-matnr
                 sernr = <lfs_model>-sernr.
        IF sy-subrc EQ 0.
          <lfs_model>-mfrpn = ls_mat-typbz.
          <lfs_model>-serge = ls_mat-serge.
        ENDIF.

        IF <lfs_model>-mfrpn IS INITIAL.
          READ TABLE lt_mara INTO ls_mara
          WITH KEY matnr = <lfs_model>-matnr.
          IF sy-subrc EQ 0.
            <lfs_model>-mfrpn = ls_mara-mfrpn.
          ENDIF.
        ENDIF.

*        READ TABLE lt_ztcs_regis_warty INTO ls_ztcs_regis_warty
*        WITH KEY matnr = <lfs_model>-matnr
*                 sernr = <lfs_model>-sernr.
*        IF sy-subrc EQ 0.
*          <lfs_model>-name1  = ls_ztcs_regis_warty-name1.
*          <lfs_model>-addr1  = ls_ztcs_regis_warty-addr1.
*          REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-name1 WITH ''.
*          REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-addr1 WITH ''.
*          <lfs_model>-zipcd  = ls_ztcs_regis_warty-zipcd.
*          <lfs_model>-phone  = ls_ztcs_regis_warty-phone.
*          REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-phone WITH '/'.
*          READ TABLE lt_addr INTO ls_addr
*          WITH KEY docnr = ls_ztcs_regis_warty-docnr.
*          IF sy-subrc EQ 0.
*            <lfs_model>-provic = ls_addr-provt.
*            <lfs_model>-addr2  = ls_addr-subdt.
*            <lfs_model>-addr3  = ls_addr-distt.
*          ELSE.
*            <lfs_model>-provic = ls_ztcs_regis_warty-provic.
*            <lfs_model>-addr2  = ls_ztcs_regis_warty-addr2.
*            <lfs_model>-addr3  = ls_ztcs_regis_warty-addr3.
*          ENDIF.
*
*          REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-provic WITH ''.
*          REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-addr2  WITH ''.
*          REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-addr3  WITH ''.
*        ENDIF.

        READ TABLE lt_ser03 INTO ls_tmp
        WITH KEY matnr = <lfs_model>-matnr
                 sernr = <lfs_model>-sernr.
        IF sy-subrc EQ 0.
          <lfs_model>-grdat = ls_tmp-datum.
          CLEAR : ls_tmp.
        ENDIF.

        LOOP AT lt_jest INTO ls_jest WHERE matnr = <lfs_model>-matnr AND
                                           sernr = <lfs_model>-sernr.

          CONDENSE <lfs_model>-statu. " CH01 T41K937399

          CONCATENATE <lfs_model>-statu ls_jest-txt30 INTO <lfs_model>-statu SEPARATED BY space.
          SHIFT <lfs_model>-statu LEFT DELETING LEADING space.
          CLEAR : ls_jest.
        ENDLOOP.

        REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-statu WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX ',' IN <lfs_model>-grdat WITH ''.


      ENDLOOP.
    ENDIF.

  ENDFORM.                    " F_GET_ADDTIONAL_DATA

**&---------------------------------------------------------------------*
**&      Form  F_GET_DATA_CHANGE_FROM_Z
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*  FORM f_get_data_change_from_z.
*
*    DATA : lt_tmp LIKE gt_equi,
*           ls_tmp LIKE LINE OF lt_tmp.
*
*    DATA : ls_equi LIKE LINE OF gt_equi.
*
*    IF p_ztab EQ 'X'.
*      SELECT equi~equnr                   " Equipment Number
*             equi~matnr                   " Material Number
*             equi~sernr                   " Serial number
*             equi~erdat                   " Date on Which Record Was Created
*             equi~aedat                   " Changed On
*             equi~objnr                   " Object number
*        FROM ztcs_regis_warty
*        INNER JOIN equi ON ztcs_regis_warty~matnr EQ equi~matnr AND
*                           ztcs_regis_warty~sernr EQ equi~sernr
*        INTO TABLE lt_tmp
*       WHERE ztcs_regis_warty~matnr IN s_matnr
*         AND ztcs_regis_warty~sernr IN s_sernr
*         AND ztcs_regis_warty~erdat IN s_exdat
*         AND ztcs_regis_warty~statc NE 'LETTER'
*         AND equi~equnr             IN s_equnr.
*      IF p_letr EQ 'X'.
*        SELECT equi~equnr                   " Equipment Number
*               equi~matnr                   " Material Number
*               equi~sernr                   " Serial number
*               equi~erdat                   " Date on Which Record Was Created
*               equi~aedat                   " Changed On
*               equi~objnr                   " Object number
*          FROM ztcs_regis_warty
*          INNER JOIN equi ON ztcs_regis_warty~matnr EQ equi~matnr AND
*                             ztcs_regis_warty~sernr EQ equi~sernr
*          INTO TABLE lt_tmp
*         WHERE ztcs_regis_warty~matnr IN s_matnr
*           AND ztcs_regis_warty~sernr IN s_sernr
*           AND ztcs_regis_warty~erdat IN s_exdat
*           AND ztcs_regis_warty~statc EQ 'LETTER'
*           AND equi~equnr             IN s_equnr.
*      ENDIF.
*    ELSEIF p_letr EQ 'X'.
*      SELECT equi~equnr                   " Equipment Number
*             equi~matnr                   " Material Number
*             equi~sernr                   " Serial number
*             equi~erdat                   " Date on Which Record Was Created
*             equi~aedat                   " Changed On
*             equi~objnr                   " Object number
*        FROM ztcs_regis_warty
*        INNER JOIN equi ON ztcs_regis_warty~matnr EQ equi~matnr AND
*                           ztcs_regis_warty~sernr EQ equi~sernr
*        INTO TABLE lt_tmp
*       WHERE ztcs_regis_warty~matnr IN s_matnr
*         AND ztcs_regis_warty~sernr IN s_sernr
*         AND ztcs_regis_warty~erdat IN s_exdat
*         AND ztcs_regis_warty~statc EQ 'LETTER'
*         AND equi~equnr             IN s_equnr.
*    ELSE.
*      SELECT equi~equnr                   " Equipment Number
*             equi~matnr                   " Material Number
*             equi~sernr                   " Serial number
*             equi~erdat                   " Date on Which Record Was Created
*             equi~aedat                   " Changed On
*             equi~objnr                   " Object number
*        FROM ztcs_regis_warty
*        INNER JOIN equi ON ztcs_regis_warty~matnr EQ equi~matnr AND
*                           ztcs_regis_warty~sernr EQ equi~sernr
*        INTO TABLE lt_tmp
*       WHERE ztcs_regis_warty~matnr IN s_matnr
*         AND ztcs_regis_warty~sernr IN s_sernr
*         AND ztcs_regis_warty~erdat IN s_exdat
*         AND equi~equnr             IN s_equnr.
*    ENDIF.
*
*    LOOP AT lt_tmp INTO ls_tmp WHERE matnr IS NOT INITIAL AND
*                                     sernr IS NOT INITIAL.
*      READ TABLE gt_equi INTO ls_equi
*      WITH KEY matnr = ls_tmp-matnr
*               sernr = ls_tmp-sernr.
*      IF sy-subrc NE 0.
*        APPEND ls_tmp TO gt_equi.
*      ENDIF.
*      CLEAR : ls_tmp.
*    ENDLOOP.
*
*    SORT gt_equi.
*    DELETE ADJACENT DUPLICATES FROM gt_equi.
*
*  ENDFORM.                    " F_GET_DATA_CHANGE_FROM_Z
