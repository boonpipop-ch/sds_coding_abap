*&---------------------------------------------------------------------*
*& Include          ZSDSCOR0010_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   T A B L E S                                                        *
*----------------------------------------------------------------------*
TABLES: aufk, setheader.

*----------------------------------------------------------------------*
*  Include                                                    *
*----------------------------------------------------------------------*
##INCL_OK INCLUDE mkpt4d10.

*----------------------------------------------------------------------*
*  Constant                                                            *
*----------------------------------------------------------------------*
CONSTANTS: gc_kokrs_1000 TYPE codia-kokrs VALUE '1000',
           gc_versn_000  TYPE versn VALUE '000',
           gc_versn_z05  TYPE versn VALUE 'Z05'.

TYPES: BEGIN OF ty_aufk,
         objnr TYPE aufk-objnr,
         aufnr TYPE aufk-aufnr,
         ktext TYPE aufk-ktext,
         auart TYPE aufk-auart,
         aufex TYPE aufk-aufex,
         akstl TYPE aufk-akstl,
         kostv TYPE aufk-kostv,
       END OF ty_aufk.

TYPES: BEGIN OF ty_cosp_rept,
         gjahr  TYPE gjahr,
         poper  TYPE poper,
         kstar  TYPE kstar,
         txt20  TYPE skat-txt20,
         waers  TYPE waers,
         actual TYPE wogxxx,
         commit TYPE wogxxx,
         assign TYPE wogxxx,
         plan   TYPE wogxxx,
         avai   TYPE wogxxx,
       END OF ty_cosp_rept.

TYPES: BEGIN OF g_type_s_master.
         INCLUDE TYPE ty_aufk.
TYPES:  " hotspot    TYPE icon_d,
         t_celltype TYPE salv_t_int4_column,
         expand     TYPE char01,
       END OF g_type_s_master,

       BEGIN OF g_type_s_slave.
         INCLUDE TYPE ty_aufk.
         INCLUDE TYPE ty_cosp_rept.
TYPES END OF g_type_s_slave.

TYPES: BEGIN OF g_type_s_result,
         icon   TYPE icon,
         aufnr  TYPE aufk-aufnr,
         ktext  TYPE aufk-ktext,
         aufex  TYPE aufk-aufex,
         kstar  TYPE kstar,
         tcode  TYPE tcode,
         logmsg TYPE msgtx,
         msgtx  TYPE msgtx,
       END OF g_type_s_result.

TYPES: gty_slave TYPE STANDARD TABLE OF g_type_s_slave.
  TYPES: BEGIN OF ty_cosp,
           lednr  TYPE v_cosp_view-lednr,
           versn  TYPE v_cosp_view-versn,
           objnr  TYPE v_cosp_view-objnr,
           wrttp  TYPE v_cosp_view-wrttp,
           gjahr  TYPE v_cosp_view-gjahr,
           kstar  TYPE v_cosp_view-kstar,
           twaer  TYPE v_cosp_view-twaer,
           wog001 TYPE wogxxx,
           wog002 TYPE wogxxx,
           wog003 TYPE wogxxx,
           wog004 TYPE wogxxx,
           wog005 TYPE wogxxx,
           wog006 TYPE wogxxx,
           wog007 TYPE wogxxx,
           wog008 TYPE wogxxx,
           wog009 TYPE wogxxx,
           wog010 TYPE wogxxx,
           wog011 TYPE wogxxx,
           wog012 TYPE wogxxx,
         END OF ty_cosp.

*... §5 Definition is later
CLASS lcl_handle_events DEFINITION DEFERRED.


##NEEDED DATA: gt_master TYPE STANDARD TABLE OF g_type_s_master,
##NEEDED      gt_slave  TYPE STANDARD TABLE OF g_type_s_slave,
##NEEDED      gt_result TYPE STANDARD TABLE OF g_type_s_result.

##NEEDED DATA: gr_hierseq TYPE REF TO cl_salv_hierseq_table.

*... §5 object for handling the events of cl_salv_table
##NEEDED DATA: gr_events TYPE REF TO lcl_handle_events.
