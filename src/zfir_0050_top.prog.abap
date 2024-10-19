*&---------------------------------------------------------------------*
*& Include          ZFIR_0050_TOP
*&---------------------------------------------------------------------*
REPORT zfir_0050.

*&******************************************************************** *
*                           T A B L E S                                *
*&******************************************************************** *
*
TABLES: t001,
        bkpf.

*&******************************************************************** *
*                  T A B L A S   I N T E R N A S                       *
*&******************************************************************** *

*&******************************************************************** *
*                 V A R I A B L E S   G L O B A L E S                  *
*&******************************************************************** *
TYPES: BEGIN OF z_sociedad,
         bukrs TYPE bukrs,
       END OF z_sociedad.
TYPES: BEGIN OF zt_variante,
         vatint TYPE zvarint,
       END OF zt_variante.
DATA: gr_dte_recep TYPE REF TO object. "zcl_dte_recepcion.
DATA: co_type TYPE string.
DATA:  t_0078 TYPE STANDARD TABLE OF ztfi_0078.
DATA: wa_0078 TYPE ztfi_0078.
DATA:  itabv  TYPE STANDARD TABLE OF zt_variante.
DATA:  wa_itabv TYPE zt_variante.
DATA: gr_dte_sii TYPE REF TO object. "zcl_sii.
DATA: go_log TYPE REF TO zcl_bal_log_dte.
DATA: gs_bal_log TYPE bal_s_log.
DATA: gs_bal_msg TYPE bal_s_msg.
DATA: lt_zcb_recfactprovfechasii TYPE STANDARD TABLE OF ztfi_0074fr.
DATA: wa TYPE ztfi_0074fr.
DATA: lt_zcb_recfactprovfechasii_soc TYPE STANDARD TABLE OF ztfi_0074fr.
DATA: cant TYPE i.
DATA: itab TYPE STANDARD TABLE OF z_sociedad.
DATA: wa_itab TYPE z_sociedad.


*&******************************************************************** *
*            S E L E C T I  O N  -  S C R E E N                        *
*&******************************************************************** *
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_bukrs FOR t001-bukrs OBLIGATORY. " Sociedad

PARAMETERS: pa_fldir TYPE flag AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blk1.
