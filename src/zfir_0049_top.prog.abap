*&---------------------------------------------------------------------*
*& Include          ZFIR_0049_TOP
*&---------------------------------------------------------------------*


*&******************************************************************** *
*                           T A B L E S                                *
*&******************************************************************** *
TABLES: t001.

*&******************************************************************** *
*                             T Y P E S                                *
*&******************************************************************** *
TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_t001.
TYPES: BEGIN OF zt_variante,
         vatint TYPE zvarint,
       END OF zt_variante.


*&******************************************************************** *
*                  T A B L A S   I N T E R N A S                       *
*&******************************************************************** *
DATA: gt_t001 TYPE TABLE OF ty_t001.

*&******************************************************************** *
*                 V A R I A B L E S   G L O B A L E S                  *
*&******************************************************************** *

DATA: gr_dte_recep TYPE REF TO  object. "zcl_dte_recepcion.

DATA: itabv  TYPE STANDARD TABLE OF zt_variante.
DATA: wa_itabv TYPE zt_variante.
DATA: co_type TYPE string.
DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
DATA: wa_0078 TYPE ztfi_0078.
DATA: gs_t001 TYPE ty_t001.
DATA: lo_err  TYPE REF TO cx_root.
DATA: co_tran TYPE sy-tcode VALUE 'ZDTE_0027'.
