*&---------------------------------------------------------------------*
*&  Include           ZFIR_0055_TOP
*&---------------------------------------------------------------------*
REPORT zfir_0056.

TABLES: ztfi_0074,
        ztfi_0074fr,
        lfa1.
TYPES:
  BEGIN OF ty_mod,
    icono         TYPE icon_d,
    documento(30) TYPE c,
    mensaje(200)  TYPE c,
  END OF ty_mod .


DATA wa_objeto TYPE REF TO zcl_dte_fechac.
DATA: lv_mensaje TYPE char200.
DATA: gt_t001 TYPE STANDARD TABLE OF t001 .
DATA: gs_t001 TYPE t001.
DATA: lt_logt TYPE STANDARD TABLE OF ty_mod.
