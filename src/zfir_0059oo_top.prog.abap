*&---------------------------------------------------------------------*
*&  Include           ZFIR_0059OO_TOP
*&---------------------------------------------------------------------*
REPORT zfir_0059oo.

TABLES: ztfi_0074,
        usr21,
        wrf_ppw_us_usgp,
        lfb1,
        lfa1.

TYPES: BEGIN OF zt_variante,
         vatint TYPE zvarint,
       END OF zt_variante.

DATA alv_table      TYPE REF TO cl_salv_table.
DATA alv_columns    TYPE REF TO cl_salv_columns_table.
DATA single_column  TYPE REF TO cl_salv_column.
DATA wa_objeto TYPE REF TO object . "zcl_dte_monitor.
DATA: ti_salida TYPE zdte_t_monitor_salida.
DATA: lv_mensaje TYPE char200.

DATA: itabv  TYPE STANDARD TABLE OF zt_variante.
DATA: wa_itabv TYPE zt_variante.
DATA: co_type TYPE string.
DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
DATA: wa_0078 TYPE ztfi_0078.
DATA: v_flag, v_flag1.
