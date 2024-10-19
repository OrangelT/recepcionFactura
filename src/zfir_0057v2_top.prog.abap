*&---------------------------------------------------------------------*
*&  Include           ZFIR_0057V2_TOP
*&---------------------------------------------------------------------*
INCLUDE <icon>.

TYPE-POOLS: slis.

TABLES: ztfi_0074,
        ztfi_0079,
        ztfi_0074fr.

TYPES: BEGIN OF ty_log,
         estado    TYPE icon_d,
         documento TYPE char20,
         sociedad  TYPE bukrs,
         proveedor TYPE lfa1-lifnr,
         fecha     TYPE sydatum,
         mensaje   TYPE char256,
       END OF ty_log.
DATA: ti_log TYPE STANDARD TABLE OF ty_log WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
DATA: gt_t001 TYPE STANDARD TABLE OF t001 .
DATA: gs_t001 TYPE t001.

TYPES: BEGIN OF zt_variante,
         vatint TYPE zvarint,
       END OF zt_variante.

DATA: wa_objeto TYPE REF TO  object.

DATA: itabv  TYPE STANDARD TABLE OF zt_variante.
DATA: wa_itabv TYPE zt_variante.
DATA: co_type TYPE string.
DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
DATA: wa_0078 TYPE ztfi_0078.
