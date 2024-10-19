*&---------------------------------------------------------------------*
*& Include ZFIR_0058_TOP
*&
*&---------------------------------------------------------------------*

REPORT ZFIR_0058.

TABLES ztfi_0074.

INCLUDE <icon>.

TYPE-POOLS: slis.

TYPES: BEGIN OF ty_log,
         estado TYPE icon_d,
         documento TYPE char20,
         sociedad TYPE bukrs,
         proveedor TYPE lfa1-lifnr,
         fecha TYPE sydatum,
         mensaje TYPE char256,
      END OF ty_log.


DATA: ti_log TYPE STANDARD TABLE OF ty_log WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR  ztfi_0074-bukrs OBLIGATORY,
                s_fecha FOR  ztfi_0074-bldat,
                s_folio FOR ztfi_0074-xblnr,
                s_status FOR ztfi_0074-status OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
