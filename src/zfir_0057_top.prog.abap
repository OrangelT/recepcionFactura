*&---------------------------------------------------------------------*
*& Include ZFIR_0057_TOP
*&
*&---------------------------------------------------------------------*

REPORT ZFIR_0057.

INCLUDE <icon>.

TYPE-POOLS: slis.

TABLES: ztfi_0074,
        ztfi_0079,
        ztfi_0074fr.

TYPES: BEGIN OF ty_log,
         estado TYPE icon_d,
         documento TYPE char20,
         sociedad TYPE bukrs,
         proveedor TYPE lfa1-lifnr,
         fecha TYPE sydatum,
         mensaje TYPE char256,
       END OF ty_log.
DATA: ti_log TYPE STANDARD TABLE OF ty_log WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

DATA: ti_ztfi_0083 TYPE STANDARD TABLE OF ztfi_0083 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR  ztfi_0074-bukrs OBLIGATORY,
                s_fecha FOR  ztfi_0074-bldat,
                s_folio FOR ztfi_0074-xblnr,
                s_status FOR ztfi_0079-estatus OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


TYPES: BEGIN OF ty_t001,
            bukrs TYPE t001-bukrs,
            wfvar TYPE t001-wfvar,
         END OF ty_t001.
DATA: ti_vbwf16 TYPE STANDARD TABLE OF vbwf16 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
DATA: lt_t001 TYPE STANDARD TABLE OF ty_t001 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

DATA: ti_ztfi_0085 TYPE STANDARD TABLE OF ztfi_0085 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
