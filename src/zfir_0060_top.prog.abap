*&---------------------------------------------------------------------*
*& Include ZFIR_0060_TOP
*&
*&---------------------------------------------------------------------*
REPORT   zfir_0060.

*&******************************************************************** *
*                           T A B L E S                                *
*&******************************************************************** *
TABLES: ztfi_0074,
        t001,
        lfa1,
        lfb1.

*&******************************************************************** *
*                  G R U P O S    T I P O S                            *
*&******************************************************************** *
TYPE-POOLS: slis,
            icon,
            abap.

*&******************************************************************** *
*                           T I P O S                                  *
*&******************************************************************** *
**//..
TYPES: BEGIN OF ty_lfb1,
        lifnr     LIKE lfb1-lifnr,
        bukrs     LIKE lfb1-bukrs,
        zdte_tipo LIKE lfb1-zdte_tipo,
        name1     LIKE lfa1-name1,
       END OF ty_lfb1,

       tt_lfb1 TYPE TABLE OF ty_lfb1.

**//..
TYPES: BEGIN OF ty_salida,
        bukrs     LIKE lfb1-bukrs,
        gsber     LIKE bseg-gsber,
        zdte_tipo LIKE lfb1-zdte_tipo,
        tipodte   LIKE ztfi_0074-tipodte,
        recib     TYPE i,
        recha     TYPE i,
        okcoc     TYPE i,
        tmpacu    TYPE p DECIMALS 2, " tiempo acumulado
        tmpcon    TYPE i,            " tiempo contado
        tmprom    TYPE p DECIMALS 2, " tiempo promedio
       END OF ty_salida,

       tt_salida TYPE TABLE OF ty_salida.

**//..
TYPES: BEGIN OF ty_bkpf,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        bstat TYPE bkpf-bstat,
        budat TYPE bkpf-budat,
        bldat TYPE bkpf-bldat,
        cpudt TYPE bkpf-cpudt,
        cputm TYPE bkpf-cputm,
        awtyp TYPE bkpf-awtyp,
        awkey TYPE bkpf-awkey,
        awsys TYPE bkpf-awsys,
       END OF ty_bkpf,

       tt_bkpf TYPE TABLE OF ty_bkpf.

**//..
TYPES: BEGIN OF ty_bseg,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        buzei TYPE bseg-buzei,
        lifnr TYPE bseg-lifnr,
        gsber TYPE bseg-gsber,
       END OF ty_bseg,

       tt_bseg TYPE TABLE OF ty_bseg.

**//..
TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_t001,

       tt_t001 TYPE TABLE OF ty_t001.

**//..
TYPES: BEGIN OF ty_ztfi_0074,
         bukrs     TYPE ztfi_0074-bukrs,
         xblnr     TYPE ztfi_0074-xblnr,
         lifnr     TYPE ztfi_0074-lifnr,
         tipodte   TYPE ztfi_0074-tipodte,
         bldat     TYPE ztfi_0074-bldat,
         belnr     TYPE ztfi_0074-belnr,
         gjahr     TYPE ztfi_0074-gjahr,
         ebeln     TYPE ztfi_0074-ebeln,
         status    TYPE ztfi_0074-status,
         cpudt     TYPE ztfi_0074-cpudt,
         cputm     TYPE ztfi_0074-cputm,
         zdte_tipo TYPE lfb1-zdte_tipo,
       END OF ty_ztfi_0074.

TYPES: tt_ztfi_0074 TYPE TABLE OF ty_ztfi_0074.

**//..
TYPES: BEGIN OF ty_awkey,
         value TYPE bkpf-awkey,
       END OF ty_awkey.

*&******************************************************************** *
*                  T A B L A S   I N T E R N A S                       *
*&******************************************************************** *
DATA: gt_ztfi_0074 TYPE tt_ztfi_0074,
      gt_salida    TYPE tt_salida,
      gt_bkpf      TYPE tt_bkpf,
      gt_bseg      TYPE tt_bseg,
      gt_lfb1      TYPE tt_lfb1,
      gt_t001      TYPE tt_t001.

*&******************************************************************** *
*                 V A R I A B L E S   G L O B A L E S                  *
*&******************************************************************** *
DATA: vg_mens    TYPE char100,
      text_r(20).

FIELD-SYMBOLS: <fs_a> TYPE ANY,
               <fs_b> TYPE STANDARD TABLE.

*&******************************************************************** *
*                            R A N G O S                               *
*&******************************************************************** *

*&******************************************************************** *
*                        C O N S T A N T E S                           *
*&******************************************************************** *
CONSTANTS: co_koart TYPE koart VALUE 'K'.

*&******************************************************************** *
*         E S T R U C T U R A S   P A R A    E L    A L V              *
*&******************************************************************** *
DATA: lr_columns   TYPE REF TO cl_salv_columns_table,
      lr_column    TYPE REF TO cl_salv_column,
      lr_content   TYPE REF TO cl_salv_form_element,
      lr_layout    TYPE REF TO cl_salv_layout,
      ls_key       TYPE salv_s_layout_key,
      gr_functions TYPE REF TO cl_salv_functions_list.

**//.. Eventos
*CLASS: lcl_handle_events DEFINITION DEFERRED.
*DATA: gr_events        TYPE REF TO lcl_handle_events. " Evt.ALV Factory

**//.. reference to a functions object
DATA: gr_table     TYPE REF TO cl_salv_table.

*&******************************************************************** *
*            S E L E C T I  O N  -  S C R E E N                        *
*&******************************************************************** *
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-t01.
PARAMETERS: pa_bukrs LIKE t001-bukrs MEMORY ID buk NO-DISPLAY.

SELECT-OPTIONS: so_bukrs FOR t001-bukrs OBLIGATORY MEMORY ID buk,
                so_lifnr FOR ztfi_0074-lifnr,
                so_stcd1 FOR lfa1-stcd1,
                so_dtetp FOR lfb1-zdte_tipo,
                so_statu FOR ztfi_0074-status NO-DISPLAY,
                so_bldat FOR ztfi_0074-bldat,
                so_xblnr FOR ztfi_0074-xblnr NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK sel.
