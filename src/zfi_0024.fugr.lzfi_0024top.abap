FUNCTION-POOL zfi_0024.                     "MESSAGE-ID ..

TABLES:
  ztfi_0074,
  ztfi_0075,
  ztfi_0076,
  ztfi_0077.

TYPES: ty_t_zdt_recfactprov TYPE TABLE OF ztfi_0075,
       ty_t_zdt_recref      TYPE TABLE OF ztfi_0077,
       ty_t_zdte_dias       TYPE TABLE OF ztfi_0076,
       ty_t_zdt_otroimp     TYPE TABLE OF ztfi_0090.

TYPES: ty_boolean TYPE c.

CONSTANTS: co_true  TYPE c VALUE 'T',
           co_false TYPE c VALUE 'F'.

CONSTANTS: co_taxnr TYPE t001z-party VALUE 'TAXNR'.

CONSTANTS: co_zid_idoc             TYPE edidc-idoctp VALUE 'ZID_DTE_FELEC',
           co_z1mm_dte_encabezado  TYPE edidd-segnam VALUE 'Z1MM_DTE_ENCABEZADO',
           co_z1mm_dte_encabezado2 TYPE edidd-segnam VALUE 'Z1MM_DTE_ENCABEZADO2',
           co_z1mm_dte_detalle     TYPE edidd-segnam VALUE 'Z1MM_DTE_DETALLE',
           co_z1mm_dte_det_codl    TYPE edidd-segnam VALUE 'Z1MM_DTE_DET_CODL',
           co_z1mm_dte_refer       TYPE edidd-segnam VALUE 'Z1MM_DTE_REFER',
           co_z1mm_dte_subtot      TYPE edidd-segnam VALUE 'Z1MM_DTE_SUBTOT',
           co_z1mm_dte_montopago   TYPE edidd-segnam VALUE 'Z1MM_DTE_MONTOPAGO',
           co_z1mm_dte_impret      TYPE edidd-segnam VALUE 'Z1MM_DTE_IMPRET',
           co_z1mm_dte_impotrmon   TYPE edidd-segnam VALUE 'Z1MM_DTE_IMPOTRMON',
           co_z1mm_dte_response    TYPE edidd-segnam VALUE 'Z1MM_DTE_RESPONSE',
           co_z1mm_dte_filename    TYPE edidd-segnam VALUE 'Z1MM_DTE_FILENAME',
           co_z1mm_dte_ted         TYPE edidd-segnam VALUE 'Z1MM_DTE_TED'.


DATA: wa_z1mm_dte_encabezado  TYPE z1mm_dte_encabezado,
      wa_z1mm_dte_encabezado2 TYPE z1mm_dte_encabezado2,
      wa_z1mm_dte_detalle     TYPE z1mm_dte_detalle,
      wa_z1mm_dte_det_codl    TYPE z1mm_dte_det_codl,
      wa_z1mm_dte_refer       TYPE z1mm_dte_refer,
      wa_z1mm_dte_subtot      TYPE z1mm_dte_subtot,
      wa_z1mm_dte_montopago   TYPE z1mm_dte_montopago,
      wa_z1mm_dte_impret      TYPE z1mm_dte_impret,
      wa_z1mm_dte_impotrmon   TYPE z1mm_dte_impotrmon,
      wa_z1mm_dte_filename    TYPE z1mm_dte_filename,
      wa_z1mm_dte_ted         TYPE z1mm_dte_ted.

DATA: wa_z1mm_dte_response    TYPE z1mm_dte_response.

DATA: gv_docnum   LIKE edidd-docnum.

DATA: it_detalle      TYPE ty_t_zdt_recfactprov,
      it_dias         TYPE ty_t_zdte_dias,
      it_otroimpuesto TYPE ty_t_zdt_otroimp,
      it_recref       TYPE ty_t_zdt_recref.

DATA: wa_cabecera     TYPE ztfi_0074,
      wa_tfi_0074fr   TYPE ztfi_0074fr,
      wa_detalle      TYPE ztfi_0075,
      wa_dias         TYPE ztfi_0076,
      wa_otroimpuesto TYPE ztfi_0090,
      wa_recref       TYPE ztfi_0077,
      wa_refhes       TYPE ztfi_0077.

CONSTANTS: co_801 TYPE z1mm_dte_refer-tpodocref VALUE '801'.
CONSTANTS: co_803 TYPE z1mm_dte_refer-tpodocref VALUE '802'.
CONSTANTS: co_hes TYPE z1mm_dte_refer-tpodocref VALUE 'HES'.

CONSTANTS: co_uno                TYPE c              VALUE '1',
           co_z_dte_felec        TYPE edidc-mestyp   VALUE 'ZDTE_FELEC',
           co_zid_dte_felec_resp TYPE edidc-idoctp   VALUE 'ZID_DTE_FELEC_RESP'.

CONSTANTS: co_53 TYPE  bdidocstat-status VALUE '53',
           co_51 TYPE  bdidocstat-status VALUE '51'.
CONSTANTS: co_zfi_0001 TYPE  bdidocstat-msgid  VALUE 'ZFI_0001',
           co_zfi_0003 TYPE  bdidocstat-msgid  VALUE 'ZDTE_0001'.
CONSTANTS: co_120 TYPE  bdidocstat-msgno  VALUE '120',
           co_121 TYPE  bdidocstat-msgno  VALUE '121',
           co_122 TYPE  bdidocstat-msgno  VALUE '122',
           co_045 TYPE  bdidocstat-msgno  VALUE '045',
           co_003 TYPE  bdidocstat-msgno  VALUE '003'.

CONSTANTS: co_clp       TYPE  t001-waers VALUE 'CLP'.
CONSTANTS: co_z400      TYPE  lfa1-ktokk VALUE 'Z400'.
CONSTANTS: co_800000    TYPE  lfa1-lifnr VALUE '0000800000'.

CONSTANTS: co_rcvprn TYPE edidc-rcvprn VALUE 'OPENDTE',
           co_sndprt TYPE edidc-sndprt VALUE 'LS'.
DATA: dte_relevante(1) TYPE c.
DATA: gs_bal_log        TYPE bal_s_log,
      gs_bal_msg        TYPE bal_s_msg,
      go_log            TYPE REF TO zcl_bal_log_dte,
      go_dbnet          TYPE REF TO zcl_dte_recepcion,
      lv_rutempresa(12) TYPE c.
DATA: so_bukrs TYPE RANGE OF bukrs WITH HEADER LINE.
DATA:  wa_0078 TYPE ztfi_0078.
