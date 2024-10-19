FUNCTION-POOL zfi_0028.                     "MESSAGE-ID ..
DATA: ls_0074         TYPE ztfi_0074,
      lc_mnttotal     TYPE char20,
      lc_bldat        TYPE char10,
      lv_nombreprovee TYPE string,
      lv_tratamiento  TYPE ad_titletx,
      lv_bldat        TYPE ztfi_0074-bldat,
      lv_mnttotal     TYPE ztfi_0074-mnttotal,
      lv_waers        TYPE ztfi_0074-waers,
      lv_ebeln        TYPE ztfi_0074-ebeln,
      ls_0078         TYPE ztfi_0078,
      lc_lifnr        TYPE lfa1-lifnr,
      lc_bukrs        TYPE ztfi_0074-bukrs,
      lc_xblnr        TYPE ztfi_0074-xblnr,
      lc_tipodte      TYPE ztfi_0074-tipodte,
      lc_glosa        TYPE zglosa.

* INCLUDE LZFI_0028D...                      " Local class definition
