FUNCTION-POOL zfi_0023.                     "MESSAGE-ID ..

* INCLUDE LZFI_0023D...                      " Local class definition

DATA: gv_bukrs TYPE bkpf-bukrs,
      gv_tipo  TYPE zfi_tipo,
      gv_act   TYPE c,
      ls_tipo  TYPE zfi_tipo.
DATA: BEGIN OF datos9000,
        analista          TYPE znombana,
        aprobador         TYPE xubname,
        zzdtcta           TYPE zfi_dtcta,
      END OF datos9000.
DATA: ls_bname TYPE usr02-bname.
DATA: ls_dtcta TYPE zfi_dtcta.
