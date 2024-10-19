FUNCTION-POOL ZGF_ZVFI_0074              MESSAGE-ID SV.


* INCLUDE LZGF_ZVFI_0074D...                 " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
*  INCLUDE LZGF_ZVFI_0074T00                       . "view rel. data dcl.
  INCLUDE ZGF_ZVFI_0074T00                       . "view rel. data dcl.

**//.. Tipos
  TYPES: BEGIN OF ty_extract.
          INCLUDE STRUCTURE zvfi_0074.
          INCLUDE STRUCTURE vimflagtab.
  TYPES: END OF ty_extract.

data: tab_fi_0074_old type standard table of ty_extract.
data: tab_fi_0074_md type standard table of ty_extract.
