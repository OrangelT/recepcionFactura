FUNCTION-POOL ZGF_ZTFI_0076              MESSAGE-ID SV.

* INCLUDE LZGF_ZTFI_0076D...                 " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
**  INCLUDE LZGF_ZTFI_0076T00                     . "view rel. data dcl.
  INCLUDE ZGF_ZTFI_0076T00                        . "view rel. data dcl.

**//.. Tipos
  TYPES: BEGIN OF ty_extract.
          INCLUDE STRUCTURE ztfi_0076.
          INCLUDE STRUCTURE vimflagtab.
  TYPES: END OF ty_extract.
