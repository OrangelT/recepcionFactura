FUNCTION-POOL zgf_ztfi_0089              MESSAGE-ID sv.

* INCLUDE LZGF_ZTFI_0089D...                 " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzgf_ztfi_0089t00                       . "view rel. data dcl.

TYPES: BEGIN OF ty_extract.
    INCLUDE STRUCTURE ztfi_0089.
    INCLUDE STRUCTURE vimflagtab.
TYPES: END OF ty_extract.
