FUNCTION-POOL zgf_zvfi_0075              MESSAGE-ID sv.


* INCLUDE LZGF_ZVFI_0075D...                 " Local class definition
INCLUDE lsvimdat                                . "general data decl.
*  INCLUDE LZGF_ZVFI_0075T00                      . "view rel. data dcl.
INCLUDE zgf_zvfi_0075t00                        . "view rel. data dcl.

**//.. Tipos
TYPES: BEGIN OF ty_extract.
    INCLUDE STRUCTURE zvfi_0075.
    INCLUDE STRUCTURE vimflagtab.
TYPES: END OF ty_extract.
