*&---------------------------------------------------------------------*
*&  Include           ZFIR_0060_E01
*&---------------------------------------------------------------------*
*&******************************************************************** *
*                 I N I T I A L I Z A T I O N                          *
*&******************************************************************** *
INITIALIZATION.
**//.. Inicializar
  PERFORM fo_initialization.

**//.. Authority check
  PERFORM fo_authority_check USING sy-tcode.

*&******************************************************************** *
*              A T   S E L E C T I O N - S C R E E N                   *
*&******************************************************************** *
AT SELECTION-SCREEN.


AT SELECTION-SCREEN ON so_bukrs.
  PERFORM fo_validar_sociedad.

*&******************************************************************** *
*               S T A R T - O F - S E L E C T I O N                    *
*&******************************************************************** *
START-OF-SELECTION.
**//.. Buscar Info.
  PERFORM fo_get_info CHANGING vg_mens.
  IF vg_mens IS NOT INITIAL.
    MESSAGE vg_mens TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

**//.. Procesar Info.
  PERFORM fo_proc_info CHANGING vg_mens.
  IF vg_mens IS NOT INITIAL.
    MESSAGE vg_mens TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

*&******************************************************************** *
*                  E N D - O F - S E L E C T I O N                     *
*&******************************************************************** *
END-OF-SELECTION.
**//.. Imprimir ALV
  PERFORM fo_display_alv USING gt_salida.
