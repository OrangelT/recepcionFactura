class ZCL_XML_DOCUMENT definition
  public
  inheriting from CL_XML_DOCUMENT
  final
  create public .

public section.
protected section.

  methods RENDER
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_XML_DOCUMENT IMPLEMENTATION.


METHOD render.
    retcode = c_failed.

    CHECK NOT m_document IS INITIAL.
    CHECK NOT stream IS INITIAL.

    " render the DOM back into an output stream ---
    stream->set_pretty_print( pretty_print = pretty_print ).

    DATA: l_renderer TYPE REF TO if_ixml_renderer.

    l_renderer = g_ixml->create_renderer( document = m_document
                                          ostream  = stream ).

    IF pretty_print = abap_true.
      l_renderer->set_normalizing( abap_true ). " pretty print does not work if normalizing is off!!!
    ELSE.
      l_renderer->set_normalizing( space ).
    ENDIF.

    retcode = l_renderer->render( ).
  ENDMETHOD.
ENDCLASS.
