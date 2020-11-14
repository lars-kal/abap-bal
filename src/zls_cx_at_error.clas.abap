class zls_cx_at_error definition
  public
  inheriting from cx_no_check
  create public .

  public section.

    data i18n_txt type string.

    class-data hlp type ref to zls_cl_at_main.
    class-methods class_constructor.

    data ms_txt type hlp->ty_s-txt.


    methods constructor
      importing
        textid   like textid optional
        previous like previous optional
        txt      type any optional
          preferred parameter txt.
  protected section.
  private section.
ENDCLASS.



CLASS ZLS_CX_AT_ERROR IMPLEMENTATION.


  method class_constructor.
    hlp = new #(  ).
  endmethod.


  method constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid = textid previous = previous ).

*     ms_txt = hlp->text( txt )->get( txt  ).


   endmethod.
ENDCLASS.
