*"* use this source file for your ABAP unit test classes
class ltcl_unit_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      first_test for testing raising cx_static_check.
endclass.


class ltcl_unit_test implementation.

  method first_test.
    zls_cl_at_unit_test=>main(  ).
  endmethod.

endclass.
