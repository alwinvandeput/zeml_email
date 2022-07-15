CLASS ltcl_ DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      format_decimal FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_ IMPLEMENTATION.

  METHOD format_decimal.

*    DATA(act_value) =
*      zeml_value_formatter=>format_decimal(
*        country_code    = 'US'
*        decimal_value   = CONV vbap-kwmeng( '12345678.456' )
*        decimal_count   = 2 ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = act_value
*      exp = |12,345,678.46| ).
*
*    act_value =
*      zeml_value_formatter=>format_decimal(
*        country_code    = 'NL'
*        decimal_value   = CONV vbap-kwmeng( '12345678.456' )
*        decimal_count   = 2 ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = act_value
*      exp = |12.345.678,46| ).

  ENDMETHOD.

ENDCLASS.
