*"* use this source file for your ABAP unit test classes

CLASS unit_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS format_number_nl FOR TESTING.
    METHODS format_number_us FOR TESTING.

ENDCLASS.       "unit_Test

CLASS zeml_abap_transform_prc DEFINITION
  LOCAL FRIENDS unit_test.


CLASS unit_test IMPLEMENTATION.

  METHOD format_number_nl.

    DATA(lo_transform) = zeml_abap_transform_prc_ft=>get_factory( )->get_abap_transform_prc(
      iv_class_name  = 'ZEML_ABAP_TRANSFORM_PRC'
      ia_content     = ''
      is_labels      = ''
      is_settings    =
        VALUE #(
          country_key            = 'NL'
          language_key           = 'N'
          currency_key           = 'EUR'
          currency_decimal_count = 2
*
*          number_format          TYPE t005x-xdezp,
*          date_format            TYPE t005x-datfm,
*          time_format            TYPE t005x-timefm,
*          user_name              TYPE syst-uname,
*
*          input_country_key      TYPE t005x-land,
*          input_language_key     TYPE syst-langu,
*          input_user_name        TYPE syst-uname,
*          system_user_name       TYPE syst-uname,
        )
       ).

    DATA lv_source_number TYPE p DECIMALS 4.
    lv_source_number = '1234.567'.

    "Format number - 2 decimals
    DATA(lv_act_number) =
      lo_transform->format_number(
        ia_number        = lv_source_number
        iv_decimal_count = 2 ).

    DATA(lv_exp_number) = |1.234,57|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "Format number - 1 decimals
    lv_act_number =
      lo_transform->format_number(
        ia_number        = lv_source_number
        iv_decimal_count = 1 ).

    lv_exp_number = |1.234,6|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "Format number - 0 decimals
    lv_act_number =
      lo_transform->format_number(
        ia_number        = lv_source_number
        iv_decimal_count = 0 ).

    lv_exp_number = |1.235|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "Format amount
    lv_act_number =
      lo_transform->format_amount( lv_source_number ).

    lv_exp_number = |1.234,57|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "--------------------------------------
    "Format amount with symbol
    lv_act_number =
      lo_transform->format_amount_w_symbol( lv_source_number ).

    lv_exp_number = |€ 1.234,57|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "--------------------------------------
    "Format date
    DATA(lv_date_text) = lo_transform->format_date( '20200522' ).

    cl_abap_unit_assert=>assert_equals(
      act  = lv_date_text
      exp  = '22.05.2020' ).

    "--------------------------------------
    "Format time
    DATA(lv_time_text) = lo_transform->format_time( '164850' ).

    cl_abap_unit_assert=>assert_equals(
      act  = lv_time_text
      exp  = '16:48:50' ).

  ENDMETHOD.


  METHOD format_number_us.

    DATA(lo_transform) = zeml_abap_transform_prc_ft=>get_factory( )->get_abap_transform_prc(
      iv_class_name  = 'ZEML_ABAP_TRANSFORM_PRC'
      ia_content     = ''
      is_labels      = ''
      is_settings    =
        VALUE #(
          country_key            = 'US'
          language_key           = 'E'
          currency_key           = 'USD'
          currency_decimal_count = 2
*
*          number_format          TYPE t005x-xdezp,
*          date_format            TYPE t005x-datfm,
*          time_format            TYPE t005x-timefm,
*          user_name              TYPE syst-uname,
*
*          input_country_key      TYPE t005x-land,
*          input_language_key     TYPE syst-langu,
*          input_user_name        TYPE syst-uname,
*          system_user_name       TYPE syst-uname,
        )
       ).

    DATA lv_source_number TYPE p DECIMALS 4.
    lv_source_number = '1234.567'.

    DATA(lv_act_number) =
      lo_transform->format_number(
        ia_number        = lv_source_number
        iv_decimal_count = 2 ).

    DATA(lv_exp_number) = |1,234.57|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "--------------------------------------
    "Format amount with symbol
    lv_act_number =
      lo_transform->format_amount_w_symbol( lv_source_number ).

    lv_exp_number = |$1,234.57|.

    cl_abap_unit_assert=>assert_equals(
      act  = lv_act_number
      exp  = lv_exp_number ).

    "--------------------------------------
    "Format date
    DATA(lv_date_text) = lo_transform->format_date( '20200522' ).

    cl_abap_unit_assert=>assert_equals(
      act  = lv_date_text
      exp  = '05/22/2020' ).

    "--------------------------------------
    "Format time
    DATA(lv_time_text) = lo_transform->format_time( '164850' ).

    cl_abap_unit_assert=>assert_equals(
      act  = lv_time_text
      exp  = '04:48:50 PM' ).


  ENDMETHOD.

ENDCLASS.
