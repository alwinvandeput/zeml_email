class ZEML_ABAP_TRANSFORM_UTIL definition
  public
  create public .

public section.

  types t_country_key type T005X-LAND .
  types t_currency_key type TCURC-WAERS .
  types:
    t_currency_position TYPE c LENGTH 5 .
  types:
    BEGIN OF t_currency_setting,
        country_key  TYPE t_country_key,
        currency_key type t_currency_key,
        symbol       TYPE string,
        position     TYPE t_currency_position,
      END OF t_currency_setting .
  types:
    t_currency_settomg_list TYPE STANDARD TABLE OF t_currency_setting WITH DEFAULT KEY .

  constants:
    BEGIN OF c_currency_position,
        left  TYPE t_currency_position VALUE 'LEFT',
        right TYPE t_currency_position VALUE 'RIGHT',
      END OF c_currency_position .
  class-data m_currency_list type t_currency_settomg_list .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_LEFT_CURRENCY_SYMBOL
    importing
      !IV_COUNTRY_KEY type STRING
      !IV_CURRENCY_KEY type STRING
    returning
      value(RV_SYMBOL) type STRING .
  class-methods GET_RIGHT_CURRENCY_SYMBOL
    importing
      !IV_COUNTRY_KEY type t_country_key
      !IV_CURRENCY_KEY type t_currency_key
    returning
      value(RV_SYMBOL) type STRING .
  class-methods GET_CURRENCY_SETTING
    importing
      !IV_COUNTRY_KEY type t_country_key
      !IV_CURRENCY_KEY type t_currency_key
    returning
      value(RS_CURRENCY_SETTINGS) type t_currency_setting .
  methods GET_NUMBER_FORMAT
    importing
      !IV_SAP_NUMBER_FORMAT type STRING
      !IV_DECIMAL_COUNT type I
      !IV_DECIMAL_CHARACTER type STRING
    returning
      value(RV_NUMBER_FORMAT) type STRING .
  methods GET_SAME_CHARACTERS
    importing
      !IV_DECIMAL_CHARACTER type STRING
      !IV_DECIMAL_COUNT type I
    returning
      value(RV_TEXT) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEML_ABAP_TRANSFORM_UTIL IMPLEMENTATION.


  METHOD CLASS_CONSTRUCTOR.

    "See website: https://en.wikipedia.org/wiki/Language_and_the_euro#Summary

    m_currency_list =
      VALUE #(
        ( country_key = 'US' currency_key = 'USD'  symbol = |$|  position = c_currency_position-left )
        ( country_key = 'US' currency_key = 'USDN' symbol = |$|  position = c_currency_position-left )
        ( country_key = 'GB' currency_key = 'EUR'  symbol = |€|  position = c_currency_position-left )
        ( country_key = 'FR' currency_key = 'EUR'  symbol = | €| position = c_currency_position-right )
        ( country_key = 'DE' currency_key = 'EUR'  symbol = | €| position = c_currency_position-right )
        ( country_key = 'NL' currency_key = 'EUR'  symbol = |€ | position = c_currency_position-left )
      ).

  ENDMETHOD.


  METHOD GET_CURRENCY_SETTING.

    READ TABLE m_currency_list
      WITH KEY
        country_key = iv_country_key
        currency_key = iv_currency_key
      INTO rs_currency_settings.

  ENDMETHOD.


  METHOD GET_LEFT_CURRENCY_SYMBOL.

    DATA(ls_currency_setting) =
      get_currency_setting(
        iv_country_key  = CONV #( iv_country_key )
        iv_currency_key = CONV #( iv_currency_key ) ).

    IF ls_currency_setting-position = c_currency_position-left.
      rv_symbol = ls_currency_setting-symbol.
    ENDIF.

  ENDMETHOD.


  METHOD GET_NUMBER_FORMAT.

    rv_number_format =
      SWITCH string( iv_sap_number_format
        WHEN ''  THEN '###.###.###.###.###.##0'
        WHEN 'X' THEN '###,###,###,###,###,##0'
        WHEN 'Y' THEN '### ### ### ### ### ##0'
        ELSE '' )
      &&
      COND string(
        WHEN iv_decimal_count > 0 THEN
          SWITCH #( iv_sap_number_format
            WHEN ''  THEN ','
            WHEN 'X' THEN '.'
            WHEN 'Y' THEN ','
            ELSE '' )
        ELSE
          '' )
      &&
      get_same_characters(
         iv_decimal_character = iv_decimal_character
         iv_decimal_count     = iv_decimal_count ).

  ENDMETHOD.


  METHOD GET_RIGHT_CURRENCY_SYMBOL.

    DATA(ls_currency_setting) =
      get_currency_setting(
        iv_country_key  = iv_country_key
        iv_currency_key = iv_currency_key ).

    IF ls_currency_setting-position = c_currency_position-right.
      rv_symbol = ls_currency_setting-symbol.
    ENDIF.

  ENDMETHOD.


  METHOD GET_SAME_CHARACTERS.

    DO iv_decimal_count TIMES.

      rv_text = rv_text && iv_decimal_character.

    ENDDO.

  ENDMETHOD.
ENDCLASS.
