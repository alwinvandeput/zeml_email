CLASS zeml_xslt_value_formatter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS format_decimal
      IMPORTING country_code_nc        TYPE REF TO if_ixml_node_collection
                decimal_value_nc       TYPE REF TO if_ixml_node_collection
                decimal_count          TYPE i
      RETURNING VALUE(formatted_value) TYPE string.

    CLASS-METHODS format_date
      IMPORTING country_code_nc        TYPE REF TO if_ixml_node_collection
                date_nc                TYPE REF TO if_ixml_node_collection
      RETURNING VALUE(formatted_value) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS _get_node_coll_value
      IMPORTING xml_node_collection TYPE REF TO if_ixml_node_collection
      RETURNING VALUE(value_string) TYPE string.

ENDCLASS.

CLASS zeml_xslt_value_formatter IMPLEMENTATION.

  METHOD format_decimal.

    TRY.

        DATA(temp_country_code) = CONV char2( _get_node_coll_value( country_code_nc ) ).
        DATA(temp_decimal_value) = _get_node_coll_value( decimal_value_nc ).

        DATA value_packed TYPE p DECIMALS 4.
        value_packed = temp_decimal_value.

        DATA(user_country_code) = zeml_user_bo_ft=>get_factory( )->get_user_bo_by_user_name( )->get_country_key( ).
        SET COUNTRY temp_country_code.

        DATA formatted_value_char TYPE c LENGTH 200.

        WRITE value_packed
          DECIMALS decimal_count
          TO formatted_value_char LEFT-JUSTIFIED.

        SET COUNTRY user_country_code.

        formatted_value = formatted_value_char.

      CATCH zcx_return3 INTO DATA(return3_exc).

        formatted_value = |Error in formatting|.

    ENDTRY.

  ENDMETHOD.

  METHOD format_date.

    TRY.

        DATA(temp_country_code) = CONV char2( _get_node_coll_value( country_code_nc ) ).
        DATA(temp_date_string) = _get_node_coll_value( date_nc ).

        CASE temp_country_code.

          WHEN 'NL'.

            formatted_value = temp_date_string+8(2) && |-| && temp_date_string+5(2) && |-| && temp_date_string+0(4).

            RETURN.

        ENDCASE.

        DATA temp_date TYPE d.
        temp_date = temp_date_string+0(4) && temp_date_string+5(2) && temp_date_string+8(2).

        DATA(user_country_code) = zeml_user_bo_ft=>get_factory( )->get_user_bo_by_user_name( )->get_country_key( ).
        SET COUNTRY temp_country_code.

        DATA formatted_value_char TYPE c LENGTH 200.

        WRITE temp_date
          TO formatted_value_char LEFT-JUSTIFIED.

        SET COUNTRY user_country_code.

        formatted_value = formatted_value_char.

      CATCH zcx_return3 INTO DATA(return3_exc).

        formatted_value = |Error in formatting|.

    ENDTRY.

  ENDMETHOD.

  METHOD _get_node_coll_value.

    DATA(temp_xml_element) = CAST if_ixml_element( xml_node_collection->get_item( 0 ) ).

    IF temp_xml_element IS INITIAL.
      RETURN.
    ENDIF.

    value_string = temp_xml_element->get_value( ).

  ENDMETHOD.

ENDCLASS.
