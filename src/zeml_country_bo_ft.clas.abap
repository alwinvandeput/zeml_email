CLASS zeml_country_bo_ft DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_factory
      RETURNING
        VALUE(rr_factory) TYPE REF TO zeml_country_bo_ft .
    CLASS-METHODS set_factory
      IMPORTING
        !ir_factory TYPE REF TO zeml_country_bo_ft .
    METHODS get_country_bo_by_key
      IMPORTING
        !iv_country                   TYPE t005x-land
      RETURNING
        VALUE(ro_country_settings_bo) TYPE REF TO zeml_country_bo_i
      RAISING
        zcx_return3 .

  PROTECTED SECTION.

    CLASS-DATA gr_factory TYPE REF TO zeml_country_bo_ft.

ENDCLASS.

CLASS zeml_country_bo_ft IMPLEMENTATION.

  METHOD get_country_bo_by_key.

    IF iv_country IS INITIAL.

      "Country key is empty.
      RAISE EXCEPTION TYPE zcx_return3
        MESSAGE e001.

    ENDIF.

    DATA(temp_country_settings_bo) = NEW zeml_country_bo( ).

    temp_country_settings_bo->m_country_key = iv_country.

    ro_country_settings_bo = temp_country_settings_bo.

  ENDMETHOD.

  METHOD get_factory.

    IF gr_factory IS NOT INITIAL.

      rr_factory = gr_factory.

      RETURN.

    ENDIF.

    rr_factory = NEW #( ).

  ENDMETHOD.

  METHOD set_factory.

    gr_factory = ir_factory.

  ENDMETHOD.

ENDCLASS.
