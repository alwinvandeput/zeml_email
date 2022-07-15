CLASS zeml_country_bo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zeml_country_bo_ft.

  PUBLIC SECTION.

    INTERFACES zeml_country_bo_i .

    TYPES:
      BEGIN OF t_settings,
        number_format             TYPE t005x-xdezp,
        date_format               TYPE t005x-datfm,
        time_format               TYPE t005x-timefm,
        number_format_description TYPE dd07d-ddtext,
        date_format_description   TYPE dd07d-ddtext,
        time_format_description   TYPE dd07d-ddtext,
      END OF t_settings .

  PROTECTED SECTION.

    DATA:
      m_country_key  TYPE t005x-land.

ENDCLASS.

CLASS zeml_country_bo IMPLEMENTATION.

  METHOD zeml_country_bo_i~get_country_settings.

    SELECT SINGLE
        xdezp  AS number_format
        datfm  AS date_format
        timefm AS time_format
      FROM t005x
      INTO rs_country_settings
      WHERE land = m_country_key.

    IF sy-subrc <> 0.

      "Country key &1 not found in Country table (T005X).
      RAISE EXCEPTION TYPE zcx_return3
        MESSAGE e002
        WITH m_country_key.

    ENDIF.

    IF rs_country_settings-time_format IS INITIAL.
      rs_country_settings-time_format = 0.
    ENDIF.

    DATA(lr_country_settings_bo) = zeml_country_settings_bo=>create_instance_by_data( rs_country_settings ).

    rs_country_settings =
      lr_country_settings_bo->zeml_country_bo_i~get_country_settings(
        iv_read_descriptions_ind = abap_true ).

  ENDMETHOD.
ENDCLASS.
