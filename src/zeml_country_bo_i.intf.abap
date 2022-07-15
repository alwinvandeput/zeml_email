INTERFACE zeml_country_bo_i
  PUBLIC .


  TYPES:
    BEGIN OF gts_country_settings,
      number_format             TYPE t005x-xdezp,
      date_format               TYPE t005x-datfm,
      time_format               TYPE t005x-timefm,
      number_format_description TYPE dd07d-ddtext,
      date_format_description   TYPE dd07d-ddtext,
      time_format_description   TYPE dd07d-ddtext,
    END OF gts_country_settings .

  METHODS get_country_settings
    IMPORTING
      !iv_read_descriptions_ind  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rs_country_settings) TYPE gts_country_settings
    RAISING
      zcx_return3 .

ENDINTERFACE.
