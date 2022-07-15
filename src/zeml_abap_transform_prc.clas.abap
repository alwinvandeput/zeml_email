CLASS zeml_abap_transform_prc DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zeml_abap_transform_prc_ft .

  PUBLIC SECTION.

    TYPES gtv_country_format_type TYPE string .
    TYPES gtv_decimal_format_name TYPE string .
    TYPES:
      BEGIN OF gts_decimal_format,
        name               TYPE gtv_decimal_format_name,
        decimal_separator  TYPE c LENGTH 1,
        grouping_separator TYPE c LENGTH 1,
      END OF gts_decimal_format .
    TYPES:
      gtt_decimal_format_list TYPE STANDARD TABLE OF gts_decimal_format WITH DEFAULT KEY .

    METHODS constructor .
    METHODS get_content
      RETURNING
        VALUE(rv_html) TYPE string .
protected section.

  types:
    BEGIN OF gts_data,
        content_data TYPE REF TO data,
        labels_data  TYPE REF TO data,
        sap_settings TYPE zeml_extended_email_bo=>t_settings,
      END OF gts_data .
  types:
    BEGIN OF gts_default_settings,
        decimal_format_name   TYPE gtv_decimal_format_name,
        amount_format         TYPE string,
        quantity_format       TYPE string,
        left_currency_symbol  TYPE string,
        right_currency_symbol TYPE string,
      END OF gts_default_settings .

  data GS_DATA type GTS_DATA .
  data GT_DECIMAL_FORMAT_LIST type GTT_DECIMAL_FORMAT_LIST .
  data GS_DEFAULT_SETTINGS type GTS_DEFAULT_SETTINGS .
  constants:
    BEGIN OF gc_country_format_types,
        decimal_format TYPE gtv_country_format_type VALUE 'DECIMAL',
        date_format    TYPE gtv_country_format_type VALUE 'DATE',
        time_format    TYPE gtv_country_format_type VALUE 'TIME',
      END OF gc_country_format_types .

  methods SET_DEFAULT_SETTINGS .
  methods GET_DECIMAL_FORMAT_NAME
    importing
      !IV_SAP_DECIMAL_FORMAT type T005X-XDEZP
    returning
      value(RV_DECIMAL_FORMAT_NAME) type GTV_DECIMAL_FORMAT_NAME .
  methods GET_NUMBER_FORMAT
    importing
      !IV_SAP_NUMBER_FORMAT type T005X-XDEZP
      !IV_SAP_DECIMAL_COUNT type I
      !IV_DECIMAL_CHARACTER type STRING
    returning
      value(RV_NUMBER_FORMAT) type STRING .
  methods GET_DECIMAL_FORMAT
    importing
      !IV_NAME type GTV_DECIMAL_FORMAT_NAME
    returning
      value(RS_DECIMAL_FORMAT) type GTS_DECIMAL_FORMAT .
  methods GET_SAME_CHARACTERS
    importing
      !IV_DECIMAL_CHARACTER type STRING
      !IV_DECIMAL_COUNT type I
    returning
      value(RV_RESULT) type STRING .
  methods GET_COUNTRY_KEY
    importing
      !IV_COUNTRY_FORMAT_TYPE type GTV_COUNTRY_FORMAT_TYPE
    returning
      value(RV_COUNTRY_KEY) type T005X-LAND .
  methods FORMAT_NUMBER
    importing
      !IA_NUMBER type ANY
      !IV_DECIMAL_COUNT type I
      !IV_DELETE_END_ZEROS_IND type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_NUMBER_TEXT) type STRING .
  methods FORMAT_AMOUNT
    importing
      !IA_NUMBER type ANY
    returning
      value(RV_NUMBER_TEXT) type STRING .
  methods FORMAT_AMOUNT_W_SYMBOL
    importing
      !IA_NUMBER type ANY
    returning
      value(RV_NUMBER_TEXT) type STRING .
  methods FORMAT_DATE
    importing
      !IV_DATE type SYST-DATUM
    returning
      value(RV_DATE_TEXT) type STRING .
  methods FORMAT_TIME
    importing
      !IV_TIME type SYST-UZEIT
      !IV_SECONDS_IND type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_TIME_TEXT) type STRING .
  methods GET_LABEL
    importing
      !IV_LABEL_NAME type STRING
    returning
      value(RV_LABEL_DESCRIPTION) type STRING .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZEML_ABAP_TRANSFORM_PRC IMPLEMENTATION.


  METHOD constructor.

    gt_decimal_format_list =
      VALUE #(
        ( name = 'POINT' decimal_separator = '.' grouping_separator = ',' )
        ( name = 'COMMA' decimal_separator = ',' grouping_separator = '.' )
        ( name = 'SPACE' decimal_separator = ',' grouping_separator = ' ' )
      ).

  ENDMETHOD.


  METHOD format_amount.

    rv_number_text =
      format_number(
        ia_number = ia_number
        iv_decimal_count = CONV #( me->gs_data-sap_settings-currency_decimal_count ) ).

  ENDMETHOD.


  METHOD format_amount_w_symbol.

    rv_number_text =
      |{ gs_default_settings-left_currency_symbol }| &&
      |{ format_amount( ia_number ) }| &&
      |{ gs_default_settings-right_currency_symbol }|.

  ENDMETHOD.


  METHOD format_date.

    DATA(lv_country_key) = get_country_key( gc_country_format_types-date_format ).
    SET COUNTRY lv_country_key.

    DATA lv_date TYPE syst-datum.
    lv_date = iv_date.
    DATA lv_date_text TYPE c LENGTH 10.
    WRITE lv_date TO lv_date_text LEFT-JUSTIFIED.

    CLEAR lv_country_key.
    SET COUNTRY lv_country_key.

    rv_date_text = lv_date_text.

  ENDMETHOD.


  METHOD format_number.

    DATA(lv_country_key) = get_country_key( gc_country_format_types-decimal_format ).

    DATA lv_number_text TYPE c LENGTH 100.

    SET COUNTRY lv_country_key.

    WRITE ia_number TO lv_number_text DECIMALS iv_decimal_count LEFT-JUSTIFIED.

    IF iv_delete_end_zeros_ind = abap_true.

      IF iv_decimal_count > 0.

        DO.

          DATA(lv_length) = strlen( lv_number_text ).
          lv_length = lv_length - 1.
          DATA(lv_last_char) = substring( val  = lv_number_text off = lv_length len = 1 ).

          IF lv_last_char <> '0' AND lv_last_char = '.' AND lv_last_char = ','.
            EXIT.
          ENDIF.

          lv_number_text = substring( val  = lv_number_text off = 0 len = lv_length ).

          IF lv_last_char = '.' OR
             lv_last_char = ','.

            EXIT.

          ENDIF.

        ENDDO.

      ENDIF.

    ENDIF.

    CLEAR lv_country_key.
    SET COUNTRY lv_country_key.

    rv_number_text = lv_number_text.

  ENDMETHOD.


  METHOD format_time.

    DATA(lv_country_key) = get_country_key( gc_country_format_types-time_format ).

    DATA lv_time_text TYPE c LENGTH 12.

    SET COUNTRY lv_country_key.

    lv_time_text = |{ iv_time COUNTRY = lv_country_key }|.

    CLEAR lv_country_key.
    SET COUNTRY lv_country_key.

    IF iv_seconds_ind = abap_false.
      lv_time_text = lv_time_text+0(5).
    ENDIF.

    rv_time_text = lv_time_text.

  ENDMETHOD.


  METHOD get_content.

  ENDMETHOD.


  METHOD get_country_key.

    IF me->gs_data-sap_settings-country_key IS NOT INITIAL.

      rv_country_key = gs_data-sap_settings-country_key.

    ELSE.

      IF me->gs_data-sap_settings-user_name = sy-uname.
        RETURN.
      ENDIF.

      CASE iv_country_format_type.

        WHEN gc_country_format_types-date_format.

          SELECT SINGLE
              land AS country_key,
              xdezp AS decimal_format
            FROM t005x
            WHERE
              xdezp = @gs_data-sap_settings-number_format
            INTO @DATA(ls_decimal_t005x).

          IF sy-subrc <> 0.
            BREAK-POINT.
          ENDIF.

          rv_country_key = ls_decimal_t005x-country_key.

        WHEN gc_country_format_types-date_format.

          SELECT SINGLE
              land AS country_key,
              datfm AS date_format
            FROM t005x
            WHERE
              datfm = @gs_data-sap_settings-date_format
            INTO @DATA(ls_date_t005x).

          IF sy-subrc <> 0.
            BREAK-POINT.
          ENDIF.

          rv_country_key = ls_date_t005x-country_key.

        WHEN gc_country_format_types-date_format.

          SELECT SINGLE
              land AS country_key,
              timefm AS time_format
            FROM t005x
            WHERE
              timefm = @gs_data-sap_settings-time_format
            INTO @DATA(ls_time_t005x).

          IF sy-subrc <> 0.
            BREAK-POINT.
          ENDIF.

          rv_country_key = ls_time_t005x-country_key.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD get_decimal_format.

    READ TABLE me->gt_decimal_format_list
      WITH KEY name = iv_name
      INTO rs_decimal_format.

    IF sy-subrc <> 0.

      "TODO
      BREAK-POINT.

    ENDIF.

  ENDMETHOD.


  METHOD get_decimal_format_name.

    CASE iv_sap_decimal_format.

      WHEN ''.
        rv_decimal_format_name = 'COMMA'.

      WHEN 'X'.
        rv_decimal_format_name = 'POINT'.

      WHEN 'Y'.
        rv_decimal_format_name = 'SPACE'.

    ENDCASE.

  ENDMETHOD.


  METHOD get_label.

    ASSIGN gs_data-labels_data->* TO FIELD-SYMBOL(<ls_labels>).

    ASSIGN COMPONENT iv_label_name
      OF STRUCTURE <ls_labels>
      TO FIELD-SYMBOL(<lv_label>).

    IF sy-subrc = 0.
      rv_label_description = <lv_label>.
    ENDIF.

  ENDMETHOD.


  METHOD get_number_format.

    rv_number_format =
      |{  SWITCH string( iv_sap_number_format
            WHEN ''  THEN '###.###.###.###.###.##0'
            WHEN 'X' THEN '###,###,###,###,###,##0'
            WHEN 'Y' THEN '### ### ### ### ### ##0' ) }| &&
      |{  COND string(
            WHEN iv_sap_decimal_count > 0 THEN
              SWITCH string( iv_sap_number_format
                WHEN ''  THEN ','
                WHEN 'X' THEN '.'
                WHEN 'Y' THEN ','
                ELSE '' )
          ) }| &&
      |{ get_same_characters(
           iv_decimal_character = iv_decimal_character
           iv_decimal_count     = iv_sap_decimal_count ) }|.


*<sap:function name="eml:get_number_format">
*  <sap:argument name ="iv_sap_number_format"/>
*  <sap:argument name ="iv_decimal_count"/>
*  <sap:argument name ="iv_decimal_character"/>
*  <sap:result
*    define = "
*      concat(
*        sap:if($iv_sap_number_format = '', '###.###.###.###.###.##0',
*          sap:if($iv_sap_number_format = 'X', '###,###,###,###,###,##0',
*            sap:if($iv_sap_number_format = 'Y', '### ### ### ### ### ##0','')
*          )
*        )
*        ,
*        sap:if($iv_decimal_count > 0,
*          sap:if($iv_sap_number_format = '', ',',
*            sap:if($iv_sap_number_format = 'X', '.',
*              sap:if($iv_sap_number_format = 'Y', ',','')
*            )
*          )
*          ,
*          ''
*        )
*        ,
*        eml:get_same_characters($iv_decimal_character,$iv_decimal_count)
*      )
*    "/>
*</sap:function>

  ENDMETHOD.


  METHOD get_same_characters.

    DO iv_decimal_count TIMES.
      rv_result = rv_result && iv_decimal_character.
    ENDDO.

  ENDMETHOD.


  METHOD set_default_settings.

    gs_default_settings-decimal_format_name = me->get_decimal_format_name( gs_data-sap_settings-number_format ).
    "TODO: hernoem number_format naar decimal_format. SAP noemt het Decimal Format

    "TODO: <sap:function name="eml:get_decimal_format"> hernoemen naar get_decimal_format_name

    gs_default_settings-amount_format =
      get_number_format(
        iv_sap_number_format  = gs_data-sap_settings-number_format          "TODO: hernoemen naar decimal_format en settings hernoemen  naar SAP_SETTINGS
        iv_sap_decimal_count  = CONV #( gs_data-sap_settings-currency_decimal_count )
        iv_decimal_character  =  '0' ).

    gs_default_settings-quantity_format =
      get_number_format(
        iv_sap_number_format = gs_data-sap_settings-number_format          "TODO: hernoemen naar decimal_format en settings hernoemen  naar SAP_SETTINGS
        iv_sap_decimal_count = 2
        iv_decimal_character = '#' ).

*<xsl:variable name="date_format"     select="eml:get_date_format(//asx:values/SETTINGS/DATE_FORMAT)"/>

*<xsl:variable name="time_format"     select="eml:get_time_format(//asx:values/SETTINGS/TIME_FORMAT)"/>
*
*<xsl:variable name="left_currency_symbol" select="eml:get_left_currency_symbol(//asx:values/SETTINGS/COUNTRY_KEY, //asx:values/SETTINGS/CURRENCY_KEY)"/>
    gs_default_settings-left_currency_symbol =
      zeml_abap_transform_util=>get_left_currency_symbol(
        iv_country_key  = CONV #( gs_data-sap_settings-country_key )      "TODO: types controleren
        iv_currency_key = CONV #( gs_data-sap_settings-currency_key ) ).  "TODO: types controleren

*<xsl:variable name="right_currency_symbol"  select="eml:get_right_currency_symbol(//asx:values/SETTINGS/COUNTRY_KEY, //asx:values/SETTINGS/CURRENCY_KEY)"/>
    gs_default_settings-right_currency_symbol =
      zeml_abap_transform_util=>get_right_currency_symbol(
        iv_country_key  = gs_data-sap_settings-country_key
        iv_currency_key = gs_data-sap_settings-currency_key ).

  ENDMETHOD.
ENDCLASS.
