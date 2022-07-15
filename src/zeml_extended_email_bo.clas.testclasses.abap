CLASS ltd_text_labels_bo DEFINITION FOR TESTING
  INHERITING FROM ztxd_text_labels_obj.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_sales_order_labels,
        order_no    TYPE string,
        items       TYPE string,
        description TYPE string,
        unit        TYPE string,
        date        TYPE string,
        item_no     TYPE string,
        quantity    TYPE string,
        nett_amount TYPE string,
        total       TYPE string,
        footer      TYPE string,
      END OF gts_sales_order_labels.

    METHODS get_labels_data_obj REDEFINITION.

ENDCLASS.

CLASS ltd_text_labels_bo IMPLEMENTATION.

  METHOD get_labels_data_obj.

    DATA ls_labels TYPE gts_sales_order_labels.

    ls_labels-order_no    = get_long_label(
      iv_data_element_name  = 'VBELN_VA'
      iv_language_code      = language_code ).
    ls_labels-description = get_long_label(
      iv_data_element_name  = 'MAKTX'
      iv_language_code      = language_code ).
    ls_labels-item_no     = get_short_label(
      iv_data_element_name  = 'POSNR_VA'
      iv_language_code      = language_code ).
    ls_labels-quantity    = get_long_label(
      iv_data_element_name  = 'DZMENG'
      iv_language_code      = language_code ).
    ls_labels-nett_amount = get_long_label(
      iv_data_element_name  = 'NETWR_AK'
      iv_language_code      = language_code ).

    CASE language_code.

      WHEN 'E'.

          ls_labels-items       = 'Items'.
          ls_labels-unit        = 'Unit'.
          ls_labels-date        = 'Date'.
          ls_labels-total       = 'Total'.
          ls_labels-footer      = 'Thank you for buying at <b><i>myCompany</i></b>.<br>Till next time...'.

      WHEN 'N'.

          ls_labels-items       = 'Posities'.
          ls_labels-unit        = 'Eenheid'.
          ls_labels-date        = 'Datum'.
          ls_labels-total       = 'Totaal'.
          ls_labels-footer      = 'Bedankt dat u koopt bij <b><i>myCompany</i></b>.'.

      WHEN 'F'.

          ls_labels-items       = 'Articles'.
          ls_labels-unit        = 'Unité'.
          ls_labels-date        = 'Date'.
          ls_labels-total       = 'Le total'.
          ls_labels-footer      = |Merci d'avoir acheté chez <b><i>myCompany</i></b>.|.

      WHEN 'D'.

          ls_labels-items       = 'Material'.
          ls_labels-unit        = 'Einheit'.
          ls_labels-date        = 'Datum'.
          ls_labels-total       = 'Gesamt'.
          ls_labels-footer      = |Vielen Dank für den Kauf bei <b><i>myCompany</i></b>.<br>Auf wiedersehen...|.

    ENDCASE.

    CREATE DATA ro_labels_data TYPE gts_sales_order_labels.

    ASSIGN ro_labels_data->* TO FIELD-SYMBOL(<ls_labels>).

    <ls_labels> = ls_labels.

  ENDMETHOD.

ENDCLASS.

CLASS ltd_text_labels_bo_ft DEFINITION FOR TESTING
  INHERITING FROM ztxd_text_labels_obj_ft.

  PUBLIC SECTION.
    METHODS get_text_labels_obj REDEFINITION.

ENDCLASS.

CLASS ltd_text_labels_bo_ft IMPLEMENTATION.

  METHOD get_text_labels_obj.

    text_labels_obj = NEW ltd_text_labels_bo( ).

    text_labels_obj->gs_data-text_name = text_name.

  ENDMETHOD.

ENDCLASS.

CLASS unit_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION LONG.

  PUBLIC SECTION.

    METHODS create_email_xslt_html_us     FOR TESTING.

    METHODS create_email_xslt_html_nl     FOR TESTING.


    "The test cases below might not be valid. Mostly HTML + XSLT is used

*    METHODS create_email_class_html       ."FOR TESTING.
*
*    METHODS create_email_xslt_plain_text  ."FOR TESTING.
*
*    METHODS create_email_class_plain_text ."FOR TESTING.

    "----------------------------------------------------------------

    METHODS _create_email
      IMPORTING layout_transform_type TYPE zeml_extended_email_bo=>t_layout_transform_type
                content_type          TYPE zeml_extended_email_bo=>t_content_type
                layout_transform_name TYPE zeml_extended_email_bo=>t_email-layout_transform_name
                test_country_key      TYPE char2.

ENDCLASS.

CLASS unit_test IMPLEMENTATION.

  METHOD create_email_xslt_html_us.

    _create_email(
      layout_transform_type = zeml_extended_email_bo=>c_layout_transform_types-xslt
      content_type          = zeml_extended_email_bo=>c_content_types-html
      layout_transform_name = 'ZEML_EXAMPLE_SO_HTML_EMAIL'
      test_country_key      = 'US' ).

  ENDMETHOD.

  METHOD create_email_xslt_html_nl.

    _create_email(
      layout_transform_type = zeml_extended_email_bo=>c_layout_transform_types-xslt
      content_type          = zeml_extended_email_bo=>c_content_types-html
      layout_transform_name = 'ZEML_EXAMPLE_SO_HTML_EMAIL'
      test_country_key      = 'NL' ).

  ENDMETHOD.

*  METHOD create_email_class_html.
*
*    _create_email(
*      layout_transform_type = zeml_extended_email_bo=>c_layout_transform_types-abap_class
*      content_type          = zeml_extended_email_bo=>c_content_types-html
*      layout_transform_name = 'ZEML_EXAMPLE_SO_HTML_LAY_PRC'
*      test_country_key      = 'US' ).
*
*  ENDMETHOD.
*
*  METHOD create_email_xslt_plain_text.
*
*    _create_email(
*      layout_transform_type = zeml_extended_email_bo=>c_layout_transform_types-xslt
*      content_type          = zeml_extended_email_bo=>c_content_types-html
*      layout_transform_name = 'ZEML_EXAMPLE_SO_TEXT_EMAIL'
*      test_country_key      = 'US' ).
*
*  ENDMETHOD.
*
*  METHOD create_email_class_plain_text.
*
*    _create_email(
*      layout_transform_type = zeml_extended_email_bo=>c_layout_transform_types-xslt
*      content_type          = zeml_extended_email_bo=>c_content_types-html
*      layout_transform_name = 'ZEML_EXAMPLE_SO_TEXT_LAY_PRC'
*      test_country_key      = 'US' ).
*
*  ENDMETHOD.

  METHOD _create_email.

    TRY.

        "---------------------------------------------------------
        "Set test country
        CASE test_country_key.

          WHEN 'US'.

            DATA(country_key)  = CONV t005x-land( 'US' ).  "Dec.: point, Date: mm/dd/yyyy, Time: 12:05:10 PM
            DATA(currency_key) = CONV tcurx-currkey( 'USD' ).  "USD: 2 decimals, USDN: 5 decimals
            DATA(language_code)  = 'E'.

          WHEN 'NL'.

            country_key  = CONV t005x-land( 'NL' ). "dec.: comma, date: dd.mm.yyyy, time: 14:05:10
            currency_key = CONV tcurx-currkey( 'EUR' ).
            language_code  = CONV syst-langu( 'N' )    ##operator[langu].

          WHEN 'FR'.

            country_key  = CONV t005x-land( 'FR' ). "dec.: comma, date: dd.mm.yyyy, time: 14:05:10, currency symbol to the right
            currency_key = CONV tcurx-currkey( 'EUR' ).
            language_code  = CONV syst-langu( 'F' )    ##operator[langu].

          WHEN 'DE'.

            country_key  = CONV t005x-land( 'DE' ). "dec.: comma, date: dd.mm.yyyy, time: 14:05:10
            currency_key = CONV tcurx-currkey( 'EUR' ).
            language_code  = CONV syst-langu( 'D' )    ##operator[langu].

        ENDCASE.

        "---------------------------------------------------------
        "Get sales order data
        DATA(sales_order_dp) = NEW zeml_example_so_dp( ).
        DATA(sales_order_data) =
          sales_order_dp->get_sales_order(
            iv_country_key  = country_key
            iv_currency_key = currency_key
            iv_language_id  = language_code ).
        DATA(email_data_obj) = REF #( sales_order_data ).

        "---------------------------------------------------------
        "Set Text label test double - so no SO10 text is needed
        DATA(text_labels_bo_ft) = NEW ltd_text_labels_bo_ft( ).
        ztxd_text_labels_obj_ft=>set_factory( text_labels_bo_ft ).

        "Set Email data
        DATA(email_data) =
          VALUE zeml_extended_email_bo=>t_email(

            content_type            = content_type
            layout_transform_type   = layout_transform_type
            layout_transform_name   = layout_transform_name
            email_data_obj          = email_data_obj
            labels_data_obj =
              ztxd_text_labels_obj_ft=>get_factory( )->get_text_labels_obj(
                  text_name = 'ZEML_EXAMPLE_SO_EMAIL_LABELS'
                    )->get_labels_data_obj(
                      language_code = sales_order_data-customer-language_id )

            country_key             = sales_order_data-country_key
            currency_key            = sales_order_data-currency_key
            language_key            = sales_order_data-customer-language_id

            importance              = '5'
            sensitivity             = ''

            sender =
              VALUE #(
                name  = 'myCompany - noreply'
                email = 'noreply@mycompany.nl'
              )

            receivers =
              VALUE #(
                (
                  name  = 'Alwin van de Put'
                  email = 'alwin.vandeput@mycompany.com'
                )
              )


            attachments = VALUE #( )
          ).

        "Send email
        DATA(email_bo) =
          zeml_extended_email_bo_ft=>get_factory(
            )->create_email(
              email_data ).

        email_bo->send( ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        DATA(bapiret2) = return3_exc->get_bapiret2_struc( ).

        cl_abap_unit_assert=>fail(
          msg    = |ZCX_EML_RETURN3 - ID: { bapiret2-id }, Number { bapiret2-number }|
          detail = |Error message: { return3_exc->get_text( ) }|  ).

      CATCH cx_bcs INTO DATA(bcs_exc).

        cl_abap_unit_assert=>fail( |BCS: | && bcs_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
