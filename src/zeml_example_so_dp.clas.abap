CLASS zeml_example_so_dp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_customer,
        language_id TYPE syst-langu,
      END OF gts_customer,

      BEGIN OF gts_schedule_line,
        quantity     TYPE vbep-wmeng,
        arrival_date TYPE vbep-edatu,
        arrival_time TYPE vbep-ezeit,
      END  OF gts_schedule_line,

      BEGIN OF gts_sales_order_item,
        item_no     TYPE vbap-posnr,
        description TYPE makt-maktx,
        quantity    TYPE vbap-zmeng,
        unit        TYPE vbap-meins,
        nett_amount TYPE vbap-netwr,
      END OF gts_sales_order_item,

      BEGIN OF gts_sales_order,
        order_no     TYPE vbak-vbeln,
        nett_amount  TYPE vbak-netwr,
        currency_key TYPE vbak-waerk,
        country_key  TYPE t005x-land,
        customer     TYPE gts_customer,
        items        TYPE STANDARD TABLE OF gts_sales_order_item WITH DEFAULT KEY,
      END OF gts_sales_order.

    METHODS get_sales_order
      IMPORTING iv_country_key        TYPE t005x-land
                iv_currency_key       TYPE vbak-waerk
                iv_language_id        TYPE syst-langu
      RETURNING VALUE(rs_sales_order) TYPE gts_sales_order.

ENDCLASS.



CLASS ZEML_EXAMPLE_SO_DP IMPLEMENTATION.


  METHOD get_sales_order.

    rs_sales_order =
      VALUE #(
        order_no      = '3'
*            sold_to_party TYPE gts_address,
*            ship_to_party TYPE gts_address,
        nett_amount   = '1700.99'
        currency_key  = iv_currency_key
        country_key   = iv_country_key
        customer = VALUE #(
          language_id   = iv_language_id
        )
        items = VALUE #(
          (
            item_no     = '0010'
            description = COND string(
              WHEN iv_language_id = 'E' THEN 'SIM card'
              WHEN iv_language_id = 'F' THEN 'Carte SIM'
              WHEN iv_language_id = 'D' THEN 'SIM Karte'
              WHEN iv_language_id = 'N' THEN 'SIMkaart' )
            quantity    = '4'
            unit        = 'ST'
            nett_amount = '200.00'
          )
          (
            item_no     = '0020'
            description = COND string(
              WHEN iv_language_id = 'E' THEN 'Mobile phone'
              WHEN iv_language_id = 'F' THEN 'Téléphone mobile'
              WHEN iv_language_id = 'D' THEN 'Mobiltelefon'
              WHEN iv_language_id = 'N' THEN 'Mobiele telefoon' )
            quantity    = '1'
            unit        = 'ST'
            nett_amount = '1500.99'
          )
        )
      ).

  ENDMETHOD.
ENDCLASS.
