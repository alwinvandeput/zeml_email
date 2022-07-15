CLASS zeml_example_so_html_lay_prc DEFINITION
  PUBLIC
  INHERITING FROM zeml_abap_transform_prc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_content
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEML_EXAMPLE_SO_HTML_LAY_PRC IMPLEMENTATION.


  METHOD get_content.

    FIELD-SYMBOLS <ls_sales_order> TYPE zeml_example_so_dp=>gts_sales_order.

    ASSIGN me->gs_data-content_data->* TO <ls_sales_order>.

    rv_html =
      |<html>| &&
        |<title>{ get_label( 'ORDER_NO' ) } { <ls_sales_order>-order_no }</title>| &&
        |<body>| &&
          |<table class="center main1 content" style="table-layout: auto; width: 700px;" cellpadding="0" cellspacing="0"><tr><td>| &&

            |<h1><b><i>myCompany</i></b></h1>| &&

            |<p>{ get_label( 'ORDER_NO' ) } { <ls_sales_order>-order_no }</p>| &&
            |<p>{ get_label( 'DATE' ) } { format_date( sy-datum ) } { format_time( sy-uzeit ) }</p>| &&

            |<h1>Items</h1>| &&
            |<table border="1">| &&

              |<tr class="head">| &&
                |<th width="50px"  class="text_label">| &&
                  |{ get_label( 'ITEM_NO' ) }</th>| &&
                |<th width="300px" class="text_label">| &&
                  |{ get_label( 'DESCRIPTION' ) }</th>| &&
                |<th width="250px" class="number_label" align="right">| &&
                  |{ get_label( 'QUANTITY' ) }</th>| &&
                |<th width="50px"  class="text_label">| &&
                  |{ get_label( 'UNIT' ) }</th>| &&
                |<th width="150px" class="number_label" align="right">| &&
                  |{ get_label( 'NETT_AMOUNT' ) }</th>| &&
              |</tr>| &&

              |{
                REDUCE string(
                  INIT lv_result TYPE string
                  FOR <ls_item>
                  IN <ls_sales_order>-items
                  NEXT
                    lv_result = lv_result &&
                      |<tr>| &&
                        |<td>{ <ls_item>-item_no ALPHA = OUT }</td>| &&
                        |<td>{ <ls_item>-description }</td>| &&
                        |<td align="right">| &&
                          |{ format_number(
                               ia_number               = <ls_item>-quantity
                               iv_decimal_count        = 2
                               iv_delete_end_zeros_ind = abap_true ) }| &&
                        |</td>| &&
                        |<td>{ <ls_item>-unit }</td>| &&
                        |<td align="right">| &&
                          |{ format_amount_w_symbol( <ls_item>-nett_amount ) }</td>| &&
                      |</tr>| )
              }| &&
            |</table>| &&

          |</td></tr></table>| &&
        |</body>| &&
      |</html>|.

  ENDMETHOD.
ENDCLASS.
