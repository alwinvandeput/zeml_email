CLASS zeml_example_so_text_lay_prc DEFINITION
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



CLASS ZEML_EXAMPLE_SO_TEXT_LAY_PRC IMPLEMENTATION.


  METHOD get_content.

    FIELD-SYMBOLS <ls_sales_order> TYPE zeml_example_so_dp=>gts_sales_order.

    ASSIGN me->gs_data-content_data->* TO <ls_sales_order>.

    DATA(cr_lf) = cl_abap_char_utilities=>cr_lf.

    rv_html =
        |<title>{ get_label( 'ORDER_NO' ) } { <ls_sales_order>-order_no }</title>| &&
        |myCompany{ cr_lf }| &&
        |{ get_label( 'ORDER_NO' ) } { <ls_sales_order>-order_no }{ cr_lf }| &&
        |{ get_label( 'DATE' ) } { format_date( sy-datum ) } { format_time( sy-uzeit ) }{ cr_lf }| &&
        |{ cr_lf }| &&
        |********************{ cr_lf }| &&
        |Items{ cr_lf }| &&
        |********************{ cr_lf }| &&
        |{
          REDUCE string(
            INIT lv_result TYPE string
            FOR <ls_item>
            IN <ls_sales_order>-items
            NEXT
              lv_result = lv_result &&
                |***{ get_label( 'ITEM_NO' ) }: { <ls_item>-item_no ALPHA = OUT }***{ cr_lf }| &&
                |{ get_label( 'DESCRIPTION' ) }: { <ls_item>-description }{ cr_lf }| &&
                |{ get_label( 'QUANTITY' ) }: | &&
                  |{ format_number(
                       ia_number               = <ls_item>-quantity
                       iv_decimal_count        = 2
                       iv_delete_end_zeros_ind = abap_true ) }| &&
                |{ cr_lf }| &&
                |{ get_label( 'UNIT' ) }: { <ls_item>-unit }{ cr_lf }| &&
                |{ get_label( 'NETT_AMOUNT' ) }: | &&
                  |{ format_amount_w_symbol( <ls_item>-nett_amount ) }{ cr_lf }| &&
                |{ cr_lf }| )
        }|.

  ENDMETHOD.
ENDCLASS.
