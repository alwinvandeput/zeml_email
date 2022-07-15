class ZEML_ABAP_TRANSFORM_PRC_FT definition
  public
  create public .

public section.

  class-methods GET_FACTORY
    returning
      value(RR_FACTORY) type ref to ZEML_ABAP_TRANSFORM_PRC_FT .
  class-methods SET_FACTORY
    importing
      !IR_FACTORY type ref to ZEML_ABAP_TRANSFORM_PRC_FT .
  methods GET_ABAP_TRANSFORM_PRC
    importing
      !IV_CLASS_NAME type STRING
      !IA_CONTENT type ANY
      !IS_LABELS type ANY
      !IS_SETTINGS type zeml_extended_email_bo=>t_settings
    returning
      value(RO_ABAP_TRANSFORM_PRC) type ref to ZEML_ABAP_TRANSFORM_PRC .
*  methods CREATE_EMAIL
*    importing
*      !IS_EMAIL_DATA type ZEML_abap_transform_prc=>GTS_DATA
*    returning
*      value(RR_abap_transform_prc) type ref to ZEML_abap_transform_prc
*    raising
*      ZCX_EML_RETURN3 .
  PROTECTED SECTION.

    CLASS-DATA gr_factory TYPE REF TO Zeml_ABAP_TRANSFORM_PRC_FT .

  PRIVATE SECTION.

ENDCLASS.



CLASS ZEML_ABAP_TRANSFORM_PRC_FT IMPLEMENTATION.


  METHOD get_abap_transform_prc.

    TRY.

        CREATE OBJECT ro_abap_transform_prc
          TYPE (iv_class_name).

      CATCH cx_sy_create_object_error  INTO DATA(lo_create_object_error).

        "TODO: ERROR HANDLING
        BREAK-POINT.

    ENDTRY.

    ro_abap_transform_prc->gs_data =
      VALUE #(
        content_data = REF #( ia_content )
        labels_data  = REF #( is_labels )
        sap_settings = is_settings
    ).

    ro_abap_transform_prc->set_default_settings( ).

  ENDMETHOD.


  METHOD GET_FACTORY.

    IF gr_factory IS NOT INITIAL.

      rr_factory = gr_factory.

      RETURN.

    ENDIF.

    rr_factory = NEW #( ).

  ENDMETHOD.


  METHOD SET_FACTORY.

    gr_factory = ir_factory.

  ENDMETHOD.
ENDCLASS.
