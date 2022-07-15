CLASS zeml_extended_email_bo_ft DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_factory
      RETURNING
        VALUE(factory) TYPE REF TO zeml_extended_email_bo_ft .
    CLASS-METHODS set_factory
      IMPORTING
        !factory TYPE REF TO zeml_extended_email_bo_ft .
    METHODS create_email
      IMPORTING
        !email_data     TYPE zeml_extended_email_bo=>t_email
      RETURNING
        VALUE(email_bo) TYPE REF TO zeml_extended_email_bo
      RAISING
        zcx_return3 .
  PROTECTED SECTION.

    CLASS-DATA m_factory TYPE REF TO zeml_extended_email_bo_ft .

  PRIVATE SECTION.

ENDCLASS.

CLASS zeml_extended_email_bo_ft IMPLEMENTATION.

  METHOD create_email.

    email_bo = NEW #( ).

    email_bo->m_email = email_data.

    IF email_bo->m_email-layout_transform_type IS INITIAL.

      email_bo->m_email-layout_transform_type = zeml_extended_email_bo=>c_layout_transform_types-xslt.

    ENDIF.

  ENDMETHOD.

  METHOD get_factory.

    IF m_factory IS NOT INITIAL.

      factory = m_factory.

      RETURN.

    ENDIF.

    factory = NEW #( ).

  ENDMETHOD.


  METHOD set_factory.

    m_factory = factory.

  ENDMETHOD.

ENDCLASS.
