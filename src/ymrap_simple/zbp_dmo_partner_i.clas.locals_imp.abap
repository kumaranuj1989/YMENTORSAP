CLASS lhc_PartnerBDI DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR PartnerBDI RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR PartnerBDI RESULT result.

    METHODS validateCoreData FOR VALIDATE ON SAVE
      IMPORTING keys FOR PartnerBDI~validateCoreData.

    METHODS validateKeyIsFilled FOR VALIDATE ON SAVE
      IMPORTING keys FOR PartnerBDI~validateKeyIsFilled.

ENDCLASS.

CLASS lhc_PartnerBDI IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD validateCoreData.
    READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    ALL FIELDS WITH VALUE #( ( %tky-PartnerNumber = keys[ 1 ]-PartnerNumber ) )
    RESULT DATA(lt_result)
    FAILED DATA(lt_failed)
    REPORTED DATA(lt_reported).

    SELECT FROM I_Country AS a
    INNER JOIN @lt_result  AS b ON a~Country = b~Country
    FIELDS a~Country
    INTO TABLE @DATA(lt_county).

    SELECT FROM I_Currency AS a
    INNER JOIN @lt_result  AS b ON a~Currency = b~PaymentCurrency
    FIELDS a~Currency
    INTO TABLE @DATA(lt_Currency).

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
      IF NOT line_Exists( lt_county[ Country = <lfs_result>-Country ]  ).
        INSERT VALUE #( PartnerNumber = <lfs_result>-PartnerNumber  ) INTO TABLE failed-partnerbdi.
        INSERT VALUE #( PartnerNumber          = <lfs_result>-PartnerNumber
                        %state_area            = 'Partner Mandt'
                        %element-Country       = if_abap_behv=>mk-on
                        %msg                   = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                                   text = 'Country not found in I_Country' ) ) INTO TABLE reported-partnerbdi.
      ENDIF.

      IF NOT line_Exists( lt_Currency[ Currency = <lfs_result>-PaymentCurrency ]  ).
        INSERT VALUE #( PartnerNumber = <lfs_result>-PartnerNumber  ) INTO TABLE failed-partnerbdi.
        INSERT VALUE #( PartnerNumber            = <lfs_result>-PartnerNumber
                        %state_area              = 'Partner Mandt'
                        %element-PaymentCurrency = if_abap_behv=>mk-on
                        %msg                     = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                                          text = 'Currency not found in I_Currency' ) ) INTO TABLE reported-partnerbdi.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateKeyIsFilled.
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_keys>) WHERE %tky-PartnerNumber IS INITIAL.
      INSERT VALUE #( PartnerNumber = <lfs_keys>-PartnerNumber  ) INTO TABLE failed-partnerbdi.

      "%state_area used to categorize the message, linking it to a specific validation or condition
      "%element used to specify which field should be highlighted
      "%msg used to provide the message for display along with severity
      INSERT VALUE #( PartnerNumber          = <lfs_keys>-PartnerNumber
                      %state_area            = 'Partner Info'
                      %element-partnernumber = if_abap_behv=>mk-on
                      %msg                   = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                                 text = 'PartnerNumber is mandatory' ) ) INTO TABLE reported-partnerbdi.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
