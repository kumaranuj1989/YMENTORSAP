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

    METHODS fillCurrency FOR DETERMINE ON MODIFY
      IMPORTING keys FOR PartnerBDI~fillCurrency.

    METHODS clearAllEmptyStreets FOR MODIFY
      IMPORTING keys FOR ACTION PartnerBDI~clearAllEmptyStreets.

    METHODS fillEmptyStreets FOR MODIFY
      IMPORTING keys FOR ACTION PartnerBDI~fillEmptyStreets RESULT result.

    METHODS copyLine FOR MODIFY
      IMPORTING keys FOR ACTION PartnerBDI~copyLine.

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

  METHOD fillCurrency.
    "Read with keys using corresponding key words
    READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result).

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<lfs_result>) WHERE PaymentCurrency IS INITIAL.
      MODIFY ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
      UPDATE FIELDS ( PaymentCurrency )
      WITH VALUE #( ( %tky = <lfs_result>-%tky PaymentCurrency = 'EUR' %control-paymentcurrency = if_abap_behv=>mk-on ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD clearAllEmptyStreets.
    SELECT * FROM ydmo_partner_db
    WHERE street = 'EMPTY'
    INTO TABLE @DATA(lt_table).


    LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<lfs_table>).
      MODIFY ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
      UPDATE FIELDS ( Street )
      WITH VALUE #( ( %tky-PartnerNumber = <lfs_table>-partner
                                  Street = space
                         %control-Street = if_abap_behv=>mk-on )  )
      FAILED DATA(lt_failed)
      MAPPED DATA(lt_mapped)
      REPORTED DATA(lt_reported).
    ENDLOOP.

    DATA : lt_update TYPE TABLE FOR UPDATE ydmo_partner_i.
    lt_update = CORRESPONDING #( lt_table MAPPING TO ENTITY ).

    MODIFY ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
     UPDATE FIELDS ( Street )
     WITH VALUE #( FOR ls_update IN lt_update ( %tky-PartnerNumber = ls_update-PartnerNumber
                                 Street = space
                        %control-Street = if_abap_behv=>mk-on ) ).

    INSERT VALUE #( %msg     = new_message_with_text( text = |{ lines( lt_table ) } records changed|
                    severity = if_abap_behv_message=>severity-success  ) )
     INTO TABLE reported-partnerbdi.
  ENDMETHOD.

  METHOD fillEmptyStreets.
    READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    FIELDS ( Street )
    WITH VALUE #( ( %tky-PartnerNumber = keys[ 1 ]-PartnerNumber ) )
    RESULT DATA(lt_result).

    READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    FIELDS ( Street )
    WITH CORRESPONDING #( keys )
    RESULT lt_result.

    READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result1).

    MODIFY ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    UPDATE FIELDS ( Street )
    WITH VALUE #( ( %tky-PartnerNumber = lt_result[ 1 ]-PartnerNumber Street = 'EMPTY' %control-Street = if_abap_behv=>mk-on ) )
    MAPPED mapped.

    INSERT VALUE #( %msg     = new_message_with_text( text = |{ keys[ 1 ]-PartnerNumber } records changed|
                    severity = if_abap_behv_message=>severity-success  ) )
       INTO TABLE reported-partnerbdi.

    "Insert key field and the whole structure of that particular records
    INSERT VALUE #( %tky = lt_result[ 1 ]-%tky %param = lt_result[ 1 ] ) INTO TABLE result.
  ENDMETHOD.

  METHOD copyLine.
    DATA : lt_creation  TYPE TABLE FOR CREATE      ydmo_partner_i.

    READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result).

    SELECT FROM ydmo_partner_db
    FIELDS MAX( partner )
    INTO @DATA(lv_partner).

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<lfs_result>).
       lv_partner = lv_partner + 1.
      <lfs_result>-PartnerNumber = lv_partner.
      <lfs_result>-PartnerName &&= |Copy|.

      INSERT VALUE #( %cid = keys[ sy-tabix ]-%cid ) INTO TABLE lt_creation REFERENCE INTO DATA(lr_create).
      lr_create->* = CORRESPONDING #( <lfs_result> ).
      lr_create->%control-PartnerNumber = if_abap_behv=>mk-on.
      lr_create->%control-PartnerName   = if_abap_behv=>mk-on.
      lr_create->%control-Street        = if_abap_behv=>mk-on.
      lr_create->%control-City          = if_abap_behv=>mk-on.
      lr_create->%control-Street        = if_abap_behv=>mk-on.
      lr_create->%control-PaymentCurrency = if_abap_behv=>mk-on.
    ENDLOOP.

    MODIFY ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
    CREATE FROM lt_creation
    FAILED DATA(lt_failed)
    MAPPED DATA(lt_mapped)
    REPORTED DATA(lt_reported).

    mapped-partnerbdi = lt_mapped-partnerbdi.
  ENDMETHOD.

ENDCLASS.
