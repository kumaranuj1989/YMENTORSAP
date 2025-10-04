CLASS lsc_ydmo_partner_i DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_ydmo_partner_i IMPLEMENTATION.

  METHOD adjust_numbers.
    SELECT FROM ydmo_partner_db
    FIELDS MAX( partner )
    INTO @DATA(ld_max_partner).

    LOOP AT mapped-partnerbdi REFERENCE INTO DATA(lr_partner).
      ld_max_partner += 1.
      lr_partner->PartnerNumber = ld_max_partner.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

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

    METHODS withpopup FOR MODIFY
      IMPORTING keys FOR ACTION PartnerBDI~withpopup.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR PartnerBDI RESULT result.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR partnerbdi RESULT result.

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
*    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_keys>) WHERE %tky-PartnerNumber IS INITIAL.
*      INSERT VALUE #( PartnerNumber = <lfs_keys>-PartnerNumber  ) INTO TABLE failed-partnerbdi.
*
*      "%state_area used to categorize the message, linking it to a specific validation or condition
*      "%element used to specify which field should be highlighted
*      "%msg used to provide the message for display along with severity
*      INSERT VALUE #( PartnerNumber          = <lfs_keys>-PartnerNumber
*                      %state_area            = 'Partner Info'
*                      %element-partnernumber = if_abap_behv=>mk-on
*                      %msg                   = new_message_with_text( severity = if_abap_behv_message=>severity-error
*                                                                 text = 'PartnerNumber is mandatory' ) ) INTO TABLE reported-partnerbdi.
*    ENDLOOP.

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

      DATA(lr_data_ref) = REF #( lt_creation ).
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

  METHOD withpopup.
    TRY.
        DATA(ls_key) = keys[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    CASE ls_key-%param-MessageType.
      WHEN 1.
        INSERT VALUE #(
          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Dummy message' )
        ) INTO TABLE reported-partnerbdi.
      WHEN 2.
        INSERT VALUE #(
          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-information text = 'Dummy message' )
        ) INTO TABLE reported-partnerbdi.
      WHEN 3.
        INSERT VALUE #(
          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-warning text = 'Dummy message' )
        ) INTO TABLE reported-partnerbdi.
      WHEN 4.
        INSERT VALUE #(
          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Dummy message' )
        ) INTO TABLE reported-partnerbdi.
      WHEN 5.
        INSERT VALUE #(
          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-none text = 'Dummy message' )
        ) INTO TABLE reported-partnerbdi.
      WHEN 6.
        reported-partnerbdi = VALUE #(
          ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Dummy message' ) )
          ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-information text = 'Dummy message' ) )
        ).
      WHEN 7.
        reported-partnerbdi = VALUE #(
          ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Dummy message' ) )
          ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Dummy message' ) )
          ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-warning text = 'Dummy message' ) )
          ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-information text = 'Dummy message' ) )
        ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_instance_features.
    "KEYS - Transfer all instances to be checked. When the list loads for the first time, all records to be displayed are loaded.
    "REQUESTED FEATURES - Structure where the features to be checked are marked. Is used for the different tests.
    "RESULT - Result of the entities for which the feature should be deactivated
    if requested_features-%action-fillEmptyStreets = if_abap_behv=>mk-on.
     READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
     FIELDS ( Street )
     with CORRESPONDING #( keys )
     result data(lt_result).

     "The features are all activated by default and must now be deactivated for the entities that do not match
     loop at lt_result ASSIGNING FIELD-SYMBOL(<lfs_Result>) where Street is not INITIAL.
     insert value #( partnernumber = <lfs_Result>-PartnerNumber %action-fillEmptyStreets = conv #( if_abap_behv=>mk-on ) ) INTO TABLE result.
     ENDLOOP.
    endif.
  ENDMETHOD.

  METHOD get_global_features.
  if requested_features-%delete = if_abap_behv=>mk-on.
  data(lv_deactivate) = cond #( when cl_abap_context_info=>get_user_alias( ) = 'ABCD' THEN if_abap_behv=>mk-off
                                ELSE if_abap_behv=>mk-on ).
  result-%delete = conv #( lv_deactivate ).
  endif.
  ENDMETHOD.

ENDCLASS.
