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
     WITH value #( for ls_update in lt_update ( %tky-PartnerNumber = ls_update-PartnerNumber
                                 Street = space
                        %control-Street = if_abap_behv=>mk-on ) ).

    INSERT value #( %msg     = new_message_with_text( text = |{ lines( lt_table ) } records changed|
                    severity = if_abap_behv_message=>severity-success  ) )
     into table reported-partnerbdi.
  ENDMETHOD.

  METHOD fillEmptyStreets.
  READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
  FIELDS ( Street )
  WITH VALUE #( ( %tky-PartnerNumber = keys[ 1 ]-PartnerNumber ) )
  RESULT data(lt_result).

  READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
  FIELDS ( Street )
  with CORRESPONDING #( keys )
  RESULT lt_result.

  READ ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
  ALL FIELDS WITH corresponding #( keys )
  result data(lt_result1).

  MODIFY ENTITIES OF ydmo_partner_i IN LOCAL MODE ENTITY PartnerBDI
  UPDATE FIELDS ( Street )
  with value #( ( %tky-PartnerNumber = lt_result[ 1 ]-PartnerNumber Street = 'EMPTY' %control-Street = if_abap_behv=>mk-on ) )
  MAPPED mapped.

  INSERT value #( %msg     = new_message_with_text( text = |{ keys[ 1 ]-PartnerNumber } records changed|
                  severity = if_abap_behv_message=>severity-success  ) )
     into table reported-partnerbdi.

  insert value #( %tky = lt_result[ 1 ]-%tky %param = lt_result[ 1 ] ) into table result.
  ENDMETHOD.

ENDCLASS.
