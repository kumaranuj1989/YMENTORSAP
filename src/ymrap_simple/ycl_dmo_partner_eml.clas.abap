CLASS ycl_dmo_partner_eml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES : if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_dmo_partner_eml IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
**********************************************************************
* Data Declaration
    DATA:
      lt_selection TYPE TABLE FOR READ IMPORT ydmo_partner_i,
      lt_creation  TYPE TABLE FOR CREATE      ydmo_partner_i,
      lt_update    TYPE TABLE FOR UPDATE      ydmo_partner_i.

**********************************************************************
    " Long form for selection (ALL FIELDS)
    lt_selection = VALUE #(
      ( PartnerNumber = '1234' )
      ( PartnerNumber = '1000000003' )
    ).

    "Read Entity
    READ ENTITIES OF ydmo_partner_i ENTITY PartnerBDI
      ALL FIELDS WITH lt_selection
      RESULT DATA(lt_partner_long)
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported).

    out->write( lt_partner_long ).

    "Read Entity Short form for selection (SOME FIELDS)
    READ ENTITIES OF ydmo_partner_i ENTITY PartnerBDI
      FIELDS ( PartnerName Street City ) WITH VALUE #(
        ( PartnerNumber = '1234' )
        ( PartnerNumber = '1000000003' )
      )
      RESULT DATA(lt_partner_short)
      FAILED ls_failed
      REPORTED ls_reported.

    out->write( lt_partner_short ).

    "Create new partner
    lt_creation = VALUE #(
      (
        %cid = 'DummyKey1'
        PartnerNumber = '56781'
        PartnerName = 'Amazon'
        Country = 'US'
        %control-PartnerNumber = if_abap_behv=>mk-on
        %control-PartnerName = if_abap_behv=>mk-on
        %control-Country = if_abap_behv=>mk-on
      )
    ).

    "Create Entity
    MODIFY ENTITIES OF ydmo_partner_i ENTITY PartnerBDI
      CREATE FROM lt_creation
      FAILED ls_failed
      MAPPED DATA(ls_mapped)
      REPORTED ls_reported.

    TRY.
        out->write( ls_mapped-partnerbdi[ 1 ]-PartnerNumber ).
        COMMIT ENTITIES.

      CATCH cx_sy_itab_line_not_found.
        out->write( ls_failed-partnerbdi[ 1 ]-%cid ).
    ENDTRY.

    "we also pass a new name for the partner, but do not fill the CONTROL structure.
    "So we assume that the Name field is not updated and keeps the original value.
    lt_update = VALUE #(
      (
        PartnerNumber = '56781'
        PartnerName = 'Amazon Fake'
        City = 'Seattle'
        PaymentCurrency = 'USD'
        %control-PaymentCurrency = if_abap_behv=>mk-on
        %control-City = if_abap_behv=>mk-on
      )
    ).

    "Update Entity partner
    MODIFY ENTITIES OF ydmo_partner_i ENTITY PartnerBDI
      UPDATE FROM lt_update
      FAILED ls_failed
      MAPPED ls_mapped
      REPORTED ls_reported.

    IF ls_failed-partnerbdi IS INITIAL.
      out->write( 'Updated' ).
      COMMIT ENTITIES.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
