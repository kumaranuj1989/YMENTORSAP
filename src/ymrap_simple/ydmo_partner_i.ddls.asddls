@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Partner Interface view'
define root view entity YDMO_PARTNER_I
  as select from ydmo_partner_db
{
  key partner               as PartnerNumber,
      name                  as PartnerName,
      street                as Street,
      city                  as City,
      country               as Country,
      payment_currency      as PaymentCurrency,
      @Semantics.systemDateTime.createdAt: true
      created_at            as CreatedAt,
      @Semantics.user.createdBy: true
      created_by            as CreatedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at            as ChangedAt,
      @Semantics.user.lastChangedBy: true
      changed_by            as ChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt
}
