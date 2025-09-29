@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Partner Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity YDMO_PARTNER_C
  provider contract transactional_query
  as projection on YDMO_PARTNER_I
{
  @EndUserText.label: 'Partner Number'
  key PartnerNumber,
  @EndUserText.label: 'Partner Name'
      PartnerName,
      Street,
      City,
      Country,
      PaymentCurrency,      
      CreatedAt,      
      CreatedBy,      
      ChangedAt,      
      ChangedBy,      
      LocalLastChangedAt
}
