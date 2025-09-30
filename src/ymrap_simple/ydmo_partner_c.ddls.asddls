@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Partner Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity YDMO_PARTNER_C
  provider contract transactional_query
  as projection on YDMO_PARTNER_I
{
  key PartnerNumber,
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
