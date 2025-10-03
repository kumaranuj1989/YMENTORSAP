@EndUserText.label: 'Abstract Entity for pop up'
define abstract entity ydmo_popup
{
  @Consumption.valueHelpDefinition: [{ entity: { name: 'YDMO_COUNTRY_I_VH', element: 'Country' } }]
  @EndUserText.label: 'Search Country'
  SearchCountry : land1;
  @EndUserText.label: 'New date'
  NewDate       : abap.dats;
  @EndUserText.label: 'Message type'
  MessageType   : abap.int4;
  @EndUserText.label: 'Update data'
  FlagUpdate    : abap.char(1);
  @EndUserText.label: 'Show Messages'
  FlagMessage   : abap_boolean;

}
