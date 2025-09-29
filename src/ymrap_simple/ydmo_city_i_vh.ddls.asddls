@EndUserText.label: 'City Interface for value help'
@ObjectModel.query.implementedBy: 'ABAP:YCL_DMO_CITY_QUERY'
define custom entity ydmo_city_i_vh
{
  key City      : abap.char(60);
      CityShort : abap.char(10);

}
