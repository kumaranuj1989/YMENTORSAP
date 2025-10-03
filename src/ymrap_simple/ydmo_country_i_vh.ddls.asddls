@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface for country value help'
@Search.searchable: true                        //Used for general search field is displayed in the view
@ObjectModel.resultSet.sizeCategory: #XS        // used to display search help has dropdown
define view entity ydmo_country_i_vh
  as select from I_Country
{
  @Search.fuzzinessThreshold: 0.8
  @Search.defaultSearchElement: true      //used to activate the field for search 
  @Search.ranking: #HIGH
  //@ObjectModel.text.element: [ 'Description' ]  //used to display only the text for the key field
  @ObjectModel.text.association: '_Text'          //used to display only the text & Key for the key field
  key Country,
  @Semantics.text: true
  @Search.fuzzinessThreshold: 0.8
  @Search.defaultSearchElement: true
  @Search.ranking: #LOW
      _Text[1: Language = $session.system_language].CountryName as Description,
      _Text
}
