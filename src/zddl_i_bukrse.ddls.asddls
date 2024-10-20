@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cod. Sociedad editada'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_BUKRSE as select from  t001 as a {
key a.bukrs,
case length(a.bukrs)
when 1 then concat( a.bukrs,'|-|   |-|' )
when 2 then concat( a.bukrs,'|-|  |-|' ) 
when 3 then concat( a.bukrs,'|-| |-|' )
when 4 then concat( a.bukrs,'|-||-|' )
end as bukrse
}
