@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Primera referencia Para NC'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_0077_1 
as select from ztfi_0077 as a 
inner join ztfi_0078 as b
on   a.bukrs = b.bukrs 
// Tiene los tipoDte Permitidos por Variante-Sociedad
inner join ztfi_0078a as c
on c.vatint = b.vatint
and c.tipodte = a.tiporef
{
//key a.mandt,
key a.bukrs,
key a.tipodte,
key a.xblnr,
key a.lifnr,
min(a.pos) as posmin
    
} 
group by
   // a.mandt,
    a.bukrs,
    a.tipodte,
    a.xblnr,
    a.lifnr
//    a.tiporef
  
