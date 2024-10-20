@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Analisis de Historial de Pedido'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_EKBE as select from ekbe as a
{
   key a.ebeln ,
   key a.ebelp,
   key a.bewtp,
   key a.shkzg,
   key a.lsmeh,
   key a.waers,
   @Semantics.quantity.unitOfMeasure:'LSMEH'
   sum ( a.menge ) as totalc,
   @Semantics.amount.currencyCode: 'WAERS'
   sum ( a.dmbtr ) as totalv
    
}
group by a.ebeln , a.ebelp, a.bewtp, a.shkzg, a.lsmeh, a.waers  
 
