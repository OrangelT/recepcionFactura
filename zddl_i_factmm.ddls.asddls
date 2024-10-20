@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Facturas MM y PEDIDO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_FACTMM as select from rbkp as a
inner join rseg  as b
    on a.belnr = b.belnr and 
       a.gjahr = b.gjahr and
       a.bukrs = a.bukrs
left outer join   ztfi_0001b as c
on c.bukrs = b.bukrs and
   c.tipop = '2' and
   c.blart = a.blart and
   c.knttp = ' '                
{
    a.bukrs,
    a.belnr,
    a.gjahr,
    a.xblnr,
    a.lifnr,
    c.tipodte,     
    b.buzei,
    b.ebeln,
    b.ebelp          
} 
where a.stblg = '          '
