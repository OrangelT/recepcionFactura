@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Doc. de referencias para NC'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_0077 
as select from ztfi_0077 as a 
//Con este INNER solo obtengo un registro 33/34
//La zddL_i_0077 ya agrupa por la minima Posicion 
inner join ZDDL_I_0077_1 as b 
     // on  a.mandt = b.mandt  and
      on a.bukrs = b.bukrs 
      and a.xblnr = b.xblnr
      and a.tipodte = b.tipodte
      and a.lifnr = b.lifnr
      and a.pos = b.posmin
{
//key a.mandt, 
key a.bukrs, 
key a.xblnr, 
key a.lifnr, 
key a.tipodte, 
key a.tiporef, 
key a.pos, 
a.folioref, 
a.fecharef
   
}
