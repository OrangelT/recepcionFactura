@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Factura Electronica Monitor'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_P_0074U  
with parameters
P_DIA : sydate,
P_TOTDIAS : zdte_a_evaluation_period
as select from ZDDL_I_0074_1( P_DIA: $parameters.P_DIA,  P_TOTDIAS :  $parameters.P_TOTDIAS ) as a 
//left outer join ZDDL_I_0074CP_1 as d
//    on  d.bukrs = a.bukrs and
//        d.uuid = a.uuid
left outer join 
//bsak 
zddl_bsak as c
on  c.bukrs = a.bukrs
and c.belnr = a.zdocfi
and c.gjahr = a.zanodoc
{   
 a.bukrs, 
 a.xblnr, 
 a.stcd1, 
 a.tipodte, 
 a.lifnr, 
 a.bldat, 
 a.belnr, 
 a.gjahr, 
 c.augbl, 
 a.zterm, 
 a.name1, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.wrbtr, 
 a.waers, 
 a.ebeln, 
 a.texto, 
 a.nombana, 
 a.aprobador, 
 a.diasing, 
 a.glosa, 
 a.docnum, 
 a.tcode, 
 a.status, 
 a.fchcancel, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.mntcancel, 
 a.termpagocdg, 
 a.termpagoglosa, 
 a.termpagodias, 
 a.fchvenc, 
 a.rutemisor, 
 a.rznsoc, 
 a.giroemis, 
 a.correoemisor, 
 a.dirorigen, 
 a.cmnaorigen, 
 a.ciudadorigen, 
 a.rutmandante, 
 a.rutrecep, 
 a.rznsocrecep, 
 a.totitems, 
 a.totbultos, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.mntneto, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.mntexe, 
 a.tasaiva, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.iva, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.ivaprop, 
      @Semantics.amount.currencyCode: 'WAERS'
 a.ivaterc, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.ivanoret, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.mnttotal, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.vlrpagar, 
 a.tpocambio, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.mntnetootrmnda, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.mntexeotrmnda, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.ivaotrmnda, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.ivanoretotrmnda, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.mnttototrmnda, 
 a.fchpago, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.mntpago, 
 a.periododesde, 
 a.tipoimp, 
 a.tasaimp, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.montoimp, 
 a.tipoimpotrmnda, 
 a.tasaimpotrmnda, 
 @Semantics.amount.currencyCode: 'WAERS'
 a.valorimpotrmnda, 
 a.filename, 
 a.codlib, 
 a.fechabase, 
 a.fechaaprob, 
 a.horaaprob, 
 a.aprobador_real, 
 a.texto_libre, 
 a.cpudt, 
 a.cputm, 
 a.fmapago, 
 a.zdte_tipo, 
 a.formapago, 
 a.fecharsii as zfecharsii, 
 a.desc_tipo, 
 a.awkey, 
 a.icono as icon, 
 a.zdocfi, 
 a.zfechacont, 
 a.zanodoc,
 // verificar si dias transcurridos excede los 99
 case when a.dias_trans > 999
        then 999
        else a.dias_trans
        end as dias_trans,
 a.tiporef,
 a.folioref
 // Inicio. Campos Mexico   
 //a.frmpagomx,
 //a.descuento,
 //a.metodopagomx,
 //a.regfiscmx,
 //a.usocfdi,
 //a.uuid,
 //a.montoret,
 //a.numregidtrib,
 //d.uuidp,
 //d.fechapago,
 //d.fmpagop,
 //d.monto,
 //@EndUserText.label: 'MonedaPago' 
 //d.waers as waerspago,
 //d.foliocp,
 //a.tiporelacion,
// a.estadocfdi,
 //a.estadocanc,
 //a.cancelable
// Fin. Campos Mexico 
}
union all select from ZDDL_I_0074FR_1(  P_DIA: $parameters.P_DIA,  P_TOTDIAS :  $parameters.P_TOTDIAS ) as b
 //left outer join ZDDL_I_0074CP_1 as c
   // on  c.bukrs = b.bukrs and
  //      c.uuid = b.uuid
{
b.bukrs, 
b.xblnr, 
b.stcd1,
b.tipodte, 
b.lifnr, 
b.bldat, 
b.belnr, 
b.gjahr, 
b.augbl, 
b.zterm, 
b.name1, 
// @Semantics.amount.currencyCode: 'WAERS'
b.WRBTR, 
b.waers, 
b.ebeln, 
b.texto, 
b.nombana, 
b.aprobador, 
b.diasing, 
b.glosa, 
b.docnum, 
b.tcode, 
b.status, 
b.fchcancel, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mntcancel, 
b.termpagocdg, 
b.termpagoglosa, 
b.termpagodias, 
b.fchvenc, 
b.rutemisor, 
b.rznsoc, 
b.giroemis, 
b.correoemisor, 
b.dirorigen, 
b.cmnaorigen, 
b.ciudadorigen, 
b.rutmandante, 
b.rutrecep, 
b.rznsocrecep, 
b.totitems, 
b.totbultos, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mntneto, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mntexe, 
b.tasaiva, 
//@Semantics.amount.currencyCode: 'WAERS'
b.iva, 
//@Semantics.amount.currencyCode: 'WAERS'
b.ivaprop, 
//@Semantics.amount.currencyCode: 'WAERS'
b.ivaterc, 
//@Semantics.amount.currencyCode: 'WAERS'
b.ivanoret, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mnttotal, 
//@Semantics.amount.currencyCode: 'WAERS'
b.vlrpagar, 
b.tpocambio, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mntnetootrmnda, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mntexeotrmnda, 
//@Semantics.amount.currencyCode: 'WAERS'
b.ivaotrmnda, 
//@Semantics.amount.currencyCode: 'WAERS'
b.ivanoretotrmnda, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mnttototrmnda, 
b.fchpago, 
//@Semantics.amount.currencyCode: 'WAERS'
b.mntpago, 
b.periododesde, 
b.tipoimp, 
b.tasaimp, 
//@Semantics.amount.currencyCode: 'WAERS'
b.montoimp, 
b.tipoimpotrmnda, 
b.tasaimpotrmnda, 
b.valorimpotrmnda, 
//@Semantics.amount.currencyCode: 'WAERS'
b.filename, 
b.codlib, 
b.fechabase, 
b.fechaaprob, 
b.horaaprob, 
b.aprobador_real, 
b.texto_libre, 
b.cpudt, 
b.cputm, 
b.fmapago, 
b.zdte_tipo, 
b.formapago, 
b.fecharsii as zfecharsii, 
b.desc_tipo, 
b.awkey, 
b.icon, 
b.zdocfi, 
b.zfechacont, 
b.zanodoc,
 // verificar si dias transcurridos excede los  
 case when b.dias_trans > 999
        then 999
        else b.dias_trans
        end as dias_trans,
 b.tiporef,       
 b.folioref
  // Inicio. Campos Mexico   
   // b.frmpagomx,
   // b.descuento,
   // b.metodopagomx,
   // b.regfiscmx,
   // b.usocfdi,
   // b.uuid,
   // b.montoret,
  //  b.numregidtrib,
 //c.uuidp,
 //c.fechapago,
// c.fmpagop,
 //c.monto,
 //@EndUserText.label: 'MonedaPago' 
 //c.waers as waerspago,
 //c.foliocp,
 //b.tiporelacion,
 //b.estadocfdi,
// b.estadocanc,
// b.cancelable
// Fin. Campos Mexico 

}
