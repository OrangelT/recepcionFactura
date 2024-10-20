@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cabecera fact.Elect Solo  rec SII'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_0074FR_1 
with parameters
P_DIA : sydate,
P_TOTDIAS : zdte_a_evaluation_period
as select from ZDDL_I_0074FR ( P_DIA: $parameters.P_DIA ,  P_TOTDIAS : $parameters.P_TOTDIAS )  as a 
left outer join ztfi_0079 as c
on c.sociedad = a.bukrs and
   c.estatus = a.status
 { 
  a.bukrs, 
  a.xblnr, 
  a.tipodte, 
  a.stcd1, 
  a.fecharsii, 
  a.horarsii, 
  a.lifnr, 
  a.bldat, 
  a.belnr, 
  a.gjahr, 
  a.augbl, 
  a.zterm, 
  a.name1, 
   @Semantics.amount.currencyCode: 'WAERS'
  a.WRBTR, 
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
 // a.tiporelacion,
  a.zdte_tipo, 
  a.today, 
  a.formapago, 
  a.desc_tipo, 
  a.awkey,
  c.icono as icon,
  cast ('' as char10) as zdocfi,
  cast ('00000000' as abap.dats) as zfechacont,
  cast ( '0000' as numc4 )  as zanodoc,
 //Calculo de dias transcurridos
 case when  a.fecharsii = '00000000' or a.fecharsii is null then             
         cast( dats_days_between(a.bldat, a.today) as int4 )                   
 else 
        cast( dats_days_between( a.fecharsii, a.today) as int4 )        
 end as dias_trans, 
 cast (' ' as abap.char( 6 )) as tiporef,
 cast (' ' as abap.char( 18 )) as folioref,
 a.CalendarMonth,
 a.CalendarQuarter,
 a.CalendarWeek,
 a.CalendarYear
  
} where  a.status = 'S'  or a.status ='A'
