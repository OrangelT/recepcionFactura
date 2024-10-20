@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fact. elect Con Datos Fi'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_0074_1 
with parameters
P_DIA : sydate,
P_TOTDIAS : zdte_a_evaluation_period
as select from ZDDL_I_0074( P_DIA: $parameters.P_DIA, P_TOTDIAS : $parameters.P_TOTDIAS ) as a
left outer join bkpf as b
    on
    // a.mandt = b.mandt and
       a.awtyp = b.awtyp and
       a.awkey = b.awkey and
       a.awsys = b.awsys
       
left outer join ztfi_0079 as c
on c.sociedad = a.bukrs and
   c.estatus = a.status   
{
key a.bukrs, 
 key a.xblnr, 
 key a.stcd1, 
 key a.tipodte, 
 a.lifnr, 
 a.bldat, 
 a.belnr, 
 a.gjahr, 
 a.augbl, 
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
  @Semantics.amount.currencyCode: 'WAERS'
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
 a.formapago, 
 a.fecharsii,
 a.desc_tipo, 
 a.awkey,
 c.icono,
 b.belnr as zdocfi,
 a.today,
 b.budat as zfechacont,
 b.gjahr as zanodoc,
 //Calculo de dias transcurridos
 // Se valida con null por el leftjoin o 00000000 con los ya grabados
 case when a.fecharsii = '00000000' or  a.fecharsii is null then 
         case when   b.budat = '00000000' or b.budat is null then 
                  cast( dats_days_between(a.bldat, a.today) as int4 )     
         else cast( dats_days_between(a.bldat, b.budat) as int4)
        end 
 else 
        case when  b.budat = '00000000' or  b.budat is null then  
                  cast( dats_days_between( a.fecharsii, a.today ) as int4 ) 
        else cast( dats_days_between(a.fecharsii, b.budat ) as int4)
        end
        
 end as dias_trans ,
 a.tiporef,
 a.folioref,
 a.periodo,
 a.CalendarMonth,
 a.CalendarQuarter,
 a.CalendarWeek,
 a.CalendarYear

}
