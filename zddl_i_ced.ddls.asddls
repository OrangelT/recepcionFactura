@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Factura para cedibles contabyNoComp'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_CED
with parameters 
    P_dia : sydate,
    P_TOTDIAS : zdte_a_evaluation_period
     as select from ZDDL_I_0074_1( P_DIA: $parameters.P_dia, P_TOTDIAS: $parameters.P_TOTDIAS ) as a 
inner join
 //bsik 
 zddl_Bsik as b 
on  b.bukrs = a.bukrs  and
    b.belnr = a.zdocfi and
    b.gjahr = a.zanodoc 
{
a.bukrs,
a.xblnr,
a.stcd1,
a.tipodte,
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
a.fecharsii,
a.desc_tipo,
a.icono,
a.zdocfi,
a.today,
a.zfechacont,
a.zanodoc,
a.dias_trans,
a.tiporef,
a.folioref,
a.periodo,
a.aprobador,
a.awkey
    
} where a.status = '5'
