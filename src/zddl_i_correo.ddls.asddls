@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds Notificacion Mon.Rec.Doc.Electrónico'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zddl_i_correo as select from ztfi_0074 as a 
left outer join ztfi_0078 as b 
 on b.bukrs = a.bukrs
{
key a.bukrs as Bukrs,
key a.xblnr as Xblnr,
key a.tipodte as Tipodte,
key a.stcd1 as Stcd1,
a.lifnr as Lifnr,
a.bldat as Bldat,
a.belnr as Belnr,
a.gjahr as Gjahr,
a.augbl as Augbl,
a.zterm as Zterm,
a.name1 as Name1,
 @Semantics.amount.currencyCode: 'WAERS'
a.wrbtr as Wrbtr,
a.waers as Waers,
a.ebeln as Ebeln,
a.texto as Texto,
a.nombana as Nombana,
a.aprobador as Aprobador,
a.diasing as Diasing,
a.glosa as Glosa,
a.docnum as Docnum,
a.tcode as Tcode,
a.status as Status,
a.fchcancel as Fchcancel,
 @Semantics.amount.currencyCode: 'WAERS'
a.mntcancel as Mntcancel,
a.termpagocdg as Termpagocdg,
a.termpagoglosa as Termpagoglosa,
a.termpagodias as Termpagodias,
a.fchvenc as Fchvenc,
a.rutemisor as Rutemisor,
a.rznsoc as Rznsoc,
a.giroemis as Giroemis,
a.correoemisor as Correoemisor,
a.dirorigen as Dirorigen,
a.cmnaorigen as Cmnaorigen,
a.ciudadorigen as Ciudadorigen,
a.rutmandante as Rutmandante,
a.rutrecep as Rutrecep,
a.rznsocrecep as Rznsocrecep,
a.totitems as Totitems,
a.totbultos as Totbultos,
 @Semantics.amount.currencyCode: 'WAERS'
a.mntneto as Mntneto,
 @Semantics.amount.currencyCode: 'WAERS'
a.mntexe as Mntexe,
a.tasaiva as Tasaiva,
 @Semantics.amount.currencyCode: 'WAERS'
a.iva as Iva,
 @Semantics.amount.currencyCode: 'WAERS'
a.ivaprop as Ivaprop,
 @Semantics.amount.currencyCode: 'WAERS'
a.ivaterc as Ivaterc,
 @Semantics.amount.currencyCode: 'WAERS'
a.ivanoret as Ivanoret,
 @Semantics.amount.currencyCode: 'WAERS'
a.mnttotal as Mnttotal,
 @Semantics.amount.currencyCode: 'WAERS'
a.vlrpagar as Vlrpagar,
a.tpocambio as Tpocambio,
 @Semantics.amount.currencyCode: 'WAERS'
a.mntnetootrmnda as Mntnetootrmnda,
 @Semantics.amount.currencyCode: 'WAERS'
a.mntexeotrmnda as Mntexeotrmnda,
 @Semantics.amount.currencyCode: 'WAERS'
a.ivaotrmnda as Ivaotrmnda,
 @Semantics.amount.currencyCode: 'WAERS'
a.ivanoretotrmnda as Ivanoretotrmnda,
 @Semantics.amount.currencyCode: 'WAERS'
a.mnttototrmnda as Mnttototrmnda,
a.fchpago as Fchpago,
 @Semantics.amount.currencyCode: 'WAERS'
a.mntpago as Mntpago,
a.periododesde as Periododesde,
a.tipoimp as Tipoimp,
a.tasaimp as Tasaimp,
 @Semantics.amount.currencyCode: 'WAERS'
a.montoimp as Montoimp,
a.tipoimpotrmnda as Tipoimpotrmnda,
a.tasaimpotrmnda as Tasaimpotrmnda,
 @Semantics.amount.currencyCode: 'WAERS'
a.valorimpotrmnda as Valorimpotrmnda,
a.filename as Filename,
a.codlib as Codlib,
a.fechabase as Fechabase,
a.fechaaprob as Fechaaprob,
a.horaaprob as Horaaprob,
a.aprobador_real as AprobadorReal,
a.texto_libre as TextoLibre,
a.zfechapro as Zfechapro,
a.zfecharec as Zfecharec,
a.cpudt as Cpudt,
a.cputm as Cputm,
a.fmapago as Fmapago,
a.dummy as Dummy,
a._dataaging as Dataaging,
b.correo_idoc as correo_idoc,
b.sender as sender,
b.correo_texto as correo_texto
}
