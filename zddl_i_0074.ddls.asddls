@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cabecera fact.Elect SII'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_0074 
with parameters
P_DIA : sydate,
P_TOTDIAS : zdte_a_evaluation_period
as select from ztfi_0074 as a 
//** Restricción por fecha
inner join zddl_p_fecharelevante( P_Date: $parameters.P_DIA , P_EvaluationTimeFrameInDays : $parameters.P_TOTDIAS ) as _Date 
    on a.bldat = _Date.CalendarDate
left outer join lfb1 as b
    on a.lifnr = b.lifnr  and 
       a.bukrs = b.bukrs 
left outer join dd07t as c
    on c.domname = 'ZFI_DOM_TIPO' and
       c.ddlanguage = 'S' and
       c.as4local ='A' and
       c.domvalue_l = b.zzdte_tipo   
left outer join ztfi_0074fr as d
on d.bukrs = a.bukrs and
   d.xblnr = a.xblnr and
   d.tipodte = a.tipodte and
   d.stcd1 = a.stcd1
left outer join ZDDL_I_0077 as e
on
// e.mandt = a.mandt and
e.bukrs = a.bukrs and
e.tipodte = a.tipodte and
e.xblnr = a.xblnr and
e.lifnr = a.lifnr 
left outer join ZDDL_I_BUKRSE as _buk
on _buk.bukrs = a.bukrs                                                                                                                           

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
// Mod. Para Zterm No actualizada muestre la del proveedor    
    case a.zterm 
    when '' then b.zterm
    else a.zterm
    end as zterm,         
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
 //   a.tiporelacion,
    b.zzdte_tipo as zdte_tipo,
    case a.fmapago
    when '1' then 'Contado'
    when '2' then 'Credito' 
    when '3' then 'Gratuito' 
    else ''
    end as formapago,
    c.ddtext as desc_tipo,
    d.fecharsii,
     case a.belnr
    when '' then ''
    else 
        case a.tcode
//        when 'FBV1' then concat(concat(a.belnr, a.bukrs), a.gjahr)
//        when 'FBV3' then concat(concat(a.belnr, a.bukrs), a.gjahr)
//        when 'MIR4' then concat(a.belnr, a.gjahr)
         when 'FBV1' then replace(concat(concat(a.belnr, _buk.bukrse), a.gjahr),'|-|','')
         when 'FBV3' then replace(concat(concat(a.belnr, _buk.bukrse), a.gjahr),'|-|','')
         when 'MIR4' then concat(a.belnr, a.gjahr)
        else ''
        end
    end as awkey,
//Esto para indice BKPF-4    
      case a.belnr
    when '' then ''
    else 
        case a.tcode
        when 'FBV1' then 'BKPFF'
        when 'FBV3' then 'BKPFF'
        when 'MIR4' then 'RMRP'
        else ''
        end
    end as awtyp,
    '' as awsys,
    $parameters.P_DIA as today,
    e.tiporef,
    e.folioref,
    d.periodo,
    _Date.CalendarWeek    as CalendarWeek,
    _Date.CalendarMonth   as CalendarMonth,
    _Date.CalendarQuarter as CalendarQuarter,
    _Date.CalendarYear    as CalendarYear
    
}
