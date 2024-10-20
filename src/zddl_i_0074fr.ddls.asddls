@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cabecera fact.Elect SII'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_I_0074FR 
with parameters
   P_DIA : sydate,
   P_TOTDIAS : zdte_a_evaluation_period
   as select from ztfi_0074fr as a
//Colocar fechas de bldad y registro en ZFI0074FR
inner join zddl_p_fecharelevante(P_Date : $parameters.P_DIA , P_EvaluationTimeFrameInDays : $parameters.P_TOTDIAS ) as _Date
   on a.bldat = _Date.CalendarDate
left outer join ztfi_0074 as s
  on s.bukrs = a.bukrs and 
     s.xblnr = a.xblnr and
     s.tipodte = a.tipodte and
     s.stcd1 = a.stcd1
     
left outer join lfa1 as d
on d.stcd1 = a.stcd1  and 
   d.sperr = ' ' and
   d.sperq = ' ' and
   d.loevm = ' '  
   
left outer join lfb1 as b
    on b.lifnr =  d.lifnr and 
       b.bukrs =  a.bukrs and
       b.loevm =  ' '     and
       b.sperr  = ' '
       
left outer join dd07t as c
    on c.domname = 'ZFI_DOM_TIPO' and
       c.ddlanguage = 'S' and
       c.as4local ='A' and
       c.domvalue_l = b.zzdte_tipo
{ 
 //   a.mandt,     
    a.bukrs, 
    a.xblnr,  
    a.tipodte, 
    a.stcd1, 
    a.fecharsii, 
    a.horarsii, 
    d.lifnr,
    a.bldat, 
    s.belnr, 
    s.gjahr, 
    s.augbl, 
    s.zterm,     
 //   s.name1, 
    case  
    when s.name1 is null then  substring( a.rznsoc, 1, 30)
    else s.name1
    end as name1,  
     @Semantics.amount.currencyCode: 'WAERS'    
    a.mnttotal as WRBTR,    
    case
    when s.waers is null then a.waers
    else s.waers 
    end as waers, 
    s.ebeln, 
    s.texto, 
    s.nombana, 
    s.aprobador, 
    s.diasing, 
    s.glosa, 
 //   s.docnum, 
    a.docnum,
    s.tcode,    
    case 
    when s.status is null then         
        case a.exml
        when 'X' then 'A'
        else 'S'
        end
    else  s.status   
    end as status,  
    s.fchcancel, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.mntcancel, 
    s.termpagocdg, 
    s.termpagoglosa, 
    s.termpagodias, 
    s.fchvenc, 
    case
    when s.rutemisor is null then a.rutemisor
    else a.rutemisor
    end as rutemisor,
    s.rznsoc, 
    s.giroemis, 
    s.correoemisor, 
    s.dirorigen, 
    s.cmnaorigen, 
    s.ciudadorigen, 
    s.rutmandante, 
    case 
   when s.rutrecep is null then a.rutrecep
        else s.rutrecep
         end as rutrecep,
    s.rznsocrecep, 
    s.totitems, 
    s.totbultos, 
//  s.mntneto,  
 @Semantics.amount.currencyCode: 'WAERS'   
    a.mntneto, 
//  s.mntexe,     
 @Semantics.amount.currencyCode: 'WAERS'
    a.mntoexe as mntexe, 
    s.tasaiva, 
//    s.iva, 
 @Semantics.amount.currencyCode: 'WAERS'
    a.ivarec as iva,
     @Semantics.amount.currencyCode: 'WAERS'
    s.ivaprop, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.ivaterc, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.ivanoret, 
     @Semantics.amount.currencyCode: 'WAERS'
    a.mnttotal,
     @Semantics.amount.currencyCode: 'WAERS'
    s.vlrpagar, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.tpocambio, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.mntnetootrmnda, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.mntexeotrmnda,
     @Semantics.amount.currencyCode: 'WAERS' 
    s.ivaotrmnda, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.ivanoretotrmnda, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.mnttototrmnda, 
    s.fchpago, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.mntpago, 
    s.periododesde, 
    s.tipoimp, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.tasaimp, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.montoimp, 
    s.tipoimpotrmnda, 
    s.tasaimpotrmnda, 
     @Semantics.amount.currencyCode: 'WAERS'
    s.valorimpotrmnda, 
    s.filename, 
    s.codlib, 
    s.fechabase, 
    s.fechaaprob, 
    s.horaaprob, 
    s.aprobador_real, 
    s.texto_libre, 
    s.cpudt, 
    s.cputm, 
    s.fmapago,
//    s.tiporelacion,
    b.zzdte_tipo as zdte_tipo,
    $parameters.P_DIA as today, 
    case s.fmapago
    when '1' then cast('Credito' as char10 )
    when '2' then cast('Contado' as char10 )
    when '3' then cast('Gratuito' as char10 )
    end as formapago,
    c.ddtext as desc_tipo,
    case s.belnr
    when '' then ''
    else 
        case s.tcode
        when 'FBV1' then concat(concat(s.belnr, s.bukrs), s.gjahr)
        when 'FBV3' then concat(concat(s.belnr, s.bukrs), s.gjahr)
        when 'MIR4' then concat(s.belnr, s.gjahr)
        else ''
        end
    end as awkey ,
 a.exml,
 _Date.CalendarWeek    as CalendarWeek,
 _Date.CalendarMonth   as CalendarMonth,
 _Date.CalendarQuarter as CalendarQuarter,
 _Date.CalendarYear    as CalendarYear
}
