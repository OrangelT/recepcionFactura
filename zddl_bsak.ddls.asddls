@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'vista entidad de bsak'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zddl_bsak as select from 
bsak_bck 
{
   key bukrs,
   key lifnr,
   key umsks,
   key umskz,
   key augdt,
   key augbl,
   key zuonr,
   key gjahr,
   key belnr,
   key buzei,
   budat,
   bldat,
   cpudt,
//   @Semantics.currencyCode
   waers,
   xblnr,
   blart,
   monat,
   bschl,
   zumsk,
   shkzg,
   gsber,
   tax_country, mwskz, txdat_from,
   @Semantics.amount.currencyCode: 'WAERS'
    dmbtr,
   @Semantics.amount.currencyCode: 'WAERS' 
   wrbtr,
   @Semantics.amount.currencyCode: 'RFCCUR' cast(0 as fins_vfccur12) as fcsl,
//   @Semantics.currencyCode 
   cast('     ' as fins_currfc) as rfccur,
   @Semantics.amount.currencyCode: 'WAERS'
    mwsts,
   @Semantics.amount.currencyCode: 'WAERS'
    wmwst,
   @Semantics.amount.currencyCode: 'WAERS' 
   lwsts,
   @Semantics.amount.currencyCode: 'WAERS' 
   bdiff,
   @Semantics.amount.currencyCode: 'WAERS' 
   bdif2,
   sgtxt,
   projn,
   aufnr,
   anln1,
   anln2,
   ebeln,
   ebelp,
   saknr,
   hkont,
   fkont,
   filkd,
   zfbdt,
   zterm,
   zbd1t,
   zbd2t,
   zbd3t,
   zbd1p,
   zbd2p,
   @Semantics.amount.currencyCode: 'WAERS' skfbt,
   @Semantics.amount.currencyCode: 'WAERS' 
   sknto,
   @Semantics.amount.currencyCode: 'WAERS' wskto,
   zlsch,
   zlspr,
   zbfix,
   hbkid,
   bvtyp,
   rebzg,
   rebzj,
   rebzz,
   samnr,
   zollt,
   zolld,
   lzbkz,
   landl,
   diekz,
   mansp,
   mschl,
   madat,
   manst,
   maber,
   xnetb,
   xanet,
   xcpdd,
   xesrd,
   xzahl,
   mwsk1, txdat_from1, tax_country1,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmbt1,
   @Semantics.amount.currencyCode: 'WAERS' wrbt1, hist_tax_factor1,
   mwsk2, txdat_from2, tax_country2,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmbt2,
   @Semantics.amount.currencyCode: 'WAERS' wrbt2, hist_tax_factor2,
   mwsk3, txdat_from3, tax_country3,
   @Semantics.amount.currencyCode: 'WAERS'
    dmbt3,
   @Semantics.amount.currencyCode: 'WAERS' wrbt3, hist_tax_factor3, hist_tax_factor,
   qsskz,
   @Semantics.amount.currencyCode: 'WAERS' qsshb,
   @Semantics.amount.currencyCode: 'WAERS' qbshb,
   bstat,
   anfbn,
   anfbj,
   anfbu,
   vbund,
   rebzt,
   stceg,
   egbld,
   eglld,
   qsznr,
   @Semantics.amount.currencyCode: 'WAERS'
    qsfbt,
   xinve,
   projk,
   fipos,
   nplnr,
   aufpl,
   aplzl,
   xegdr,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmbe2,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmbe3,
   @Semantics.amount.currencyCode: 'WAERS'
    dmb21,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmb22,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmb23,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmb31,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmb32,
   @Semantics.amount.currencyCode: 'WAERS' 
   dmb33,
   @Semantics.amount.currencyCode: 'WAERS'
    mwst2,
   @Semantics.amount.currencyCode: 'WAERS' 
   mwst3,
   @Semantics.amount.currencyCode: 'WAERS' 
   sknt2,
   @Semantics.amount.currencyCode: 'WAERS' 
   sknt3,
   @Semantics.amount.currencyCode: 'WAERS' 
   bdif3,
   xragl,
   rstgr,
   uzawe,
   kostl,
   lnran,
   xstov,
   @Semantics.amount.currencyCode: 'WAERS'
    kzbtr,
   xref1,
   xref2,
   xarch,
//   @Semantics.currencyCode
    pswsl,
   @Semantics.amount.currencyCode: 'PSWSL' 
   pswbt,
   imkey,
   zekkn,
   fistl,
   geber,
   dabrz,
   xnegp,
   empfb,
   prctr,
   xref3,
   dtws1,
   dtws2,
   dtws3,
   dtws4,
   xpypr,
   kidno,
//   @Semantics.currencyCode
    pycur,
   @Semantics.amount.currencyCode: 'PYCUR' 
   pyamt,
   bupla,
   secco,
   @Semantics.amount.currencyCode: 'WAERS'
    ppdiff,
   @Semantics.amount.currencyCode: 'WAERS'
    ppdif2,
   @Semantics.amount.currencyCode: 'WAERS' 
   ppdif3,
   @Semantics.amount.currencyCode: 'WAERS'
    penlc1,
   @Semantics.amount.currencyCode: 'WAERS'
    penlc2,
   @Semantics.amount.currencyCode: 'WAERS' 
   penlc3,
   @Semantics.amount.currencyCode: 'WAERS'
    penfc,
   pendays,
   penrc,
   vertt,
   vertn,
   vbewa,
   kblnr,
   kblpos,
   grant_nbr,
   gmvkz,
   srtype,
   lotkz,
   zinkz,
   fkber,
   intreno,
   pprct,
   buzid,
   auggj,
   hktid,
   budget_pd,
   cast('          ' as psm_bdgt_account preserving type) as bdgt_account,
   cast('          ' as fagl_re_account preserving type) as re_account,
   cast('    ' as farp_payt_rsn preserving type) as payt_rsn,
   _dataaging,
   cast('  ' as kontt_fi preserving type) as kontt,
   cast('                                                  ' as kontl_fi preserving type) as kontl,
   cast('00000000' as uebgdatum) as uebgdat,
   vname,
   egrup,
   btype,
   propmano,
//<$VF>
//<$FIELDS>
//<$VF>
   gkont, gkart, ghkon,
   cast( '00000000' as pernr_d preserving type ) as pernr,
   cast( '    ' as vorgn preserving type ) as vorgn,
   cast( '     ' as awtyp preserving type ) as awtyp,
   cast( '          ' as logsystem_sender preserving type ) as logsystem_sender,
   cast( '    '       as bukrs_sender preserving type ) as bukrs_sender,
   cast( '          ' as belnr_sender preserving type ) as belnr_sender,
   cast( '0000'       as gjahr_sender preserving type ) as gjahr_sender,
   cast( '000'        as buzei_sender preserving type ) as buzei_sender,
   cast( '     ' as bcode preserving type ) as j_1tpbupl  
//from bsak_bck
// where xarch = 'X'
}
where xarch = 'X'
union all select from bseg as i
left outer join bkpf as h
 on
 // h.mandt = i.mandt and 
    h.bukrs = i.bukrs
and h.gjahr = i.gjahr
and h.belnr = i.belnr
{
   key i.bukrs,
   key i.lifnr,
   key i.umsks,
   key i.umskz,
   key i.augdt,
   key i.augbl,
   key i.zuonr,
   key i.gjahr,
   key i.belnr,
   key i.buzei,
   i.h_budat as budat,
   i.h_bldat as bldat,
   h.cpudt,
   //@Semantics.currencyCode 
   i.h_waers as waers,
   h.xblnr,
   i.h_blart as blart,
   i.h_monat as monat,
   i.bschl,
   i.zumsk,
   i.shkzg,
   i.gsber,
   i.tax_country, i.mwskz, i.txdat_from,
   //@Semantics.amount.currencyCode: 'WAERS'
    i.dmbtr,
   //@Semantics.amount.currencyCode: 'WAERS' 
   i.wrbtr,
   //@Semantics.amount.currencyCode: 'RFCCUR'    
    i.fcsl,
  // @Semantics.currencyCode                     
    i.rfccur,
   //@Semantics.amount.currencyCode: 'T001.WAERS'
    i.mwsts,
   //@Semantics.amount.currencyCode: 'WAERS'
    i.wmwst,
   //@Semantics.amount.currencyCode: 'T005.WAERS' 
   i.lwsts,
  // @Semantics.amount.currencyCode: 'T001.WAERS'
    i.bdiff,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE2' 
   i.bdif2,
   i.sgtxt,
   i.projn,
   i.aufnr,
   i.anln1,
   i.anln2,
   i.ebeln,
   i.ebelp,
   i.saknr,
   i.hkont,
   i.fkont,
   i.filkd,
   i.zfbdt,
   i.zterm,
   i.zbd1t,
   i.zbd2t,
   i.zbd3t,
   i.zbd1p,
   i.zbd2p,
   //@Semantics.amount.currencyCode: 'WAERS' 
   i.skfbt,
   //@Semantics.amount.currencyCode: 'T001.WAERS'
    i.sknto,
   //@Semantics.amount.currencyCode: 'WAERS'
    i.wskto,
   i.zlsch,
   i.zlspr,
   i.zbfix,
   i.hbkid,
   i.bvtyp,
   i.rebzg,
   i.rebzj,
   i.rebzz,
   i.samnr,
   i.zollt,
   i.zolld,
   i.lzbkz,
   i.landl,
   i.diekz,
   i.mansp,
   i.mschl,
   i.madat,
   i.manst,
   i.maber,
   h.xnetb,
   i.xanet,
   i.xcpdd,
   cast( case i.esrnr when ''
   then ''
   else 'X'
   end as xesrd preserving type ) as xesrd,
   i.xzahl,
   i.mwsk1, i.txdat_from1, i.tax_country1,
  // @Semantics.amount.currencyCode: 'RFCCUR'
    i.dmbt1,
  // @Semantics.amount.currencyCode: 'RFCCUR'
    i.wrbt1, i.hist_tax_factor1, 
   i.mwsk2, i.txdat_from2, i.tax_country2,
   //@Semantics.amount.currencyCode: 'RFCCUR'
    i.dmbt2,
  // @Semantics.amount.currencyCode: 'RFCCUR'
    i.wrbt2, i.hist_tax_factor2,
   i.mwsk3, i.txdat_from3, i.tax_country3,
  // @Semantics.amount.currencyCode: 'RFCCUR'
    i.dmbt3,
   //@Semantics.amount.currencyCode: 'RFCCUR' 
   i.wrbt3, i.hist_tax_factor3, i.hist_tax_factor,
   i.qsskz,
  // @Semantics.amount.currencyCode: 'RFCCUR'
    i.qsshb,
   //@Semantics.amount.currencyCode: 'WAERS' 
   i.qbshb,
   i.h_bstat as bstat,
   i.anfbn,
   i.anfbj,
   i.anfbu,
   i.vbund,
   i.rebzt,
   i.stceg,
   i.egbld,
   i.eglld,
   i.qsznr,
//   @Semantics.amount.currencyCode: 'BKPF.WAERS'
    i.qsfbt,
   i.xinve,
   i.projk,
   i.fipos,
   i.nplnr,
   i.aufpl,
   i.aplzl,
   i.xegdr,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE2' 
   i.dmbe2,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE3' 
   i.dmbe3,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE2' 
   i.dmb21,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE2'
    i.dmb22,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE2'
    i.dmb23,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.dmb31,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.dmb32,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.dmb33,
  // @Semantics.amount.currencyCode: 'BKPF.HWAE2'
    i.mwst2,
   //@Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.mwst3,
   //@Semantics.amount.currencyCode: 'BKPF.HWAE2' 
   i.sknt2,
   //@Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.sknt3,
   //@Semantics.amount.currencyCode: 'BKPF.HWAE3' 
   i.bdif3,
   i.xragl,
   i.rstgr,
   i.uzawe,
   i.kostl,
   i.lnran,
   h.xstov,
//   @Semantics.amount.currencyCode: 'T001.WAERS'
    i.kzbtr,
   i.xref1,
   i.xref2,
   cast( case i._dataaging when '00000000' then '' else 'X' end as xarch preserving type ) as xarch,
//   @Semantics.currencyCode
    i.pswsl,
//   @Semantics.amount.currencyCode: 'PSWSL'
    i.pswbt,
   i.imkey,
   i.zekkn,
   i.fistl,
   i.geber,
   i.dabrz,
   i.xnegp,
   i.empfb,
   i.prctr,
   i.xref3,
   i.dtws1,
   i.dtws2,
   i.dtws3,
   i.dtws4,
   i.xpypr,
   i.kidno,
//   @Semantics.currencyCode
    i.pycur,
//   @Semantics.amount.currencyCode: 'PYCUR'
    i.pyamt,
   i.bupla,
   i.secco,
//   @Semantics.amount.currencyCode: 'T001.WAERS' 
   i.ppdiff,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE2'
    i.ppdif2,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.ppdif3,
//   @Semantics.amount.currencyCode: 'BKPF.HWAER'
    i.penlc1,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE2'
    i.penlc2,
//   @Semantics.amount.currencyCode: 'BKPF.HWAE3'
    i.penlc3,
//   @Semantics.amount.currencyCode: 'BKPF.HWAER'
    i.penfc,
   i.pendays,
   h.penrc,
   i.vertt,
   i.vertn,
   i.vbewa,
   i.kblnr,
   i.kblpos,
   i.grant_nbr,
   i.gmvkz,
   i.srtype,
   h.lotkz,
   i.zinkz,
   cast( case i.fkber_long when ''
   then i.fkber
   else i.fkber_long
   end as fkber preserving type ) as fkber,
   i.intreno,
   i.pprct,
   i.buzid,
   i.auggj,
   i.hktid,
   i.budget_pd,
   i.bdgt_account,
   i.re_account,
   i.payt_rsn,
   i._dataaging,
   i.kontt,
   i.kontl,
   cast('00000000' as uebgdatum) as uebgdat,
   i.vname,
   i.egrup,
   i.btype,
   h.propmano,
//<$VF_PREFIX>
//<$FIELDS_PREFIX>
//<$VF_PREFIX>
   i.gkont, i.gkart, i.ghkon,
   i.pernr,
   i.vorgn,
   h.awtyp,
   h.logsystem_sender,
   h.bukrs_sender,
   h.belnr_sender,
   h.gjahr_sender,
   i.buzei_sender,
   i.j_1tpbupl
   }
//from bseg as i
//left outer join bkpf as h
// on h.mandt = i.mandt
//and h.bukrs = i.bukrs
//and h.gjahr = i.gjahr
//and h.belnr = i.belnr
where
  i.koart = 'K'
  and i.augbl <> ''
  and i.h_bstat <> 'D'
  and i.h_bstat <> 'M';



