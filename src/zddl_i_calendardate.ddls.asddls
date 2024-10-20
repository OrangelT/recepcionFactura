@AbapCatalog.viewEnhancementCategory: [#NONE] 
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fecha'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED    
}
define view entity ZDDL_I_CalendarDate
 as select from scal_tt_date 
 {
key scal_tt_date.calendardate as CalendarDate,
    scal_tt_date.calendaryear as CalendarYear,
    scal_tt_date.calendarquarter as CalendarQuarter,
    scal_tt_date.calendarmonth as CalendarMonth,
    scal_tt_date.calendarweek as CalendarWeek,
    scal_tt_date.calendarday as CalendarDay,
    scal_tt_date.yearmonth as YearMonth,
    scal_tt_date.yearquarter as YearQuarter,
    scal_tt_date.yearweek as YearWeek,
    scal_tt_date.weekday as WeekDay,
    scal_tt_date.firstdayofweekdate as FirstDayOfWeekDate,
    scal_tt_date.firstdayofmonthdate as FirstDayOfMonthDate,
    scal_tt_date.calendardayofyear as CalendarDayOfYear,
    scal_tt_date.yearday as YearDay
}
