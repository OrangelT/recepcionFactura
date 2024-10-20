@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fecha de calendario'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDDL_P_calendarDate 
with parameters 
    P_Date : sydate
as select from ZDDL_I_CalendarDate as a {
 a.CalendarDate,
 a.CalendarYear,
 a.CalendarQuarter,
 a.CalendarMonth,
 a.CalendarWeek,
 a.CalendarDay,
 a.YearMonth,
 a.YearQuarter,
 a.YearWeek,
 a.WeekDay,
 a.FirstDayOfWeekDate,
 a.FirstDayOfMonthDate,
 a.CalendarDayOfYear,
 a.YearDay,
 dats_days_between (a.CalendarDate, $parameters.P_Date) as DaysBetween
 }
