@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fecha relevantes'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zddl_p_fecharelevante 
with parameters 
    P_Date : sydate,
    P_EvaluationTimeFrameInDays : zdte_a_evaluation_period
    as select from ZDDL_P_calendarDate(P_Date :$parameters.P_Date ) as a
     {
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
a.DaysBetween
    
} where  DaysBetween >= 0 and DaysBetween < $parameters.P_EvaluationTimeFrameInDays
