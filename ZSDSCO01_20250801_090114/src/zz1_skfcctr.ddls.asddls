@AbapCatalog.sqlViewName : 'ZZ1_6B5CA98K4YY2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #PRIVILEGED_ONLY

@Analytics.query: true
@VDM.viewType: #CONSUMPTION

@Analytics.settings.maxProcessingEffort: #HIGH
@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.buffering.status: #NOT_ALLOWED
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.sizeCategory: #XL
@ObjectModel.usageType.dataClass: #MIXED

@EndUserText.label: 'Cost Center Ststcl Key Figure KPI'

@ObjectModel.supportedCapabilities:[#ANALYTICAL_QUERY]
define view ZZ1_SKFCCTR  



  as select from I_ActualPlanStatKeyFigSemTag 
{



  
  @AnalyticsDetails.query.hierarchyBinding : [
{type :#CONSTANT,value :'#'}
]
  @AnalyticsDetails.query.displayHierarchy: #OFF
  @AnalyticsDetails.query.hierarchyInitialLevel: 3
  @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: true }
  CompanyCode,
  
  
  //Ledger Fiscal Year has to be included
//  LedgerFiscalYear, use this one instead
  @AnalyticsDetails.query.display: #KEY
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #HIDE
  @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: true }
  
  
  
  
  FiscalYear,
  
  @AnalyticsDetails.query.totals: #HIDE
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.display: #KEY
  @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: false }
  FiscalPeriod,
//  @Consumption.filter: { selectionType: #SINGLE, multipleSelections: true, mandatory: true }
//  @AnalyticsDetails.query.totals: #HIDE
//  @AnalyticsDetails.query.axis: #ROWS
//  //@AnalyticsDetails.query.display: #KEY
////   @Consumption.valueHelpDefinition: [{
////     entity: {
////         name:    'I_ProfitCenterVH',
////         element: 'ProfitCenter'
////             }
////    }]
//  ProfitCenter,
  
  @AnalyticsDetails.query.totals: #HIDE
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.variableSequence : 40
  
  @AnalyticsDetails.query.displayHierarchy: #OFF
  @AnalyticsDetails.query.hierarchyInitialLevel: 3
  
  @Consumption.filter :{ selectionType: #INTERVAL, multipleSelections: true, mandatory: false }
  @AnalyticsDetails.query.hierarchyBinding : [
{type :#ELEMENT,value :'ControllingArea'},
{type :#PARAMETER,value :'P_ControllingArea'}
]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  CostCenter,
  @AnalyticsDetails.query.hidden: true
  
  @AnalyticsDetails.query.displayHierarchy: #OFF
  @AnalyticsDetails.query.hierarchyInitialLevel: 3
  
  @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: false }
  @AnalyticsDetails.query.hierarchyBinding : [
{type :#ELEMENT,value :'ControllingArea'},
{type :#CONSTANT,value :'#'}
]
  
  
  
  
  
  
  
  
  
  
  
  
  StatisticalKeyFigure, //SKFHCQuantity,
  
  @AnalyticsDetails.query.onCharacteristicStructure: true
  @EndUserText.label: 'Actual'
  @AnalyticsDetails.query.axis: #COLUMNS
  @AnalyticsDetails.query.decimals: 0
  //@AnalyticsDetails.query.elementHierarchy.parent: 'PlanActualDeltaValue'
  case when ( ActualPlanCode = 'A' ) then 1 end                                                        as ActualValue,

  @AnalyticsDetails.query.onCharacteristicStructure: true
  @EndUserText.label: 'Plan'
  @AnalyticsDetails.query.decimals: 0
  @AnalyticsDetails.query.axis: #COLUMNS
  //@AnalyticsDetails.query.elementHierarchy.parent: 'PlanActualDeltaValue'
  case 
when 
 ( 
	ActualPlanCode	=	'P'
 )  then 1
end                                                        as PlanValue,
  
  @AnalyticsDetails.query.onCharacteristicStructure: true
  @EndUserText.label: 'Delta(Plan-Actual)'
  @AnalyticsDetails.query.axis: #COLUMNS
  @Aggregation.default: #FORMULA
  @AnalyticsDetails.query.decimals: 0
  @AnalyticsDetails.query.collisionHandling: { formula: #THIS}
  @AnalyticsDetails.query.formula: '$projection.PlanValue - $projection.ActualValue'
  1                                                                                                    as   PlanActualDeltaValue,
         StatisticalKeyFigureQuantity,
         StatisticalKeyFigQtyUnit,
         FiscalYearPeriod,
         ControllingArea,
         PlanningCategory,
         @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: false }
         StatKeyFigurePlanningCategory,
         GLAccountHierarchy,
         Ledger


}
                               ;
