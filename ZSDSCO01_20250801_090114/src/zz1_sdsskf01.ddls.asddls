@AbapCatalog.sqlViewName : 'ZZ1_IBAMCXF9NHX7'
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

@EndUserText.label: 'SDS_Cost Center SKF Report'

@ObjectModel.supportedCapabilities:[#ANALYTICAL_QUERY]
define view ZZ1_SDSSKF01  with parameters
    //@AnalyticsDetails.query.variableSequence: 30
    
    //    @Consumption.valueHelp: '_PlanningCategory'
    @Consumption.valueHelpDefinition: [{
     entity: {
         name:    'I_PlanningCategory',
         element: 'PlanningCategory'
             }
    }]
    @Consumption.hidden :false
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    P_PlanningCategory    : fcom_category,
     @EndUserText.label: 'GLAccount Hierarchy'
     @Consumption.valueHelpDefinition: [{
    entity: {
     name:    'I_GLAccountHierarchyStdVH',
     //   I_FinancialStatementHierarchy
     element: 'GLAccountHierarchy'
         }
    }]
     @Consumption.hidden :false
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     

    P_GLAccountHierarchy  : fins_sem_tag_hryid,
//    @EndUserText.label: 'Statistical Key Figure Planning Category'
//    @Consumption.valueHelpDefinition: [{
//     entity: {
//         name:    'I_PlanningCategory',
//         element: 'PlanningCategory'
//             }
//    }]
//
//    P_StatKeyFigurePlngCategory : fcom_skf_category,
    @Consumption.hidden: true
    @Environment.systemField: #USER
    P_BusinessUser        : syuname,
    @Consumption.hidden: true
    @Consumption.derivation: { lookupEntity: 'I_UserSetGetParamForCtrlgArea',
          resultElement: 'ControllingArea',
          binding: [ { targetElement : 'BusinessUser' , type : #PARAMETER, value : 'P_BusinessUser' } ] }

    P_ControllingArea     : kokrs,
    @Consumption.hidden: true
    @Consumption.derivation: { lookupEntity: 'I_Ledger',
    resultElement: 'Ledger',
    binding:
    [ { targetElement : 'IsLeadingLedger' ,
        type : #CONSTANT,
        value : 'X'
      }
    ]
    }
    P_Ledger              : fins_ledger



  as select from I_ActualPlanStatKeyFigSemTag 
{



  @Consumption.filter: { selectionType: #SINGLE , mandatory: false }
  CompanyCode,
  
  
  //Ledger Fiscal Year has to be included
//  LedgerFiscalYear, use this one instead
  @AnalyticsDetails.query.display: #KEY
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #HIDE
  @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: true, mandatory: true }
  FiscalYear,
  @Consumption.filter: { selectionType: #RANGE, multipleSelections: true, mandatory: false }
  @AnalyticsDetails.query.totals: #HIDE
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.display: #KEY_TEXT
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
  @Consumption.filter: {  selectionType: #HIERARCHY_NODE, multipleSelections: true, mandatory: false,
                        hierarchyBinding : [{type : #USER_INPUT, value : 'P_CostCenterHierarchyName', variableSequence: 35},{type : #PARAMETER, value : 'P_ControllingArea',variableSequence: 36} ] }
  CostCenter,
  @AnalyticsDetails.query.hidden: true
  @AnalyticsDetails.query.axis:#ROWS
  @AnalyticsDetails.query.hierarchyBinding : [
{type :#ELEMENT,value :'ControllingArea'},
{type :#CONSTANT,value :'#'}
]
  @AnalyticsDetails.query.displayHierarchy: #OFF
  @AnalyticsDetails.query.hierarchyInitialLevel: 3
  @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: false }
  
  StatisticalKeyFigure,
FiscalYearPeriod,
  //@AnalyticsDetails.query.hidden: true
  @Consumption.hidden: true
  AmountInCompanyCodeCurrency,
  @AnalyticsDetails.query.hidden: true
  StatisticalKeyFigQtyUnit,
   @AnalyticsDetails.query.hidden: true
  StatisticalKeyFigureQuantity, //SKFHCQuantity,
  
  @AnalyticsDetails.query.onCharacteristicStructure: true
  @EndUserText.label: 'Actual'
  @AnalyticsDetails.query.axis: #COLUMNS
  @AnalyticsDetails.query.decimals: 3
  //@AnalyticsDetails.query.elementHierarchy.parent: 'PlanActualDeltaValue'
  case when ( ActualPlanCode = 'A' ) then 1 end                                                        as ActualValue,

  @AnalyticsDetails.query.onCharacteristicStructure: true
  @EndUserText.label: 'Plan'
  @AnalyticsDetails.query.decimals: 3
  @AnalyticsDetails.query.axis: #COLUMNS
  //@AnalyticsDetails.query.elementHierarchy.parent: 'PlanActualDeltaValue'
  case when ( ActualPlanCode = 'P' ) then 1 end                                                        as PlanValue,
  
  @AnalyticsDetails.query.onCharacteristicStructure: true
  @EndUserText.label: 'Delta(Plan-Actual)'
  @AnalyticsDetails.query.axis: #COLUMNS
  @Aggregation.default: #FORMULA
  @AnalyticsDetails.query.decimals: 3
  @AnalyticsDetails.query.collisionHandling: { formula: #THIS}
  @AnalyticsDetails.query.formula: '$projection.PlanValue - $projection.ActualValue'
  1                                                                                                    as   PlanActualDeltaValue,
         @Consumption.hidden : true
         $parameters.P_PlanningCategory as P_PlanningCategory_param,
         @Consumption.hidden : true
         $parameters.P_GLAccountHierarchy as P_GLAccountHierarchy_param,
         @Consumption.hidden : true
         $parameters.P_BusinessUser as P_BusinessUser_param,
         @Consumption.hidden : true
         $parameters.P_ControllingArea as P_ControllingArea_param,
         @Consumption.hidden : true
         $parameters.P_Ledger as P_Ledger_param,
         @Consumption.hidden : true
         @Consumption.filter :{ selectionType: #SINGLE, multipleSelections: false, mandatory: true }
         
         StatKeyFigurePlanningCategory,
         @Consumption.hidden : true
         ControllingArea,
         @Consumption.hidden : true
         Ledger


}
where 
 ( 
	ControllingArea	=	:P_ControllingArea
 ) 

 and 
 ( 
	Ledger	=	:P_Ledger
 )              ;
