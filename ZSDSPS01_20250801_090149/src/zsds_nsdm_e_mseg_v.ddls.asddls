@AbapCatalog.sqlViewAppendName:'ZSDS_E_MSEG_V'
@EndUserText.label: 'Extend view NSDM_E_MSEG'
@AccessControl.authorizationCheck:#NOT_REQUIRED
extend view nsdm_e_mseg with ZSDS_E_MSEG_E
{
   zz1_prodh
}
