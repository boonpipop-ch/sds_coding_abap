@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_15D7041211C2'

extend view E_ALLOCATIONRUNRESULT with ZZ1_24YDUQS7HDEYEDKLJ35AWZJTDM
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  Persistence.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
