*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
DEFINE MC_NAMETAB.
  INSERT VALUE #( FIELDNAME = &1 ) INTO TABLE GT_NAMETAB.
END-OF-DEFINITION.
