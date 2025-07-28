!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_y.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'y_turbine')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wp_opt(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wp_opt(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'y_turbine')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wp_opt(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wp_opt(id).EQ.3
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
