       module params_dyptop
       !////////////////////////////////////////////////////////////////
       !  Module contains global variables used for the TOPMODEL
       !  Benjamin Stocker, May 2013 - June 2014         
       !----------------------------------------------------------------

       implicit none

       ! Minimum grid cell fraction of peatland area
       real, parameter :: min_peat_fraction = 0.00001

       ! Minimum number of months in previous 'ptbuf' (s. soil.inc) years  where area has
       ! to be flooded in order to be classified as (potential) peatland
       integer, parameter :: min_peat_inundmonths = 18

       ! Maximum peatland area rate of change (per year)
       real, parameter :: max_peat_areachangerate = 0.01

       ! Minimum C stored in soil to classify as peatland (in gC/m2)
       real, parameter:: min_peat_amount = 50000.

       ! Minimum peat C input in slow soil pool to classify as peatland (in gC/m2)
       real, parameter :: min_peat_balance = 10.

       ! Minimum precipitation over eq. evapotranspiration for the initiation of peatlands
       real, parameter :: min_poaet = 1.
       

       end module params_dyptop

