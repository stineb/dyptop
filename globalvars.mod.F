       module globalvars
       !////////////////////////////////////////////////////////////////
       !  Module contains all global variables used for the TOPMODEL
       !  Benjamin Stocker, May 2013 - June 2014         
       !----------------------------------------------------------------
       use params_core

       implicit none

       !----------------------------------------------------------------
       ! VARIABLES FOR TOPMODEL IMPLEMENTATION FOLLOWING DYPTOP
       !----------------------------------------------------------------
       real, dimension(4,maxgrid)      :: topmpar              ! TOPMODEL parameters for Eq.5 in ST14
                                                               ! topmpar(1,:) is k
                                                               ! topmpar(2,:) is q 
                                                               ! topmpar(3,:) is v 
                                                               ! topmpar(4,:) is fmax
       real, dimension(maxgrid,nmonth) :: outwtpos             ! "water table position" index, Gamma in ST14, in mm above surface.
       real, dimension(maxgrid,nmonth) :: inund                ! inundated area fraction, f in ST14

       !----------------------------------------------------------------
       ! VARIABLES FOR DYNAMIC PEATLAND MODELLING FOLLOWING DYPTOP
       !----------------------------------------------------------------
       real, dimension(maxgrid)               :: peatlandfrac  ! annually variying peatland area fraction (to which lu_area(lupeat,:) converges)
       logical, dimension(maxgrid)            :: pt_criterium  ! criterium whether peatland expansion allowed, pt_crit in ST14
       real, dimension(maxgrid)               :: apet          ! annual total potential ET [mm/d]
       real, dimension(maxgrid)               :: aprec         ! annual total precipitation [mm/yr]
       real, dimension(ptbuf,maxgrid)         :: soilc_buf     ! buffer of previous ptbuf years' soil C [gC/m2]
       real, dimension(ptbuf,maxgrid)         :: scbal_buf     ! buffer of previous ptbuf years' soil C balance [gC/yr/m2]
       real, dimension(ptbuf*nmonth,maxgrid)  :: inund_buf     ! buffer of previous ptbuf*nmonth months' inundated area fraction

       !----------------------------------------------------------------
       ! LPX-SPECIFIC GLOBAL VARIABLES
       ! Sum over 'lu_area' is equal to the total land area in respective
       ! gridcell (=landfraction) and varies between 0 and 1 (1=100% land for vegetation growth).
       ! In case peatlands are not simulated, lu_area is not affected by variations in 'inund'.
       !----------------------------------------------------------------
       real, dimension(nlu,maxgrid)    :: lu_area              ! this year's gridcell area fractions
       real, dimension(nlu,maxgrid)    :: lu_area_old          ! previous year's gridcell area fractions

       real, dimension(maxgrid,nlu,nelements) :: cpool_fast    ! soil C pool, fast turnover, represents acrothelm on lupeat [gC/m2]
       real, dimension(maxgrid,nlu,nelements) :: cpool_slow    ! soil C pool, slow turnover, represents catothelm on lupeat [gC/m2]
       real, dimension(maxgrid,nlu)    :: input_slow_out       ! annual input to catotelm (soil C balance) [gC/m2/yr]
       real, dimension(maxgrid)        :: land_fraction        ! fraction of land for vegetation growth (1 = 100% land, 0 = 100% ice or water)

       end module globalvars

