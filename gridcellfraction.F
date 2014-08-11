      subroutine gridcellfraction(year)
      !///////////////////////////////////////////////////////////////////
      ! DYPTOP - GRIDCELLFRACTION SUBROUTINE
      !------------------------------------------------------------------
      ! Subroutine to be called once a year (at the beginning of the year), 
      ! and before the loop over all land gridcells.
      !
      ! This SR  updates the gridcell land unit area fraction array 
      ! 'lu_area' (actual peatland area fraction = lu_area(lupeat,jpngr))
      ! in response to 'peatlandfrac', calculated at the ond of the 
      ! previous year. Changes in between lu_area_old (previous year's
      ! gridcell land unit area fraction array) and lu_area trigger mass
      ! to be re-allocated between gridcell fractions. This is handles by 
      ! a separate SR that is NOT provided with DYPTOP.
      
      ! All equations are documented in Stocker et al. (2014), GMDD XXX
      ! Benjamin Stocker, May 2013 - June 2014         
      !------------------------------------------------------------------

      ! Load modules
      use globalvars
      use params_dyptop

      implicit none

      ! Arguments
      integer,intent(in) :: year              ! simulation year

      ! Local variables
      real               :: dlu               ! Change in peatland area
      integer            :: jpngr             ! grid cell number

      ! store previous year's gridcell land unit area fractions
      lu_area_old(:,jpngr) = lu_area(:,jpngr)

      ! initialise
      lu_area(:,:)=0.
      lu_area(lunat,:)=land_fraction(:) !nature

      ! LOOP OVER ALL LAND GRIDCELLS
      do jpngr=1,maxgrid
        
        ! ----------------------------------------------------------------------------
        ! DEFINE f_peat
        ! ----------------------------------------------------------------------------
        if (spinup.and.year<soil_equil_year+conversion_wait) then
          ! ----------------------------------------------------------------------------
          ! Phase I and II of the spinup
          ! Hold peatland area at minimum
          ! ----------------------------------------------------------------------------
          lu_area(lupeat,jpngr)=min_peat_fraction
        else
          if (spinup.and.year==soil_equil_year+conversion_wait) then
            ! ----------------------------------------------------------------------------
            ! End of Phase II of the spinup
            ! Set peatland area immediately to simulated inundation fraction
            ! ----------------------------------------------------------------------------
            lu_area(lupeat,jpngr)=peatlandfrac(jpngr)
            lu_area_old(lupeat,jpngr)=peatlandfrac(jpngr)

          endif
          ! ----------------------------------------------------------------------------
          ! Phase III of the spinup and transient simulation part
          ! Transient peatland area change. peatlandfrac is set dynamically (see fflooded.F) 
          ! based on inundated areas over the previous 31 yr, and is updated annually. 
          ! 'lu_area(lupeat,jpngr)' follows 'peatlandfrac(jpngr)' with prescribed inertia 
          ! (max_peat_areachangerate). 'inund' is independent of peatland area.
          ! ----------------------------------------------------------------------------          
          if (lu_area_old(lupeat,jpngr)>peatlandfrac(jpngr)) then

            ! shrinking peatland
            lu_area(lupeat,jpngr) = max(lu_area_old(lupeat,jpngr)
     $           *(1.-max_peat_areachangerate)
     $           ,peatlandfrac(jpngr))

          else if (lu_area_old(lupeat,jpngr)<peatlandfrac(jpngr)) then

            ! expanding peatland
            lu_area(lupeat,jpngr) = min(lu_area_old(lupeat,jpngr)
     $           *(1.+max_peat_areachangerate)
     $           ,peatlandfrac(jpngr))

          else

            ! constant peatland
            lu_area(lupeat,jpngr) = peatlandfrac(jpngr)

          endif
        endif

        ! set absoulte grid cell boundaries
        lu_area(lupeat,jpngr)=min(lu_area(lupeat,jpngr),land_fraction(jpngr))
        lu_area(lupeat,jpngr)=max(lu_area(lupeat,jpngr),min_peat_fraction)
        
        ! ----------------------------------------------------------------------------
        ! DEFINE f_oldpeat
        ! ----------------------------------------------------------------------------
        ! Change in peatland area 
        dlu = lu_area(lupeat,jpngr)-lu_area_old(lupeat,jpngr)

        if (dlu>0.) then
          ! Expanding peatland -> reclaim former peatland
          lu_area(lupeatold,jpngr)=
     $         max(lu_area_old(lupeatold,jpngr)-dlu,0.)

        else if (dlu<0.) then
          ! Contracting peatland -> reallocate to peatold
          lu_area(lupeatold,jpngr)=
     $         min(lu_area_old(lupeatold,jpngr)-dlu,
     $         lu_area(lunat,jpngr)-lu_area(lupeat,jpngr))

        else
          ! no change
          lu_area(lupeatold,jpngr)=lu_area_old(lupeatold,jpngr)

        endif

        ! ----------------------------------------------------------------------------
        ! DEFINE f_mineral
        ! ----------------------------------------------------------------------------
        lu_area(lunat,jpngr)=max(lu_area(lunat,jpngr)
     $       -lu_area(lupeat,jpngr)-lu_area(lupeatold,jpngr),0.)

      enddo
  

      end subroutine gridcellfraction
