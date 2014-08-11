      subroutine fflooded(year,jpngr)
      !///////////////////////////////////////////////////////////////////
      ! DYPTOP - FFLOODED SUBROUTINE
      !------------------------------------------------------------------
      ! This is coded as a Fortran 90 subroutine to be called once a year
      ! (at the end of the year), after the monthly "water table position"
      ! index is calculated, and inside the loop over all land gridcells.
      !
      ! This SR calculates
      ! 1. Inundated area fraction 'inund'
      ! 2. Peatland area fraction 'peatlandfrac'
      !
      ! Note that 'peatlandfrac' is NOT the actual peatland area fraction. 
      ! The latter converges to 'peatlandfrac' with a prescribed inertia
      ! (calculated in gridcellfraction.F). Actual peatland area fraction 
      ! is represented in LPX by the variable lu_area(lupeat,jpngr). Ana-
      ! logously, area fraction of non-peat mineral soils is 
      ! lu_area(lunat,jpngr) and area fraction of former peatlands is 
      ! lu_area(lupeatold,jpngr).
      !
      ! This SR has been implemented in LPX-Bern, that provides a number of
      ! global variables, accessed by 'fflooded' via loading respective modules
      ! 
      ! All equations are documented in Stocker et al. (2014), GMDD XXX
      ! referred to in this file as 'ST14'.
      !
      ! Benjamin Stocker, May 2013 - June 2014         
      !------------------------------------------------------------------

      ! Load modules
      use params_core
      use params_dyptop
      use globalvars

      implicit none
      
      ! Arguments
      integer,intent(in) :: year              ! simulation year
      integer,intent(in) :: jpngr             ! grid cell index (combines lon and lat indeces)

      ! Local Variables
      integer :: m,i                          ! counters
      integer :: itmp,iswap1                  ! temporary vars for vector sorting
      integer, dimension(nmonth*ptbuf) :: iy  ! temporary vars for vector sorting
      integer, dimension(1) :: iswap          ! temporary vars for vector sorting
      real, dimension(ptbuf*nmonth) :: sorted ! temporary vars for vector sorting
      real :: tmp                             ! temporary vars for vector sorting
      real :: applf                           ! annually updated potential peatland fraction
      real :: PoAET                           ! ann. precip over ann. equilibrium ET [mm/mm]

      intrinsic maxloc

      !//////////////////////////////////////////////////////////////////
      ! INUNDATED AREA FRACTION
      ! Calculate wetland fraction as a function of the monthly water
      ! table position ('outwtpos'). 'outwtpos' is in mm above surface.
      ! Use asymetrical sigmoid function to relate inund to water table 
      ! position (Eq.5 in ST14).  This function is a best fit to an 
      ! "empirical" (based on sub-grid scale topography, TOPMODEL approach) 
      ! relation. The fit was done offline.
      !------------------------------------------------------------------

      !------------------------------------------------------------------
      ! DIAGNOSTIC INUNDATION FUNCTION (Psi in ST14)
      ! When first soil layer is frozen, wtpos is set to -1900. To be 
      ! interpreted so that inundated area is zero (almost no liquid water present).
      ! One should theoretically use -2000, but this can cause numerical
      ! problems in the equation below.
      !------------------------------------------------------------------
      do m=1,nmonth
        if (outwtpos(jpngr,m)<-1900.) then
          inund(jpngr,m) = 0.0d0
        else
          ! Protect from numerical problems (expression can become extremely large)
          if (exp((-1.)*topmpar(1,jpngr)*(outwtpos(jpngr,m)
     $         /1.d3-topmpar(2,jpngr)))>1.d100) then
            inund(jpngr,m) = 0.0d0
          else
            inund(jpngr,m) = min(
     $           topmpar(4,jpngr),
     $           (1.
     $           / (
     $           1.+topmpar(3,jpngr)*exp((-1.)
     $           *topmpar(1,jpngr)
     $           *(outwtpos(jpngr,m)/1.d3-topmpar(2,jpngr))
     $           )
     $           ))
     $           **(1./topmpar(3,jpngr))
     $           )
          endif
        endif
      enddo

      !//////////////////////////////////////////////////////////////////
      ! PEATLAND AREA FRACTION
      ! This calculates the annually updated 'peatlandfrac'. Note that
      ! this is NOT the actual peatland area fraction (f_peat in ST14). 
      ! In LPX-Bern, each gridcell is 
      !------------------------------------------------------------------

      !------------------------------------------------------------------
      ! UPDATE BUFFERS
      ! Shift stored yearly values up, adding new values for this year
      !------------------------------------------------------------------
      if (year>ptbuf) then
        
      ! Soil C buffer
        do i=2,ptbuf
          soilc_buf(i-1,jpngr) = soilc_buf(i,jpngr)
        enddo
        soilc_buf(ptbuf,jpngr) = cpool_fast(jpngr,lupeat,1)
     $       + cpool_slow(jpngr,lupeat,1)
        
      ! Soil C balance buffer
        do i=2,ptbuf
          scbal_buf(i-1,jpngr) = scbal_buf(i,jpngr)
        enddo
        scbal_buf(ptbuf,jpngr) =  input_slow_out(jpngr,lupeat)
        
      ! Inundated area buffer
        do i=13,ptbuf*nmonth
          inund_buf(i-12,jpngr) = inund_buf(i,jpngr)
        enddo
        inund_buf(((ptbuf-1)*nmonth+1):(ptbuf*nmonth),jpngr) = inund(jpngr,:)
        
      else
        
        inund_buf((year-1)*nmonth+1:year*nmonth,jpngr) = inund(jpngr,:)
        soilc_buf(year,jpngr) =
     $       cpool_fast(jpngr,lupeat,1) + cpool_slow(jpngr,lupeat,1)
        scbal_buf(year,jpngr) = input_slow_out(jpngr,lupeat)

      endif
      
      !------------------------------------------------------------------
      ! SORT INUNDATION VALUES
      ! Sort previous' ptbuf monthly values in inund_buf in descending 
      ! order (Eq.12 in ST14). Adopted from 
      ! http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/sorc/nam_gridgen_sfc.fd/sort.f
      !------------------------------------------------------------------
      sorted = inund_buf(:,jpngr)
      iy(:) = 0
      do i=1,ptbuf*nmonth-1
        iswap = maxloc(sorted(i:ptbuf*nmonth))
        iswap1 = iswap(1)+i-1
        if (iswap1.ne.i) then
          tmp = sorted(i)
          sorted(i)=sorted(iswap1)
          sorted(iswap1)=tmp
          itmp=iy(i)
          iy(i)=iy(iswap1)
          iy(iswap1)=itmp
        endif
      enddo
      
      !------------------------------------------------------------------
      ! POTENTIAL PEATLAND AREA FRACTION
      ! To get annually updated potential peatland fraction take the minimum
      ! fraction flooded in min_peat_inundmonths of all months over the
      ! previous 31 years (ptbuf) (Eq.13 in ST14).
      !------------------------------------------------------------------
      applf = sorted(min_peat_inundmonths)
      
      !------------------------------------------------------------------
      ! EVALUATE PEATLAND CRITERIUM ...
      !------------------------------------------------------------------
      ! Initialize pt_criterium:
      ! spinup/no restart: all are false (first part of spinup period with
      !                    minimum peatland area)
      ! when restart (no spinup): read information from restart file
      !------------------------------------------------------------------
      if (year==1) then
        pt_criterium(jpngr) = .false.
      endif

      !------------------------------------------------------------------
      ! 1. POAET CRITERIUM
      !------------------------------------------------------------------      
      ! Define PoAET as annual total precip. over annual total potential ET
      if (apet(jpngr)<=0.0d0) then
        PoAET=0.0d0
      else
        PoAET=aprec(jpngr)/apet(jpngr)
      endif

      if (PoAET<min_poaet) then
        ! Set peat criterium (Fig.3 in ST14)
        pt_criterium(jpngr)=.false.
      
      else
        !------------------------------------------------------------------
        ! 2. C MASS BALANCE CRITERIA
        ! If POAET criterium is satisfied, evaluate mass balance criteria.
        !------------------------------------------------------------------
        if (sum(scbal_buf(:,jpngr))/ptbuf>=min_peat_balance
     $       .or.cpool_fast(jpngr,lupeat,1)+cpool_slow(jpngr,lupeat,1)
     $       >=min_peat_amount) then
          pt_criterium(jpngr)=.true.
        else
          pt_criterium(jpngr)=.false.
        endif
      endif
      
      !------------------------------------------------------------------
      ! Set peatlandfrac for this cell to annual potential peatland 
      ! fraction based on inundated areas if C-mass-related criteria are 
      ! fulfilled. Note that actual peatland area fraction 
      ! (lu_area(lupeat,jpngr)) is set in gridcellfraction.F
      !------------------------------------------------------------------
      if (pt_criterium(jpngr)) then
      !  sum(lu_area(:,jpngr)) is the total land fraction for veg. growth
        peatlandfrac(jpngr) = max( sum(lu_area(:,jpngr))*applf, min_peat_fraction )
      else
        peatlandfrac(jpngr) = min_peat_fraction
      endif
      
      
      end subroutine fflooded
