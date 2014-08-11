       program main
       !////////////////////////////////////////////////////////////////
       ! Main program for DYPTOP demonstration. 
       ! This is used to exemplify the use of provided DYPTOP Fortran90
       ! subroutines within a vegetation model framework. Specifically,
       ! this is to demonstrate at what points of model, the DYPTOP
       ! subroutines are to be called.
       ! Benjamin Stocker, May 2013 - June 2014         
       !----------------------------------------------------------------

       ! Load modules
       use params_core
       use globalvars

       implicit none

       integer :: year                      ! simulation year
       integer :: jpngr                     ! counter for gricell number
       integer :: testcell                  ! example gridcell number

       ! write simulation name to standard output (screen)
       write(0,*) '-------------DYPTOP demonstration--------------'


       !----------------------------------------------------------------
       ! Define variables necessary for demonstration code. These variables
       ! would be defined elsewhere in the respective vegetation model.
       ! For demonstration, use values from LPX-Bern for a gridcell in 
       ! the Hudson Bay Lowlands (HBL, 89.5W/55.5N).
       !----------------------------------------------------------------
       ! only one cell used for demonstration
       testcell = 1

       ! gridcell fraction available for growth, equals sum over lu_area(:,testcell)
       land_fraction(testcell) = 0.9950

       ! fast and slow soil carbon pool
       cpool_fast(testcell,lupeat,1) =  14978.
       cpool_slow(testcell,lupeat,1) = 200000.

       ! input to catotelm (soil C balance)
       input_slow_out(testcell,lupeat) = 16.47

       ! annual total precipitation
       aprec(testcell) = 528.8

       ! annual total potential evapotranspiration
       apet(testcell) = 394.0

       ! monthly water table position (Gamma), gridcell fraction weighted average, taken from an example year simulation of HBL cell
       outwtpos(testcell,:) = (/-2000.000000000000,-2000.000000000000,-2000.000000000000,-1929.863737003827,333.7803247923802,73.54735422593097,15.78849462854697,-57.47868084993456,-11.83258170284965,46.60021897703703,-1670.845575308536,-1999.879398178127/)


       !----------------------------------------------------------------
       ! Read DYPTOP parameters used for Eq.5 in ST14
       !----------------------------------------------------------------
       call getdyptoppar


       ! LOOP THROUGH YEARS
       do year=1,runyears

         if (year==1) then
           write(0,*) '-------------PHASE I----------------'
         endif

         if (year==soil_equil_year) then
           write(0,*) '-------------PHASE II---------------'
         endif

         if (year==soil_equil_year+conversion_wait) then
           write(0,*) '-------------PHASE III--------------'
         endif

         if (year==spinupyears) then
           write(0,*) '-------------TRANSIENT--------------'
         endif

         !----------------------------------------------------------------
         ! define gridcell area fractions (tiles)
         !----------------------------------------------------------------
         call gridcellfraction(year)


         ! LOOP THROUGH GRIDCELLS
         do jpngr=1,maxgrid

           !----------------------------------------------------------------
           ! define inundated area fraction
           !----------------------------------------------------------------
           call fflooded(year,jpngr)


         end do

         ! write to standard output
         write(0,100) 'year, f_peat',year,lu_area(lupeat,1)

100      format(A,I6,F10.5)

       enddo

       write(0,*) '---------END OF SIMULATION------------'

       end program
