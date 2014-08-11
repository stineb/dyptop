      subroutine getdyptoppar
      !////////////////////////////////////////////////////////////////
      ! Read TOPMODEL parameters used for DYPTOP Eq. 5 in ST14 from 
      ! NetCDF input file provided. 
      ! For demonstration code, directly prescribe parameter values
      ! for a gridcell in the Hudson Bay Lowlands (89.5W/55.5N).
      ! Benjamin Stocker, May 2013 - June 2014         
      !----------------------------------------------------------------

      use params_core
      use globalvars

      implicit none

      ! Local variables
      integer :: jpngr                 ! gridcell number

      do jpngr=1,maxgrid

        ! k ('k_par' in NetCDF file)
        topmpar(1,jpngr) = 16.83

        ! q ('xm_par' in NetCDF file)
        topmpar(2,jpngr) = 0.07628

        ! v ('v_par' in NetCDF file)
        topmpar(3,jpngr) = 4.946

        ! f_max ('maxf' in NetCDF file)
        topmpar(4,jpngr) = 0.9589

      end do 

      end subroutine getdyptoppar