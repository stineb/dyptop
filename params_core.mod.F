       module params_core
       !////////////////////////////////////////////////////////////////
       !  This module contains model parameters that are not modified, 
       !  necessary for grid definition, etc. (fixed, not subject to 
       !  uncertainty)
       !  Benjamin Stocker, May 2013 - June 2014         
       !----------------------------------------------------------------
       implicit none

       integer, parameter :: maxgrid = 1              ! number of land gridcells (for demonstration use 1)
       integer, parameter :: nmonth = 12              ! number of months per year
       integer, parameter :: ptbuf  = 31              ! buffer for peatland variables (period over which averaged), [yr]
       integer, parameter :: nlu    = 3               ! number of gridcell fractions (tiles). For ST14, we used (see Section 2 in ST14):
                                                      ! 1 = natural land on mineral soils (=lunat)
                                                      ! 2 = peatlands (=lupeat)
                                                      ! 3 = former peatlands, now treated as mineral soils (=luoldpeat)
       integer, parameter :: lunat = 1                ! index identifier for natural land on mineral soils                                               
       integer, parameter :: lupeat = 2               ! index identifier for peatland gridcell land unit (tile)         
       integer, parameter :: lupeatold = 3            ! index identifier for former peatland gridcell land unit (tile)         
       integer, parameter :: nelements = 2            ! number of elements in organic pools. For ST14, we used:
                                                      ! 1 = carbon
                                                      ! 2 = nitrogen                                   
       integer, parameter, dimension(nmonth) :: ndaymonth = (/31,28,31,30,31,30,31,31,30,31,30,31/) ! days per month

       integer, parameter :: runyears = 1600          ! number of simulation years including model spinup
       integer, parameter :: soil_equil_year = 1000   ! simulation year of soil pool equilibration (see Section 5 in ST14)
       integer, parameter :: conversion_wait = 200    ! length of second spinup phase (see Section 5 in ST14)
       logical, parameter :: spinup = .true.          ! for demonstration, always do spinup
       integer, parameter :: spinupyears = 1500       ! number of model spin-up years 
                                                      ! phase I: 1000 yr (soil_equil_year) 
                                                      ! phase II: 200 yr (conversion_wait) 
                                                      ! phase III: 300 yr (spinupyears-soil_equil_year-conversion_wait)


       end module params_core

