module global
   implicit none
   
   integer, parameter :: dp = selected_real_kind(14, 300)
   real(dp), parameter :: pi = 4.0_dp * atan(1.0_dp)
   
   type system
      integer ::     &
         &  l = 10,  &
         &  d = 4,   &
         &  i = 1,   &
         &  n = 1000  
      
      integer :: &
         &    nC = 200, &
         &    nX = 20,  &
         &    ne = 220, &
         &   dim = 220, &
         & dimax = 400
      
      integer, allocatable :: &
         &        ls(:),      &
         &       map(:),      &
         &        px(:),      &
         & neighbors(:, :),   &
         &   matches(:)
      
      real(dp) ::       &
         & eC = 0.0_dp, &
         &  t = 2.6_dp, &
         & eX = 0.0_dp, &
         &  V = 6.0_dp, &
         &  p = 0.5_dp, &
         & kT = 0.0_dp, &
         &  r = 4.0_dp
      
      real(dp) :: E(2), penalty(2)
      
      real(dp), allocatable :: &
         &       H(:, :),      &
         &      wH(:, :),      &
         &       W(:, :),      &
         &     psi(:, :),      &
         & chances(:),         &
         &   table(:, :)
      
      character ::     &
         &    C = '.', &
         &    X = 'o', &
         & jobz = 'N'   
      
      character(10) :: file = 'data.txt'
      
      character, allocatable :: plot(:)
      character(3), allocatable :: labels(:)
      
      logical ::             &
         &   time = .false., &
         &   show = .true.,  &
         &    rrr = .true.,  &
         &  color = .true.
   end type system
   
   type dependencies
      logical ::                   &
         &   hamiltonian = .true., &
         &      energies = .true., &
         &        energy = .true., &
         &       penalty = .true., &
         &     neighbors = .true., &
         &       matches = .true., &
         &  correlations = .true., &
         & configuration = .true., &
         &         space = .true., &
         &          plot = .true., &
         &         table = .true.
   end type dependencies
   
   type(system), save :: s
   type(dependencies), save :: todo

end module global
