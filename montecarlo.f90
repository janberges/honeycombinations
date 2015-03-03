module montecarlo
   use approx
   use conversion
   use energy
   use global
   use plot
   use transposition
   implicit none
   private
   
   public :: vary, markov
contains
   
   subroutine vary
      integer, save :: i = 0
      
      real(dp) :: r, a, x, y
      
      integer :: old, new
      
      if (todo%energy) call total_energy
      
      if (s%nX .lt. 2) return
      
      i = modulo(i, s%nX) + 1
      
      call random_number(r)
      call random_number(a)
      
      r = 0.5_dp + (s%r - 0.5_dp) * r
      a = 2.0_dp * pi * a
      
      old = s%ls(i)
      
      call n2xy(old, x, y)
      
      x = x + r * cos(a)
      y = y + r * sin(a)
      
      call xy2n(x, y, new)
      
      if (s%map(new) .le. s%nX) return
      
      call swap(old, new)
      
      todo%energies = .true.
      todo%penalty = .true.
      
      call total_energy
      
      if (s%E(s%i) .lt. s%E(3 - s%i)) return
      
      if (s%kT .na. 0.0_dp) then
         call random_number(r)
         
         if (r .lt. exp((s%E(3 - s%i) - s%E(s%i)) / s%kT)) return
      end if
      
      call swap(old, new)
   end subroutine vary
   
   subroutine markov
      integer :: i
      
      do i = 1, s%n
         call vary
         call clear
         call show_lattice
      end do
   end subroutine markov
end module montecarlo
