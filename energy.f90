module energy
   use approx
   use diagonalization
   use global
   use neighborhood
   implicit none
   private
   
   public :: total_energy, show_energy, count_zeros

contains
   
   subroutine total_energy
      integer :: i
      
      if (todo%energies) call energies
      if (todo%penalty)  call penalty
      
      todo%energy = .false.
      
      i = s%ne / 2
      
      s%E(s%i) = 2 * sum(s%W(:i, s%i)) + s%penalty(s%i)
      
      if (s%ne .gt. 2 * i) s%E(s%i) = s%E(s%i) + s%W(i + 1, s%i)
   end subroutine total_energy
   
   subroutine show_energy
      if (todo%energy) call total_energy
      
      write (*, "('E = ', F0.3, ' eV')") s%E(s%i)
   end subroutine show_energy
   
   subroutine count_zeros(zeros)
      integer, intent(out), optional :: zeros
      
      integer :: i, n
      
      if (todo%energies) call energies
      
      n = 0
      
      do i = 1, s%nX
         n = n + 2 * modulo(s%ls(i), 2) - 1
      end do

      if (present(zeros)) then
         zeros = count(s%W(:s%dim, s%i) .ap. 0.0_dp)
      else
         write (*, '(I0, 1X, I0)') count(s%W(:s%dim, s%i) .ap. 0.0_dp), n
      end if
   end subroutine count_zeros
end module energy
