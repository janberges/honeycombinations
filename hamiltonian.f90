module hamiltonian
   use global
   use memory
   use neighborhood
   use random
   implicit none
   private

   public :: new_hamiltonian, show_hamiltonian

contains

   subroutine new_hamiltonian
      integer :: i, j

      if (todo%configuration) call randomize
      if (todo%neighbors)     call neighbors

      todo%hamiltonian = .false.

      call alloc(s%H, s%dimax, s%dimax)
      call alloc(s%wH, s%dim, s%dim)

      do i = 1, s%nC
         s%H(:i - 1, i) = 0.0_dp
         s%H(i, i) = s%eC
      end do

      do i = s%nC + 1, s%dimax
         s%H(:i - 1, i) = 0.0_dp
         s%H(i, i) = s%eX
      end do

      do i = 1, s%nC
         do j = 1, 3
            if (i .lt. s%neighbors(j, i)) then
               s%H(i, s%neighbors(j, i)) = -s%t
            end if
         end do
      end do

      do i = 1, s%nC
         s%H(s%ls(i), s%nC + i) = s%V
      end do
   end subroutine new_hamiltonian

   subroutine show_hamiltonian
      integer :: i, j

      if (todo%hamiltonian) call new_hamiltonian

      do i = 1, s%dim
         do j = 1, i - 1
            write (*, "(3X, '.', 1X)", advance='no')
         end do
         do j = i, s%dim
            write (*, '(F5.1)', advance='no') s%H(i, j)
         end do
         write (*, '()')
      end do
   end subroutine show_hamiltonian
end module hamiltonian
