module move_one
   use diagonalization
   use neighborhood
   use global
   use transposition
   use plot
   implicit none
   private

   public :: stability

contains

   subroutine stability
      integer :: i
      character(20) :: file
      integer, parameter :: unit = 14

      if (todo%energies) call energies

      call correlations

      open(unit, file='move1/0.txt', action='write', status='replace')
      write (unit, '(I0)') s%matches(1)
      write (unit, '(ES22.14E3)') s%W(:s%dim, s%i)
      close(unit)

      do i = s%nX + 1, s%nC
         call swap(s%ls(1), s%ls(i))

         todo%plot = .true.
         todo%energies = .true.
         todo%penalty = .true.

         call clear
         call show_lattice
         call energies
         call correlations

         write (file, "('move1/', I0, '.txt')") i - s%nX

         open(unit, file=file, action='write', status='replace')
         write (unit, '(I0)') s%matches(1)
         write (unit, '(ES22.14E3)') s%W(:s%dim, s%i)
         close(unit)
      end do
   end subroutine stability
end module move_one
