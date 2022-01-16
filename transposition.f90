module transposition
   use global
   implicit none
   private

   public :: swap

contains

   subroutine swap(old, new)
      integer, intent(in) :: old, new

      integer :: n(2)

      s%map([old, new]) = s%map([new, old])
      s%ls(s%map([old, new])) = [old, new]

      if (.not. todo%plot) then
         s%plot(s%px([old, new])) = s%plot(s%px([new, old]))
      end if

      s%i = 3 - s%i

      if (.not. todo%hamiltonian) then
         n = s%nC + s%map([old, new])

         s%H([old, new], n) = s%H([new, old], n)
      end if
   end subroutine swap
end module transposition
