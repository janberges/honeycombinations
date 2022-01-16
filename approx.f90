module approx
   use global
   implicit none
   private

   public :: operator(.ap.), operator(.na.)

   real(dp), parameter :: tolerance = 1e-10_dp

   interface operator(.ap.)
      module procedure ap
   end interface operator(.ap.)

   interface operator(.na.)
      module procedure na
   end interface operator(.na.)

contains

   elemental logical function ap(a, b)
      real(dp), intent(in) :: a, b

      ap = abs(b - a) .le. tolerance
   end function ap

   elemental logical function na(a, b)
      real(dp), intent(in) :: a, b

      na = abs(b - a) .gt. tolerance
   end function na
end module approx
