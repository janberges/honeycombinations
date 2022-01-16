module timer
   use global
   implicit none
   private

   public :: time

contains

   subroutine time(it)
      character(*), intent(in), optional :: it

      real(dp), save :: t0 = 0.0_dp
      real(dp) :: t

      call cpu_time(t)

      print *, t

      if (present(it)) then
         write (*, "(A, ' took ', ES8.3E1, ' seconds')") it, t - t0
      end if

      t0 = t
   end subroutine time
end module timer
