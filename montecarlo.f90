module montecarlo
   use approx
   use conversion
   use energy
   use global
   use neighborhood
   use plot
   use transposition
   implicit none
   private

   interface vary
      module procedure vary_blind, vary_sighted
   end interface vary

   public :: vary, markov
contains

   subroutine vary_blind(change)
      logical, intent(out) :: change

      integer :: from, to
      logical :: tmp

      if (todo%energy) call total_energy

      change = .false.

      if (s%nX .lt. 2 .or. s%nC - s%nX .lt. 2) return

      call jump(from, to)

      if (s%map(to) .le. s%nX) return

      change = .true.

      call swap(from, to)

      tmp = todo%correlations
      todo%energies     = .true.
      todo%energy       = .true.
      todo%penalty      = .true.
      todo%correlations = .true.

      if (.not. accepted()) then
         call swap(from, to)

         change = .false.

         todo%correlations = tmp
      end if
   end subroutine vary_blind

   subroutine vary_sighted
      integer :: from, to
      logical :: tmp

      if (todo%energy) call total_energy

      if (s%nX .lt. 2 .or. s%nC - s%nX .lt. 2) return

      call transpose(from, to)

      call swap(from, to)

      tmp = todo%correlations
      todo%energies     = .true.
      todo%energy       = .true.
      todo%penalty      = .true.
      todo%correlations = .true.

      if (.not. accepted()) then
         call swap(from, to)

         todo%correlations = tmp
      end if
   end subroutine vary_sighted

   function accepted()
      logical :: accepted

      real(dp) :: r

      accepted = .true.

      if (todo%energy) call total_energy

      if (s%E(s%i) .lt. s%E(3 - s%i)) return

      if (s%kT .na. 0.0_dp) then
         call random_number(r)

         if (r .lt. exp((s%E(3 - s%i) - s%E(s%i)) / s%kT)) return
      end if

      accepted = .false.
   end function accepted

   subroutine jump(from, to)
      integer, intent(out) :: from, to

      integer, save :: i = 0
      real(dp) :: r, a, x, y

      i = modulo(i, s%nX) + 1

      from = s%ls(i)

      call n2xy(from, x, y)

      call random_number(r)
      call random_number(a)

      r = 0.5_dp + (s%r - 0.5_dp) * r
      a = 2.0_dp * pi * a

      x = x + r * cos(a)
      y = y + r * sin(a)

      call xy2n(x, y, to)
   end subroutine jump

   subroutine transpose(from, to)
      integer, intent(out) :: from, to

      integer, save :: i = 0
      real(dp) :: r

      i = modulo(i, s%nX) + 1

      from = s%ls(i)

      call random_number(r)

      to = s%nX + int((s%nC - s%nX) * r) + 1
   end subroutine transpose

   subroutine markov(change)
      logical, intent(out), optional :: change

      integer :: i
      real(dp) :: average, error

      if (todo%table) call table

      do i = 1, s%n
         if (present(change)) then
            call vary(change)
         else
            call vary
         end if

         if (todo%correlations) call correlations

         s%table(i, :) = s%chances

         if (.not. s%show) cycle

         if (present(change)) then
            if (.not. change) cycle
         end if

         call clear

         call show_lattice

         write (*, "(/, 'Neighborship probabilities &
            &(no correlation at ', I0, '%):')") nint(s%nX * 100.0_dp / s%nC)
         call show_correlations
      end do

      write (*, "(/, 'Averages and average absoulte deviations:')")

      do i = 1, size(s%table, 2)
         average = sum(s%table(:, i)) / s%n
         error = sum(abs(s%table(:, i) - average)) / s%n

         write (*, "('P(', A, ') = ', F5.3, ' +/- ', F5.3)") &
            trim(adjustl(s%labels(i))), average, error
      end do
   end subroutine markov
end module montecarlo
