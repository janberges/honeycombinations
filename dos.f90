module dos
   use diagonalization
   use global
   implicit none
   private

   public :: histogram

contains

   subroutine histogram
      integer, parameter :: width = 80, height = 20, ticks = 7

      character(*), parameter ::   &
         & D = char(27) // '[39m', &
         & Y = char(27) // '[33m', &
         & B = char(27) // '[34m'

      integer :: i, j, lower, upper, bins, exponent, decimals, gap, EF, E(s%dim)
      real(dp) :: minimum, maximum, interval, step

      integer, allocatable :: H(:)
      character, allocatable :: plot(:, :)
      character(15) :: fixed

      if (todo%energies) call energies

      minimum = s%W(    1, s%i)
      maximum = s%W(s%dim, s%i)

      interval = (maximum - minimum) / (ticks - 1)
      exponent = floor(log10(abs(interval)))
      decimals = max(1, -exponent)

      step = 10.0_dp ** exponent
      step = anint(interval / step) * step

      lower =   floor(minimum / step)
      upper = ceiling(maximum / step)

      minimum = lower * step
      maximum = upper * step

      gap = (width - 1) / (upper - lower)
      bins = (upper - lower) * gap + 1

      allocate(H(bins))
      allocate(plot(bins, height))

      H(:) = 0
      plot(:, :) = ' '

      E = nint((s%W(:s%dim, s%i) - minimum) &
         * (bins - 1) / (maximum - minimum)) + 1

      do i = 1, s%dim
         H(E(i)) = H(E(i)) + 1
      end do

      H(:) = nint(2.0_dp * height / maxval(H) * H)

      EF = E((s%ne + 1) / 2)

      do i = 1, EF
         call fill(i, ';', ',')
      end do

      do i = EF + 1, bins
         call fill(i, ':', '.')
      end do

      do i = height, 1, -1
         write (*, '(A)', advance='no') B

         do j = 1, EF
            write (*, '(A)', advance='no') plot(j, i)
         end do

         write (*, '(A)', advance='no') Y

         do j = EF + 1, bins
            write (*, '(A)', advance='no') plot(j, i)
         end do

         write (*, '()')
      end do

      write (*, '(A)', advance='no') D

      write (*, '(A)') &
         repeat('+' // repeat('-', gap - 1), upper - lower) // '>'

      write (*, "('E', A)", advance='no') repeat(' ', decimals)

      write (fixed, "('(', I0, 'F', I0, '.', I0, ')')") &
         upper - lower - 1, gap, decimals

      write (*, fixed) (i * step, i = lower + 1, upper - 1)

   contains

      subroutine fill(i, full, half)
         integer, intent(in) :: i
         character, intent(in) :: full, half

         integer :: j

         j = H(i) / 2
         plot(i, :j) = full
         if (H(i) .gt. 2 * j) plot(i, j + 1) = half
      end subroutine fill
   end subroutine histogram

end module dos
