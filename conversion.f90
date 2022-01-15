module conversion
   use global
   implicit none
   private

   public :: n2ijk, ijk2n, xy2ijk, ijk2xy, ijk2c, n2xy, xy2n, n2c
contains

   elemental subroutine n2ijk(n, i, j, k)
      integer, intent(in) :: n
      integer, intent(out) :: i, j, k

      integer :: m
      m = n - 1

      k = modulo(m, 2)
      j = modulo(m / 2, s%l)
      i = m / (2 * s%l)
   end subroutine n2ijk

   elemental subroutine ijk2n(i, j, k, n)
      integer, intent(in) :: i, j, k
      integer, intent(out) :: n

      n = 2 * (i * s%l + j) + k + 1
   end subroutine ijk2n

   elemental subroutine xy2ijk(x, y, i, j, k)
      real(dp), intent(in) :: x, y
      integer, intent(out) :: i, j, k

      real(dp) :: a, b

      a = x / sqrt(3.0_dp) - y / 3.0_dp
      b = y / 1.5_dp

      i = modulo(floor(a), s%l)
      j = modulo(floor(b), s%l)
      k = int(modulo(a, 1.0_dp) + modulo(b, 1.0_dp))
   end subroutine xy2ijk

   elemental subroutine ijk2xy(i, j, k, x, y)
      integer, intent(in) :: i, j, k
      real(dp), intent(out) :: x, y

      x = 0.5_dp * sqrt(3.0_dp) * (modulo(2 * i + j + k, 2 * s%l) + 1)
      y = 1.5_dp * j + 0.5_dp * k + 0.5_dp
   end subroutine ijk2xy

   elemental subroutine ijk2c(i, j, k, c)
      integer, intent(inout) :: i, j, k
      integer, intent(out) :: c

      integer :: x, y

      x = modulo(2 * i + j + k, 2 * s%l)
      y = 3 * j + k

      c = 3 * s%l * x + y + 1
   end subroutine ijk2c

   elemental subroutine n2xy(n, x, y)
      integer, intent(in) :: n
      real(dp), intent(out) :: x, y

      integer :: i, j, k

      call n2ijk(n, i, j, k)
      call ijk2xy(i, j, k, x, y)
   end subroutine n2xy

   elemental subroutine xy2n(x, y, n)
      real(dp), intent(in) :: x, y
      integer, intent(out) :: n

      integer :: i, j, k

      call xy2ijk(x, y, i, j, k)
      call ijk2n(i, j, k, n)
   end subroutine xy2n

   elemental subroutine n2c(n, c)
      integer, intent(in) :: n
      integer, intent(out) :: c

      integer :: i, j, k

      call n2ijk(n, i, j, k)
      call ijk2c(i, j, k, c)
   end subroutine n2c
end module conversion
