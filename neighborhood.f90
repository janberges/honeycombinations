module neighborhood
   use global
   use memory
   use random
   use transposition
   implicit none
   private

   public :: neighbors, new_order, table, correlations, show_correlations, &
      penalty, get_kind

   integer :: kinds

   integer, allocatable :: offset(:), steps(:), lower(:), upper(:), howmany(:)

contains

   subroutine neighbors
      integer :: i, j, k, l, m, step, count, ll, sgn, site, side
      logical :: seen(s%nC)

      if (todo%configuration) call randomize

      todo%neighbors = .false.
      todo%table     = .true.

      ! There are 3n nth nearest neighbors. Following Gauß:

      s%d = max(min(s%d, s%l - 1), 1)

      call alloc(offset, s%d + 1)

      do i = 1, s%d + 1
         offset(i) = 3 * (i - 1) * i / 2
      end do

      kinds = (s%d * (s%d + 1) - s%d / 2 * (s%d / 2 + 1)) / 2

      call alloc(lower,     kinds)
      call alloc(upper,     kinds)
      call alloc(steps,     kinds)
      call alloc(howmany,   kinds)
      call alloc(s%matches, kinds)
      call alloc(s%chances, kinds)
      call alloc(s%labels,  kinds)

      i = 1
      do j = 1, s%d
         if (modulo(j, 2) .eq. 0) then
            step = j / 2
            count = 6
         else
            step = j
            count = 3
         end if

         do k = 1, step
            lower(i) = offset(j) + k
            upper(i) = offset(j + 1)
            steps(i) = step
            howmany(i) = count

            write(s%labels(i), '(I2, A)') j, achar(96 + k)

            i = i + 1
         end do
      end do

      call alloc(s%neighbors, offset(s%d + 1), s%nC)

      ll = 2 * s%l

      sgn = -1
      do i = 0, s%nC - 1
         sgn = -sgn

         s%neighbors(:3, i + 1) = 1 + [       &
            i + sgn,                          &
            modulo(i + sgn - sgn * ll, s%nC), &
            modulo(i - sgn, ll) + i / ll * ll ]
      end do

      seen(:) = .false.

      do i = 1, s%nC
         m = 3

         seen(i) = .true.
         seen(s%neighbors(:m, i)) = .true.

         do j = 1, s%d - 1
            do k = offset(j) + 1, offset(j + 1)
               site = s%neighbors(k, i)

               sgn = merge(-1, 1, seen(s%neighbors(2, site)))

               do l = 2 - sgn, 2 + sgn, sgn
                  side = s%neighbors(l, site)

                  if (seen(side)) cycle

                  m = m + 1
                  s%neighbors(m, i) = side
                  seen(side) = .true.
               end do
            end do
         end do

         seen(i) = .false.
         seen(s%neighbors(:, i)) = .false.
      end do
   end subroutine neighbors

   subroutine new_order(kind)
      integer, intent(in) :: kind

      integer :: i, j, k, n

      if (todo%neighbors) call neighbors

      todo%energies     = .true.
      todo%energy       = .true.
      todo%penalty      = .true.
      todo%correlations = .true.

      write (*, "('New order: ', A)") trim(adjustl(s%labels(kind)))

      call swap(s%l * (1 + s%l / 4 * 2), s%ls(1))

      i = 1
      do j = 1, s%nC
         do k = lower(kind), upper(kind), steps(kind)
            n = s%neighbors(k, s%ls(j))

            if (s%map(n) .gt. i) then
               i = i + 1
               call swap(n, s%ls(i))
               if (i .ge. s%nX) return
            end if
         end do
      end do
   end subroutine new_order

   subroutine table
      if (todo%neighbors) call neighbors

      todo%table = .false.

      call alloc(s%table, s%n, kinds)
   end subroutine table

   subroutine correlations
      integer :: i, j, k

      if (todo%neighbors) call neighbors

      todo%correlations = .false.

      s%matches(:) = 0

      do i = 1, s%nX
         do j = 1, kinds
            do k = lower(j), upper(j), steps(j)
               if (s%map(s%neighbors(k, s%ls(i))) .le. s%nX) then
                  s%matches(j) = s%matches(j) + 1
               end if
            end do
         end do
      end do

      s%chances(:) = real(s%matches(:), dp) / (howmany * s%nX)

      s%matches(:) = s%matches / 2
   end subroutine correlations

   subroutine show_correlations
      integer :: i

      if (todo%correlations) call correlations

      do i = 1, kinds
         write (*, '(A, 1X, I3, 2A)')                    &
            s%labels(i), nint(100 * s%chances(i)), '% ', &
            repeat('=',  nint( 80 * s%chances(i)))
      end do
   end subroutine show_correlations

   subroutine penalty
      integer :: i, n

      if (todo%neighbors) call neighbors

      todo%penalty = .false.
      todo%energy = .true.

      n = 0
      do i = 1, s%nX
         n = n + count(s%map(s%neighbors(:3, s%ls(i))) .le. s%nX)
      end do

      s%penalty(s%i) = n / 2 * s%p
   end subroutine penalty

   subroutine get_kind(str, kind)
      character(*), intent(in) :: str
      integer, intent(out) :: kind

      character(:), allocatable :: label

      integer :: i

      if (todo%neighbors) call neighbors

      kind = 1

      label = str

      select case(label)
         case ('c2x2', 'C2X2')
            label = '1a'

         case ('c2x', 'C2X')
            label = '2a'

         case ('c8x2', 'C8X2')
            label = '3a'

         case ('c14x2', 'C14X2')
            label = '3b'

         case ('c6x', 'C6X')
            label = '4a'

         case ('c8x', 'C8X')
            label = '4b'
      end select

      do i = 1, kinds
         if (label .eq. adjustl(s%labels(i))) kind = i
      end do
   end subroutine get_kind
end module neighborhood
