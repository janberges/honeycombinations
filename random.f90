module random
   use global
   use memory
   implicit none
   private

   public :: sow, shuffle, randomize, randomize2

contains

   subroutine sow
      integer :: i, n, t
      integer, allocatable :: seed(:)

      call random_seed(size=n)
      allocate(seed(n))

      call system_clock(count=t)

      do i = 1, n
         seed(i) = t + 42 * i
      end do

      call random_seed(put=seed)
   end subroutine sow

   subroutine shuffle(list, k)
      integer, intent(inout) :: list(0:)
      integer, intent(in), optional :: k

      real(dp) :: r
      integer :: n, m, i, j

      n = size(list)

      m = n - 1
      if (present(k)) m = min(k, m)

      do i = 0, m - 1
         call random_number(r)
         j = i + floor((n - i) * r)
         list([i, j]) = list([j, i])
      end do
   end subroutine shuffle

   subroutine randomize
      integer :: i

      todo%hamiltonian = .true.
      todo%energies    = .true.
      todo%energy      = .true.
      todo%penalty     = .true.
      todo%plot        = .true.

      todo%configuration = .false.

      call alloc(s%ls, s%nC)
      call alloc(s%map, s%nC)

      do i = 1, s%nC
         s%ls(i) = i
      end do

      call shuffle(s%ls)

      do i = 1, s%nC
         s%map(s%ls(i)) = i
      end do
   end subroutine randomize

   subroutine randomize2
      integer :: i, n

      todo%hamiltonian = .true.
      todo%energies    = .true.
      todo%energy      = .true.
      todo%penalty     = .true.
      todo%plot        = .true.

      todo%configuration = .false.

      call alloc(s%ls, s%nC)
      call alloc(s%map, s%nC)

      n = s%nC / 2

      do i = 1, n
         s%ls(i) = 2 * i - 1
         s%ls(n + i) = 2 * i
      end do

      call shuffle(s%ls(:n))
      call shuffle(s%ls(n + 1:))

      do i = 1, s%nC
         s%map(s%ls(i)) = i
      end do
   end subroutine randomize2
end module random
