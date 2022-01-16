module memory
   use global
   implicit none
   private

   public :: alloc

   interface alloc
      module procedure                            &
         &   logical1D,   logical2D,   logical3D, &
         &   integer1D,   integer2D,   integer3D, &
         &      real1D,      real2D,      real3D, &
         & character1D, character2D, character3D
   end interface alloc

contains

   subroutine logical1D(array, height)
      logical, allocatable, intent(inout) :: array(:)
      integer, intent(in) :: height

      integer :: status

      if (allocated(array)) then
         if (size(array) .eq. height) return

         deallocate(array)
      end if

      allocate(array(height), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' logicals')") height

      stop
   end subroutine logical1D

   subroutine logical2D(array, height, width)
      logical, allocatable, intent(inout) :: array(:, :)
      integer, intent(in) :: height, width

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width) return

         deallocate(array)
      end if

      allocate(array(height, width), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' logicals')") height, width

      stop
   end subroutine logical2D

   subroutine logical3D(array, height, width, depth)
      logical, allocatable, intent(inout) :: array(:, :, :)
      integer, intent(in) :: height, width, depth

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width  .and. &
             size(array, 3) .eq. depth) return

         deallocate(array)
      end if

      allocate(array(height, width, depth), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' x ', I0, ' logicals')") height, width, depth

      stop
   end subroutine logical3D

   subroutine integer1D(array, height)
      integer, allocatable, intent(inout) :: array(:)
      integer, intent(in) :: height

      integer :: status

      if (allocated(array)) then
         if (size(array) .eq. height) return

         deallocate(array)
      end if

      allocate(array(height), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' integers')") height

      stop
   end subroutine integer1D

   subroutine integer2D(array, height, width)
      integer, allocatable, intent(inout) :: array(:, :)
      integer, intent(in) :: height, width

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width) return

         deallocate(array)
      end if

      allocate(array(height, width), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' integers')") height, width

      stop
   end subroutine integer2D

   subroutine integer3D(array, height, width, depth)
      integer, allocatable, intent(inout) :: array(:, :, :)
      integer, intent(in) :: height, width, depth

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width  .and. &
             size(array, 3) .eq. depth) return

         deallocate(array)
      end if

      allocate(array(height, width, depth), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' x ', I0, ' integers')") height, width, depth

      stop
   end subroutine integer3D

   subroutine real1D(array, height)
      real(dp), allocatable, intent(inout) :: array(:)
      integer, intent(in) :: height

      integer :: status

      if (allocated(array)) then
         if (size(array) .eq. height) return

         deallocate(array)
      end if

      allocate(array(height), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' doubles')") height

      stop
   end subroutine real1D

   subroutine real2D(array, height, width)
      real(dp), allocatable, intent(inout) :: array(:, :)
      integer, intent(in) :: height, width

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width) return

         deallocate(array)
      end if

      allocate(array(height, width), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' doubles')") height, width

      stop
   end subroutine real2D

   subroutine real3D(array, height, width, depth)
      real(dp), allocatable, intent(inout) :: array(:, :, :)
      integer, intent(in) :: height, width, depth

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width  .and. &
             size(array, 3) .eq. depth) return

         deallocate(array)
      end if

      allocate(array(height, width, depth), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' x ', I0, ' doubles')") height, width, depth

      stop
   end subroutine real3D

   subroutine character1D(array, height)
      character(*), allocatable, intent(inout) :: array(:)
      integer, intent(in) :: height

      integer :: status

      if (allocated(array)) then
         if (size(array) .eq. height) return

         deallocate(array)
      end if

      allocate(array(height), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' characters')") height

      stop
   end subroutine character1D

   subroutine character2D(array, height, width)
      character(*), allocatable, intent(inout) :: array(:, :)
      integer, intent(in) :: height, width

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width) return

         deallocate(array)
      end if

      allocate(array(height, width), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' characters')") height, width

      stop
   end subroutine character2D

   subroutine character3D(array, height, width, depth)
      character(*), allocatable, intent(inout) :: array(:, :, :)
      integer, intent(in) :: height, width, depth

      integer :: status

      if (allocated(array)) then
         if (size(array, 1) .eq. height .and. &
             size(array, 2) .eq. width  .and. &
             size(array, 3) .eq. depth) return

         deallocate(array)
      end if

      allocate(array(height, width, depth), stat=status)

      if (status .eq. 0) return

      write (*, "('Unable to allocate memory for array of ',&
         & I0, ' x ', I0, ' x ', I0, ' characters')") height, width, depth

      stop
   end subroutine character3D
end module memory
