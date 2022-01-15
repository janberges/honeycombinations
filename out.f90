module out
   use global
   implicit none
   private

   public :: save

   integer, parameter :: unit = 14

   interface save
      module procedure           &
         & integer1D, integer2D, &
         &    real1D,    real2D
   end interface save

contains

   subroutine integer1D(file, data, form)
      character(*), intent(in) :: file
      integer, intent(in) :: data(:)
      character(*), intent(in), optional :: form

      open(unit, file=file, action='write', status='replace', form='formatted')

      if (present(form)) then
         write (unit, form) data
      else
         write (unit, '(I0)') data
      end if

      close(unit)
   end subroutine integer1D

   subroutine real1D(file, data, form)
      character(*), intent(in) :: file
      real(dp), intent(in) :: data(:)
      character(*), intent(in), optional :: form

      open(unit, file=file, action='write', status='replace', form='formatted')

      if (present(form)) then
         write (unit, form) data
      else
         write (unit, '(ES21.13E3)') data
      end if

      close(unit)
   end subroutine real1D

   subroutine integer2D(file, data, form)
      character(*), intent(in) :: file
      integer, intent(in) :: data(:, :)
      character(*), intent(in), optional :: form

      integer :: i, j
      character(:), allocatable :: fmt

      if (present(form)) then
         fmt = form
      else
         fmt = '(I5)'
      end if

      open(unit, file=file, action='write', status='replace', form='formatted')

      do i = 1, size(data, 1)
         do j = 1, size(data, 2)
            write (unit, fmt, advance='no') data(i, j)
         end do
         write (unit, '()')
      end do

      close(unit)
   end subroutine integer2D

   subroutine real2D(file, data, form)
      character(*), intent(in) :: file
      real(dp), intent(in) :: data(:, :)
      character(*), intent(in), optional :: form

      integer :: i, j
      character(:), allocatable :: fmt

      if (present(form)) then
         fmt = form
      else
         fmt = '(ES22.13E3)'
      end if

      open(unit, file=file, action='write', status='replace', form='formatted')

      do i = 1, size(data, 1)
         do j = 1, size(data, 2)
            write (unit, fmt, advance='no') data(i, j)
         end do
         write (unit, '()')
      end do

      close(unit)
   end subroutine real2D
end module out
