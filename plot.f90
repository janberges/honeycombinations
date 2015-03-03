module plot
   use conversion
   use global
   use memory
   use random
   implicit none
   private
   
   public :: new_plot, clear, show_lattice
   
   character(10) :: form
   
contains
   
   subroutine new_plot
      integer :: i
      
      if (todo%configuration) call randomize
      
      todo%plot = .false.
      
      call alloc(s%px, s%nC)
      call alloc(s%plot, 3 * s%nC)
      
      do i = 1, s%nC
         call n2c(i, s%px(i))
      end do
      
      s%plot(:) = ' '
      
      s%plot(s%px(s%ls(:s%nX))) = s%X
      s%plot(s%px(s%ls(s%nX + 1:))) = s%C
      
      do i = 3 * s%l, 3 * s%nC, 3 * s%l
         s%plot(i) = char(10)
      end do
      
      write (form, "('(', I0, 'A)')") max(1, 3 * s%nC)
   end subroutine new_plot
   
   subroutine clear
      write (*,'(4A)', advance='no') char(27), '[2J', char(27), '[;H'
   end subroutine clear
   
   subroutine show_lattice
      if (todo%plot) call new_plot
      
      if (s%color) then
         call show_colored_lattice
      else
         write (*, form, advance='no') s%plot
      end if
   end subroutine show_lattice
   
   subroutine show_colored_lattice
      integer :: i
      
      character(*), parameter ::   &
         & D = char(27) // '[39m', &
         & Y = char(27) // '[33m', &
         & B = char(27) // '[34m'

      if (todo%plot) call new_plot
      
      write (*, '(A)', advance='no') Y
      
      do i = 1, 3 * s%nC
         if (s%plot(i) .eq. s%X) then
            write (*, '(3A)', advance='no') B, s%X, Y
         else
            write (*, '(A)', advance='no') s%plot(i)
         end if
      end do
      
      write (*, '(A)', advance='no') D
   end subroutine show_colored_lattice
end module plot
