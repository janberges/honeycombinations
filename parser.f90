module parser
   use global
   implicit none
   private
   
   public :: parse
   
   interface parse
      module procedure parse_logical, parse_integer, parse_real
   end interface parse
   
contains
   
   subroutine parse_logical(str, var, ok)
      character(*), intent(in) :: str
      logical, intent(out) :: var
      logical, intent(out), optional :: ok
      
      logical :: tmp
      integer :: stat
      
      read (str, *, iostat=stat) tmp
      
      if (present(ok)) ok = stat .eq. 0
      
      if (stat .eq. 0) then
         var = tmp
      else
         write (*, "('Ignored statement because of a bad logical value')")
      end if
   end subroutine parse_logical
   
   subroutine parse_integer(str, var, ok)
      character(*), intent(in) :: str
      integer, intent(out) :: var
      logical, intent(out), optional :: ok
      
      integer :: tmp, stat
      
      read (str, *, iostat=stat) tmp
      
      if (present(ok)) ok = stat .eq. 0
      
      if (stat .eq. 0) then
         var = tmp
      else
         write (*, "('Ignored statement because of a bad integer value')")
      end if
   end subroutine parse_integer
   
   subroutine parse_real(str, var, ok)
      character(*), intent(in) :: str
      real(dp), intent(out) :: var
      logical, intent(out), optional :: ok
      
      real(dp) :: enumerator, denominator, tmp
      integer :: i, stat
      
      i = scan(str, ':/')
      
      if (i .eq. 0) then
         read (str, *, iostat=stat) tmp
         call check
         
         var = tmp
      else
         read (str(:i - 1), *, iostat=stat) enumerator
         call check
         
         read (str(i + 1:), *, iostat=stat) denominator
         call check
         
         var = enumerator / denominator
      end if
   
   contains
      
      subroutine check
         if (present(ok)) ok = stat .eq. 0
         
         if (stat .ne. 0) then
            write (*, "('Ignored statement because of a bad real value')")
            return
         end if
      end subroutine check
   end subroutine parse_real
end module parser
