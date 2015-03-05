module dope
   use global
   use memory
   implicit none
   private
   
   public :: change_coverage, change_charge
   
contains
   
   subroutine change_coverage(nX)
      integer, intent(inout) :: nX
      
      todo%energies     = .true.
      todo%energy       = .true.
      todo%penalty      = .true.
      todo%correlations = .true.
      
      nX = min(max(nX, 0), s%nC)
      
      if (nX .eq. s%nX) return
      
      if (.not. todo%plot) then
         s%plot(s%px(s%ls(s%nX + 1:nX))) = s%X
         s%plot(s%px(s%ls(nX + 1:s%nX))) = s%C
      end if

      s%ne = s%ne - s%nX + nX
      s%nX = nX
      s%dim = s%nC + s%nX
      
      if (.not. todo%space) then
         call alloc(s%wH,  s%dim, s%dim)
         call alloc(s%psi, s%dim, s%dim)
      end if
   end subroutine change_coverage
   
   subroutine change_charge(ne)
      integer, intent(inout) :: ne
      
      todo%energy = .true.
      
      ne = min(max(ne, 0), 2 * s%dim)
      
      if (ne .eq. s%ne) return
      
      s%ne = ne
   end subroutine change_charge
end module dope
