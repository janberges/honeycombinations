module control
   use diagonalization
   use dope
   use dos
   use energy
   use global
   use hamiltonian
   use montecarlo
   use neighborhood
   use out
   use parser
   use plot
   use random
   use timer
   
   use move_one
   implicit none
   private
   
   public :: read_file, read_args, read_stdin
   
   logical, save :: loop = .true.
   
contains
   
   subroutine read_file(file)
      character(*), intent(in) :: file
      
      character(50) :: cmd
      
      integer, parameter :: unit = 11
      integer, save      :: stat = 0
      
      open(unit, file=file, action='read', status='old', form='formatted')
      
      do
         read (unit, *, iostat=stat) cmd
         if (stat .ne. 0) exit
         call eval(trim(cmd))
      end do
      
      close(unit)
   end subroutine read_file
   
   subroutine read_args
      integer :: i, l, lmax
      character(:), allocatable :: cmd
      
      lmax = 1
      
      do i = 1, command_argument_count()
         call get_command_argument(i, length=l)
         if (l .gt. lmax) lmax = l
      end do
      
      allocate(character(lmax) :: cmd)
      
      do i = 1, command_argument_count()
         call get_command_argument(i, cmd, l)
         call eval(cmd(:l))
      end do
   end subroutine read_args
   
   subroutine read_stdin
      character(50) :: cmd
      
      do while (loop)
         write (*, "('> ')", advance='no')
         read (*, *) cmd
         call eval(trim(cmd))
      end do
   end subroutine read_stdin
   
   subroutine eval(cmd)
      character(*), intent(in) :: cmd
      
      integer :: i
      
      i = index(cmd, '=')
      
      if (i .eq. 0) then
         i = index(cmd, '>')
         
         if (i .eq. 0) then
            call run(cmd)
         else
            call record(cmd(:i - 1), cmd(i + 1:))
         end if
      else
         call set(cmd(:i - 1), cmd(i + 1:))
      end if
   end subroutine eval
   
   subroutine run(cmd)
      character(*), intent(in) :: cmd
      
      if (s%time) call time
      
      select case(cmd)
         case ('.', 'show')
            call show_lattice
         
         case ('clear')
            call clear
         
         case ('vary')
            call vary
         
         case ('H', 'Hamiltonian', 'h', 'hamiltonian')
            call show_hamiltonian
         
         case ('E', 'energy')
            call show_energy
         
         case ('P')
            call show_correlations
         
         case ('dos', 'DOS')
            call histogram
         
         case ('seed', 'sow')
            call sow
         
         case ('markov')
            call markov
            
         case ('mix')
            call randomize
            
         case ('mix2')
            call randomize2
         
         case ('0')
            call count_zeros
			
         case ('move1')
            call stability
         
         case ('bye')
            loop = .false.
         
         case default
            write (*, '(3A)') "Ignored unknown parameter '", cmd, "'"
      end select
      
      if (s%time) call time('This')
   end subroutine run
   
   subroutine record(content, file)
      character(*), intent(in) :: content, file
      
      select case(content)
         case ('psi')
            if (todo%energies) call energies

            call save(file, s%psi, '(ES13.5E2)')
         
         case ('ls', 'config')
            if (todo%configuration) call randomize

            call save(file, s%ls(:s%nX))
         
         case ('E', 'energies')
            if (todo%energies) call energies

            call save(file, s%W(:s%dim, s%i))
         
         case ('P', 'chances')
            if (todo%correlations) call correlations

            call save(file, s%chances(:), '(F16.14)')
         
         case ('matches')
            if (todo%correlations) call correlations

            call save(file, s%matches(:))
         
         case default
            write (*, '(3A)') "Did not save unknown content '", content, "'"
      end select
   end subroutine record
   
   subroutine set(key, value)
      character(*), intent(in) :: key, value
      
      integer :: nX = 0, ne = 0, kind
      real(dp) :: cX = 0.1, ce = 0.0
      
      select case(key)
         case ('l', 'size')
            call parse(value, s%l)
            
            todo = dependencies()
            
            s%l = max(s%l, 0)
            
            s%nC = 2 * s%l ** 2
            
            nX = 2
            call change_coverage(nX)
            
            s%dim = s%nC + s%nX
            
            call change_charge(s%dim)
            
            s%dimax = 2 * s%nC
         
         case ('cX', 'coverage')
            call parse(value, cX)
            
            nX = nint(cX * s%nC)
            call change_coverage(nX)
         
         case ('ce', 'doping')
            call parse(value, ce)
            
            ne = nint((1.0_dp + ce) * s%nC + s%nX)
            call change_charge(ne)
         
         case ('nX', 'adatoms')
            call parse(value, nX)
            call change_coverage(nX)
         
         case ('ne', 'electrons')
            call parse(value, ne)
            call change_coverage(ne)
         
         case ('eC')
            call parse(value, s%eC)
            todo%hamiltonian = .true.
         
         case ('t')
            call parse(value, s%t)
            todo%hamiltonian = .true.
         
         case ('eX')
            call parse(value, s%eX)
            todo%hamiltonian = .true.
         
         case ('V')
            call parse(value, s%V)
            todo%hamiltonian = .true.
         
         case ('p', 'penalties')
            call parse(value, s%p)
         
         case ('kT')
            call parse(value, s%kT)
         
         case ('R')
            call parse(value, s%r)
         
         case ('n')
            call parse(value, s%n)
         
         case ('show')
            call parse(value, s%show)
         
         case ('rrr')
            call parse(value, s%rrr)
            todo%space = .true.
         
         case ('time')
            call parse(value, s%time)
         
         case ('color')
            call parse(value, s%color)
         
         case ('jobz')
            s%jobz = value
         
         case ('d')
            call parse(value, s%d)
            todo%neighbors = .true.
         
         case ('C')
            s%C = value
            todo%plot = .true.
         
         case ('X')
            s%X = value
            todo%plot = .true.
         
         case ('filename')
            s%file = value
         
         case ('new_order', 'order')
            call get_kind(value, kind)
            call new_order(kind)
         
         case default
            write (*, '(3A)') "Ignored unknown parameter '", key, "'"
      end select
   end subroutine set
end module control
