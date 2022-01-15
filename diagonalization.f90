module diagonalization
   use global
   use hamiltonian
   use memory
   implicit none
   private

   public :: energies

   integer :: lwork, liwork

   integer, allocatable :: iwork(:), isuppz(:)

   real(dp), allocatable :: work(:)

   interface
      subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info)
         use global

         character, intent(in) :: jobz, uplo

         integer, intent(in) :: n, lda, lwork
         integer, intent(out) :: info

         real(dp), intent(inout) :: a(lda, *)
         real(dp), intent(out) :: w(*), work(*)
      end subroutine dsyev

      subroutine dsyevr(jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol, &
            m, w, z, ldz, isuppz, work, lwork, iwork, liwork, info)
         use global

         character, intent(in) :: jobz, range, uplo

         integer, intent(in) :: n, lda, il, iu, ldz, lwork, liwork
         integer, intent(out) :: m, info, isuppz(*), iwork(*)

         real(dp), intent(inout) :: a(lda, *)
         real(dp), intent(in) :: vl, vu, abstol
         real(dp), intent(out) :: w(*), z(ldz, *), work(*)
      end subroutine dsyevr

      integer function ilaenv(ispec, name, opts, n1, n2, n3, n4)
         integer, intent(in) :: ispec, n1, n2, n3, n4

         character(*), intent(in) :: name, opts
      end function ilaenv
   end interface

contains

   subroutine energies(info)
      integer, intent(out), optional :: info

      integer :: i, m, error

      if (todo%hamiltonian) call new_hamiltonian
      if (todo%space)       call space

      todo%energies = .false.

      do i = 1, s%dim
         s%wH(:i, i) = s%H(:i, i)
      end do

      if (s%rrr) then
         call dsyevr(               &
            &   jobz = s%jobz,      &
            &  range = 'A',         &
            &   uplo = 'U',         &
            &      n = s%dim,       &
            &      a = s%wH(1, 1),  &
            &    lda = s%dim,       &
            &     vl = 0.0_dp,      &
            &     vu = 0.0_dp,      &
            &     il = 0,           &
            &     iu = 0,           &
            & abstol = 0.0_dp,      &
            &      m = m,           &
            &      w = s%W(1, s%i), &
            &      z = s%psi(1, 1), &
            &    ldz = s%dim,       &
            & isuppz = isuppz(1),   &
            &   work = work(1),     &
            &  lwork = lwork,       &
            &  iwork = iwork(1),    &
            & liwork = liwork,      &
            &   info = error        )
      else
         call dsyev(               &
            &  jobz = s%jobz,      &
            &  uplo = 'U',         &
            &     n = s%dim,       &
            &     a = s%wH(1, 1),  &
            &   lda = s%dim,       &
            &     w = s%W(1, s%i), &
            &  work = work(1),     &
            & lwork = lwork,       &
            &  info = error        )

         if (s%jobz .eq. 'V') s%psi(:, :) = s%wH
      end if

      if (present(info)) info = error
   end subroutine energies

   subroutine space
      todo%space = .false.

      call alloc(s%W, s%dimax, 2)
      call alloc(s%psi, s%dim, s%dim)

      if (s%rrr) then
         call alloc(isuppz, 2 * s%dimax)

         lwork = max(             &
            & 1,                  &
            & 26 * s%dimax,       &
            & s%dimax * (1 + max( &
               & env('DSYTRD'),   &
               & env('DORMTR'))))

         liwork = max(1, 10 * s%dimax)
         call alloc(iwork, liwork)
      else
         lwork = s%dimax * (env('DSYTRD') + 2)
      end if

      call alloc(work, lwork)
   end subroutine space

   integer function env(fun)
      character(*), intent(in) :: fun

      env = ilaenv(1, fun, 'U', s%dimax, -1, -1, -1)
   end function env
end module diagonalization
