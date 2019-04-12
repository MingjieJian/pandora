      subroutine HOGRO
     $(VSB,VM,VXS,X,N,CVSB,CVXS,FR,NVSB,KVSB)
C
C     Rudolf Loeser, 1990 Apr 26
C---- Checks "Sobolev velocity".
C     !DASH
      save
C     !DASH
      real*8 CVSB, CVXS, FR, VM, VSB, VXS, X
      integer JJFMV, JJHEA, JJHND, JJZ, KVSB, N, NVSB
      logical DONE, KILROY
      character SOBO*16, SRCE*35
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(225),JJFMV)
C     !DASH
      external KALMIA, TEARO, ORATE, KRATE, HI, BYE
C
      dimension X(*)
C
C               VSB(N), VM(N), VXS(N), FR(N)
      dimension VSB(*), VM(*), VXS(*), FR(*)
C
      data SRCE /'Source function expansion velocity'/
      data SOBO /'Sobolev velocity'/
C     !EJECT
C
      call HI ('HOGRO')
C     !BEG
      if(NVSB.le.0) then
        KVSB = 0
      else
C----   Make sure VSB=-VM when VM exists
        call TEARO      (N, VM, VSB, SOBO, .true., DONE)
        if(DONE) then
          KVSB = 1
        else
C----     Make sure VSB=VXS when VXS exists
          call ORATE    (N, CVXS, VXS, SRCE, CVSB, VSB, SOBO, .false.,
     $                   DONE)
          if(DONE) then
            KVSB = 2
          else
C----       Make sure VSB is otherwise set up properly
            KILROY = .true.
            call KALMIA (N, CVSB, X(JJZ), X(JJHND), X(JJHEA), FR,
     $                   X(JJFMV), KILROY, SOBO, 0, VSB)
            KVSB = 3
          end if
        end if
C----   Make sure VSB exists now, and abort if not
        call KRATE      (NVSB, N, VSB, VM, VXS, KVSB)
      end if
C     !END
      call BYE ('HOGRO')
C
      return
      end
