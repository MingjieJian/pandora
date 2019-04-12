      subroutine HIGRI
     $(VM,X,N,FR,HEND,KVLG)
C
C     Rudolf Loeser, 1990 Apr 26
C---- Checks "mass-motion velocity".
C     (This is version 2 of HIGRI.)
C     !DASH
      save
C     !DASH
      real*8 FR, HEND, VM, X
      integer JJFMV, JJHEA, JJHND, JJVBM, JJZ, KVLG, N
      logical VZERO
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(225),JJFMV)
      equivalence (IZOQ(196),JJVBM)
C     !DASH
      external TAMERS, NAUGHTD, WENDY, HALT, HI, BYE
C
      dimension X(*)
C
C               VM(N), FR(N), HEND(N)
      dimension VM(*), FR(*), HEND(*)
C
      call HI ('HIGRI')
C     !BEG
C---- Compute
      call TAMERS  (N,X(JJZ),X(JJHND),X(JJHEA),X(JJVBM),X(JJFMV),FR,
     $              HEND,VM)
C---- Verify
      call NAUGHTD (VM,1,N,VZERO)
      if((KVLG.gt.0).and.VZERO) then
        write (MSSLIN(1),100) KVLG
  100   format('KVLG =',I2,': mass-motion velocity equals zero when ',
     $         'it MUST be nonzero.')
        call HALT  ('HIGRI',1)
      end if
C---- Continuum Recalculation control
      call WENDY   (VM,1,N,39,'HIGRI')
C     !END
      call BYE ('HIGRI')
C
      return
      end
