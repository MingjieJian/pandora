      subroutine GUSHER
     $(X,N,GD,ELABD)
C
C     Rudolf Loeser, 1991 Jan 09
C---- Computes gas density GD.
C     !DASH
      save
C     !DASH
      real*8 ELABD, GD, HEABD, X, dummy
      integer JJHND, KODE, N
      character QHE*3
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
C     !DASH
      external FRANK, LUNA, GUSH, HI, BYE
C
      dimension X(*)
C
C               GD(N), ELABD(N)
      dimension GD(*), ELABD(*)
C
      data QHE /'HE '/
C
      call HI ('GUSHER')
C     !BEG
      call FRANK (QHE,0,HEABD,dummy,dummy,dummy,KODE)
      call LUNA  (X,QHE,HEABD,ELABD)
C
      call GUSH  (N,X(JJHND),ELABD,GD)
C     !END
      call BYE ('GUSHER')
C
      return
      end
