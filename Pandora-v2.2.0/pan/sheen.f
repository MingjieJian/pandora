      subroutine SHEEN
     $(N,ICE,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,XJNU,SIG,T1,T2,T3)
C
C     Rudolf Loeser, 2005 Jan 24
C---- Sets up PRD data in Diana/Orion data blocks for the
C     frequency-angle summations.
C     (This is version 5 of SHEEN.)
C     !DASH
      save
C     !DASH
      real*8 PJNU, PRXI, PSIG, PXRD, PYRD, PZRD, SIG, T1, T2, T3, XJNU
      integer ICE, N
C     !DASH
      external ONE1, ZERO1, MOVE1, ARRMUL, HI, BYE
C
C               PJNU(N), PRXI(N), PSIG(N), PXRD(N), PYRD(N), XJNU(N),
      dimension PJNU(*), PRXI(*), PSIG(*), PXRD(*), PYRD(*), XJNU(*),
C
C               SIG(N), T1(N), T2(N), T3(N), PZRD(N)
     $          SIG(*), T1(*), T2(*), T3(*), PZRD(*)
C
      call HI ('SHEEN')
C     !BEG
      call MOVE1    (PJNU, N, XJNU)
      call MOVE1    (PSIG, N, SIG)
      if(ICE.eq.2) then
        call MOVE1  (PXRD, N, T1)
        call MOVE1  (PZRD, N, T2)
        call MOVE1  (PYRD, N, T3)
      else
        call ONE1   (T1, N)
        call ZERO1  (T2, N)
        call ARRMUL (PRXI, PJNU, T3, N)
      end if
C     !END
      call BYE ('SHEEN')
C
      return
      end
