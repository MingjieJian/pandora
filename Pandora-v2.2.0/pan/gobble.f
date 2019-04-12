      subroutine GOBBLE
     $(ELSYM,N,BD1,POP1,POPK,TE,XNE,CPR,CHI,ABDEL)
C
C     Rudolf Loeser, 1978 Sep 21
C---- Computes default BDj, when j=1
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BD1, CHI, CPR, FL, ONE, POP1, POPK, PPK, TE, XNE,
     $       ZERO, dummy
      integer I, INDX, N
      logical STOP
      character ELSYM*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external FRANK, SAGA, DIVIDE, FLOCK, HI, BYE
C
C               BD1(N), POP1(N), POPK(N), ABDEL(N), XNE(N), CPR(N,NMT),
      dimension BD1(*), POP1(*), POPK(*), ABDEL(*), XNE(*), CPR(N,*),
C
C               CHI(NMT), TE(N)
     $          CHI(*),   TE(*)
C
      data STOP /.true./
C
      call HI ('GOBBLE')
C     !BEG
C---- Get index of ion in ELEMENT data table
      call FRANK        (ELSYM, 0, dummy, dummy, dummy, dummy, INDX)
      if(INDX.eq.0) then
        call SAGA       (ELSYM, 'GOBBLE', STOP)
      end if
C
C---- Compute
      do 100 I = 1,N
        if(BD1(I).lt.ZERO) then
          if(ABDEL(I).eq.ZERO) then
            BD1(I) = ONE
          else
            call DIVIDE (POP1(I), POPK(I), PPK)
            call FLOCK  (TE(I), XNE(I), CPR(I,INDX), CHI(INDX), FL)
            BD1(I) = PPK*FL
          end if
        end if
  100 continue
C     !END
      call BYE ('GOBBLE')
C
      return
      end
