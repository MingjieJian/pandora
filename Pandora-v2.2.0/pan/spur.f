      subroutine SPUR
     $(N,NL,IQRK,IQRL,XNU,CP,TR,TE,RK,RL,UP,UPJ,HJ,KOOL)
C
C     Rudolf Loeser, 1979 Nov 26
C---- Computes RK and RL, for RATES.
C     !DASH
      save
C     !DASH
      real*8 CP, CPJ, F, HJ, ONE, RJ, RK, RL, TE, TR, UP, UPJ, XNU,
     $       XNUJ, dummy
      integer I, IQRK, IQRL, J, N, NL, jummy
      logical IRK, IRL, KOOL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external VOOM, FATE, GATE, HI, BYE
C
C               CP(NSL+1), TE(N), RK(N,NSL), RL(N,NSL), UP(N), UPJ(N),
      dimension CP(*),     TE(*), RK(N,*),   RL(N,*),   UP(*), UPJ(*),
C
C               HJ(N), IQRK(NSL), IQRL(NSL), XNU(NSL), TR(N,NSL)
     $          HJ(*), IQRK(*),   IQRL(*),   XNU(*),   TR(*)
C
      call HI ('SPUR')
C     !BEG
      do 101 J = 1,NL
        XNUJ = XNU(J)
        CPJ  = CP(J)
        IRK  = ((IQRK(J).gt.0).or.KOOL)
        IRL  = ((IQRL(J).gt.0).or.KOOL)
        call VOOM     (TR,XNU,jummy,N,TE,dummy,J,NL,RJ,UP,UPJ,
     $                 .false.,2)
        do 100 I = 1,N
          if(IRK) then
            call FATE (CPJ,XNUJ,RJ,ONE,ONE,UPJ(I),KOOL,RK(I,J),F)
            RK(I,J) = HJ(I)*RK(I,J)
          end if
          if(IRL) then
            call GATE (CPJ,XNUJ,RJ,ONE,ONE,UP(I) ,KOOL,RL(I,J),F)
          end if
  100   continue
  101 continue
C     !END
      call BYE ('SPUR')
C
      return
      end
