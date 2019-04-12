      subroutine PARASO
     $(IT,N,NL,NSL,BDI,BDX)
C
C     Rudolf Loeser, 1978 May 18
C---- Sets up an extended BD-set, for MOREL.
C     (This is version 2 of PARASO.)
C     !DASH
      save
C     !DASH
      real*8 BDI, BDX, F, OMF, ONE
      integer I, IT, J, N, NL, NSL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ONE1, MANE, HI, BYE
C
C               BDI(N,NL), BDX(N,NSL)
      dimension BDI(N,*),  BDX(N,*)
C
      call HI ('PARASO')
C     !BEG
      do 101 J = NL+1,NSL
        if(IT.le.1) then
          call ONE1 (BDX(1,J),N)
        else
          call MANE (NL,J,NSL,F)
          OMF = ONE-F
          do 100 I = 1,N
            BDX(I,J) = BDI(I,NL)*OMF+F
  100     continue
        end if
  101 continue
C     !END
      call BYE ('PARASO')
C
      return
      end
