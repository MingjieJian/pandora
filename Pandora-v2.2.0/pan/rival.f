      subroutine RIVAL
     $(YHZ,TCX,RES,K,KRES)
C
C     Rudolf Loeser, 2004 Sep 08
C---- Sets up a residual line profile.
C     Returns KRES = 1 only if there is at least one nonzero value,
C     returns KRES = 0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 RES, TCX, YHZ, ZERO
      integer I, K, KRES
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               YHZ(K), TCX(K), RES(K)
      dimension YHZ(*), TCX(*), RES(*)
C
      call HI ('RIVAL')
C     !BEG
      KRES = 0
C
      do 100 I = 1,K
        if((YHZ(I).ne.ZERO).and.(TCX(I).ne.ZERO)) then
          RES(I) = YHZ(I)/TCX(I)
          if(RES(I).ne.ZERO) then
            KRES = 1
          end if
        else
          RES(I) = ZERO
        end if
  100 continue
C     !END
      call BYE ('RIVAL')
C
      return
      end
