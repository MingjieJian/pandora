      subroutine HER
     $(LIM,KNOT,IN,IU,IL,NL,ARHO,Z,X)
C
C     Rudolf Loeser, 1974 Mar 20
C---- Computes XU or XL, for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 ARHO, X, Z, ZERO
      integer IL, IN, INK, IU, K, KNOT, LIM, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXUL, HI, BYE
C
C               ARHO(MUL), Z(NL,NL)
      dimension ARHO(*),   Z(NL,*)
C
      call HI ('HER')
C     !BEG
      X = ZERO
C
      if(LIM.gt.0) then
        do 100 K = 1,LIM
          if(K.ne.KNOT) then
            call INDXUL (IN,K,INK)
            X = X+ARHO(INK)
          end if
  100   continue
      end if
C
      do 101 K = 1,NL
        if(K.ne.KNOT) then
          X = X+Z(IN,K)
        end if
  101 continue
C     !END
      call BYE ('HER')
C
      return
      end
