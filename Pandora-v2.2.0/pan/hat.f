      subroutine HAT
     $(LIM,KNOT,IN,IU,IL,NL,ARHO,Z,GM,BD,V)
C
C     Rudolf Loeser, 1974 Mar 21
C---- Computes VU or VL, for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 ARHO, BD, BDK1, GM, GMK1, V, Z, ZERO
      integer IL, IN, IU, K, KIN, KNOT, LIM, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXUL, DIVIDE, HI, BYE
C
C               ARHO(MUL), Z(NL,NL), GM(NL), BD(NL)
      dimension ARHO(*),   Z(NL,*),  GM(*),  BD(*)
C
C
      call HI ('HAT')
C     !BEG
      V = ZERO
C
      if(LIM.le.NL) then
        do 100 K = LIM,NL
          if(K.ne.KNOT) then
            call DIVIDE (BD(K), BD(1), BDK1)
            call DIVIDE (GM(K), GM(1), GMK1)
            call INDXUL (K, IN, KIN)
            V = V+ARHO(KIN)*(GMK1*BDK1)
          end if
  100   continue
      end if
C
      do 101 K = 1,NL
        if((K.ne.IL).and.(K.ne.IU)) then
          call DIVIDE   (BD(K), BD(1), BDK1)
          call DIVIDE   (GM(K), GM(1), GMK1)
          V = V+Z(K,IN)*(GMK1*BDK1)
        end if
  101 continue
C     !END
      call BYE ('HAT')
C
      return
      end
