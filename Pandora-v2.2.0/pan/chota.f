      subroutine CHOTA
     $(T,V,INDEX,SRT)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Computes SRT for CO-lines opacity.
C     INDEX=1 for C(12), =2 for C(13).
C     !DASH
      save
C     !DASH
      real*8 COMASS, ONE, SRT, T, V, dummy
      integer INDEX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DOPPLER, HI, BYE
C
      dimension COMASS(2)
C
      data COMASS /2.8D1, 2.9D1/
C
      call HI ('CHOTA')
C     !BEG
      call DOPPLER (ONE,T,COMASS(INDEX),(V**2),SRT,dummy)
C     !END
      call BYE ('CHOTA')
C
      return
      end
