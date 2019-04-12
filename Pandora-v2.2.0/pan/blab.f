      subroutine BLAB
     $(NE,NZ,KK,EMUX,V,CALLER)
C
C     Rudolf Loeser, 1982 Mar 30
C---- Dumps, for DURIAN.
C     !DASH
      save
C     !DASH
      real*8 EMUX, V
      integer KK, LUEO, NE, NZ
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ARROUT, HI, BYE
C
C               EMUX(NZ,KKX), V(NZ,KKX)
      dimension EMUX(*),      V(*)
C
      call HI ('BLAB')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) NE,NZ,KK
  100 format(' ','Details of Lyman WN matrices calculation: NE =',I5,
     $           ', NZ =',I5,', KK =',I5)
C
      call ARROUT (LUEO, EMUX, NZ, KK, 'EMUX')
      call ARROUT (LUEO, V,    NZ, KK, 'V')
C     !END
      call BYE ('BLAB')
C
      return
      end
