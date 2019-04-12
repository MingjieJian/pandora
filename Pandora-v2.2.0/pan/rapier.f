      subroutine RAPIER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Jan 24
C---- Computes scratch storage requirements of LINCOLN.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, N2
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C     !DASH
      external  WGET, WLCK, HI, BYE
      dimension IN(*)
C
      call HI ('RAPIER')
C     !BEG
      call WGET (IS ,CALLER)
C
      N2 = N**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N2
      IN( 3) = IN( 2)+LPDLEN
      MUX    = IN( 3)+N2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('RAPIER')
C
      return
      end
