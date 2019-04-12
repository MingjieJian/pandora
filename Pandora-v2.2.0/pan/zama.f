      subroutine ZAMA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Nov 20
C---- Allocates scratch storage for GROUND.
C     (This is version 2 of ZAMA.)
C     !DASH
      save
C     !DASH
      integer IN, IS, M, MPNT, MUX, N, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 3),M  )
      equivalence (JZQ( 5),NT )
C
C---- ALVIN       as of 1995 Aug 08
      integer     LIMSCL
      parameter   (LIMSCL=11)
C     The number of TAU-scales to be printed fancily.
C     (Used in PICTURE, etc.)
C     .
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic min
C
      dimension IN(*)
C
      call HI ('ZAMA')
C     !BEG
      call WGET (IS ,CALLER)
C
      MPNT = (min(NT,LIMSCL))*(N-1)+M
C
      IN( 1) = IS
C
      MUX    = IN( 1)+MPNT
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('ZAMA')
C
      return
      end
