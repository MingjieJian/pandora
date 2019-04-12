      subroutine LOLA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1977 Apr 05
C---- Allocates scratch storage for FREIA.
C     (This is version 2 of LOLA.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NCP
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(44),NCP)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LOLA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+MIKLEN
      IN( 3) = IN( 2)+N
      MUX    = IN( 3)+NCP*N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LOLA')
C
      return
      end
