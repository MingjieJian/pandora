      subroutine LUCIFER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 Sep 24
C---- Allocates scratch storage for BLEND.
C     (This is version 2 of LUCIFER.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX
      character CALLER*(*)
C     !COM
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
      call HI ('LUCIFER')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+MIKLEN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LUCIFER')
C
      return
      end
