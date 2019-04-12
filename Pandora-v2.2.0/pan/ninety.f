      subroutine NINETY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1986 Jun 06
C---- Allocates scratch storage for FIFTY.
C     (This is version 2 of NINETY.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
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
      call HI ('NINETY')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+MIKLEN
      IN( 3) = IN( 2)+KM
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NINETY')
C
      return
      end
