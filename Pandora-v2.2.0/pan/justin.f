      subroutine JUSTIN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1975 Sep 10
C---- Allocates scratch storage for ANCHOR.
C     !DASH
      save
C     !DASH
      integer IN, IS, MLS, MUX, N, NMLS
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(33),MLS)
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
      call HI ('JUSTIN')
C     !BEG
      call WGET (IS,  CALLER)
C
      NMLS = N*MLS
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NMLS
      IN( 3) = IN( 2)+NMLS
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+MIKLEN
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JUSTIN')
C
      return
      end
