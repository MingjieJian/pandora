      subroutine JINN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 Aug 21
C---- Allocates scratch storage for TRIDENS.
C     (This is version 2 of JINN.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX
      character CALLER*(*)
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
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
      call HI ('JINN')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+LI1LEN
      IN( 3) = IN( 2)+LI2LEN
      MUX    = IN( 3)+MIKLEN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('JINN')
C
      return
      end
