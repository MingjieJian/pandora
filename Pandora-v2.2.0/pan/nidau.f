      subroutine NIDAU
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Sep 17
C---- Allocates scratch storage for WISSANT.
C     (This is version 2 od NIDAU.)
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
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NIDAU')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+LI1LEN
      MUX    = IN( 2)+LI2LEN
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('NIDAU')
C
      return
      end
