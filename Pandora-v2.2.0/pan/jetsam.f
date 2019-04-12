      subroutine JETSAM
     $(LU,I,XLB1,DWS)
C
C     Rudolf Loeser, 1992 Oct 15
C---- Saves Doppler Widths for printing, for CRUMB.
C     (This is version 2 of JETSAM.)
C     !DASH
      save
C     !DASH
      real*8 DWS, XLB1
      integer I, LU, MMDW, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(13),MMDW )
C     !DASH
      external MOVE1, HI, BYE
C
C               XLB1(Li1len), DWS(N,NT)
      dimension XLB1(*),      DWS(N,*)
C
      call HI ('JETSAM')
C     !BEG
      if(LU.gt.0) then
        call MOVE1 (XLB1(MMDW),N,DWS(1,I))
      end if
C     !END
      call BYE ('JETSAM')
C
      return
      end
