      subroutine BABOON
     $(N,XLB1,S)
C
C     Rudolf Loeser, 2005 May 11
C---- Sets up "Perseus" Line Source Function, for PRD calculations.
C     (This is version 3 of BABOON.)
C     !DASH
      save
C     !DASH
      real*8 S, XLB1
      integer MMS, MMSN, N
      logical SZERO
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(67),MMSN )
      equivalence (MML(26),MMS  )
C     !DASH
      external MOVE1, NAUGHTD, HI, BYE
C
C               XLB1(Li1len), S(N)
      dimension XLB1(*),      S(*)
C
      call HI ('BABOON')
C     !BEG
      call NAUGHTD (XLB1(MMS), 1, N, SZERO)
      if(SZERO) then
        call MOVE1 (XLB1(MMSN), N, S)
      else
        call MOVE1 (XLB1(MMS),  N, S)
      end if
C     !END
      call BYE ('BABOON')
C
      return
      end
