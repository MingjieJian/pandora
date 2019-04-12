      subroutine RYMON
     $(XLB1,XLB2)
C
C     Rudolf Loeser, 2003 Apr 11
C---- Supervises writing to Spectrum Save File.
C     !DASH
      save
C     !DASH
      real*8 XLB1, XLB2
      integer MMB, MMDL, MMPGD, MMPGL, MMS, MMSLF, MMTAU
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(26),MMS  )
      equivalence (MML(22),MMB  )
      equivalence (MML(16),MMTAU)
      equivalence (MML(53),MMPGL)
      equivalence (MML(54),MMPGD)
      equivalence (MML(59),MMDL )
      equivalence (MMP( 6),MMSLF)
C     !DASH
      external MYRON, HI, BYE
C
C               XLB1(Li1len), XLB2(Li2len)
      dimension XLB1(*),      XLB2(*)
C
      call HI ('RYMON')
C     !BEG
      call MYRON (XLB1(MMS), XLB1(MMB), XLB1(MMTAU), XLB1(MMPGL),
     $            XLB1(MMPGD), XLB1(MMDL), XLB2(MMSLF))
C     !END
      call BYE ('RYMON')
C
      return
      end
