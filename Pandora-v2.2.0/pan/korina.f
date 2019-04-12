      subroutine KORINA
     $(N,IU,IL,RHO,YBAR,CHI,AW,XLB1)
C
C     Rudolf Loeser, 1997 Sep 26
C---- Moves data (possibly fudged) into Line Intensity Data Block.
C     (This is version 3 of KORINA.)
C     !DASH
      save
C     !DASH
      real*8 AW, CHI, RHO, XLB1, YBAR
      integer IL, IU, IUL, MMAW, MMJBR, MMQHI, MMRHO, N
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(28),MMRHO)
      equivalence (MML(27),MMJBR)
      equivalence (MML(66),MMQHI)
      equivalence (MML(65),MMAW )
C     !DASH
      external INTRANS, MOVE1, HI, BYE
C
C               RHO(N,NT), YBAR(N,NT), CHI(N,NT), AW(N,NT), XLB1(Li1len)
      dimension RHO(N,*),  YBAR(N,*),  CHI(N,*),  AW(N,*),  XLB1(*)
C
      call HI ('KORINA')
C     !BEG
      call INTRANS (IU, IL, 'KORINA', IUL)
C
      call MOVE1   (RHO (1,IUL), N, XLB1(MMRHO))
      call MOVE1   (YBAR(1,IUL), N ,XLB1(MMJBR))
      call MOVE1   (CHI (1,IUL), N ,XLB1(MMQHI))
      call MOVE1   (AW  (1,IUL), N ,XLB1(MMAW ))
C     !END
      call BYE ('KORINA')
C
      return
      end
