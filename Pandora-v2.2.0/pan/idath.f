      subroutine IDATH
     $(NO,N,RAT,PRAT,KNT,INP,LABEL,MODE)
C
C     Rudolf Loeser, 1991 Jun 07
C---- Plots Iterative Behavior = Iterative Ratio.
C     !DASH
      save
C     !DASH
      real*8 PRAT, RAT
      integer INP, J, KNT, KS, L, LF, LS, MODE, N, NO, NPNT
      character LABEL*15
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external  ISUA, REPAIR, DARUK, DIBE, HI, BYE
      intrinsic max
C
C               RAT(N,KNT), PRAT(N)
      dimension RAT(N,*),   PRAT(*)
C     !EJECT
C
      call HI ('IDATH')
C     !BEG
C---- Initialize plot image
      call ISUA     (IMAGE,N)
      NPNT = 0
      KS   = 0
C
C---- Loop over all ratios (skipping "input" set and last set)
      LF = max((KNT-26),(INP+1))
      LS = KNT-1
      J  = 0
      do 100 L = LF,LS
        J  = J+1
C----   Set up plottable version of this ratio
        call REPAIR (N,RAT(1,L),PRAT,NPNT)
C----   Enter into plot image
        call DARUK  (IMAGE,RAT(1,L),PRAT,N,KS,ALPHS(J))
  100 continue
C
      if(NPNT.gt.(N/8)) then
C----   Print
        call DIBE   (NO,IMAGE,N,LF,ALPHS(1),LS,ALPHS(J),LABEL,KS,MODE)
      end if
C     !END
      call BYE ('IDATH')
C
      return
      end
