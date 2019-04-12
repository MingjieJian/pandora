      subroutine SEKLA
     $(LU,N,LDL,DP,SAP,DW,GMA,DRLMI,XJBR)
C
C     Rudolf Loeser, 1988 Nov 09
C---- Prints, for TURNIP.
C     (This is version 2 of SEKLA.)
C     !DASH
      save
C     !DASH
      real*8 DP, DRLMI, DW, GMA, SAP, XJBR
      integer I, IPRDD, LDL, LU, N
      character BLANK*1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 98),IPRDD)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external LINER, MENTHA, HI, BYE
C
C               DP(N,LDL), SAP(N,LDL), DW(N), XJBR(N), GMA(N), DRLMI(N)
      dimension DP(N,*),   SAP(N,*),   DW(*), XJBR(*), GMA(*), DRLMI(*)
C
      call HI ('SEKLA')
C     !BEG
      call LINER    (2, LU)
      if(LDL.le.1) then
        write (LU,100) BLANK
  100   format(' ',4X,9X,'JBAR',10X,'GMA',8X,'DRLIM',11X,'DW',:,A,
     $             10X,'DP',10X,'SAP')
        call LINER  (1, LU)
        write (LU,101) (I,XJBR(I),GMA(I),DRLMI(I),DW(I),DP(I,1),
     $                    SAP(I,1),I=1,N,IPRDD)
  101   format(5(' ',I4,1P6E13.5/))
C
      else
        write (LU,100)
        call LINER  (1, LU)
        write (LU,102) (I,XJBR(I),GMA(I),DRLMI(I),DW(I),I=1,N,IPRDD)
  102   format(5(' ',I4,1P4E13.5/))
C
        call MENTHA (LU, IPRDD, N, LDL, DP, SAP)
      end if
C     !END
      call BYE ('SEKLA')
C
      return
      end
