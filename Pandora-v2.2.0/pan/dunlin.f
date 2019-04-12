      subroutine DUNLIN
     $(NO,NR,IVX,IPA,DW,DP,VX,DV,A,Z,MPROM,XNE,IHSSP,FDDL)
C
C     Rudolf Loeser, 1985 Jun 20
C---- Prints depth-dependent tables, for BANANA.
C     !DASH
      save
C     !DASH
      real*8 A, DP, DV, DW, FDDL, VX, XNE, Z
      integer I, IHSSP, LAB, MPROM, NO, NR
      logical IPA, IVX
C     !DASH
      external ZEROI, LINER, ENEAS, HI, BYE
C
C               DW(N), DP(N), VX(N), DV(N), A(N), Z(N), XNE(N), FDDL(N)
      dimension DW(*), DP(*), VX(*), DV(*), A(*), Z(*), XNE(*), FDDL(*)
C
      dimension LAB(9)
C     !EJECT
C
      call HI ('DUNLIN')
C     !BEG
      call ZEROI   (LAB,1,9)
C
      call LINER   (2,NO)
  100 format(' ',10X,A4,1P10E11.3)
C
      write (NO,100)  '  DW',(DW(I),I=1,NR)
      LAB( 1) = 1
C
      write (NO,100)  '  DP',(DP(I),I=1,NR)
      LAB( 2) = 1
C
      if((MPROM.eq.1).or.(IHSSP.gt.0)) then
        write (NO,100)'  NE',(XNE(I),I=1,NR)
        LAB( 3) = 1
      end if
C
      if(IHSSP.gt.0) then
        write (NO,100)'FDDL',(FDDL(I),I=1,NR)
        LAB( 4) = 1
      end if
C
      if(IVX) then
        write (NO,100)'  VX',(VX(I),I=1,NR)
        LAB( 5) = 1
        write (NO,100)'  DV',(DV(I),I=1,NR)
        LAB( 6) = 1
      end if
C
      if(IPA) then
        write (NO,100)'   A',(A(I),I=1,NR)
        LAB( 7) = 1
      end if
C
      write (NO,100)  '   Z',(Z(I),I=1,NR)
      LAB( 8) = 1
      LAB( 9) = 1
C
      if(IVX) then
        call LINER (1,NO)
        write (NO,101)
  101   format(' ',9X,'(  DV  is computed for Mu = 1)')
      end if
C
      call ENEAS   (NO,LAB)
C     !END
      call BYE ('DUNLIN')
C
      return
      end
