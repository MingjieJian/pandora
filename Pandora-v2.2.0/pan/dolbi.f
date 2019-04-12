      subroutine DOLBI
     $(L,TEMP,DENS,DUMP,CI)
C
C     Rudolf Loeser, 1990 Dec 07
C---- Computes default value of CII for Hydrogen.
C
C     Vriens, L., and Smeets, A.H.M.  1980, Phys.Rev.A, 72, 940.
C
C     !DASH
      save
C     !DASH
      real*8 BOLZMN, C, C1, C2, CI, CQL, DENS, ONE, P1, P2, P3, TEMP,
     $       TP, Y1, Y2, YL, YT, dummy
      integer L, LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  CUEL, ORNIS, LINER, HI, BYE
      intrinsic abs
C
      data C /1.939D-23/
      data C1,C2 /4.38D0, 1.32D0/
      data P1,P2,P3 /1.5D0, 2.33D0, 1.72D0/
C
      call HI ('DOLBI')
C     !BEG
      call CUEL   (L, TEMP, DENS, CQL)
      call ORNIS  (L, ONE, CQL, ONE, TEMP, YL, dummy)
C
      TP = (BOLZMN*TEMP)**P1
      Y2 = YL**P2
      Y1 = YL**P3
      YT = Y2+C1*Y1+C2*YL
C
      CI = abs(C/(TP*YT))
C
      if(DUMP) then
        call LINER (2, LUEO)
        write (LUEO,100) L,TEMP,DENS,CQL,YL,Y2,Y1,YT,TP,CI
  100   format(' ','CII according to Vriens and Smeets.',5X,'L =',I5,
     $             5X,'TEMP =',1PE16.8,5X,'DENS =',E16.8/
     $         ' ','QL =',E16.8,5X,'YL =',E16.8,5X,'Y2 =',E16.8,5X,
     $             'Y1 =',E16.8/
     $         ' ','YT =',E16.8,5X,'TP =',E16.8,5X,'CII =',E16.8)
      end if
C     !END
      call BYE ('DOLBI')
C
      return
      end
