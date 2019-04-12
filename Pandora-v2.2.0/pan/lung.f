      subroutine LUNG
     $(NW,WAVES,LTYPE,L,NCP,WAVESB,YHZ,YAN,BT,YHZB,YANB,BTB,J)
C
C     Rudolf Loeser, 1983 Oct 28
C---- Culls out Composite Line Opacity wavelengths, for HEART.
C     !DASH
      save
C     !DASH
      real*8 BT, BTB, WAVES, WAVESB, YAN, YANB, YHZ, YHZB
      integer I, J, L, LTYPE, NCP, NW
C     !COM
C---- KWACK       as of 2006 Mar 14
      integer     MWKS,NWKS,KSWPR,KSWSH
      parameter   (MWKS=27)
      logical     KWKS
      dimension   KWKS(MWKS),KSWPR(MWKS),KSWSH(MWKS)
C     (Need to revise     TURKOIS     when changing MWKS ! )
      common      /KWACK1/ NWKS,KWKS
      common      /KWACK2/ KSWPR
      common      /KWACK3/ KSWSH
C---- Codes describing "continuum" wavelengths
C
C      1: regular (constant background) line center, printed;
C      2: "additional" wavelength, no Eclipse;
C      3: "additional" wavelength, with Eclipse;
C      4: line source function background, PRD;
C      5: rates integrations, regular;
C      6: additional photoionization;
C      7: H- calculation;
C      8: dust temperature adjustment procedure;
C      9: HSE calculation;
C     10: "Lyman" calculation (level-K-to-continuum integration);
C     11: incident coronal radiation;
C     12: rates integrations, K-shell;
C     13: composite line opacity, no Eclipse;
C     14: miscellaneous;
C     15: composite line opacity, with Eclipse;
C     16: line source function background, FDB;
C     17: actual CO-lines opacity, fundamental;
C     18: FDB line center, printed;
C     19: regular (constant background) line center, not printed;
C     20: actual CO-lines opacity, first overtone;
C     21: actual CO-lines opacity, band limit;
C     22: actual CO-lines opacity, rotational;
C     23: actual CO-lines opacity, second overtone.
C     24: PRD line center, printed;
C     25: PRD line center, not printed;
C     26: FDB line center, not printed;
C     27: standard background.
C     .
C     !DASH
C     !EJECT
      external BEECH, MOVED, HI, BYE
C
C               BT(Nmkuse,L),  YHZB(NCP,L), YHZ(Nmkuse,L), WAVESB(NCP),
      dimension BT(NW,*),      YHZB(NCP,*), YHZ(NW,*),     WAVESB(*),
C
C               BTB(NCP,L), YANB(NCP,L), LTYPE(Nmkuse), WAVES(Nmkuse),
     $          BTB(NCP,*), YANB(NCP,*), LTYPE(*),      WAVES(*),
C
C               YAN(Nmkuse,L)
     $          YAN(NW,*)
C
      call HI ('LUNG')
C     !BEG
      J = 0
      do 100 I = 1,NW
        call BEECH   (LTYPE(I))
C
        if(KWKS(13) .or. KWKS(15)) then
          J         = J+1
          WAVESB(J) = WAVES(I)
          call MOVED (YHZ(I,1), NW, L, YHZB(J,1), NCP, L)
          call MOVED (YAN(I,1), NW, L, YANB(J,1), NCP, L)
          call MOVED ( BT(I,1), NW, L,  BTB(J,1), NCP, L)
        end if
C
  100 continue
C     !END
      call BYE ('LUNG')
C
      return
      end
