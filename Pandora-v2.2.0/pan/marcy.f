      subroutine MARCY
     $(WAVES,LTYPE,NW,EMU,N,DIDH,KMX)
C
C     Rudolf Loeser, 2007 Jan 25
C---- Saves some continuum dI/dh tables.
C     !DASH
      save
C     !DASH
      real*8 DIDH, EMU, WAVES
      integer I, J, KMX, KNT, LTYPE, LUSO, N, NW
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
C
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
      external BEECH, HI, BYE
C
C               WAVES(Nmkuse), LTYPE(Nmkuse), DIDH(N,Nmkuse)
      dimension WAVES(*),      LTYPE(*),      DIDH(N,*)
C
      call HI ('MARCY')
C     !BEG
      KNT = 0
C
      do 104 J = 1,NW
        call BEECH (LTYPE(J))
        if(KWKS(2)) then
          KNT = KNT+1
          if(KNT.eq.1) then
            write (LUSO,100) EMU
  100       format('----9  DI/DH'/ 1P,
     $             E20.12,2X,'Mu')
          end if
          write (LUSO,101) J
  101     format(I10,2X,'index of wavelength in DIDH(n,nw,1)')
          write (LUSO,102) WAVES(J)
  102     format(1PE20.12,2X,'wavelength (Angstroms)')
          write (LUSO,103) (DIDH(I,J),I=1,N)
  103     format(1P10E12.5)
          if(KNT.eq.KMX) goto 105
        end if
  104 continue
C
  105 continue
C     !END
      call BYE ('MARCY')
C
      return
      end
