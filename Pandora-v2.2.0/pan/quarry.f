      subroutine QUARRY
     $(DDT,MDTRY1,MDTRY2,N,NDT,XDT,APD,RJ,RB,NTRY1,NTRY2,TDUSTO,TDUSTN,
     $ TDUSTF,PLANK,XLDT,XJNU,H,Z,HINT,DBH,HT,HK,WTD,YFLUX,TLTR)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Prints for QUIVER.
C     !DASH
      save
C     !DASH
      real*8 APD, DBH, DDT, H, HINT, HK, HT, PLANK, RB, RJ, TDUSTF,
     $       TDUSTN, TDUSTO, TLTR, WTD, XDT, XJNU, XLDT, YFLUX, Z
      integer I, MDTRY1, MDTRY2, MO, N, NDT, NTRY1, NTRY2
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external PRIAM, LINER, TIP, BENITO, HI, BYE
C
C               XDT(NDT), APD(NDT), HINT(N), RB(N), NTRY1(N), NTRY2(N),
      dimension XDT(*),   APD(*),   HINT(*), RB(*), NTRY1(*), NTRY2(*),
C
C               TDUSTO(N), TDUSTN(N), PLANK(N,NDT), XJNU(N,NDT), RJ(N),
     $          TDUSTO(*), TDUSTN(*), PLANK(*),     XJNU(*),     RJ(*),
C
C               XLDT(NDT), TDUSTF(N), H(N,NDT), HK(N), HT(N), DBH(N),
     $          XLDT(*),   TDUSTF(*), H(*),     HK(*), HT(*), DBH(*),
C
C               Z(N)
     $          Z(*)
C     !EJECT
C
      call HI ('QUARRY')
C     !BEG
      if(MO.gt.0) then
C----   Print heading
        call PRIAM   (MO, 'DUST', 4)
        call LINER   (3, MO)
        write (MO,100)
  100   format(' ','Dust Temperature Calculation')
C
C----   Print XJNU and APD
        call TIP     (MO, N, NDT, 'Mean Intensities', 'LDT', XLDT,
     $                APD, XJNU)
C
C----   Print iterated functions
        call LINER   (3, MO)
        write (MO,101)
  101   format(' ',24X,'Final',7X,'Number of trials',
     $             11X,'Dust Temperature'/
     $         ' ',12X,'RJ',11X,'RB',10X,'(Incr)  (Decr)',14X,'Old',
     $             7X,'New')
        call LINER   (2, MO)
        write (MO,102) (I,RJ(I),RB(I),NTRY1(I),NTRY2(I),TDUSTO(I),
     $                    TDUSTN(I),I=1,N)
  102   format(5(' ',I3,1PE15.3,E13.3,2X,2I8,10X,0P2F10.2/))
        call LINER   (2, MO)
        write (MO,103) DDT,MDTRY1,MDTRY2
  103   format(' ',15X,'DDT',F10.4,10X,'(',I2,')',4X,'(',I2,')')
C
C----   Print PLANK and APD
        call TIP     (MO, N, NDT, 'Planck Function (TDUST)', 'XDT',
     $                XDT, APD, PLANK)
C
C----   Print monochromatic flux
        call BENITO  (MO, XLDT, NDT, N, H, 'Monochromatic Flux')
C
C----   Print data for flux-based correction
        call LINER   (3, MO)
        write (MO,104) WTD,TLTR,YFLUX
  104   format(' ','Flux-based temperature correction.'//
     $         ' ','Restraining parameter (default =1 ) WTD =',0PF8.3/
     $         ' ','Limiting multiplier for temperature correction ',
     $             '(default = 1.3) TLTR =',0PF8.3/
     $         ' ','Damping parameter for Flux, YFLUX =',0PF8.3///
     $         ' ',24X,'Flux-weighted',3X,'Flux-weighted',
     $             6X,'Integrated',28X,'Dust Temperature'/
     $         ' ',20X,'Z',9X,'Opacity',13X,'Tau',12X,'Flux',13X,'DBH',
     $             15X,'New',5X,'Final')
        call LINER   (1, MO)
        write (MO,105) (I,Z(I),HK(I),HT(I),HINT(I),DBH(I),TDUSTN(I),
     $                  TDUSTF(I),I=1,N)
  105   format(5(' ',I5,1P5E16.8,8X,0P2F10.2/))
      end if
C     !END
      call BYE ('QUARRY')
C
      return
      end
