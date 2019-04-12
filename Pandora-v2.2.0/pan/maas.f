      subroutine MAAS
     $(IND,I,PHII,XKLI,DEMI,PKJI,BCI,TNUI,DLCI,OMDI,PQI,PDI,GTNI,CI,A,
     $ XKPCI,SIGI,XJNUI,T1I,T2I,T3I,CAP,ANTI,WNII,AWI)
C
C     Rudolf Loeser, 1980 Jan 28
C---- Prints debug output, for PALLAS.
C     !DASH
      save
C     !DASH
      real*8 A, ANTI, AWI, BCI, CAP, CI, DEMI, DLCI, GTNI, OMDI, PDI,
     $       PHII, PKJI, PQI, SIGI, T1I, T2I, T3I, TNUI, WNII, XJNUI,
     $       XKLI, XKPCI
      integer I, IND, IPR01, IPR02, IPR03, IPR04, LUEO
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
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
      equivalence (KZQ( 59),IPR03)
      equivalence (KZQ( 60),IPR04)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      call HI ('MAAS')
C     !BEG
      if((IND.ge.IPR01).and.(IND.le.IPR02)) then
        if((I.ge.IPR03).and.(I.le.IPR04)) then
C
          call LINER (1, LUEO)
          write (LUEO,100) I,PHII,XKLI,DEMI,PKJI,BCI,TNUI,DLCI,OMDI,
     $                     PDI,GTNI,CI,A,XKPCI,SIGI,XJNUI,PQI,T1I,
     $                     T2I,T3I,CAP,ANTI,WNII,AWI
  100     format(' ','      I   J',11X,'PHI',12X,'KL',11X,'DEM',11X,
     $               'PKJ',12X,'BC',11X,'TNU',11X,'DLC',11X,'OMD'/
     $           ' ',3X,I4,4X,1P8E14.7//
     $           ' ',23X,'PD',11X,'GTN',13X,'C',13X,'A',11X,'KPC',
     $               11X,'SIG',11X,'JNU',12X,'PQ'/
     $           ' ',11X,8E14.7//
     $           ' ',23X,'T1',12X,'T2',12X,'T3',11X,'CAP',11X,'ANT',
     $               7X,'WN(I,I)',12X,'AW'/
     $           ' ',11X,7E14.7)
C
        end if
      end if
C     !END
      call BYE ('MAAS')
C
      return
      end
