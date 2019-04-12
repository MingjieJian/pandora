      subroutine BOLZANO
     $(CONWAV,KONTYP,KNT,XLHM,AHM,MHM,WAVE,A,IND,NW)
C
C     Rudolf Loeser, 1984 Aug 14
C---- Sets up tables of wavelength (WAVE), opacity (A), and code (IND),
C     for the non-LTE H- calculation.
C     (This is version 2 of BOLZANO.)
C     !DASH
      save
C     !DASH
      real*8 A, AHM, CONWAV, WAVE, XLHM
      integer I, IND, IPEX, KNT, KODE, KONTYP, LUEO, MHM, NW
      logical IS7, USE
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external NORBERT, LININT, BEECH, HALT, MESHED, MASHED, HI, BYE
C
C               CONWAV(KNT), KONTYP(KNT), WAVE(NW), XLHM(MHM), IND(NW),
      dimension CONWAV(*),   KONTYP(*),   WAVE(*),  XLHM(*),   IND(*),
C
C               AHM(MHM), A(NW)
     $          AHM(*),   A(*)
C     !EJECT
C
      call HI ('BOLZANO')
C     !BEG
      NW = 0
C
      do 102 I = 1,KNT
        call NORBERT    (KONTYP(I), USE, IS7)
        if(USE) then
C
          NW = NW+1
          WAVE(NW) = CONWAV(I)
C
          call LININT   (XLHM, 1, AHM, 1, MHM, WAVE(NW), A(NW), 1, 1,
     $                   KODE)
          if(KODE.ne.1) then
            write (MSSLIN(1),100) WAVE(NW)
  100       format('WAVE =',1PE16.8,', does not fall within the ',
     $             'limits of the LHM table.')
            MSSLIN(2) = 'Try running with IPEX = 2 for more output.'
            call HALT   ('BOLZANO', 2)
          end if
C
          if(IS7) then
            IND(NW) = 1
          else
            IND(NW) = 0
          end if
C
          if((IPEX.lt.0).or.(IPEX.eq.2)) then
            call MESHED ('BOLZANO', 2)
            write (LUEO,100) I,CONWAV(I),NW,WAVE(NW),IND(NW)
  101       format(' ','BOLZANO',2(I4,1PE15.8),I4)
            call MASHED ('BOLZANO')
          end if
C
        end if
  102 continue
C     !END
      call BYE ('BOLZANO')
C
      return
      end
