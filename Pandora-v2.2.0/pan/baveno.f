      subroutine BAVENO
     $(XLHM,AHM,WAVE,A,IND,NW)
C
C     Rudolf Loeser, 1984 Aug 14
C---- Sets up a table of wavelengths, etc., for ISIS.
C     (This is version 2 of BAVENO.)
C     !DASH
      save
C     !DASH
      real*8 A, AHM, WAVE, WL, XLHM, ZERO
      integer I, IND, IPEX, IQHMO, JF, JL, LUEO, MHL, MHM, N, NW
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(22),MHM)
C
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
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 5),MHL  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(183),IQHMO)
C
C---- TABLET      as of 2007 Feb 21
      integer     KONLIM,KONWAL
      parameter   (KONLIM=20000)
      real*8      CONTIT,CONWAV
      integer     NUMKON,NUMTRU,NMKUSE,KONADR,KONTYP,KONLIC,KONNSH
      dimension   CONTIT(KONLIM),CONWAV(KONLIM),KONADR(KONLIM),
     $            KONTYP(KONLIM),KONLIC(KONLIM),KONNSH(KONLIM)
      common      /TABLET0/ KONWAL,NUMKON,NUMTRU,NMKUSE
      common      /TABLET1/ CONWAV
      common      /TABLET2/ CONTIT
      common      /TABLET3/ KONADR
      common      /TABLET4/ KONTYP
      common      /TABLET5/ KONLIC
      common      /TABLET6/ KONNSH
C
C     Index, and other data, for Continuum Data Blocks.
C
C     KONWAL - (= KONLIM)
C     NUMKON - total number of Blocks
C     NUMTRU - number of line-specific Blocks ( .le. NUMKON)
C     NMKUSE - number of Blocks to be used for SIAM scratch storage
C
C     CONTIT - Block name (also called "Header Code" or XLTIT,SLTIT)
C     CONWAV - wavelength (Angstroms)
C     KONADR - file address of Block
C     KONTYP - Block code (labelled-common "kwack" via subroutine BEECH)
C     KONNSH - number of supplementary headers (shared blocks only)
C     KONLIC - line transition descriptor, = 100*iu+il (if needed)
C     .
C     !DASH
C     !EJECT
      external MOVE1, ZEROI, RAYMOND, MESHED, BOLZANO, ADONIS, MASHED,
     $         LINER, HALT, ABORT, HI, BYE
C
C               XLHM(MHM), WAVE(NW), A(NW), IND(NW), AHM(MHM)
      dimension XLHM(*),   WAVE(*),  A(*),  IND(*),  AHM(*)
C
      data WL /1.6421D4/
C
      call HI ('BAVENO')
C     !BEG
      OK = .true.
      if(IQHMO.le.0) then
C----   Try to use all wavelengths in the range
        call RAYMOND   (XLHM(  1), 0, JF)
        call RAYMOND   (XLHM(MHM), 0, JL)
        OK = (JL.ge.JF).and.(JL.ne.0)
        if(OK) then
          call BOLZANO (CONWAV(JF), KONTYP(JF), (JL-JF+1), XLHM, AHM,
     $                  MHM, WAVE, A, IND, NW)
        else
          call MESHED  ('BAVENO', 3)
          write (LUEO,100)
  100     format(' ','Trouble in BAVENO: selecting H- wavelengths.'//
     $           ' ','Only the specified H- wavelengths will be used.')
          call MASHED  ('BAVENO')
        end if
      end if
C
      if((IQHMO.gt.0).or.(.not.OK)) then
C----   Use only the specified H- wavelengths
        call MOVE1     (XLHM, MHM, WAVE)
        call MOVE1     (AHM,  MHM, A   )
        call ZEROI     (IND, 1, MHM)
        NW = MHM
      end if
C
C---- Set up upper wavelength limit
      NW       = NW+1
      WAVE(NW) = WL
      A   (NW) = ZERO
      IND (NW) = 0
C
      if((IPEX.lt.0).or.(IPEX.eq.2)) then
        call MESHED    ('BAVENO', 2)
        write (LUEO,101) 'BAVENO',JF,JL,MHM,MHL,NW
  101   format(' ',A,2X,'JF=',I4,2X,'JL=',I4,2X,'MHM=',I4,2X,'MHL=',I4,
     $             2X,'NW=',I4)
        call MASHED    ('BAVENO')
      end if
C     !EJECT
      if(NW.gt.MHL) then
C----   Error: final NW is greater than the predicted limit
        call MESHED ('BAVENO', 1)
        call ADONIS
        call LINER  (1, LUEO)
        write (LUEO,102)
  102   format(' ','Trouble selecting H- wavelengths.')
        write (LUEO,101) ' ',JF,JL,MHM,MHL,NW
        write (LUEO,103)
  103   format(' ',22X,'WAVE',15X,'A',9X,'IND')
        write (LUEO,104) (I,WAVE(I),A(I),IND(I),I=1,NW)
  104   format(' ',I10,1P2E16.8,I12)
        call ABORT
      end if
C     !END
      call BYE ('BAVENO')
C
      return
      end
