      subroutine PINANG
     $(NCP,N,ARRCO,CORC,XCBL,GOOD)
C
C     Rudolf Loeser, 2002 Oct 04
C---- Constructs the array of relative values of the Composite Line
C     Opacity, at all data wavelengths.
C     !DASH
      save
C     !DASH
      real*8 ARRCO, CORC, XCBL
      integer J, KKOPAC, LUEO, N, NCP, NW
      logical GOOD, USE
C     !COM
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
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 6),KKOPAC)
C     !EJECT
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external BEECH, LEYTE, MUAR, MESHED, MASHED, HI, BYE
C
C               ARRCO(NCP,N), CORC(NCP,N), XCBL(Miklen)
      dimension ARRCO(*),     CORC(*),     XCBL(*)
C
      call HI ('PINANG')
C     !BEG
      NW = 0
      J  = 0
  100 continue
        J = J+1
        if(J.le.NUMKON) then
          call BEECH      (KONTYP(J))
          USE = KWKS(13).or.KWKS(15)
C
          if(USE) then
            call LEYTE    (XCBL, MIKLEN, KONADR(J))
            NW = NW+1
            if(NW.le.NCP) then
              call MUAR   (NW, N, NCP, XCBL(KKOPAC), ARRCO, CORC)
            else
              call MESHED ('PINANG', 3)
              write (LUEO,101) NW,NCP
  101         format(' ','NW =',I12,' while NCP =',I12,', which is ',
     $                   'not allowed; COMOPAN analysis will be ',
     $                   'skipped.')
              call MASHED ('PINANG')
              goto 102
            end if
          end if
C
          goto 100
        end if
C
  102 continue
      GOOD = NW.eq.NCP
C
      if(.not.GOOD) then
        call MESHED       ('PINANG', 3)
        write (LUEO,101) NW,NCP
        call MASHED       ('PINANG')
      end if
C     !END
      call BYE ('PINANG')
C
      return
      end
