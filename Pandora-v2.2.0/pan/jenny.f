      subroutine JENNY
     $(X,W,XCBL,KIPE,OPAC,TAU,CNTRB,IMG,WTAB,INDX,NOPAC,IPER,MF,ML,
     $ WTABW,INDXW,NW)
C
C     Rudolf Loeser, 1973 May 17
C---- Gleans contributions summary data.
C     (This is version 3 of JENNY.)
C     !DASH
      save
C     !DASH
      real*8 CNTRB, OPAC, TAU, W, WTAB, WTABW, X, XCBL
      integer I, IMG, INDX, INDXW, IPER, KIPE, KKCB, KKCO, MF, ML, N,
     $        NOPAC, NW
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(21),KKCB  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !EJECT
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
      external ZEROI, LEYTE, PEBBLES, JOEY, HALT, HI, BYE
C
      dimension X(*), W(*)
C
C               INDX(Numkon), XCBL(Miklen), IPER(Nopac,Numkon), IMG(N),
      dimension INDX(*),      XCBL(*),      IPER(*),            IMG(*),
C
C               WTABW(Numkon), INDXW(Numkon), CNTRB(Nopac), OPAC(N),
     $          WTABW(*),      INDXW(*),      CNTRB(*),     OPAC(*),
C
C               TAU(N), WTAB(Numkon)
     $          TAU(*), WTAB(*)
C
      call HI ('JENNY')
C     !BEG
      if((KIPE.lt.1).or.(KIPE.gt.3)) then
        write (MSSLIN(1),100) KIPE
  100   format('KIPE =',I12,', which is not 1, 2, or 3.')
        call HALT        ('JENNY', 1)
      end if
C
      call ZEROI         (IPER, 1, (NUMKON*NOPAC))
C
      NW = 0
      do 104 I = MF,ML
        if(INDX(I).gt.0) then
          NW        = NW+1
          WTABW(NW) = WTAB(I)
          INDXW(NW) = INDX(I)
C
C----     Read Continuum Data block
          call LEYTE     (XCBL, MIKLEN, KONADR(I))
C
C----     Get edited contributions at this wavelength
          goto (101, 102, 103), KIPE
  101     continue
C----       Absorbers
            call PEBBLES (XCBL(KKCO), IPER, CNTRB, INDXW(NW), N, NW,
     $                    NOPAC, KIPE)
            goto 104
  102     continue
C----       Emitters
            call PEBBLES (XCBL(KKCB), IPER, CNTRB, INDXW(NW), N, NW,
     $                    NOPAC, KIPE)
            goto 104
  103     continue
C----       Optical Depths
            call JOEY    (X, W, XCBL(KKCO), IPER, INDXW(NW), N, NW,
     $                    OPAC, TAU, CNTRB, WTABW(NW), IMG, NOPAC)
            goto 104
        end if
  104 continue
C     !END
      call BYE ('JENNY')
C
      return
      end
