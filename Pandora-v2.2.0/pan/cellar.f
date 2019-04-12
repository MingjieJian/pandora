      subroutine CELLAR
C
C     Rudolf Loeser, 1982 Aug 20
C---- Allocates some storage blocks.
C     !DASH
      save
C     !DASH
      integer LEN, LUEO, NOION
C     !COM
C---- WORLD       as of 2002 Jun 04
C
      integer     LISTK
      parameter   (LISTK = 100)
      integer     ISTCK,INEXT,ILMIT,IUMAX,IUKNT
      dimension   ISTCK(LISTK)
      common      /WORLD/ ISTCK,INEXT,ILMIT,IUMAX,IUKNT
C     Management of floating point working/scratch storage in X
C     - ISTCK is the allocation stack
C     - INEXT is the stack index for the next allocation
C     - ILMIT is the length of X
C     - IUMAX and IUKNT are cumulative usage statistics.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     !EJECT
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  MIKI, JASPER, LAPIS, STROPHE, MESHED, ABORT, HI, BYE
      intrinsic max
C
      call HI ('CELLAR')
C     !BEG
      call MIKI     (KKK, MIKLEN)
      call STROPHE  (ILB, LENLYB)
      if(NOION.le.0) then
        call JASPER (LPD, LPDLEN)
        call LAPIS  (LOD, LODLEN)
      else
        LPDLEN = 0
        LODLEN = 0
      end if
C
      LEN = max(MIKLEN,LPDLEN,LODLEN,LENLYB)
      if(LEN.gt.ILMIT) then
        call MESHED ('CELLAR', 1)
        write (LUEO,100) MIKLEN,LPDLEN,LODLEN,LENLYB,ILMIT
  100   format(' ','A data block is too large.'/
     $         ' ','Continuum',T15,'MIKLEN =',I16/
     $         ' ','Diana',T15,'LPDLEN =',I16/
     $         ' ','Orion',T15,'LODLEN =',I16/
     $         ' ','Lyman',T15,'LENLYB =',I16//
     $         ' ','Storage limit',T15,'LNGTHX =',I16)
        call ABORT
      end if
C     !END
      call BYE ('CELLAR')
C
      return
      end
