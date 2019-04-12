      subroutine WYMANA
     $(INDX,XLM,N,NOPAC,TE,XNE,V,HN,XLMXX,XLMDR,EN,WN,AN,SA,SF,VEC,
     $ CONT,COMP)
C
C     Rudolf Loeser, 1988 Oct 27
C---- Computes a set of H Lyman alpha "continuum" opacity values, and
C     saves corresponding scattering values in COMP.
C     (This is version 2 of WYMANA.)
C     !DASH
      save
C     !DASH
      real*8 AN, COMP, CONT, EN, HN, OPAC, SA, SF, TE, V, VEC, WN, XLM,
     $       XLMDR, XLMXX, XNE
      integer I, INDX, LLY, LYODS, N, NOPAC
      logical DMPI, DUMP, YES
C     !COM
C---- HILYLI      as of 2005 Dec 22
      integer     LYLINO
      common      /HILYLI/ LYLINO
C     Index of upper level, if this is an H Lyman line wavelength
C     (see subroutine ISTUR).
C     .
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(27),LLY)
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
      equivalence (KZQ( 89),LYODS)
C     !DASH
C     !EJECT
      external KIDDER, MINNA, BIE, ZEROD, ZERO1, MALTA, YALTA, HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY), XNE(N), CONT(Nopac,N), COMP(N),
      dimension XLMXX(*),   XLMDR(*),   XNE(*), CONT(NOPAC,*), COMP(*),
C
C               V(N), HN(N,LIMP), EN(NLL), AN(NLL,N), SA(NLL,N), TE(N),
     $          V(*), HN(N,*),    EN(*),   AN(*),     SA(*),     TE(*),
C
C               WN(NLL), SF(NLL), VEC(NLL)
     $          WN(*),   SF(*),   VEC(*)
C
      call HI ('WYMANA')
C     !BEG
      call KIDDER    (XLM, YES, DUMP)
C
      if(YES.and.(LYLINO.ne.2)) then
        call MALTA   (XLM, DUMP, 'WYMANA')
        do 100 I = 1,N
          call MINNA (DUMP, I, LYODS, DMPI)
          call BIE   (XLM, TE(I), XNE(I), V(I), HN(I,1), XLMXX, XLMDR,
     $                LLY, I, EN, WN, AN, SA, SF, OPAC, COMP(I), DMPI)
          CONT(INDX,I) = FLNRML(2)*OPAC
  100   continue
        call YALTA   (DUMP, 'WYMANA')
C
      else
        call ZEROD   (CONT(INDX,1), NOPAC, N)
        call ZERO1   (COMP, N)
      end if
C     !END
      call BYE ('WYMANA')
C
      return
      end
