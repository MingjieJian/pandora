      subroutine SCATLY
     $(INDX,XLM,N,NOPAC,TE,XNE,V,HN,XLMXX,XLMDR,EN,WN,AN,SA,SF,VEC,
     $ CONT,COMP)
C
C     Rudolf Loeser, 1988 Oct 27
C---- Computes a set of H Lyman alpha "continuum" scattering values.
C     Will use existing values in COMP, if a preceding call to
C     WYMANA (q.v.) left some there.
C     (This is version 2 of SCATLY.)
C     !DASH
      save
C     !DASH
      real*8 AN, COMP, CONT, EN, HN, SA, SF, TE, V, VEC, WN, XLM, XLMDR,
     $       XLMXX, XNE, dummy
      integer I, INDX, LLY, LYODS, N, NOPAC
      logical COMPZ, DMPI, DUMP, YES
C     !COM
C---- HILYLI      as of 2005 Dec 22
      integer     LYLINO
      common      /HILYLI/ LYLINO
C     Index of upper level, if this is an H Lyman line wavelength
C     (see subroutine ISTUR).
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
      external KIDDER, MINNA, BIE, ZEROD, NAUGHTD, MOVED, MALTA, YALTA,
     $         HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY), XNE(N), CONT(Nopac,N), COMP(N),
      dimension XLMXX(*),   XLMDR(*),   XNE(*), CONT(NOPAC,*), COMP(*),
C
C               HN(N,LIMP), TE(N), EN(NLL), AN(NLL,N), SA(NLL,N), V(N),
     $          HN(N,*),    TE(*), EN(*),   AN(*),     SA(*),     V(*),
C
C               WN(NLL), SF(NLL), VEC(NLL)
     $          WN(*),   SF(*),   VEC(*)
C     !EJECT
C
      call HI ('SCATLY')
C     !BEG
      call KIDDER      (XLM, YES, DUMP)
C
      if(YES.and.(LYLINO.ne.2)) then
C----   Check to see whether any value of COMP is non-zero
        call NAUGHTD   (COMP, 1, N, COMPZ)
        if(COMPZ) then
C
C----     Nothing is there -- must compute
          call MALTA   (XLM, DUMP, 'SCATLY')
          do 100 I = 1,N
            call MINNA (DUMP, I, LYODS, DMPI)
            call BIE   (XLM, TE(I), XNE(I), V(I), HN(I,1), XLMXX, XLMDR,
     $                  LLY, I, EN, WN, AN, SA, SF, dummy, CONT(INDX,I),
     $                  DMPI)
  100     continue
          call YALTA   (DUMP, 'SCATLY')
        else
C----     Something is there -- use it
          call MOVED   (COMP, 1, N, CONT(INDX,1), NOPAC, N)
        end if
C
      else
        call ZEROD     (CONT(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('SCATLY')
C
      return
      end
