      subroutine RELLA
     $(INDX,XLM,N,NOPAC,TE,XNE,V,HN,XLMXX,XLMDR,EN,WN,AN,SA,SF,VEC,
     $ CONT,COMP,P)
C
C     Rudolf Loeser, 2002 Sep 24
C---- Computes a set of higher H Ly lines background scattering values.
C     Will use existing values in COMP, if a preceding call to HELLA
C     left some there.
C     (This is version 2 of RELLA.)
C     !DASH
      save
C     !DASH
      real*8 AN, COMP, CONT, EN, HN, P, SA, SF, TE, V, VEC, WN, XLM,
     $       XLMDR, XLMXX, XNE, dummy
      integer I, INDX, LLY, LYODS, N, NLY, NOPAC
      logical COMPZ, DMPI, DUMP, YES
C     !COM
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
      equivalence (KZQ(184),NLY  )
      equivalence (KZQ( 89),LYODS)
C     !DASH
      external KIDDER, MINNA, AMETH, ZEROD, NAUGHTD, MOVED, RONGA,
     $         MALTA, YALTA, HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY), TE(N), CONT(Nopac,N), P(NLY,N),
      dimension XLMXX(*),   XLMDR(*),   TE(*), CONT(NOPAC,*), P(NLY,*),
C
C               HN(N,LIMP), V(N), COMP(N), XNE(N), VEC(NLL), AN(NLL,N),
     $          HN(N,*),    V(*), COMP(*), XNE(*), VEC(*),   AN(*),
C
C               SA(NLL,N), EN(NLL), WN(NLL), SF(NLL)
     $          SA(*),     EN(*),   WN(*),   SF(*)
C     !EJECT
C
      call HI ('RELLA')
C     !BEG
      call KIDDER      (XLM, YES, DUMP)
C
      if(YES.and.(NLY.gt.2)) then
C----   Check to see whether any value of COMP is non-zero
        call NAUGHTD   (COMP, 1, N, COMPZ)
        if(COMPZ) then
C
C----     Nothing is there -- must compute
          call MALTA   (XLM, DUMP, 'RELLA')
          do 100 I = 1,N
            call MINNA (DUMP, I, LYODS, DMPI)
            call AMETH (NLY, XLM, TE(I), XNE(I), V(I), HN(I,1), XLMXX,
     $                  XLMDR, LLY, I, EN, WN, AN, SA, SF, P(1,I),
     $                  dummy, CONT(INDX,I), DMPI)
  100     continue
          call YALTA   (DUMP, 'RELLA')
        else
C----     Something is there -- use it
          call MOVED   (COMP, 1, N, CONT(INDX,1), NOPAC, N)
        end if
C
      else
        call ZEROD     (CONT(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('RELLA')
C
      return
      end
