      subroutine KELLA
     $(INDX,XLM,N,NOPAC,TE,HN,P,CB)
C
C     Rudolf Loeser, 2002 Sep 24
C---- Computes H Lyman alpha "continuum" source function.
C     (This is version 4 of KELLA.)
C     !DASH
      save
C     !DASH
      real*8 CB, HN, P, TE, XLM
      integer I, INDX, LYODS, N, NLS, NLY, NOPAC
      logical DMPI, DUMP, YES
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
      equivalence (KZQ(184),NLY  )
      equivalence (KZQ( 89),LYODS)
C     !DASH
      external KIDDER, MINNA, CALAMUS, MALTA, YALTA, ZEROD, HI, BYE
C
C               TE(N), HN(N,Limp), P(NLY,N), CB(Nopac,N)
      dimension TE(*), HN(N,*),    P(NLY,*), CB(NOPAC,*)
C
      data NLS /3/
C
      call HI ('KELLA')
C     !BEG
      call KIDDER      (XLM, YES, DUMP)
C
      if(YES.and.(NLY.gt.2)) then
        call MALTA     (XLM, DUMP, 'KELLA')
        do 100 I = 1,N
          call MINNA   (DUMP, I, LYODS, DMPI)
          call CALAMUS (NLS, NLY, XLM, TE(I), N, HN, P(1,I), CB(INDX,I),
     $                  I, DMPI)
  100   continue
        call YALTA     (DUMP, 'KELLA')
C
      else
        call ZEROD     (CB(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('KELLA')
C
      return
      end
