      subroutine ANGELA
     $(INDX,XLM,N,NOPAC,TE,HN,CO,CB)
C
C     Rudolf Loeser, 1995 May 11
C---- Computes H Lyman alpha "continuum" source function.
C     (This is version 4 of ANGELA.)
C     !DASH
      save
C     !DASH
      real*8 CB, CO, HN, SL, TE, XLM
      integer I, INDX, IU, LYODS, N, NOPAC
      logical DMPI, DUMP, YES
C     !COM
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
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
      equivalence (KZQ( 89),LYODS)
C     !DASH
C     !EJECT
      external KIDDER, MINNA, HILLARY, MALTA, YALTA, ZEROD, HI, BYE
C
C               TE(N), HN(N,Limp), CO(Nopac,N), CB(Nopac,N)
      dimension TE(*), HN(N,*),    CO(NOPAC,*), CB(NOPAC,*)
C
      data IU /2/
C
      call HI ('ANGELA')
C     !BEG
      call KIDDER      (XLM, YES, DUMP)
C
      if(YES) then
        call MALTA     (XLM, DUMP, 'ANGELA')
        do 100 I = 1,N
          call MINNA   (DUMP, I, LYODS, DMPI)
          call HILLARY (IU, XLM, TE(I), HN(I,1), HN(I,IU), CO(INDX,I),
     $                  SL, I, DMPI)
          CB(INDX,I) = (SL*FLNRML(2))*CO(INDX,I)
  100   continue
        call YALTA     (DUMP, 'ANGELA')
C
      else
        call ZEROD     (CB(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('ANGELA')
C
      return
      end
