      subroutine SAIGA
     $(INDX,XLM,N,NOPAC,TE,HN,BDH,UL,CO,CB)
C
C     Rudolf Loeser, 2003 Jan 06
C---- Computes the emission contribution from the highest Ly H lines.
C     (This is version 2 of SAIGA.)
C     !DASH
      save
C     !DASH
      real*8 BDH, CB, CO, EMA, EMB, HN, TE, UL, XLM
      integer I, INDX, LYODS, N, NOPAC
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
      equivalence (KZQ( 89),LYODS)
C     !DASH
      external DIKKER, GASIA, CUSALAM, MINIVET, MALTA, YALTA, MINNA,
     $         LASS, ZEROD, HI, BYE
C
C               HN(N,Limdat(1)), BDH(N,Limdat(1)), TE(N), CB(Nopac,N),
      dimension HN(*),           BDH(*),           TE(*), CB(NOPAC,*),
C
C               UL(N), CO(Nopac,N)
     $          UL(*), CO(NOPAC,*)
C     !EJECT
C
      call HI ('SAIGA')
C     !BEG
      call DIKKER      (XLM, YES, DUMP)
C
      if(YES) then
        call MALTA     (XLM, DUMP, 'SAIGA')
        do 100 I = 1,N
          call MINNA   (DUMP, I, LYODS, DMPI)
          if(DMPI) then
            call GASIA (XLM, I)
          end if
          call CUSALAM (I, XLM, TE, HN, N, EMA, DMPI)
          call MINIVET (I, N, UL, BDH, EMB, DMPI)
          call LASS    (XLM, I, EMA, EMB, CO(INDX,I), CB(INDX,I),
     $                  DMPI)
  100   continue
        call YALTA     (DUMP, 'SAIGA')
C
      else
        call ZEROD     (CB(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('SAIGA')
C
      return
      end
