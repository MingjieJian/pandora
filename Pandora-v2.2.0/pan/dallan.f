      subroutine DALLAN
     $(NO,DODIDH,SVDIDH,DIDH,MYX,MUX,YY,WTAB,K,Z,TE,N,IU,IL,EMU,
     $ LDL,INDL,W,IW)
C
C     Rudolf Loeser, 1991 Jul 05
C---- Prints/plots dI/dh for emergent line profiles.
C     !DASH
      save
C     !DASH
      real*8 DIDH, EMU, TE, W, WTAB, YY, Z, dummy
      integer IDFSW, IL, ILST, IN, INDL, IRUN, IS, IU, IW, IWS, JLST,
     $        JN, K, KODE, LDL, LNGTH, MEX, MOX, MUX, MYX, N, NO
      logical DETAIL, DODIDH, SVDIDH
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
      equivalence (KZQ(  5),IDFSW)
C     !DASH
C     !EJECT
      external DONTAS, OSTUNI, NALDA, LADAN, CREMY, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               DIDH(N,KM), Z(N), TE(N), YY(N), WTAB(KM), INDL(LDLMX),
      dimension DIDH(*),    Z(*), TE(*), YY(*), WTAB(*),  INDL(*),
C
C               MYX(KM), MUX(KM)
     $          MYX(*),  MUX(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IRUN  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),ILST  ),(JN( 2),JLST  )
C
      data KODE /1/
C
      call HI ('DALLAN')
C     !BEG
      if((NO.gt.0).and.DODIDH) then
C       (Get, and allocate, W & IW allotments)
        call NALDA    (IN, IS,  MOX, 'DALLAN')
        call LADAN    (JN, IWS, MEX, 'DALLAN')
C
        DETAIL = IDFSW.eq.1
C
        call DONTAS   (DETAIL, LDL, INDL, K, IW(ILST), LNGTH)
        if(LNGTH.gt.0) then
          call OSTUNI (IW(ILST), IW(JLST), LNGTH, DIDH, MYX, MUX, YY,
     $                 dummy, K, IU, IL, EMU, W(IRUN), Z, TE, N, NO,
     $                 WTAB, KODE, DETAIL)
        end if
        if(SVDIDH) then
          call CREMY  (IW(ILST), LNGTH, N, DIDH)
        end if
C
C       (Give back W & IW allotments)
        call WGIVE    (W,  'DALLAN')
        call IGIVE    (IW, 'DALLAN')
      end if
C     !END
      call BYE ('DALLAN')
C
      return
      end
