      subroutine SMOOTH
     $(X,F,N, TYPE,LABEL,INDX, W,IW, KRET,OK)
C     Rudolf Loeser, 1998 Oct 15
C---- Improved sequential smoothing with irregular point spacing.
C     TYPE may equal "log" or "lin".
C     (Only up to the first 100 characters of LABEL are used.)
C
C     Upon return, OK = .false. means that something went wrong.
C     The smoothed values of F(X) are returned in F, and KRET is the
C     count of values that were modified.
C     (This is version 2 of SMOOTH.)
C     !DASH
      save
C     !DASH
      real*8 ASMCR, F, W, X
      integer IDF, IDX, IFF, IFOLD, IFP, IMG, IN, INDX, IS, IW, IWS, JN,
     $        KRET, MOX, MUX, N, NIASM
      logical DOLOG, OK
      character LABEL*(*), TYPE*3
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
      equivalence (RZQ(111),ASMCR)
      equivalence (KZQ( 67),NIASM)
C     !DASH
      external SLATHER, THOMOS, SHOMOT, BOMBAY, BABMOY, MOYBBA, MABOBY,
     $         OBBAMY, MOVE1, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               X(N), F(N)
      dimension X(*), F(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IFOLD ),(IN( 2),IFF   ),(IN( 3),IDX   ),(IN( 4),IFP   ),
     $(IN( 5),IDF   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IMG   )
C     !EJECT
C
      call HI ('SMOOTH')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call THOMOS   (IN, IS , MOX, 'SMOOTH', N)
      call SHOMOT   (JN, IWS, MUX, 'SMOOTH', N)
C
C---- Save input F
      call MOVE1    (F, N, W(IFOLD))
C---- Set up "DOLOG" switch
      call BABMOY   (F, N, TYPE, DOLOG)
C---- Set up table to be smoothed
      call MOYBBA   (F, N, DOLOG, W(IFF))
C---- Apply smoothing
      call SLATHER  (X, W(IFF), N, ASMCR, NIASM, INDX, W(IDX), W(IFP),
     $               W(IDF), IW(IMG), KRET)
C
      if(KRET.lt.0) then
C----   A fatal error occurred:
C       1) print error message ...
        call OBBAMY (X, F, N, TYPE, LABEL, KRET)
C       ... and, 2) restore original values
        call MOVE1  (W(IFOLD), N, F)
      else if(KRET.gt.0) then
C
C----   Some values were modified:
C       1) copy new values back into F ...
        call MABOBY (W(IFF), IW(IMG), N, DOLOG, F)
C       ... and, 2) print (if required)
        call BOMBAY (X, F, N, TYPE, LABEL, W(IFOLD), IW(IMG), INDX)
      end if
C
      OK = KRET.ge.0
C
C     (Give back W & IW allotments)
      call WGIVE    (W , 'SMOOTH')
      call IGIVE    (IW, 'SMOOTH')
C     !END
      call BYE ('SMOOTH')
C
      return
      end
