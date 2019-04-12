      subroutine DISTOMA
     $(XISS,ASS,KSS,XIFS,AFS,KFS,XIST,KST,XIBT,KBT,XIRT,KRT,XIS,AS,KS,
     $ XIF,AF,KF,MODE,W,IW,LAB,YUSED,GOODX)
C
C     Rudolf Loeser, 1991 Aug 29
C
C---- Sets up the "candidate tables" which might be used for the
C     C U R R E N T   T R A N S I T I O N :
C
C     Symmetric half         XIS, AS (length KS), and
C     Full profile           XIF, AF (length KF).
C
C---- Returns with
C     MODE=1 if transition-specific data were used (i.e. suffix "T"),
C     MODE=2 if "standard"          data were used (i.e. suffix "S").
C
C     Also upon return, GOODX tells whether any required
C     calculations (for AS and/or AF) went all right.
C     !DASH
      save
C     !DASH
      real*8 AF, AFS, AS, ASS, W, XIBT, XIF, XIFS, XIRT, XIS, XISS,
     $       XIST, Y, YUSED
      integer IW, KBT, KF, KFS, KRT, KS, KSS, KST, MODE
      logical GOODF, GOODS, GOODX, STOP
      character LAB*(*)
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
      equivalence (RZQ(  8),Y    )
C     !DASH
C     !EJECT
      external MOVE1, COWRY, WARNINC, HAPAX, HI, BYE
C
      dimension W(*), IW(*)
C
C               XISS(KSS), ASS(KSS), XIBT(KM), XIS(KM), AS(KM), AF(KM),
      dimension XISS(*),   ASS(*),   XIBT(*),  XIS(*),  AS(*),  AF(*),
C
C               XIFS(KFS), AFS(KFS), XIF(KM), XIST(KST), XIRT(KM)
     $          XIFS(*),   AFS(*),   XIF(*),  XIST(*),   XIRT(*)
C
      call HI ('DISTOMA')
C     !BEG
      GOODX = .true.
C
      if((KST.gt.0).or.(KBT.gt.0).or.(KRT.gt.0)) then
C
C----   At least some transition-specific data exist -
C       they have priority; so, use whatever is available
C
C----   Set return code
        MODE = 1
C
        if(KST.le.0) then
C----     Transition-specific symmetric XI is missing -
C         set it equal to standard set
          KST = KSS
          call MOVE1  (XISS, KSS, XIST)
        end if
C----   Now set up candidate symmetric frequency set, XIS
        KS = KST
        call MOVE1    (XIST, KST, XIS)
C----   Check to make sure XIS is ok
        STOP = .false.
        call WARNINC  (XIS, KS, 'XIsym'//LAB(2:), 'DISTOMA', STOP)
        if(.not.STOP) then
C----     Yes, XIS is ok;
C         compute and check corresponding summation weights, AS
          call COWRY (XIS, KS, Y, AS, W, IW, LAB, YUSED, GOODS)
        else
C----     No, XIS is bad; set return signal
          GOODS = .false.
        end if
C     !EJECT
        if(KBT.le.0) then
C----     Transition-specific blue XI is missing -
C         set it equal to symmetric set
          KBT = KST
          call MOVE1 (XIST, KST, XIBT)
        end if
        if(KRT.le.0) then
C----     Transition-specific red XI is missing -
C         set it equal to symmetric set
          KRT = KST
          call MOVE1 (XIST, KST, XIRT)
        end if
C----   Now set up transition-specific full frequency set, XIF
        KF = KBT+KRT-1
        call HAPAX   (XIBT, KBT, XIRT, KRT, XIF, KF, 1)
C----   Check to make sure XIF is ok
        STOP = .false.
        call WARNINC (XIF, KF, 'XIful'//LAB(2:), 'DISTOMA', STOP)
        if(.not.STOP) then
C----     Yes, XIF is ok;
C         compute and check corresponding summation weights, AF
          call COWRY (XIF, KF, Y, AF, W, IW, LAB, YUSED, GOODF)
        else
C----     No, XIF is bad; set return signal
          GOODF = .false.
        end if
      else
C
C----   No transition-specific data exist -
C       so, use existing standard tables
C
C----   Set return code and signals
        MODE  = 2
        GOODS = .true.
        GOODF = .true.
        YUSED = Y
C----   Set up candidate symmetric sets, XIS and AS
        KS = KSS
        call MOVE1   (XISS, KSS, XIS)
        call MOVE1   (ASS,  KSS, AS )
C----   Set up candidate full sets, XIF and AF
        KF = KFS
        call MOVE1   (XIFS, KFS, XIF)
        call MOVE1   (AFS,  KFS, AF )
C
      end if
C
      if((.not.GOODS).or.(.not.GOODF)) then
        GOODX = .false.
      end if
C     !END
      call BYE ('DISTOMA')
C
      return
      end
