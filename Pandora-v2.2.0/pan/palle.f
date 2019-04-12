      subroutine PALLE
     $(X,W,IW,WVL,CDW,DNU,XI,K,YES)
C
C     Rudolf Loeser, 2003 Mar 11
C---- Makes an augmented, sorted XI-table.
C     (This is version 2 of PALLE.)
C     !DASH
      save
C     !DASH
      real*8 CDW, CRIT, DNU, W, WVHI, WVL, WVLO, X, XI
      integer IN, IPEX, IS, IW, IWS, IXIA, JN, JPNT, K, KA, KM, KS,
     $        LUEO, MOX, MUX
      logical DUMP, YEES, YES, YES2, YES3, YESE, YESH, YESO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external PUSA, UPSA, DUSTUP, RUSTAM, SORT, WGIVE, MESHED, MASHED,
     $         VECOUT, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XI(KM)
      dimension XI(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXIA  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),JPNT  )
C
      data CRIT /1.D-8/
C
      call HI ('PALLE')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call PUSA (IN, IS , MOX, 'PALLE')
      call UPSA (JN, IWS, MUX, 'PALLE')
C
      YES  = .false.
      DUMP = (IPEX.lt.0).or.(IPEX.eq.25)
      WVLO = WVL+CDW*XI(1)
      WVHI = WVL+CDW*XI(K)
C
C---- Set up potential additional XI-values
      call DUSTUP     (X, WVLO, WVL, WVHI, CRIT, CDW, DNU, DUMP,
     $                 W(IXIA), KA, YESH, YESO, YEES, YESE, YES2, YES3)
C     !EJECT
      if(KA.gt.0) then
C
        if(DUMP) then
          call MESHED ('PALLE', 2)
          write (LUEO,100) WVL,CDW,WVLO,WVHI,DNU,CRIT,YESH,YESO,
     $                     YEES,YESE,YES2,YES3
  100     format(' ','WVL =',1PE20.12,', CDW =',E16.8,'; WVLO =',E16.8,
     $               ', WVHI =',E16.8/
     $           ' ','DNU =',E20.12,', CRIT =',E10.2/
     $           ' ','YESH =',L5,', YESO =',L5,', YEES =',L5,
     $               ', YESE =',L5,', YES2 =',L5,', YES3 =',L5)
          call VECOUT (LUEO, XI, K, 'Starting XI')
          call VECOUT (LUEO, W(IXIA), KA, 'XIA')
        end if
C
C----   Add to existing table, and update K
        KS = K
        call RUSTAM   (XI, K, KM, W(IXIA), KA, CRIT)
        YES = K.gt.KS
        if(YES) then
C----     Sort
          call SORT   (XI, K, IW(JPNT), 'Augmented XI')
        end if
C
        if(DUMP) then
          call VECOUT (LUEO, XI, K, 'Augmented XI')
          call MASHED ('PALLE')
        end if
      end if
C
C     (Give back W & IW allotments)
      call WGIVE      (W , 'PALLE')
      call IGIVE      (IW, 'PALLE')
C     !END
      call BYE ('PALLE')
C
      return
      end
