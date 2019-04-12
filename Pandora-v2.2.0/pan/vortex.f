      subroutine VORTEX
     $(TE,V,AMASS,WVL,LDL,DDL,BXI,KBX,XI,K)
C
C     Rudolf Loeser, 2007 Feb 01
C---- Constructs an augmented XI-table for blended background lines.
C     !DASH
      save
C     !DASH
      real*8 AMASS, BXI, CDW, DDL, DELTA, OFF, TE, V, WVL, XI
      integer IPNT, K, KBX, LDL, NDWM
C     !COM
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
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
      equivalence (KZQ(128),NDWM )
C     !DASH
      external MOLE, MOVE1, CONDIV, GOURD, ABRAXAS, HI, BYE
C
C               TE(N), V(N), DDL(*), BXI(*), XI(*)
      dimension TE(*), V(*), DDL(*), BXI(*), XI(*)
C
      dimension OFF(NIAUGM), IPNT(NIAUGM)
C
      data DELTA /1.D-10/
C
      call HI ('VORTEX')
C     !BEG
C---- Compute CDW, the reference doppler width
      call MOLE    (TE, V, NDWM, AMASS, WVL, CDW)
C---- Convert wavelength offsets to frequency
      call MOVE1   (DDL, LDL, OFF)
      call CONDIV  (CDW, OFF, LDL)
C---- Set up augmented XI table, using the standard set of
C     background XIs
      call GOURD   (LDL, OFF, KBX, BXI, K, XI, DELTA, IPNT)
C---- Test validity of augmented table
      call ABRAXAS (XI, K)
C     !END
      call BYE ('VORTEX')
C
      return
      end
