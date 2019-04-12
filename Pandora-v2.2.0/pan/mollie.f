      subroutine MOLLIE
     $(LL,TMU,JJ,LG,LABEL,MIK,XMU,TAU)
C
C     Rudolf Loeser, 1985 Jun 28
C---- Dumps, for CARAMAN.
C     !DASH
      save
C     !DASH
      real*8 TAU, TMU, XMU
      integer IPR01, IPR02, JJ, LG, LL, MIK
      logical DUMP
      character LABEL*(*)
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
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
C     !DASH
      external KENT, HI, BYE
C
C               TAU(N), XMU(LG), TMU(N,LG)
      dimension TAU(*), XMU(*),  TMU(*)
C
      call HI ('MOLLIE')
C     !BEG
      DUMP = (LL.ge.IPR01).and.(LL.le.IPR02)
C
      call KENT (TMU, JJ, LG, LABEL, MIK, XMU, TAU, DUMP)
C     !END
      call BYE ('MOLLIE')
C
      return
      end
