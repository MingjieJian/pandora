      subroutine ALMAND
     $(MHL,XLHM,MHM)
C
C     Rudolf Loeser, 1984 Aug 14
C---- Computes the limit on the number of wavelengths to be used for
C     the non-LTE H- calculation.
C     (This is version 2 of ALMAND.)
C     !DASH
      save
C     !DASH
      real*8 XLHM
      integer IPEX, JF, JL, LUEO, MHL, MHM
      logical OK
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external RAYMOND, MESHED, MASHED, HI, BYE
C
C               XLHM(MHM)
      dimension XLHM(*)
C     !EJECT
C
      call HI ('ALMAND')
C     !BEG
      call RAYMOND  (XLHM(  1), 0, JF)
      call RAYMOND  (XLHM(MHM), 0, JL)
      OK = (JL.ge.JF).and.(JL.ne.0)
C
      if(OK) then
        MHL = JL-JF+1
      else
        MHL = MHM+1
      end if
C
      if(((IPEX.lt.0).or.(IPEX.eq.2)).or.(.not.OK)) then
        call MESHED ('ALMAND', 2)
        write (LUEO,100) XLHM(1),JF,XLHM(MHM),JL,MHM,MHL
  100   format(' ','LHM(1)=',1PE16.8,2X,'JF=',I5,2X,'LHM(MHM)=',E16.8,
     $             2X,'JL=',I5,2X,'MHM=',I5,2X,'MHL=',I5)
        call MASHED ('ALMAND')
      end if
C     !END
      call BYE ('ALMAND')
C
      return
      end
